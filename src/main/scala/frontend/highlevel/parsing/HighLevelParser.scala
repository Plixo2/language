package frontend.highlevel.parsing

import frontend.exceptions.LanguageException
import frontend.highlevel.*
import frontend.parser.{Node, NodeResult}

object HighLevelParser {
    def parseMethod(node: Node): HighLevelFunction = {
        node.assertType("function")
        val name = node.id()
        val parameters = node.map("parameter", node => parseParameter(node))
        val returnType = node("type")
            .map(HighLevelTypeParser.parseType)
            .getOrElse(HighLevelType.UNIT(node.region))
        val expression = HighLevelExpressionParser.parse(node("expression").expect("expression on function node"))
        HighLevelFunction(node.region, name, parameters, returnType, expression)
    }

    private def parseParameter(node: Node): Parameter = {
        node.assertType("parameter")
        val name = node.id()
        val tpe = HighLevelTypeParser.parseType(node("type").expect("type on parameter node"))
        Parameter(node.region, name, tpe)
    }
}

object HighLevelTypeParser {
    def parseType(node: Node): HighLevelType = {
        node.assertType("type")
        val region = node.region
        node.id() match {
            case "int"  => HighLevelType.INT(region)
            case "bool" => HighLevelType.BOOL(region)
            case "unit" => HighLevelType.UNIT(region)
            case rest   => HighLevelType.Class(region, rest)
        }
    }
}

object HighLevelExpressionParser {

    def parse(node: Node): HighLevelExpression = {
        node.assertType("expression")
        val expression = parseObject(node("object").expect("object on expression"))
        val postFixApplied = node.getAll("postfix").foldLeft(expression)(parsePostFix)
        postFixApplied
    }

    private def parsePostFix(left: HighLevelExpression, node: Node): HighLevelExpression = {
        node.assertType("postfix")
        val region = node.region
        node("call") match {
            case NodeResult.Some(node) => {
                parseCall(left, node)
            }
            case NodeResult.None(region) => {
                node("id") match {
                    case NodeResult.Some(node) => {
                        parseDot(left, node)
                    }
                    case NodeResult.None(region) => {
                        throw new LanguageException(region, "Invalid postfix node")
                    }
                }
            }
        }
    }

    private def parseObject(node: Node): HighLevelExpression = {
        node.assertType("object")
        node("ifElse")
            .map(parseIfElse)
            .orElse(
              node("block").map(parseBlock)
            )
            .orElse(
              node("define").map(parseDefine)
            )
            .orElse(
              node("id").map(parseID)
            )
            .orElse(
              node("number").map(parseNumber)
            )
            .orElse(
              node("expression").map(parse)
            )
            .getOrElse(throw new LanguageException(node.region, "Invalid object node"))
    }

    private def parseCall(left: HighLevelExpression, node: Node): HighLevelExpression = {
        node.assertType("call")
        val expressions = node.map("expression", parse)
        HIRCall(node.region, left, expressions)
    }

    private def parseDot(left: HighLevelExpression, node: Node): HighLevelExpression = {
        HIRDot(node.region, left, node.id())
    }

    private def parseIfElse(node: Node): HighLevelExpression = {
        node.assertType("ifElse")
        val condition = parse(node("expression").expect("condition on ifElse node"))
        val thenBlock = parseBlock(node("block").expect("block on IfElse Node"))
        val elseBlock = node("else").map(ref => parseBlock(ref("block").expect("block on else part of IfElse Node")))
        HIRIfElse(
          node.region,
          condition,
          thenBlock,
          elseBlock
        )
    }

    private def parseBlock(node: Node): HighLevelExpression = {
        node.assertType("block")
        val expressions = node.map("expression", parse)
        HIRBlock(node.region, expressions)
    }

    private def parseDefine(node: Node): HighLevelExpression = {
        node.assertType("define")
        val varName = node.id()
        val varValue = parse(node("expression").expect("Expression on define node"))
        HIRVariableDefinition(node.region, varName, varValue)
    }

    private def parseID(node: Node): HighLevelExpression = {
        node.assertType("id")
        val name = node.id()
        name match {
            case "true"  => HIRBoolean(node.region, true)
            case "false" => HIRBoolean(node.region, false)
            case rest    => HIRVariable(node.region, rest)
        }
    }

    private def parseNumber(node: Node): HighLevelExpression = {
        node.assertType("number")
        val numberStr = node.number()
        numberStr.toDoubleOption match {
            case Some(value) => HIRNumber(node.region, value)
            case None        => throw new LanguageException(node.region, s"Invalid number format '${numberStr}'")
        }

    }
}
