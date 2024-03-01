package frontend.highlevel.parsing

import frontend.exceptions.LanguageException
import frontend.highlevel.*
import frontend.lexer.*
import frontend.parser.{Node, NodeResult, expect}

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
        if (node("define").isDefined) {
            parseDefine(node("define").expect("define on expression node"))
        } else {
            val conditional = node("conditionalOrExpression")
                .expect("conditionalOrExpression on expression node, if no define node")
            parseBinaryNode(conditional, binaryFunctions)
        }
    }

    private val binaryFunctions: List[String] = List(
      "conditionalOrExpression",
      "conditionalAndExpression",
      "equalityExpression",
      "relationalExpression",
      "additiveExpression",
      "multiplicativeExpression",
      "unaryExpression"
    )
    private def parseBinaryNode(node: Node, functions: List[String]): HighLevelExpression = {
        assert(functions.nonEmpty, "No functions left to parse")
        if (functions.size == 1) {
            return unaryExpression(node)
        }
        node.assertType(functions.head)
        val left = parseBinaryNode(node(functions(1)).expect("left on binary node"), functions.tail)
        if (node(functions.head).isDefined) {
            val functionNode = node.find(child => child.isLiteral).expect("function on binary node");
            val tokenType = functionNode.tokenRecord.token
            tokenType match {
                case OperatorToken(operator, subToken) => {
                    val right = parseBinaryNode(node(functions.head).expect("right on binary node"), functions)
                    HIRBinaryOperation(node.region, operator, left, right)
                }
                case _ =>
                    throw new LanguageException(functionNode.region, "Invalid token type, expected operator token")
            }
        } else {
            left
        }
    }

    private def unaryExpression(node: Node): HighLevelExpression = {
        node.assertType("unaryExpression")
        val factor = parseFactor(node("factor").expect("factor on unary"))
        node.find(child => child.isLiteral).map(_.tokenRecord.token) match {
            case Some(OperatorToken(operator, subToken)) => {
                HIRUnaryOperation(node.region, operator, factor)
            }
            case Some(rest) => {
                throw new LanguageException(node.region, s"Invalid token type, expected operator token, got ${rest}")
            }
            case None => factor
        }
    }

    private def parseFactor(node: Node): HighLevelExpression = {
        node.assertType("factor")
        val left = parseObject(node("object").expect("object on factor"))
        val postFixApplied = node.getAll("postfix").foldLeft(left)(parsePostFix)
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
        val arguments = node.map(
          "argument",
          argNode => {
              val value = parse(argNode("expression").expect("expression on argument node"))
              val nameOption = argNode("id").map(_.id())
              Argument(argNode.region, nameOption, value)
          }
        )
        HIRCall(node.region, left, arguments)
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
        if (expressions.size == 1) {
            expressions.head
        } else {
            HIRBlock(node.region, expressions)
        }
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
