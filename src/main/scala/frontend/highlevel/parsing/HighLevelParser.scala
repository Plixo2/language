package frontend.highlevel.parsing

import frontend.highlevel.{HighLevelFunction, HighLevelType, Parameter}
import frontend.parser.Node

object HighLevelParser {
    def parseMethod(node: Node): HighLevelFunction = {
        node.assertType("function")
        val name = node.id()
        val parameters = node.map("parameter", node => parseParameter(node))
        val returnType = node("type")
            .map(HighLevelTypeParser.parseType)
            .getOrElse(HighLevelType.UNIT(node.region))
        HighLevelFunction(node.region, name, parameters, returnType)
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

object HighLevelExpressionParser {}
