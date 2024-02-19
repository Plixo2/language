package frontend.highlevel

import frontend.lexer.Region

case class HighLevelFunction(
    region: Region,
    name: String,
    parameters: List[Parameter],
    returnType: HighLevelType,
    expression: HighLevelExpression
) {}
