package frontend.highlevel

import frontend.lexer.Region

sealed trait HighLevelExpression {}

case class HIRNumber(region: Region, double: Double) extends HighLevelExpression
case class HIRBlock(region: Region, expressions: List[HighLevelExpression]) extends HighLevelExpression
case class HIRVariable(region: Region, name: String) extends HighLevelExpression
case class HIRIfElse(
    region: Region,
    condition: HighLevelExpression,
    thenBlock: HighLevelExpression,
    elseBlock: Option[HighLevelExpression]
) extends HighLevelExpression

case class HIRVariableDefinition(region: Region, name: String, value: HighLevelExpression) extends HighLevelExpression

case class HIRBoolean(region: Region, value: Boolean) extends HighLevelExpression

case class HIRCall(region: Region, left: HighLevelExpression, arguments: List[Argument]) extends HighLevelExpression
case class HIRDot(region: Region, left: HighLevelExpression, name: String) extends HighLevelExpression

case class HIRBinaryOperation(
    region: Region,
    operator: Operators,
    left: HighLevelExpression,
    right: HighLevelExpression
) extends HighLevelExpression

case class HIRUnaryOperation(region: Region, operator: Operators, expression: HighLevelExpression)
    extends HighLevelExpression

case class Argument(region: Region, name: Option[String], value: HighLevelExpression)
