package se
package parser

import lexer.TokenRecord

import scala.annotation.tailrec

final class Parser(private val rule: Rule) {

  def parse(records: List[TokenRecord]): Unit = {

  }

  def testRule(rule: Rule, stream: ModifiableTokenStream): SyntaxResult = {
    SyntaxResult.Failure
  }

  def testExpression(expression: GrammarElement, stream: ModifiableTokenStream): SyntaxResult = {
    var result: SyntaxResult =
      expression match {
        case Literal(token) => {
          val record = stream.current()
          if (record.token == token) {
            SyntaxResult.Match(List(Node(token.alias, record.region, List(), record, true)))
          } else {
            SyntaxResult.Failure
          }
        }
        case Rule(name, expression) => {
          val testedRule = testRule(rule, stream)
          testedRule
        }
        case ListElement(expressions) => {
          //TODO change this to foldRight, if this works
          @tailrec
          def testElements(elements: List[GrammarElement]): SyntaxResult = {
            elements match {
              case Nil => SyntaxResult.Match(List())
              case head :: tail => {
                val subExpr = testExpression(head, stream)
                subExpr match {
                  case SyntaxResult.Match(elements) => {
                    testElements(tail) match {
                      case SyntaxResult.Match(elements) => {
                        SyntaxResult.Match(elements)
                      }
                      case SyntaxResult.Failure => {
                        SyntaxResult.Failure
                      }
                    }
                  }
                  case SyntaxResult.Failure => {
                    SyntaxResult.Failure
                  }
                }
              }
            }
          }

          testElements(expressions)
        }
        case OptionsElement(elements) => {

        }
        case MultiElement(count, element) => ???
        case NecessaryElement(element) => {
          val syntaxResult = testExpression(element, stream)
          syntaxResult match {
            case SyntaxResult.Match(elements) => syntaxResult
            case SyntaxResult.Failure => throw new Exception("Necessary element failed")
          }
        }
      }
    SyntaxResult.Match(result)
  }

}

enum SyntaxResult {
  case Match(elements: List[Node])
  case Failure

  def allElements(): List[Node] = this match {
    case Match(elements) => elements
    case Failure => List()
  }
}
