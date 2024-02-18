package frontend
package parser

import frontend.lexer.TokenRecord
import frontend.parser.SyntaxResult.Match

import scala.annotation.tailrec

final class Parser(private val entryRule: Rule) {

    def parse(records: List[TokenRecord]): SyntaxResult = {
        val stream = ModifiableTokenStream(records)
        val result = testRule(entryRule, stream)
        result
    }

    private def testRule(rule: Rule, stream: ModifiableTokenStream): SyntaxResult = {
        if (!stream.hasLeft) {
            return SyntaxResult.NoMatch()
        }
        val startRegion = stream.current().region
        val result = testExpression(rule.expression, stream)
        val currentOrLast = if (stream.hasLeft) {
            stream.current()
        } else {
            stream(stream.index() - 1)
        }
        val endPosition = currentOrLast.region.to
        val region = startRegion.expandRight(endPosition)
        result match {
            case SyntaxResult.Match(elements) => {
                SyntaxResult.Match(List(Node(rule.name, region, elements, currentOrLast, false)))
            }
            case rest => rest
        }
    }

    private def testExpression(expression: GrammarElement, stream: ModifiableTokenStream): SyntaxResult = {
        expression match {
            case Literal(token) => {
                if (!stream.hasLeft) {
                    SyntaxResult.NoMatch()
                } else {
                    val record = stream.current()
                    if (record.token == token) {
                        stream.consume()
                        SyntaxResult.Match(List(Node(token.alias, record.region, List(), record, true)))
                    } else {
                        SyntaxResult.NoMatch()
                    }
                }
            }
            case RuleEntry(rule) => {
                val testedRule = testRule(rule, stream)
                testedRule
            }
            case SequenceElement(expressions) => {
                def testElements(elements: List[GrammarElement]): SyntaxResult = {
                    elements match {
                        case Nil => SyntaxResult.Match(List())
                        case head :: tail => {
                            val subExpr = testExpression(head, stream)
                            subExpr.and(() => testElements(tail))
                        }
                    }
                }
                testElements(expressions)
            }
            case OptionsElement(elements) => {
                @tailrec
                def testElements(elements: List[GrammarElement]): SyntaxResult = {
                    elements match {
                        case Nil => SyntaxResult.NoMatch()
                        case head :: tail => {
                            val tokenPosition = stream.index()
                            val subExpr = testExpression(head, stream)
                            subExpr match {
                                case SyntaxResult.Match(elements) => SyntaxResult.Match(elements)
                                case SyntaxResult.NoMatch() => {
                                    stream.reset(tokenPosition)
                                    testElements(tail)
                                }
                                case fail: SyntaxResult.Failure => fail
                            }
                        }
                    }
                }
                testElements(elements)
            }
            case MultiElement(count, element) => {
                count match {
                    case Multiplicity.ZeroOrOne => {
                        val pos = stream.index()
                        stream.reset(pos)
                        val subResult = testExpression(element, stream)
                        subResult match {
                            case SyntaxResult.Match(elements) => SyntaxResult.Match(elements)
                            case SyntaxResult.NoMatch()       => {
                                stream.reset(pos)
                                SyntaxResult.Match(List())
                            }
                            case fail: SyntaxResult.Failure   => fail
                        }
                    }
                    case Multiplicity.ZeroOrMore => {
                        var nodes = List[Node]()
                        var latest = SyntaxResult.Match(List());
                        // that's a do while loop
                        while ({
                            val pos = stream.index()
                            latest = testExpression(element, stream)
                            latest match {
                                case SyntaxResult.Match(elements) => {
                                    nodes = nodes ++ elements
                                    true
                                }
                                case SyntaxResult.NoMatch()     => {
                                    stream.reset(pos)
                                    false
                                }
                                case fail: SyntaxResult.Failure => false
                            }
                        }) ()
                        latest match {
                            case SyntaxResult.Match(elements) => SyntaxResult.Match(nodes)
                            case SyntaxResult.NoMatch()       => SyntaxResult.Match(nodes)
                            case fail: SyntaxResult.Failure   => fail
                        }
                    }
                }
            }
            case NecessaryElement(element) => {
                val syntaxResult = testExpression(element, stream)
                syntaxResult match {
                    case SyntaxResult.Match(elements) => syntaxResult
                    case SyntaxResult.NoMatch()       => SyntaxResult.Failure()
                    case fail: SyntaxResult.Failure   => fail
                }
            }
        }
    }

}

enum SyntaxResult {
    case Match(elements: List[Node])
    case NoMatch()

    case Failure()

    def and(other: () => SyntaxResult): SyntaxResult = {
        this match {
            case SyntaxResult.Match(elements) => {
                other.apply() match {
                    case SyntaxResult.Match(otherElements) => {
                        SyntaxResult.Match(elements ++ otherElements)
                    }
                    case rest => rest
                }
            }
            case rest => rest
        }
    }
}
