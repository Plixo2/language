package frontend
package parser

import frontend.exceptions.LanguageException
import frontend.files.File
import frontend.lexer.*

final class Grammar(file: File) {
    private var solveLater = List[RuleSet => Unit]()
    def parse(languageTokens: List[Token]): RuleSet = {
        solveLater = List()
        val tokens = this.tokens()
        val tokenizer = Tokenizer(tokens)
        var rules = List[Rule]()
        for ((line, index) <- file.lines().zipWithIndex) {
            val tokens = tokenizer.tokenizeLine(file, line, index)
            val filter =
                tokens.getOrThrow().filter(token => token.token != WhiteSpaceToken && token.token != CommentToken)
            if (filter.nonEmpty) {
                val rule = parseRule(ModifiableTokenStream(filter), languageTokens)
                if (rules.exists(foundRule => foundRule.name.equalsIgnoreCase(rule.name))) {
                    throw new LanguageException(filter.head.region, s"Rule ${rule.name.toLowerCase} already defined")
                }
                rules = rules :+ rule
            }
        }
        val ruleSet = RuleSet(rules)
        solveLater.foreach(_(ruleSet))
        solveLater = List()
        ruleSet
    }

    private def parseRule(stream: ModifiableTokenStream, tokens: List[Token]): Rule = {
        val name = stream.expectWord()
        stream.consume()
        stream.expectLiteral("::=")
        stream.consume()
        val expression = parseExpression(stream, tokens)
        Rule(name.toLowerCase, expression)
    }

    private def parseExpression(stream: ModifiableTokenStream, tokens: List[Token]): GrammarElement = {
        var options = List[GrammarElement]()
        var matchedOption = true
        while (matchedOption) {
            matchedOption = false
            var matchedList = true;
            var innerList = List[GrammarElement]()
            while (matchedList) {
                if (stream.isString) {
                    val tokenRecord = stream.current()
                    val stringedLiteral = tokenRecord.literal
                    assert(stringedLiteral.length >= 2)
                    val literalName = stringedLiteral.substring(1, stringedLiteral.length - 1)
                    stream.consume()
                    val foundToken = tokens.find(token => token.alias == literalName)
                    foundToken match {
                        case Some(value) => {
                            val literal = Literal(value)
                            innerList = innerList :+ applyPostfix(stream, literal)
                        }
                        case None => throw new LanguageException(tokenRecord.region, s"Token $literalName not found")
                    }
                } else if (stream.isWord) {
                    val tokenRecord = stream.current()
                    val word = stream.expectWord()
                    stream.consume()
                    val rule = RuleEntry(null)
                    solveLater = solveLater :+ (ruleSet => {
                        val found = ruleSet.findRule(word)
                        found match {
                            case Some(value) => rule.rule = value
                            case None        => throw new LanguageException(tokenRecord.region, s"Rule $word not found")
                        }
                    })
                    innerList = innerList :+ applyPostfix(stream, rule)
                } else if (stream.testLiteral("(")) {
                    stream.consume()
                    val subExpression = parseExpression(stream, tokens)
                    stream.expectLiteral(")")
                    stream.consume()
                    innerList = innerList :+ applyPostfix(stream, subExpression)
                } else {
                    matchedList = false;
                }
            }
            if (innerList.size == 1) {
                options = options :+ innerList.head
            } else {
                options = options :+ SequenceElement(innerList)
            }
            if (stream.testLiteral("|")) {
                stream.consume()
                matchedOption = true
            }
        }

        if (options.size == 1) {
            options.head
        } else {
            OptionsElement(options)
        }

    }

    private def applyPostfix(stream: ModifiableTokenStream, element: GrammarElement): GrammarElement = {
        if (stream.testLiteral("!")) {
            stream.consume()
            NecessaryElement(element)
        } else if (stream.testLiteral("?")) {
            stream.consume()
            MultiElement(Multiplicity.ZeroOrOne, element)
        } else if (stream.testLiteral("*")) {
            stream.consume()
            MultiElement(Multiplicity.ZeroOrMore, element)
        } else {
            element
        }
    }

    private def tokens(): List[Token] = {
        List(
          WhiteSpaceToken,
          StringToken,
          WordToken,
          CommentToken,
          CharToken('!'),
          CharToken('*'),
          CharToken('?'),
          CharToken('('),
          CharToken(')'),
          CharToken('|'),
          LiteralToken("::=")
        )
    }
}

sealed trait GrammarElement {}

class Rule(val name: String, var expression: GrammarElement) {
    override def toString: String = {
        s"Rule($name)"
    }
}

case class Literal(token: Token) extends GrammarElement

case class RuleEntry(var rule: Rule) extends GrammarElement

case class SequenceElement(expressions: List[GrammarElement]) extends GrammarElement

case class OptionsElement(elements: List[GrammarElement]) extends GrammarElement

case class MultiElement(count: Multiplicity, element: GrammarElement) extends GrammarElement

case class NecessaryElement(element: GrammarElement) extends GrammarElement

enum Multiplicity {
    case ZeroOrOne
    case ZeroOrMore
}

case class RuleSet(private val rules: List[Rule]) {
    def findRule(name: String): Option[Rule] = {
        rules.find(rule => rule.name.equalsIgnoreCase(name))
    }
}
