package se
package parser

import files.File
import lexer.*

final class Grammar(file: File) {

  def parse(): List[Rule] = {
    val tokens = this.tokens()
    val tokenizer = Tokenizer(tokens)
    var rules = List[Rule]()
    for ((line, index) <- file.lines().zipWithIndex) {
      val tokens = tokenizer.tokenizeLine(file, line, index)
      val filter = tokens.filter(token => token.token != WhiteSpaceToken && token.token != CommentToken)
      if (filter.nonEmpty) {
        rules = rules :+ parseRule(ModifiableTokenStream(filter))
      }
    }
    rules
  }

  private def parseRule(stream: ModifiableTokenStream): Rule = {
    val name = stream.expectWord()
    stream.consume()
    stream.expectLiteral("::=")
    stream.consume()
    val expression = parseExpression(stream)
    println(stream.index())
    println(stream.tokenLeft)
    Rule(name, expression)
  }

  private def parseExpression(stream: ModifiableTokenStream): GrammarElement = {
    var options = List[GrammarElement]()
    var matchedOption = true
    while (matchedOption) {
      matchedOption = false
      var matchedList = true;
      var innerList = List[GrammarElement]()
      while (matchedList) {
        matchedList = false
        if (stream.isString) {
          var word = stream.current().literal
          stream.consume()
          val literal = Literal(null)
          innerList = innerList :+ applyPostfix(stream, literal)
        } else if (stream.isWord) {
          val word = stream.expectWord()
          stream.consume()
          val rule = Rule(word, null)
          innerList = innerList :+ applyPostfix(stream, rule)
          matchedList = true
        } else if (stream.testLiteral("(")) {
          stream.consume()
          val subExpression = parseExpression(stream)
          stream.expectLiteral(")")
          stream.consume()
          innerList = innerList :+ applyPostfix(stream, subExpression)
          matchedList = true;
        }
      }
      if (innerList.size == 1) {
        options = options :+ innerList.head
      } else {
        options = options :+ ListElement(innerList)
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
      LiteralToken("::="),
    )
  }
}

sealed trait GrammarElement {

}

case class Literal(token: Token) extends GrammarElement

case class Rule(name: String, expression: GrammarElement) extends GrammarElement

case class ListElement(expressions: List[GrammarElement]) extends GrammarElement

case class OptionsElement(elements: List[GrammarElement]) extends GrammarElement

case class MultiElement(count: Multiplicity, element: GrammarElement) extends GrammarElement

case class NecessaryElement(element: GrammarElement) extends GrammarElement


enum Multiplicity {
  case ZeroOrOne
  case ZeroOrMore
}

