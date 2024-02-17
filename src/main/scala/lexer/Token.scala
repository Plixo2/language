package se
package lexer

import java.util.regex.Pattern

case class TokenRecord(token: Token, literal: String, region: Region) {

}

sealed trait Token {
  def alias: String

  def matches(content: String, index: Int): TokenMatch
}

enum TokenMatch {
  case Matched(next: Int)
  case Unmatched()

  def hasMatched: Boolean = this match {
    case Matched(_) => true
    case Unmatched() => false
  }
}

object UnknownToken extends Token() {
  def alias: String = "unknown"

  def matches(content: String, index: Int): TokenMatch = TokenMatch.Unmatched()
}

object WhiteSpaceToken extends Token() {
  def alias: String = "whitespace"

  def matches(content: String, index: Int): TokenMatch = {
    if (content(index).isWhitespace) {
      TokenMatch.Matched(index + 1)
    }
    else {
      TokenMatch.Unmatched()
    }
  }
}

case class RegexToken(alias: String, regex: String) extends Token {
  private val pattern = Pattern.compile(regex)
  def matches(content: String, index: Int): TokenMatch = {
    val matcher = pattern.matcher(content)
    matcher.region(index, content.length)
    if (matcher.lookingAt()) {
      TokenMatch.Matched(matcher.end())
    }
    else {
      TokenMatch.Unmatched()
    }
  }
}

object WordToken extends Token {
  def alias: String = "word"
  def matches(content: String, index: Int): TokenMatch = {
    if (content(index).isLetter) {
      var i = index + 1
      while (i < content.length && content(i).isLetter) {
        i += 1
      }
      TokenMatch.Matched(i)
    }
    else {
      TokenMatch.Unmatched()
    }
  }
}

case class LiteralToken(literal: String) extends Token {
  def alias: String = literal
  def matches(content: String, index: Int): TokenMatch = {
    if (content.regionMatches(index, literal, 0, literal.length)) {
      TokenMatch.Matched(index + literal.length)
    }
    else {
      TokenMatch.Unmatched()
    }
  }
}

case class CharToken(char: Char) extends Token {
  def alias: String = char.toString
  def matches(content: String, index: Int): TokenMatch = {
    if (content(index) == char) {
      TokenMatch.Matched(index + 1)
    }
    else {
      TokenMatch.Unmatched()
    }
  }
}

object StringToken extends Token {
  def alias: String = "string"
  def matches(content: String, index: Int): TokenMatch = {
    if (content(index) == '"') {
      var i = index + 1
      while (i < content.length && content(i) != '"') {
        i += 1
      }
      if (i < content.length) {
        TokenMatch.Matched(i + 1)
      }
      else {
        TokenMatch.Unmatched()
      }
    }
    else {
      TokenMatch.Unmatched()
    }
  }
}

object CommentToken extends Token {
  def alias: String = "comment"
  def matches(content: String, index: Int): TokenMatch = {
    if (content.regionMatches(index, "//", 0, 2)) {
      TokenMatch.Matched(content.length)
    }
    else {
      TokenMatch.Unmatched()
    }
  }
}
