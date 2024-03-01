package frontend

import frontend.highlevel.Operators
import frontend.lexer.*

object LanguageToken {

    // this list has to be ordered
    def allTokens(): List[Token] = {
        List(
          WhiteSpaceToken,
          CommentToken,
          StringToken,
          LiteralToken("->"),
          LiteralToken("fn"),
          LiteralToken("let"),
          LiteralToken("if"),
          LiteralToken("else"),
          OperatorToken(Operators.OR, LiteralToken("||")),
          OperatorToken(Operators.AND, LiteralToken("&&")),
          OperatorToken(Operators.GREATER_OR_EQUAL, LiteralToken(">=")),
          OperatorToken(Operators.LESS_OR_EQUAL, LiteralToken("<=")),
          OperatorToken(Operators.NOT_EQUAL, LiteralToken("!=")),
          OperatorToken(Operators.EQUAL, LiteralToken("==")),
          OperatorToken(Operators.ADDITION, CharToken('+')),
          OperatorToken(Operators.SUBTRACTION, CharToken('-')),
          OperatorToken(Operators.DIVISION, CharToken('/')),
          OperatorToken(Operators.MULTIPLICATION, CharToken('*')),
          OperatorToken(Operators.MODULO, CharToken('%')),
          OperatorToken(Operators.GREATER, CharToken('>')),
          OperatorToken(Operators.LESS, CharToken('<')),
          OperatorToken(Operators.NOT, CharToken('!')),
          CharToken(':'),
          CharToken('='),
          CharToken('('),
          CharToken(')'),
          CharToken('{'),
          CharToken('}'),
          CharToken(','),
          CharToken('.'),
          WordToken,
          RegexToken("number", "[0-9]+\\.?[0-9]*".r)
        )
    }
}
