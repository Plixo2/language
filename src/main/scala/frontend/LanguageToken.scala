package frontend

import frontend.lexer.{CharToken, CommentToken, LiteralToken, RegexToken, StringToken, Token, WhiteSpaceToken, WordToken}

object LanguageToken {

    //this list has to be ordered
    def allTokens() : List[Token] = {
        List(
          WhiteSpaceToken,
          CommentToken,
          StringToken,
          LiteralToken("->"),
          LiteralToken("fn"),
          LiteralToken("let"),
          CharToken(':'),
          CharToken('='),
          CharToken('('),
          CharToken(')'),
          CharToken('{'),
          CharToken('}'),
          CharToken(','),
          WordToken,
          RegexToken("number", "[0-9]+\\.?[0-9]*".r),
        )
    }
}
