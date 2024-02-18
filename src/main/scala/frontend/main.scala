package frontend

import frontend.files.{VirtualFile, fileFromResource}
import frontend.lexer.{Tokenizer, WhiteSpaceToken, WordToken}
import frontend.parser.{Grammar, Parser, SyntaxResult}

@main
def main(): Unit = {

    val frontEnd = new FrontEnd()
    val content =
        """
          |fn main(s: String, w: int, ww: bool) -> int {
          |     let x = 1
          |     let y = 2
          |     let sub = {
          |         let z = x
          |     }
          |     x
          |}
          |""".stripMargin

    frontEnd.parse(VirtualFile(
        content))
}
