package frontend

import frontend.exceptions.LanguageException
import frontend.files.VirtualFile

@main
def main(): Unit = {
    val frontEnd = new FrontEnd()
    val content =
        """
          |fn main(s: String, w: int, ww: bool) -> int {
          |     a().id
          |}
          |""".stripMargin

    try {
        frontEnd.parse(VirtualFile(content, "test file"))
    } catch {
        case e: LanguageException => {
            System.err.println(e.prettyString())
            System.err.println("\n")
            e.printStackTrace(System.err)
        }
    }

}
