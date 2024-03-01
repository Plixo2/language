package frontend

import frontend.exceptions.LanguageException
import frontend.files.fileFromPath

@main
def main(): Unit = {
    val frontEnd = new FrontEnd()
    val file = fileFromPath("resources/test.redo")

    try {
        frontEnd.parse(file)
    } catch {
        case e: LanguageException => {
            e.printStackTrace(System.err)
        }
    }

}
