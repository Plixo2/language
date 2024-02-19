package frontend.exceptions

import frontend.lexer.Region

class LanguageException(region: Region, msg: String) extends RuntimeException(msg + " at " + region.toString()) {

    def prettyString(): String = {
        var lines = List[String]()
        lines = lines :+ s"Error: $msg at ${region.toString}"
        lines = lines ++ prettyPrintRegion(region)
        region.UIRLocation match {
            case Some(value) =>{
                lines = lines :+ value
            }
            case None => {}
        }
        lines = lines :+ "\n"

        lines.mkString("\n")
    }
}

def prettyPrintRegion(region: Region): List[String] = {
    val left = region.from
    val right = region.to
    val loadedLines = region.file.lines().toList
    val minLine = Math.min(loadedLines.length - 1, left.line)
    val maxLine = Math.min(loadedLines.length - 1, right.line)
    if (minLine == maxLine && loadedLines.nonEmpty) {
        val line = loadedLines(minLine)
        val minChar = Math.min(left.column, right.column)
        val maxChar = Math.max(left.column, right.column)
        val dChar = Math.max(maxChar - minChar, 1)
        val paddingLeft = (" " * minChar) + "v" * dChar
        List(paddingLeft, line)
    } else {
        var lines = List[String]()
        lines = lines :+ ("v" * left.column)
        for (i <- minLine to maxLine) yield {
            if (i >= minLine && i <= maxLine) {
                val line = loadedLines(i)
                lines = lines :+ line
            }
        }
        lines = lines :+ ("^" * right.column)
        lines
    }
}
