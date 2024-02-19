package frontend
package lexer

import frontend.files.File

case class Region(file: File, from: Position, to: Position) {

    def expandRight(to: Position): Region = Region(file, from, to)

    override def toString: String = {
        s"$from -> $to"
    }

    def UIRLocation: Option[String] = {
        file.URIString() match {
            case Some(value) => {
                Some(s"in: $value:${(from.line + 1)}:${from.column + 1}")
            }
            case None => None
        }
    }
}

case class Position(line: Int, column: Int) {

    def expandRight(add: Int): Position = Position(line, column + add)

    override def toString: String = {
        s"($line:$column)"
    }

}
