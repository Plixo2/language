package frontend
package lexer

import frontend.files.File

case class Region(file: File, from: Position, to: Position) {

    def expandRight(to: Position): Region = Region(file, from, to)

    override def toString: String = {
        s"$from -> $to"
    }
}

case class Position(line: Int, column: Int) {
    override def toString: String = {
        s"($line:$column)"
    }
}
