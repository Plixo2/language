package se
package lexer

import files.File

case class Region(file: File, from: Position, to: Position) {
  
}

case class Position(line: Int, column: Int) {
  
}
