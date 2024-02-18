package frontend
package lexer

import frontend.files.File

import scala.annotation.tailrec

case class Tokenizer(tokens: List[Token]) {

    def tokenize(file: File, lines: Iterator[String]): List[TokenRecord] = {
        lines.zipWithIndex.flatMap { case (line, index) =>
            tokenizeLine(file, line, index)
        }.toList
    }

    def tokenizeLine(file: File, line: String, lineIndex: Int): List[TokenRecord] = {

        @tailrec
        def findFirstMatch(line: String, index: Int, tokens: List[Token]): Option[(Token, Int)] = {
            tokens match {
                case Nil => None
                case token :: rest => {
                    token.matches(line, index) match {
                        case TokenMatch.Matched(next) => Some(token, next)
                        case TokenMatch.Unmatched()   => findFirstMatch(line, index, rest)
                    }
                }
            }
        }

        def nextRecord(foundToken: Option[(Token, Int)], startPosition: Position, position: Int): (TokenRecord, Int) = {
            foundToken match {
                case Some((token, nextIndex)) => {
                    val endPosition = Position(lineIndex, nextIndex)
                    val region = Region(file, startPosition, endPosition)
                    val literal = line.substring(position, Math.min(nextIndex, line.length))
                    (TokenRecord(token, literal, region), nextIndex);
                }
                case None => {
                    val literal = line.substring(position, Math.min(position + 1, line.length))
                    val endPosition = Position(lineIndex, position + 1)
                    val region = Region(file, startPosition, endPosition)
                    (TokenRecord(UnknownToken, literal, region), position + 1)
                }
            }
        }

        var records = List[TokenRecord]()
        var char = 0
        while (char < line.length) {
            val startPosition = Position(lineIndex, char);
            val test = findFirstMatch(line, char, tokens)
            val token = nextRecord(test, startPosition, char)
            if (char <= token._2) {
                // TODO: Error handling, bc tokenizer is stuck now :(
            }
            char = token._2
            records = records :+ token._1
        }
        records
    }

}
