package se
package parser

import lexer.{Region, TokenRecord}

case class Node(name: String, region: Region, children: List[Node], tokenRecord: TokenRecord, isLiteral: Boolean) {

  def apply(name: String): Option[Node] = {
    children.find(_.name == name.toLowerCase)
  }
}
