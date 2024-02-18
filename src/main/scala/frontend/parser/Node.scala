package frontend
package parser

import frontend.lexer.{Region, TokenRecord}

case class Node(name: String, region: Region, children: List[Node], tokenRecord: TokenRecord, isLiteral: Boolean) {

    def apply(name: String): NodeResult = {
        children.find(_.name == name.toLowerCase) match {
            case Some(value) => NodeResult.Some(value)
            case None        => NodeResult.None()
        }
    }

    def id(): String = {
        if (name.toLowerCase == "id") {
            apply("word").expect("Expected word on rule 'id'").tokenRecord.literal
        } else {
            apply("id").expect(s"token literal called id on ${this.name}").id()
        }
    }

    def map[T](name: String, f: Node => T): List[T] = {
        children.filter(_.name == name.toLowerCase).map(f)
    }

    def getAll(name: String): List[Node] = {
        children.filter(_.name == name.toLowerCase)
    }

    def assertType(name: String): Unit = {
        assert(
          name.toLowerCase == this.name.toLowerCase,
          s"Expected ${name.toLowerCase}, found ${this.name.toLowerCase}"
        )
    }

    override def toString: String = {
        val buffer = new StringBuilder(50)
        print(buffer, "", "")
        s"\n $buffer"
    }

    private def print(buffer: StringBuilder, prefix: String, childrenPrefix: String): Unit = {
        buffer.append(prefix)
        buffer.append(name)
        buffer.append(" ")
        if (isLiteral) {
            buffer.append("\"").append(tokenRecord.literal).append("\"")
        }
        buffer.append('\n')
        val it = children.iterator
        while (it.hasNext) {
            val next: Node = it.next
            if (it.hasNext) next.print(buffer, childrenPrefix + "├── ", childrenPrefix + "│   ")
            else next.print(buffer, childrenPrefix + "└── ", childrenPrefix + "    ")
        }
    }
}

enum NodeResult {
    case Some(node: Node)
    case None()

    def expect(msg: String): Node = {
        this match {
            case NodeResult.Some(node) => node
            case NodeResult.None()     => throw new Exception(msg)
        }
    }

    def map[T](f: Node => T): Option[T] = {
        this match {
            case NodeResult.Some(node) => Option.apply(f(node))
            case NodeResult.None()     => Option.empty
        }
    }
}
