package se

import files.{VirtualFile, fileFromResource}
import lexer.{RegexToken, Tokenizer, WhiteSpaceToken, WordToken}

import se.parser.Grammar

@main
def main(): Unit = {
//  println("Hello world!")
//  val testFile = VirtualFile("hello world \n rest")
//  val tokens = List(WhiteSpaceToken, WordToken)
//  val tokenizer = Tokenizer(tokens)
//  val value = tokenizer.tokenize(testFile, testFile.lines())
//  value.filter(_.token != WhiteSpaceToken).foreach(ref => println(ref.literal))

  grammar()
}

def grammar(): Unit = {
  val testFile = fileFromResource("grammar.txt")
  val grammar = new Grammar(testFile)
  val grammarRules = grammar.parse()
  println(grammarRules)
}