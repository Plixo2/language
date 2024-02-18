package frontend

import frontend.files.{File, fileFromResource}
import frontend.highlevel.parsing.HighLevelParser
import frontend.lexer.*
import frontend.parser.{Grammar, Parser, RuleSet, SyntaxResult}

class FrontEnd {
    private val grammarFile: File = fileFromResource("grammar.txt")
    private val grammarEntry: String = "entry"

    def parse(file: File): Unit = {
        val languageTokens = LanguageToken.allTokens()
        val grammar = loadGrammar(languageTokens)
        val tokenizer = Tokenizer(languageTokens)
        val entryRule = grammar.findRule(grammarEntry).get
        val expression = grammar.findRule("function").get.expression
        val parser = Parser(entryRule)
        val records = tokenize(file, tokenizer)
        val result = parser.parse(records)
        result match {
            case SyntaxResult.Match(elements) => {
                val entry = elements.head
                entry.assertType("entry")
                val functionNodes = entry.getAll("function")
                functionNodes.foreach(ref => {
                    println(ref)
                    val function = HighLevelParser.parseMethod(ref)
                    println(function)

                })
            }
            case SyntaxResult.NoMatch() => throw new RuntimeException("No match")
            case SyntaxResult.Failure() => throw new RuntimeException("Failure")
        }
    }

    private def tokenize(file: File, tokenizer: Tokenizer): List[TokenRecord] = {
        val records = tokenizer.tokenize(file, file.lines())
        records.filter(record => record.token != WhiteSpaceToken && record.token != CommentToken)
    }

    private def loadGrammar(tokens: List[Token]): RuleSet = {
        val grammar = Grammar(grammarFile)
        grammar.parse(tokens)
    }

}
