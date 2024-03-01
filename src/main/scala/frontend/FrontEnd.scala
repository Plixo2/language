package frontend

import frontend.exceptions.LanguageException
import frontend.files.{File, fileFromResource}
import frontend.highlevel.parsing.HighLevelParser
import frontend.lexer.*
import frontend.parser.*

class FrontEnd {
    private val grammarFile: File = fileFromResource("grammar.txt")
    private val grammarEntry: String = "entry"

    def parse(file: File): Unit = {
        val languageTokens = LanguageToken.allTokens()
        val grammar = loadGrammar(languageTokens)
        val tokenizer = Tokenizer(languageTokens)
        val entryRule = grammar.findRule(grammarEntry).expect(s"Rule '$grammarEntry'")
        val parser = Parser(entryRule)
        val records = tokenize(file, tokenizer)
        val result = parser.parse(records)
        result match {
            case RuleResult.RuleMatch(entry) => {
                entry.assertType("entry")
                val functionNodes = entry.getAll("function")
                functionNodes.foreach(ref => {
                    val function = HighLevelParser.parseMethod(ref)
                    println(function)
                    println(function.expression)
                })
            }
            case RuleResult.RuleNoMatch() => {
                throw new LanguageException(file.startRegion(), s"No match for Rule '$grammarEntry'")
            }
            case RuleResult.RuleFailure(region, rule, element) =>
                throw new LanguageException(
                  region,
                  s"Failure of Element ${element} in Rule '$rule' \n${element.region.prettyString()}\nin Source"
                )
        }
    }

    private def tokenize(file: File, tokenizer: Tokenizer): List[TokenRecord] = {
        val records = tokenizer.tokenize(file, file.lines()).getOrThrow()
        records.filter(record => record.token != WhiteSpaceToken && record.token != CommentToken)
    }

    private def loadGrammar(tokens: List[Token]): RuleSet = {
        val grammar = Grammar(grammarFile)
        grammar.parse(tokens)
    }

}
