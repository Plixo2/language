package frontend.highlevel

import frontend.lexer.Region

case class Parameter (region: Region, name: String, _type: HighLevelType)
