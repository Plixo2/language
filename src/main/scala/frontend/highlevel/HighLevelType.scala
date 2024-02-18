package frontend.highlevel

import frontend.lexer.Region

enum HighLevelType(region: Region) {
    case Class(region: Region, name: String) extends HighLevelType(region)
    case UNIT(region: Region) extends HighLevelType(region)
    case INT(region: Region) extends HighLevelType(region)
    case BOOL(region: Region) extends HighLevelType(region)
}
