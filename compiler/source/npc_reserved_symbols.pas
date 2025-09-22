//
// Nitro Pascal Compiler
// version 1.0
//
// Reserved words
//

unit npc_reserved_symbols;

interface

uses
  npc_tokens;

type
  TNPCReservedSymbols = (
    rs_Unknown,
    rs_OParen,      // (
    rs_CParen,      // )
    rs_OCurly,      // {
    rs_CCurly,      // }
    rs_OBracket,    // [
    rs_CBracket,    // ]

    rs_Dot,         // .
    rs_Comma,       // ,
    rs_Colon,       // :
    rs_Semicolon,   // ;
    rs_Quote,       // '
    rs_DQuote,      // "

    rs_Tilda,       // ~
    rs_Exclamation, // !
    rs_At,          // @
    rs_Hash,        // #
    rs_Dollar,      // $
    rs_Percent,     // %
    rs_Dash,        // ^
    rs_Ampersand,   // &
    rs_Asterisk,    // *
    rs_Minus,       // -
    rs_Plus,        // +
    rs_Equal,       // =
    rs_LessThan,    // <
    rs_GreaterThan, // >
    rs_Div,         // /

    rs_CommentSL,    // singleline: //
    rs_CommentMLB,   // multiline-begin: /.
    rs_CommentMLE,   // multiline-end  : ./
    rs_CommentMLB1,  // multiline-begin: {.
    rs_CommentMLE1,  // multiline-end  : .}
    rs_CommentMLB2,  // multiline-begin: (*
    rs_CommentMLE2,  // multiline-end  : *)
    rs_DoubleDot,    // ..
    rs_Assign,       // :=
    rs_LessEqual,    // <=
    rs_GreaterEqual, // >=
    rs_NotEqual,     // <> or != - alias for '<>' or '!=': same as not (A = B) used like in C++: A != B
    rs_NotEqual1,    // <> see rs_NotEqual
    rs_NotEqual2,    // != see rs_NotEqual
    rs_DoublePlus,   // ++
    rs_DoubleMinus,  // --
    rs_PlusEqual,    // +=
    rs_MinusEqual,   // -=
    rs_MulEqual,     // *=
    rs_DivEqual,     // /=
    rs_ModEqual      // %=
  );

  TNPCReservedDoubleSymbols = (
    rds_CommentSL,    // singleline: //
    rds_CommentMLB,   // multiline-begin: /.
    rds_CommentMLE,   // multiline-end  : ./
    rds_CommentMLB1,  // multiline-begin: {.
    rds_CommentMLE1,  // multiline-end  : .}
    rds_CommentMLB2,  // multiline-begin: (*
    rds_CommentMLE2,  // multiline-end  : *)
    rds_DoubleDot,    // ..
    rds_Assign,       // :=
    rds_LessEqual,    // <=
    rds_GreaterEqual, // >=
    rds_NotEqual,     // != - alias for '<>': same as not (A = B) used like in C++: A != B
    rds_NotEqual1,    // != - alias for '<>': same as not (A = B) used like in C++: A != B
    rds_NotEqual2,    // != - alias for '<>': same as not (A = B) used like in C++: A != B
    rds_DoublePlus,   // ++
    rds_DoubleMinus,  // --
    rds_PlusEqual,    // +=
    rds_MinusEqual,   // -=
    rds_MulEqual,     // *=
    rds_DivEqual,     // /=
    rds_ModEqual      // %=
  );

function NPCReservedSymbolToString(const ReservedSymbols: TNPCReservedSymbols): String;// inline;
function TokenTypeToReservedSymbol(const TokenType: TNPCTokenType): TNPCReservedSymbols;

implementation

function NPCReservedSymbolToString(const ReservedSymbols: TNPCReservedSymbols): String;
begin
  Result := 'unknown';
  case ReservedSymbols of
    rs_OParen      : Result := '(';
    rs_CParen      : Result := ')';
    rs_OCurly      : Result := '{';
    rs_CCurly      : Result := '}';
    rs_OBracket    : Result := '[';
    rs_CBracket    : Result := ']';

    rs_Dot         : Result := '.';
    rs_Comma       : Result := ',';
    rs_Colon       : Result := ':';
    rs_Semicolon   : Result := ';';
    rs_Quote       : Result := '''';
    rs_DQuote      : Result := '"';

    rs_Tilda       : Result := '~';
    rs_Exclamation : Result := '!';
    rs_At          : Result := '@';
    rs_Hash        : Result := '#';
    rs_Dollar      : Result := '$';
    rs_Percent     : Result := '%';
    rs_Dash        : Result := '^';
    rs_Ampersand   : Result := '&';
    rs_Asterisk    : Result := '*';
    rs_Minus       : Result := '-';
    rs_Plus        : Result := '+';
    rs_Equal       : Result := '=';
    rs_LessThan    : Result := '<';
    rs_GreaterThan : Result := '>';
    rs_Div         : Result := '/';

    rs_CommentSL   : Result := '//';
    rs_CommentMLB  : Result := '/.';
    rs_CommentMLE  : Result := './';
    rs_CommentMLB1 : Result := '{.';
    rs_CommentMLE1 : Result := '.}';
    rs_CommentMLB2 : Result := '(*';
    rs_CommentMLE2 : Result := '*)';
    //
    rs_DoubleDot   : Result := '..';
    rs_Assign      : Result := ':=';
    rs_LessEqual   : Result := '<=';
    rs_GreaterEqual: Result := '>=';
    rs_NotEqual    : Result := '<> or !=';
    rs_NotEqual1   : Result := '<>';
    rs_NotEqual2   : Result := '!=';
    rs_DoublePlus  : Result := '++';
    rs_DoubleMinus : Result := '--';
    rs_PlusEqual   : Result := '+=';
    rs_MinusEqual  : Result := '-=';
    rs_MulEqual    : Result := '*=';
    rs_DivEqual    : Result := '/=';
    rs_ModEqual    : Result := '%=';
  end;
end;

function TokenTypeToReservedSymbol(const TokenType: TNPCTokenType): TNPCReservedSymbols;
begin
  Result := rs_Unknown;
  case TokenType of
    tokOParen       : Result := rs_OParen;       // (
    tokCParen       : Result := rs_CParen;       // )
    tokOCurly       : Result := rs_OCurly;       // {
    tokCCurly       : Result := rs_CCurly;       // }
    tokOBracket     : Result := rs_OBracket;     // [
    tokCBracket     : Result := rs_CBracket;     // ]

    tokDot          : Result := rs_Dot;          // .
    tokComma        : Result := rs_Comma;        // ,
    tokColon        : Result := rs_Colon;        // :
    tokSemicolon    : Result := rs_Semicolon;    // ;
    tokQuote        : Result := rs_Quote;        // '
    tokDQuote       : Result := rs_DQuote;       // "

    tokTilda        : Result := rs_Tilda;        // ~
    tokExclamation  : Result := rs_Exclamation;  // !
    tokAt           : Result := rs_At;           // @
    tokHash         : Result := rs_Hash;         // #
    tokDollar       : Result := rs_Dollar;       // $
    tokPercent      : Result := rs_Percent;      // %
    tokDash         : Result := rs_Dash;         // ^
    tokAmpersand    : Result := rs_Ampersand;    // &
    tokAsterisk     : Result := rs_Asterisk;     // *
    tokMinus        : Result := rs_Minus;        // -
    tokPlus         : Result := rs_Plus;         // +
    tokEqual        : Result := rs_Equal;        // =
    tokLessThan     : Result := rs_LessThan;     // <
    tokGreaterThan  : Result := rs_GreaterThan;  // >
    tokDiv          : Result := rs_Div;          // /

    tokCommentSL    : Result := rs_CommentSL;    // singleline: //
    tokCommentMLB   : Result := rs_CommentMLB;   // multiline-begin: /.
    tokCommentMLE   : Result := rs_CommentMLE;   // multiline-end  : ./
    tokCommentMLB1  : Result := rs_CommentMLB1;  // multiline-begin: {.
    tokCommentMLE1  : Result := rs_CommentMLE1;  // multiline-end  : .}
    tokCommentMLB2  : Result := rs_CommentMLB2;  // multiline-begin: (*
    tokCommentMLE2  : Result := rs_CommentMLE2;  // multiline-end  : *)
    tokDoubleDot    : Result := rs_DoubleDot;    // ..
    tokAssign       : Result := rs_Assign;       // :=
    tokLessEqual    : Result := rs_LessEqual;    // <=
    tokGreaterEqual : Result := rs_GreaterEqual; // >=
    tokNotEqual1    : Result := rs_NotEqual1;    // <>
    tokNotEqual2    : Result := rs_NotEqual2;    // !=
    tokDoublePlus   : Result := rs_DoublePlus;   // ++
    tokDoubleMinus  : Result := rs_DoubleMinus;  // --
    tokPlusEqual    : Result := rs_PlusEqual;    // +=
    tokMinusEqual   : Result := rs_MinusEqual;   // -=
    tokMulEqual     : Result := rs_MulEqual;     // *=
    tokDivEqual     : Result := rs_DivEqual;     // /=
    tokModEqual     : Result := rs_ModEqual;     // %=
  end;
end;

end.
