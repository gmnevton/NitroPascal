unit npc_tokens;

interface

uses
  Types;

type
  TNPCTokenType = Char;

const
//  TNPCTokens = (
  tokEOF          = TNPCTokenType(0);

  tokOParen       = TNPCTokenType(128); // (
  tokCParen       = TNPCTokenType(129); // )
  tokOCurly       = TNPCTokenType(130); // {
  tokCCurly       = TNPCTokenType(131); // }
  tokOBracket     = TNPCTokenType(132); // [
  tokCBracket     = TNPCTokenType(133); // ]

  tokDot          = TNPCTokenType(134); // .
  tokComma        = TNPCTokenType(135); // ,
  tokColon        = TNPCTokenType(136); // :
  tokSemicolon    = TNPCTokenType(137); // ;
  tokQuote        = TNPCTokenType(138); // '
  tokDQuote       = TNPCTokenType(139); // "

  tokApostrof     = TNPCTokenType(140); // `
  tokTilda        = TNPCTokenType(141); // ~

  tokExclamation  = TNPCTokenType(142); // !
  tokAt           = TNPCTokenType(143); // @
  tokHash         = TNPCTokenType(144); // #
  tokDollar       = TNPCTokenType(145); // $
  tokPercent      = TNPCTokenType(146); // %
  tokDash         = TNPCTokenType(147); // ^
  tokAmpersand    = TNPCTokenType(148); // &
  tokAsterisk     = TNPCTokenType(149); // *
  tokUnderline    = TNPCTokenType(150); // _

  tokMinus        = TNPCTokenType(151); // -
  tokPlus         = TNPCTokenType(152); // +
  tokEqual        = TNPCTokenType(153); // =
  tokLessThan     = TNPCTokenType(154); // <
  tokGreaterThan  = TNPCTokenType(155); // >
  tokDiv          = TNPCTokenType(156); // /

  tokCommentSL    = TNPCTokenType(157); // singleline: //
  tokCommentMLB   = TNPCTokenType(158); // multiline-begin: /.
  tokCommentMLE   = TNPCTokenType(159); // multiline-end  : ./
  tokCommentMLB1  = TNPCTokenType(160); // multiline-begin: {.
  tokCommentMLE1  = TNPCTokenType(161); // multiline-end  : .}
  tokCommentMLB2  = TNPCTokenType(162); // multiline-begin: (*
  tokCommentMLE2  = TNPCTokenType(163); // multiline-end  : *)

  tokDoubleDot    = TNPCTokenType(164); // ..
  tokAssign       = TNPCTokenType(165); // :=
  tokLessEqual    = TNPCTokenType(166); // <=
  tokGreaterEqual = TNPCTokenType(167); // >=
  tokNotEqual     = TNPCTokenType(168); // <> - same as not (A = B)
  tokNotEqual1    = TNPCTokenType(169); // <> - same as not (A = B)
  tokNotEqual2    = TNPCTokenType(170); // != - alias for '<>': same as not (A = B) used like in C++: A != B
  tokDoublePlus   = TNPCTokenType(171); // ++
  tokDoubleMinus  = TNPCTokenType(172); // --
  tokPlusEqual    = TNPCTokenType(173); // +=
  tokMinusEqual   = TNPCTokenType(174); // -=
  tokMulEqual     = TNPCTokenType(175); // *=
  tokDivEqual     = TNPCTokenType(176); // /=
  tokModEqual     = TNPCTokenType(177); // %=

  tokIdent        = TNPCTokenType(178); // reserved word or reserved symbol or other name
  tokNumber       = TNPCTokenType(179); // ([$]/[0x]/[-])(0..9[_])[.](0..9[_])
  tokString       = TNPCTokenType(180); // 'text'
  tokChar         = TNPCTokenType(181); // #12345; #12_345; #$AB; #$AB_CD

  tokSetting      = TNPCTokenType(182); // {$...} or {$define ...} or {$(condition) ...}
  tokResource     = TNPCTokenType(183); // {$resources ...}
  tokSpecifier    = TNPCTokenType(184); // {@...}
  tokCompilerVar  = TNPCTokenType(185); // %...%

type
  TNPCLiteralToken = record
    Literal: Char;
    TokenType: TNPCTokenType;
  end;

  TNPCDoubleLiteral = String[2];
  TNPCDoubleLiteralToken = record
    DoubleLiteral: TNPCDoubleLiteral;
    TokenType: TNPCTokenType;
  end;

const
  NPCLiteralTokens: Array[0..22] of TNPCLiteralToken = (
    (Literal: '(';  TokenType: tokOParen),
    (Literal: ')';  TokenType: tokCParen),
    (Literal: '{';  TokenType: tokOCurly),
    (Literal: '}';  TokenType: tokCCurly),
    (Literal: '[';  TokenType: tokOBracket),
    (Literal: ']';  TokenType: tokCBracket),

    (Literal: '.';  TokenType: tokDot),
    (Literal: ',';  TokenType: tokComma),
    (Literal: ':';  TokenType: tokColon),
    (Literal: ';';  TokenType: tokSemicolon),
//    (Literal: ''''; TokenType: tokQuote),
//    (Literal: '"';  TokenType: tokDQuote)

    (Literal: '!';  TokenType: tokExclamation),
    (Literal: '@';  TokenType: tokAt),
//    (Literal: '#';  TokenType: tokHash),
    (Literal: '$';  TokenType: tokDollar),
    (Literal: '%';  TokenType: tokPercent),
    (Literal: '^';  TokenType: tokDash),
    (Literal: '&';  TokenType: tokAmpersand),
    (Literal: '*';  TokenType: tokAsterisk),
    (Literal: '-';  TokenType: tokMinus),
    (Literal: '+';  TokenType: tokPlus),
    (Literal: '=';  TokenType: tokEqual),
    (Literal: '<';  TokenType: tokLessThan),
    (Literal: '>';  TokenType: tokGreaterThan),
    (Literal: '/';  TokenType: tokDiv)
  );

  NPCDoubleLiteralTokens: Array[0..19] of TNPCDoubleLiteralToken = (
    (DoubleLiteral: '//';  TokenType: tokCommentSL),
    (DoubleLiteral: '/.';  TokenType: tokCommentMLB),
    (DoubleLiteral: './';  TokenType: tokCommentMLE),
    (DoubleLiteral: '{.';  TokenType: tokCommentMLB1),
    (DoubleLiteral: '.}';  TokenType: tokCommentMLE1),
    (DoubleLiteral: '(*';  TokenType: tokCommentMLB2),
    (DoubleLiteral: '*)';  TokenType: tokCommentMLE2),
    (DoubleLiteral: '..';  TokenType: tokDoubleDot),
    (DoubleLiteral: ':=';  TokenType: tokAssign),
    (DoubleLiteral: '<=';  TokenType: tokLessEqual),
    (DoubleLiteral: '>=';  TokenType: tokGreaterEqual),
    (DoubleLiteral: '<>';  TokenType: tokNotEqual1),
    (DoubleLiteral: '!=';  TokenType: tokNotEqual2),
    (DoubleLiteral: '++';  TokenType: tokDoublePlus),
    (DoubleLiteral: '--';  TokenType: tokDoubleMinus),
    (DoubleLiteral: '+=';  TokenType: tokPlusEqual),
    (DoubleLiteral: '-=';  TokenType: tokMinusEqual),
    (DoubleLiteral: '*=';  TokenType: tokMulEqual),
    (DoubleLiteral: '/=';  TokenType: tokDivEqual),
    (DoubleLiteral: '%=';  TokenType: tokModEqual)
  );

function NPCTokensTypeToString(const TokenType: TNPCTokenType): String; inline;
function ArrayOfTokenToArrayOfString(const Values: Array of TNPCTokenType): TStringDynArray;
function LiteralTokenToChar(const Token: TNPCTokenType): Char; inline;

implementation

function NPCTokensTypeToString(const TokenType: TNPCTokenType): String;
begin
  Result := 'unknown';
  if TokenType = #0 then
    Exit('end of file');

//  if (Ord(TokenType) > 31) and (Ord(TokenType) < 157) then
  if TokenType < tokCommentSL then
    Exit('literal');

  case TokenType of
    tokCommentSL   : Result := 'comment-sl';
    tokCommentMLB  : Result := 'comment-mlb';
    tokCommentMLE  : Result := 'comment-mle';
    tokCommentMLB1 : Result := 'comment-mlb1';
    tokCommentMLE1 : Result := 'comment-mle1';
    tokCommentMLB2 : Result := 'comment-mlb2';
    tokCommentMLE2 : Result := 'comment-mle2';

    tokDoubleDot   : Result := 'set/array';
    tokAssign      : Result := 'assign';
    tokLessEqual   : Result := 'less-equal';
    tokGreaterEqual: Result := 'greater-equal';
    tokNotEqual1   : Result := 'not-equal';
    tokNotEqual2   : Result := 'not-equal';
    tokDoublePlus  : Result := 'plus-plus';
    tokDoubleMinus : Result := 'minus-minus';
    tokPlusEqual   : Result := 'plus-equal';
    tokMinusEqual  : Result := 'minus-equal';
    tokMulEqual    : Result := 'multiply-equal';
    tokDivEqual    : Result := 'divide-equal';
    tokModEqual    : Result := 'modulo-equal';

    tokIdent       : Result := 'identifier';
    tokNumber      : Result := 'number';
    tokString      : Result := 'string';
    tokChar        : Result := 'char';

    tokSetting     : Result := 'setting';
    tokResource    : Result := 'resource';
    tokSpecifier   : Result := 'specifier';
    tokCompilerVar : Result := 'compiler-variable';
  end;
end;

function ArrayOfTokenToArrayOfString(const Values: Array of TNPCTokenType): TStringDynArray;
var
  i: Integer;
  lit: TNPCLiteralToken;
begin
  SetLength(Result, Length(Values));
  for i:=0 to High(Values) do begin
    for lit in NPCLiteralTokens do begin
      if lit.TokenType = Values[i] then
        Result[i] := lit.Literal;
    end;
  end;
end;

function LiteralTokenToChar(const Token: TNPCTokenType): Char;
var
  lit: TNPCLiteralToken;
begin
  Result := #0; // empty
  for lit in NPCLiteralTokens do begin
    if lit.TokenType = Token then begin
      Result := lit.Literal;
      Exit;
    end;
  end;
end;

end.

