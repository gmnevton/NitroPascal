//
// Nitro Pascal Compiler
// version 1.0
//
// Lexer
//

unit npc_lexer;

interface

uses
  SysUtils,
  Classes,
  npc_md5,
  npc_location,
  npc_error;

type
  TNPCTokenType = Char;

const
  tokEOF         = TNPCTokenType(0);

  tokOParen      = TNPCTokenType(1);  // (
  tokCParen      = TNPCTokenType(2);  // )
  tokOCurly      = TNPCTokenType(3);  // {
  tokCCurly      = TNPCTokenType(4);  // }
  tokOBracket    = TNPCTokenType(5);  // [
  tokCBracket    = TNPCTokenType(6);  // ]

  tokDot         = TNPCTokenType(7);  // .
  tokComma       = TNPCTokenType(8);  // ,
  tokColon       = TNPCTokenType(9);  // :
  tokSemicolon   = TNPCTokenType(10); // ;
  tokQuote       = TNPCTokenType(11); // '
  tokDQuote      = TNPCTokenType(12); // "

  tokAt          = TNPCTokenType(13); // @
  tokDollar      = TNPCTokenType(14); // $
  tokPercent     = TNPCTokenType(15); // %
  tokDash        = TNPCTokenType(16); // ^
  tokAmpersand   = TNPCTokenType(17); // &
  tokAsterisk    = TNPCTokenType(18); // *
  tokMinus       = TNPCTokenType(19); // -
  tokPlus        = TNPCTokenType(20); // +
  tokEqual       = TNPCTokenType(21); // =
  tokLess        = TNPCTokenType(22); // <
  tokMore        = TNPCTokenType(23); // >
  tokDiv         = TNPCTokenType(24); // /

  tokIdent       = TNPCTokenType(25); // reserved word or reserved symbol or other name
  tokNumber      = TNPCTokenType(26); // ([$]/[0x]/[-])(0..9[_])[.](0..9[_])
  tokString      = TNPCTokenType(27); // 'text'
  tokCommentSL   = TNPCTokenType(28); // singleline: //
  tokCommentMLB  = TNPCTokenType(29); // multiline-begin: {. (*
  tokCommentMLE  = TNPCTokenType(30); // multiline-end  : .} *)
  tokAssign      = TNPCTokenType(31); // :=


  tokSetting     = TNPCTokenType(32); // {$...} or {$define ...} or {$(condition) ...}
  tokResource    = TNPCTokenType(33); // {$resources ...}
  tokSpecifier   = TNPCTokenType(34); // {@...}
  tokCompilerVar = TNPCTokenType(35); // %...%

  tokMAX = 36;

  NPCTokensType: Array[0..tokMAX - 1] of String = (
    'end of file',
    'literal',
    'literal',
    'literal',
    'literal',
    'literal',
    'literal',
    'literal',
    'literal',
    'literal',
    'literal',
    'literal',
    'literal',
    'literal',
    'literal',
    'literal',
    'literal',
    'literal',
    'literal',
    'literal',
    'literal',
    'literal',
    'literal',
    'literal',
    'literal',
    'identifier',
    'number',
    'string',
    'comment-sl',
    'comment-mlb',
    'comment-mle',
    'assign',
    'setting',
    'resource',
    'specifier',
    'compiler-variable'
  );

type
  TNPCLiteralToken = record
    Literal: Char;
    TokenType: TNPCTokenType;
  end;

const
  NPCLiteralTokens: Array[0..21] of TNPCLiteralToken = (
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

    (Literal: '@';  TokenType: tokAt),
    (Literal: '$';  TokenType: tokDollar),
    (Literal: '%';  TokenType: tokPercent),
    (Literal: '^';  TokenType: tokDash),
    (Literal: '&';  TokenType: tokAmpersand),
    (Literal: '*';  TokenType: tokAsterisk),
    (Literal: '-';  TokenType: tokMinus),
    (Literal: '+';  TokenType: tokPlus),
    (Literal: '=';  TokenType: tokEqual),
    (Literal: '<';  TokenType: tokLess),
    (Literal: '>';  TokenType: tokMore),
    (Literal: '/';  TokenType: tokDiv)
  );

type
  TNPCToken = class
  public
    &Type: TNPCTokenType;
    Location: TNPCLocation;
    ReservedWord: Boolean;
    ReservedSymbol: Boolean;
//    i32Value: Integer;
//    i64Value: Int64;
//    f32Value: Single;
//    f64Value: Double;
//    number_flags: Word; // 16 bits
    Value: String;
    ValueHash: TNPCMD5;
    //
    constructor Create(const AType: TNPCTokenType; const ALocation: TNPCLocation; const AReservedWord, AReservedSymbol: Boolean; const AValue: String; const AValueHash: TNPCMD5);
    destructor Destroy; override;
  end;

  TNPCTokens = Array of TNPCToken;

  NPCLexerException = class(TNPCError);

  TNPCLexer = class
  private
    FFileName: String;
    FFormatSettings: TFormatSettings;
    FEncoding: TEncoding;
    //
    FSource: TBytes;
    PSource: PByte;
    FLines: Integer;
    //FTokens: TBytes;
    //
    cur: PByte;
    bol: PByte;
    row: Integer;
    //
  protected
    procedure SkipPreamble; inline;
    procedure SkipWhitespace; inline;
    procedure SkipLine; inline;
    function GetChar: Char; overload; inline; // MBCS implementation
    function GetChar(const Index: Integer): Char; overload; inline; // MBCS implementation
    function NextChar: Char; inline;
    procedure EatChar; inline;
    function Location: TNPCLocation; inline;
    function TokenString(const AStart, ACur: PByte): String; inline;
    function TokenMD5(const AValue: String): TNPCMD5; inline;
    function TokenIsReserved(const AMD5: TNPCMD5): Boolean; inline;
  public
    constructor Create(const AFileName: String); overload;
    constructor Create(const AFileName: String; const AEncoding: TEncoding); overload;
    constructor Create(const AFileName: String; const AFormatSettings: TFormatSettings); overload;
    constructor Create(const AFileName: String; const AFormatSettings: TFormatSettings; const AEncoding: TEncoding); overload;
    constructor Create(const AStream: TStringStream; const AFileName: String); overload;
    constructor Create(const AStream: TStringStream; const AFileName: String; const AEncoding: TEncoding); overload;
    constructor Create(const AStream: TStringStream; const AFileName: String; const AFormatSettings: TFormatSettings); overload;
    constructor Create(const AStream: TStringStream; const AFileName: String; const AFormatSettings: TFormatSettings; const AEncoding: TEncoding); overload;
    constructor Create(const AStream: TMemoryStream; const AFormatSettings: TFormatSettings; const AEncoding: TEncoding); overload;
    constructor Create(const AStream: TStringStream; const AFormatSettings: TFormatSettings; const AEncoding: TEncoding); overload;
    destructor Destroy; override;
    //
    function IsNotEmpty: Boolean; inline;
    function IsEmpty: Boolean; inline;
    function IsCurrentSymbol(const AValue: TNPCTokenType): Boolean; inline;
    function IsNextSymbol(const AValue: TNPCTokenType): Boolean; inline;
    function NextToken: TNPCToken;
    function ExpectToken(const ATokens: Array of TNPCTokenType): TNPCToken;
    //
    function Lines: Integer;
  end;

implementation

uses
  Types,
  Character,
  Hash,
  npc_consts,
  npc_reserved_words;

function ArrayOfTokenToArrayOfString(const Values: Array of Char): TStringDynArray;
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

function LiteralTokenToChar(const Token: Char): Char;
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

{ TNPCToken }

constructor TNPCToken.Create(const AType: TNPCTokenType; const ALocation: TNPCLocation; const AReservedWord, AReservedSymbol: Boolean; const AValue: String; const AValueHash: TNPCMD5);
begin
  &Type := AType;
  Location := ALocation;
  ReservedWord := AReservedWord;
  ReservedSymbol := AReservedSymbol;
  Value := AValue;
  ValueHash := AValueHash;
end;

destructor TNPCToken.Destroy;
begin
  &Type := #0;
  Location.Free;
  Value := '';
  inherited;
end;

{ TNPCLexer }

constructor TNPCLexer.Create(const AFileName: String);
begin
  Create(AFileName, FormatSettings);
end;

constructor TNPCLexer.Create(const AFileName: String; const AEncoding: TEncoding);
begin
  Create(AFileName, FormatSettings, TEncoding.UTF8);
end;

constructor TNPCLexer.Create(const AFileName: String; const AFormatSettings: TFormatSettings);
begin
  Create(AFileName, AFormatSettings, TEncoding.UTF8);
end;

constructor TNPCLexer.Create(const AFileName: String; const AFormatSettings: TFormatSettings; const AEncoding: TEncoding);
var
  stream: TFileStream;
  reader: TStringStream;
begin
  reader := TStringStream.Create('', AEncoding);
  try
    stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      reader.LoadFromStream(stream);
      reader.Position := 0;
    finally
      stream.Free;
    end;
    //
    Create(reader, AFileName, AFormatSettings, AEncoding);
  finally
    reader.Free;
  end;
end;

constructor TNPCLexer.Create(const AStream: TStringStream; const AFileName: String);
begin
  Create(AStream, AFileName, FormatSettings, TEncoding.UTF8);
end;

constructor TNPCLexer.Create(const AStream: TStringStream; const AFileName: String; const AEncoding: TEncoding);
begin
  Create(AStream, AFileName, FormatSettings, AEncoding);
end;

constructor TNPCLexer.Create(const AStream: TStringStream; const AFileName: String; const AFormatSettings: TFormatSettings);
begin
  Create(AStream, AFileName, AFormatSettings, TEncoding.UTF8);
end;

constructor TNPCLexer.Create(const AStream: TStringStream; const AFileName: String; const AFormatSettings: TFormatSettings; const AEncoding: TEncoding);
begin
  FFileName := AFileName;
  FFormatSettings := AFormatSettings;
  if AEncoding = Nil then
    FEncoding := TEncoding.UTF8
  else
    FEncoding := AEncoding;
  //
  if AStream = Nil then
    raise Exception.CreateFmt(sLexerStreamNotSpecified, [FFileName]);
  //
  FSource := Copy(AStream.Bytes, 0, AStream.Size);
  PSource := PByte(FSource);
  FLines := 1;
//  SetLength(FTokens, 0);
  cur := PSource;
  bol := PSource;
  row := 1;
  //
  SkipPreamble;
end;

constructor TNPCLexer.Create(const AStream: TMemoryStream; const AFormatSettings: TFormatSettings; const AEncoding: TEncoding);
var
  reader: TStringStream;
begin
  reader := TStringStream.Create('', AEncoding);
  try
    reader.LoadFromStream(AStream);
    reader.Position := 0;
    AStream.Clear;
    //
    Create(reader, AFormatSettings, AEncoding);
  finally
    reader.Free;
  end;
end;

constructor TNPCLexer.Create(const AStream: TStringStream; const AFormatSettings: TFormatSettings; const AEncoding: TEncoding);
begin
  FFileName := '';
  FFormatSettings := AFormatSettings;
  if AEncoding = Nil then
    FEncoding := TEncoding.UTF8
  else
    FEncoding := AEncoding;
  //
  if AStream = Nil then
    raise Exception.Create(sLexerStreamNotSpecifiedStream);
  //
  FSource := Copy(AStream.Bytes, 0, AStream.Size);
  PSource := PByte(FSource);
  FLines := 1;
//  SetLength(FTokens, 0);
  cur := PSource;
  bol := PSource;
  row := 1;
  //
  SkipPreamble;
end;

destructor TNPCLexer.Destroy;
begin
  FFileName := '';
  SetLength(FSource, 0);
  PSource := Nil;
//  SetLength(FTokens, 0);
//  FEncoding.Free;
  inherited;
end;

function TNPCLexer.IsNotEmpty: Boolean;
begin
  Result := (cur <> Nil) and (cur^ <> 0);
end;

function TNPCLexer.IsEmpty: Boolean;
begin
  Result := not IsNotEmpty;
end;

function TNPCLexer.IsCurrentSymbol(const AValue: TNPCTokenType): Boolean;
begin
  Result := Boolean(GetChar = AValue);
end;

function TNPCLexer.IsNextSymbol(const AValue: TNPCTokenType): Boolean;
begin
  Result := Boolean(NextChar = AValue);
end;

procedure TNPCLexer.SkipPreamble;
var
  i, l: Byte;
begin
  if IsEmpty then
    Exit;
  l := Length(FEncoding.GetPreamble);
  for i:=0 to l - 1 do begin
    if IsNotEmpty and (cur^ = FEncoding.GetPreamble[i]) then
      Inc(cur);
  end;
  bol := cur;
end;

procedure TNPCLexer.SkipWhitespace;
begin
  while IsNotEmpty and (cur^ in [32, 9, 11, 12, $A0]) do // SPACE, TAB, LINE-TAB, FormFeed, NBSP
    Inc(cur);
end;

procedure TNPCLexer.SkipLine;
begin
  while IsNotEmpty and not (cur^ in [13, 10]) do // CR, LF
    Inc(cur);

  if IsNotEmpty and (cur^ = 13) then
    Inc(cur);
  if IsNotEmpty and (cur^ = 10) then
    Inc(cur);

  Inc(row);
  Inc(FLines);
  bol := cur;
end;

function TNPCLexer.GetChar: Char;
begin
  if cur^ < 128 then begin
    Result := Char(cur^);
    Exit;
  end;
  //
  Result := FEncoding.GetString(FSource, cur - PSource, 1)[1];
end;

function TNPCLexer.GetChar(const Index: Integer): Char;
var
  x: PByte;
begin
  x := PSource;
  Inc(x, Index);
  if x^ < 128 then begin
    Result := Char(x^);
    Exit;
  end;
  //
  Result := FEncoding.GetString(FSource, Index, 1)[1];
end;

function TNPCLexer.NextChar: Char;
var
  x: PByte;
begin
  x := cur;
  try
    if cur^ < 128 then
      Inc(x)
    else
      Inc(x, FEncoding.GetCharCount(FSource, cur - PSource, 1));
    Result := GetChar(x - PSource);
  except
    Result := #0;
  end;
end;

procedure TNPCLexer.EatChar;
var
  x: Char;
begin
  if IsNotEmpty then begin
    x := GetChar;
    if cur^ < 128 then
      Inc(cur)
    else
      Inc(cur, FEncoding.GetCharCount(FSource, cur - PSource, 1));
    if CharInSet(x, [#13, #10]) then begin
      if x = #13 then begin // CR
        if IsNotEmpty and (cur^ = 10) then begin
          Inc(cur);
          bol := cur;
          Inc(FLines);
          Inc(row);
        end;
      end
      else if x = #10 then begin // LF
        bol := cur;
        Inc(FLines);
        Inc(row);
      end;
    end;
  end;
end;

function TNPCLexer.Location: TNPCLocation;
begin
  Result := TNPCLocation.Create(FFileName, row, cur - bol + 1);
end;

function TNPCLexer.TokenString(const AStart, ACur: PByte): String;
var
  L: Integer;
begin
  L := ACur - AStart;
  Result := FEncoding.GetString(FSource, AStart - PSource, L);
end;

function TNPCLexer.TokenMD5(const AValue: String): TNPCMD5;
var
  md5: TBytes;
begin
  md5 := THashMD5.GetHashBytes(AValue);
  Result := BytesAsNPCMD5(md5);
end;

function TNPCLexer.TokenIsReserved(const AMD5: TNPCMD5): Boolean;
begin
  Result := IsReservedWord(AMD5);
end;

function TNPCLexer.NextToken: TNPCToken;
var
  loc: TNPCLocation;
  first, escape: Char;
  start: PByte;
  stemp: String;
  md5: TNPCMD5;
  reserved: Boolean;
  token: TNPCLiteralToken;
begin
  Result := Nil;
  SkipWhitespace;
  while IsNotEmpty do begin
    if {not ((Char(cur^) = '/') and (NextChar = '/')) and}
       not (Char(cur^) in [#13, #10]) then
      Break;
    SkipLine;
    SkipWhitespace;
  end;

  loc := Location;

  if IsEmpty then begin
    Result := TNPCToken.Create(tokEOF, loc, False, False, '', EmptyTokenMD5);
    Exit;
  end;

  first := GetChar;

  if first.IsLetter or (first = '_') then begin
    start := cur;
    EatChar;
    while IsNotEmpty and (GetChar.IsLetterOrDigit or (Char(cur^) = '_')) do
      EatChar;
    loc.SetEndRowCol(row, cur - bol + 1);
    stemp := TokenString(start, cur);
    md5 := TokenMD5(LowerCase(stemp));
    reserved := TokenIsReserved(md5);
    Result := TNPCToken.Create(tokIdent, loc, reserved, False, stemp, md5);
    stemp := '';
    Exit;
  end;

  if first in ['(', ')', '{', '}', '[', ']', '.', ',', ':', ';', '@', '$', '%', '^', '&', '*', '-', '+', '=', '<', '>', '/'] then begin
    // check for biliterals
    if (first = '/') and (NextChar = '/') then begin // // - singleline comment
      start := cur;
      EatChar;
      while IsNotEmpty and not (Char(cur^) in [#13, #10]) do
        EatChar;
      loc.SetEndRowCol(row, cur - bol + 1);
      stemp := TokenString(start, cur);
      Result := TNPCToken.Create(tokCommentSL, loc, False, True, stemp, EmptyTokenMD5);
      Exit;
    end;

    if ((first = '(') and (NextChar = '*')) or ((first = '{') and (NextChar = '.')) then begin // (* {. - multiline comment begin
      start := cur;
      EatChar;
      loc.SetEndRowCol(row, cur - bol + 1);
      stemp := TokenString(start, cur);
      Result := TNPCToken.Create(tokCommentMLB, loc, False, True, stemp, EmptyTokenMD5);
      Exit;
    end;

    if ((first = '*') and (NextChar = ')')) or ((first = '.') and (NextChar = '}')) then begin // *) .} - multiline comment end
      start := cur;
      EatChar;
      loc.SetEndRowCol(row, cur - bol + 1);
      stemp := TokenString(start, cur);
      Result := TNPCToken.Create(tokCommentMLE, loc, False, True, stemp, EmptyTokenMD5);
      Exit;
    end;

    if (first = ':') and (NextChar = '=') then begin // := - assign
      start := cur;
      EatChar;
      loc.SetEndRowCol(row, cur - bol + 1);
      stemp := TokenString(start, cur);
      Result := TNPCToken.Create(tokAssign, loc, False, True, stemp, EmptyTokenMD5);
      Exit;
    end;

    // check for literals
    for token in NPCLiteralTokens do begin
      if first = token.Literal then begin
        EatChar;
        Result := TNPCToken.Create(token.TokenType, loc, False, True, first, EmptyTokenMD5);
        Exit;
      end;
    end;
  end;

  if first.IsNumber or (first = '-') or (first = '$') then begin // 1234567890; 12345.67890; 0x12345678; 123_456_789; 0x12_34_56_78; 12_345.67890
    start := cur;
    EatChar;
    while IsNotEmpty and (Char(cur^).IsDigit or (Char(cur^) = '.') or (Char(cur^) = '_') or (Char(cur^) = 'x')) do
      EatChar;
    loc.SetEndRowCol(row, cur - bol + 1);
    stemp := TokenString(start, cur);
    Result := TNPCToken.Create(tokNumber, loc, False, False, stemp, EmptyTokenMD5);
    stemp := '';
    Exit;
  end;

  if first = '''' then begin // string
    EatChar;
    //start := cur;
    stemp := '';
    while IsNotEmpty do begin
      first := GetChar;
      case first of
        '''': Break;
        '\': begin // escaping string
          EatChar;
          if IsEmpty then
            raise NPCLexerException.LexerError(loc, 'unfinished escape sequence');

          escape := GetChar;
          case escape of
            'n': begin // escaping \n - new line
              stemp := stemp + #13#10;
              EatChar;
            end;
            '\': begin // escaping \\ - single \
              stemp := stemp + '\';
              EatChar;
            end;
            '''': begin // escaping \' - single quote
              stemp := stemp + '''';
              EatChar;
            end;
          else
            raise NPCLexerException.LexerError(loc, 'unknown escape sequence starts with \' + escape);
          end;
        end;
      else
        stemp := stemp + first;
        EatChar;
      end;
    end;
    loc.SetEndRowCol(row, cur - bol + 1);

    if IsNotEmpty then begin
      EatChar;
      Result := TNPCToken.Create(tokString, loc, False, False, stemp, EmptyTokenMD5);
      stemp := '';
      Exit;
    end
    else
      raise NPCLexerException.LexerError(loc, Format('unclosed string literal "%s"', [stemp]));
  end;

  raise NPCLexerException.LexerError(loc, Format('unknown token starts with "%s"', [first]));
end;

function TNPCLexer.ExpectToken(const ATokens: Array of TNPCTokenType): TNPCToken;
var
  token: Char;
begin
  Result := NextToken;
  if Result = Nil then begin
    raise NPCLexerException.LexerError(Location, Format('expected "%s" but got end of file', [String.Join('" or "', ArrayOfTokenToArrayOfString(ATokens))]));
  end;

  for token in ATokens do begin
    if Result.&Type = token then
      Exit;
  end;

  raise NPCLexerException.LexerError(Location, Format('expected "%s" but got "%s"', [String.Join('" or "', ArrayOfTokenToArrayOfString(ATokens)), LiteralTokenToChar(Result.&Type)]));
end;

function TNPCLexer.Lines: Integer;
begin
  Result := FLines;
end;

end.
