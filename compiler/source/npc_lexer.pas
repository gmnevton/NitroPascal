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
  npc_tokens,
  npc_reserved_words,
  npc_reserved_symbols;

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
    //
    function TokenToString: String;
  end;

  TNPCTokens = Array of TNPCToken;

  TNPCCharset = set of AnsiChar;

  TNPCLexerOption = (
    loIdentWithMinus
  );
  TNPCLexerOptions = set of TNPCLexerOption;

  TNPCLexer = class
  private
    FFileName: String;
    FFormatSettings: TFormatSettings;
    FEncoding: TEncoding;
    FOptions: TNPCLexerOptions;
    //
    FSource: TBytes;
    FSourceLen: Int64;
    PSource: PByte;
    FLines: Integer;
    //FTokens: TBytes;
    //
    cur: PByte;
    bol: PByte;
    row: Integer;
    //
    skip_cur: PByte;
    skip_bol: PByte;
    skip_row: Integer;
  protected
    procedure SkipPreamble; inline;
    procedure SkipWhitespace; inline;
    procedure SkipLine; inline;
    function GetChar: Char; overload;// inline; // MBCS implementation
    function GetChar(const Index: Integer): Char; overload;// inline; // MBCS implementation
    function Get2Chars: TNPCDoubleLiteral; inline;
    function NextChar: Char; //inline;
    procedure SkipChar(const SkipTwoChars: Boolean = False); inline;
    function Location: TNPCLocation; inline;
    function TokenString(const AStart, ACur: PByte): String; inline;
    function TokenMD5(const AValue: String): TNPCMD5; inline;
    function TokenIsReserved(const AMD5: TNPCMD5): Boolean; inline;
    function GetToken(const ConsumeToken: Boolean): TNPCToken; overload;
  public
    LiteralCharset: TNPCCharset;
    //DoubleLiterals: Array of String[2];
    IdentifierCharset: TNPCCharset;
    NumberCharset: TNPCCharset;
    StringCharset: TNPCCharset;
  public
    constructor Create; overload;
    constructor Create(const AFileName: String); overload;
    constructor Create(const AFileName: String; const AEncoding: TEncoding); overload;
    constructor Create(const AFileName: String; const AFormatSettings: TFormatSettings); overload;
    constructor Create(const AFileName: String; const AFormatSettings: TFormatSettings; const AEncoding: TEncoding); overload;
    constructor Create(const AStream: TStringStream; const AFileName: String); overload;
    constructor Create(const AStream: TStringStream; const AFileName: String; const AEncoding: TEncoding); overload;
    constructor Create(const AStream: TStringStream; const AFileName: String; const AFormatSettings: TFormatSettings); overload;
    constructor Create(const AStream: TStringStream; const AFileName: String; const AFormatSettings: TFormatSettings; const AEncoding: TEncoding); overload; // calls Create
    constructor Create(const AStream: TMemoryStream; const AFormatSettings: TFormatSettings; const AEncoding: TEncoding); overload;
    constructor Create(const AStream: TStringStream; const AFormatSettings: TFormatSettings; const AEncoding: TEncoding); overload; // calls Create
    destructor Destroy; override;
    //
    function IsNotEmpty: Boolean; inline;
    function IsEmpty: Boolean; inline;
    function IsCurrentSymbol(const AValue: TNPCTokenType): Boolean; inline;
    function IsNextSymbol(const AValue: TNPCTokenType): Boolean; inline;
    procedure IdentWithMinus(const AValue: Boolean); inline;
    //
    function GetToken: TNPCToken; overload; inline; // grabs token and moves stream forward
    function NextToken: TNPCToken; inline; // saves current stream position, grabs token and restores stream position
    procedure SkipToken; inline; // if we used NextToken then this fast-forwards stream position to after grabing token by NextToken
    //
    function Lines: Integer;
  end;

implementation

uses
//  Types,
  Character,
  Hash,
  npc_consts,
  npc_error;

{ TNPCToken }

constructor TNPCToken.Create(const AType: TNPCTokenType; const ALocation: TNPCLocation; const AReservedWord, AReservedSymbol: Boolean; const AValue: String; const AValueHash: TNPCMD5);
begin
  &Type := AType;
  Location := ALocation;
//  StringBeginRow := 0;
//  StringBeginCol := 0;
//  StringEndRow := 0;
//  StringEndCol := 0;
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

function TNPCToken.TokenToString: String;
begin
  if &Type in [tokOParen..tokDiv] then
    Result := NPCTokensTypeToString(Self.&Type)
  else begin
    if Length(Self.Value) > 1 then
      Result := NPCTokensTypeToString(Self.&Type)
    else
      Result := Self.Value;
  end;
end;

{ TNPCLexer }

constructor TNPCLexer.Create;
begin
  FFileName := '';
  FFormatSettings := TFormatSettings.Create;
  FEncoding := Nil;
  FOptions := [];
  //
  LiteralCharset := ['(', ')', '{', '}', '[', ']', '.', ',', ':', ';', '!', '@', '#', '$', '%', '^', '&', '*', '-', '+', '=', '<', '>', '/'];
  IdentifierCharset := ['0'..'9', 'a'..'z', 'A'..'Z', '_'];
  NumberCharset := ['0'..'9', '-', '.', 'e', 'E', '$', 'x'];
  StringCharset := ['_', '-', '.'];
  //
  SetLength(FSource, 0);
  FSourceLen := 0;
  PSource := Nil;
  FLines := 0;
  //
  cur := Nil;
  bol := Nil;
  row := 0;
  //
  skip_cur := Nil;
  skip_bol := Nil;
  skip_row := 0;
end;

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
  Create;
  //
  FFileName := AFileName;
  FFormatSettings := AFormatSettings;
  if AEncoding = Nil then
    FEncoding := TEncoding.UTF8
  else
    FEncoding := AEncoding;
  //
  if AStream = Nil then
    raise NPCCompilerError.CreateFmt(sLexerStreamNotSpecified, [FFileName]);
  //
  FSource := Copy(AStream.Bytes, 0, AStream.Size);
  FSourceLen := Length(FSource);
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
  Create;
  //
  FFileName := '';
  FFormatSettings := AFormatSettings;
  if AEncoding = Nil then
    FEncoding := TEncoding.UTF8
  else
    FEncoding := AEncoding;
  //
  if AStream = Nil then
    raise NPCCompilerError.Create(sLexerStreamNotSpecifiedStream);
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
  //FFormatSettings := TFormatSettings.Create;
  FEncoding := Nil;
  //
  IdentifierCharset := [];
  NumberCharset := [];
  StringCharset := [];
  //
  SetLength(FSource, 0);
  PSource := Nil;
  FLines := 0;
  //
  cur := Nil;
  bol := Nil;
  row := 0;
  //
  skip_cur := Nil;
  skip_bol := Nil;
  skip_row := 0;
  //
  inherited;
end;

function TNPCLexer.IsNotEmpty: Boolean;
begin
  Result := (cur <> Nil) and ((cur - PSource) < FSourceLen) and (cur^ <> 0);
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
  if cur^ < 128 then begin // @??: what about ASCII
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
  if Index >= FSourceLen then
    Exit(#0);
  //
  x := PSource;
  Inc(x, Index);
  if x^ < 128 then begin // @??: what about ASCII
    Result := Char(x^);
    Exit;
  end;
  //
  Result := FEncoding.GetString(FSource, Index, 1)[1];
end;

function TNPCLexer.Get2Chars: TNPCDoubleLiteral;
var
  temp: String;
begin
  temp := FEncoding.GetString(FSource, cur - PSource, 2);
  Result := temp[1] + temp[2];
  temp := '';
end;

function TNPCLexer.NextChar: Char;
var
  x: PByte;
begin
  x := cur;
  try
    if cur^ < 128 then // @??: what about ASCII
      Inc(x)
    else
      Inc(x, FEncoding.GetCharCount(FSource, cur - PSource, 1));
    Result := GetChar(x - PSource);
  except
    Result := #0;
  end;
end;

procedure TNPCLexer.SkipChar(const SkipTwoChars: Boolean = False);
var
  x: Char;
  xadd: Byte;
begin
  xadd := 1;
  if SkipTwoChars then
    xadd := 2;
  //
  if IsNotEmpty then begin
    x := GetChar;
    if cur^ < 128 then // @??: what about ASCII
      Inc(cur, xadd)
    else
      Inc(cur, FEncoding.GetCharCount(FSource, cur - PSource, xadd));
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

function TNPCLexer.GetToken(const ConsumeToken: Boolean): TNPCToken;
var
  loc: TNPCLocation;
  first, escape: Char;
  start: PByte;
  stemp: String;
  first2: TNPCDoubleLiteral;
  md5: TNPCMD5;
  reserved: Boolean;
  ltoken: TNPCLiteralToken;
  dltoken: TNPCDoubleLiteralToken;
//  StringBeginRow,
//  StringBeginCol,
//  StringEndRow,
//  StringEndCol: Integer;
  //
  save_cur: PByte;
  save_bol: PByte;
  save_row: Integer;
begin
//  Result := Nil;
  if not ConsumeToken then begin
    save_cur := cur;
    save_bol := bol;
    save_row := row;
  end;
  //
  try
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

//  LiteralCharset := ['(', ')', '{', '}', '[', ']', '.', ',', ':', ';', '!', '@', '#', '$', '%', '^', '&', '*', '-', '+', '=', '<', '>', '/'];
//  IdentifierCharset := ['0'..'9', 'a'..'z', 'A'..'Z', '_', '.'];
//  NumberCharset := ['0'..'9', '-', '.', 'e', 'E', '$', 'x'];
//  StringCharset := ['_', '-', '.'];

    if loIdentWithMinus in FOptions then
      IdentifierCharset := IdentifierCharset + ['-'];

    if first.IsLetter or (first = '_') then begin // ident
      start := cur;
      SkipChar;
      while IsNotEmpty and (GetChar.IsLetterOrDigit or (Char(cur^) in IdentifierCharset)) do
        SkipChar;
      loc.SetEndRowCol(row, cur - bol + 1);
      stemp := TokenString(start, cur);
      md5 := TokenMD5(LowerCase(stemp));
      reserved := TokenIsReserved(md5);
      Result := TNPCToken.Create(tokIdent, loc, reserved, False, stemp, md5);
      stemp := '';
      Exit;
    end;

    if first in LiteralCharset then begin // literals
      // check for biliterals

      if (first = '/') and (NextChar = '/') then begin // // - singleline comment
        start := cur;
        SkipChar;
        while IsNotEmpty and not (Char(cur^) in [#13, #10]) do
          SkipChar;
        loc.SetEndRowCol(row, cur - bol + 1);
        stemp := TokenString(start, cur);
        Result := TNPCToken.Create(tokCommentSL, loc, False, True, stemp, EmptyTokenMD5);
        Exit;
      end;

//      if ((first = '(') and (NextChar = '*')) or ((first = '{') and (NextChar = '.')) then begin // (* {. - multiline comment begin
//        start := cur;
//        SkipChar;
//        loc.SetEndRowCol(row, cur - bol + 1);
//        stemp := TokenString(start, cur);
//        Result := TNPCToken.Create(tokCommentMLB, loc, False, True, stemp, EmptyTokenMD5);
//        Exit;
//      end;
//
//      if ((first = '*') and (NextChar = ')')) or ((first = '.') and (NextChar = '}')) then begin // *) .} - multiline comment end
//        start := cur;
//        SkipChar;
//        loc.SetEndRowCol(row, cur - bol + 1);
//        stemp := TokenString(start, cur);
//        Result := TNPCToken.Create(tokCommentMLE, loc, False, True, stemp, EmptyTokenMD5);
//        Exit;
//      end;
//
//      if (first = ':') and (NextChar = '=') then begin // := - assign
//        start := cur;
//        SkipChar;
//        loc.SetEndRowCol(row, cur - bol + 1);
//        stemp := TokenString(start, cur);
//        Result := TNPCToken.Create(tokAssign, loc, False, True, stemp, EmptyTokenMD5);
//        Exit;
//      end;

      if NextChar <> #0 then begin
        for dltoken in NPCDoubleLiteralTokens do begin
          first2 := Get2Chars;
          if first2 = dltoken.DoubleLiteral then begin
            SkipChar(True);
            loc.SetEndRowCol(row, cur - bol + 1);
            Result := TNPCToken.Create(dltoken.TokenType, loc, False, True, first2, EmptyTokenMD5);
            first2 := '';
            Exit;
          end;
          first2 := '';
        end;
      end;

      // check for literals
      for ltoken in NPCLiteralTokens do begin
        if first = ltoken.Literal then begin
          SkipChar;
          Result := TNPCToken.Create(ltoken.TokenType, loc, False, True, first, EmptyTokenMD5);
          Exit;
        end;
      end;
    end;

    if (first = '#') or (first in NumberCharset) then begin // 1234567890; 12345.67890; 0x12345678; 123_456_789; 0x12_34_56_78; 12_345.67890; #12345; #$AB
      start := cur;
      SkipChar;
      if first = '#' then begin
        if Char(cur^) = '$' then begin
          SkipChar;
          while IsNotEmpty and (Char(cur^) in ['0'..'9', 'a'..'f', 'A'..'F', '_']) do
            SkipChar;
        end
        else begin
          while IsNotEmpty and (Char(cur^) in ['0'..'9', '_']) do
            SkipChar;
        end;
        loc.SetEndRowCol(row, cur - bol + 1);
        stemp := TokenString(start, cur);
        Result := TNPCToken.Create(tokChar, loc, False, False, stemp, EmptyTokenMD5);
        stemp := '';
      end
      else begin
        //while IsNotEmpty and (Char(cur^).IsDigit or (Char(cur^) = '.') or (Char(cur^) = '_') or (Char(cur^) = 'x')) do
        while IsNotEmpty and (Char(cur^) in NumberCharset) do
          SkipChar;
        loc.SetEndRowCol(row, cur - bol + 1);
        stemp := TokenString(start, cur);
        Result := TNPCToken.Create(tokNumber, loc, False, False, stemp, EmptyTokenMD5);
        stemp := '';
      end;
      Exit;
    end;

    if first = '''' then begin // string
//      StringBeginRow := row;
//      StringBeginCol := cur - bol + 1;
//      StringEndRow := 0;
//      StringEndCol := 0;
      SkipChar;
      //start := cur;
      stemp := '';
      while IsNotEmpty do begin
        first := GetChar;
        case first of
          '''': begin
//            StringEndRow := row;
//            StringEndCol := cur - bol + 1;
            Break;
          end;
          '\': begin // escaping string
            SkipChar;
            if IsEmpty then
              raise NPCSyntaxError.LexerError(loc, 'unfinished escape sequence');

            escape := GetChar;
            case escape of
              'r': begin // escaping \r - carret return
                stemp := stemp + #13;
                SkipChar;
              end;
              'n': begin // escaping \n - new line
                stemp := stemp + #10;
                SkipChar;
              end;
              '\': begin // escaping \\ - single \
                stemp := stemp + '\';
                SkipChar;
              end;
              '''': begin // escaping \' - single quote
                stemp := stemp + '''';
                SkipChar;
              end;
//              '.': begin // escaping \. - backslash-dot, but this is not escapable character, so add it all
//                stemp := stemp + '\.';
//                SkipChar;
//              end;
            else
              Inc(loc.StartCol);
              loc.SetEndRowCol(row, cur - bol + 1);
              raise NPCSyntaxError.LexerError(loc, 'unknown escape sequence starts with \' + escape);
            end;
          end;
        else
          stemp := stemp + first;
          SkipChar;
        end;
      end;
      loc.SetEndRowCol(row, cur - bol + 1);
      SkipChar;

      if IsNotEmpty then begin
        Result := TNPCToken.Create(tokString, loc, False, False, stemp, EmptyTokenMD5);
//        Result.StringBeginRow := StringBeginRow;
//        Result.StringBeginCol := StringBeginCol;
//        Result.StringEndRow := StringEndRow;
//        Result.StringEndCol := StringEndCol;
        stemp := '';
        Exit;
      end
      else
        raise NPCSyntaxError.LexerError(loc, Format('unclosed string literal "%s"', [stemp]));
    end;

    raise NPCSyntaxError.LexerError(loc, Format('unknown token starts with "%s"', [first]));
  finally
    if loIdentWithMinus in FOptions then
      IdentifierCharset := IdentifierCharset - ['-'];
    //
    if not ConsumeToken then begin
      skip_cur := cur;
      skip_bol := bol;
      skip_row := row;
      //
      cur := save_cur;
      bol := save_bol;
      row := save_row;
    end;
  end;
end;

procedure TNPCLexer.IdentWithMinus(const AValue: Boolean);
begin
  if AValue then begin
    FOptions := FOptions + [loIdentWithMinus];
  end
  else begin
    FOptions := FOptions - [loIdentWithMinus];
  end;
end;

function TNPCLexer.GetToken: TNPCToken;
begin
  if skip_cur <> Nil then begin
    skip_cur := Nil;
    skip_bol := Nil;
    skip_row := 0;
  end;
  Result := GetToken(True);
end;

function TNPCLexer.NextToken: TNPCToken;
begin
  Result := GetToken(False);
end;

procedure TNPCLexer.SkipToken;
begin
  if skip_cur <> Nil then begin
    cur := skip_cur;
    bol := skip_bol;
    row := skip_row;
    //
    skip_cur := Nil;
    skip_bol := Nil;
    skip_row := 0;
  end;
end;

function TNPCLexer.Lines: Integer;
begin
  Result := FLines;
end;

end.

