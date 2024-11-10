//
// Nitro Pascal Compiler
// version 1.0
//
// Tokenizer
//

unit npc_tokenizer;

interface

uses
  SysUtils,
  Classes,
  npc_lexer,
  npc_reserved_words,
  npc_reserved_symbols,
  npc_tokens,
  npc_location,
  npc_utils,
  npc_error;

type
//  NPCTokenizerException = class(TNPCError);

  TNPCTokenizer = class
  private
    Lexer: TNPCLexer;
    TokensArray: TNPCTokens;
    //
    FIndex: UInt64;
    FFileName: String;
    FFormatSettings: TFormatSettings;
    FEncoding: TEncoding;
  protected
    function Unescape(const Value: String): String; inline;
    procedure Grow; inline;
    procedure Trim; inline;
    procedure AddToken(const AToken: TNPCToken); inline;
  public
    constructor Create(const AFormatSettings: TFormatSettings);
    destructor Destroy; override;
    //
    procedure Clear;
    //
    procedure TokenizeFile(const AFileName: String; const AEncoding: TEncoding);
    procedure OutputTokens;
    //
    property Tokens: TNPCTokens read TokensArray;
  end;

  TNPCTokensParser = class
  private
    TokensArray: TNPCTokens;
    FFileName: String;
    FCount: UInt64;
    FIndex: UInt64;
    FOptions: TNPCLexerOptions;
    FLastTokenLocation: TNPCLocation;
  protected
    function Location: TNPCLocation; inline;
  public
    constructor Create(const AFileName: String; const ATokens: TNPCTokens);
    destructor Destroy; override;
    //
    function IsNotEmpty: Boolean; inline;
    function IsEmpty: Boolean; inline;
    function IsCurrentSymbol(const AValue: TNPCTokenType): Boolean; inline;
    procedure IdentWithMinus(const AValue: Boolean); inline;


    function GetToken: TNPCToken; inline; // grabs token and moves index forward
    function NextToken: TNPCToken; inline; // grabs token and preserves actual index
    procedure SkipToken; inline; // increase index to next position

    function ExpectToken(const ATokens: Array of TNPCTokenType; const IdentWithMinus: Boolean = False): TNPCToken;
    function ExpectReservedToken(const AReservedWord: TNPCReservedIdents): TNPCToken; inline;
    function ExpectReservedSymbol(const AReservedSymbol: TNPCReservedSymbols): TNPCToken; inline;
  end;

implementation

uses
  StrUtils,
  npc_consts,
  npc_project,
  npc_md5,
  npc_types;

{ TNPCTokenizer }

constructor TNPCTokenizer.Create(const AFormatSettings: TFormatSettings);
begin
  Lexer := Nil;
  SetLength(TokensArray, 0);
  FIndex := 0;
  FFileName := '';
  FFormatSettings := AFormatSettings;
  FEncoding := Nil;
end;

destructor TNPCTokenizer.Destroy;
begin
  Clear;
  FEncoding := Nil;
  FFileName := '';
  FreeAndNil(Lexer);
  inherited;
end;

procedure TNPCTokenizer.Clear;
var
  i: UInt64;
begin
  for i:=0 to High(TokensArray) do
    FreeAndNil(TokensArray[i]);
  //
  SetLength(TokensArray, 0);
  FIndex := 0;
end;

procedure TNPCTokenizer.Grow;
var
  size: UInt64;
begin
  size := Length(TokensArray);
  SetLength(TokensArray, size + 100);
end;

procedure TNPCTokenizer.Trim;
begin
  SetLength(TokensArray, FIndex);
end;

function TNPCTokenizer.Unescape(const Value: String): String;
var
  sLen, sIndex: Integer;
begin
  sLen:=Length(Value);
  sIndex := 1;
  Result:=Value;
  while sIndex <= sLen do begin
    case Result[sIndex] of
      #13: begin
        Result[sIndex]:='\';
        Insert('r', Result, sIndex + 1);
        Inc(sIndex);
        Inc(sLen);
      end;
      #10: begin
        Result[sIndex]:='\';
        Insert('n', Result, sIndex + 1);
        Inc(sIndex);
        Inc(sLen);
      end;
      #9: begin
        Result[sIndex]:='\';
        Insert('t', Result, sIndex + 1);
        Inc(sIndex);
        Inc(sLen);
      end;
      '"': begin
        Result[sIndex]:='\';
        Insert('"', Result, sIndex + 1);
        Inc(sIndex);
        Inc(sLen);
      end;
      '''': begin
        Result[sIndex]:='\';
        Insert('''', Result, sIndex + 1);
        Inc(sIndex);
        Inc(sLen);
      end;
    end;
    Inc(sIndex);
  end;
end;

procedure TNPCTokenizer.AddToken(const AToken: TNPCToken);
var
  size: UInt64;
begin
  size := Length(TokensArray);
  if FIndex = size then
    Grow;
  //
  TokensArray[FIndex] := AToken;
  Inc(FIndex);
end;

procedure TNPCTokenizer.TokenizeFile(const AFileName: String; const AEncoding: TEncoding);
var
  token: TNPCToken;
begin
  FFileName := AFileName;
  FEncoding := AEncoding;
  Lexer := TNPCLexer.Create(AFileName, FFormatSettings, AEncoding);
//  while Lexer.IsNotEmpty do begin
  repeat
    token := Lexer.GetToken;
    AddToken(token);
  until token.&Type = tokEOF;
//  end;
  Trim;
end;

procedure TNPCTokenizer.OutputTokens;
var
  i: Integer;
  tf: TStreamWriter;
  token: TNPCToken;
begin
  try
    tf := TStreamWriter.Create(FFileName + '.tokens', False, TEncoding.UTF8, 32768);
    try
      tf.BaseStream.Position := 0;
      tf.BaseStream.Size := 0;
      //
      for i:=0 to Length(TokensArray) - 1 do begin
        token := TokensArray[i];
        if Assigned(token) then
          tf.WriteLine(Format('%s (%d:%d) - %s: "%s"',
                              [token.Location.FileName, token.Location.StartRow, token.Location.StartCol,
                               IfThen(token.ReservedWord or token.ReservedSymbol, 'reserved ') +
                               IfThen(token.ReservedWord, 'word ') +
                               IfThen(token.ReservedSymbol, 'symbol ') + NPCTokensTypeToString(token.&Type), Unescape(token.Value)]));
      end;
    finally
      tf.Free;
    end;
  except
  end;
end;

{ TNPCTokensParser }

constructor TNPCTokensParser.Create(const AFileName: String; const ATokens: TNPCTokens);
begin
  FFileName := AFileName;
  TokensArray := ATokens;
  FCount := Length(TokensArray);
  FIndex := 0;
  FOptions := [];
  FLastTokenLocation := Nil;
end;

destructor TNPCTokensParser.Destroy;
begin
  FLastTokenLocation := Nil;
  SetLength(TokensArray, 0);
  FFileName := '';
  inherited;
end;

function TNPCTokensParser.Location: TNPCLocation;
begin
  if not Assigned(FLastTokenLocation) then
    Result := TNPCLocation.Create(FFileName, 1, 1)
  else begin
    Result := FLastTokenLocation.Copy;
    Result.SetEndAsStart;
  end;
end;

function TNPCTokensParser.IsNotEmpty: Boolean;
begin
  Result := (Length(TokensArray) > 0) and (FIndex < FCount);
end;

function TNPCTokensParser.IsEmpty: Boolean;
begin
  Result := not IsNotEmpty;
end;

function TNPCTokensParser.IsCurrentSymbol(const AValue: TNPCTokenType): Boolean;
begin
  Result := False;
  if FIndex >= FCount then
    Exit;
  //
  Result := TokensArray[FIndex].Value[1] = AValue;
end;

procedure TNPCTokensParser.IdentWithMinus(const AValue: Boolean);
begin
  if AValue then begin
    FOptions := FOptions + [loIdentWithMinus];
  end
  else begin
    FOptions := FOptions - [loIdentWithMinus];
  end;
end;

function TNPCTokensParser.GetToken: TNPCToken;
begin
  Result := Nil;
  if FIndex >= FCount then
    Exit;
  //
  Result := TokensArray[FIndex];
  FLastTokenLocation := Result.Location;
  Inc(FIndex);
end;

function TNPCTokensParser.NextToken: TNPCToken;
begin
  Result := Nil;
  if FIndex >= FCount - 1 then
    Exit;
  Result := TokensArray[FIndex + 1];
end;

procedure TNPCTokensParser.SkipToken;
begin
  if FIndex >= FCount then
    Exit;
  Inc(FIndex);
end;

function TNPCTokensParser.ExpectToken(const ATokens: Array of TNPCTokenType; const IdentWithMinus: Boolean = False): TNPCToken;
var
  token: TNPCTokenType;
  tempOptions: TNPCLexerOptions;
begin
  tempOptions := FOptions;
  try
    if IdentWithMinus then
      FOptions := FOptions + [loIdentWithMinus];
    Result := GetToken;
    if Result = Nil then begin
      raise NPCLexerException.LexerError(Location, Format('expected "%s" but got end of file', [String.Join('" or "', ArrayOfTokenToArrayOfString(ATokens))]));
    end;

    for token in ATokens do begin
      if Result.&Type = token then
        Exit;
    end;

    raise NPCLexerException.LexerError(Result.Location, Format('expected "%s" but got "%s"', [String.Join('" or "', ArrayOfTokenToArrayOfString(ATokens)), LiteralTokenToChar(Result.&Type)]));
  finally
    FOptions := tempOptions;
  end;
end;

function TNPCTokensParser.ExpectReservedToken(const AReservedWord: TNPCReservedIdents): TNPCToken;
begin
  Result := GetToken;
  if Result = Nil then begin
    raise NPCLexerException.LexerError(Location, Format('expected "%s" but got end of file', ['ident']));
  end;

  if (Result.&Type = tokIdent) and Result.ReservedWord and (MD5ToReservedWord(Result.ValueHash) = AReservedWord) then
    Exit;

  raise NPCLexerException.LexerError(Result.Location, Format('expected "%s" but got "%s"', [NPCReservedIdentifiers[AReservedWord].Ident, Result.TokenToString]));
end;

function TNPCTokensParser.ExpectReservedSymbol(const AReservedSymbol: TNPCReservedSymbols): TNPCToken;
begin
  Result := GetToken;
  if Result = Nil then begin
    raise NPCLexerException.LexerError(Location, Format('expected "%s" but got end of file', ['symbol']));
  end;

  if (Result.&Type in [tokOParen..tokNotEqual]) and not Result.ReservedWord and Result.ReservedSymbol and (TokenTypeToReservedSymbol(Result.&Type) = AReservedSymbol) then
    Exit;

  raise NPCLexerException.LexerError(Result.Location, Format('expected "%s" but got "%s"', [NPCReservedSymbolToString(AReservedSymbol), Result.TokenToString]));
end;

end.

