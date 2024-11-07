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
    procedure Grow;
    procedure Trim;
    procedure AddToken(const AToken: TNPCToken);
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

implementation

uses
  StrUtils,
  npc_consts,
  npc_project,
  npc_md5,
  npc_tokens,
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

procedure TNPCTokenizer.TokenizeFile(const AFileName: String; const AEncoding: TEncoding; const AProject: Boolean);
var
  token: TNPCToken;
begin
  FFileName := AFileName;
  FEncoding := AEncoding;
  Lexer := TNPCLexer.Create(AFileName, FFormatSettings, AEncoding);
  while Lexer.IsNotEmpty do begin
    token := Lexer.GetToken;
    AddToken(token);
  end;
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

end.

