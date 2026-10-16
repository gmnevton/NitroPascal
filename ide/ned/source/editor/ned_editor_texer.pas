//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-10
// All rights reserved.
//

unit ned_editor_texer;

interface

uses
  SysUtils,
  Classes,
  Graphics,
  Generics.Collections,
  ned_editor_lexer,
  ned_editor_types;

type
  TNEDTextTokenKindEnum = (
    ttkDefault,
    ttkKeyword,
    ttkIdentifier,
    ttkString,
    ttkNumber,
    ttkComment,
    ttkOperator
    //ttkType,
    //ttkFunction,
    //ttkStructure
  );

//  TNEDTextTokenStructureKindEnum = (
//    ttsEnum,
//    ttsSet,
//    ttsBitSet,
//    ttsTuple,
//    ttsRecord,
//    ttsStruct,
//    ttsUnion,
//    ttsInterface,
//    ttsClass
//  );

  TNEDTextLineBreakStyleEnum = (tlbsNone, tlbsCR, tlbsLF, tlbsCRLF);

  TNEDTokenFormat = record
    StyleID: Integer;
    //Font: TFont;
    FontBackground: TColor;
    FontForeground: TColor;
    FontStyle: TFontStyles;
    TextAlignment: TVerticalAlignment; // vtop, vcenter, vbottom
    WordBreak: Boolean; // this character token is a line break
    WordWrap: Boolean; // internal; wrap line in this place while rendering
    LineBreak: String; // oryginal line break
    LineBreakType: TNEDTextLineBreakStyleEnum;
    LetterSpacing: Integer;
    WordSpacing: Integer;
    TextIndentation: Integer;
  end;

  TNEDTextToken = record
  public
    Kind: TNEDTextTokenKindEnum;
    Offset: Integer;
    Length: Integer;
    Format: TNEDTokenFormat;
    //
    //procedure SetFormat(const Value: TNEDTokenFormat);
  end;
  PNEDTextToken = ^TNEDTextToken;

  TNEDTextTokenList = TList<TNEDTextToken>;

{ Keyword collection }

  TNEDTextSetArray = Array of String;
  TNEDTextSetCharSet = Array of Char;

  TNEDTextSet = class
  private
    FTextSet: TStringList;
    //
    FTextSetArrayCache: TNEDTextSetArray;
    FTextSetCharSetCache: TNEDTextSetCharSet;
    FTextSetStringCache: UTF8String;
    //
    FTextSetArrayCacheValid: Boolean;
    FTextSetCharSetCacheValid: Boolean;
    FTextSetStringCacheValid: Boolean;
    //
    function GetCount: Integer;
    function GetTexts(AIndex: Integer): String;
    function GetCaseSensitive: Boolean;
    procedure SetCaseSensitive(const Value: Boolean);
  public
    constructor Create(const ACaseSensitive: Boolean = False);
    destructor Destroy; override;
    //
    procedure Clear;
    procedure ClearCache;
    procedure Assign(const ATextSet: TNEDTextSet); overload; inline;
    procedure Assign(const AStringList: TStringList); overload; inline;
    procedure Add(const AText: String);
    procedure AddText(const ATextSet: TNEDTextSetArray);
    //
    function Contains(const AText: String): Boolean;
    function ToArray: TNEDTextSetArray;
    function ToCharSet: TNEDTextSetCharSet;
    function ToString: UTF8String;
    //
    property Count: Integer read GetCount;
    property Texts[AIndex: Integer]: String read GetTexts;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
  end;

{ Line tokens }

  TNEDLineTokens = class
  private
    FTokens: TNEDTextTokenList;
    //
    function GetCount: Integer;
    function GetToken(const AIndex: Integer): TNEDTextToken;
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Clear;
    procedure Assign(const ATokensList: TNEDTextTokenList);
    //
    procedure Add(const AKind: TNEDTextTokenKindEnum; const AOffset, ALength: Integer);
    procedure SetTokenFormat(AIndex: Integer; const AFormat: TNEDTokenFormat);
    //
    property Count: Integer read GetCount;
    property Tokens[const AIndex: Integer]: TNEDTextToken read GetToken;
  end;

  TNEDTexerConfig = class
  private
    FWhitespaceCharSet: UTF8String; // ' ', #9, #10, #13
    //
    FIdentifierCharSet: UTF8String; // a..z, A..Z, 0..9, _
    FIdentifierUnderscoreStart: Boolean; // _ident
    FIdentifierNumberStart: Boolean; // 0ident
    FIdentifierCanContainDash: Boolean; // some-type
    //
    FNumberCharSet: UTF8String; // 0..9, '.', '_', x/X, b/B, o/O, e/E, +, -
    FNumberUnderscoreStart: Boolean; // _0123
    FNumberNumberStart: Boolean; // 00C0FE
    FNumberCanContainDash: Boolean; // 00-00
    //
    FCommentsDelimiters: Array of TNEDEditorDelimiterPair;
    FStringDelimiter: UTF8Char; // " or '

    FKeywords: TNEDTextSet;
    FOperators: TNEDTextSet;
    FPunctuation: TNEDTextSet;

    FBooleanLiterals: TNEDTextSet;
    FNilLiterals: TNEDTextSet;

    FHexPrefix: UTF8String; // 0xABCD, $ABCD
    FHexSuffix: UTF8String; // 0ABCDH or 0ABCDh - assembler
    FBinaryPrefix: UTF8String; // 0b01010101
    FBinarySuffix: UTF8String; // 01010101b
    FOctalPrefix: UTF8String; // 0o01234567
    FOctalSuffix: UTF8String; // 01234567o
    FExtendedSuffix: UTF8String; // 1.123e4 or 1.123E4

    FDecimalSeparator: UTF8Char;
    FAllowUnderscoreNumbers: Boolean;

    FCaseSensitive: Boolean;
  private
    function GetCommentsDelimitersCount: Integer;
    function GetCommentsDelimiters(const Index: Integer): TNEDEditorDelimiterPair;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(const ASource: TNEDTexerConfig);
    //
    procedure Reset;
    //
    property WhitespaceCharSet: UTF8String read FWhitespaceCharSet write FWhitespaceCharSet;
    //
    property IdentifierCharSet: UTF8String read FIdentifierCharSet write FIdentifierCharSet;
    property IdentifierUnderscoreStart: Boolean read FIdentifierUnderscoreStart write FIdentifierUnderscoreStart;
    property IdentifierNumberStart: Boolean read FIdentifierNumberStart write FIdentifierNumberStart;
    property IdentifierCanContainDash: Boolean read FIdentifierCanContainDash write FIdentifierCanContainDash;
    //
    property NumberCharSet: UTF8String read FNumberCharSet write FNumberCharSet;
    property NumberUnderscoreStart: Boolean read FNumberUnderscoreStart write FNumberUnderscoreStart;
    property NumberNumberStart: Boolean read FNumberNumberStart write FNumberNumberStart;
    property NumberCanContainDash: Boolean read FNumberCanContainDash write FNumberCanContainDash;
    //
    property CommentsDelimitersCount: Integer read GetCommentsDelimitersCount;
    property CommentsDelimiters[const Index: Integer]: TNEDEditorDelimiterPair read GetCommentsDelimiters;
    property StringDelimiter: UTF8Char read FStringDelimiter write FStringDelimiter;
    //
    property Keywords: TNEDTextSet read FKeywords;
    property Operators: TNEDTextSet read FOperators;
    property Punctuation: TNEDTextSet read FPunctuation;

    property BooleanLiterals: TNEDTextSet read FBooleanLiterals;
    property NilLiterals: TNEDTextSet read FNilLiterals;

    property HexPrefix: UTF8String read FHexPrefix write FHexPrefix;
    property HexSuffix: UTF8String read FHexSuffix write FHexSuffix;
    property BinaryPrefix: UTF8String read FBinaryPrefix write FBinaryPrefix;
    property BinarySuffix: UTF8String read FBinarySuffix write FBinarySuffix;
    property OctalPrefix: UTF8String read FOctalPrefix write FOctalPrefix;
    property OctalSuffix: UTF8String read FOctalSuffix write FOctalSuffix;
    property ExtendedSuffix: UTF8String read FExtendedSuffix write FExtendedSuffix;

    property DecimalSeparator: UTF8Char read FDecimalSeparator write FDecimalSeparator;
    property AllowUnderscoreNumbers: Boolean read FAllowUnderscoreNumbers write FAllowUnderscoreNumbers;

    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
  end;

{ Texer }

  TNEDTexer = class
  private
    FLexer: TNEDLexer;
    FConfig: TNEDTexerConfig;
    //
    FLine: Integer;
    FText: UTF8String;
    FTokens: TNEDTextTokenList;
    //
    FCurrentOffset: Integer;
    //
    function IsWhitespace(AChar: UTF8Char): Boolean;
    //
    function IsCommentDelimiterStart(AChar: UTF8Char): Boolean;
    //
    function IsIdentifierStart(AChar: UTF8Char): Boolean;
    function IsIdentifierChar(AChar: UTF8Char): Boolean;
    //
    function IsNumberStart(AChar: UTF8Char): Boolean;
    function IsNumberChar(AChar: UTF8Char): Boolean;
    //
    function IsOperatorChar(AChar: UTF8Char): Boolean;
    function IsPunctuationChar(AChar: UTF8Char): Boolean;
    function IsKeyword(const AText: string): Boolean;
    //
    function GetText(const AOffset, ALength: Integer): String;
    //
    procedure AddToken(const AKind: TNEDTextTokenKindEnum; const AOffset, ALength: Integer);
    //
    procedure TexIdentifier;
    procedure TexNumber;
    procedure TexString;
    procedure TexComment;
    procedure TexOperator;
    //
    procedure TexWhitespace;
    procedure TexUnknown;
  public
    constructor Create(const ALexer: TNEDLexer);
    destructor Destroy; override;
    //
    procedure Reset;
    //
    procedure SetText(const ALine: Integer; const AText: UTF8String);
    procedure Tex;
    //
    property Text: UTF8String read FText;
    //
    property Lexer: TNEDLexer read FLexer;
    property Config: TNEDTexerConfig read FConfig;
    //
    property Tokens: TNEDTextTokenList read FTokens;
  end;

implementation

//{ TNEDTextToken }
//
//procedure TNEDTextToken.SetFormat(const Value: TNEDTokenFormat);
//begin
//  Self.Format := Value;
//end;

{ TNEDTextSet }

constructor TNEDTextSet.Create(const ACaseSensitive: Boolean = False);
begin
  inherited Create;
  FTextSet := TStringList.Create;

  FTextSet.Sorted := True;
  FTextSet.Duplicates := dupIgnore;
  FTextSet.CaseSensitive := ACaseSensitive;

  ClearCache;
end;

destructor TNEDTextSet.Destroy;
begin
  ClearCache;
  FTextSet.Free;
  inherited Destroy;
end;

function TNEDTextSet.GetCount: Integer;
begin
  Result := FTextSet.Count;
end;

function TNEDTextSet.GetTexts(AIndex: Integer): string;
begin
  Result := FTextSet[AIndex];
end;

function TNEDTextSet.GetCaseSensitive: Boolean;
begin
  Result := FTextSet.CaseSensitive;
end;

procedure TNEDTextSet.SetCaseSensitive(const Value: Boolean);
begin
  if FTextSet.CaseSensitive <> Value then
    FTextSet.CaseSensitive := Value;
end;

procedure TNEDTextSet.Clear;
begin
  FTextSet.Clear;
end;

procedure TNEDTextSet.ClearCache;
begin
  SetLength(FTextSetArrayCache, 0);
  SetLength(FTextSetCharSetCache, 0);
  FTextSetStringCache := '';
  //
  FTextSetArrayCacheValid := False;
  FTextSetCharSetCacheValid := False;
  FTextSetStringCacheValid := False;
end;

procedure TNEDTextSet.Assign(const ATextSet: TNEDTextSet);
begin
  FTextSet.Assign(ATextSet.FTextSet);
  FTextSet.CaseSensitive := ATextSet.CaseSensitive;
end;

procedure TNEDTextSet.Assign(const AStringList: TStringList);
begin
  FTextSet.Assign(AStringList);
  FTextSet.CaseSensitive := AStringList.CaseSensitive;
end;

procedure TNEDTextSet.Add(const AText: String);
begin
  if AText = '' then
    Exit;

  FTextSet.Add(AText);
  ClearCache;
end;

procedure TNEDTextSet.AddText(const ATextSet: TNEDTextSetArray);
var
  I: Integer;
begin
  for I := Low(ATextSet) to High(ATextSet) do
    Add(ATextSet[I]);
  ClearCache;
end;

function TNEDTextSet.Contains(const AText: String): Boolean;
begin
  Result := FTextSet.IndexOf(AText) >= 0;
end;

function TNEDTextSet.ToArray: TNEDTextSetArray;
var
  I: Integer;
begin
  if FTextSetArrayCacheValid then begin
    Exit(FTextSetArrayCache);
  end;

  SetLength(Result, FTextSet.Count);
  for I := 0 to FTextSet.Count - 1 do
    Result[I] := FTextSet.Strings[I];

  FTextSetArrayCache := Result;
  FTextSetArrayCacheValid := True;
end;

function TNEDTextSet.ToCharSet: TNEDTextSetCharSet;
var
  I: Integer;
  temp: String;
begin
  if FTextSetCharSetCacheValid then begin
    Exit(FTextSetCharSetCache);
  end;

  SetLength(Result, FTextSet.Count);
  for I := 0 to FTextSet.Count - 1 do begin
    temp := FTextSet.Strings[I];
    if Length(temp) > 0 then
      Result[I] := temp[1];
  end;

  FTextSetCharSetCache := Result;
  FTextSetCharSetCacheValid := True;
end;

function TNEDTextSet.ToString: UTF8String;
var
  I: Integer;
begin
  if FTextSetStringCacheValid then begin
    Exit(FTextSetStringCache);
  end;

  Result := '';
  for I := 0 to FTextSet.Count - 1 do
    Result := Result + FTextSet.Strings[I];

  FTextSetStringCache := Result;
  FTextSetStringCacheValid := True;
end;

{ TNEDLineTokens }

constructor TNEDLineTokens.Create;
begin
  inherited Create;
  FTokens := TNEDTextTokenList.Create;
end;

destructor TNEDLineTokens.Destroy;
begin
  Clear;
  FTokens.Free;
  inherited Destroy;
end;

procedure TNEDLineTokens.Clear;
begin
  FTokens.Clear;
end;

procedure TNEDLineTokens.Assign(const ATokensList: TNEDTextTokenList);
var
  i: Integer;
begin
  Clear;
  for i := 0 to ATokensList.Count - 1 do
    FTokens.Add(ATokensList.Items[i]);
end;

procedure TNEDLineTokens.Add(const AKind: TNEDTextTokenKindEnum; const AOffset, ALength: Integer);
var
  Token: TNEDTextToken;
begin
//  New(Token);
  FillChar(Token, SizeOf(TNEDTextToken), 0);

  Token.Kind := AKind;
  Token.Offset := AOffset;
  Token.Length := ALength;

  FTokens.Add(Token);
end;

procedure TNEDLineTokens.SetTokenFormat(AIndex: Integer; const AFormat: TNEDTokenFormat);
var
  Token: TNEDTextToken;
begin
  Token := FTokens.Items[AIndex];
  Token.Format := AFormat;
  FTokens.Items[AIndex] := Token;
end;

function TNEDLineTokens.GetCount: Integer;
begin
  Result := FTokens.Count;
end;

function TNEDLineTokens.GetToken(const AIndex: Integer): TNEDTextToken;
begin
  Result := FTokens.Items[AIndex];
end;

{ TNEDTexerConfig }

constructor TNEDTexerConfig.Create;
begin
  inherited Create;
  FKeywords := TNEDTextSet.Create;
  FOperators := TNEDTextSet.Create(True);
  FPunctuation := TNEDTextSet.Create(True);
  FBooleanLiterals := TNEDTextSet.Create;
  FNilLiterals := TNEDTextSet.Create;

  Reset;
end;

destructor TNEDTexerConfig.Destroy;
begin
  FNilLiterals.Free;
  FBooleanLiterals.Free;
  FPunctuation.Free;
  FOperators.Free;
  FKeywords.Free;

  inherited Destroy;
end;

procedure TNEDTexerConfig.Assign(const ASource: TNEDTexerConfig);
var
  i, len: Integer;
begin
  if ASource = nil then
    Exit;

  FWhitespaceCharSet := ASource.WhitespaceCharSet;
  //
  FIdentifierCharSet := ASource.IdentifierCharSet;
  FIdentifierUnderscoreStart := ASource.IdentifierUnderscoreStart;
  FIdentifierNumberStart := ASource.IdentifierNumberStart;
  FIdentifierCanContainDash := ASource.IdentifierCanContainDash;
  //
  FNumberCharSet := ASource.NumberCharSet;
  FNumberUnderscoreStart := ASource.NumberUnderscoreStart;
  FNumberNumberStart := ASource.NumberNumberStart;
  FNumberCanContainDash := ASource.NumberCanContainDash;
  //
  len := ASource.CommentsDelimitersCount;
  SetLength(FCommentsDelimiters, len);
  for i := 0 to len - 1 do begin
    FCommentsDelimiters[i].&Type := ASource.CommentsDelimiters[i].&Type;
    FCommentsDelimiters[i].StartDelimiter := ASource.CommentsDelimiters[i].StartDelimiter;
    FCommentsDelimiters[i].EndDelimiter := ASource.CommentsDelimiters[i].EndDelimiter;
    FCommentsDelimiters[i].CanNest := ASource.CommentsDelimiters[i].CanNest;
    FCommentsDelimiters[i].CanBeNested := ASource.CommentsDelimiters[i].CanBeNested;
  end;
  FStringDelimiter := ASource.StringDelimiter;

  FKeywords.Assign(ASource.Keywords);
  FOperators.Assign(ASource.Operators);
  FPunctuation.Assign(ASource.Punctuation);

  FBooleanLiterals.Assign(ASource.BooleanLiterals);
  FNilLiterals.Assign(ASource.NilLiterals);

  FHexPrefix := ASource.HexPrefix;
  FHexSuffix := ASource.HexSuffix;
  FBinaryPrefix := ASource.BinaryPrefix;
  FBinarySuffix := ASource.BinarySuffix;
  FOctalPrefix := ASource.OctalPrefix;
  FOctalSuffix := ASource.OctalSuffix;
  FExtendedSuffix := ASource.FExtendedSuffix;

  FDecimalSeparator := ASource.DecimalSeparator;
  FAllowUnderscoreNumbers := ASource.AllowUnderscoreNumbers;

  FCaseSensitive := ASource.CaseSensitive;
end;

function TNEDTexerConfig.GetCommentsDelimitersCount: Integer;
begin
  Result := Length(FCommentsDelimiters);
end;

function TNEDTexerConfig.GetCommentsDelimiters(const Index: Integer): TNEDEditorDelimiterPair;
begin
  FillChar(Result, SizeOf(TNEDEditorDelimiterPair), 0);
  if (Index < 0) or (Index > High(FCommentsDelimiters)) then
    Exit;
  //
  Result := FCommentsDelimiters[Index];
end;

procedure TNEDTexerConfig.Reset;
begin
  FWhitespaceCharSet := ' '#9#10#13;

  FIdentifierCharSet := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'; // a..z, A..Z, 0..9, _
  FIdentifierUnderscoreStart := True; // _ident
  FIdentifierNumberStart := False; // 0ident
  FIdentifierCanContainDash := False; // some-type

  FNumberCharSet := '0123456789._xboe+-'; // 0..9, '.', '_', x/X, b/B, o/O, e/E, +, -
  FNumberUnderscoreStart := False; // _0123
  FNumberNumberStart := True; // 00C0FE
  FNumberCanContainDash := False; // 00-00

  FKeywords.Clear;
  FOperators.Clear;
  FPunctuation.Clear;
  FBooleanLiterals.Clear;
  FNilLiterals.Clear;

  FHexPrefix := '0x';
  FHexSuffix := 'h';
  FBinaryPrefix := '0b';
  FBinarySuffix := 'b';
  FOctalPrefix := '0o';
  FOctalSuffix := 'o';
  FExtendedSuffix := 'e';

  FDecimalSeparator := '.';
  FAllowUnderscoreNumbers := False;

  FCaseSensitive := False;

  // @TODO: NitroPascal / Pascal-like defaults can be added here
end;

{ TNEDTexer }

constructor TNEDTexer.Create(const ALexer: TNEDLexer);
begin
  inherited Create;
  FLexer := ALexer;
  FConfig := TNEDTexerConfig.Create;
  FTokens := TNEDTextTokenList.Create;

  FLine := 0;
  FText := '';
  FCurrentOffset := 1;
end;

destructor TNEDTexer.Destroy;
begin
  FTokens.Free;
  FConfig.Free;
  inherited Destroy;
end;

procedure TNEDTexer.Reset;
begin
  FLine := 0;
  FText := '';
  FCurrentOffset := 1;

  FTokens.Clear;

  FLexer.SetText(1, '');
end;

procedure TNEDTexer.SetText(const ALine: Integer; const AText: UTF8String);
begin
  FLine := ALine;
  FText := AText;
  FCurrentOffset := 1;
//  FTokens.Clear;
//  FLexer.SetText(ALine + 1, AText);
end;

function TNEDTexer.IsWhitespace(AChar: UTF8Char): Boolean;
begin
  Result := Pos(AChar, FConfig.FWhitespaceCharSet) > 0;
end;

function TNEDTexer.IsCommentDelimiterStart(AChar: UTF8Char): Boolean;
var
  start_len, i, idx: Integer;
  CommentDelimiters: TNEDEditorDelimiterPair;
begin
  Result := False;
  if AChar = #0 then
    Exit;
  //
  for i := 0 to FConfig.CommentsDelimitersCount - 1 do begin
    CommentDelimiters := FConfig.CommentsDelimiters[i];
    start_len := Length(CommentDelimiters.StartDelimiter);
    if (CommentDelimiters.&Type > ectNotInComment) and (start_len >= 1) then begin
      idx := 1;
      while (AChar <> #0) and (idx <= start_len) do begin
        if AChar = CommentDelimiters.StartDelimiter[idx] then begin
          Inc(idx);
          AChar := FLexer.PeekChar(idx - 1);
        end
        else
          Break;
      end;
      if idx > start_len then begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TNEDTexer.IsIdentifierStart(AChar: UTF8Char): Boolean;
begin
  if AChar = '_' then
    Exit(FConfig.FIdentifierUnderscoreStart);

  if ((AChar >= '0') and (AChar <= '9')) then
    Exit(FConfig.FIdentifierNumberStart);

  Result := Pos(AChar, FConfig.FIdentifierCharSet) > 0; // ((AChar >= 'A') and (AChar <= 'Z')) or ((AChar >= 'a') and (AChar <= 'z')) or (AChar = '_');
end;

function TNEDTexer.IsIdentifierChar(AChar: UTF8Char): Boolean;
begin
  Result := Pos(AChar, FConfig.FIdentifierCharSet) > 0;
end;

function TNEDTexer.IsNumberStart(AChar: UTF8Char): Boolean;
begin
  if AChar = '_' then
    Exit(FConfig.FNumberUnderscoreStart);

  Result := (AChar >= '0') and (AChar <= '9');
end;

function TNEDTexer.IsNumberChar(AChar: UTF8Char): Boolean;
begin
  Result := Pos(AChar, FConfig.FNumberCharSet) > 0;
end;

function TNEDTexer.IsOperatorChar(AChar: UTF8Char): Boolean;
begin
  Result := Pos(AChar, FConfig.Operators.ToString) > 0;
end;

function TNEDTexer.IsPunctuationChar(AChar: UTF8Char): Boolean;
begin
  Result := Pos(AChar, FConfig.Punctuation.ToString) > 0;
end;

function TNEDTexer.IsKeyword(const AText: string): Boolean;
begin
  Result := FConfig.Keywords.Contains(AText);
end;

function TNEDTexer.GetText(const AOffset, ALength: Integer): string;
begin
  if (AOffset < 1) or (ALength <= 0) or (AOffset > Length(FText)) then
    Exit('');

  Result := Copy(FText, AOffset, ALength);
end;

procedure TNEDTexer.AddToken(const AKind: TNEDTextTokenKindEnum; const AOffset, ALength: Integer);
var
  Token: TNEDTextToken;
begin
  if ALength <= 0 then
    Exit;

  FillChar(Token, SizeOf(Token), 0);

  Token.Kind := AKind;
  Token.Offset := AOffset;
  Token.Length := ALength;

  FTokens.Add(Token);
end;

procedure TNEDTexer.TexIdentifier;
var
  StartPosition: Integer;
  C: UTF8Char;
  S: string;
  TokenKind: TNEDTextTokenKindEnum;
begin
  StartPosition := FLexer.Position;

  while not FLexer.IsEOF do begin
    C := FLexer.PeekChar(0);
    if not IsIdentifierChar(C) then
      Break;
    if not FLexer.NextChar(C) then
      Break;
  end;

  S := GetText(StartPosition, FLexer.Position - StartPosition);
  if IsKeyword(S) then
    TokenKind := ttkKeyword
  else
    TokenKind := ttkIdentifier;

  if FLexer.Position > StartPosition then
    AddToken(TokenKind, StartPosition, FLexer.Position - StartPosition);
end;

procedure TNEDTexer.TexNumber;
var
  StartPosition: Integer;
  C: UTF8Char;
begin
  StartPosition := FLexer.Position;

  while not FLexer.IsEOF do begin
    C := FLexer.PeekChar(0);
    if not IsNumberChar(C) then
      Break;
    if not FLexer.NextChar(C) then
      Break;
  end;

  if FLexer.Position > StartPosition then
    AddToken(ttkNumber, StartPosition, FLexer.Position - StartPosition);
end;

procedure TNEDTexer.TexString;
var
  StartPosition: Integer;
  C: UTF8Char;
begin
  StartPosition := FLexer.Position;

  while not FLexer.IsEOF do begin
    C := FLexer.PeekChar(0);
    if FLexer.State.Mode = lmNormal then begin
      // the character which closed the string has already been consumed by the lexer
      if C = FConfig.StringDelimiter then begin
        Break;
      end;
    end;
    if not FLexer.NextChar(C) then
      Break;
  end;

  if FLexer.Position > StartPosition then
    AddToken(ttkString, StartPosition, FLexer.Position - StartPosition);

  // an unterminated string is perfectly normal while the user is editing source code
end;

procedure TNEDTexer.TexComment;
var
  StartPosition: Integer;
  C: UTF8Char;
begin
  StartPosition := FLexer.Position;

  while not FLexer.IsEOF do begin
    C := FLexer.PeekChar(0);
    // once the lexer returns to normal mode, the comment has ended
    if FLexer.State.Mode = lmNormal then
      Break;
    if not FLexer.NextChar(C) then
      Break;
  end;

  if FLexer.Position > StartPosition then
    AddToken(ttkComment, StartPosition, FLexer.Position - StartPosition);
end;

procedure TNEDTexer.TexOperator;
var
  StartPosition: Integer;
  C: UTF8Char;
begin
  StartPosition := FLexer.Position;

  while not FLexer.IsEOF do begin
    C := FLexer.PeekChar(0);
    if not IsOperatorChar(C) then
      Break;

    // @TODO: parametrize this
    // Comments and directives must be allowed to terminate an operator sequence.
    if (C = '/') and (FLexer.Position < Length(FText)) and (FLexer.PeekChar = '/') then
      Break;

    if (C = '/') and (FLexer.Position < Length(FText)) and (FLexer.PeekChar = '*') then
      Break;

    if not FLexer.NextChar(C) then
      Break;
  end;

  if FLexer.Position = StartPosition then begin
    FLexer.NextChar(C);
    AddToken(ttkOperator, StartPosition, 1);
    Exit;
  end;

  AddToken(ttkOperator, StartPosition, FLexer.Position - StartPosition);
end;

procedure TNEDTexer.TexWhitespace;
var
  C: UTF8Char;
begin
  if not FLexer.NextChar(C) then
    Exit;
  C := FLexer.PeekChar(0);
  if not IsWhitespace(C) then
    Exit;

  while FLexer.NextChar(C) do begin
    C := FLexer.PeekChar(0);
    if not IsWhitespace(C) then
      Break;
  end;
end;

procedure TNEDTexer.TexUnknown;
var
  StartPosition: Integer;
  C: UTF8Char;
begin
  StartPosition := FLexer.Position;

  if FLexer.NextChar(C) then
    AddToken(ttkOperator, StartPosition, FLexer.Position - StartPosition);
end;

procedure TNEDTexer.Tex;
var
  C: UTF8Char;
  StartPosition: Integer;
begin
  FTokens.Clear;

  FLexer.SetText(FLine + 1, FText);
  while not FLexer.IsEOF do begin
    StartPosition := FLexer.Position;
    // lexical state has priority over character classification
    case FLexer.State.Mode of
      lmString: begin
        TexString;
      end;

      lmComment: begin
        TexComment;
      end;

      lmDirective: begin
        // for now compiler directives are represented as comments at the token level,
        // later this can become its own token kind if required
        TexComment;
      end;

      lmNormal: begin
        C := FLexer.PeekChar(0); // FText[FLexer.Position];

        if IsWhitespace(C) then
          TexWhitespace
        else if IsIdentifierStart(C) then
          TexIdentifier
        else if IsNumberStart(C) then
          TexNumber
        else if IsOperatorChar(C) then
          TexOperator
        else if IsPunctuationChar(C) then begin
          // special handling for constructs which cause the lexer to enter another lexical state
          if C = FConfig.StringDelimiter then
            TexString
          else if IsCommentDelimiterStart(C) then // this checks all possible comments types
            TexComment
          else
            TexUnknown;
        end
        else
          TexUnknown;
      end;
    end;

    // safety against a malformed lexer/texer combination which does not advance the input
    if FLexer.Position = StartPosition then
      FLexer.NextChar(C);
  end;
end;

end.

