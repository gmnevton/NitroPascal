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
    ttkUnknown,
    ttkDefault,
    ttkWhitespace,
    ttkKeyword,
    ttkIdentifier,
    ttkString,
    ttkNumber,
    ttkPunctuation,
    ttkComment,
    ttkDirective,
    ttkDirectiveKeyword,
    ttkCompilerVariable,
    ttkOperator
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

  TNEDTextSetArray = TArray<String>;
  TNEDTextSetCharSet = TArray<Char>;
  TNEDIntegerArray = TArray<Integer>;
  TNEDLengthIndex = TArray<TNEDIntegerArray>;

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
    FLengthIndex: TNEDLengthIndex;
    //
    function GetCount: Integer;
    function GetTexts(AIndex: Integer): String;
    function GetCaseSensitive: Boolean;
    procedure SetCaseSensitive(const Value: Boolean);
    function BuildLengthIndex(const AItems: TNEDTextSetArray): TNEDLengthIndex;
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
    function Contains(const ASearch: String): Boolean;
    function PosInArrayExactIndexed(const ASearch: String): Integer;
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

    FDirectiveKeywords: TNEDTextSet;
    FCompilerVariableKeywords: TNEDTextSet;
    FDirectiveCanContainDash: Boolean; // {$program-type Windows32 GUI} {$search-path '..\\..\\concepts\\'}
    FCompilerVariableCanContainDash: Boolean; // %project-name%

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
    procedure Clear;
    procedure Reset;
    procedure ClearCommentDelimiters;
    procedure AddCommentDelimiter(const ADelimiter: TNEDLexicalDelimiterPair); overload;
    procedure AddCommentDelimiter(const AType: TNEDLexicalCommentTypeEnum; const AStartDelimiter, AEndDelimiter: String; const ACanNest, ACanBeNested: Boolean); overload;
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
    //
    property BooleanLiterals: TNEDTextSet read FBooleanLiterals;
    property NilLiterals: TNEDTextSet read FNilLiterals;
    //
    property DirectiveKeywords: TNEDTextSet read FDirectiveKeywords;
    property CompilerVariableKeywords: TNEDTextSet read FCompilerVariableKeywords;
    property DirectiveCanContainDash: Boolean read FDirectiveCanContainDash write FDirectiveCanContainDash;
    property CompilerVariableCanContainDash: Boolean read FCompilerVariableCanContainDash write FCompilerVariableCanContainDash;
    //
    property HexPrefix: UTF8String read FHexPrefix write FHexPrefix;
    property HexSuffix: UTF8String read FHexSuffix write FHexSuffix;
    property BinaryPrefix: UTF8String read FBinaryPrefix write FBinaryPrefix;
    property BinarySuffix: UTF8String read FBinarySuffix write FBinarySuffix;
    property OctalPrefix: UTF8String read FOctalPrefix write FOctalPrefix;
    property OctalSuffix: UTF8String read FOctalSuffix write FOctalSuffix;
    property ExtendedSuffix: UTF8String read FExtendedSuffix write FExtendedSuffix;
    //
    property DecimalSeparator: UTF8Char read FDecimalSeparator write FDecimalSeparator;
    property AllowUnderscoreNumbers: Boolean read FAllowUnderscoreNumbers write FAllowUnderscoreNumbers;
    //
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
    function IsDirectiveDelimiterStart(AChar: UTF8Char): Boolean;
    function IsCompilerVariableDelimiterStart(AChar: UTF8Char): Boolean;
    //
    function IsIdentifierStart(AChar: UTF8Char): Boolean;
    function IsIdentifierChar(AChar: UTF8Char): Boolean;
    //
    function IsNumberStart(AChar: UTF8Char): Boolean;
    function IsNumberChar(AChar: UTF8Char): Boolean;
    //
    function IsOperatorChar(AChar: UTF8Char): Boolean;
    function IsPunctuationChar(AChar: UTF8Char): Boolean;
    function IsKeyword(const AIdentifier: UTF8String): Boolean;
    function IsDirectiveKeyword(const AIdentifier: UTF8String): Boolean;
    function IsCompilerVariableKeyword(const AIdentifier: UTF8String): Boolean;
    //
    function GetText(const AOffset, ALength: Integer): UTF8String;
    //
    procedure AddToken(const AKind: TNEDTextTokenKindEnum; const AOffset, ALength: Integer);
    //
    procedure TexWhitespace;
    procedure TexIdentifier;
    procedure TexNumber;
    procedure TexString(const AExitMode: TNEDLexicalModeEnum);
    procedure TexComment;
    procedure TexDirective;
    procedure TexDirectiveIdentifier;
    procedure TexCompilerVariable;
    procedure TexCompilerVariableIdentifier;
    procedure TexOperator;
    procedure TexPunctuation;
    //
    procedure TexUnknown;
  public
    constructor Create(const ALexer: TNEDLexer);
    destructor Destroy; override;
    //
    procedure Init;
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

function TNEDTextSet.BuildLengthIndex(const AItems: TNEDTextSetArray): TNEDLengthIndex;
var
  Counts: TNEDIntegerArray;
  NextPos: TNEDIntegerArray;
  I, L, MaxLen: Integer;
begin
  MaxLen := 0;

  for I := 0 to High(AItems) do
    if Length(AItems[I]) > MaxLen then
      MaxLen := Length(AItems[I]);

  // zero index is not used, so add one for 1-based arrays
  SetLength(Result, MaxLen + 1);
  SetLength(Counts, MaxLen + 1);
  SetLength(NextPos, MaxLen + 1);

  // count items of each length
  for I := 0 to High(AItems) do begin
    L := Length(AItems[I]);
    if L > 0 then
      Inc(Counts[L]);
  end;

  // allocate each group exactly once
  for L := 1 to MaxLen do
    SetLength(Result[L], Counts[L]);

  // fill groups with original, zero-based indexes
  for I := 0 to High(AItems) do begin
    L := Length(AItems[I]);
    if L = 0 then
      Continue;

    Result[L][NextPos[L]] := I;
    Inc(NextPos[L]);
  end;
end;

procedure TNEDTextSet.Clear;
begin
  FTextSet.Clear;
  ClearCache;
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
  //
  SetLength(FLengthIndex, 0);
end;

procedure TNEDTextSet.Assign(const ATextSet: TNEDTextSet);
begin
  FTextSet.Assign(ATextSet.FTextSet);
  FTextSet.CaseSensitive := ATextSet.CaseSensitive;
  ClearCache;
  FLengthIndex := BuildLengthIndex(ToArray);
end;

procedure TNEDTextSet.Assign(const AStringList: TStringList);
begin
  FTextSet.Assign(AStringList);
  FTextSet.CaseSensitive := AStringList.CaseSensitive;
  ClearCache;
  FLengthIndex := BuildLengthIndex(ToArray);
end;

procedure TNEDTextSet.Add(const AText: String);
begin
  if AText = '' then
    Exit;

  FTextSet.Add(AText);
  ClearCache;
  FLengthIndex := BuildLengthIndex(ToArray);
end;

procedure TNEDTextSet.AddText(const ATextSet: TNEDTextSetArray);
var
  I: Integer;
begin
  for I := Low(ATextSet) to High(ATextSet) do
    Add(ATextSet[I]);
  ClearCache;
  FLengthIndex := BuildLengthIndex(ToArray);
end;

function TNEDTextSet.Contains(const ASearch: String): Boolean;
begin
  Result := FTextSet.IndexOf(ASearch) >= 0;
end;

function TNEDTextSet.PosInArrayExactIndexed(const ASearch: String): Integer;
var
  I, L: Integer;
begin
  Result := -1;
  L := Length(ASearch);

  if (L = 0) or not FTextSetArrayCacheValid or (L >= Length(FLengthIndex)) then
    Exit;

  for I := 0 to High(FLengthIndex[L]) do
    if FTextSetArrayCache[FLengthIndex[L][I]] = ASearch then
      Exit(FLengthIndex[L][I]);
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
  FDirectiveKeywords := TNEDTextSet.Create;
  FCompilerVariableKeywords := TNEDTextSet.Create;

  Reset;
end;

destructor TNEDTexerConfig.Destroy;
begin
  FCompilerVariableKeywords.Free;
  FDirectiveKeywords.Free;
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

  FDirectiveKeywords.Assign(ASource.DirectiveKeywords);
  FCompilerVariableKeywords.Assign(ASource.CompilerVariableKeywords);
  FDirectiveCanContainDash := ASource.DirectiveCanContainDash;
  FCompilerVariableCanContainDash := ASource.CompilerVariableCanContainDash;

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

procedure TNEDTexerConfig.Clear;
begin
  FWhitespaceCharSet := ''; // ' ', #9, #10, #13
  //
  FIdentifierCharSet := ''; // a..z, A..Z, 0..9, _
  FIdentifierUnderscoreStart := False; // _ident
  FIdentifierNumberStart := False; // 0ident
  FIdentifierCanContainDash := False; // some-type
  //
  FNumberCharSet := ''; // 0..9, '.', '_', x/X, b/B, o/O, e/E, +, -
  FNumberUnderscoreStart := False; // _0123
  FNumberNumberStart := False; // 00C0FE
  FNumberCanContainDash := False; // 00-00
  //
  SetLength(FCommentsDelimiters, 0);
  FStringDelimiter := #0; // " or '

  FKeywords.Clear;
  FOperators.Clear;
  FPunctuation.Clear;

  FBooleanLiterals.Clear;
  FNilLiterals.Clear;

  FDirectiveKeywords.Clear;
  FCompilerVariableKeywords.Clear;
  FDirectiveCanContainDash := False; // {$program-type Windows32 GUI} {$search-path '..\\..\\concepts\\'}
  FCompilerVariableCanContainDash := False; // %project-name%

  FHexPrefix := ''; // 0xABCD, $ABCD
  FHexSuffix := ''; // 0ABCDH or 0ABCDh - assembler
  FBinaryPrefix := ''; // 0b01010101
  FBinarySuffix := ''; // 01010101b
  FOctalPrefix := ''; // 0o01234567
  FOctalSuffix := ''; // 01234567o
  FExtendedSuffix := ''; // 1.123e4 or 1.123E4

  FDecimalSeparator := #0;
  FAllowUnderscoreNumbers := False;

  FCaseSensitive := False;
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

  SetLength(FCommentsDelimiters, 0);
  AddCommentDelimiter(ectSingleLine, '//', '',     True, True);
  AddCommentDelimiter(ectMultiLine,  '/.', './',   True, True);
  AddCommentDelimiter(ectMultiLine,  '{.', '.}',   True, True);
  AddCommentDelimiter(ectMultiLine,  '(*', '*)',   True, True);
  // special case comments
  AddCommentDelimiter(ectSingleLine, '//-', '',    True, False);
  AddCommentDelimiter(ectMultiLine,  '//+', '+//', True, False);

  FStringDelimiter := #0; // " or '

  FKeywords.Clear;
  FOperators.Clear;
  FPunctuation.Clear;
  FBooleanLiterals.Clear;
  FNilLiterals.Clear;
  FDirectiveKeywords.Clear;
  FCompilerVariableKeywords.Clear;
  FDirectiveCanContainDash := False; // {$program-type Windows32 GUI} {$search-path '..\\..\\concepts\\'}
  FCompilerVariableCanContainDash := False; // %project-name%

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

procedure TNEDTexerConfig.ClearCommentDelimiters;
var
  i: Integer;
begin
  for i := 0 to High(FCommentsDelimiters) do begin
    FCommentsDelimiters[i].StartDelimiter := '';
    FCommentsDelimiters[i].EndDelimiter := '';
  end;

  SetLength(FCommentsDelimiters, 0);
end;

procedure TNEDTexerConfig.AddCommentDelimiter(const ADelimiter: TNEDLexicalDelimiterPair);
begin
  AddCommentDelimiter(ADelimiter.&Type, ADelimiter.StartDelimiter, ADelimiter.EndDelimiter, ADelimiter.CanNest, ADelimiter.CanBeNested);
end;

procedure TNEDTexerConfig.AddCommentDelimiter(const AType: TNEDLexicalCommentTypeEnum; const AStartDelimiter, AEndDelimiter: String; const ACanNest, ACanBeNested: Boolean);
var
  idx: Integer;
begin
  idx := Length(FCommentsDelimiters);
  SetLength(FCommentsDelimiters, idx + 1);
  //
  FCommentsDelimiters[idx].&Type          := AType;
  FCommentsDelimiters[idx].StartDelimiter := AStartDelimiter;
  FCommentsDelimiters[idx].EndDelimiter   := AEndDelimiter;
  FCommentsDelimiters[idx].CanNest        := ACanNest;
  FCommentsDelimiters[idx].CanBeNested    := ACanBeNested;
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

procedure TNEDTexer.Init;
begin
  FLine := 0;
  FText := '';
  FCurrentOffset := 1;

  FTokens.Clear;

  FLexer.Clear;
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
begin
  Result := FLexer.IsCommentStart;
end;

function TNEDTexer.IsDirectiveDelimiterStart(AChar: UTF8Char): Boolean;
begin
  Result := FLexer.IsDirectiveStart;
end;

function TNEDTexer.IsCompilerVariableDelimiterStart(AChar: UTF8Char): Boolean;
begin
  Result := FLexer.IsCompilerVariableStart;
end;

function TNEDTexer.IsIdentifierStart(AChar: UTF8Char): Boolean;
begin
  if AChar = '_' then
    Exit(FConfig.FIdentifierUnderscoreStart);

  if ((AChar >= '0') and (AChar <= '9')) then
    Exit(FConfig.FIdentifierNumberStart);

  Result := Pos(AChar, FConfig.FIdentifierCharSet) > 0;
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
  Result := FConfig.Operators.PosInArrayExactIndexed(AChar) > -1;
end;

function TNEDTexer.IsPunctuationChar(AChar: UTF8Char): Boolean;
begin
  Result := FConfig.Punctuation.PosInArrayExactIndexed(AChar) > -1;
end;

function TNEDTexer.IsKeyword(const AIdentifier: UTF8String): Boolean;
begin
  Result := FConfig.Keywords.Contains(AIdentifier);
end;

function TNEDTexer.IsDirectiveKeyword(const AIdentifier: UTF8String): Boolean;
begin
  Result := FConfig.DirectiveKeywords.Contains(AIdentifier);
end;

function TNEDTexer.IsCompilerVariableKeyword(const AIdentifier: UTF8String): Boolean;
begin
  Result := FConfig.CompilerVariableKeywords.Contains(AIdentifier);
end;

function TNEDTexer.GetText(const AOffset, ALength: Integer): UTF8String;
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

procedure TNEDTexer.TexIdentifier;
var
  StartPosition: Integer;
  C: UTF8Char;
  Identifier: UTF8String;
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

  Identifier := GetText(StartPosition, FLexer.Position - StartPosition);
  if IsKeyword(Identifier) then
    TokenKind := ttkKeyword
  else
    TokenKind := ttkIdentifier;
  Identifier := '';

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

procedure TNEDTexer.TexString(const AExitMode: TNEDLexicalModeEnum);
var
  StartPosition: Integer;
  C: UTF8Char;
begin
  StartPosition := FLexer.Position;

  while not FLexer.IsEOF do begin
    if not FLexer.NextChar(C) then
      Break;

    if FLexer.State.Mode = AExitMode then begin
      // the character which closed the string has already been consumed by the lexer
      if C = FConfig.StringDelimiter then begin
        Break;
      end;
    end;
  end;

  if FLexer.Position > StartPosition then
    AddToken(ttkString, StartPosition, FLexer.Position - StartPosition)
  // an unterminated string is perfectly normal while the user is editing source code
//  else if FLexer.Position = StartPosition then
//    AddToken(ttkString, StartPosition, 1);
  else
    raise Exception.Create('Tokenizing string of size 0, probably string punctuation char only !');
end;

procedure TNEDTexer.TexComment;
var
  StartPosition: Integer;
  C: UTF8Char;
begin
  StartPosition := FLexer.Position;

  while not FLexer.IsEOF do begin
    if not FLexer.NextChar(C) then
      Break;
    // once the lexer returns to normal mode, the comment has ended
    if FLexer.State.Mode = lmNormal then
      Break;
  end;

  if FLexer.Position > StartPosition then
    AddToken(ttkComment, StartPosition, FLexer.Position - StartPosition);
end;

procedure TNEDTexer.TexDirective;
var
  C: UTF8Char;
  StartPosition: Integer;
  TokenKind: TNEDTextTokenKindEnum;
begin
  StartPosition := FLexer.Position;

  // opening delimiter: for example, "{$"
  if FLexer.State.Mode = lmNormal then begin
    TokenKind := ttkDirective;

    FLexer.NextChar(C);

    // consume any remaining characters of the opening delimiter
    while not FLexer.IsEOF do begin
      if FLexer.State.Mode = lmDirective then
        Break;

      FLexer.NextChar(C);
    end;

    if FLexer.Position > StartPosition then
      AddToken(TokenKind, StartPosition, FLexer.Position - StartPosition);
    Exit;
  end;

  // closing delimiter: for example, "}"
  if FLexer.IsDirectiveEnd then begin
    TokenKind := ttkDirective;

    FLexer.NextChar(C);

    if FLexer.Position > StartPosition then
      AddToken(TokenKind, StartPosition, FLexer.Position - StartPosition);
    Exit;
  end;

  // Inside a directive, emit ordinary individual tokens.
  C := FLexer.PeekChar(0);

  if IsWhitespace(C) then
    TexWhitespace
  else if IsIdentifierStart(C) then
    TexDirectiveIdentifier
  else if IsNumberStart(C) then
    TexNumber
  else if IsOperatorChar(C) then
    TexOperator
  else if IsPunctuationChar(C) then begin
    // special handling for constructs which cause the lexer to enter another lexical state
    if C = FConfig.StringDelimiter then
      TexString(lmDirective)
    else
      TexPunctuation
  end
  else
    TexUnknown;
end;

procedure TNEDTexer.TexDirectiveIdentifier;
var
  C: UTF8Char;
  StartPosition: Integer;
  Identifier: UTF8String;
  TokenKind: TNEDTextTokenKindEnum;
begin
  StartPosition := FLexer.Position;

  // consume the complete identifier
  while not FLexer.IsEOF do begin
    C := FLexer.PeekChar(0);
    if not IsIdentifierChar(C) and not (FConfig.DirectiveCanContainDash and (C = '-')) then
      Break;

    if not FLexer.NextChar(C) then
      Break;
  end;

  // default classification inside a directive
  TokenKind := ttkIdentifier;
  Identifier := GetText(StartPosition, FLexer.Position - StartPosition);
  // check the directive-specific keyword list
  if IsDirectiveKeyword(Identifier) then
    TokenKind := ttkDirectiveKeyword;
  Identifier := '';

  if FLexer.Position > StartPosition then
    AddToken(TokenKind, StartPosition, FLexer.Position - StartPosition);
end;

procedure TNEDTexer.TexCompilerVariable;
var
  C: UTF8Char;
  StartPosition: Integer;
begin
  StartPosition := FLexer.Position;

  // opening delimiter: for example, "{$"
  if FLexer.State.Mode = lmNormal then begin
    FLexer.NextChar(C);

    // consume any remaining characters of the opening delimiter
    while not FLexer.IsEOF do begin
      if FLexer.State.Mode = lmCompilerVariable then
        Break;

      FLexer.NextChar(C);
    end;

    if FLexer.Position > StartPosition then
      AddToken(ttkCompilerVariable, StartPosition, FLexer.Position - StartPosition);
    Exit;
  end;

  // closing delimiter: for example, "}"
  if FLexer.IsCompilerVariableEnd then begin
    FLexer.NextChar(C);

    if FLexer.Position > StartPosition then
      AddToken(ttkCompilerVariable, StartPosition, FLexer.Position - StartPosition);
    Exit;
  end;

  // Inside a directive, emit ordinary individual tokens.
  C := FLexer.PeekChar(0);

  if IsWhitespace(C) then
    TexWhitespace
  else if IsIdentifierStart(C) then
    TexCompilerVariableIdentifier
  else if IsNumberStart(C) then
    TexNumber
  else if IsOperatorChar(C) then
    TexOperator
  else if IsPunctuationChar(C) then begin
    // special handling for constructs which cause the lexer to enter another lexical state
//    if C = FConfig.StringDelimiter then
//      TexString(lmDirective)
//    else
      TexPunctuation
  end
  else
    TexUnknown;
end;

procedure TNEDTexer.TexCompilerVariableIdentifier;
var
  C: UTF8Char;
  StartPosition: Integer;
  Identifier: UTF8String;
  TokenKind: TNEDTextTokenKindEnum;
begin
  StartPosition := FLexer.Position;

  // consume the complete identifier
  while not FLexer.IsEOF do begin
    C := FLexer.PeekChar(0);
    if not IsIdentifierChar(C) and not (FConfig.CompilerVariableCanContainDash and (C = '-')) then
      Break;

    if not FLexer.NextChar(C) then
      Break;
  end;

  // default classification inside a directive
  TokenKind := ttkIdentifier;
  Identifier := GetText(StartPosition, FLexer.Position - StartPosition);
  // check the directive-specific keyword list
  if IsCompilerVariableKeyword(Identifier) then
    TokenKind := ttkCompilerVariable;
  Identifier := '';

  if FLexer.Position > StartPosition then
    AddToken(TokenKind, StartPosition, FLexer.Position - StartPosition);
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

    // comments and directives must be allowed to terminate an operator sequence
    if IsCommentDelimiterStart(C) then
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

procedure TNEDTexer.TexPunctuation;
var
  StartPosition: Integer;
  C: UTF8Char;
begin
  StartPosition := FLexer.Position;

  if FLexer.NextChar(C) then
    AddToken(ttkPunctuation, StartPosition, FLexer.Position - StartPosition);
end;

procedure TNEDTexer.TexUnknown;
var
  StartPosition: Integer;
  C: UTF8Char;
begin
  StartPosition := FLexer.Position;

  if FLexer.NextChar(C) then
    AddToken(ttkUnknown, StartPosition, FLexer.Position - StartPosition);
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
        TexString(lmNormal);
      end;

      lmComment: begin
        TexComment;
      end;

      lmDirective: begin
        TexDirective;
      end;

      lmCompilerVariable: begin
        TexCompilerVariable;
      end;

      lmNormal: begin
        C := FLexer.PeekChar(0); // FText[FLexer.Position];

        if IsWhitespace(C) then
          TexWhitespace
        else if IsDirectiveDelimiterStart(C) then
          TexDirective
        else if IsCompilerVariableDelimiterStart(C) then
          TexCompilerVariable
        else if IsCommentDelimiterStart(C) then
          TexComment
        else if IsIdentifierStart(C) then
          TexIdentifier
        else if IsNumberStart(C) then
          TexNumber
        else if IsOperatorChar(C) then
          TexOperator
        else if IsPunctuationChar(C) then begin
          // special handling for constructs which cause the lexer to enter another lexical state
          if C = FConfig.StringDelimiter then
            TexString(lmNormal)
          else if IsCommentDelimiterStart(C) then // this checks all possible comments types
            TexComment
          else
            TexPunctuation;
        end
        else
          TexUnknown;
      end;
    end;

    // safety against a malformed lexer/texer combination which does not advance the input
    if FLexer.Position = StartPosition then
      FLexer.NextChar(C);
  end;
  if (FLexer.State.Mode = lmComment) and (FLexer.State.CommentKind = lckLine) then begin
    FLexer.LeaveMode;
  end;
end;

end.

