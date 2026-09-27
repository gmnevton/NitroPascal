//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-09
// All rights reserved.
//

unit ned_editor_texer;

interface

uses
  SysUtils,
  Classes,
  Graphics,
  Generics.Collections,
  ned_editor_lexer;

type
  TNEDTextTokenKindEnum = (
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

  TNEDTextLineBreakStyle = (tlbsNone, tlbsLF, tlbsCRLF);

  TNEDTokenFormat = record
    Font: TFont;
    TextAlignment: TVerticalAlignment; // vtop, vcenter, vbottom
    WordBreak: Boolean; // this character token is a line break
    WordWrap: Boolean; // internal; wrap line in this place while rendering
    LineBreak: String; // oryginal line break
    LineBreakType: TNEDTextLineBreakStyle;
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

  TNEDTextKeywordSet = class
  private
    FKeywords: TStringList;
    //
    function GetCount: Integer;
    function GetKeyword(AIndex: Integer): String;
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Clear;
    procedure Add(const AKeyword: String);
    procedure AddKeywords(const AKeywords: Array of String);
    //
    function Contains(const AKeyword: String): Boolean;
    //
    property Count: Integer read GetCount;
    property Keywords[AIndex: Integer]: String read GetKeyword;
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

{ Texer }

  TNEDTexer = class
  private
    FLexer: TNEDLexer;
    FKeywords: TNEDTextKeywordSet;
    //
    FLine: Integer;
    FText: UTF8String;
    FTokens: TNEDTextTokenList;
    //
    FCurrentOffset: Integer;
    //
    function IsIdentifierStart(AChar: UTF8Char): Boolean;
    function IsIdentifierChar(AChar: UTF8Char): Boolean;
    //
    function IsNumberStart(AChar: UTF8Char): Boolean;
    function IsNumberChar(AChar: UTF8Char): Boolean;
    //
    function IsWhitespace(AChar: UTF8Char): Boolean;
    //
    function IsOperatorChar(AChar: UTF8Char): Boolean;
    //
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
    property Keywords: TNEDTextKeywordSet read FKeywords;
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

{ TNEDTextKeywordSet }

constructor TNEDTextKeywordSet.Create;
begin
  inherited Create;
  FKeywords := TStringList.Create;

  FKeywords.CaseSensitive := False;
  FKeywords.Sorted := True;
  FKeywords.Duplicates := dupIgnore;
end;

destructor TNEDTextKeywordSet.Destroy;
begin
  FKeywords.Free;
  inherited Destroy;
end;

procedure TNEDTextKeywordSet.Clear;
begin
  FKeywords.Clear;
end;

procedure TNEDTextKeywordSet.Add(const AKeyword: string);
begin
  if AKeyword = '' then
    Exit;

  FKeywords.Add(AKeyword);
end;

procedure TNEDTextKeywordSet.AddKeywords(const AKeywords: array of string);
var
  I: Integer;
begin
  for I := Low(AKeywords) to High(AKeywords) do
    Add(AKeywords[I]);
end;

function TNEDTextKeywordSet.Contains(const AKeyword: string): Boolean;
begin
  Result := FKeywords.IndexOf(AKeyword) >= 0;
end;

function TNEDTextKeywordSet.GetCount: Integer;
begin
  Result := FKeywords.Count;
end;

function TNEDTextKeywordSet.GetKeyword(AIndex: Integer): string;
begin
  Result := FKeywords[AIndex];
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

{ TNEDTexer }

constructor TNEDTexer.Create(const ALexer: TNEDLexer);
begin
  inherited Create;
  FLexer := ALexer;
  FKeywords := TNEDTextKeywordSet.Create;
  FTokens := TNEDTextTokenList.Create;

  FLine := 0;
  FText := '';
  FCurrentOffset := 1;
end;

destructor TNEDTexer.Destroy;
begin
  FTokens.Free;
  FKeywords.Free;
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

function TNEDTexer.IsIdentifierStart(AChar: UTF8Char): Boolean;
begin
  Result := ((AChar >= 'A') and (AChar <= 'Z')) or ((AChar >= 'a') and (AChar <= 'z')) or (AChar = '_');
end;

function TNEDTexer.IsIdentifierChar(AChar: UTF8Char): Boolean;
begin
  Result := IsIdentifierStart(AChar) or ((AChar >= '0') and (AChar <= '9'));
end;

function TNEDTexer.IsNumberStart(AChar: UTF8Char): Boolean;
begin
  Result := (AChar >= '0') and (AChar <= '9');
end;

function TNEDTexer.IsNumberChar(AChar: UTF8Char): Boolean;
begin
  Result := ((AChar >= '0') and (AChar <= '9')) or
            (AChar = '.') or
            (AChar = '_') or
            (AChar = 'x') or
            (AChar = 'X') or
            (AChar = 'b') or
            (AChar = 'B') or
            (AChar = 'e') or
            (AChar = 'E') or
            (AChar = '+') or
            (AChar = '-');
end;

function TNEDTexer.IsWhitespace(AChar: UTF8Char): Boolean;
begin
  Result := (AChar = ' ') or (AChar = #9) or (AChar = #10) or (AChar = #13);
end;

function TNEDTexer.IsOperatorChar(AChar: UTF8Char): Boolean;
begin
  Result := Pos(AChar, '+-*/=<>:;,.()[]{}^@&|~%!?') > 0;
end;

function TNEDTexer.IsKeyword(const AText: string): Boolean;
begin
  Result := FKeywords.Contains(AText);
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
  CurrentPosition: Integer;
  C: UTF8Char;
  S: string;
  TokenKind: TNEDTextTokenKindEnum;
begin
  StartPosition := FLexer.Position;
  CurrentPosition := StartPosition;

  while CurrentPosition <= Length(FText) do begin
    C := FText[CurrentPosition];
    if not IsIdentifierChar(C) then
      Break;

    Inc(CurrentPosition);
  end;

  S := GetText(StartPosition, CurrentPosition - StartPosition);
  if IsKeyword(S) then
    TokenKind := ttkKeyword
  else
    TokenKind := ttkIdentifier;

  AddToken(TokenKind, StartPosition, CurrentPosition - StartPosition);

  while FLexer.Position < CurrentPosition do
    FLexer.NextChar(C);
end;

procedure TNEDTexer.TexNumber;
var
  StartPosition: Integer;
  CurrentPosition: Integer;
  C: UTF8Char;
begin
  StartPosition := FLexer.Position;
  CurrentPosition := StartPosition;

  while CurrentPosition <= Length(FText) do begin
    C := FText[CurrentPosition];
    if not IsNumberChar(C) then
      Break;

    Inc(CurrentPosition);
  end;

  AddToken(ttkNumber, StartPosition, CurrentPosition - StartPosition);

  while FLexer.Position < CurrentPosition do
    FLexer.NextChar(C);
end;

procedure TNEDTexer.TexString;
var
  StartPosition: Integer;
  C: UTF8Char;
  Finished: Boolean;
begin
  StartPosition := FLexer.Position;
  Finished := False;

  while not FLexer.IsEOF do begin
    if not FLexer.NextChar(C) then
      Break;

    if FLexer.State.Mode = lmNormal then begin
      // The character which closed the string has already been consumed by the lexer.
      if C = '''' then begin
        Finished := True;
        Break;
      end;

      if C = '"' then begin
        Finished := True;
        Break;
      end;
    end;
  end;

  if FLexer.Position > StartPosition then
    AddToken(ttkString, StartPosition, FLexer.Position - StartPosition);

  // Finished is intentionally not used to force an error.
  //
  // An unterminated string is perfectly normal while the
  // user is editing source code.
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

    // Once the lexer returns to normal mode, the comment has ended.
    if FLexer.State.Mode = lmNormal then
      Break;
  end;

  if FLexer.Position > StartPosition then
    AddToken(ttkComment, StartPosition, FLexer.Position - StartPosition);
end;

procedure TNEDTexer.TexOperator;
var
  StartPosition: Integer;
  CurrentPosition: Integer;
  C: UTF8Char;
begin
  StartPosition := FLexer.Position;
  CurrentPosition := StartPosition;

  while CurrentPosition <= Length(FText) do begin
    C := FText[CurrentPosition];
    if not IsOperatorChar(C) then
      Break;

    // Comments and directives must be allowed to terminate an operator sequence.
    if (C = '/') and (CurrentPosition < Length(FText)) and (FText[CurrentPosition + 1] = '/') then
      Break;

    if (C = '/') and (CurrentPosition < Length(FText)) and (FText[CurrentPosition + 1] = '*') then
      Break;

    Inc(CurrentPosition);
  end;

  if CurrentPosition = StartPosition then begin
    FLexer.NextChar(C);
    AddToken(ttkOperator, StartPosition, 1);
    Exit;
  end;

  AddToken(ttkOperator, StartPosition, CurrentPosition - StartPosition);

  while FLexer.Position < CurrentPosition do
    FLexer.NextChar(C);
end;

procedure TNEDTexer.TexWhitespace;
var
  C: UTF8Char;
begin
  while not FLexer.IsEOF do begin
    C := FText[FLexer.Position];
    if not IsWhitespace(C) then
      Break;

    FLexer.NextChar(C);
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
    // Lexical state has priority over character classification.
    case FLexer.State.Mode of
      lmString: begin
        TexString;
      end;

      lmComment: begin
        TexComment;
      end;

      lmDirective: begin
        // For now compiler directives are represented as comments at the token level.
        // Later this can become its own token kind if required.
        TexComment;
      end;

      lmNormal: begin
        C := FText[FLexer.Position];
        if IsWhitespace(C) then
          TexWhitespace
        else if IsIdentifierStart(C) then
          TexIdentifier
        else if IsNumberStart(C) then
          TexNumber
        else if IsOperatorChar(C) then begin
          // Special handling for constructs which cause the lexer to enter another lexical state.
          if ((C = '''') or (C = '"')) then
            TexString
          else if (C = '/') and (FLexer.PeekChar = '/') then
            TexComment
          else if (C = '/') and (FLexer.PeekChar = '*') then
            TexComment
          else if (C = '{') and (FLexer.PeekChar = '$') then
            TexComment
          else if C = '{' then
            TexComment
          else
            TexOperator;
        end
        else
          TexUnknown;
      end;
    end;

    // Safety against a malformed lexer/texer combination which does not advance the input.
    if FLexer.Position = StartPosition then
      FLexer.NextChar(C);
  end;
end;

end.

