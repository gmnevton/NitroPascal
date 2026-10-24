//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-10
// All rights reserved.
//

unit ned_editor_lexer;

interface

uses
  SysUtils,
  ned_editor_types;

type
  TNEDLexicalModeEnum = (
    lmNormal,
    lmString,
    lmComment,
    lmDirective,
    lmCompilerVariable,
    lmNotation
  );

  TNEDLexicalCommentKindEnum = (
    lckNone,
    lckLine,
    lckMultiLine
  );

  TNEDLexicalStringKindEnum = (
    lskNone,
    lskSingleQuote,
    lskDoubleQuote
  );

const
  NEDMaxLexicalModeDepth = 8;

type
  TNEDLexicalModeStack = Array[0..NEDMaxLexicalModeDepth - 1] of TNEDLexicalModeEnum;

  TNEDLexicalState = record
    Mode: TNEDLexicalModeEnum;
    //
    // modes to resume when a nested mode ends
    ModeStack: TNEDLexicalModeStack;
    ModeDepth: Byte;
    //
    StringDelimiter: UTF8Char;
    //
    CommentKind: TNEDLexicalCommentKindEnum;
    CommentEndDelimiter: UTF8String;
    CommentCanNest: Boolean;
    CommentCanBeNested: Boolean;
    CommentDepth: Integer;
    //
    //CompilerDirective: UTF8String; // stores the compiler directive text: {$IF ....}
    DirectiveEndDelimiter: UTF8String;
    DirectiveDepth: Integer;
    //
    //CompilerVariableDirective: UTF8String; // stores the compiler variable text: %....%
    CompilerVariableEndDelimiter: UTF8String;
    CompilerVariableDepth: Integer;
  end;

  TNEDSourceLocation = record
    Line: Integer;
    Column: Integer;
    Length: Integer;
  end;

  TNEDSourceRange = record
    StartPos: TNEDSourceLocation;
    EndPos: TNEDSourceLocation;
  end;

  TNEDLexicalCommentTypeEnum = TNEDEditorCommentTypeEnum;
  TNEDLexicalDelimiterPair = TNEDEditorDelimiterPair;

  TNEDLexerConfig = class
  private
    FLineBreak: UTF8String; // CRLF or LF or CR
    FStringDelimiter: UTF8Char; // " or '
    FAllowStringDelimiterNesting: Boolean;
    FNestedStringDelimiterEscapeing: Boolean;
    FCommentsDelimiters: Array of TNEDLexicalDelimiterPair; // see the compiler\concepts\comment\comment.npc file for detail explanation
    FDirectiveDelimiter: TNEDLexicalDelimiterPair; // '{$' - directive begin; '}' directive end
    FDirectiveCanContainDash: Boolean; // {$program-type Windows32 GUI} {$search-path '..\\..\\concepts\\'}
    FCompilerVariableDelimiter: TNEDLexicalDelimiterPair; // '%' - directive begin; '%' directive end
    FCompilerVariableCanContainDash: Boolean; // %program-type%
    //
    FAllowNestedComments: Boolean;
    FAllowMultilineStrings: Boolean;
    FAllowMultilineComments: Boolean;
  public
    constructor Create;
    procedure Assign(const ASource: TNEDLexerConfig);
    //
    procedure Clear;
    procedure Reset;
    procedure ClearCommentDelimiters;
    procedure AddCommentDelimiter(const ADelimiter: TNEDLexicalDelimiterPair); overload;
    procedure AddCommentDelimiter(const AType: TNEDLexicalCommentTypeEnum; const AStartDelimiter, AEndDelimiter: String; const ACanNest, ACanBeNested: Boolean); overload;
    //
    // properties
    property LineBreak: UTF8String read FLineBreak write FLineBreak;
    property StringDelimiter: UTF8Char read FStringDelimiter write FStringDelimiter;
    property AllowStringDelimiterNesting: Boolean read FAllowStringDelimiterNesting write FAllowStringDelimiterNesting;
    property NestedStringDelimiterEscapeing: Boolean read FNestedStringDelimiterEscapeing write FNestedStringDelimiterEscapeing;

    //property FCommentsDelimiter: Array of TNEDLexicalDelimiterPair;
    property DirectiveDelimiter: TNEDLexicalDelimiterPair read FDirectiveDelimiter write FDirectiveDelimiter;
    property DirectiveCanContainDash: Boolean read FDirectiveCanContainDash write FDirectiveCanContainDash;
    property CompilerVariableDelimiter: TNEDLexicalDelimiterPair read FCompilerVariableDelimiter write FCompilerVariableDelimiter;
    property CompilerVariableCanContainDash: Boolean read FCompilerVariableCanContainDash write FCompilerVariableCanContainDash;

    property AllowNestedComments: Boolean read FAllowNestedComments write FAllowNestedComments;
    property AllowMultilineStrings: Boolean read FAllowMultilineStrings write FAllowMultilineStrings;
    property AllowMultilineComments: Boolean read FAllowMultilineComments write FAllowMultilineComments;
  end;

  TNEDLexer = class
  private
    FConfig: TNEDLexerConfig;
    FText: UTF8String;
    FTextLength: Integer; // its per line, don't know if anybody has 2GB long code lines, maybe
    FPosition: Integer;
    //
    FLine: Integer;
    FColumn: Integer;
    //
    FState: TNEDLexicalState;
    //
    FTokenStart: Integer;
    FTokenStartLine: Integer;
    FTokenStartColumn: Integer;
    //
    function GetEndOfText: Boolean;
    function GetCurrentChar: UTF8Char;
    procedure AdvanceChar(const AOffset: Integer);
    function CheckBufferForCharacters(const ACharacters: UTF8String): Boolean;

    procedure StartLexeme;

    procedure EnterMode(const AMode: TNEDLexicalModeEnum);

    procedure EnterNormalMode;
    procedure EnterStringMode(const ADelimiter: UTF8Char);
    procedure EnterCommentMode(AKind: TNEDLexicalCommentKindEnum; const AEndDelimiter: UTF8String; const ACanNest, ACanBeNested: Boolean);
    procedure EnterDirectiveMode(const ADelimiter: UTF8String);
    procedure EnterCompilerVariableMode(const ADelimiter: UTF8String);

    procedure ProcessNormalCharacter;
    procedure ProcessStringCharacter;
    procedure ProcessCommentCharacter;
    procedure ProcessDirectiveCharacter;
    procedure ProcessCompilerVariableCharacter;
  private
    // this part needs to be parametrized for configuration support
    function IsEndOfLine: Boolean;
    function IsLineBreak(out AdvanceOffset: Integer): Boolean;
    function IsStringDelimiter: Boolean;

    function IsInComment: Boolean;
    function IsLineCommentStart(out CommentInfo: TNEDLexicalDelimiterPair; out AdvanceOffset: Integer): Boolean;
    function IsMultiLineCommentStart(out CommentInfo: TNEDLexicalDelimiterPair; out AdvanceOffset: Integer): Boolean;
    function IsMultiLineCommentEnd(const AEndDelimiter: UTF8String; out AdvanceOffset: Integer): Boolean;
    function IsNestedComment(out AdvanceOffset: Integer): Boolean;
    function IsDirectiveStart(out DirectiveInfo: TNEDLexicalDelimiterPair; out AdvanceOffset: Integer): Boolean; overload;
    function IsCompilerVariableStart(out DirectiveInfo: TNEDLexicalDelimiterPair; out AdvanceOffset: Integer): Boolean; overload;
    function IsDirectiveEnd(const AEndDelimiter: UTF8String; out AdvanceOffset: Integer): Boolean; overload;

    function GetCurrentLocation: TNEDSourceLocation;
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Clear;
    procedure Reset;

    procedure SetText(const ALine: Integer; const AText: UTF8String);

    function NextChar(out AChar: UTF8Char): Boolean;
    function PeekChar(AOffset: Integer = 1): UTF8Char;
    procedure LeaveMode;

    function CurrentLocation: TNEDSourceLocation;
    function CurrentRange: TNEDSourceRange;

    function CurrentState: TNEDLexicalState;

    function IsEOF: Boolean;
    function IsCommentStart: Boolean;
    function IsDirectiveStart: Boolean; overload;
    function IsDirectiveEnd: Boolean; overload;
    function IsCompilerVariableStart: Boolean; overload;
    function IsCompilerVariableEnd: Boolean;
    //
    // properties
    property Config: TNEDLexerConfig read FConfig;

    //
    // lexer state properties
    property Text: UTF8String read FText;
    property Position: Integer read FPosition;
    property Line: Integer read FLine;
    property Column: Integer read FColumn;
    //
    property State: TNEDLexicalState read FState;
  end;

implementation

{ TNEDLexerConfig }

constructor TNEDLexerConfig.Create;
begin
  inherited Create;

  Reset;
end;

procedure TNEDLexerConfig.Assign(const ASource: TNEDLexerConfig);
var
  i: Integer;
begin
  if ASource = Nil then
    Exit;

  FLineBreak := ASource.FLineBreak;
  FStringDelimiter := ASource.StringDelimiter;
  FAllowStringDelimiterNesting := ASource.AllowStringDelimiterNesting;
  FNestedStringDelimiterEscapeing := ASource.NestedStringDelimiterEscapeing;

  SetLength(FCommentsDelimiters, 0);
  for i := 0 to High(ASource.FCommentsDelimiters) do
    AddCommentDelimiter(ASource.FCommentsDelimiters[i]);

  FDirectiveDelimiter := ASource.DirectiveDelimiter;
  FDirectiveCanContainDash := ASource.DirectiveCanContainDash;

  FCompilerVariableDelimiter := ASource.CompilerVariableDelimiter;
  FCompilerVariableCanContainDash := ASource.CompilerVariableCanContainDash;

  FAllowNestedComments := ASource.AllowNestedComments;
  FAllowMultilineStrings := ASource.AllowMultilineStrings;
  FAllowMultilineComments := ASource.AllowMultilineComments;
end;

procedure TNEDLexerConfig.Clear;
begin
  FLineBreak := '';
  FStringDelimiter := #0;
  FAllowStringDelimiterNesting := False;
  FNestedStringDelimiterEscapeing := False;

  SetLength(FCommentsDelimiters, 0);

  FDirectiveDelimiter.&Type          := ectNotInComment;
  FDirectiveDelimiter.StartDelimiter := '';
  FDirectiveDelimiter.EndDelimiter   := '';
  FDirectiveDelimiter.CanNest        := False;
  FDirectiveDelimiter.CanBeNested    := False;
  FDirectiveCanContainDash := False;
  //
  FCompilerVariableDelimiter.&Type          := ectNotInComment;
  FCompilerVariableDelimiter.StartDelimiter := '';
  FCompilerVariableDelimiter.EndDelimiter   := '';
  FCompilerVariableDelimiter.CanNest        := False;
  FCompilerVariableDelimiter.CanBeNested    := False;
  FCompilerVariableCanContainDash := False;

  FAllowNestedComments := False;
  FAllowMultilineStrings := False;
  FAllowMultilineComments := False;
end;

procedure TNEDLexerConfig.Reset;
begin
  FLineBreak := #13#10; // CRLF(#13#10) or LF(#10) or CR(#13)
  FStringDelimiter := ''''; // ' or "
  FAllowStringDelimiterNesting := False;
  FNestedStringDelimiterEscapeing := False;

  SetLength(FCommentsDelimiters, 0);
  AddCommentDelimiter(ectSingleLine, '//', '',     True, True);
  AddCommentDelimiter(ectMultiLine,  '/.', './',   True, True);
  AddCommentDelimiter(ectMultiLine,  '{.', '.}',   True, True);
  AddCommentDelimiter(ectMultiLine,  '(*', '*)',   True, True);
  // special case comments
  AddCommentDelimiter(ectSingleLine, '//-', '',    True, False);
  AddCommentDelimiter(ectMultiLine,  '//+', '+//', True, False);

  FDirectiveDelimiter.&Type          := ectMixed;
  FDirectiveDelimiter.StartDelimiter := '{$';
  FDirectiveDelimiter.EndDelimiter   := '}';
  FDirectiveDelimiter.CanNest        := True;
  FDirectiveDelimiter.CanBeNested    := True;
  FDirectiveCanContainDash := True;
  //
  FCompilerVariableDelimiter.&Type          := ectMixed;
  FCompilerVariableDelimiter.StartDelimiter := '%';
  FCompilerVariableDelimiter.EndDelimiter   := '%';
  FCompilerVariableDelimiter.CanNest        := False;
  FCompilerVariableDelimiter.CanBeNested    := True;
  FCompilerVariableCanContainDash := TRue;

  FAllowNestedComments := False;
  FAllowMultilineStrings := False;
  FAllowMultilineComments := False;
end;

procedure TNEDLexerConfig.ClearCommentDelimiters;
var
  i: Integer;
begin
  for i := 0 to High(FCommentsDelimiters) do begin
    FCommentsDelimiters[i].StartDelimiter := '';
    FCommentsDelimiters[i].EndDelimiter := '';
  end;

  SetLength(FCommentsDelimiters, 0);
end;

procedure TNEDLexerConfig.AddCommentDelimiter(const ADelimiter: TNEDLexicalDelimiterPair);
begin
  AddCommentDelimiter(ADelimiter.&Type, ADelimiter.StartDelimiter, ADelimiter.EndDelimiter, ADelimiter.CanNest, ADelimiter.CanBeNested);
end;

procedure TNEDLexerConfig.AddCommentDelimiter(const AType: TNEDLexicalCommentTypeEnum; const AStartDelimiter, AEndDelimiter: String; const ACanNest, ACanBeNested: Boolean);
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

{ TNEDLexer }

constructor TNEDLexer.Create;
begin
  inherited Create;
  FConfig := TNEDLexerConfig.Create;
  //
  FText := '';
  FTextLength := 0;
  //
  Reset;
end;

destructor TNEDLexer.Destroy;
begin
  Reset;
  FConfig.Free;
  inherited Destroy;
end;

function TNEDLexer.GetEndOfText: Boolean;
begin
  Result := FPosition > FTextLength;
end;

function TNEDLexer.GetCurrentChar: UTF8Char;
begin
  if GetEndOfText then
    Result := #0
  else
    Result := FText[FPosition];
end;

procedure TNEDLexer.AdvanceChar(const AOffset: Integer);
var
  I: Integer;
//  C: UTF8Char;
begin
  I := 0;
  while I < AOffset do begin
    if GetEndOfText then
      Exit;

    Inc(FPosition);
    Inc(FColumn);
    Inc(I);
  end;

// this is for full file lexing, in our editor this is not usefull,
// because lexer takes only one line of text at a time, and there is no line-break at the end or in the middle
//
//    C := FText[FPosition];
//    Inc(FPosition);
//
//    if C = #13 then begin
//      if (FPosition <= Length(FText)) and (FText[FPosition] = #10) then
//        Inc(FPosition);
//
//      Inc(FLine);
//      FColumn := 1;
//    end
//    else if C = #10 then begin
//      Inc(FLine);
//      FColumn := 1;
//    end
//    else
//      Inc(FColumn);
//  end;
end;

function TNEDLexer.CheckBufferForCharacters(const ACharacters: UTF8String): Boolean;
var
  CharactersLen: Integer;
  i: Integer;
begin
  Result := False;
  if ACharacters = '' then
    Exit;

  CharactersLen := Length(ACharacters);
  if FPosition + CharactersLen - 1 > FTextLength then
    Exit;

  for i := 1 to CharactersLen do
    if FText[FPosition + i - 1] <> UTF8Char(ACharacters[i]) then
      Exit;

  Result := True;
end;

procedure TNEDLexer.StartLexeme;
begin
  FTokenStart := FPosition;
  FTokenStartLine := FLine;
  FTokenStartColumn := FColumn;
end;

procedure TNEDLexer.EnterMode(const AMode: TNEDLexicalModeEnum);
begin
  if FState.ModeDepth >= NEDMaxLexicalModeDepth then
    Exit;

  FState.ModeStack[FState.ModeDepth] := FState.Mode;
  Inc(FState.ModeDepth);

  FState.Mode := AMode;
end;

procedure TNEDLexer.EnterNormalMode;
begin
  FState.Mode := lmNormal;
end;

procedure TNEDLexer.EnterStringMode(const ADelimiter: UTF8Char);
begin
  EnterMode(lmString);

  FState.StringDelimiter := ADelimiter;
end;

procedure TNEDLexer.EnterCommentMode(AKind: TNEDLexicalCommentKindEnum; const AEndDelimiter: UTF8String; const ACanNest, ACanBeNested: Boolean);
begin
  EnterMode(lmComment);

  FState.CommentKind := AKind;
  FState.CommentEndDelimiter := AEndDelimiter;
  FState.CommentCanNest := ACanNest;
  FState.CommentCanBeNested := ACanBeNested;
  FState.CommentDepth := 1;
end;

procedure TNEDLexer.EnterDirectiveMode(const ADelimiter: UTF8String);
begin
  EnterMode(lmDirective);

  //FState.CompilerDirective := '';
  FState.DirectiveEndDelimiter := ADelimiter;
  FState.DirectiveDepth := 1;
end;

procedure TNEDLexer.EnterCompilerVariableMode(const ADelimiter: UTF8String);
begin
  EnterMode(lmCompilerVariable);

  //FState.CompilerVariableDirective := '';
  FState.CompilerVariableEndDelimiter := ADelimiter;
  FState.CompilerVariableDepth := 1;
end;

procedure TNEDLexer.ProcessNormalCharacter; // (AChar: UTF8Char);
var
  CommentInfo: TNEDLexicalDelimiterPair;
  DirectiveInfo: TNEDLexicalDelimiterPair;
  AdvanceOffset: Integer;
begin
  if IsStringDelimiter then begin
    EnterStringMode(FConfig.FStringDelimiter);
    AdvanceChar(1);
    Exit;
  end;

  if IsDirectiveStart(DirectiveInfo, AdvanceOffset) then begin
    EnterDirectiveMode(DirectiveInfo.StartDelimiter);
    AdvanceChar(AdvanceOffset);
    Exit;
  end;

  if IsCompilerVariableStart(DirectiveInfo, AdvanceOffset) then begin
    EnterCompilerVariableMode(DirectiveInfo.StartDelimiter);
    AdvanceChar(AdvanceOffset);
    Exit;
  end;

  if IsLineCommentStart(CommentInfo, AdvanceOffset) then begin
    EnterCommentMode(lckLine, CommentInfo.EndDelimiter, CommentInfo.CanNest, CommentInfo.CanBeNested);
    AdvanceChar(AdvanceOffset);
    Exit;
  end;

  if IsMultiLineCommentStart(CommentInfo, AdvanceOffset) then begin
    EnterCommentMode(lckMultiLine, CommentInfo.EndDelimiter, CommentInfo.CanNest, CommentInfo.CanBeNested);
    AdvanceChar(AdvanceOffset);
    Exit;
  end;

  AdvanceChar(1);
end;

procedure TNEDLexer.ProcessStringCharacter;
begin
  if GetCurrentChar = FState.StringDelimiter then begin
    // Pascal-style escaped quote: 'John''s'
    if PeekChar = FState.StringDelimiter then begin
      AdvanceChar(2);
      Exit;
    end;

    AdvanceChar(1);

    LeaveMode;
    Exit;
  end;

  AdvanceChar(1);
end;

procedure TNEDLexer.ProcessCommentCharacter;
var
  AdvanceOffset: Integer;
begin
  case FState.CommentKind of
    lckLine: begin
      AdvanceChar(1);

      // a line comment ends at the end of this line's buffer
      if IsEndOfLine then begin
        LeaveMode;
        Exit;
      end;

      // also support explicit CR/LF characters if present
      if IsLineBreak(AdvanceOffset) then begin
        LeaveMode;
        AdvanceChar(AdvanceOffset);
        Exit;
      end;
    end;

    lckMultiLine: begin
      if IsMultiLineCommentEnd(FState.CommentEndDelimiter, AdvanceOffset) then begin
        Dec(FState.CommentDepth);
        AdvanceChar(AdvanceOffset);
        if FState.CommentDepth <= 0 then
          LeaveMode;

        Exit;
      end;

      // allow nested comments
      if FState.CommentCanNest and IsNestedComment(AdvanceOffset) then begin
        Inc(FState.CommentDepth);
        AdvanceChar(AdvanceOffset);
        Exit;
      end;

      AdvanceChar(1);
    end;
  end;
end;

procedure TNEDLexer.ProcessDirectiveCharacter;
var
  AdvanceOffset: Integer;
begin
  // {$IF ...}
  // {$ENDIF}
  //
  // at this level we deliberately don't interpret the directive semantically,
  // we only maintain the lexical state and collect its textual contents

  if IsStringDelimiter then begin
    EnterStringMode(FConfig.FStringDelimiter);
    AdvanceChar(1);
    Exit;
  end;

  if IsDirectiveEnd(FConfig.FDirectiveDelimiter.EndDelimiter, AdvanceOffset) then begin
    Dec(FState.DirectiveDepth);
    AdvanceChar(AdvanceOffset);
    if FState.DirectiveDepth <= 0 then
      LeaveMode;

    Exit;
  end;

  if IsEndOfLine then begin
    // an unterminated compiler directive is allowed to remain in directive mode,
    // this is important while the user is editing incomplete source
    Exit;
  end;

  //FState.CompilerDirective := FState.CompilerDirective + GetCurrentChar;

  AdvanceChar(1);
end;

procedure TNEDLexer.ProcessCompilerVariableCharacter;
var
  AdvanceOffset: Integer;
begin
  // %...%
  //
  // at this level we deliberately don't interpret the directive semantically,
  // we only maintain the lexical state and collect its textual contents

  if IsDirectiveEnd(FConfig.FCompilerVariableDelimiter.EndDelimiter, AdvanceOffset) then begin
    Dec(FState.DirectiveDepth);
    AdvanceChar(AdvanceOffset);
    if FState.DirectiveDepth <= 0 then
      LeaveMode;

    Exit;
  end;

  if IsEndOfLine then begin
    // an unterminated compiler directive is allowed to remain in directive mode,
    // this is important while the user is editing incomplete source
    Exit;
  end;

  //FState.CompilerVariableDirective := FState.CompilerVariableDirective + GetCurrentChar;

  AdvanceChar(1);
end;

function TNEDLexer.IsEndOfLine: Boolean;
begin
  Result := GetEndOfText;
end;

function TNEDLexer.IsLineBreak(out AdvanceOffset: Integer): Boolean;
begin
  Result := False; // satisfy compiler
  AdvanceOffset := 1;
  Result := CheckBufferForCharacters(FConfig.FLineBreak);
  if Result then
    AdvanceOffset := Length(FConfig.FLineBreak);
end;

function TNEDLexer.IsStringDelimiter: Boolean;
begin
  Result := GetCurrentChar = FConfig.FStringDelimiter;
end;

function TNEDLexer.IsInComment: Boolean;
begin
  Result := FState.CommentKind > lckNone;
end;

function TNEDLexer.IsLineCommentStart(out CommentInfo: TNEDLexicalDelimiterPair; out AdvanceOffset: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  CommentInfo.&Type := ectNotInComment;
  CommentInfo.StartDelimiter := '';
  CommentInfo.EndDelimiter := '';
  CommentInfo.CanNest := False;
  CommentInfo.CanBeNested := False;
  AdvanceOffset := 0;
  //
  for i := 0 to Length(FConfig.FCommentsDelimiters) - 1 do begin
    if (FConfig.FCommentsDelimiters[i].&Type = ectSingleLine) or (FConfig.FCommentsDelimiters[i].&Type = ectMixed) then begin
      Result := CheckBufferForCharacters(FConfig.FCommentsDelimiters[i].StartDelimiter);
      if Result then begin
        CommentInfo := FConfig.FCommentsDelimiters[i];
        AdvanceOffset := Length(FConfig.FCommentsDelimiters[i].StartDelimiter);
        Break;
      end;
    end;
  end;
end;

function TNEDLexer.IsMultiLineCommentStart(out CommentInfo: TNEDLexicalDelimiterPair; out AdvanceOffset: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  CommentInfo.&Type := ectNotInComment;
  CommentInfo.StartDelimiter := '';
  CommentInfo.EndDelimiter := '';
  CommentInfo.CanNest := False;
  CommentInfo.CanBeNested := False;
  AdvanceOffset := 0;
  //
  for i := 0 to Length(FConfig.FCommentsDelimiters) - 1 do begin
    if (FConfig.FCommentsDelimiters[i].&Type = ectMultiLine) or (FConfig.FCommentsDelimiters[i].&Type = ectMixed) then begin
      Result := CheckBufferForCharacters(FConfig.FCommentsDelimiters[i].StartDelimiter);
      if Result then begin
        CommentInfo := FConfig.FCommentsDelimiters[i];
        AdvanceOffset := Length(FConfig.FCommentsDelimiters[i].StartDelimiter);
        Break;
      end;
    end;
  end;
end;

function TNEDLexer.IsMultiLineCommentEnd(const AEndDelimiter: UTF8String; out AdvanceOffset: Integer): Boolean;
begin
  Result := False;
  AdvanceOffset := 0;
  //
  Result := CheckBufferForCharacters(AEndDelimiter);
  if Result then
    AdvanceOffset := Length(AEndDelimiter);
end;

function TNEDLexer.IsNestedComment(out AdvanceOffset: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  AdvanceOffset := 0;
  //
  for i := 0 to Length(FConfig.FCommentsDelimiters) - 1 do begin
    if FConfig.FCommentsDelimiters[i].CanBeNested then begin
      Result := CheckBufferForCharacters(FConfig.FCommentsDelimiters[i].StartDelimiter);
      if Result then begin
        AdvanceOffset := Length(FConfig.FCommentsDelimiters[i].StartDelimiter);
        Break;
      end;
    end;
  end;
end;

function TNEDLexer.IsDirectiveStart(out DirectiveInfo: TNEDLexicalDelimiterPair; out AdvanceOffset: Integer): Boolean;
begin
  Result := False;
  DirectiveInfo.&Type := ectNotInComment;
  DirectiveInfo.StartDelimiter := '';
  DirectiveInfo.EndDelimiter := '';
  DirectiveInfo.CanNest := False;
  DirectiveInfo.CanBeNested := False;
  AdvanceOffset := 0;
  //
  Result := CheckBufferForCharacters(FConfig.DirectiveDelimiter.StartDelimiter);
  if Result then begin
    DirectiveInfo := FConfig.DirectiveDelimiter;
    AdvanceOffset := Length(FConfig.DirectiveDelimiter.StartDelimiter);;
  end;
end;

function TNEDLexer.IsCompilerVariableStart(out DirectiveInfo: TNEDLexicalDelimiterPair; out AdvanceOffset: Integer): Boolean;
begin
  Result := False;
  DirectiveInfo.&Type := ectNotInComment;
  DirectiveInfo.StartDelimiter := '';
  DirectiveInfo.EndDelimiter := '';
  DirectiveInfo.CanNest := False;
  DirectiveInfo.CanBeNested := False;
  AdvanceOffset := 0;
  //
  Result := CheckBufferForCharacters(FConfig.CompilerVariableDelimiter.StartDelimiter);
  if Result then begin
    DirectiveInfo := FConfig.CompilerVariableDelimiter;
    AdvanceOffset := Length(FConfig.CompilerVariableDelimiter.StartDelimiter);;
  end;
end;

function TNEDLexer.IsDirectiveEnd(const AEndDelimiter: UTF8String; out AdvanceOffset: Integer): Boolean;
begin
  Result := False;
  AdvanceOffset := 0;
  //
  Result := CheckBufferForCharacters(AEndDelimiter);
  if Result then
    AdvanceOffset := Length(AEndDelimiter);
end;

function TNEDLexer.GetCurrentLocation: TNEDSourceLocation;
begin
  Result.Line := FLine;
  Result.Column := FColumn;
  Result.Length := 0;
end;

procedure TNEDLexer.Clear;
begin
  FText := '';
  FTextLength := 0;
  FLine := 0;
  FTokenStartLine := 0;

  FillChar(FState, SizeOf(FState), 0);

  FState.Mode := lmNormal;
  FState.StringDelimiter := #0;
  FState.CommentKind := lckNone;
  FState.CommentEndDelimiter := '';
  FState.CommentCanNest := False;
  FState.CommentCanBeNested := False;
  FState.CommentDepth := 0;
  //FState.CompilerDirective := '';
  FState.DirectiveEndDelimiter := '';
  FState.DirectiveDepth := 0;
  //FState.CompilerVariableDirective := '';
  FState.CompilerVariableEndDelimiter := '';
  FState.CompilerVariableDepth := 0;

  Reset;
end;

procedure TNEDLexer.Reset;
begin
  FPosition := 1;

  FLine := 1;
  FColumn := 1;

  FTokenStart := 1;
  FTokenStartLine := 1;
  FTokenStartColumn := 1;
end;

procedure TNEDLexer.SetText(const ALine: Integer; const AText: UTF8String);
begin
  FText := AText;
  FTextLength := Length(FText);
  Reset;
  FLine := ALine;
  FTokenStartLine := ALine;
end;

function TNEDLexer.NextChar(out AChar: UTF8Char): Boolean;
begin
  if IsEOF then begin
    AChar := #0;
    Result := False;
    Exit;
  end;

  StartLexeme;

  AChar := GetCurrentChar;
  case FState.Mode of
    lmNormal : ProcessNormalCharacter;
    lmString : ProcessStringCharacter;
    lmComment : ProcessCommentCharacter;
    lmDirective : ProcessDirectiveCharacter;
    lmCompilerVariable : ProcessCompilerVariableCharacter;
  end;

  Result := True;
end;

function TNEDLexer.PeekChar(AOffset: Integer): UTF8Char;
var
  P: Integer;
begin
  P := FPosition + AOffset;

  if (P < 1) or (P > FTextLength) then
    Result := #0
  else
    Result := FText[P];
end;

procedure TNEDLexer.LeaveMode;
begin
  if FState.ModeDepth > 0 then begin
    Dec(FState.ModeDepth);
    FState.Mode := FState.ModeStack[FState.ModeDepth];
  end
  else
    EnterNormalMode;
end;

function TNEDLexer.CurrentLocation: TNEDSourceLocation;
begin
  Result := GetCurrentLocation;
end;

function TNEDLexer.CurrentRange: TNEDSourceRange;
begin
  Result.StartPos.Line := FTokenStartLine;
  Result.StartPos.Column := FTokenStartColumn;
  Result.StartPos.Length := FPosition - FTokenStart;

  Result.EndPos.Line := FLine;
  Result.EndPos.Column := FColumn;
  Result.EndPos.Length := 0;
end;

function TNEDLexer.CurrentState: TNEDLexicalState;
begin
  Result := FState;
end;

function TNEDLexer.IsEOF: Boolean;
begin
  Result := GetEndOfText;
end;

function TNEDLexer.IsCommentStart: Boolean;
var
  i: Integer;
begin
  Result := False;

  if FConfig = Nil then
    Exit;

  for i := 0 to Length(FConfig.FCommentsDelimiters) - 1 do begin
    if FConfig.FCommentsDelimiters[i].&Type > ectNotInComment then begin
      Result := CheckBufferForCharacters(FConfig.FCommentsDelimiters[i].StartDelimiter);
      if Result then
        Break;
    end;
  end;
end;

function TNEDLexer.IsDirectiveStart: Boolean;
begin
  Result := False;

  if FConfig = Nil then
    Exit;

  Result := CheckBufferForCharacters(FConfig.DirectiveDelimiter.StartDelimiter);
end;

function TNEDLexer.IsDirectiveEnd: Boolean;
begin
  Result := False;

  if FConfig = Nil then
    Exit;

  Result := CheckBufferForCharacters(FConfig.DirectiveDelimiter.EndDelimiter);
end;

function TNEDLexer.IsCompilerVariableStart: Boolean;
begin
  Result := False;

  if FConfig = Nil then
    Exit;

  Result := CheckBufferForCharacters(FConfig.CompilerVariableDelimiter.StartDelimiter);
end;

function TNEDLexer.IsCompilerVariableEnd: Boolean;
begin
  Result := False;

  if FConfig = Nil then
    Exit;

  Result := CheckBufferForCharacters(FConfig.CompilerVariableDelimiter.EndDelimiter);
end;

end.

