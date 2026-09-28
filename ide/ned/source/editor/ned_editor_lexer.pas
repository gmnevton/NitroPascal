//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-09
// All rights reserved.
//

unit ned_editor_lexer;

interface

uses
  SysUtils;

type
  TNEDLexicalModeEnum = (
    lmNormal,
    lmString,
    lmComment,
    lmDirective,
    lmNotation
  );

  TNEDLexicalCommentTypeEnum = (
    lctNotInComment,
    lctSingleLine,
    lctMultiLine,
    lctMixed
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

  TNEDLexicalState = record
    Mode: TNEDLexicalModeEnum;
    StringDelimiter: UTF8Char;
    CommentKind: TNEDLexicalCommentKindEnum;
    CommentEndDelimiter: UTF8String;
    CommentCanNest: Boolean;
    CommentCanBeNested: Boolean;
    CommentDepth: Integer;
    CompilerDirective: UTF8String; // stores the compiler directive text: {$IF ....}
    DirectiveEndDelimiter: UTF8String;
    DirectiveDepth: Integer;
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

  TNEDLexicalDelimiterPair = record
    &Type: TNEDLexicalCommentTypeEnum;
    StartDelimiter: UTF8String;
    EndDelimiter: UTF8String; // optional, for single-line comment leave empty
    CanNest: Boolean; // can nest other comments
    CanBeNested: Boolean; // can be nested inside other comments
  end;

  TNEDLexer = class
  private
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

    procedure StartLexeme;

    procedure EnterNormalMode;
    procedure EnterStringMode(const ADelimiter: UTF8Char);
    procedure EnterCommentMode(AKind: TNEDLexicalCommentKindEnum; const AEndDelimiter: UTF8String; const ACanNest, ACanBeNested: Boolean);
    procedure EnterDirectiveMode(const ADelimiter: UTF8String);

    procedure ProcessNormalCharacter;
    procedure ProcessStringCharacter;
    procedure ProcessCommentCharacter;
    procedure ProcessDirectiveCharacter;
  private
    FLineBreak: UTF8String; // CRLF or LF or CR
    FStringDelimiter: UTF8Char; // " or '
    FCommentsDelimiter: Array of TNEDLexicalDelimiterPair; // see the compiler\concepts\comment\comment.npc file for detail explanation
    FDirectiveDelimiter: TNEDLexicalDelimiterPair; // '{$' - directive begin; '}' directive end

    // this part needs to be parametrized for configuration support
    function IsLineBreak(out AdvanceOffset: Integer): Boolean;
    function IsStringDelimiter: Boolean;

    function IsInComment: Boolean;
    function IsLineCommentStart(out CommentInfo: TNEDLexicalDelimiterPair; out AdvanceOffset: Integer): Boolean;
    function IsMultiLineCommentStart(out CommentInfo: TNEDLexicalDelimiterPair; out AdvanceOffset: Integer): Boolean;
    function IsMultiLineCommentEnd(const AEndDelimiter: UTF8String; out AdvanceOffset: Integer): Boolean;
    function IsNestedComment(out AdvanceOffset: Integer): Boolean;
    function IsDirectiveStart(out DirectiveInfo: TNEDLexicalDelimiterPair; out AdvanceOffset: Integer): Boolean;
    function IsDirectiveEnd(const AEndDelimiter: UTF8String; out AdvanceOffset: Integer): Boolean;

    function GetCurrentLocation: TNEDSourceLocation;
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Reset;
    procedure ClearCommentDelimiters;
    procedure AddCommentDelimiter(const AType: TNEDLexicalCommentTypeEnum; const AStartDelimiter, AEndDelimiter: String; const ACanNest, ACanBeNested: Boolean);

    procedure SetText(const ALine: Integer; const AText: UTF8String);

    function NextChar(out AChar: UTF8Char): Boolean;
    function PeekChar(AOffset: Integer = 1): UTF8Char;

    function CurrentLocation: TNEDSourceLocation;
    function CurrentRange: TNEDSourceRange;

    function CurrentState: TNEDLexicalState;

    function IsEOF: Boolean;

    //
    // properties
    property LineBreak: UTF8String read FLineBreak write FLineBreak;
    property StringDelimiter: UTF8Char read FStringDelimiter write FStringDelimiter;
    //property FCommentsDelimiter: Array of TNEDLexicalDelimiterPair;
    property DirectiveDelimiter: TNEDLexicalDelimiterPair read FDirectiveDelimiter write FDirectiveDelimiter;

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

{ TNEDLexer }

constructor TNEDLexer.Create;
begin
  inherited Create;
  //
  FText := '';
  FTextLength := 0;
  //
  FLineBreak := #13#10; // CRLF(#13#10) or LF(#10) or CR(#13)
  FStringDelimiter := ''''; // ' or "

  SetLength(FCommentsDelimiter, 0);
  AddCommentDelimiter(lctSingleLine, '//', '',     True, True);
  AddCommentDelimiter(lctMultiLine,  '/.', './',   True, True);
  AddCommentDelimiter(lctMultiLine,  '{.', '.}',   True, True);
  AddCommentDelimiter(lctMultiLine,  '(*', '*)',   True, True);
  // special case comments
  AddCommentDelimiter(lctSingleLine, '//-', '',    True, False);
  AddCommentDelimiter(lctMultiLine,  '//+', '+//', True, False);

  FDirectiveDelimiter.&Type          := lctMixed;
  FDirectiveDelimiter.StartDelimiter := '{$';
  FDirectiveDelimiter.EndDelimiter   := '}';
  FDirectiveDelimiter.CanNest        := True;
  FDirectiveDelimiter.CanBeNested    := True;
  //
  Reset;
end;

destructor TNEDLexer.Destroy;
begin
  Reset;
  inherited Destroy;
end;

procedure TNEDLexer.Reset;
begin
  FPosition := 1;

  FLine := 1;
  FColumn := 1;

  FillChar(FState, SizeOf(FState), 0);

  FState.Mode := lmNormal;
  FState.StringDelimiter := #0;
  FState.CommentKind := lckNone;
  FState.CommentEndDelimiter := '';
  FState.CommentCanNest := False;
  FState.CommentCanBeNested := False;
  FState.CommentDepth := 0;
  FState.CompilerDirective := '';
  FState.DirectiveEndDelimiter := '';
  FState.DirectiveDepth := 0;

  FTokenStart := 1;
  FTokenStartLine := 1;
  FTokenStartColumn := 1;
end;

procedure TNEDLexer.ClearCommentDelimiters;
var
  i: Integer;
begin
  for i := 0 to High(FCommentsDelimiter) do begin
    FCommentsDelimiter[i].StartDelimiter := '';
    FCommentsDelimiter[i].EndDelimiter := '';
  end;

  SetLength(FCommentsDelimiter, 0);
end;

procedure TNEDLexer.AddCommentDelimiter(const AType: TNEDLexicalCommentTypeEnum; const AStartDelimiter, AEndDelimiter: String; const ACanNest, ACanBeNested: Boolean);
var
  idx: Integer;
begin
  idx := Length(FCommentsDelimiter);
  SetLength(FCommentsDelimiter, idx + 1);
  //
  FCommentsDelimiter[idx].&Type          := AType;
  FCommentsDelimiter[idx].StartDelimiter := AStartDelimiter;
  FCommentsDelimiter[idx].EndDelimiter   := AEndDelimiter;
  FCommentsDelimiter[idx].CanNest        := ACanNest;
  FCommentsDelimiter[idx].CanBeNested    := ACanBeNested;
end;

procedure TNEDLexer.SetText(const ALine: Integer; const AText: UTF8String);
begin
  FText := AText;
  FTextLength := Length(FText);
  Reset;
  FLine := ALine;
  FTokenStartLine := ALine;
end;

function TNEDLexer.GetEndOfText: Boolean;
begin
  Result := FPosition > FTextLength;
end;

function TNEDLexer.IsEOF: Boolean;
begin
  Result := GetEndOfText;
end;

function TNEDLexer.GetCurrentChar: UTF8Char;
begin
  if GetEndOfText then
    Result := #0
  else
    Result := FText[FPosition];
end;

function TNEDLexer.IsLineBreak(out AdvanceOffset: Integer): Boolean;
begin
  Result := False; // satisfy compiler
  AdvanceOffset := 1;
  if Length(FLineBreak) > 1 then begin
    if GetCurrentChar = FLineBreak[1] then begin // check if it is exactly as our expected line break
      if PeekChar = FLineBreak[2] then begin
        AdvanceOffset := 2;
        Result := True;
        Exit;
      end;
    end
    else begin // check for lonely CR or LF
      if (GetCurrentChar = FLineBreak[1]) or (GetCurrentChar = FLineBreak[2]) then
        Exit(True);
    end;
  end
  else
    Result := GetCurrentChar = FLineBreak; // CR or LF only
end;

function TNEDLexer.IsStringDelimiter: Boolean;
begin
  Result := GetCurrentChar = FStringDelimiter;
end;

function TNEDLexer.IsInComment: Boolean;
begin
  Result := FState.CommentKind > lckNone;
end;

function TNEDLexer.IsLineCommentStart(out CommentInfo: TNEDLexicalDelimiterPair; out AdvanceOffset: Integer): Boolean;
var
  CommentOneChar: UTF8Char;
  CommentTwoChars: UTF8String;
  CommentThreeChars: UTF8String;
  i: Integer;
begin
  Result := False;
  CommentInfo.&Type := lctNotInComment;
  CommentInfo.StartDelimiter := '';
  CommentInfo.EndDelimiter := '';
  CommentInfo.CanNest := False;
  CommentInfo.CanBeNested := False;
  AdvanceOffset := 0;
  //
  CommentOneChar := GetCurrentChar;
  CommentTwoChars := GetCurrentChar + PeekChar;
  CommentThreeChars := GetCurrentChar + PeekChar + PeekChar(2);
  for i := 0 to Length(FCommentsDelimiter) - 1 do begin
    if (FCommentsDelimiter[i].&Type = lctSingleLine) or (FCommentsDelimiter[i].&Type = lctMixed) then begin
      // to avoid false positives, first check 3 chars comments, than 2 chars, than 1 char
      Result := FCommentsDelimiter[i].StartDelimiter = CommentThreeChars;
      if Result then begin
        CommentInfo := FCommentsDelimiter[i];
        AdvanceOffset := 3;
        Break;
      end;
      Result := FCommentsDelimiter[i].StartDelimiter = CommentTwoChars;
      if Result then begin
        CommentInfo := FCommentsDelimiter[i];
        AdvanceOffset := 2;
        Break;
      end;
      Result := FCommentsDelimiter[i].StartDelimiter = CommentOneChar;
      if Result then begin
        CommentInfo := FCommentsDelimiter[i];
        AdvanceOffset := 1;
        Break;
      end;
    end;
  end;
  CommentOneChar := #0;
  CommentTwoChars := '';
  CommentThreeChars := '';
end;

function TNEDLexer.IsMultiLineCommentStart(out CommentInfo: TNEDLexicalDelimiterPair; out AdvanceOffset: Integer): Boolean;
var
  CommentOneChar: UTF8Char;
  CommentTwoChars: UTF8String;
  CommentThreeChars: UTF8String;
  i: Integer;
begin
  Result := False;
  CommentInfo.&Type := lctNotInComment;
  CommentInfo.StartDelimiter := '';
  CommentInfo.EndDelimiter := '';
  CommentInfo.CanNest := False;
  CommentInfo.CanBeNested := False;
  AdvanceOffset := 0;
  //
  CommentOneChar := GetCurrentChar;
  CommentTwoChars := GetCurrentChar + PeekChar;
  CommentThreeChars := GetCurrentChar + PeekChar + PeekChar(2);
  for i := 0 to Length(FCommentsDelimiter) - 1 do begin
    if (FCommentsDelimiter[i].&Type = lctMultiLine) or (FCommentsDelimiter[i].&Type = lctMixed) then begin
      // to avoid false positives, first check 3 chars comments, than 2 chars, than 1 char
      Result := FCommentsDelimiter[i].StartDelimiter = CommentThreeChars;
      if Result then begin
        CommentInfo := FCommentsDelimiter[i];
        AdvanceOffset := 3;
        Break;
      end;
      Result := FCommentsDelimiter[i].StartDelimiter = CommentTwoChars;
      if Result then begin
        CommentInfo := FCommentsDelimiter[i];
        AdvanceOffset := 2;
        Break;
      end;
      Result := FCommentsDelimiter[i].StartDelimiter = CommentOneChar;
      if Result then begin
        CommentInfo := FCommentsDelimiter[i];
        AdvanceOffset := 1;
        Break;
      end;
    end;
  end;
  CommentOneChar := #0;
  CommentTwoChars := '';
  CommentThreeChars := '';
end;

function TNEDLexer.IsMultiLineCommentEnd(const AEndDelimiter: UTF8String; out AdvanceOffset: Integer): Boolean;
var
  Chars: UTF8String;
  i, len: Integer;
begin
  Result := False;
  AdvanceOffset := 0;
  //
  Chars := '';
  len := Length(AEndDelimiter);
  for i := 0 to len - 1 do begin
    Chars := Chars + PeekChar(i);
  end;
  Result := AEndDelimiter = Chars;
  if Result then
    AdvanceOffset := len;
  Chars := '';
end;

function TNEDLexer.IsNestedComment(out AdvanceOffset: Integer): Boolean;
var
  CommentOneChar: UTF8Char;
  CommentTwoChars: UTF8String;
  CommentThreeChars: UTF8String;
  i: Integer;
begin
  Result := False;
  AdvanceOffset := 0;
  //
  CommentOneChar := GetCurrentChar;
  CommentTwoChars := GetCurrentChar + PeekChar;
  CommentThreeChars := GetCurrentChar + PeekChar + PeekChar(2);
  for i := 0 to Length(FCommentsDelimiter) - 1 do begin
    if FCommentsDelimiter[i].CanBeNested then begin
      // to avoid false positives, first check 3 chars comments, than 2 chars, than 1 char
      Result := FCommentsDelimiter[i].StartDelimiter = CommentThreeChars;
      if Result then begin
        AdvanceOffset := 3;
        Break;
      end;
      Result := FCommentsDelimiter[i].StartDelimiter = CommentTwoChars;
      if Result then begin
        AdvanceOffset := 2;
        Break;
      end;
      Result := FCommentsDelimiter[i].StartDelimiter = CommentOneChar;
      if Result then begin
        AdvanceOffset := 1;
        Break;
      end;
    end;
  end;
  CommentOneChar := #0;
  CommentTwoChars := '';
  CommentThreeChars := '';
end;

function TNEDLexer.IsDirectiveStart(out DirectiveInfo: TNEDLexicalDelimiterPair; out AdvanceOffset: Integer): Boolean;
var
  DirectiveTwoChars: UTF8String;
begin
  Result := False;
  DirectiveInfo.&Type := lctNotInComment;
  DirectiveInfo.StartDelimiter := '';
  DirectiveInfo.EndDelimiter := '';
  DirectiveInfo.CanNest := False;
  DirectiveInfo.CanBeNested := False;
  AdvanceOffset := 0;
  //
  DirectiveTwoChars := GetCurrentChar + PeekChar;
  Result := FDirectiveDelimiter.StartDelimiter = DirectiveTwoChars;
  if Result then begin
    DirectiveInfo := FDirectiveDelimiter;
    AdvanceOffset := 2;
  end;
  DirectiveTwoChars := '';
end;

function TNEDLexer.IsDirectiveEnd(const AEndDelimiter: UTF8String; out AdvanceOffset: Integer): Boolean;
var
  Chars: UTF8String;
  i, len: Integer;
begin
  Result := False;
  AdvanceOffset := 0;
  //
  Chars := '';
  len := Length(AEndDelimiter);
  for i := 0 to len - 1 do begin
    Chars := Chars + PeekChar(i);
  end;
  Result := AEndDelimiter = Chars;
  if Result then
    AdvanceOffset := len;
  Chars := '';
end;

procedure TNEDLexer.StartLexeme;
begin
  FTokenStart := FPosition;
  FTokenStartLine := FLine;
  FTokenStartColumn := FColumn;
end;

procedure TNEDLexer.EnterNormalMode;
begin
  FState.Mode := lmNormal;

  FState.StringDelimiter := #0;

  FState.CommentKind := lckNone;
  FState.CommentEndDelimiter := '';
  FState.CommentCanNest := False;
  FState.CommentCanBeNested := False;
  FState.CommentDepth := 0;

  FState.CompilerDirective := '';
  FState.DirectiveEndDelimiter := '';
  FState.DirectiveDepth := 0;
end;

procedure TNEDLexer.EnterStringMode(const ADelimiter: UTF8Char);
begin
  FState.Mode := lmString;

  FState.StringDelimiter := ADelimiter;

  FState.CommentKind := lckNone;
  FState.CommentEndDelimiter := '';
  FState.CommentCanNest := False;
  FState.CommentCanBeNested := False;
  FState.CommentDepth := 0;

  FState.CompilerDirective := '';
  FState.DirectiveEndDelimiter := '';
  FState.DirectiveDepth := 0;
end;

procedure TNEDLexer.EnterCommentMode(AKind: TNEDLexicalCommentKindEnum; const AEndDelimiter: UTF8String; const ACanNest, ACanBeNested: Boolean);
begin
  FState.Mode := lmComment;

  FState.StringDelimiter := #0;

  FState.CommentKind := AKind;
  FState.CommentEndDelimiter := AEndDelimiter;
  FState.CommentCanNest := ACanNest;
  FState.CommentCanBeNested := ACanBeNested;
  FState.CommentDepth := 1;

  FState.CompilerDirective := '';
  FState.DirectiveEndDelimiter := '';
  FState.DirectiveDepth := 0;
end;

procedure TNEDLexer.EnterDirectiveMode(const ADelimiter: UTF8String);
begin
  FState.Mode := lmDirective;

  FState.StringDelimiter := #0;

  FState.CommentKind := lckNone;
  FState.CommentEndDelimiter := '';
  FState.CommentCanNest := False;
  FState.CommentCanBeNested := False;
  FState.CommentDepth := 0;

  FState.CompilerDirective := '';
  FState.DirectiveEndDelimiter := ADelimiter;
  FState.DirectiveDepth := 1;
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

procedure TNEDLexer.ProcessNormalCharacter; // (AChar: UTF8Char);
var
  CommentInfo: TNEDLexicalDelimiterPair;
  DirectiveInfo: TNEDLexicalDelimiterPair;
  AdvanceOffset: Integer;
begin
  if IsStringDelimiter then begin
    EnterStringMode(FStringDelimiter);
    AdvanceChar(1);
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

  if IsDirectiveStart(DirectiveInfo, AdvanceOffset) then begin
    EnterDirectiveMode(DirectiveInfo.StartDelimiter);
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

    EnterNormalMode;
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
      if IsLineBreak(AdvanceOffset) then begin
        EnterNormalMode;
        AdvanceChar(AdvanceOffset);
        Exit;
      end;

      AdvanceChar(1);
    end;

    lckMultiLine: begin
      if IsMultiLineCommentEnd(FState.CommentEndDelimiter, AdvanceOffset) then begin
        Dec(FState.CommentDepth);
        AdvanceChar(AdvanceOffset);
        if FState.CommentDepth <= 0 then
          EnterNormalMode;

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

  if IsDirectiveEnd(FDirectiveDelimiter.EndDelimiter, AdvanceOffset) then begin
    Dec(FState.DirectiveDepth);
    AdvanceChar(AdvanceOffset);
    if FState.DirectiveDepth <= 0 then
      EnterNormalMode;

    Exit;
  end;

  if IsLineBreak(AdvanceOffset) then begin
    // an unterminated compiler directive is allowed to remain in directive mode,
    // this is important while the user is editing incomplete source
    AdvanceChar(1);
    Exit;
  end;

  FState.CompilerDirective := FState.CompilerDirective + GetCurrentChar;

  AdvanceChar(1);
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
    lmNormal   : ProcessNormalCharacter;
    lmString   : ProcessStringCharacter;
    lmComment  : ProcessCommentCharacter;
    lmDirective: ProcessDirectiveCharacter;
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

function TNEDLexer.GetCurrentLocation: TNEDSourceLocation;
begin
  Result.Line := FLine;
  Result.Column := FColumn;
  Result.Length := 0;
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

end.

