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
    lmDirective
  );

  TNEDLexicalCommentKindEnum = (
    lckNone,
    lckLine,
    lckBrace,
    lckParenthesis
  );

  TNEDLexicalStringKindEnum = (
    lskNone,
    lskSingleQuote,
    lskDoubleQuote
  );

  TNEDLexicalState = record
    Mode: TNEDLexicalModeEnum;
    StringDelimiter: UTF8Char;
    CommentDepth: Integer;
    CommentKind: TNEDLexicalCommentKindEnum;
    DirectiveDepth: Integer;
    CompilerDirective: UTF8String;
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

  TNEDLexer = class
  private
    FText: UTF8String;
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

    procedure AdvanceChar;

    procedure StartLexeme;

    procedure EnterNormalMode;
    procedure EnterStringMode(ADelimiter: UTF8Char);
    procedure EnterCommentMode(AKind: TNEDLexicalCommentKindEnum);
    procedure EnterDirectiveMode;

    procedure ProcessNormalCharacter(AChar: UTF8Char);
    procedure ProcessStringCharacter(AChar: UTF8Char);
    procedure ProcessCommentCharacter(AChar: UTF8Char);
    procedure ProcessDirectiveCharacter(AChar: UTF8Char);

    function IsLineBreak(AChar: UTF8Char): Boolean;
    function IsStringDelimiter(AChar: UTF8Char): Boolean;

    function IsLineCommentStart: Boolean;
    function IsBraceCommentStart: Boolean;
    function IsParenthesisCommentStart: Boolean;
    function IsDirectiveStart: Boolean;

    function GetCurrentLocation: TNEDSourceLocation;
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Reset;

    procedure SetText(const ALine: Integer; const AText: UTF8String);

    function NextChar(out AChar: UTF8Char): Boolean;
    function PeekChar(AOffset: Integer = 1): UTF8Char;

    function CurrentLocation: TNEDSourceLocation;
    function CurrentRange: TNEDSourceRange;

    function CurrentState: TNEDLexicalState;

    function IsEOF: Boolean;

    property Text: UTF8String read FText;
    property Position: Integer read FPosition;
    property Line: Integer read FLine;
    property Column: Integer read FColumn;

    property State: TNEDLexicalState read FState;
  end;

implementation

{ TNEDLexer }

constructor TNEDLexer.Create;
begin
  inherited Create;

  FText := '';
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
  FState.CommentDepth := 0;
  FState.CommentKind := lckNone;
  FState.DirectiveDepth := 0;
  FState.CompilerDirective := '';

  FTokenStart := 1;
  FTokenStartLine := 1;
  FTokenStartColumn := 1;
end;

procedure TNEDLexer.SetText(const ALine: Integer; const AText: UTF8String);
begin
  FText := AText;
  Reset;
  FLine := ALine;
  FTokenStartLine := ALine;
end;

function TNEDLexer.GetEndOfText: Boolean;
begin
  Result := FPosition > Length(FText);
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

function TNEDLexer.IsLineBreak(AChar: UTF8Char): Boolean;
begin
  Result := AChar = #10;
end;

function TNEDLexer.IsStringDelimiter(AChar: UTF8Char): Boolean;
begin
  Result := (AChar = '''') or (AChar = '"');
end;

function TNEDLexer.IsLineCommentStart: Boolean;
begin
  Result := (GetCurrentChar = '/') and (PeekChar = '/');
end;

function TNEDLexer.IsBraceCommentStart: Boolean;
begin
  Result := GetCurrentChar = '{';
end;

function TNEDLexer.IsParenthesisCommentStart: Boolean;
begin
  Result := (GetCurrentChar = '(') and (PeekChar = '*');
end;

function TNEDLexer.IsDirectiveStart: Boolean;
begin
  Result := (GetCurrentChar = '{') and (PeekChar = '$');
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
  FState.CommentDepth := 0;
  FState.CommentKind := lckNone;
  FState.DirectiveDepth := 0;
  FState.CompilerDirective := '';
end;

procedure TNEDLexer.EnterStringMode(ADelimiter: UTF8Char);
begin
  FState.Mode := lmString;
  FState.StringDelimiter := ADelimiter;
  FState.CommentDepth := 0;
  FState.CommentKind := lckNone;
  FState.DirectiveDepth := 0;
  FState.CompilerDirective := '';
end;

procedure TNEDLexer.EnterCommentMode(AKind: TNEDLexicalCommentKindEnum);
begin
  FState.Mode := lmComment;
  FState.StringDelimiter := #0;
  FState.CommentKind := AKind;
  FState.CommentDepth := 1;
  FState.DirectiveDepth := 0;
  FState.CompilerDirective := '';
end;

procedure TNEDLexer.EnterDirectiveMode;
begin
  FState.Mode := lmDirective;
  FState.StringDelimiter := #0;
  FState.CommentDepth := 0;
  FState.CommentKind := lckNone;
  FState.DirectiveDepth := 1;
  FState.CompilerDirective := '';
end;

procedure TNEDLexer.AdvanceChar;
var
  C: UTF8Char;
begin
  if GetEndOfText then
    Exit;

  C := FText[FPosition];

  Inc(FPosition);

  if C = #13 then begin
    if (FPosition <= Length(FText)) and (FText[FPosition] = #10) then
      Inc(FPosition);

    Inc(FLine);
    FColumn := 1;
  end
  else if C = #10 then begin
    Inc(FLine);
    FColumn := 1;
  end
  else begin
    Inc(FColumn);
  end;
end;

procedure TNEDLexer.ProcessNormalCharacter(AChar: UTF8Char);
begin
  if IsDirectiveStart then begin
    EnterDirectiveMode;
    AdvanceChar;
    AdvanceChar;
    Exit;
  end;

  if IsLineCommentStart then begin
    EnterCommentMode(lckLine);
    AdvanceChar;
    AdvanceChar;
    Exit;
  end;

  if IsParenthesisCommentStart then begin
    EnterCommentMode(lckParenthesis);
    AdvanceChar;
    AdvanceChar;
    Exit;
  end;

  if IsBraceCommentStart then begin
    EnterCommentMode(lckBrace);
    AdvanceChar;
    Exit;
  end;

  if IsStringDelimiter(AChar) then begin
    EnterStringMode(AChar);
    AdvanceChar;
    Exit;
  end;

  AdvanceChar;
end;

procedure TNEDLexer.ProcessStringCharacter(AChar: UTF8Char);
begin
  if AChar = FState.StringDelimiter then begin
    // Pascal-style escaped quote:
    // 'John''s'
    if PeekChar = AChar then begin
      AdvanceChar;
      AdvanceChar;
      Exit;
    end;

    AdvanceChar;

    EnterNormalMode;
    Exit;
  end;

  AdvanceChar;
end;

procedure TNEDLexer.ProcessCommentCharacter(AChar: UTF8Char);
begin
  case FState.CommentKind of
    lckLine: begin
      if IsLineBreak(AChar) then begin
        EnterNormalMode;
        AdvanceChar;
        Exit;
      end;

      AdvanceChar;
    end;

    lckBrace: begin
      if AChar = '}' then begin
        Dec(FState.CommentDepth);
        AdvanceChar;
        if FState.CommentDepth <= 0 then
          EnterNormalMode;

        Exit;
      end;

      // Allow nested brace comments.
      if AChar = '{' then begin
        Inc(FState.CommentDepth);
        AdvanceChar;
        Exit;
      end;

      AdvanceChar;
    end;

    lckParenthesis: begin
      if (AChar = '*') and (PeekChar = ')') then begin
        Dec(FState.CommentDepth);
        AdvanceChar;
        AdvanceChar;

        if FState.CommentDepth <= 0 then
          EnterNormalMode;

        Exit;
      end;

      // Allow nested (* ... *) comments.
      if (AChar = '(') and (PeekChar = '*') then begin
        Inc(FState.CommentDepth);
        AdvanceChar;
        AdvanceChar;

        Exit;
      end;

      AdvanceChar;
    end;
  end;
end;

procedure TNEDLexer.ProcessDirectiveCharacter(AChar: UTF8Char);
begin
  // {$IF ...}
  // {$ENDIF}
  //
  // At this level we deliberately don't interpret the directive semantically.
  // We only maintain the lexical state and collect its textual contents.

  if AChar = '}' then begin
    Dec(FState.DirectiveDepth);
    AdvanceChar;
    if FState.DirectiveDepth <= 0 then
      EnterNormalMode;

    Exit;
  end;

  if IsLineBreak(AChar) then begin
    // An unterminated compiler directive is allowed to remain in directive mode.
    // This is important while the user is editing incomplete source.
    AdvanceChar;
    Exit;
  end;

  FState.CompilerDirective := FState.CompilerDirective + AChar;

  AdvanceChar;
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
    lmNormal   : ProcessNormalCharacter(AChar);
    lmString   : ProcessStringCharacter(AChar);
    lmComment  : ProcessCommentCharacter(AChar);
    lmDirective: ProcessDirectiveCharacter(AChar);
  end;

  Result := True;
end;

function TNEDLexer.PeekChar(AOffset: Integer): UTF8Char;
var
  P: Integer;
begin
  P := FPosition + AOffset;

  if (P < 1) or (P > Length(FText)) then
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

