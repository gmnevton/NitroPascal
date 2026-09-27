//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-09
// All rights reserved.
//

unit ned_editor_parser;

interface

uses
  SysUtils,
  Classes,
  Generics.Collections,
  ned_editor_lexer,
  ned_editor_texer;

type
{ Structural state }

  TNEDStructuralFrameKindEnum = (
    sfkUnknown,

    sfkProgram,
    sfkUnit,
    sfkNamespace,

    sfkClass,
    sfkStructure,
    sfkRecord,
    sfkUnion,
    sfkInterface,
    sfkEnum,

    sfkProcedure,
    sfkFunction,

    sfkBeginBlock,

    sfkIf,
    sfkElse,

    sfkCase,
    sfkCaseBranch,

    sfkFor,
    sfkWhile,
    sfkRepeat,

    sfkWith,

    sfkTry,
    sfkExcept,
    sfkFinally
  );

  TNEDParserExpectationEnum = (
    peNone,
    peIdentifier,
    peType,
    peExpression,
    peStatement,
    peParameter,
    peMember,
    peOperator,
    peThen,
    peElse,
    peEnd,
    peDeclaration
  );

  TNEDStructuralFrame = record
    Kind: TNEDStructuralFrameKindEnum;
    Name: UTF8String;
    ScopeID: Cardinal;
    StartLine: Integer;
    StartColumn: Integer;
  end;

  TNEDStructuralStack = record
  private
    FFrames: Array of TNEDStructuralFrame;
    //
    function GetCount: Integer;
    function GetFrame(AIndex: Integer): TNEDStructuralFrame;
  public
    procedure Clear;

    procedure Push(const AFrame: TNEDStructuralFrame);
    function Pop(out AFrame: TNEDStructuralFrame): Boolean;
    function Peek(out AFrame: TNEDStructuralFrame): Boolean;
    function Find(const AKind: TNEDStructuralFrameKindEnum; out AFrame: TNEDStructuralFrame): Boolean;
    function Contains(const AKind: TNEDStructuralFrameKindEnum): Boolean;

    property Count: Integer read GetCount;
    property Frames[AIndex: Integer]: TNEDStructuralFrame read GetFrame;
  end;

  TNEDStructuralState = record
    Stack: TNEDStructuralStack;
    CurrentConstruct: TNEDStructuralFrameKindEnum;
    Expectation: TNEDParserExpectationEnum;
    PendingConstruct: TNEDStructuralFrameKindEnum;
    PendingName: UTF8String;
    PendingStartLine: Integer;
    PendingStartColumn: Integer;
  end;

{ Semantic state }

  TNEDScopeKindEnum = (
    skUnknown,
    skGlobal,
    skNamespace,
    skUnit,
    skClass,
    skStructure,
    skProcedure,
    skFunction,
    skBlock
  );

  TNEDSymbolKindEnum = (
    symUnknown,
    symVariable,
    symConstant,
    symType,
    symField,
    symProperty,
    symParameter,
    symProcedure,
    symFunction,
    symClass,
    symStructure,
    symEnum,
    symEnumValue,
    symNamespace,
    symUnit
  );

  TNEDSymbol = class;
  TNEDScope = class;

  TNEDSymbol = class
  public
    ID: Cardinal;
    Name: UTF8String;
    Kind: TNEDSymbolKindEnum;
    SymbolTypeID: Cardinal;

    Declaration: TNEDSourceLocation;
    OwnerScope: TNEDScope;
    OwnerSymbol: TNEDSymbol;
  end;

  TNEDScope = class
  public
    ID: Cardinal;
    Kind: TNEDScopeKindEnum;
    Name: UTF8String;

    Parent: TNEDScope;
    OwnerSymbol: TNEDSymbol;
  end;

  TNEDSemanticState = record
    CurrentScopeID: Cardinal;
    CurrentSymbolID: Cardinal;
    CurrentTypeID: Cardinal;
  end;

{ Complete parser state }

  TNEDParserState = record
    Lexical: TNEDLexicalState;
    Structural: TNEDStructuralState;
    Semantic: TNEDSemanticState;

    StateID: Cardinal;
  end;

{ Line flags }

  TNEDLineContextFlagEnum = (
    lcfContainsDeclaration,
    lcfContainsReference,

    lcfContainsTypeDeclaration,
    lcfContainsClassDeclaration,
    lcfContainsStructureDeclaration,
    lcfContainsProcedureDeclaration,
    lcfContainsFunctionDeclaration,

    lcfOpensScope,
    lcfClosesScope,

    lcfContainsExpression,
    lcfContainsStatement,

    lcfContainsError,
    lcfIncomplete,

    lcfContextDependsOnPreviousLine,
    lcfContextStable
  );

  TNEDLineContextFlags = set of TNEDLineContextFlagEnum;

{ Code elements }

  TNEDCodeElementKindEnum = (
    cekUnknown,
    cekToken,
    cekDeclaration,
    cekReference,
    cekStatement,
    cekExpression,
    cekType,
    cekBlock
  );

  TNEDCodeElement = record
    Kind: TNEDCodeElementKindEnum;

    SourceRange: TNEDSourceRange;
  end;

  TNEDSymbolReference = record
    SymbolID: Cardinal;

    SourceRange: TNEDSourceRange;
  end;

{ Line context }

  TNEDLineContext = record
    InState: TNEDParserState;
    OutState: TNEDParserState;

    Flags: TNEDLineContextFlags;

    Version: Cardinal;
  end;

  TNEDLineAnalysis = class
  public
    Context: TNEDLineContext;
    Tokens: TNEDLineTokens;
    //
    Elements: Array of TNEDCodeElement;
    Declarations: Array of TNEDSymbolReference;
    References: Array of TNEDSymbolReference;
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Clear;
    //
    procedure AddElement(const AKind: TNEDCodeElementKindEnum; const AStartOffset, ALength: Integer);
    procedure AddDeclaration(const ASymbolID: Cardinal; const AStartOffset, ALength: Integer);
    procedure AddReference(const ASymbolID: Cardinal; const AStartOffset, ALength: Integer);
  end;

//  TNEDCursorPosition = record
//    Line: Integer;
//    Column: Integer;
//    TokenIndex: Integer;
//  end;
//
//  TNEDAnalysisCursor = class
//  private
//    FPosition: TNEDCursorPosition;
//    FState: TNEDParserState;
//  public
//    procedure MoveNextToken;
//    procedure MovePreviousToken;
//
//    procedure MoveNextStatement;
//    procedure MovePreviousStatement;
//
//    procedure MoveIntoScope;
//    procedure MoveOutOfScope;
//
//    procedure MoveNextLine;
//    procedure MovePreviousLine;
//
//    property Position: TNEDCursorPosition read FPosition;
//    property State: TNEDParserState read FState;
//  end;

{ Parser }

  TNEDParser = class
  private
    FVersion: Cardinal;

    function GetTokenText(const AText: UTF8String; const AToken: TNEDTextToken): UTF8String;
    function IsToken(const AText: UTF8String; const AToken: TNEDTextToken; const AValue: UTF8String): Boolean;
    function IsTokenKind(const AToken: TNEDTextToken; const AKind: TNEDTextTokenKindEnum): Boolean;
    function TokenLocation(const AText: UTF8String; const AToken: TNEDTextToken): TNEDSourceLocation;
    function TokenRange(const AText: UTF8String; const AToken: TNEDTextToken): TNEDSourceRange;
    function IsInsideRange(const AToken: TNEDTextToken; const AStartOffset, AEndOffset: Integer): Boolean;
    function GetFrameScopeKind(const AKind: TNEDStructuralFrameKindEnum): TNEDScopeKindEnum;
    function GetDeclarationSymbolKind(const AKind: TNEDStructuralFrameKindEnum): TNEDSymbolKindEnum;
    function CreateScopeID(const AParentScopeID: Cardinal; const AFrame: TNEDStructuralFrame): Cardinal;
    function CalculateStateID(const AState: TNEDParserState): Cardinal;
    procedure PushFrame(var AState: TNEDParserState; const AKind: TNEDStructuralFrameKindEnum; const AName: UTF8String; const ALine, AColumn: Integer);
    procedure PopFrame(var AState: TNEDParserState);
    procedure ProcessKeyword(const AText: UTF8String; const AToken: TNEDTextToken; var AState: TNEDParserState; AAnalysis: TNEDLineAnalysis);
    procedure ProcessIdentifier(const AToken: TNEDTextToken; var AState: TNEDParserState; AAnalysis: TNEDLineAnalysis);
    procedure ProcessOperator(const AText: UTF8String; const AToken: TNEDTextToken; var AState: TNEDParserState; AAnalysis: TNEDLineAnalysis);
    procedure ProcessToken(const AText: UTF8String; const AToken: TNEDTextToken; var AState: TNEDParserState; AAnalysis: TNEDLineAnalysis);
    procedure UpdateExpectation(const AText: UTF8String; const AToken: TNEDTextToken; var AState: TNEDParserState);
  public
    constructor Create;
    //
    procedure Reset;
    //
    class function InitialState: TNEDParserState; static;
    //
    procedure ParseRange(const AText: UTF8String; const ATokens: TNEDTextTokenList; const AStartOffset, AEndOffset: Integer; const AInState: TNEDParserState; out AAnalysis: TNEDLineAnalysis);
    function StatesEqual(const ALeft, ARight: TNEDParserState): Boolean;
    //
    property Version: Cardinal read FVersion;
  end;

implementation

{ Helper functions }

function TNEDParser.GetTokenText(const AText: UTF8String; const AToken: TNEDTextToken): UTF8String;
begin
  Result := Copy(AText, AToken.Offset, AToken.Length);
end;

function TNEDParser.IsToken(const AText: UTF8String; const AToken: TNEDTextToken; const AValue: UTF8String): Boolean;
begin
  Result := SameText(String(GetTokenText(AText, AToken)), String(AValue));
end;

function TNEDParser.IsTokenKind(const AToken: TNEDTextToken; const AKind: TNEDTextTokenKindEnum): Boolean;
begin
  Result := AToken.Kind = AKind;
end;

function TNEDParser.TokenLocation(const AText: UTF8String; const AToken: TNEDTextToken): TNEDSourceLocation;
var
  I: Integer;
begin
  Result.Line := 1;
  Result.Column := 1;
  Result.Length := AToken.Length;

  if AToken.Offset <= 1 then
    Exit;

  for I := 1 to AToken.Offset - 1 do begin
    if AText[I] = #10 then begin
      Inc(Result.Line);
      Result.Column := 1;
    end
    else
      Inc(Result.Column);
  end;
end;

function TNEDParser.TokenRange(const AText: UTF8String; const AToken: TNEDTextToken): TNEDSourceRange;
begin
  Result.StartPos := TokenLocation(AText, AToken);
  Result.EndPos := Result.StartPos;

  Inc(Result.EndPos.Column, AToken.Length);
  Result.EndPos.Length := 0;
end;

function TNEDParser.IsInsideRange(const AToken: TNEDTextToken; const AStartOffset, AEndOffset: Integer): Boolean;
var
  TokenEnd: Integer;
begin
  TokenEnd := AToken.Offset + AToken.Length - 1;

  Result := (AToken.Offset <= AEndOffset) and (TokenEnd >= AStartOffset);
end;

function TNEDParser.GetFrameScopeKind(const AKind: TNEDStructuralFrameKindEnum): TNEDScopeKindEnum;
begin
  case AKind of
    sfkProgram  : Result := skGlobal;

    sfkUnit     : Result := skUnit;

    sfkNamespace: Result := skNamespace;

    sfkClass    : Result := skClass;

    sfkStructure,
    sfkRecord,
    sfkUnion,
    sfkInterface: Result := skStructure;

    sfkProcedure: Result := skProcedure;

    sfkFunction : Result := skFunction;

    sfkBeginBlock,
    sfkIf,
    sfkElse,
    sfkCase,
    sfkCaseBranch,
    sfkFor,
    sfkWhile,
    sfkRepeat,
    sfkWith,
    sfkTry,
    sfkExcept,
    sfkFinally  : Result := skBlock;
  else
    Result := skUnknown;
  end;
end;

function TNEDParser.GetDeclarationSymbolKind(const AKind: TNEDStructuralFrameKindEnum): TNEDSymbolKindEnum;
begin
  case AKind of
    sfkClass    : Result := symClass;

    sfkStructure,
    sfkRecord,
    sfkUnion,
    sfkInterface: Result := symStructure;

    sfkEnum     : Result := symEnum;

    sfkProcedure: Result := symProcedure;

    sfkFunction : Result := symFunction;
  else
    Result := symUnknown;
  end;
end;

function TNEDParser.CreateScopeID(const AParentScopeID: Cardinal; const AFrame: TNEDStructuralFrame): Cardinal;
var
  I: Integer;
  H: Cardinal;
begin
  // Deterministic scope ID.
  //
  // This is intentional.
  // A scope ID should not change merely because the parser was restarted from an earlier line.
  H := 2166136261;

  H := H xor AParentScopeID;
  H := H * 16777619;

  H := H xor Ord(AFrame.Kind);
  H := H * 16777619;

  for I := 1 to Length(AFrame.Name) do begin
    H := H xor Ord(AFrame.Name[I]);
    H := H * 16777619;
  end;

  H := H xor Cardinal(AFrame.StartLine);
  H := H * 16777619;

  H := H xor Cardinal(AFrame.StartColumn);
  H := H * 16777619;

  if H = 0 then
    H := 1;

  Result := H;
end;

function TNEDParser.CalculateStateID(const AState: TNEDParserState): Cardinal;
var
  I: Integer;
  H: Cardinal;
  Frame: TNEDStructuralFrame;
begin
  H := 2166136261;

  H := H xor Ord(AState.Lexical.Mode);
  H := H * 16777619;

  H := H xor Cardinal(AState.Lexical.CommentDepth);
  H := H * 16777619;

  H := H xor Ord(AState.Lexical.CommentKind);
  H := H * 16777619;

  H := H xor Cardinal(AState.Lexical.DirectiveDepth);
  H := H * 16777619;

  H := H xor Ord(AState.Structural.CurrentConstruct);
  H := H * 16777619;

  H := H xor Ord(AState.Structural.Expectation);
  H := H * 16777619;

  H := H xor Ord(AState.Structural.PendingConstruct);
  H := H * 16777619;

  H := H xor AState.Semantic.CurrentScopeID;
  H := H * 16777619;

  for I := 0 to AState.Structural.Stack.Count - 1 do begin
    Frame := AState.Structural.Stack.Frames[I];

    H := H xor Ord(Frame.Kind);
    H := H * 16777619;

    H := H xor Frame.ScopeID;
    H := H * 16777619;
  end;

  if H = 0 then
    H := 1;

  Result := H;
end;

procedure TNEDParser.PushFrame(var AState: TNEDParserState; const AKind: TNEDStructuralFrameKindEnum; const AName: UTF8String; const ALine, AColumn: Integer);
var
  Frame: TNEDStructuralFrame;
  ParentScopeID: Cardinal;
begin
  FillChar(Frame, SizeOf(Frame), 0);

  Frame.Kind := AKind;
  Frame.Name := AName;

  Frame.StartLine := ALine;
  Frame.StartColumn := AColumn;

  ParentScopeID := AState.Semantic.CurrentScopeID;
  Frame.ScopeID := CreateScopeID(ParentScopeID, Frame);

  AState.Structural.Stack.Push(Frame);
  AState.Structural.CurrentConstruct := AKind;

  AState.Semantic.CurrentScopeID := Frame.ScopeID;
  AState.Semantic.CurrentSymbolID := 0;

  AState.Structural.Expectation := peNone;
end;

procedure TNEDParser.PopFrame(var AState: TNEDParserState);
var
  Frame: TNEDStructuralFrame;
  ParentFrame: TNEDStructuralFrame;
begin
  if not AState.Structural.Stack.Pop(Frame) then begin
    AState.Structural.CurrentConstruct := sfkUnknown;
    Exit;
  end;

  if AState.Structural.Stack.Peek(ParentFrame) then begin
    AState.Structural.CurrentConstruct := ParentFrame.Kind;

    AState.Semantic.CurrentScopeID := ParentFrame.ScopeID;
  end
  else begin
    AState.Structural.CurrentConstruct := sfkUnknown;

    AState.Semantic.CurrentScopeID := 0;
  end;

  AState.Semantic.CurrentSymbolID := 0;

  AState.Structural.Expectation := peNone;
end;

procedure TNEDParser.ProcessKeyword(const AText: UTF8String; const AToken: TNEDTextToken; var AState: TNEDParserState; AAnalysis: TNEDLineAnalysis);
var
  Location: TNEDSourceLocation;
  Name: UTF8String;
  FrameKind: TNEDStructuralFrameKindEnum;
begin
  Location := TokenLocation(AText, AToken);
  Name := GetTokenText(AText, AToken);

  { program }
  if SameText(string(Name), 'program') then begin
    PushFrame(AState, sfkProgram, '', Location.Line, Location.Column);
    Include(AAnalysis.Context.Flags, lcfOpensScope);
    Exit;
  end;

  { unit }
  if SameText(string(Name), 'unit') then begin
    PushFrame(AState, sfkUnit, '', Location.Line, Location.Column);
    Include(AAnalysis.Context.Flags, lcfOpensScope);
    Exit;
  end;

  { namespace }
  if SameText(string(Name), 'namespace') then begin
    PushFrame(AState, sfkNamespace, '', Location.Line, Location.Column);
    Include(AAnalysis.Context.Flags, lcfOpensScope);
    Exit;
  end;

  { class }
  if SameText(string(Name), 'class') then begin
    FrameKind := sfkClass;
    PushFrame(AState, FrameKind, '', Location.Line, Location.Column);
    Include(AAnalysis.Context.Flags, lcfContainsClassDeclaration);
    Include(AAnalysis.Context.Flags, lcfContainsTypeDeclaration);
    Include(AAnalysis.Context.Flags, lcfOpensScope);
    AAnalysis.AddElement(cekType, AToken.Offset, AToken.Length);
    Exit;
  end;

  { structure }
  if SameText(string(Name), 'structure') then begin
    FrameKind := sfkStructure;
    PushFrame(AState, FrameKind, '', Location.Line, Location.Column);
    Include(AAnalysis.Context.Flags, lcfContainsStructureDeclaration);
    Include(AAnalysis.Context.Flags, lcfContainsTypeDeclaration);
    Include(AAnalysis.Context.Flags, lcfOpensScope);
    AAnalysis.AddElement(cekType, AToken.Offset, AToken.Length);
    Exit;
  end;

  { record }
  if SameText(string(Name), 'record') then begin
    PushFrame(AState, sfkRecord, '', Location.Line, Location.Column);
    Include(AAnalysis.Context.Flags, lcfContainsTypeDeclaration);
    Include(AAnalysis.Context.Flags, lcfOpensScope);
    Exit;
  end;

  { union }
  if SameText(string(Name), 'union') then begin
    PushFrame(AState, sfkUnion, '', Location.Line, Location.Column);
    Include(AAnalysis.Context.Flags, lcfContainsTypeDeclaration);
    Include(AAnalysis.Context.Flags, lcfOpensScope);
    Exit;
  end;

  { interface }
  if SameText(string(Name), 'interface') then begin
    PushFrame(AState, sfkInterface, '', Location.Line, Location.Column);
    Include(AAnalysis.Context.Flags, lcfContainsTypeDeclaration);
    Include(AAnalysis.Context.Flags, lcfOpensScope);
    Exit;
  end;

  { enum }
  if SameText(string(Name), 'enum') then begin
    PushFrame(AState, sfkEnum, '', Location.Line, Location.Column);
    Include(AAnalysis.Context.Flags, lcfContainsTypeDeclaration);
    Include(AAnalysis.Context.Flags, lcfOpensScope);
    Exit;
  end;

  { procedure }
  if SameText(string(Name), 'procedure') then begin
    AState.Structural.PendingConstruct := sfkProcedure;
    AState.Structural.PendingName := '';
    AState.Structural.PendingStartLine := Location.Line;
    AState.Structural.PendingStartColumn := Location.Column;
    AState.Structural.Expectation := peIdentifier;
    Include(AAnalysis.Context.Flags, lcfContainsProcedureDeclaration);
    Include(AAnalysis.Context.Flags, lcfContainsDeclaration);
    Exit;
  end;

  { function }
  if SameText(string(Name), 'function') then begin
    AState.Structural.PendingConstruct := sfkFunction;
    AState.Structural.PendingName := '';
    AState.Structural.PendingStartLine := Location.Line;
    AState.Structural.PendingStartColumn := Location.Column;
    AState.Structural.Expectation := peIdentifier;
    Include(AAnalysis.Context.Flags, lcfContainsFunctionDeclaration);
    Include(AAnalysis.Context.Flags, lcfContainsDeclaration);
    Exit;
  end;

  { begin }
  if SameText(string(Name), 'begin') then begin
    // If a procedure/function was pending, this is its body.
    if AState.Structural.PendingConstruct in [sfkProcedure, sfkFunction] then begin
      PushFrame(AState, AState.Structural.PendingConstruct, AState.Structural.PendingName, AState.Structural.PendingStartLine, AState.Structural.PendingStartColumn);
      AState.Structural.PendingConstruct := sfkUnknown;
      AState.Structural.PendingName := '';
      Include(AAnalysis.Context.Flags, lcfOpensScope);
    end;
    PushFrame(AState, sfkBeginBlock, '', Location.Line, Location.Column);
    Include(AAnalysis.Context.Flags, lcfOpensScope);
    Include(AAnalysis.Context.Flags, lcfContainsStatement);
    AState.Structural.Expectation := peStatement;
    Exit;
  end;

  { end }
  if SameText(string(Name), 'end') then begin
    PopFrame(AState);
    Include(AAnalysis.Context.Flags, lcfClosesScope);
    AState.Structural.Expectation := peNone;
    Exit;
  end;

  { if }
  if SameText(string(Name), 'if') then begin
    PushFrame(AState, sfkIf, '', Location.Line, Location.Column);
    Include(AAnalysis.Context.Flags, lcfContainsStatement);
    AState.Structural.Expectation := peExpression;
    Exit;
  end;

  { else }
  if SameText(string(Name), 'else') then begin
    if AState.Structural.Stack.Contains(sfkIf) then begin
      PushFrame(AState, sfkElse, '', Location.Line, Location.Column);
      Include(AAnalysis.Context.Flags, lcfContainsStatement);
    end;
    AState.Structural.Expectation := peStatement;
    Exit;
  end;

  { case }
  if SameText(string(Name), 'case') then begin
    PushFrame(AState, sfkCase, '', Location.Line, Location.Column);
    Include(AAnalysis.Context.Flags, lcfContainsStatement);
    AState.Structural.Expectation := peExpression;
    Exit;
  end;

  { for }
  if SameText(string(Name), 'for') then begin
    PushFrame(AState, sfkFor, '', Location.Line, Location.Column);
    Include(AAnalysis.Context.Flags, lcfContainsStatement);
    AState.Structural.Expectation := peIdentifier;
    Exit;
  end;

  { while }
  if SameText(string(Name), 'while') then begin
    PushFrame(AState, sfkWhile, '', Location.Line, Location.Column);
    Include(AAnalysis.Context.Flags, lcfContainsStatement);
    AState.Structural.Expectation := peExpression;
    Exit;
  end;

  { repeat }
  if SameText(string(Name), 'repeat') then begin
    PushFrame(AState, sfkRepeat, '', Location.Line, Location.Column);
    Include(AAnalysis.Context.Flags, lcfContainsStatement);
    AState.Structural.Expectation := peStatement;
    Exit;
  end;

  { with }
  if SameText(string(Name), 'with') then begin
    PushFrame(AState, sfkWith, '', Location.Line, Location.Column);
    Include(AAnalysis.Context.Flags, lcfContainsStatement);
    AState.Structural.Expectation := peExpression;
    Exit;
  end;

  { try }
  if SameText(string(Name), 'try') then begin
    PushFrame(AState, sfkTry, '', Location.Line, Location.Column);
    Include(AAnalysis.Context.Flags, lcfContainsStatement);
    Include(AAnalysis.Context.Flags, lcfOpensScope);
    AState.Structural.Expectation := peStatement;
    Exit;
  end;

  { except }
  if SameText(string(Name), 'except') then begin
    PushFrame(AState, sfkExcept, '', Location.Line, Location.Column);
    Include(AAnalysis.Context.Flags, lcfContainsStatement);
    Exit;
  end;

  { finally }
  if SameText(string(Name), 'finally') then begin
    PushFrame(AState, sfkFinally, '', Location.Line, Location.Column);
    Include(AAnalysis.Context.Flags, lcfContainsStatement);
    Exit;
  end;

  { then }
  if SameText(string(Name), 'then') then begin
    AState.Structural.Expectation := peStatement;
    Exit;
  end;

  { var }
  if SameText(string(Name), 'var') then begin
    AState.Structural.Expectation := peDeclaration;
    Exit;
  end;

  { const }
  if SameText(string(Name), 'const') then begin
    AState.Structural.Expectation := peDeclaration;
    Exit;
  end;

  { type }
  if SameText(string(Name), 'type') then begin
    AState.Structural.Expectation := peDeclaration;
    Exit;
  end;

  { return }
  if SameText(string(Name), 'return') then begin
    Include(AAnalysis.Context.Flags, lcfContainsStatement);
    AState.Structural.Expectation := peExpression;
    Exit;
  end;

  { operators / declarations that terminate an expectation }
  if SameText(string(Name), 'do') then begin
    AState.Structural.Expectation := peStatement;
    Exit;
  end;

  if SameText(string(Name), 'of') then begin
    AState.Structural.Expectation := peType;
    Exit;
  end;

  if SameText(string(Name), 'is') then begin
    AState.Structural.Expectation := peType;
    Exit;
  end;

  if SameText(string(Name), 'as') then begin
    AState.Structural.Expectation := peType;
    Exit;
  end;
end;

procedure TNEDParser.ProcessIdentifier(const AToken: TNEDTextToken; var AState: TNEDParserState; AAnalysis: TNEDLineAnalysis);
begin
  // At this stage an identifier is deliberately NOT resolved to a symbol.
  // That belongs to the semantic resolver.
  AAnalysis.AddElement(cekReference, AToken.Offset, AToken.Length);
  Include(AAnalysis.Context.Flags, lcfContainsReference);

  if AState.Structural.PendingConstruct in [sfkProcedure, sfkFunction] then begin
    if AState.Structural.PendingName = '' then begin
      AState.Structural.PendingName := GetTokenText('', AToken);
    end;
  end;

  case AState.Structural.Expectation of
    peType: begin
      AAnalysis.AddElement(cekType, AToken.Offset, AToken.Length);
    end;

    peDeclaration: begin
      AAnalysis.AddElement(cekDeclaration, AToken.Offset, AToken.Length);
      Include(AAnalysis.Context.Flags, lcfContainsDeclaration);
    end;
  end;

  AState.Structural.Expectation := peNone;
end;

procedure TNEDParser.ProcessOperator(const AText: UTF8String; const AToken: TNEDTextToken; var AState: TNEDParserState; AAnalysis: TNEDLineAnalysis);
var
  S: UTF8String;
begin
  S := GetTokenText(AText, AToken);

  AAnalysis.AddElement(cekToken, AToken.Offset, AToken.Length);

  if S = ':' then begin
    AState.Structural.Expectation := peType;
    Exit;
  end;

  if S = '=' then begin
    AState.Structural.Expectation := peExpression;
    Exit;
  end;

  if S = ':=' then begin
    AState.Structural.Expectation := peExpression;
    Include(AAnalysis.Context.Flags, lcfContainsStatement);
    Exit;
  end;

  if S = ';' then begin
    Include(AAnalysis.Context.Flags, lcfContainsStatement);
    // A pending procedure/function declaration which has no body on this line is left pending.
    // The declaration can continue on a later line.
    if not (AState.Structural.PendingConstruct in [sfkProcedure, sfkFunction]) then
      AState.Structural.Expectation := peNone;
    Exit;
  end;

  if S = ',' then begin
    if AState.Structural.Expectation = peType then
      AState.Structural.Expectation := peIdentifier;
    Exit;
  end;

  if S = '.' then begin
    AState.Structural.Expectation := peMember;
    Exit;
  end;

  if S = '(' then begin
    AState.Structural.Expectation := peExpression;
    Exit;
  end;

  if S = ')' then begin
    AState.Structural.Expectation := peNone;
    Exit;
  end;

  if S = '[' then begin
    AState.Structural.Expectation := peExpression;
    Exit;
  end;

  if S = ']' then begin
    AState.Structural.Expectation := peNone;
    Exit;
  end;

  AState.Structural.Expectation := peExpression;
end;

procedure TNEDParser.UpdateExpectation(const AText: UTF8String; const AToken: TNEDTextToken; var AState: TNEDParserState);
begin
  if AToken.Kind = ttkNumber then begin
    AState.Structural.Expectation := peNone;
    Exit;
  end;

  if AToken.Kind = ttkString then begin
    AState.Structural.Expectation := peNone;
    Exit;
  end;
end;

procedure TNEDParser.ProcessToken(const AText: UTF8String; const AToken: TNEDTextToken; var AState: TNEDParserState; AAnalysis: TNEDLineAnalysis);
begin
  case AToken.Kind of
    ttkKeyword: begin
      ProcessKeyword(AText, AToken, AState, AAnalysis);
    end;

    ttkIdentifier: begin
      ProcessIdentifier(AToken, AState, AAnalysis);
    end;

    ttkOperator: begin
      ProcessOperator(AText, AToken, AState, AAnalysis);
    end;

    ttkNumber,
    ttkString: begin
      AAnalysis.AddElement(cekExpression, AToken.Offset, AToken.Length);
      Include(AAnalysis.Context.Flags, lcfContainsExpression);
      UpdateExpectation(AText, AToken, AState);
    end;

    ttkComment: begin
      // Comments have no structural or semantic effect.
    end;
  end;
end;

procedure TNEDParser.ParseRange(const AText: UTF8String; const ATokens: TNEDTextTokenList; const AStartOffset, AEndOffset: Integer; const AInState: TNEDParserState; out AAnalysis: TNEDLineAnalysis);
var
  I: Integer;
  Frame: TNEDStructuralFrame;
  Token: TNEDTextToken;
  State: TNEDParserState;
begin
  AAnalysis := TNEDLineAnalysis.Create;
  AAnalysis.Clear;
  AAnalysis.Context.InState := AInState;

  State := AInState;
  Include(AAnalysis.Context.Flags, lcfContextDependsOnPreviousLine);

  for I := 0 to ATokens.Count - 1 do begin
    Token := ATokens[I];
    if not IsInsideRange(Token, AStartOffset, AEndOffset) then
      Continue;

    ProcessToken(AText, Token, State, AAnalysis);
  end;

  State.Structural.CurrentConstruct := sfkUnknown;

  if State.Structural.Stack.Peek(Frame) then
    State.Structural.CurrentConstruct := Frame.Kind
  else
    State.Structural.CurrentConstruct := sfkUnknown;

  State.StateID := CalculateStateID(State);

  AAnalysis.Context.OutState := State;

  Inc(FVersion);

  AAnalysis.Context.Version := FVersion;
end;

function TNEDParser.StatesEqual(const ALeft, ARight: TNEDParserState): Boolean;
var
  I: Integer;
  LFrame: TNEDStructuralFrame;
  RFrame: TNEDStructuralFrame;
begin
  Result := False;

  if ALeft.StateID <> ARight.StateID then
    Exit;

  if ALeft.Lexical.Mode <> ARight.Lexical.Mode then
    Exit;

  if ALeft.Lexical.CommentDepth <> ARight.Lexical.CommentDepth then
    Exit;

  if ALeft.Lexical.CommentKind <> ARight.Lexical.CommentKind then
    Exit;

  if ALeft.Lexical.DirectiveDepth <> ARight.Lexical.DirectiveDepth then
    Exit;

  if ALeft.Structural.CurrentConstruct <> ARight.Structural.CurrentConstruct then
    Exit;

  if ALeft.Structural.Expectation <> ARight.Structural.Expectation then
    Exit;

  if ALeft.Structural.PendingConstruct <> ARight.Structural.PendingConstruct then
    Exit;

  if ALeft.Structural.Stack.Count <> ARight.Structural.Stack.Count then
    Exit;

  for I := 0 to ALeft.Structural.Stack.Count - 1 do begin
    LFrame := ALeft.Structural.Stack.Frames[I];
    RFrame := ARight.Structural.Stack.Frames[I];

    if LFrame.Kind <> RFrame.Kind then
      Exit;
    if LFrame.ScopeID <> RFrame.ScopeID then
      Exit;
  end;

  if ALeft.Semantic.CurrentScopeID <> ARight.Semantic.CurrentScopeID then
    Exit;

  Result := True;
end;

constructor TNEDParser.Create;
begin
  inherited Create;
  Reset;
end;

procedure TNEDParser.Reset;
begin
  FVersion := 0;
end;

class function TNEDParser.InitialState: TNEDParserState;
begin
  Result.Lexical.Mode := lmNormal;
  Result.Lexical.StringDelimiter := #0;
  Result.Lexical.CommentDepth := 0;
  Result.Lexical.DirectiveDepth := 0;
  Result.Lexical.CompilerDirective := '';

  Result.Structural.Stack.Clear;
  Result.Structural.CurrentConstruct := sfkUnknown;
  Result.Structural.Expectation := peNone;
  Result.Structural.PendingConstruct := sfkUnknown;
  Result.Structural.PendingName := '';
  Result.Structural.PendingStartLine := 0;
  Result.Structural.PendingStartColumn := 0;

  Result.Semantic.CurrentScopeID := 0;
  Result.Semantic.CurrentSymbolID := 0;
  Result.Semantic.CurrentTypeID := 0;

  Result.StateID := 0;
end;

{ TNEDStructuralStack }

procedure TNEDStructuralStack.Clear;
begin
  SetLength(FFrames, 0);
end;

procedure TNEDStructuralStack.Push(const AFrame: TNEDStructuralFrame);
var
  N: Integer;
begin
  N := Length(FFrames);
  SetLength(FFrames, N + 1);
  FFrames[N] := AFrame;
end;

function TNEDStructuralStack.Pop(out AFrame: TNEDStructuralFrame): Boolean;
var
  N: Integer;
begin
  N := Length(FFrames);
  if N = 0 then begin
    FillChar(AFrame, SizeOf(AFrame), 0);
    Result := False;
    Exit;
  end;

  AFrame := FFrames[N - 1];
  SetLength(FFrames, N - 1);
  Result := True;
end;

function TNEDStructuralStack.Peek(out AFrame: TNEDStructuralFrame): Boolean;
var
  N: Integer;
begin
  N := Length(FFrames);
  if N = 0 then begin
    FillChar(AFrame, SizeOf(AFrame), 0);
    Result := False;
    Exit;
  end;

  AFrame := FFrames[N - 1];
  Result := True;
end;

function TNEDStructuralStack.Find(const AKind: TNEDStructuralFrameKindEnum; out AFrame: TNEDStructuralFrame): Boolean;
var
  I: Integer;
begin
  for I := Length(FFrames) - 1 downto 0 do begin
    if FFrames[I].Kind = AKind then begin
      AFrame := FFrames[I];
      Result := True;
      Exit;
    end;
  end;

  FillChar(AFrame, SizeOf(AFrame), 0);
  Result := False;
end;

function TNEDStructuralStack.Contains(const AKind: TNEDStructuralFrameKindEnum): Boolean;
var
  Dummy: TNEDStructuralFrame;
begin
  Result := Find(AKind, Dummy);
end;

function TNEDStructuralStack.GetCount: Integer;
begin
  Result := Length(FFrames);
end;

function TNEDStructuralStack.GetFrame(AIndex: Integer): TNEDStructuralFrame;
begin
  Result := FFrames[AIndex];
end;

{ TNEDLineAnalysis }

constructor TNEDLineAnalysis.Create;
begin
  Tokens := TNEDLineTokens.Create;
  Clear;
end;

destructor TNEDLineAnalysis.Destroy;
begin
  Clear;
  Tokens.Free;
  inherited;
end;

procedure TNEDLineAnalysis.Clear;
begin
  Tokens.Clear;
  FillChar(Context, SizeOf(TNEDLineContext), 0);
  SetLength(Elements, 0);
  SetLength(Declarations, 0);
  SetLength(References, 0);
end;

procedure TNEDLineAnalysis.AddElement(const AKind: TNEDCodeElementKindEnum; const AStartOffset, ALength: Integer);
var
  N: Integer;
begin
  if ALength <= 0 then
    Exit;

  N := Length(Elements);
  SetLength(Elements, N + 1);
  Elements[N].Kind := AKind;
  Elements[N].SourceRange.StartPos.Line := 0;
  Elements[N].SourceRange.StartPos.Column := AStartOffset;
  Elements[N].SourceRange.StartPos.Length := ALength;
  Elements[N].SourceRange.EndPos := Elements[N].SourceRange.StartPos;
  Inc(Elements[N].SourceRange.EndPos.Column, ALength);
end;

procedure TNEDLineAnalysis.AddDeclaration(const ASymbolID: Cardinal; const AStartOffset, ALength: Integer);
var
  N: Integer;
begin
  N := Length(Declarations);
  SetLength(Declarations, N + 1);
  Declarations[N].SymbolID := ASymbolID;
  Declarations[N].SourceRange.StartPos.Column := AStartOffset;
  Declarations[N].SourceRange.StartPos.Length := ALength;
  Declarations[N].SourceRange.EndPos := Declarations[N].SourceRange.StartPos;
  Inc(Declarations[N].SourceRange.EndPos.Column, ALength);
end;

procedure TNEDLineAnalysis.AddReference(const ASymbolID: Cardinal; const AStartOffset, ALength: Integer);
var
  N: Integer;
begin
  N := Length(References);
  SetLength(References, N + 1);
  References[N].SymbolID := ASymbolID;
  References[N].SourceRange.StartPos.Column := AStartOffset;
  References[N].SourceRange.StartPos.Length := ALength;
  References[N].SourceRange.EndPos := References[N].SourceRange.StartPos;
  Inc(References[N].SourceRange.EndPos.Column, ALength);
end;

end.

