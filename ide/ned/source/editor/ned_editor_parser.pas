//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-10
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

    sfkProject, // sfkProgram,
    sfkCode,
    sfkNamespace,

    sfkObject,
    sfkLibrary,
    sfkAssembly,
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
    peDeclaration,
    peType,
    peIdentifier,
    peExpression,
    peStatement,
    peParameter,
    peMember,
    peOperator,
    peThen,
    peElse,
    peEnd
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
    skLocal,
    skNamespace,
    skClass,
    skStructure,
    skProcedure,
    skFunction,
    skBlock
  );

  TNEDSymbolKindEnum = (
    symUnknown,
    symCode,
    symNamespace,
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
    symEnumValue
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

  TNEDParserKeywordRoleEnum = (
    pkrOther,
    pkrDeclaration,
    pkrType,
    pkrModifier,
    pkrSection,
    pkrStatement,
    pkrOperator,
    pkrLiteral,
    pkrStructural
  );

  TNEDParserKeywordActionEnum = (
    pkaNone,

    pkaOpenFrame,
    pkaPendingDeclaration,

    pkaBeginBlock,
    pkaEndBlock,
    pkaElse,

    pkaSetExpectation
  );

  TNEDParserKeyword = class
  public
    Text: UTF8String;
    Kind: TNEDStructuralFrameKindEnum;
    Role: TNEDParserKeywordRoleEnum;
    Action: TNEDParserKeywordActionEnum;
    Expectation: TNEDParserExpectationEnum;
    ContextFlags: TNEDLineContextFlags;
    //
    destructor Destroy; override;
    //
    //
    class function StructuralFrameKindToString(const Value: TNEDStructuralFrameKindEnum): String;
    class function StringToStructuralFrameKind(Value: String): TNEDStructuralFrameKindEnum;
    //
    class function ParserKeywordRoleToString(const Value: TNEDParserKeywordRoleEnum): String;
    class function StringToParserKeywordRole(Value: String): TNEDParserKeywordRoleEnum;
    //
    class function ParserKeywordActionToString(const Value: TNEDParserKeywordActionEnum): String;
    class function StringToParserKeywordAction(Value: String): TNEDParserKeywordActionEnum;
    //
    class function ParserExpectationToString(const Value: TNEDParserExpectationEnum): String;
    class function StringToParserExpectation(Value: String): TNEDParserExpectationEnum;
    //
    class function LineContextFlagsToString(const Value: TNEDLineContextFlags): String;
    class function StringToLineContextFlags(Value: String): TNEDLineContextFlags;
  end;

  TNEDParserConfig = class
  private
    FKeywords: TObjectList<TNEDParserKeyword>;
    FCaseSensitive: Boolean;
    //
    function GetKeyword(const AText: UTF8String): TNEDParserKeyword;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(const ASource: TNEDParserConfig);
    //
    procedure Clear;
    procedure Reset;
    //
    function AddKeyword(const AKeyword: TNEDParserKeyword): TNEDParserKeyword; overload;
    //function AddKeyword(const AText: UTF8String; const AKind: TNEDStructuralFrameKindEnum; const ARole: TNEDParserKeywordRoleEnum): TNEDParserKeyword; overload;
    function AddKeyword(const AText: UTF8String; const AKind: TNEDStructuralFrameKindEnum; const ARole: TNEDParserKeywordRoleEnum; const AAction: TNEDParserKeywordActionEnum;
                        const AExpectation: TNEDParserExpectationEnum; const AContextFlags: TNEDLineContextFlags): TNEDParserKeyword; overload;
    function FindKeyword(const AText: UTF8String): TNEDParserKeyword;
    //
    property Keywords: TObjectList<TNEDParserKeyword> read FKeywords;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
  end;

{ Parser }

  TNEDParser = class
  private
    FConfig: TNEDParserConfig;
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
    destructor Destroy; override;
    //
    procedure Reset;
    //
    class function InitialState: TNEDParserState; static;
    //
    procedure ParseRange(const AText: UTF8String; const ATokens: TNEDTextTokenList; const AStartOffset, AEndOffset: Integer; const AInState: TNEDParserState; out AAnalysis: TNEDLineAnalysis);
    function StatesEqual(const ALeft, ARight: TNEDParserState): Boolean;
    //
    property Config: TNEDParserConfig read FConfig;
    property Version: Cardinal read FVersion;
  end;

implementation

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

{ TNEDParserKeyword }

destructor TNEDParserKeyword.Destroy;
begin
  Text := '';
  inherited;
end;

class function TNEDParserKeyword.StructuralFrameKindToString(const Value: TNEDStructuralFrameKindEnum): String;
begin
  case Value of
    sfkUnknown   : Result := 'Unknown';

    sfkProject   : Result := 'Project';
    sfkCode      : Result := 'Code';
    sfkNamespace : Result := 'Namespace';

    sfkObject    : Result := 'Object';
    sfkLibrary   : Result := 'Library';
    sfkAssembly  : Result := 'Assembly';
    sfkClass     : Result := 'Class';
    sfkStructure : Result := 'Structure';
    sfkRecord    : Result := 'Record';
    sfkUnion     : Result := 'Union';
    sfkInterface : Result := 'Interface';
    sfkEnum      : Result := 'Enum';

    sfkProcedure : Result := 'Procedure';
    sfkFunction  : Result := 'Function';

    sfkBeginBlock: Result := 'BeginBlock';

    sfkIf        : Result := 'If';
    sfkElse      : Result := 'Else';

    sfkCase      : Result := 'Case';
    sfkCaseBranch: Result := 'CaseBranch';

    sfkFor       : Result := 'For';
    sfkWhile     : Result := 'While';
    sfkRepeat    : Result := 'Repeat';

    sfkWith      : Result := 'With';

    sfkTry       : Result := 'Try';
    sfkExcept    : Result := 'Except';
    sfkFinally   : Result := 'Finally';
  else
    Result := 'Unknown';
  end;
end;

class function TNEDParserKeyword.StringToStructuralFrameKind(Value: String): TNEDStructuralFrameKindEnum;
begin
  Value := LowerCase(Value);
  Result := sfkUnknown;
  //
  if Value = 'project' then
    Result := sfkProject
  else if Value = 'code' then
    Result := sfkCode
  else if Value = 'namespace' then
    Result := sfkNamespace
  else if Value = 'object' then
    Result := sfkObject
  else if Value = 'library' then
    Result := sfkLibrary
  else if Value = 'assembly' then
    Result := sfkAssembly
  else if Value = 'class' then
    Result := sfkClass
  else if Value = 'structure' then
    Result := sfkStructure
  else if Value = 'record' then
    Result := sfkRecord
  else if Value = 'union' then
    Result := sfkUnion
  else if Value = 'interface' then
    Result := sfkInterface
  else if Value = 'enum' then
    Result := sfkEnum
  else if Value = 'procedure' then
    Result := sfkProcedure
  else if Value = 'function' then
    Result := sfkFunction
  else if Value = 'beginblock' then
    Result := sfkBeginBlock
  else if Value = 'if' then
    Result := sfkIf
  else if Value = 'else' then
    Result := sfkElse
  else if Value = 'case' then
    Result := sfkCase
  else if Value = 'casebranch' then
    Result := sfkCaseBranch
  else if Value = 'for' then
    Result := sfkFor
  else if Value = 'while' then
    Result := sfkWhile
  else if Value = 'repeat' then
    Result := sfkRepeat
  else if Value = 'with' then
    Result := sfkWith
  else if Value = 'try' then
    Result := sfkTry
  else if Value = 'except' then
    Result := sfkExcept
  else if Value = 'finally' then
    Result := sfkFinally;
end;

class function TNEDParserKeyword.ParserKeywordRoleToString(const Value: TNEDParserKeywordRoleEnum): String;
begin
  case Value of
    pkrOther      : Result := 'Other';
    pkrDeclaration: Result := 'Declaration';
    pkrType       : Result := 'Type';
    pkrModifier   : Result := 'Modifier';
    pkrSection    : Result := 'Section';
    pkrStatement  : Result := 'Statement';
    pkrOperator   : Result := 'Operator';
    pkrLiteral    : Result := 'Literal';
    pkrStructural : Result := 'Structural';
  else
    Result := 'Other';
  end;
end;

class function TNEDParserKeyword.StringToParserKeywordRole(Value: String): TNEDParserKeywordRoleEnum;
begin
  Value := LowerCase(Value);
  Result := pkrOther;
  //
  if Value = 'declaration' then
    Result := pkrDeclaration
  else if Value = 'type' then
    Result := pkrType
  else if Value = 'modifier' then
    Result := pkrModifier
  else if Value = 'section' then
    Result := pkrSection
  else if Value = 'statement' then
    Result := pkrStatement
  else if Value = 'operator' then
    Result := pkrOperator
  else if Value = 'literal' then
    Result := pkrLiteral
  else if Value = 'structural' then
    Result := pkrStructural;
end;

class function TNEDParserKeyword.ParserKeywordActionToString(const Value: TNEDParserKeywordActionEnum): String;
begin
  case Value of
    pkaNone      : Result := 'None';
    pkaOpenFrame : Result := 'OpenFrame';
    pkaPendingDeclaration: Result := 'PendingDeclaration';
    pkaBeginBlock: Result := 'BeginBlock';
    pkaEndBlock  : Result := 'EndBlock';
    pkaElse      : Result := 'Else';
    pkaSetExpectation    : Result := 'SetExpectation';
  end;
end;

class function TNEDParserKeyword.StringToParserKeywordAction(Value: String): TNEDParserKeywordActionEnum;
begin
  Value := LowerCase(Value);
  Result := pkaNone;
  //
  if Value = 'openframe' then
    Result := pkaOpenFrame
  else if Value = 'pendingdeclaration' then
    Result := pkaPendingDeclaration
  else if Value = 'beginblock' then
    Result := pkaBeginBlock
  else if Value = 'endblock' then
    Result := pkaEndBlock
  else if Value = 'else' then
    Result := pkaElse
  else if Value = 'setexpectation' then
    Result := pkaSetExpectation;
end;

class function TNEDParserKeyword.ParserExpectationToString(const Value: TNEDParserExpectationEnum): String;
begin
  case Value of
    peNone       : Result := 'None';
    peDeclaration: Result := 'Declaration';
    peType       : Result := 'Type';
    peIdentifier : Result := 'Identifier';
    peExpression : Result := 'Expression';
    peStatement  : Result := 'Statement';
    peParameter  : Result := 'Parameter';
    peMember     : Result := 'Member';
    peOperator   : Result := 'Operator';
    peThen       : Result := 'Then';
    peElse       : Result := 'Else';
    peEnd        : Result := 'End';
  end;
end;

class function TNEDParserKeyword.StringToParserExpectation(Value: String): TNEDParserExpectationEnum;
begin
  Value := LowerCase(Value);
  Result := peNone;
  //
  if Value = 'declaration' then
    Result := peDeclaration
  else if Value = 'type' then
    Result := peType
  else if Value = 'identifier' then
    Result := peIdentifier
  else if Value = 'expression' then
    Result := peExpression
  else if Value = 'statement' then
    Result := peStatement
  else if Value = 'parameter' then
    Result := peParameter
  else if Value = 'member' then
    Result := peMember
  else if Value = 'operator' then
    Result := peOperator
  else if Value = 'then' then
    Result := peThen
  else if Value = 'else' then
    Result := peElse
  else if Value = 'end' then
    Result := peEnd;
end;

class function TNEDParserKeyword.LineContextFlagsToString(const Value: TNEDLineContextFlags): String;
begin
  Result := '';
  //
  if lcfContainsDeclaration in Value then
    Result := Result + 'ContainsDeclaration;';

  if lcfContainsReference in Value then
    Result := Result + 'ContainsReference;';

  if lcfContainsTypeDeclaration in Value then
    Result := Result + 'ContainsTypeDeclaration;';

  if lcfContainsClassDeclaration in Value then
    Result := Result + 'ContainsClassDeclaration;';

  if lcfContainsStructureDeclaration in Value then
    Result := Result + 'ContainsStructureDeclaration;';

  if lcfContainsProcedureDeclaration in Value then
    Result := Result + 'ContainsProcedureDeclaration;';

  if lcfContainsFunctionDeclaration in Value then
    Result := Result + 'ContainsFunctionDeclaration;';

  if lcfOpensScope in Value then
    Result := Result + 'OpensScope;';

  if lcfClosesScope in Value then
    Result := Result + 'ClosesScope;';

  if lcfContainsExpression in Value then
    Result := Result + 'ContainsExpression;';

  if lcfContainsStatement in Value then
    Result := Result + 'ContainsStatement;';

  if lcfContainsError in Value then
    Result := Result + 'ContainsError;';

  if lcfIncomplete in Value then
    Result := Result + 'Incomplete;';

  if lcfContextDependsOnPreviousLine in Value then
    Result := Result + 'ContextDependsOnPreviousLine;';

  if lcfContextStable in Value then
    Result := Result + 'ContextStable;';
end;

class function TNEDParserKeyword.StringToLineContextFlags(Value: String): TNEDLineContextFlags;
begin
  Value := LowerCase(Value);
  Result := [];
  //
  if Value.Contains('containsdeclaration') then
    Include(Result, lcfContainsDeclaration);

  if Value.Contains('containsreference') then
    Include(Result, lcfContainsReference);

  if Value.Contains('containstypedeclaration') then
    Include(Result, lcfContainsTypeDeclaration);

  if Value.Contains('containsclassdeclaration') then
    Include(Result, lcfContainsClassDeclaration);

  if Value.Contains('containsstructuredeclaration') then
    Include(Result, lcfContainsStructureDeclaration);

  if Value.Contains('containsproceduredeclaration') then
    Include(Result, lcfContainsProcedureDeclaration);

  if Value.Contains('containsfunctiondeclaration') then
    Include(Result, lcfContainsFunctionDeclaration);

  if Value.Contains('opensscope') then
    Include(Result, lcfOpensScope);

  if Value.Contains('closesscope') then
    Include(Result, lcfClosesScope);

  if Value.Contains('containsexpression') then
    Include(Result, lcfContainsExpression);

  if Value.Contains('containsstatement') then
    Include(Result, lcfContainsStatement);

  if Value.Contains('containserror') then
    Include(Result, lcfContainsError);

  if Value.Contains('incomplete') then
    Include(Result, lcfIncomplete);

  if Value.Contains('contextdependsonpreviousline') then
    Include(Result, lcfContextDependsOnPreviousLine);

  if Value.Contains('contextstable') then
    Include(Result, lcfContextStable);
end;

{ TNEDParserConfig }

constructor TNEDParserConfig.Create;
begin
  inherited Create;
  FKeywords := TObjectList<TNEDParserKeyword>.Create(True);
  Reset;
end;

destructor TNEDParserConfig.Destroy;
begin
  Clear;
  FKeywords.Free;
  inherited Destroy;
end;

procedure TNEDParserConfig.Assign(const ASource: TNEDParserConfig);
var
  I: Integer;
begin
  if ASource = Nil then
    Exit;

  FKeywords.Clear;
  FCaseSensitive := ASource.CaseSensitive;

  for I := 0 to ASource.Keywords.Count - 1 do begin
    AddKeyword(ASource.Keywords[I]);
  end;
end;

function TNEDParserConfig.GetKeyword(const AText: UTF8String): TNEDParserKeyword;
var
  I: Integer;
  S1: UTF8String;
  S2: UTF8String;
begin
  Result := nil;

  for I := 0 to FKeywords.Count - 1 do begin
    S1 := FKeywords[I].Text;
    S2 := AText;

    if not FCaseSensitive then begin
      S1 := UTF8String(AnsiLowerCase(String(S1)));
      S2 := UTF8String(AnsiLowerCase(String(S2)));
    end;

    if S1 = S2 then begin
      Result := FKeywords[I];
      Exit;
    end;
  end;
end;

procedure TNEDParserConfig.Clear;
begin
  FKeywords.Clear;
  FCaseSensitive := False;
end;

procedure TNEDParserConfig.Reset;
begin
  FKeywords.Clear;
  FCaseSensitive := False;

  // operators
  AddKeyword('and',       sfkUnknown, pkrOperator, pkaNone, peNone, []);
  AddKeyword('as',        sfkUnknown, pkrOperator, pkaSetExpectation, peType, []);
  AddKeyword('div',       sfkUnknown, pkrOperator, pkaNone, peNone, []);
  AddKeyword('in',        sfkUnknown, pkrOperator, pkaNone, peNone, []);
  AddKeyword('is',        sfkUnknown, pkrOperator, pkaSetExpectation, peType, []);
  AddKeyword('mod',       sfkUnknown, pkrOperator, pkaNone, peNone, []);
  AddKeyword('not',       sfkUnknown, pkrOperator, pkaNone, peNone, []);
  AddKeyword('of',        sfkUnknown, pkrOperator, pkaSetExpectation, peType, []);
  AddKeyword('or',        sfkUnknown, pkrOperator, pkaNone, peNone, []);
  AddKeyword('shl',       sfkUnknown, pkrOperator, pkaNone, peNone, []);
  AddKeyword('shr',       sfkUnknown, pkrOperator, pkaNone, peNone, []);
  AddKeyword('to',        sfkUnknown, pkrOperator, pkaNone, peNone, []);
  AddKeyword('xor',       sfkUnknown, pkrOperator, pkaNone, peNone, []);

  // literals and special predefined values
  AddKeyword('nil',       sfkUnknown, pkrLiteral, pkaNone, peNone, []);
  AddKeyword('result',    sfkUnknown, pkrLiteral, pkaNone, peNone, []);

  // type-related keywords
  AddKeyword('array',     sfkUnknown, pkrType, pkaNone, peNone, []);
  AddKeyword('packed',    sfkUnknown, pkrType, pkaNone, peNone, []);
  AddKeyword('record',    sfkRecord,  pkrStructural, pkaOpenFrame, peNone, [lcfContainsTypeDeclaration, lcfOpensScope]);
  AddKeyword('set',       sfkUnknown, pkrType, pkaNone, peNone, []);
  AddKeyword('string',    sfkUnknown, pkrType, pkaNone, peNone, []);

  // declaration keywords
  AddKeyword('const',          sfkUnknown, pkrDeclaration, pkaSetExpectation, peDeclaration, []);
  AddKeyword('label',          sfkUnknown, pkrDeclaration, pkaSetExpectation, peDeclaration, []);
  AddKeyword('resourcestring', sfkUnknown, pkrDeclaration, pkaSetExpectation, peDeclaration, []);
  AddKeyword('threadvar',      sfkUnknown, pkrDeclaration, pkaSetExpectation, peDeclaration, []);
  AddKeyword('type',           sfkUnknown, pkrDeclaration, pkaSetExpectation, peDeclaration, []);
  AddKeyword('var',            sfkUnknown, pkrDeclaration, pkaSetExpectation, peDeclaration, []);


  // structural declarations
  AddKeyword('class',         sfkClass,     pkrStructural,  pkaOpenFrame, peNone, [lcfContainsClassDeclaration, lcfContainsTypeDeclaration, lcfOpensScope]);
  AddKeyword('code',          sfkCode,      pkrStructural,  pkaOpenFrame, peNone, [lcfOpensScope]);
  AddKeyword('constructor',   sfkProcedure, pkrDeclaration, pkaPendingDeclaration, peIdentifier, [lcfContainsProcedureDeclaration, lcfContainsDeclaration]);
  AddKeyword('destructor',    sfkProcedure, pkrDeclaration, pkaPendingDeclaration, peIdentifier, [lcfContainsProcedureDeclaration, lcfContainsDeclaration]);
  AddKeyword('dispinterface', sfkInterface, pkrStructural,  pkaOpenFrame, peNone, [lcfContainsTypeDeclaration, lcfOpensScope]);
//  AddKeyword('function',      sfkFunction,   pkrStructural);
  AddKeyword('interface',     sfkInterface, pkrStructural,  pkaOpenFrame, peNone, [lcfContainsTypeDeclaration, lcfOpensScope]);
  AddKeyword('object',        sfkObject,    pkrStructural,  pkaOpenFrame, peNone, [lcfContainsTypeDeclaration, lcfOpensScope]);
//  AddKeyword('procedure',     sfkProcedure,  pkrStructural);
//  AddKeyword('program',       sfkProgram,    pkrStructural);
  AddKeyword('library',       sfkLibrary,   pkrStructural,  pkaOpenFrame, peNone, [lcfOpensScope]);
  AddKeyword('project',       sfkProject,   pkrStructural,  pkaOpenFrame, peNone, [lcfOpensScope]);


  // structural blocks and control flow
  AddKeyword('begin',   sfkBeginBlock, pkrStructural, pkaBeginBlock, peStatement,  [lcfOpensScope, lcfContainsStatement]);
  AddKeyword('case',    sfkCase,       pkrStructural, pkaOpenFrame,  peExpression, [lcfContainsStatement]);
  AddKeyword('else',    sfkElse,       pkrStructural, pkaElse,       peStatement,  [lcfContainsStatement]);
  AddKeyword('end',     sfkUnknown,    pkrStructural, pkaEndBlock,   peNone,       [lcfClosesScope]);
  AddKeyword('except',  sfkExcept,     pkrStructural, pkaOpenFrame,  peNone,       [lcfContainsStatement]);
  AddKeyword('finally', sfkFinally,    pkrStructural, pkaOpenFrame,  peNone,       [lcfContainsStatement]);
  AddKeyword('for',     sfkFor,        pkrStructural, pkaOpenFrame,  peIdentifier, [lcfContainsStatement]);
  AddKeyword('if',      sfkIf,         pkrStructural, pkaOpenFrame,  peExpression, [lcfContainsStatement]);
  AddKeyword('repeat',  sfkRepeat,     pkrStructural, pkaOpenFrame,  peStatement,  [lcfContainsStatement]);
  AddKeyword('try',     sfkTry,        pkrStructural, pkaOpenFrame,  peStatement,  [lcfContainsStatement, lcfOpensScope]);
  AddKeyword('while',   sfkWhile,      pkrStructural, pkaOpenFrame,  peExpression, [lcfContainsStatement]);
  AddKeyword('with',    sfkWith,       pkrStructural, pkaOpenFrame,  peExpression, [lcfContainsStatement]);

  // contextual control-flow keywords
  AddKeyword('do',    sfkUnknown, pkrStructural, pkaSetExpectation, peStatement,  []);
  AddKeyword('then',  sfkUnknown, pkrStructural, pkaSetExpectation, peStatement,  []);
  AddKeyword('until', sfkUnknown, pkrStructural, pkaSetExpectation, peExpression, []);

  // Assembly
  AddKeyword('asm', sfkAssembly, pkrStructural, pkaOpenFrame, peNone, [lcfOpensScope]);

  // statements and control transfer
  AddKeyword('break',     sfkUnknown, pkrStatement, pkaNone, peNone, [lcfContainsStatement]);
  AddKeyword('defer',     sfkUnknown, pkrStatement, pkaNone, peNone, [lcfContainsStatement]);
  AddKeyword('exit',      sfkUnknown, pkrStatement, pkaNone, peNone, [lcfContainsStatement]);
  AddKeyword('goto',      sfkUnknown, pkrStatement, pkaNone, peNone, [lcfContainsStatement]);
  AddKeyword('inherited', sfkUnknown, pkrStatement, pkaNone, peNone, [lcfContainsStatement]);
  AddKeyword('raise',     sfkUnknown, pkrStatement, pkaNone, peNone, [lcfContainsStatement]);
  AddKeyword('return',    sfkUnknown, pkrStatement, pkaSetExpectation, peExpression, [lcfContainsStatement]);

  // modifiers and member access
  AddKeyword('default',   sfkUnknown, pkrModifier, pkaNone, peNone, []);
  AddKeyword('export',    sfkUnknown, pkrModifier, pkaNone, peNone, []);
  AddKeyword('exports',   sfkUnknown, pkrModifier, pkaNone, peNone, []);
  AddKeyword('inline',    sfkUnknown, pkrModifier, pkaNone, peNone, []);
  AddKeyword('out',       sfkUnknown, pkrModifier, pkaNone, peNone, []);
  AddKeyword('private',   sfkUnknown, pkrModifier, pkaNone, peNone, []);
  AddKeyword('protected', sfkUnknown, pkrModifier, pkaNone, peNone, []);
  AddKeyword('public',    sfkUnknown, pkrModifier, pkaNone, peNone, []);
  AddKeyword('read',      sfkUnknown, pkrModifier, pkaNone, peNone, []);
  AddKeyword('write',     sfkUnknown, pkrModifier, pkaNone, peNone, []);

  // module, and initialization sections
  AddKeyword('finalization',   sfkUnknown, pkrSection, pkaNone, peNone, []);
  AddKeyword('init',           sfkUnknown, pkrSection, pkaNone, peNone, []);
  AddKeyword('initialization', sfkUnknown, pkrSection, pkaNone, peNone, []);
  AddKeyword('import',         sfkUnknown, pkrSection, pkaNone, peNone, []);
  AddKeyword('imports',        sfkUnknown, pkrSection, pkaNone, peNone, []);
  AddKeyword('uses',           sfkUnknown, pkrSection, pkaNone, peNone, []);

  // remaining contextual keywords
  AddKeyword('property', sfkUnknown, pkrDeclaration, pkaSetExpectation, peDeclaration, []);
end;

function TNEDParserConfig.AddKeyword(const AKeyword: TNEDParserKeyword): TNEDParserKeyword;
begin
  //Result := AddKeyword(AKeyword.Text, AKeyword.Kind, AKeyword.Role);
  Result := AddKeyword(AKeyword.Text, AKeyword.Kind, AKeyword.Role, AKeyword.Action, AKeyword.Expectation, AKeyword.ContextFlags);
end;

//function TNEDParserConfig.AddKeyword(const AText: UTF8String; const AKind: TNEDStructuralFrameKindEnum; const ARole: TNEDParserKeywordRoleEnum): TNEDParserKeyword;
//begin
//  Result := AddKeyword(AText, AKind, ARole, pkaNone, peNone, []);
//end;

function TNEDParserConfig.AddKeyword(const AText: UTF8String; const AKind: TNEDStructuralFrameKindEnum; const ARole: TNEDParserKeywordRoleEnum;
  const AAction: TNEDParserKeywordActionEnum; const AExpectation: TNEDParserExpectationEnum; const AContextFlags: TNEDLineContextFlags): TNEDParserKeyword;
begin
  Result := TNEDParserKeyword.Create;

  Result.Text := AText;
  Result.Kind := AKind;
  Result.Role := ARole;
  Result.Action := AAction;
  Result.Expectation := AExpectation;
  Result.ContextFlags := AContextFlags;

  FKeywords.Add(Result);
end;

function TNEDParserConfig.FindKeyword(const AText: UTF8String): TNEDParserKeyword;
begin
  Result := GetKeyword(AText);
end;

{ TNEDParser }

constructor TNEDParser.Create;
begin
  inherited Create;
  FConfig := TNEDParserConfig.Create;
  Reset;
end;

destructor TNEDParser.Destroy;
begin
  FConfig.Free;
  inherited Destroy;
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
  Result.Lexical.CommentKind := lckNone;
  Result.Lexical.CommentEndDelimiter := '';
  Result.Lexical.CommentCanNest := False;
  Result.Lexical.CommentCanBeNested := False;
  Result.Lexical.DirectiveEndDelimiter := '';
  Result.Lexical.DirectiveDepth := 0;
  Result.Lexical.CompilerVariableEndDelimiter := '';
  Result.Lexical.CompilerVariableDepth := 0;

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
    sfkProject  : Result := skGlobal;

    sfkCode     : Result := skLocal;

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
  Name: UTF8String;
  Keyword: TNEDParserKeyword;
  Location: TNEDSourceLocation;
  FrameKind: TNEDStructuralFrameKindEnum;
begin
  Name := GetTokenText(AText, AToken);

  Keyword := FConfig.FindKeyword(Name);

  if Keyword = Nil then
    Exit;

  Location := TokenLocation(AText, AToken);

  // apply keyword-specific context flags
  AAnalysis.Context.Flags := AAnalysis.Context.Flags + Keyword.ContextFlags;

  case Keyword.Action of
    // open a structural frame
    pkaOpenFrame: begin
      PushFrame(AState, Keyword.Kind, '', Location.Line, Location.Column);
      Include(AAnalysis.Context.Flags, lcfOpensScope);
      if Keyword.Role = pkrType then
        AAnalysis.AddElement(cekType, AToken.Offset, AToken.Length);
    end;

    // start a pending procedure/function declaration
    pkaPendingDeclaration: begin
      AState.Structural.PendingConstruct := Keyword.Kind;
      AState.Structural.PendingName := '';
      AState.Structural.PendingStartLine := Location.Line;
      AState.Structural.PendingStartColumn := Location.Column;
      AState.Structural.Expectation := Keyword.Expectation;
    end;

    // begin block
    pkaBeginBlock: begin
      // complete a pending procedure/function declaration
      if AState.Structural.PendingConstruct in [sfkProcedure, sfkFunction] then begin
        PushFrame(AState, AState.Structural.PendingConstruct, AState.Structural.PendingName, AState.Structural.PendingStartLine, AState.Structural.PendingStartColumn);

        AState.Structural.PendingConstruct := sfkUnknown;
        AState.Structural.PendingName := '';

        Include(AAnalysis.Context.Flags, lcfOpensScope);
      end;

      PushFrame(AState, Keyword.Kind, '', Location.Line, Location.Column);
      Include(AAnalysis.Context.Flags, lcfOpensScope);
    end;

    // close structural frame
    pkaEndBlock: begin
      PopFrame(AState);
      Include(AAnalysis.Context.Flags, lcfClosesScope);
    end;

    // else branch
    pkaElse: begin
      if AState.Structural.Stack.Contains(sfkIf) then begin
        PushFrame(AState, Keyword.Kind, '', Location.Line, Location.Column);
      end;
    end;

    // set parser expectation
    pkaSetExpectation: begin
      AState.Structural.Expectation := Keyword.Expectation;
    end;
  end;

(*
  { program }
  if SameText(string(Name), 'project') then begin
    PushFrame(AState, sfkProject, '', Location.Line, Location.Column);
    Include(AAnalysis.Context.Flags, lcfOpensScope);
    Exit;
  end;

  { unit }
  if SameText(string(Name), 'code') then begin
    PushFrame(AState, sfkCode, '', Location.Line, Location.Column);
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
*)
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

end.

