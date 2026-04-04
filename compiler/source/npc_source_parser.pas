//
// Nitro Pascal Compiler
// version 1.0
//
// Source Parser
//

unit npc_source_parser;

interface

uses
  SysUtils,
  Classes,
  Variants,
  Generics.Collections,
//  Rtti,
  npc_lexer,
  npc_reserved_words,
  npc_reserved_symbols,
  npc_location,
  npc_tokens,
  npc_tokenizer,
  npc_compiler,
  npc_utils,
  npc_error,
  npc_ast;

type
  TNPCParsingType = (
    PROJECT,
    SOURCE
  );

  TNPCSettingType = (
    setProgram,
    setProgramType,
    setSearchPath,
    setDefines,
    setResources
  );

  TNPCImportType = (
    itCode,
    itExternalObject,
    itDLL
  );

  TNPCImport = packed record
    &Type: TNPCImportType;
    Name: String;
    Path: String;
    Resolved: Boolean;
  end;
  TNPCImportArray = Array of TNPCImport;

  TNPCParseStatementFlag = (
    stafStartNewScope,                   // start new scope
    stafStatementIsRequired,             // { } statement is required
    stafStatementIsRequiredWithNewScope, // { } statement is required and start new scope
    stafEmptyStatementIsAcceptable       // empty statement ';' is really ok
  );
  TNPCParseStatementFlags = set of TNPCParseStatementFlag;

  TNPCParseDeclarationFlag = (
    decfNone,
    decfDoNotAddSymbol,
    decfBodyMayBeSkipped,
    decfInlineSymbolDeclaration,
    decfParamDeclarationAsTuple,
    decfParamMayContainModifier
  );
  TNPCParseDeclarationFlags = set of TNPCParseDeclarationFlag;

  TNPCSourceParser = class
  strict private
    Compiler: TNPCCompiler;
    ParentParser: TNPCSourceParser;
    Tokenizer: TNPCTokenizer;
    Texer: TNPCTokensParser;
    //
    SettingsPtr: Pointer;
    ASTree: TNPC_AST;
    ScopeStack: TObjectList<TNPCScope>;
    CurrentScope: TNPCScope;
    ReturnToScope: TNPCScope;
    CurrentBlock: TNPC_ASTStatementBlock;
  private
    Imports: TNPCImportArray;
    Builtin_Type_Boolean: TNPC_ASTTypeDefinition;
    //
    Builtin_Type_Byte: TNPC_ASTTypeDefinition;
    Builtin_Type_Integer: TNPC_ASTTypeDefinition;
    Builtin_Type_Integer64: TNPC_ASTTypeDefinition;
    //
    Builtin_Type_Single: TNPC_ASTTypeDefinition;
    Builtin_Type_Double: TNPC_ASTTypeDefinition;
    Builtin_Type_Extended: TNPC_ASTTypeDefinition;
    //
    Builtin_Type_String: TNPC_ASTTypeDefinition;
    //
    Builtin_Type_Pointer: TNPC_ASTTypeDefinition;
    Builtin_Type_Null: TNPC_ASTTypeDefinition;
    //
    Builtin_Type_ClassSymbol: TNPC_ASTTypeDefinition;
  private
    FLevel: Integer;
    //
    procedure AddProjectImport(const ASourceFile, ACodeName: String);
    function  TypeToString(const Sym: TNPCSymbol): String;
    function  TypeToName(const Typ: TNPC_ASTExpression): String;
    procedure NotImplemented;
  protected
    ParsingType: TNPCParsingType;
    //
    function Unescape(const Value: String): String; inline;
    function  GetPrecedence(const AToken: TNPCToken): Integer;
    procedure PushScope(const AParentScope: TNPCScope; const AScopeName: UTF8String); //inline;
    procedure PopScope; //inline;
    procedure EnterScope(const AScope: TNPCScope); //inline;
    procedure LeaveScope; //inline;
    procedure InitBuiltins(const AScope: TNPCScope);
    procedure Clear;
    //
    procedure AddImport(const AImportType: TNPCImportType; const AImportName: String; const AImportPath: String);
    function  TokenIsOfType(const AToken: TNPCToken; const ATokenTypes: Array of TNPCTokenType): Boolean;
    function  TokenIsIdent(const AToken: TNPCToken): Boolean;// inline;
    function  TokenIsNumber(const AToken: TNPCToken): Boolean;// inline;
    function  TokenIsString(const AToken: TNPCToken): Boolean;// inline;
    function  TokenIsChar(const AToken: TNPCToken): Boolean;// inline;
    function  TokenIsReserved(const AToken: TNPCToken): Boolean;// inline;
    function  TokenIsReservedIdent(const AToken: TNPCToken): Boolean; overload;// inline;
    function  TokenIsReservedIdent(const AToken: TNPCToken; const AReservedIdent: TNPCReservedIdents): Boolean; overload;// inline;
    function  TokenIsReservedIdent(const AToken: TNPCToken; const AReservedIdents: Array of TNPCReservedIdents): Boolean; overload;
    function  TokenIsReservedIdent(const AToken: TNPCToken; const AReservedIdents: Array of String): Boolean; overload;
    function  TokenIsLiteral(const AToken: TNPCToken): Boolean;// inline;
    function  TokenIsBiLiteral(const AToken: TNPCToken): Boolean;// inline;
    function  TokenIsReservedSymbol(const AToken: TNPCToken): Boolean; overload;// inline;
    function  TokenIsReservedSymbol(const AToken: TNPCToken; const AReservedSymbol: TNPCReservedSymbols): Boolean; overload;// inline;
    function  TokenIsReservedSymbol(const AToken: TNPCToken; const AReservedSymbols: Array of TNPCReservedSymbols): Boolean; overload;
    procedure SkipComments;// inline;
    function  SkipComment(const AToken: TNPCToken; const AConsumeToken: Boolean = False): Boolean;// inline;
    function  IsCodeFileNamedAs(const AFileName: String; const ACodeName: String): Boolean;
    function  LookupTypeDefinition(const AToken: TNPCToken): TNPC_ASTTypeDefinition;
    function  IsDeclaration(var AToken: TNPCToken): Boolean;
    procedure AddStatement(const AStmt: TNPC_ASTStatement);
    procedure DeclareSymbol(const AName: String; const ASym: TNPCSymbol);
    function  LookupSymbol(const AName: String): TNPCSymbol;
    function  ResolveType(const ATypeName: String; out AType: TNPC_ASTType): Boolean;
    function  ResolveTypeSize(const ATypeRef: TNPC_ASTTypeReference): Integer;
    function  FindClassMethod(const AClassSym: TNPCSymbol; const AMethodName: String; const AClassMethodDefinitionLocation: TNPCLocation): TNPCSymbol;
    function  ClassContainsMethod(const AClassType: TNPC_ASTTypeClass; const AMethodName: UTF8String; out AMethod: TNPC_ASTType; out AMethodSymbol: TNPCSymbol): Boolean;
    function  ClassContainsProperty(const AClassType: TNPC_ASTTypeClass; const APropertyName: UTF8String; out AProperty: TNPC_ASTType; out APropertySymbol: TNPCSymbol): Boolean;
    procedure RegisterProcedureSymbols(const AProc: TNPC_ASTStatementProcedure);
    function  CompareProcedureSignatures(Decl, Impl: TNPC_ASTStatementProcedure): Boolean;
    function  CompareTypeRef(A, B: TNPC_ASTTypeReference): Boolean;
    function  CompareRecordTypes(A, B: TNPC_ASTTypeReference): Boolean;
    function  CompareFunctionTypes(A, B: TNPC_ASTTypeReference): Boolean;
    procedure BindTupleToVariables(const Targets: TObjectList<TNPCSymbol>; Tuple: TNPC_ASTExpressionTuple; Scope: TNPCScope);
    procedure ValidateTypeCompatibility(Expected: TNPC_ASTType; Expr: TNPC_ASTExpression);
    procedure ValidateTupleTypes(Proc: TNPC_ASTStatementProcedure; Tuple: TNPC_ASTExpressionTuple);
    procedure AssignFieldValue(FieldSym: TNPCSymbol; const Value: TNPC_ASTValue);


//    function  ExpressionToType(const AExpression: TNPC_ASTExpression): TNPC_ASTTypeExpression;

    //
    function  Parse(const ASource: TStringStream; const ASourceFile: String; const ParsingImport: Boolean = False): Boolean;
    //
    procedure ParseProjectBody(const ABlock: TNPC_AST);
    procedure ParseCodeBody(const ABlock: TNPC_AST);
    procedure ParseSettingDefineCondition(const ABlock: TNPC_AST);
    procedure ParseSettingOrDefineDirective(const ABlock: TNPC_AST);
    procedure ParseSettings(const AToken: TNPCToken; const ASettingType: TNPCSettingType; const ABlock: TNPC_AST);
    procedure ParseSettingProgram(const AToken: TNPCToken; const ABlock: TNPC_AST);
    procedure ParseSettingProgramType(const AToken: TNPCToken; const ABlock: TNPC_AST);
    procedure ParseSearchPath(const AToken: TNPCToken; const ABlock: TNPC_AST);

    procedure ParseDefines(const AToken: TNPCToken; const ABlock: TNPC_AST);
    procedure ParseDefinition(const ABlock: TNPC_AST);
    procedure ParseDirectives(const ABlock: TNPC_AST);
    procedure ParseComment;

//  NUD (Null Denotation) and LED (Left Denotation) are two types of parsing functions in a Pratt parser
//  that handle different contexts for a given operator or token.
//  A nud function is called when a token appears at the beginning of an expression (in a prefix or "null" context),
//  such as a number or a unary minus.
//  An led function is called when a token appears after an expression in an infix context,
//  like a binary operator such as addition or multiplication.
//
//  Feature | NUD (Null Denotation)                    | LED (Left Denotation)
//  -----------------------------------------------------------------------------------------------------
//  Context	| Prefix (no expression to its left)       | Infix (an expression is to its left)
//          |                                          |
//  Example | Handling - in -5 or a number literal 123 | Handling + in 1 + 2 or * in 2 * 3
//          |                                          |
//  Purpose | To parse the expression that follows the | To parse the token and the expression that follows it,
//          | token without a preceding expression.	   | using the preceding expression as the left-hand side.
//          |                                          |
//  Function| Often returns a literal value or         | Often creates a new AST node with the left-hand side expression
//          | a unary AST node.                        | and a right-hand side expression parsed by a recursive call.

//type
//  TNudFunc = function: TExpr of object;
//  TLedFunc = function(Left: TExpr): TExpr of object;
//
//procedure TParser.RegisterNud(TokenKind: TTokenKind; NudFunc: TNudFunc);
//begin
//  FNudTable[TokenKind] := NudFunc;
//end;

    function  ParseStatement(const AFlags: TNPCParseStatementFlags): TNPC_ASTStatement;
    function  ParseExpression(const AToken: TNPCToken; Precedence: Integer = 0): TNPC_ASTExpression;
    function  ParsePrimaryExpression(const AToken: TNPCToken): TNPC_ASTExpression;
    //function  ParsePrimaryExpression(const ALeftToken: TNPCToken; const AToken: TNPCToken): TNPC_ASTExpression;
    function  ParseSimpleExpression(const AToken: TNPCToken; const ALeft: TNPC_ASTExpression): TNPC_ASTExpression;
    function  ParseVariableExpression: TNPC_ASTExpressionVariable;

    procedure ParseTypeDeclaration(const AToken: TNPCToken; const AFlags: TNPCParseDeclarationFlags);
    function  ParseTypeDefinition(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
    //function  ParseTypeReference: TNPC_ASTType;
    function  ParseTypeReference: TNPC_ASTTypeReference;
    function  IsFunctionTypeAhead(const AToken: TNPCToken): Boolean;
    function  ParseTypeReference_NamedType(const AToken: TNPCToken): TNPC_ASTTypeReference;
    function  ParseTypeReference_ArrayType(const AToken: TNPCToken): TNPC_ASTTypeReference;
    function  ParseTypeReference_RecordType(const AToken: TNPCToken): TNPC_ASTTypeReference;
    function  ParseTypeReference_FunctionType(const AToken: TNPCToken): TNPC_ASTTypeReference;
    function  ParseTypeReference_QualifiedIdent(const AToken: TNPCToken): TNPCSymbol;

    function  ParseEnumTypeDeclaration(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
    function  ParseSetTypeDeclaration(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
    function  ParseSetOfTypeDeclaration(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
    function  ParseArrayTypeDeclaration(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
    function  ParseRecordTypeDeclaration(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
    //
    function  ParseClassTypeDeclaration(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
    procedure ParseClassPropertyDeclaration(const ATypeClass: TNPC_ASTTypeClass; const AVisibilityType: TNPC_ASTClassVisibilityTypeEnum);
    procedure ParseClassMemberDeclaration(const ATypeClass: TNPC_ASTTypeClass; const AVisibilityType: TNPC_ASTClassVisibilityTypeEnum);
    function  ParseClassMethodDefinition(const AClassSym: TNPCSymbol; const AMethod: TNPCToken; const AClassMethodDefinitionLocation: TNPCLocation): TNPC_ASTStatementProcedure;
    //
    procedure ParseVariableDeclaration(const AToken: TNPCToken);
    function  ParseVariableType(const AVarToken: TNPCToken; const AVarName: String): TNPC_ASTType;

    function  ParseBlock(const AToken: TNPCToken): TNPC_ASTStatement;
    function  ParseQualifiedName(const AToken: TNPCToken; out AClassSym: TNPCSymbol; out AMethod: TNPCToken; out AClassMethodDefinitionLocation: TNPCLocation): Boolean;
    //
    function  ParseProcedureDeclaration(const AToken: TNPCToken; const AMethodSymbol: TNPCSymbol; const AFlags: TNPCParseDeclarationFlags): TNPC_ASTStatement;
    function  ParseProcedureDefinition(const AToken: TNPCToken; const AMethodSymbol: TNPCSymbol; const AFlags: TNPCParseDeclarationFlags): TNPC_ASTStatement;
    procedure ParseProcedureDefinitionHeader(const AProc: TNPC_ASTStatementProcedure; const AFlags: TNPCParseDeclarationFlags);
    function  ParseProcedureDefinitionBody(const AToken: TNPCToken; const AFlags: TNPCParseDeclarationFlags): TNPC_ASTStatement;
    function  ParseParameterList(const AProcedureName: UTF8String; AContext: TNPC_ASTParamContext; const AFlags: TNPCParseDeclarationFlags): TObjectList<TNPC_ASTParameter>;
    function  ParseProcedureParameter(const AToken: TNPCToken; const AProcedureName: UTF8String; AContext: TNPC_ASTParamContext; const AFlags: TNPCParseDeclarationFlags): TNPC_ASTParameter;
    function  ParseProcedureSignatureOnly(const AParent: TNPC_ASTStatementBlock; const AName: String): TNPC_ASTStatementProcedure;
    //
    function  ParseAssignment(const ALeftToken: TNPCToken): TNPC_ASTStatement;
    function  ParseAssignmentNew(const AToken: TNPCToken): TNPC_ASTStatement;
    function  ParseAssignmentOrMulti: TNPC_ASTExpression;
    function  ParseIdentifier(const AToken: TNPCToken): TNPC_ASTExpression;
    function  ParseLiteral(const AToken: TNPCToken): TNPC_ASTExpression;
    //function  ParseLiteral(const ALeftToken: TNPCToken; const AToken: TNPCToken): TNPC_ASTExpression;
    function  ParseTupleExpression(const AToken: TNPCToken): TNPC_ASTExpression;
    function  ParseReturn(const AToken: TNPCToken): TNPC_ASTStatement;
    function  ParseResult(const AToken: TNPCToken): TNPC_ASTStatement;
    function  ParseDefer(const AToken: TNPCToken): TNPC_ASTStatement;

    function  ParseSet(const AToken: TNPCToken): TNPC_ASTExpression;
    function  ParseIndex(const AToken: TNPCToken; const ALeft: TNPC_ASTExpression): TNPC_ASTExpression;
    function  ParseRecordMember(const AToken: TNPCToken; const ALeft: TNPC_ASTExpression): TNPC_ASTExpression;
    function  ParseClassMember(const AToken: TNPCToken; const ALeft: TNPC_ASTExpression): TNPC_ASTExpression;
    function  ParseIfExpression(const AToken: TNPCToken): TNPC_ASTExpressionIf;
    function  ParseIfStatement(const AToken: TNPCToken): TNPC_ASTStatementIf;
    function  ParseCaseExpression(const AToken: TNPCToken): TNPC_ASTExpressionCase;
    function  ParseCaseExpressionBranches(const ASelector: TNPC_ASTExpression; const ACaseOf: Boolean; out ADefaultBranch: TNPC_ASTExpression): TNPC_ASTExpressionCaseBranches;
    function  ParseCaseStatement(const AToken: TNPCToken): TNPC_ASTStatementCase;
    function  ParseCaseStatementBranches(const ASelector: TNPC_ASTExpression; const ACaseOf: Boolean; out ADefaultBranch: TNPC_ASTStatement): TNPC_ASTStatementCaseBranches;
    function  ParseFor(const AToken: TNPCToken): TNPC_ASTStatementFor;
    function  ParseWhile(const AToken: TNPCToken): TNPC_ASTStatementWhile;
    function  ParseCall(const AToken: TNPCToken; const ALeft: TNPC_ASTExpression): TNPC_ASTExpressionCall;
    procedure ParseCallParams(const AToken: TNPCToken);

    procedure SearchImportByCodeName(const AToken: TNPCToken; const SearchPath: String; const SearchFileName: String);
    procedure SearchImportByFileName(const AToken: TNPCToken; const SearchPath: String; const SearchFileName: String);
    procedure ParseImports(const ABlock: TNPC_AST);
    procedure ParseExports(const AToken: TNPCToken);
    procedure ParseTypes(const AToken: TNPCToken);
    procedure ParseConsts(const AToken: TNPCToken);
    procedure ParseVariables(const AToken: TNPCToken);
    procedure ParseInitialization(const AToken: TNPCToken);
    procedure ParseFinalization(const AToken: TNPCToken);
    procedure ParseBegin(const AToken: TNPCToken);
    procedure ParseEnd(const AToken: TNPCToken);

//    function  ParseDeclarations(const AToken: TNPCToken): Boolean;
//    function  ParseStatements(var AToken: TNPCToken; const AExitOnReservedIdents: Array of TNPCReservedIdents; const ALevel: Integer): Boolean;
//    procedure ParseLambdaParams(const AToken: TNPCToken);
    //
    property Settings: Pointer read SettingsPtr;
  public
    constructor Create(const ACompiler: TNPCCompiler; const AParentParser: TNPCSourceParser);
    destructor Destroy; override;
    //
    function ParseImportFile(const ASourceFile: String): Boolean; overload;
    function ParseImportFile(const ASource: TStringStream; const ASourceFile: String): Boolean; overload;
    function ParseSourceCode(const ASourceCode: String): Boolean; overload;
    function ParseSourceCode(const ASource: TStringStream; const ASourceFile: String): Boolean; overload;
    //
    procedure OutputAST(const AFileName: String);
    //
    property AST: TNPC_AST read ASTree;
  end;

procedure BindTupleToReturns(Tuple: TNPC_ASTExpressionTuple; Proc: TNPC_ASTStatementProcedure; out Values: Array of TNPC_ASTExpression);

// additional compiler entry points

function  NPC_CompileImport(const ASourceFileName: PChar; const ACompiler: TNPCCompiler; const AParentParser: TNPCSourceParser; out ASource: TNPCSourceParser): Boolean; stdcall; overload;
function  NPC_CompileImport(const ASourceStream: TStringStream; const ASourceFileName: PChar; const ACompiler: TNPCCompiler; const AParentParser: TNPCSourceParser; out ASource: TNPCSourceParser): Boolean; stdcall; overload;

function  NPC_CompileSource(const ASourceCode: PChar; const ACompiler: TNPCCompiler; const AParentParser: TNPCSourceParser; out ASource: TNPCSourceParser): Boolean; stdcall; overload;
function  NPC_CompileSource(const ASourceStream: TStringStream; const ACompiler: TNPCCompiler; const AParentParser: TNPCSourceParser; out ASource: TNPCSourceParser): Boolean; stdcall; overload;

procedure NPC_CompilerFree(var ASource: TNPCSourceParser); stdcall;

implementation

uses
  Math,
//  Variants,
  StrUtils,
  npc_consts,
  npc_project,
  npc_project_settings,
  npc_md5,
  npc_types;

(*
function EvalTuple(Expr: TNPC_ASTExpressionTuple): TValueArray;
var
  i: Integer;
begin
  SetLength(Result, Expr.Values.Count);

  for i := 0 to Expr.Values.Count - 1 do
    Result[i] := Evaluate(Expr.Values[i]);
end;

procedure AssignReturnValues(Proc: TNPC_ASTStatementProcedure; const Values: TValueArray);
var
  i: Integer;
  RetSym: TNPCSymbol;
begin
  if Length(Values) <> Proc.Returns.Count then
    raise Exception.Create('Return value count mismatch');

  for i := 0 to Proc.Returns.Count - 1 do begin
    RetSym := Proc.ReturnSymbols[i];
    SetSymbolValue(RetSym, Values[i]);
  end;
end;

procedure ExecuteReturn(Stmt: TNPC_ASTStatementReturn);
var
  Values: TValueArray;
begin
  if Stmt.Expr is TNPC_ASTExpressionTuple then
    Values := EvalTuple(TNPC_ASTExpressionTuple(Stmt.Expr))
  else
    Values := [Evaluate(Stmt.Expr)];

  AssignReturnValues(CurrentProcedure, Values);

  CurrentReturnFrame.HasReturned := True;
end;

procedure ExecuteReturn(Stmt: TNPC_ASTStatementReturn);
var
  Values: array of TNPC_ASTExpression;
  i: Integer;
begin
  BindTupleToReturns(Stmt.Expr, CurrentProcedure, Values);

  // type validation
  for i := 0 to High(Values) do
    ValidateTypeCompatibility(CurrentProcedure.Returns[i].TypeRef, Values[i]);

  // assignment
  for i := 0 to High(Values) do
    SetSymbolValue(CurrentProcedure.ReturnSymbols[i], Evaluate(Values[i]));

  ReturnFrame.HasReturned := True;
end;

procedure ExecuteResult(Stmt: TNPC_ASTStatementResult);
var
  Values: TValueArray;
begin
  if Stmt.Expr is TNPC_ASTExpressionTuple then
    Values := EvalTuple(TNPC_ASTExpressionTuple(Stmt.Expr))
  else
    Values := [Evaluate(Stmt.Expr)];

  AssignReturnValues(CurrentProcedure, Values);

  // IMPORTANT: no exit
end;

function Evaluate(Expr: TNPC_ASTExpression): TValue;
begin
  if Expr is TNPC_ASTExpressionNull then
    Exit(ResolveNullValue(Expr.ExpectedType));

  ...
end;

procedure InitializeClassInstance(Cls: TTypeClass);
var
  Prop: TClassProperty;
  Value: TValue;
begin
  for Prop in Cls.Properties do
  begin
    if Prop.InitExpr <> nil then
      Value := Evaluate(Prop.InitExpr)
    else if Prop.DefaultExpr <> nil then
      Value := Evaluate(Prop.DefaultExpr)
    else
      Continue;

    AssignToSymbol(Prop.WriteSym, Value);
  end;
end;
*)

procedure BindTupleToReturns(Tuple: TNPC_ASTExpressionTuple; Proc: TNPC_ASTStatementProcedure; out Values: Array of TNPC_ASTExpression);
var
  i, PosIndex: Integer;
  Item: TNPC_ASTTupleItem;
  Used: Array of Boolean;
begin
  if not Assigned(Tuple) then
    raise NPCCompilerError.Create('tuple object not set');
  if not Assigned(Proc) then
    raise NPCCompilerError.Create('procedure object not set');

  if Tuple.Items.Count > Proc.Returns.Count then
    raise NPCSemanticError.Create('too many tuple values');

  SetLength(Used, Proc.Returns.Count);
  PosIndex := 0;

  for Item in Tuple.Items do begin
    // NAMED
    if Item.Name <> '' then begin
      for i := 0 to Proc.Returns.Count - 1 do begin
        if SameText(Proc.Returns[i].Name, Item.Name) then begin
          Values[i] := Item.Expr;
          Used[i] := True;
          Break;
        end;
      end;
    end
    else begin // POSITIONAL
      while (PosIndex < Length(Used)) and Used[PosIndex] do
        Inc(PosIndex);

      if PosIndex >= Proc.Returns.Count then
        raise NPCSemanticError.Create('too many positional values');

      Values[PosIndex] := Item.Expr;
      Used[PosIndex] := True;
      Inc(PosIndex);
    end;
  end;

  // VALIDATION
  for i := 0 to Proc.Returns.Count - 1 do begin
    if not Used[i] then
      raise NPCSemanticError.CreateFmt('missing value for "%s"', [Proc.Returns[i].Name]);
  end;
end;

function NPC_CompileImport(const ASourceFileName: PChar; const ACompiler: TNPCCompiler; const AParentParser: TNPCSourceParser; out ASource: TNPCSourceParser): Boolean;
//var
//  Source: TNPCSourceParser;
//  idx: Integer;
begin
  ASource := TNPCSourceParser.Create(ACompiler, AParentParser);
//  try
    Result := ASource.ParseImportFile(ASourceFileName);
    if Result and (AParentParser <> Nil) then begin
      AParentParser.Imports[High(AParentParser.Imports)].Resolved := True;
    end;
//  finally
//    Source.Free;
//  end;
end;

function NPC_CompileImport(const ASourceStream: TStringStream; const ASourceFileName: PChar; const ACompiler: TNPCCompiler; const AParentParser: TNPCSourceParser; out ASource: TNPCSourceParser): Boolean;
begin

end;

function NPC_CompileSource(const ASourceCode: PChar; const ACompiler: TNPCCompiler; const AParentParser: TNPCSourceParser; out ASource: TNPCSourceParser): Boolean;
//var
//  Source: TNPCSourceParser;
begin
  ASource := TNPCSourceParser.Create(ACompiler, AParentParser);
//  try
    Result := ASource.ParseSourceCode(ASourceCode);
//  finally
//    Source.Free;
//  end;
end;

function NPC_CompileSource(const ASourceStream: TStringStream; const ACompiler: TNPCCompiler; const AParentParser: TNPCSourceParser; out ASource: TNPCSourceParser): Boolean;
//var
//  Source: TNPCSourceParser;
begin
  ASource := TNPCSourceParser.Create(ACompiler, AParentParser);
//  try
    Result := ASource.ParseSourceCode(ASourceStream, '');
//  finally
//    Source.Free;
//  end;
end;

procedure NPC_CompilerFree(var ASource: TNPCSourceParser);
begin
  FreeAndNil(ASource);
end;

{ TNPCSourceParser }

constructor TNPCSourceParser.Create(const ACompiler: TNPCCompiler; const AParentParser: TNPCSourceParser);
begin
  Compiler := ACompiler;
  ParentParser := AParentParser;
  SettingsPtr := GetSettings;
  ParsingType := SOURCE;
  Tokenizer := Nil;
  Texer := Nil;
  ASTree := Nil;
  FLevel := 0;
  ScopeStack := TObjectList<TNPCScope>.Create(True); // manage and free owned objects
  CurrentScope := Nil;
  ReturnToScope := Nil;
end;

destructor TNPCSourceParser.Destroy;
begin
  Clear;
  // clear scopes
  ScopeStack.Free; // free objects, if any left
  inherited;
end;

procedure TNPCSourceParser.NotImplemented;
begin
  Assert(False, 'not implemented');
end;

procedure TNPCSourceParser.AddProjectImport(const ASourceFile, ACodeName: String);
var
  idx: Integer;
begin
  idx := Length(TNPCProjectSettings(SettingsPtr^).Imports);
  SetLength(TNPCProjectSettings(SettingsPtr^).Imports, idx + 1);
  //
// Imports = record
//  InputPath: String;
//  CodeName: String;
//  Imports: Array of TNPCImportInfo;
  TNPCProjectSettings(SettingsPtr^).Imports[idx].InputPath := ASourceFile;
  TNPCProjectSettings(SettingsPtr^).Imports[idx].CodeName := ACodeName;
  SetLength(TNPCProjectSettings(SettingsPtr^).Imports[idx].Imports, 0);
end;

function TNPCSourceParser.TypeToString(const Sym: TNPCSymbol): String;
begin
  if Sym = Nil then
    Exit('<unresolved>');

//  if Sym.TypeRef <> Nil then
//    Result := Sym.TypeRef.Name
//  else
    Result := Sym.Name; // fallback
end;

function TNPCSourceParser.TypeToName(const Typ: TNPC_ASTExpression): String;
begin
  if Typ = Nil then
    Exit('<not-set>');

//  if Typ is TNPC_ASTExpressionLiteral then
//    Result := TNPC_ASTExpressionLiteral(Typ).Value
//  else
  if Typ is TNPC_ASTExpressionEnumConst then
    Result := TNPC_ASTExpressionEnumConst(Typ).Name
//  else if Typ is TNPC_ASTExpressionSetLiteral then
//    Result := TNPC_ASTExpressionSetLiteral(Typ).Name
  else if Typ is TNPC_ASTExpressionVariable then
    Result := TNPC_ASTExpressionVariable(Typ).Name
  else if Typ is TNPC_ASTExpressionIdent then
    Result := TNPC_ASTExpressionIdent(Typ).Name
//  else if Typ is TNPC_ASTExpressionNumber then
//    Result := TNPC_ASTExpressionNumber(Typ).Value
//  else if Typ is TNPC_ASTExpressionString then
//    Result := TNPC_ASTExpressionString(Typ).Value
  else if Typ is TNPC_ASTExpressionMember then
    Result := TNPC_ASTExpressionMember(Typ).Name
  else
    Result := '<unknown>';
end;

function TNPCSourceParser.Unescape(const Value: String): String;
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

function TNPCSourceParser.GetPrecedence(const AToken: TNPCToken): Integer;
begin
  if TokenIsReservedSymbol(AToken, rs_Assign) then // ':=' // assignment handled as statement-level
    Exit(0)
  else if TokenIsReservedIdent(AToken, ri_or) then // 'or'
    Exit(1)
  else if TokenIsReservedIdent(AToken, ri_xor) then // 'xor'
    Exit(2)
  else if TokenIsReservedIdent(AToken, ri_and) then // 'and'
    Exit(3)
  else if TokenIsReservedSymbol(AToken, [rs_Exclamation, // '!'
                                         rs_Equal,       // '='
                                         rs_LessThan,    // '<'
                                         rs_LessEqual,   // '<='
                                         rs_GreaterThan, // '>'
                                         rs_GreaterEqual // '>='
                                        ]) then
    Exit(4)
  else if TokenIsReservedSymbol(AToken, [rs_Plus, rs_Minus]) then // '+', '-'
    Exit(5)
  else if TokenIsReservedSymbol(AToken, [rs_Asterisk, // '*'
                                         rs_Div,      // '/'
                                         rs_Percent,  // '%'
                                         rs_ModEqual  // '%='
                                        ]) then
    Exit(6)
  else if TokenIsReservedIdent(AToken, ri_mod) then // 'mod'
    Exit(6)
  else if TokenIsReservedIdent(AToken, [ri_shl, ri_shr]) then // 'shl', 'shr'
    Exit(6)
  else if TokenIsReservedSymbol(AToken, [rs_OParen]) then // '(' // call
    Exit(7)
  else if TokenIsReservedSymbol(AToken, [rs_Dot, rs_OBracket]) then // '.', '[' // member access very high / indexing high
    Exit(9)
  else
    Result := 0;
end;

procedure TNPCSourceParser.PushScope(const AParentScope: TNPCScope; const AScopeName: UTF8String);
begin
  CurrentScope := TNPCScope.Create(AParentScope, AScopeName);
  ScopeStack.Add(CurrentScope);
  if AParentScope = Nil then
    InitBuiltins(CurrentScope);
end;

procedure TNPCSourceParser.PopScope;
begin
  if CurrentScope.ParentScope <> Nil then
    CurrentScope := CurrentScope.ParentScope;
end;

procedure TNPCSourceParser.EnterScope(const AScope: TNPCScope);
begin
  if AScope <> Nil then begin
    ReturnToScope := CurrentScope;
    CurrentScope := AScope;
  end;
end;

//procedure TNPCSourceParser.LeaveScope;
//var
//  s: TNPCScope;
//begin
//  if ScopeStack.Count = 0 then
//    Exit;
//  //
//  s := ScopeStack.Last;
//  ScopeStack.Delete(ScopeStack.Count - 1);
//  s.Free; // do not free the scope here, free it in the symbol where it may be used
//  //
//  if ScopeStack.Count > 0 then
//    CurrentScope := ScopeStack.Last
//  else
//    CurrentScope := Nil;
//end;

procedure TNPCSourceParser.LeaveScope;
begin
//  if CurrentScope.ParentScope <> Nil then
//    CurrentScope := CurrentScope.ParentScope;

  if ReturnToScope <> Nil then
    CurrentScope := ReturnToScope;
  ReturnToScope := Nil;
end;

procedure TNPCSourceParser.InitBuiltins(const AScope: TNPCScope);
var
  loc: TNPCLocation;
begin
  if Builtin_Type_Boolean <> Nil then
    Exit;
  //
  loc := TNPCLocation.Create(Self.UnitName, 638, 0);
  // register built-in types
  Builtin_Type_Boolean := TNPC_ASTTypeDefinition.Create(loc, 'Boolean', 1);
  AScope.DefineBuiltinType(loc, 'Boolean', TYPE_Literal, 1, Builtin_Type_Boolean);
  AScope.DefineConst(loc, 'True', TYPE_Literal, 1, Builtin_Type_Boolean, Nil, 1);
  AScope.DefineConst(loc, 'False', TYPE_Literal, 1, Builtin_Type_Boolean, Nil, 0);
  AScope.DefineConst(loc, 'Error', TYPE_Literal, 1, Builtin_Type_Boolean, Nil, -1);
  //
  Builtin_Type_Byte := TNPC_ASTTypeDefinition.Create(loc, 'Byte', 1);
  AScope.DefineBuiltinType(loc, 'Byte', TYPE_Literal, 1, Builtin_Type_Byte);
  //
  Builtin_Type_Integer := TNPC_ASTTypeDefinition.Create(loc, 'Integer', 4);
  AScope.DefineBuiltinType(loc, 'Integer', TYPE_Literal, 4, Builtin_Type_Integer);
  //
  Builtin_Type_Integer64 := TNPC_ASTTypeDefinition.Create(loc, 'Integer64', 8);
  AScope.DefineBuiltinType(loc, 'Integer64', TYPE_Literal, 8, Builtin_Type_Integer64);
  //
  Builtin_Type_Single := TNPC_ASTTypeDefinition.Create(loc, 'Single', 4);
  AScope.DefineBuiltinType(loc, 'Single', TYPE_Literal, 4, Builtin_Type_Single);
  //
  Builtin_Type_Double := TNPC_ASTTypeDefinition.Create(loc, 'Double', 8);
  AScope.DefineBuiltinType(loc, 'Double', TYPE_Literal, 8, Builtin_Type_Double);
  AScope.DefineBuiltinTypeAlias(loc, 'Real', TYPE_Literal, 8, Builtin_Type_Double); // alias
  //
  Builtin_Type_Extended := TNPC_ASTTypeDefinition.Create(loc, 'Extended', 10);
  AScope.DefineBuiltinType(loc, 'Extended', TYPE_Literal, 10, Builtin_Type_Extended);

  // String type definition:
  //                | offset | field: type: size
  //                |    -32 | Reserved: Byte (10)
  //                |    -22 | CodePage: Word (2)
  //                |    -20 | RefCount: Integer (4)
  //                |    -16 | CharLength: UInt64 (8)
  //                |     -8 | ByteCount: UInt64 (8)
  // entry point -> |      0 | Data: Byte[ByteCount]
  Builtin_Type_String := TNPC_ASTTypeDefinition.Create(loc, 'String', 32);
  AScope.DefineBuiltinType(loc, 'String', TYPE_Literal, 32, Builtin_Type_String);
  //
  Builtin_Type_Pointer := TNPC_ASTTypeDefinition.Create(loc, 'Pointer', -1);
  AScope.DefineBuiltinType(loc, 'Pointer', TYPE_Pointer, -1, Builtin_Type_Pointer);
  //
  Builtin_Type_Null := TNPC_ASTTypeDefinition.Create(loc, 'Null', -1);
  AScope.DefineConst(loc, 'Null', TYPE_Literal, -1, Builtin_Type_Null, Nil, 0);
  AScope.DefineBuiltinTypeAlias(loc, 'null', TYPE_Literal, -1, Builtin_Type_Null); // alias
  //
  Builtin_Type_ClassSymbol := TNPC_ASTTypeDefinition.Create(loc, 'Symbol', 32);
  AScope.DefineBuiltinType(loc, 'Symbol', TYPE_Procedure, 32, Builtin_Type_ClassSymbol);
  //
  loc.Free;
end;

//var
//  intSym, boolSym, strSym: TNPCSymbol;
//  globalScope: TScope; // adjust to your scope variable name
//begin
//  // assume you have a top-level scope created already (FScopeStack or similar)
//  // create the type symbols
//  intSym := TNPCSymbol.Create('Integer', skType, nil);
//  boolSym := TNPCSymbol.Create('Boolean', skType, nil);
//  strSym := TNPCSymbol.Create('String', skType, nil);
//
//  // register them into the global scope (so lookup("Integer") works)
//  globalScope := FScopeStack.Last; // or however you get the global scope
//  globalScope.AddOrSetValue('Integer', intSym);
//  globalScope.AddOrSetValue('Boolean', boolSym);
//  globalScope.AddOrSetValue('String', strSym);
//end;

procedure TNPCSourceParser.Clear;
var
  i: Integer;
begin
  if Assigned(Tokenizer) then
    Tokenizer.Clear;
  //
  for i:=0 to High(Imports) do begin
    Imports[i].Name := '';
    Imports[i].Path := '';
  end;
  SetLength(Imports, 0);
  //
//  for i:=0 to High(TokensArray) do
//    FreeAndNil(TokensArray[i]);
//  SetLength(TokensArray, 0);
  FLevel := 0;
  if ASTree <> Nil then
    FreeAndNil(ASTree);
  // clear scopes
//  while ScopeStack.Count > 0 do
//    LeaveScope;
  ScopeStack.Clear; // free objects
end;

procedure TNPCSourceParser.AddImport(const AImportType: TNPCImportType; const AImportName, AImportPath: String);
var
  idx: Integer;
begin
  idx := Length(Imports);
  SetLength(Imports, idx + 1);
  Imports[idx].&Type := AImportType;
  Imports[idx].Name := AImportName;
  Imports[idx].Path := AImportPath;
  Imports[idx].Resolved := False;
end;

function TNPCSourceParser.TokenIsOfType(const AToken: TNPCToken; const ATokenTypes: Array of TNPCTokenType): Boolean;
var
  i: Integer;
begin
  Result := False;
  //
  for i:=0 to Length(ATokenTypes) - 1 do begin
    Result := AToken.&Type = ATokenTypes[i];
    if Result then
      Break;
  end;
end;

function TNPCSourceParser.TokenIsIdent(const AToken: TNPCToken): Boolean;
begin
  Result := (AToken.&Type = tokIdent) and not AToken.ReservedWord;
end;

function TNPCSourceParser.TokenIsNumber(const AToken: TNPCToken): Boolean;
begin
  Result := (AToken.&Type = tokNumber);
end;

function TNPCSourceParser.TokenIsString(const AToken: TNPCToken): Boolean;
begin
  Result := (AToken.&Type = tokString);
end;

function TNPCSourceParser.TokenIsChar(const AToken: TNPCToken): Boolean;
begin
  Result := (AToken.&Type = tokChar);
end;

function TNPCSourceParser.TokenIsReserved(const AToken: TNPCToken): Boolean;
begin
  Result := AToken.ReservedWord or AToken.ReservedSymbol;
end;

function TNPCSourceParser.TokenIsReservedIdent(const AToken: TNPCToken): Boolean;
begin
  Result := (AToken.&Type = tokIdent) and AToken.ReservedWord;
end;

function TNPCSourceParser.TokenIsReservedIdent(const AToken: TNPCToken; const AReservedIdent: TNPCReservedIdents): Boolean;
begin
  Result := TokenIsReservedIdent(AToken) and SameText(AToken.Value, NPCReservedIdentifiers[AReservedIdent].Ident);
end;

function TNPCSourceParser.TokenIsReservedIdent(const AToken: TNPCToken; const AReservedIdents: Array of TNPCReservedIdents): Boolean;
var
  i: Integer;
begin
  Result := False;
  if not TokenIsReservedIdent(AToken) then
    Exit;
  //
  for i:=0 to Length(AReservedIdents) - 1 do begin
    Result := SameText(AToken.Value, NPCReservedIdentifiers[AReservedIdents[i]].Ident);
    if Result then
      Break;
  end;
end;

function TNPCSourceParser.TokenIsReservedIdent(const AToken: TNPCToken; const AReservedIdents: Array of String): Boolean;
var
  i: Integer;
begin
  Result := False;
  //
  for i:=0 to Length(AReservedIdents) - 1 do begin
    Result := SameText(AToken.Value, AReservedIdents[i]);
    if Result then
      Break;
  end;
end;

function TNPCSourceParser.TokenIsLiteral(const AToken: TNPCToken): Boolean;
begin
  Result := (AToken.&Type in [tokOParen..tokDiv]) and AToken.ReservedSymbol;
end;

function TNPCSourceParser.TokenIsBiLiteral(const AToken: TNPCToken): Boolean;
begin
  Result := (AToken.&Type in [tokCommentSL..tokCommentMLE2, tokDoubleDot..tokModEqual]) and AToken.ReservedSymbol;
end;

function TNPCSourceParser.TokenIsReservedSymbol(const AToken: TNPCToken): Boolean;
begin
  Result := (AToken.&Type = tokIdent) and AToken.ReservedSymbol;
end;

function TNPCSourceParser.TokenIsReservedSymbol(const AToken: TNPCToken; const AReservedSymbol: TNPCReservedSymbols): Boolean;
var
  ReservedSymbol: String;
  ReservedSymbolLength: Byte;
  TempValue: String;
  TempValueLength: Byte;
begin
  ReservedSymbol := NPCReservedSymbolToString(AReservedSymbol);
  ReservedSymbolLength := Length(ReservedSymbol);
  //
  Result := TokenIsLiteral(AToken) and (AToken.Value = ReservedSymbol);
  if not Result then begin
    TempValueLength := Length(AToken.Value);
    if TempValueLength > 2 then
      TempValueLength := 2;
    TempValue := Copy(AToken.Value, 1, Max(ReservedSymbolLength, TempValueLength));
    Result := TokenIsBiLiteral(AToken) and (TempValue = ReservedSymbol);
    TempValue := '';
  end;
  ReservedSymbol := '';
end;

function TNPCSourceParser.TokenIsReservedSymbol(const AToken: TNPCToken; const AReservedSymbols: Array of TNPCReservedSymbols): Boolean;
var
  i: Integer;
begin
  Result := False;
  //
  for i:=0 to Length(AReservedSymbols) - 1 do begin
    Result := (AToken.&Type in [tokOParen..tokDiv, tokDoubleDot..tokModEqual]) and AToken.ReservedSymbol and (AToken.Value = NPCReservedSymbolToString(AReservedSymbols[i]));
    if Result then
      Break;
  end;
end;

procedure TNPCSourceParser.SkipComments;
var
  token: TNPCToken;
begin
  while Texer.IsNotEmpty do begin
    token := Texer.PeekToken;
    // skip comment
    if SkipComment(token, True) then
      Continue
    else
      Break;
  end;
end;

function TNPCSourceParser.SkipComment(const AToken: TNPCToken; const AConsumeToken: Boolean = False): Boolean;
var
  token: TNPCToken;
begin
  Result := False;
  if TokenIsReservedSymbol(Atoken, rs_CommentSL) then begin // single-line comment
    if AConsumeToken then
      Texer.SkipToken;
    Exit(True);
  end;
  if TokenIsReservedSymbol(Atoken, rs_CommentMLB) then begin // multi-line comment /. ... ./
    if AConsumeToken then
      Texer.SkipToken;
    while Texer.IsNotEmpty do begin
      token := Texer.PeekToken;
      // @TODO: count opened comments
      Texer.SkipToken;
      if TokenIsReservedSymbol(token, rs_CommentMLE) then // skip comments until closing section
        Break;
    end;
    Exit(True);
  end;
  if TokenIsReservedSymbol(Atoken, rs_CommentMLB1) then begin // multi-line comment {. ... .}
    if AConsumeToken then
      Texer.SkipToken;
    while Texer.IsNotEmpty do begin
      token := Texer.PeekToken;
      // @TODO: count opened comments
      Texer.SkipToken;
      if TokenIsReservedSymbol(token, rs_CommentMLE1) then // skip comments until closing section
        Break;
    end;
    Exit(True);
  end;
  if TokenIsReservedSymbol(Atoken, rs_CommentMLB2) then begin // multi-line comment (* ... *)
    if AConsumeToken then
      Texer.SkipToken;
    while Texer.IsNotEmpty do begin
      token := Texer.PeekToken;
      // @TODO: count opened comments
      Texer.SkipToken;
      if TokenIsReservedSymbol(token, rs_CommentMLE2) then // skip comments until closing section
        Break;
    end;
    Exit(True);
  end;
end;

function TNPCSourceParser.IsCodeFileNamedAs(const AFileName: String; const ACodeName: String): Boolean;
var
  LTokenizer: TNPCTokenizer;
  LTexer: TNPCTokensParser;
  token: TNPCToken;
begin
  Result := False;
  LTokenizer := TNPCTokenizer.Create(TNPCProjectSettings(SettingsPtr^).ProjectFormatSettings^);
  try
    LTokenizer.TokenizeFile(AFileName, TNPCProjectSettings(SettingsPtr^).ProjectEncoding);
    LTexer := TNPCTokensParser.Create(AFileName, LTokenizer.Tokens);
    try
      token := LTexer.GetToken;
      if (token.&Type = tokEOF) or not TokenIsReservedIdent(token, ri_code) then
        Exit;
      //
      // ok we are inside code file
      //
      token := LTexer.GetToken;
      if (token.&Type = tokEOF) or not (token.&Type = tokString) then
        Exit;

      if not SameText(token.Value, ACodeName) then
        Exit;

      token := LTexer.GetToken;
      if (token.&Type = tokEOF) or not TokenIsReservedSymbol(token, rs_Semicolon) then
        Exit;

      Result := True;
    finally
      LTexer.Free;
    end;
  finally
    LTokenizer.Free;
  end;
end;

function TNPCSourceParser.LookupTypeDefinition(const AToken: TNPCToken): TNPC_ASTTypeDefinition;
begin
  Result := Nil;
end;

function TNPCSourceParser.IsDeclaration(var AToken: TNPCToken): Boolean;
begin

end;

procedure TNPCSourceParser.AddStatement(const AStmt: TNPC_ASTStatement);
begin
  if Assigned(CurrentBlock) then
    CurrentBlock.AddStatement(AStmt)
  else
    raise Exception.Create('No current block to add statement to');
end;

procedure TNPCSourceParser.DeclareSymbol(const AName: String; const ASym: TNPCSymbol);
var
  scope: TNPCScope;
begin
  if ScopeStack.Count = 0 then
    Exit;
  //
  scope := ScopeStack.Last;
  // allow shadowing: put symbol in current scope
  //scope.AddOrSetSymbol(AName, ASym);
  scope.AddSymbol(AName, ASym);
end;

function TNPCSourceParser.LookupSymbol(const AName: String): TNPCSymbol;
var
  i: Integer;
  scope: TNPCScope;
  sym: TNPCSymbol;
begin
  Result := Nil;
  for i := ScopeStack.Count - 1 downto 0 do begin
    scope := ScopeStack[i];
    if scope.TryGetSymbol(AName, sym) then
      Exit(sym);
  end;
end;

function TNPCSourceParser.ResolveType(const ATypeName: String; out AType: TNPC_ASTType): Boolean;
var
  VarSym: TNPCSymbol;
begin
  AType := Nil;
  VarSym := CurrentScope.ResolveSymbol(ATypeName);
  if not Assigned(VarSym) then
    Exit(False);

  AType := VarSym.TypeRef;
  Result := True;
end;

function TNPCSourceParser.ResolveTypeSize(const ATypeRef: TNPC_ASTTypeReference): Integer;
var
  Field: TNPCSymbol;
  Size, Align, FieldSize: Integer;
begin
  case ATypeRef.Kind of
    REF_Named: begin
      if Assigned(ATypeRef.BaseSymbol) then
        Exit(ATypeRef.BaseSymbol.Size)
      else
        Exit(0);
    end;

    REF_Array: begin // dynamic array = pointer
      Result := SizeOf(Pointer);
    end;

    REF_Function: begin // function reference / closure pointer
      Result := SizeOf(Pointer);
    end;

    REF_Record: begin
      Size := 0;
      for Field in ATypeRef.Fields do begin
        FieldSize := ResolveTypeSize(TNPC_ASTTypeReference(Field.TypeRef));

        // ===== OPTIONAL ALIGNMENT =====
        // Simple alignment to field size (power-of-two assumption)
        Align := FieldSize;
        if Align > 8 then
          Align := 8;

        if Align > 0 then
          Size := (Size + Align - 1) and not (Align - 1);

        // assign field offset (VERY useful later)
        Field.Offset := Size;

        Inc(Size, FieldSize);
      end;

      // final struct alignment (optional)
      if Size > 0 then begin
        Align := 8; // or max field align
        Size := (Size + Align - 1) and not (Align - 1);
      end;

      Result := Size;
    end;
  else
    Result := 0;
  end;
end;

function TNPCSourceParser.FindClassMethod(const AClassSym: TNPCSymbol; const AMethodName: String; const AClassMethodDefinitionLocation: TNPCLocation): TNPCSymbol;
var
  TypeDef: TNPC_ASTTypeDefinition;
  ClsType: TNPC_ASTTypeClass;
  Method: TNPC_ASTType; // TNPC_ASTTypeClassMethod;
  Symbol: TNPCSymbol;
  loc: TNPCLocation;
begin
  Result := Nil;

  if not ((AClassSym.Kind = KIND_Type) and (AClassSym.&Type = TYPE_Class)) then
    Exit;

  if not (AClassSym.TypeRef is TNPC_ASTTypeDefinition) then
    Exit;

  TypeDef := TNPC_ASTTypeDefinition(AClassSym.TypeRef);
  if TypeDef.DefinitionType <> DEF_Class then
    Exit;
  ClsType := TypeDef.ClassDescription;

  if ClsType.Methods = Nil then
    Exit;

  EnterScope(AClassSym.Scope);
  try
    if not ClassContainsMethod(ClsType, AMethodName, Method, Symbol) then begin
      Assert(Assigned(AClassMethodDefinitionLocation));
      loc := AClassMethodDefinitionLocation.Copy;
      loc.IncEndCol(Length(AMethodName)); // Method.Location.GetLocationSize
      raise NPCSyntaxError.SemanticError(loc, Format('method "%s" declaration not found in class "%s"', [AMethodName, ClsType.Name]));
    end;
    Assert(Assigned(Symbol)); // symbol must exist
    Result := Symbol;
  finally
    LeaveScope;
  end;
end;

function TNPCSourceParser.ClassContainsMethod(const AClassType: TNPC_ASTTypeClass; const AMethodName: UTF8String; out AMethod: TNPC_ASTType; out AMethodSymbol: TNPCSymbol): Boolean;
var
  Method: TNPC_ASTTypeClassMethod;
begin
  Result := False;
  AMethod := Nil;
  AMethodSymbol := Nil;
  if AClassType = Nil then
    Exit;
  //
  for Method in AClassType.Methods do begin
    if SameText(Method.Name, AMethodName) then begin
      AMethod := Method;
      AMethodSymbol := CurrentScope.ResolveSymbol(AMethodName);
      Exit(True);
    end;
  end;
end;

function TNPCSourceParser.ClassContainsProperty(const AClassType: TNPC_ASTTypeClass; const APropertyName: UTF8String; out AProperty: TNPC_ASTType; out APropertySymbol: TNPCSymbol): Boolean;
var
  Prop: TNPC_ASTTypeClassProperty;
begin
  Result := False;
  AProperty := Nil;
  APropertySymbol := Nil;
  if AClassType = Nil then
    Exit;
  //
  for Prop in AClassType.Properties do begin
    if SameText(Prop.Name, APropertyName) then begin
      AProperty := Prop;
      APropertySymbol := CurrentScope.ResolveSymbol(APropertyName);
      Exit(True);
    end;
  end;
end;

procedure TNPCSourceParser.RegisterProcedureSymbols(const AProc: TNPC_ASTStatementProcedure);
var
  Param: TNPC_ASTParameter;
  Sym: TNPCSymbol;
begin
  for Param in AProc.Parameters do begin
    Assert(Param.DeclaredType is TNPC_ASTTypeReference);
    Sym := TNPCSymbol.Create(Param, KIND_Param, CurrentScope.ResolveTypeKind(TNPC_ASTTypeReference(Param.DeclaredType)), CurrentBlock);
    Sym.Size := ResolveTypeSize(TNPC_ASTTypeReference(Param.DeclaredType));

    CurrentScope.DefineSymbol(Sym);
  end;

  // same for return values
  if AProc.IsFunction and (AProc.Returns <> Nil) then begin
    for Param in AProc.Returns do begin
      Assert(Param.DeclaredType is TNPC_ASTTypeReference);
      Sym := TNPCSymbol.Create(Param, KIND_Return, CurrentScope.ResolveTypeKind(TNPC_ASTTypeReference(Param.DeclaredType)), CurrentBlock);
      Sym.Size := ResolveTypeSize(TNPC_ASTTypeReference(Param.DeclaredType));

      CurrentScope.DefineSymbol(Sym);
    end;
  end;
end;

//Compare:
//  - parameter count
//  - parameter types
//  - calling convention (optional)
//  - return type (for functions)
function TNPCSourceParser.CompareProcedureSignatures(Decl, Impl: TNPC_ASTStatementProcedure): Boolean;
var
  i: Integer;
begin
  Result := False;

  // PARAM COUNT
  if Decl.Parameters.Count <> Impl.Parameters.Count then
    Exit;

  // PARAM TYPES
  for i := 0 to Decl.Parameters.Count - 1 do begin
    Assert(Decl.Parameters[i].DeclaredType is TNPC_ASTTypeReference);
    Assert(Impl.Parameters[i].DeclaredType is TNPC_ASTTypeReference);
    if not CompareTypeRef(TNPC_ASTTypeReference(Decl.Parameters[i].DeclaredType), TNPC_ASTTypeReference(Impl.Parameters[i].DeclaredType)) then
      Exit;

    if Decl.Parameters[i].Modifier <> Impl.Parameters[i].Modifier then
      Exit;
  end;

  // RETURN COUNT
  if Decl.Returns.Count <> Impl.Returns.Count then
    Exit;

  // RETURN TYPES
  for i := 0 to Decl.Returns.Count - 1 do begin
    Assert(Decl.Returns[i].DeclaredType is TNPC_ASTTypeReference);
    Assert(Impl.Returns[i].DeclaredType is TNPC_ASTTypeReference);
    if not CompareTypeRef(TNPC_ASTTypeReference(Decl.Returns[i].DeclaredType), TNPC_ASTTypeReference(Impl.Returns[i].DeclaredType)) then
      Exit;
  end;

  Result := True;
end;

function TNPCSourceParser.CompareTypeRef(A, B: TNPC_ASTTypeReference): Boolean;
begin
  if A.Kind <> B.Kind then
    Exit(False);

  case A.Kind of
    REF_Named   : Result := A.BaseSymbol = B.BaseSymbol;

    REF_Array   : Result := CompareTypeRef(A.ElementType, B.ElementType);

    REF_Record  : Result := CompareRecordTypes(A, B);

    REF_Function: Result := CompareFunctionTypes(A, B);
  else
    Result := False;
  end;
end;

function TNPCSourceParser.CompareRecordTypes(A, B: TNPC_ASTTypeReference): Boolean;
var
  i: Integer;
  FieldA, FieldB: TNPCSymbol;
begin
  Result := False;

  if (A = Nil) or (B = Nil) then
    Exit;

  if (A.Kind <> REF_Record) or (B.Kind <> REF_Record) then
    Exit;

  // FIELD COUNT
  if A.Fields.Count <> B.Fields.Count then
    Exit;

  // FIELD COMPARISON
  for i := 0 to A.Fields.Count - 1 do begin
    FieldA := A.Fields[i];
    FieldB := B.Fields[i];

    // compare names (STRICT)
    if not SameText(FieldA.Name, FieldB.Name) then
      Exit;

    // compare types (RECURSIVE)
    Assert(FieldA.TypeRef is TNPC_ASTTypeReference);
    Assert(FieldB.TypeRef is TNPC_ASTTypeReference);
    if not CompareTypeRef(TNPC_ASTTypeReference(FieldA.TypeRef), TNPC_ASTTypeReference(FieldB.TypeRef)) then
      Exit;
  end;

  Result := True;
end;

function TNPCSourceParser.CompareFunctionTypes(A, B: TNPC_ASTTypeReference): Boolean;
var
  i: Integer;
  ParamA, ParamB: TNPCSymbol;
begin
  Result := False;

  if (A = Nil) or (B = Nil) then
    Exit;

  if (A.Kind <> REF_Function) or (B.Kind <> REF_Function) then
    Exit;

  // PROCEDURE vs FUNCTION
  if A.IsProcedure <> B.IsProcedure then
    Exit;

  // PARAM COUNT
  if A.Params.Count <> B.Params.Count then
    Exit;

  // PARAM TYPES + MODIFIERS
  for i := 0 to A.Params.Count - 1 do begin
    ParamA := A.Params[i];
    ParamB := B.Params[i];

    // modifier must match
//    if ParamA.ParamModifier <> ParamB.ParamModifier then
//      Exit;
    if ParamA.IsConst <> ParamB.IsConst then
      Exit;

    // type must match
    Assert(ParamA.TypeRef is TNPC_ASTTypeReference);
    Assert(ParamB.TypeRef is TNPC_ASTTypeReference);
    if not CompareTypeRef(TNPC_ASTTypeReference(ParamA.TypeRef), TNPC_ASTTypeReference(ParamB.TypeRef)) then
      Exit;
  end;

  // RETURN
  if ((A.ReturnType <> Nil) and (B.ReturnType = Nil)) or ((A.ReturnType = Nil) and (B.ReturnType <> Nil)) then
    Exit;

  if (A.ReturnType <> Nil) and (B.ReturnType <> Nil) then begin
    if not CompareTypeRef(A.ReturnType, B.ReturnType) then
      Exit;
  end;

  Result := True;
end;

procedure TNPCSourceParser.BindTupleToVariables(const Targets: TObjectList<TNPCSymbol>; Tuple: TNPC_ASTExpressionTuple; Scope: TNPCScope);
var
  i: Integer;
  Expr: TNPC_ASTExpression;
  Val: Variant; //TNPC_ASTValue;
begin
  if Tuple.Items.Count <> Targets.Count then
    raise NPCSemanticError.SemanticError(Tuple.Location, 'assignment count mismatch');

  for i := 0 to Targets.Count - 1 do begin
    Expr := Tuple.Items[i].Expr;

    // type check BEFORE evaluation
    ValidateTypeCompatibility(Targets[i].TypeRef, Expr);

    // evaluate
    if Expr is TNPC_ASTExpressionNull then
      Val := Scope.ResolveNullValue(Targets[i].TypeRef)
    else
      Val := Expr.Eval(Scope);

    // assign
    Scope.SetSymbolValue(Targets[i], Val);
  end;
end;

procedure TNPCSourceParser.ValidateTypeCompatibility(Expected: TNPC_ASTType; Expr: TNPC_ASTExpression);
var
  Actual: TNPC_ASTTypeReference;
begin
  if not (Expected is TNPC_ASTTypeReference) then
    raise NPCSemanticError.SemanticError(Expected.Location, '?????');

//  Actual := Expr.TypeRef; // assume you assign this during semantic phase
//  Actual := Expr.InferredType; // assume you assign this during semantic phase

  // null is special (handled later)
  if Expr is TNPC_ASTExpressionNull then
    Exit;

  if not CompareTypeRef(TNPC_ASTTypeReference(Expected), Actual) then
    raise NPCSemanticError.SemanticError(Expr.Location, Format('type mismatch, expected "%s", got "%s"', [Expected.ToString, Actual.ToString]));
end;

procedure TNPCSourceParser.ValidateTupleTypes(Proc: TNPC_ASTStatementProcedure; Tuple: TNPC_ASTExpressionTuple);
var
  Values: Array of TNPC_ASTExpression;
  i: Integer;
begin
  BindTupleToReturns(Tuple, Proc, Values);

  for i := 0 to High(Values) do
    ValidateTypeCompatibility(Proc.Returns[i].DeclaredType, Values[i]);
end;

procedure TNPCSourceParser.AssignFieldValue(FieldSym: TNPCSymbol; const Value: TNPC_ASTValue);
//var
//  Instance: Pointer;
begin
//  // depends on your runtime model
//  Instance := FieldSym.OwnerInstance; // you must store this

//  if Instance = Nil then
//    raise Exception.Create('field has no instance');

//  // write into memory using offset
//  Move(Value, PByte(Instance)[FieldSym.Offset], FieldSym.Size);
  FieldSym.Value := Value;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

function TNPCSourceParser.Parse(const ASource: TStringStream; const ASourceFile: String; const ParsingImport: Boolean = False): Boolean;
var
  token: TNPCToken;
//  idx: Integer;
begin
  Result := False;

  PushScope(CurrentScope, 'global scope'); // global scope init

  Tokenizer := TNPCTokenizer.Create(TNPCProjectSettings(SettingsPtr^).ProjectFormatSettings^);
  try
    if ASource <> Nil then
      Tokenizer.TokenizeFile(ASourceFile, ASource, TNPCProjectSettings(SettingsPtr^).ProjectEncoding)
    else
      Tokenizer.TokenizeFile(TNPCProjectSettings(SettingsPtr^).InputPath, TNPCProjectSettings(SettingsPtr^).ProjectEncoding);
    if TNPCProjectSettings(SettingsPtr^).OutputTokens in [otProjectOnly, otProjectAndSources] then
      Tokenizer.OutputTokens;
    //
    if Assigned(Texer) then
      FreeAndNil(Texer);
    if ASource <> Nil then
      Texer := TNPCTokensParser.Create(ASourceFile, Tokenizer.Tokens)
    else
      Texer := TNPCTokensParser.Create(TNPCProjectSettings(SettingsPtr^).InputPath, Tokenizer.Tokens);
    try
      if ParsingType = PROJECT then begin
        SkipComments;
        token := Texer.ExpectToken(tokIdent, 'expected "project" keyword');
        if not TokenIsReservedIdent(token, ri_project) then begin
          token.Free;
          raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedType, [token.TokenToString, NPCReservedIdentifiers[ri_project].Ident]));
        end;
        //
        // ok we are inside project file
        //
//        AddToken(token);
        ASTree := TNPC_ASTStatementBlock.Create(token.Location, Nil, [BLOCK_ConsistsOfOrderedStatements, BLOCK_DoesNotHaveResult]);
        CurrentBlock := TNPC_ASTStatementBlock(ASTree);

        token := Texer.ExpectToken([tokIdent, tokString]);
        // handle project name of idents with dots, like: test1.project.nitropascal
        TNPCProjectSettings(SettingsPtr^).ProjectName := token.Value;
        ConsoleWriteln('Compiling project____: ' + token.Value);
//        AddToken(token);

//        AddToken(Texer.ExpectToken([tokSemicolon]));
        Texer.ExpectToken([tokSemicolon]);
        //
        // we have collected basic info about project, its name
        // go collect the rest of the project body
        //
        ParseProjectBody(TNPC_AST(ASTree));
        Result := True;
      end
      else begin // SOURCE
        token := Texer.ExpectToken([tokIdent]);
        if not TokenIsReservedIdent(token, ri_code) then begin
          token.Free;
          raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedType, [token.TokenToString, NPCReservedIdentifiers[ri_code].Ident]));
        end;
        //
        // ok we are inside code file
        //
//        AddToken(token);
        ASTree := TNPC_ASTStatementBlock.Create(token.Location, Nil, [BLOCK_ConsistsOfOrderedStatements, BLOCK_DoesNotHaveResult]);
        CurrentBlock := TNPC_ASTStatementBlock(ASTree);

        token := Texer.ExpectToken([tokIdent, tokString]);
        if ParsingImport then begin
          AddProjectImport(ASourceFile, token.Value);
        end;

//
//        idx := Length(Imports);
//        SetLength(Imports, idx + 1);
//        Imports[idx].&Type := itCode;
//        Imports[idx].Name := ExtractFileName(ASourceFile);
//        Imports[idx].Path := ExtractFilePath(ASourceFile);
//        Imports[idx].Resolved := False;

        ConsoleWriteln('Compiling source_____: ' + token.Value);

        Texer.ExpectToken([tokSemicolon]);
        //
        // we have collected basic info about code file, its name
        // go collect the rest of the code file body
        //
        ParseCodeBody(TNPC_AST(ASTree));
        Result := True;
      end;
    finally
      FreeAndNil(Texer);
      if TNPCProjectSettings(SettingsPtr^).OutputAST in [otProjectOnly, otSourcesOnly, otProjectAndSources] then begin
        if ASource <> Nil then
          OutputAST(ASourceFile)
        else
          OutputAST(TNPCProjectSettings(SettingsPtr^).InputPath);
      end;
    end;
  finally
    FreeAndNil(Tokenizer);
  end;
end;

procedure TNPCSourceParser.ParseProjectBody(const ABlock: TNPC_AST);
var
  token, next_token: TNPCToken;
  ClassSym: TNPCSymbol;
  ClassMethod: TNPCToken;
  ClassMethodDefinitionLocation, loc: TNPCLocation;
begin
  while Texer.IsNotEmpty do begin
    SkipComments;
    token := Texer.GetToken; // add relevant tokens
    if TokenIsReservedSymbol(token, rs_OCurly) then begin
      if Texer.IsCurrentSymbol('$') then
        ParseSettingDefineCondition(ABlock)
      else if Texer.IsCurrentSymbol('@') then
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, '', sProjectFile]))
      else
        ParseComment;
    end
    else if TokenIsIdent(token) then begin // probably function declaration, function calls are possible only in procedures/functions bodys
      next_token := Texer.PeekToken;
      if TokenIsReservedSymbol(next_token, rs_Dot) then begin
        if not ParseQualifiedName(token, ClassSym, ClassMethod, ClassMethodDefinitionLocation) then
          raise NPCSyntaxError.ParserError(token.Location, 'invalid qualified method');
        loc := ClassMethodDefinitionLocation.Copy;
        try
          loc.IncEndCol(next_token.Location.GetLocationSize);
          AddStatement(ParseClassMethodDefinition(ClassSym, ClassMethod, loc));
        finally
          loc.Free;
        end;
      end
      else if TokenIsReservedSymbol(next_token, rs_OParen) then // '('
        AddStatement(ParseProcedureDefinition(token, Nil, []));
    end
    else if TokenIsReservedIdent(token, ri_imports) then begin
      ParseImports(ABlock);
    end
    else if TokenIsReservedIdent(token, ri_exports) then begin
      ParseExports(token);
    end
    else if TokenIsReservedIdent(token, ri_type) then begin
      ParseTypeDeclaration(token, []);
    end
    else if TokenIsReservedIdent(token, ri_const) then begin
      ParseConsts(token);
    end
    else if TokenIsReservedIdent(token, ri_var) then begin
      //ParseVariables(token);
      ParseVariableDeclaration(token);
    end
    else if TokenIsReservedIdent(token, ri_initialization) then begin
      ParseInitialization(token);
    end
    else if TokenIsReservedIdent(token, ri_finalization) then begin
      ParseFinalization(token);
    end
    else if TokenIsReservedIdent(token, ri_begin) then begin
      ParseBegin(token);
    end
    else if TokenIsReservedIdent(token, ri_end) then begin
      ParseEnd(token);
      Break;
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, '', sProjectFile]));
  end;
end;

procedure TNPCSourceParser.ParseCodeBody(const ABlock: TNPC_AST);
var
  token: TNPCToken;
begin
  while Texer.IsNotEmpty do begin
    SkipComments;
    token := Texer.GetToken; // add relevant tokens
    if TokenIsReservedSymbol(token, rs_OCurly) then begin
      if Texer.IsCurrentSymbol('$') then
        ParseSettingDefineCondition(ABlock)
      else if Texer.IsCurrentSymbol('@') then
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, '', sCodeFile]))
      else
        ParseComment;
    end
    else if TokenIsReservedIdent(token, ri_imports) then begin
      ParseImports(ABlock);
    end
    else if TokenIsReservedIdent(token, ri_exports) then begin
      ParseExports(token);
    end
    else if TokenIsReservedIdent(token, ri_type) then begin
      ParseTypes(token);
    end
    else if TokenIsReservedIdent(token, ri_const) then begin
      ParseConsts(token);
    end
    else if TokenIsReservedIdent(token, ri_var) then begin
      ParseVariables(token);
    end
    else if TokenIsReservedIdent(token, ri_initialization) then begin
      ParseInitialization(token);
    end
    else if TokenIsReservedIdent(token, ri_finalization) then begin
      ParseFinalization(token);
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, '', sCodeFile]));
  end;
end;

procedure TNPCSourceParser.ParseSettingDefineCondition(const ABlock: TNPC_AST);
var
  token: TNPCToken;
begin
  while Texer.IsNotEmpty do begin
    token := Texer.PeekToken;
    if TokenIsReservedSymbol(token, rs_Dollar) then begin
      Texer.SkipToken;
      ParseSettingOrDefineDirective(ABlock);
    end
    else if TokenIsReservedSymbol(token, rs_CCurly) then begin
      Texer.SkipToken;
      Break;
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, '', sProjectFile]));
  end;
end;

procedure TNPCSourceParser.ParseSettingOrDefineDirective(const ABlock: TNPC_AST);
var
  token: TNPCToken;
begin
  while Texer.IsNotEmpty do begin
    token := Texer.GetTokenWithMinus;
    try
      if (token.&Type = tokIdent) and SameText(token.Value, 'program-type') then begin
        ParseSettings(token, setProgramType, ABlock);
        Break;
      end
      else if (token.&Type = tokIdent) and SameText(token.Value, 'search-path') then begin
        ParseSettings(token, setSearchPath, ABlock);
        Break;
      end
      else if (token.&Type = tokIdent) and SameText(token.Value, 'defines') then begin
        ParseSettings(token, setSearchPath, ABlock);
        Break;
      end
      else
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnknownDirectiveNameIn, [token.Value, '', sProjectFile]));
    finally
      token.Free;
    end;
  end;
end;

procedure TNPCSourceParser.ParseSettings(const AToken: TNPCToken; const ASettingType: TNPCSettingType; const ABlock: TNPC_AST);
begin
  case ASettingType of
    setProgram    : ParseSettingProgram(AToken, ABlock);
    setProgramType: ParseSettingProgramType(AToken, ABlock);
    setSearchPath : ParseSearchPath(AToken, ABlock);
    setDefines    : ParseDefines(AToken, ABlock);
    setResources  : ;
  end;
end;

procedure TNPCSourceParser.ParseSettingProgram(const AToken: TNPCToken; const ABlock: TNPC_AST);
var
  token: TNPCToken;
begin
  token := Texer.GetToken;
  try
//    if (token.&Type = tokIdent) and SameText(token.Value, 'type') then begin
//      ParseSettingProgramType(AToken);
//    end
//    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, '', sProjectFile]));
  finally
    token.Free;
  end;
end;

procedure TNPCSourceParser.ParseSettingProgramType(const AToken: TNPCToken; const ABlock: TNPC_AST);

  procedure AddCompilationType(var Settings: TNPCProjectSettings; const OutputPath: String; const CompilationType: TNPCProjectTypes; const OutputExtension: String);
  var
    idx: Integer;
  begin
    idx := Length(Settings.OutputTypes);
    SetLength(Settings.OutputTypes, idx + 1);
    //
    Settings.OutputTypes[idx].OutputPath := OutputPath;
    Settings.OutputTypes[idx].OutputStream := Nil;
    Settings.OutputTypes[idx].CompilationType := CompilationType;
    Settings.OutputTypes[idx].CompilationExtension := OutputExtension;
    //Settings.OutputTypes[idx].CompilationSearchPaths := ;
  end;

var
  token: TNPCToken;
  stemp: String;
  //
  OutputPath: String;
  CompilationTypes: TNPCProjectTypes;
  OutputExtension: String;
begin
  Assert(Assigned(AToken), 'no token passed');
//  AddToken(TNPCToken.Create(tokSetting, AToken.Location.Copy, False, False, '$program-type', EmptyTokenMD5));
  //
  OutputPath := '';
  CompilationTypes := [];
  OutputExtension := '';
  //
  stemp := '';
  //
  while Texer.IsNotEmpty do begin
    token := Texer.PeekToken;
    if (token.&Type = tokIdent) then begin
      Texer.SkipToken;
      if SameText(token.Value, 'Windows32') and EnsureTypeIsNotSet(token, CompilationTypes, [ptWindows, ptLinux, ptAndroid, ptWebAssembly, pt32Bit, pt64Bit]) then begin
        CompilationTypes := CompilationTypes + [ptWindows];
        CompilationTypes := CompilationTypes + [pt32Bit];
      end
      else if SameText(token.Value, 'Windows64') and EnsureTypeIsNotSet(token, CompilationTypes, [ptWindows, ptLinux, ptAndroid, ptWebAssembly, pt32Bit, pt64Bit]) then begin
        CompilationTypes := CompilationTypes + [ptWindows];
        CompilationTypes := CompilationTypes + [pt64Bit];
      end
      else if SameText(token.Value, 'Linux32') and EnsureTypeIsNotSet(token, CompilationTypes, [ptWindows, ptLinux, ptAndroid, ptWebAssembly, pt32Bit, pt64Bit]) then begin
        CompilationTypes := CompilationTypes + [ptLinux];
        CompilationTypes := CompilationTypes + [pt32Bit];
      end
      else if SameText(token.Value, 'Linux64') and EnsureTypeIsNotSet(token, CompilationTypes, [ptWindows, ptLinux, ptAndroid, ptWebAssembly, pt32Bit, pt64Bit]) then begin
        CompilationTypes := CompilationTypes + [ptLinux];
        CompilationTypes := CompilationTypes + [pt64Bit];
      end
      else if SameText(token.Value, 'Android32') and EnsureTypeIsNotSet(token, CompilationTypes, [ptWindows, ptLinux, ptAndroid, ptWebAssembly, pt32Bit, pt64Bit]) then begin
        CompilationTypes := CompilationTypes + [ptAndroid];
        CompilationTypes := CompilationTypes + [pt32Bit];
      end
      else if SameText(token.Value, 'Android64') and EnsureTypeIsNotSet(token, CompilationTypes, [ptWindows, ptLinux, ptAndroid, ptWebAssembly, pt32Bit, pt64Bit]) then begin
        CompilationTypes := CompilationTypes + [ptAndroid];
        CompilationTypes := CompilationTypes + [pt64Bit];
      end
      else if SameText(token.Value, 'WebAssembly') and EnsureTypeIsNotSet(token, CompilationTypes, [ptWindows, ptLinux, ptAndroid, ptWebAssembly, pt32Bit, pt64Bit]) then begin
        CompilationTypes := CompilationTypes + [ptWebAssembly];
        CompilationTypes := CompilationTypes + [pt32Bit];
      end
//      else if SameText(token.Value, 'WebAssembly64') and EnsureTypeIsNotSet(token, CompilationTypes, [ptWindows, ptLinux, ptAndroid, ptWebAssembly, pt32Bit, pt64Bit]) then begin
//        CompilationTypes := CompilationTypes + [ptWebAssembly];
//        CompilationTypes := CompilationTypes + [pt64Bit];
//      end
      else if SameText(token.Value, 'CONSOLE') and EnsureTypeIsNotSet(token, CompilationTypes, [ptCONSOLE, ptGUI, ptDLL, ptTEXT]) then
        CompilationTypes := CompilationTypes + [ptCONSOLE]
      else if SameText(token.Value, 'GUI') and EnsureTypeIsNotSet(token, CompilationTypes, [ptCONSOLE, ptGUI, ptDLL, ptTEXT]) then
        CompilationTypes := CompilationTypes + [ptGUI]
      else if SameText(token.Value, 'DLL') and EnsureTypeIsNotSet(token, CompilationTypes, [ptCONSOLE, ptGUI, ptDLL, ptTEXT]) then
        CompilationTypes := CompilationTypes + [ptDLL]
      else if SameText(token.Value, 'TEXT') and EnsureTypeIsNotSet(token, CompilationTypes, [ptCONSOLE, ptGUI, ptDLL, ptTEXT]) then
        CompilationTypes := CompilationTypes + [ptTEXT]
      else
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, '', sProjectFile]));
    end
    else if (token.&Type = tokString) then begin
      Texer.SkipToken;
      if (Length(token.Value) > 1) and (token.Value[1] = '.') and (Length(OutputExtension) = 0) then begin // extension
        OutputExtension := token.Value;
      end
      else if (Length(token.Value) > 0) and (Length(OutputPath) = 0) then begin
        OutputPath := token.Value;
      end
      else
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, '', sProjectFile]));
    end
    else if TokenIsReservedSymbol(token, rs_CCurly) then begin
      AddCompilationType(TNPCProjectSettings(SettingsPtr^), OutputPath, CompilationTypes, OutputExtension);
//      AddToken(TNPCToken.Create(tokIdent, AToken.Location.Copy, False, False, ProjectTypeToIdent(CompilationTypes, OutputExtension, OutputPath), EmptyTokenMD5));
      stemp := ProjectTypeToString(CompilationTypes);
      ConsoleWriteln('Compilation target___: ' + stemp + IfThen(Length(OutputExtension) > 0, ' (extension: ' + OutputExtension + ')'));
      ConsoleWriteln('          output path: ' + OutputPath);
      stemp := '';
      OutputPath := '';
      CompilationTypes := [];
      OutputExtension := '';
      Break;
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, '', sProjectFile]));
  end;
end;

procedure TNPCSourceParser.ParseSearchPath(const AToken: TNPCToken; const ABlock: TNPC_AST);

  function AddProjectSearchPath(var Settings: TNPCProjectSettings; const SearchPath: String; const Recursive: Boolean): Boolean;
  var
    i, idx: Integer;
    checked_path: String;
    is_recursive: Boolean;
  begin
    Result := False;
    for i:=0 to High(Settings.ProjectSearchPaths) do begin
      checked_path := Settings.ProjectSearchPaths[i];
      is_recursive := False;
      if EndsText('{*}', checked_path) then begin
        checked_path := LeftStr(checked_path, Length(checked_path) - 3);
        is_recursive := True;
      end;
      if SameText(checked_path, SearchPath) then begin
        if is_recursive and not Recursive then
          Exit;
        if not is_recursive and Recursive then
          ConsoleWriteln('Search path update___: ' + SearchPath + IfThen(Recursive, '{*}'));
          Settings.ProjectSearchPaths[i] := SearchPath + IfThen(Recursive, '{*}');
        Exit;
      end;
    end;
    //
    idx := Length(Settings.ProjectSearchPaths);
    SetLength(Settings.ProjectSearchPaths, idx + 1);
    //
    Settings.ProjectSearchPaths[idx] := SearchPath + IfThen(Recursive, '{*}');
    Result := True;
  end;

var
  token: TNPCToken;
  //
  SearchPath: String;
  Recursive: Boolean;
  PathExists: Boolean;
begin
  Assert(Assigned(AToken), 'no token passed');
//  AddToken(TNPCToken.Create(tokSetting, AToken.Location.Copy, False, False, '$search-path', EmptyTokenMD5));
  //
  SearchPath := '';
  Recursive := False;
  //
  while Texer.IsNotEmpty do begin
    token := Texer.PeekToken;
    if (token.&Type = tokIdent) then begin
      Texer.SkipToken;
      if SameText(token.Value, 'recursive') then begin
        Recursive := True;
      end
      else
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnknownIdentIn, [token.Value, '', sProjectSetting]));
    end
    else if (token.&Type = tokString) then begin
      Texer.SkipToken;
      if Length(token.Value) > 0 then begin // search path
        SearchPath := token.Value;
        if EndsText('*', SearchPath) then begin
          Delete(SearchPath, Length(SearchPath), 1);
          Recursive := True;
        end;
      end
      else
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, '', sProjectSetting]));
    end
    else if TokenIsReservedSymbol(token, rs_CCurly) then begin
      PathExists := DirectoryExists(SearchPath);
      if AddProjectSearchPath(TNPCProjectSettings(SettingsPtr^), SearchPath, Recursive) then begin
//        AddToken(TNPCToken.Create(tokString, AToken.Location.Copy, False, False, SearchPath + IfThen(Recursive, '{*}'), EmptyTokenMD5));
        ConsoleWriteln('Project search path__: ' + SearchPath + IfThen(Recursive, '{*}') + IfThen(PathExists, '', ' - not exist or is not reachable'));
      end;
      SearchPath := '';
      Break;
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, '', sProjectFile]));
  end;
end;

procedure TNPCSourceParser.ParseDefines(const AToken: TNPCToken; const ABlock: TNPC_AST);
var
  token: TNPCToken;
begin
  Assert(Assigned(AToken), 'no token passed');
//  AddToken(TNPCToken.Create(tokSetting, AToken.Location.Copy, False, False, '$defines', EmptyTokenMD5));
  //
  while Texer.IsNotEmpty do begin
    token := Texer.PeekToken;
    if token.&Type = tokString then begin
      Texer.SkipToken;
      if Length(token.Value) > 0 then begin // defines section name
        //TNPCProjectSettings(SettingsPtr^).Defines

      end;
    end
    else if TokenIsReservedSymbol(token, rs_OCurly) then begin
      Texer.SkipToken;
      ParseDefinition(ABlock);
    end
//    else if TokenIsReservedSymbol(token, rs_Dollar) then begin
//      Texer.SkipToken;
//      ParseSettingOrDefineDirective;
//    end
    else if TokenIsReservedSymbol(token, rs_CCurly) then begin
      Texer.SkipToken;
      Break;
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, '', sProjectFile]));
  end;
end;

procedure TNPCSourceParser.ParseDefinition;
begin

end;

procedure TNPCSourceParser.ParseDirectives;
begin

end;

procedure TNPCSourceParser.ParseComment;
var
  token: TNPCToken;
begin
  while Texer.IsNotEmpty do begin
    token := Texer.GetToken;
    if TokenIsReservedSymbol(token, rs_Dollar) then begin
    end
    else if TokenIsReservedSymbol(token, rs_At) then begin
    end
    else if TokenIsReservedSymbol(token, rs_CCurly) then begin
      Break;
    end
//    else if TokenIsReservedIdent(token, ri_imports) then begin
//      ParseImports(token);
//    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, '', sProjectFile]));
  end;
end;

//ActualParams = '(' [ ( Expression | Designator ) | { ',' ( Expression | Designator ) } ] ')' .
//
//Designator = BasicDesignator { Selector } .
//
//BasicDesignator = Ident |
//                  Ident [ ActualParams ] |
//                  Ident '(' Expression ')' .
//
//Selector = '^' |
//           '[' Expression { ',' Expression } ']' |
//           '.' Ident |
//           '(' ActualParams ')' .
//
//Statement = [ Label ':' ]
//            [ ( Designator | Ident ) [ ActualParams ] { Selector } |
//              CompoundStatement |
//              AssignStatement |
//              IfStatement |
//              CaseStatement |
//              ForStatement |
//              GotoStatement |
//              WithStatement ] .
//
//Label = Ident .
//
//Ident = ( Letter | '_' ) { Letter | '_' | Digit }.
//
//StatementList = Statement ';' { Statement ';' } .
//
//CompoundStatement = ( 'begin' | '{' )
//                    StatementList
//                    ( 'end' | '}' ) .

function TNPCSourceParser.ParseStatement(const AFlags: TNPCParseStatementFlags): TNPC_ASTStatement;
var
  token, next_token: TNPCToken;
  name: String;
  stmt: TNPC_ASTStatement;
  target: TNPC_ASTExpression;
  rhs: TNPC_ASTExpression;
  expr: TNPC_ASTExpression;
  block: TNPC_ASTStatementBlock;
begin
  SkipComments;
  token := Texer.PeekToken;
  //
  if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then begin
    Result := Nil;
  end
  else if TokenIsIdent(token) then begin // might be: label, assignment, expression-statement
    next_token := Texer.NextToken;
    if TokenIsReservedSymbol(next_token, rs_Colon) then begin // ':' - label
      Texer.SkipToken;
      Texer.SkipToken;
      stmt := ParseStatement([stafEmptyStatementIsAcceptable]);
      Result := TNPC_ASTStatementLabel.Create(token.Location, token.Value, stmt);
      Exit;
    end
    else if TokenIsReservedSymbol(next_token, rs_Assign) or TokenIsReservedSymbol(next_token, rs_Comma) then begin // ':=' - assignment or ',' - multi-assignment
      Result := ParseAssignmentNew(token);
//      Result := ParseAssignmentOrMulti;

//      Texer.SkipToken;
//      Texer.SkipToken;
//      Result := ParseAssignment(token);
      Exit;
    end
    else begin
      // expression or assignment statement
      expr := ParseExpression(token, 0);

      // expression statement
      Texer.ExpectToken([tokSemicolon]);
      Result := TNPC_ASTStatementExpression.Create(expr.Location, expr);
    end;
  end
  else if TokenIsReservedIdent(token, ri_type) then begin
    ParseTypeDeclaration(token, []);
    Result := Nil;
  end
  else if TokenIsReservedIdent(token, ri_var) then begin
    ParseVariableDeclaration(token);
    Result := Nil;
  end
  else if TokenIsReservedIdent(token, ri_begin) or TokenIsReservedSymbol(token, rs_OCurly) then begin
//    ParseBlock(token);
//    Result := Nil;
    Result := ParseBlock(token);
  end
  else if TokenIsReservedIdent(token, ri_defer) then begin
    Result := ParseDefer(token);
  end
  else if TokenIsReservedIdent(token, ri_return) then begin
    Result := ParseReturn(token);
  end
  else if TokenIsReservedIdent(token, ri_result) then begin
    Result := ParseResult(token);
  end
  else if TokenIsReservedIdent(token, ri_if) then begin
    Result := ParseIfStatement(token);
  end
  else if TokenIsReservedIdent(token, ri_case) then begin
    Result := ParseCaseStatement(token);
  end
  else if TokenIsReservedIdent(token, ri_for) then begin
    Result := ParseFor(token);
  end
  else if TokenIsReservedIdent(token, ri_while) then begin
    Result := ParseWhile(token);
  end
  else begin
    // expression or assignment statement
    expr := ParseExpression(token, 0);
    // if next token is assign ':=' -> assignment
    token := Texer.PeekToken;
    if TokenIsReservedSymbol(token, rs_Assign) then begin
      // left must be an lvalue; we keep it as expression for now
      target := expr;
      Texer.SkipToken; // consume :=
      token := Texer.PeekToken;
      rhs := ParseExpression(token, 0);
      Texer.ExpectToken([tokSemicolon]);
      Result := TNPC_ASTStatementAssign.Create(target.Location, target, rhs);
    end
    else begin
      // expression statement
      Texer.ExpectToken([tokSemicolon]);
      Result := TNPC_ASTStatementExpression.Create(expr.Location, expr);
    end;
  end;
end;

// expr        = simp_expr [ ('<' | '<=' | '=' | '>' | '>=' | ('<>' | '!=') | 'in' | 'is') simp_expr ] ';' .
//
// simp_expr   = term { ('+' | '-' | 'or' | 'xor') term } .
//
// term        = factor { ('*' | '/' | 'div' | 'mod' | 'and' | 'shr' | 'shl') factor } .
//
// factor      = number | ident | string | char | 'nil'
//             | 'not' factor
//             | '@' factor
//             | 'inherited' [ factor ]
//             | '^' ident
//             | set_factor
//             | call_factor
//             | '(' expr { ',' expr } ')'
//             | ('+' | '-') factor .
//
// set_factor  = '[' [ ident { (',' | '..') ident } ] ']' .
//
// call_factor = (ident | string) [ { call_params } ] .
//
// call_params = ('(' expr { ',' expr } ')') | ('[' expr { '.' expr } ']') | '^' | 'as' ident .

function TNPCSourceParser.ParseExpression(const AToken: TNPCToken; Precedence: Integer = 0): TNPC_ASTExpression;
//var
//  token: TNPCToken;
//  loc: TNPCLocation;
//  exp: TNPC_ASTExpression;
//begin
//  Result := Nil;
//  if Texer.IsEmpty then
//    Exit;
//
//  loc := AToken.Location;
//
//  exp := ParseAssignment(AToken); // maybe this is assignment
//
//  token := Texer.PeekToken;
//  while Texer.IsNotEmpty and TokenIsReservedSymbol(token, rs_Comma) do begin
//    token := Texer.GetToken;
//    exp := ParseAssignment(token);
////    Result := TNPC_ASTCommaExpression(loc, Result, exp, False);
//    loc := token.Location;
//  end;
//  Result := exp;
//end;
var
  token: TNPCToken;
  left: TNPC_ASTExpression;
  curPrec: Integer;
//  opTok: TNPCToken;
begin
  token := Texer.PeekToken;
  left := ParsePrimaryExpression({AToken,} token); // nud - null denotation
  left.EndToken := Texer.LastToken;
  try
    while True do begin
      token := Texer.PeekToken;
      if token.&Type = tokEOF then
        Break;
      curPrec := GetPrecedence(token);
      if (curPrec = 0) or (curPrec <= Precedence) then
        Break;
//      opTok := token;
//      Texer.SkipToken;
      left := ParseSimpleExpression(token, left); // led - left denotation
      left.EndToken := Texer.LastToken;
    end;
    Result := left;
  except
    left.Free;
    raise;
  end;
end;

function TNPCSourceParser.ParsePrimaryExpression(const AToken: TNPCToken): TNPC_ASTExpression;
//function TNPCSourceParser.ParsePrimaryExpression(const ALeftToken: TNPCToken; const AToken: TNPCToken): TNPC_ASTExpression;
//var
//  token: TNPCToken;
//  loc: TNPCLocation;
//  exp: TNPC_ASTExpression;
//  temp: String;
//  fs: TFormatSettings;
//  neg: Boolean;
//  dot_pos: Integer;
//begin
//  Result := Nil;
//  if Texer.IsEmpty then
//    Exit;
//
//  loc := AToken.Location;
//
//  case AToken.&Type of
//    tokIdent: begin
//    end;
//    tokNumber: begin
//    end;
//    tokString: begin
//    end;
//  end;
//end;
var
  token, next_token: TNPCToken;
  op: UTF8String;
  inner: TNPC_ASTExpression;
begin
  if TokenIsIdent(AToken) or TokenIsNumber(AToken) or TokenIsString(AToken) or TokenIsChar(AToken) then begin
    Result := ParseLiteral({ALeftToken,} AToken);
  end
  else if TokenIsReservedSymbol(AToken, [rs_Minus, rs_Plus]) then begin
    Texer.SkipToken;
    token := Texer.PeekToken;
    op := AToken.Value;
    inner := ParseExpression(token, GetPrecedence(token)); // unary - bind tighter than additive
    Result := TNPC_ASTExpressionUnary.Create(AToken.Location, op, inner);
  end
  else if TokenIsReservedIdent(AToken, ri_not) or TokenIsReservedSymbol(AToken, rs_Exclamation) then begin // 'not' / '!'
    Texer.SkipToken;
    token := Texer.PeekToken;
    op := token.Value;
    inner := ParseExpression(token, 7);
    Result := TNPC_ASTExpressionUnary.Create(AToken.Location, op, inner);
  end
  else if TokenIsReservedSymbol(AToken, rs_OParen) then begin // '('
    Texer.SkipToken;
    token := Texer.PeekToken;
    inner := ParseExpression(token, 0);
    Texer.ExpectReservedSymbol(rs_CParen); // ')'
    Result := inner;
  end
  else if TokenIsReservedSymbol(AToken, rs_OBracket) then begin // '['
    Texer.SkipToken;
    inner := ParseSet(AToken);
    Texer.ExpectReservedSymbol(rs_CBracket); // ']'
    Result := inner;
  end
  else if TokenIsReservedSymbol(AToken, rs_Dot) then begin // '.'
    next_token := Texer.NextToken;
    if TokenIsReservedSymbol(next_token, rs_OCurly) then begin // '{'
      Texer.SkipToken;
      //Texer.SkipToken;
      inner := ParseTupleExpression(AToken);
      //Texer.ExpectReservedSymbol(rs_Semicolon); // ';'
      Result := inner;
    end
    else begin
      NotImplemented;
    end;
  end
  else
    raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserUnexpectedTokenTypeIn, [AToken.TokenToString, AToken.Value, '', sStatement]));

  SkipComments;
end;

function TNPCSourceParser.ParseSimpleExpression(const AToken: TNPCToken; const ALeft: TNPC_ASTExpression): TNPC_ASTExpression;
var
  token: TNPCToken;
  op: UTF8String;
  right: TNPC_ASTExpression;
  idx: TNPC_ASTExpressionIndex;
  identType: TNPC_ASTTypes;
  identName: UTF8String;
  Sym: TNPCSymbol;
  SymType: TNPCSymbolType;
  SymKind: TNPCSymbolKind;
begin
  if TokenIsReservedSymbol(AToken, [rs_Plus,       // '+'
                                    rs_Minus,      // '-'
                                    rs_Asterisk,   // '*'
                                    rs_Div,        // '/'
                                    rs_Percent,    // '%'
                                    rs_Equal,      // '='
                                    rs_Exclamation // '!'
//                                    rs_NotEqual,
//                                    rs_NotEqual1,
//                                    rs_NotEqual2

                                   ]) or
     TokenIsReservedIdent(AToken, [ri_and, // 'and'
                                   ri_or,  // 'or'
                                   ri_xor, // 'xor'
                                   ri_not, // 'not'
                                   ri_shl, // 'shl'
                                   ri_shr, // 'shr'
                                   ri_mod  // 'mod'
                                  ]) then begin
    //Texer.SkipToken;
    op := AToken.Value;
    right := ParseExpression(AToken, GetPrecedence(AToken));
    Result := TNPC_ASTExpressionBinary.Create(AToken.Location, ALeft, op, right);
  end
  else if TokenIsReservedSymbol(AToken, rs_Dot) then begin // member access: ident '.' expr
    // determine what type of handling we need to use here
    identType := AST_UNKNOWN;
    if ALeft <> Nil then
      identType := ALeft.&Type;
    Assert(identType <> AST_UNKNOWN);
    if identType = AST_IDENTIFIER then begin
      Sym := TNPC_ASTExpressionIdent(ALeft).ResolvedSymbol;
      if Sym = Nil then begin
        identName := TNPC_ASTExpressionIdent(ALeft).Name;
        Sym := CurrentScope.ResolveSymbol(identName);
        TNPC_ASTExpressionIdent(ALeft).ResolvedSymbol := Sym;
      end;
      //
      SymKind := Sym.Kind;
      SymType := Sym.&Type;
      if SymKind = KIND_Type then begin
        case SymType of
          TYPE_Unresolved: ;
          TYPE_Enum: ;
          TYPE_EnumConst: ;
          TYPE_Set: ;
          TYPE_SetConst: ;
          TYPE_BitSet: ;
          TYPE_Array: ;
          TYPE_Record: begin
            Result := ParseRecordMember(AToken, ALeft);
          end;
          TYPE_RecordField: ;
          TYPE_Class: begin
            Result := ParseClassMember(AToken, ALeft);
          end;
          TYPE_Pointer: ;
          TYPE_Literal: ;
          TYPE_Procedure: ;
          TYPE_ForeignFunction: ;
        end;
      end
      else
        raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserUnexpectedTokenTypeIn, [AToken.TokenToString, AToken.Value, '', sStatement]));
    end
    else
      raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserUnexpectedTokenTypeIn, [AToken.TokenToString, AToken.Value, '', sStatement]));
  end
  else if TokenIsReservedSymbol(AToken, rs_OParen) then begin // call: ident '(' expr (, expr) ')'
    Result := ParseCall(AToken, ALeft);
  end
  else if TokenIsReservedSymbol(AToken, rs_OBracket) then begin // indexing: '[' expr ']'
    Result := ParseIndex(AToken, ALeft);
  end
  else if TokenIsReservedIdent(AToken, ri_if) then begin // 'if' expression
    right := ParseIfExpression(AToken);
    Result := TNPC_ASTExpressionBinary.Create(AToken.Location, ALeft, op, right);
  end
  else if TokenIsReservedIdent(AToken, ri_case) then begin // 'case' expression
    Result := ParseCaseExpression(AToken);
  end
  else if TokenIsReservedIdent(AToken, ri_in) then begin // 'in'
//      var RightExpr := ParseExpression;
//
//      if not (RightExpr is TExprSetLiteral) then
//        raise Exception.Create('Right side of "in" must be a set');
//
//      var SetLit := TExprSetLiteral(RightExpr);
//      var SetType := SetLit.SetType;
//
//      // Determine type of left operand
//      var LeftType: TNPC_ASTTypeExpression := nil;
//      if Left is TExprVariable then begin
//        var Sym := CurrentScope.Resolve(TExprVariable(Left).Name);
//        if Assigned(Sym) then
//          LeftType := Sym.TypeRef;
//      end
//      else if Left is TExprEnumConst then
//        LeftType := TExprEnumConst(Left).EnumType
//      else if Left is TExprLiteral then
//        LeftType := TExprLiteral(Left).LiteralType;
//
//      if not Assigned(LeftType) then
//        raise Exception.Create('Cannot determine type of left operand in "in" expression');
//
//      // If RHS is empty set -> infer type from LHS
//      if (not Assigned(SetType)) then begin
//        SetType := TTypeSet.Create;
//        SetType.ElementType := LeftType;
//        SetLit.SetType := SetType;
//      end
//      else if SetType.ElementType <> LeftType then
//        raise Exception.CreateFmt(
//          'Type mismatch in "in": left operand is %s, but set element type is %s',
//          [LeftType.Name, SetType.ElementType.Name]);
//
//      Left := TExprInOp.Create(Left, RightExpr);
//      Result := Left;
  end
  else
    raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserUnexpectedTokenTypeIn, [AToken.TokenToString, AToken.Value, '', sStatement]));
end;

function TNPCSourceParser.ParseVariableExpression: TNPC_ASTExpressionVariable;
var
  token: TNPCToken;
  Expr, Index: TNPC_ASTExpression;
  Sym: TNPCSymbol;
begin
  // BASE IDENTIFIER
  token := Texer.ExpectToken(tokIdent, 'identifier expected');

  Sym := CurrentScope.ResolveSymbol(token.Value);
  if not Assigned(Sym) then // check if it is multi-assign ???
    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnknownIdentIn, [token.Value, '', sStatement]));

  Expr := TNPC_ASTExpressionVariable.Create(token.Location, token.Value, Sym);

  // CHAIN: .field or [index]
  while True do begin
    token := Texer.PeekToken;

    // MEMBER ACCESS: .
    if TokenIsReservedSymbol(token, rs_Dot) then begin // '.'
      Texer.SkipToken;

      token := Texer.ExpectToken(tokIdent, 'member name expected');

      Sym := CurrentScope.ResolveSymbol(token.Value);
      if not Assigned(Sym) then // check if it is multi-assign ???
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnknownIdentIn, [token.Value, '', sStatement]));

      Expr := TNPC_ASTExpressionMember.Create(token.Location, Expr, token.Value, Sym, Sym.TypeRef);
    end

    // INDEX ACCESS: [ ... ]
    else if TokenIsReservedSymbol(token, rs_OBracket) then begin // '['
      Texer.SkipToken;

      token := Texer.PeekToken;
      Index := ParseExpression(token, 0);
      if not (Index is TNPC_ASTExpressionNumber) then
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserExpectedElementsButGot, ['index number', Index.ToString, '', sStatement]));

      Expr := TNPC_ASTExpressionIndex.Create(token.Location, Expr, Index);

      Texer.ExpectReservedSymbol(rs_CBracket);
    end

    else
      Break;
  end;

  // FINAL CAST
  if not (Expr is TNPC_ASTExpressionVariable) then
    Result := TNPC_ASTExpressionVariable(Expr) // assuming inheritance
  else
    Result := TNPC_ASTExpressionVariable(Expr);
end;

procedure TNPCSourceParser.ParseTypeDeclaration(const AToken: TNPCToken; const AFlags: TNPCParseDeclarationFlags);
var
  token, next_token: TNPCToken;
  stmt: TNPC_ASTStatementTypeDeclaration;
begin
  // assume current token at 'type'
  //Texer.ExpectReservedToken(ri_type);
  while Texer.IsNotEmpty do begin
    SkipComments;
    token := Texer.ExpectToken(tokIdent, Format(sParserExpectedNameAfterKeyword, ['type', 'type']));

    Texer.ExpectReservedSymbol(rs_Equal); // '='

    stmt := ParseTypeDefinition(token, token.Value);
    AddStatement(stmt);

    Texer.ExpectReservedSymbol(rs_Semicolon); // ';'

    token := Texer.PeekToken; // look for ident and '=' or bail
    next_token := Texer.NextToken; // look for ident and '=' or bail
    if not (TokenIsIdent(token) and TokenIsReservedSymbol(next_token, rs_Equal)) then
      Break;
  end;
end;

//  ElemType: TNPC_ASTTypeExpression;
//  recDecl: TNPC_ASTTypeRecord;
//  fieldName: TNPCToken;
//  fieldTyp: TNPCToken;
//  fType: TNPC_ASTTypeExpression;
//
//  loc: TNPCLocation;
//  typeSym,
//  fieldSym: TNPCSymbol;
//  typeName: String;
//  fldName,
//  fldTypeName: String;

function TNPCSourceParser.ParseTypeDefinition(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
var
  token: TNPCToken;
  is_packed: Boolean;
//  typeRef: TNPC_ASTTypeExpression;

label
  _record, _array;

begin
  is_packed := False;
  token := Texer.PeekToken; // '(', '[', '[packed] record', '[packed] array', 'class [of]', 'object [of]', 'procedure', 'function', 'set of', 'interface'
  if TokenIsReservedSymbol(token, rs_OParen) then begin // '(' - enum
    Texer.SkipToken;
    Result := ParseEnumTypeDeclaration(ATypeToken, ATypeName);
  end
  else if TokenIsReservedSymbol(token, rs_OBracket) then begin // '[' - set
    Texer.SkipToken;
    Result := ParseSetTypeDeclaration(ATypeToken, ATypeName);
//    Result := TNPC_ASTTypeDefinition.Create(ATypeToken.Location, ATypeName, -1); // size will be determined during type & size checking phase
//    Result.DefinitionType := DEF_Set;
//    Result.SetDescription := TNPC_ASTTypeSet(typeRef);
  end
  else if TokenIsReservedIdent(token, ri_set) then begin // 'set' 'of' enum
    Texer.SkipToken;
    Texer.ExpectReservedToken(ri_of); // 'of'
//    token := Texer.PeekToken;
//    ElemType := ParseType(token, ATypeName);
//    Result := TNPC_ASTTypeSet.Create(token.Location, ElemType);
//    Texer.SkipToken;
    Result := ParseSetOfTypeDeclaration(ATypeToken, ATypeName);
//    Result := TNPC_ASTTypeDefinition.Create(ATypeToken.Location, ATypeName, -1); // size will be determined during type & size checking phase
//    Result.DefinitionType := DEF_Set;
//    Result.SetDescription := TNPC_ASTTypeSet(typeRef);
  end
  else if TokenIsReservedIdent(token, ri_packed) then begin // 'packed' ident
    Texer.SkipToken;
    is_packed := True;
    token := Texer.PeekToken;
    if TokenIsReservedIdent(token, ri_record) then
      goto _record;
    if TokenIsReservedIdent(token, ri_array) then
      goto _array;

    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnknownIdentIn, [token.TokenToString, '', sStatement]));
  end
  else if TokenIsReservedIdent(token, ri_array) then begin // 'array'
    _array:
    Texer.SkipToken;
    //token := Texer.PeekToken;
    Result := ParseArrayTypeDeclaration(ATypeToken, ATypeName);
//    Result := TNPC_ASTTypeDefinition.Create(ATypeToken.Location, ATypeName, -1); // size will be determined during type & size checking phase
//    Result.DefinitionType := DEF_Array;
//    Result.RecordDescription := TNPC_ASTTypeRecord(typeRef);
  end
  else if TokenIsReservedIdent(token, ri_record) then begin // 'record'
    _record:
    Texer.SkipToken;
    token := Texer.PeekToken;
    Result := ParseRecordTypeDeclaration(ATypeToken, ATypeName);
//    Result := TNPC_ASTTypeDefinition.Create(ATypeToken.Location, ATypeName, -1); // size will be determined during type & size checking phase
//    Result.DefinitionType := DEF_Record;
//    Result.RecordDescription := TNPC_ASTTypeRecord(typeRef);
  end
  else if TokenIsReservedIdent(token, ri_class) then begin // 'class'
    Texer.SkipToken;
    Result := ParseClassTypeDeclaration(ATypeToken, ATypeName);
  end
  else if TokenIsReservedIdent(token, ri_object) then begin // 'object'
    NotImplemented;
  end
  else if TokenIsReservedIdent(token, ri_type) then begin // 'type' ident
    NotImplemented;
  end
  else if TokenIsIdent(token) then begin // procedure / function
    NotImplemented;
  end
  else
    raise NPCSyntaxError.ParserError(token.Location, Format(sPareserUnexpectedSyntax, ['type', 'ident = type_description in type ' + sStatement]));

//  if TokenIsIdent(AToken) then begin // 'type' ident (and then '=' expr)
//    // Simple type name
//    Result := TNPC_ASTTypeName.Create(AToken.Location, AToken.Value);
//    Texer.SkipToken;
//  end
//  else
//    raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserUnexpectedTokenIn, [AToken.TokenToString, '', sStatement])); // 'Unexpected type syntax'
end;
(*
function TNPCSourceParser.ParseTypeReference: TNPC_ASTType;
var
  token: TNPCToken;
  fieldTypeName: TNPCToken;
  fieldTypeSym: TNPCSymbol;
  //fieldType: TNPC_ASTType;
begin
  token := Texer.PeekToken;
  if not TokenIsIdent(token) then
    raise NPCSyntaxError.ParserError(token.Location, Format(sParserExpectedElementsButGot, ['type name', token.ToString, 'identifier ', 'type']));
  fieldTypeName := token;
  Texer.SkipToken;

  // resolve field type symbol
//  if not ResolveType(fieldName.Value, fieldType) then
//    raise NPCSyntaxError.ParserError(fieldTypeName.Location, Format(sParserUnknownTypeFor, [fieldTypeName.ToString, 'field ', ]));
  fieldTypeSym := LookupSymbol(fieldTypeName.Value);
  if fieldTypeSym = Nil then
    raise NPCSyntaxError.ParserError(fieldTypeName.Location, Format(sParserUnknownTypeFor, [fieldTypeName.ToString, '', sDeclaration]));
  Result := fieldTypeSym.TypeRef;
end;
*)
function TNPCSourceParser.ParseTypeReference: TNPC_ASTTypeReference;
var
  token: TNPCToken;
  TypeSym: TNPCSymbol;
  TypeRef: TNPC_ASTTypeReference;
begin
  token := Texer.PeekToken;
  if TokenIsReservedIdent(token, ri_array) then
    Exit(ParseTypeReference_ArrayType(token));

  if TokenIsReservedIdent(token, ri_record) then
    Exit(ParseTypeReference_RecordType(token));

  if IsFunctionTypeAhead(token) then
    Exit(ParseTypeReference_FunctionType(token));

  Result := ParseTypeReference_NamedType(token); // fallback
end;

function TNPCSourceParser.IsFunctionTypeAhead(const AToken: TNPCToken): Boolean;
var
  token, next_token: TNPCToken;
  Depth: Integer;
  idx: Integer;
begin
  Result := False;

  if not TokenIsIdent(AToken) then
    Exit;

  next_token := Texer.NextToken;

  // must be IDENT '('
  if not TokenIsReservedSymbol(next_token, rs_OParen) then
    Exit;

  // scan (...) safely
  Depth := 0;
  idx := 1;
  repeat
    token := Texer.PeekToken(idx);
    if TokenIsReservedSymbol(token, rs_OParen) then
      Inc(Depth)
    else if TokenIsReservedSymbol(token, rs_CParen) then begin
      Dec(Depth);
      if Depth = 0 then begin // after ')' we may have ':' (for function) or not (for procedure)
        token := Texer.PeekToken(idx + 1);

        if TokenIsReservedSymbol(token, rs_Colon) or
           TokenIsReservedSymbol(token, rs_Semicolon) or
           TokenIsReservedSymbol(token, rs_Comma) then
          Exit(True)
        else
          Exit(True); // still valid procedure type
      end;
    end;

    Inc(idx);
  until Texer.IsEmpty;

//  Result := True;
end;

//  next_token := Texer.NextToken;
//  if (TokenIsIdent(token) and TokenIsReservedSymbol(next_token, rs_OParen)) or TokenIsIdent(token) then

//  token := Texer.ExpectToken(tokIdent, 'type name expected');
//
//  TypeSym := CurrentScope.ResolveSymbol(token.Value);
//  if not Assigned(TypeSym) then
//    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnknownIdentIn, [token.Value, '', 'type reference']));

function TNPCSourceParser.ParseTypeReference_NamedType(const AToken: TNPCToken): TNPC_ASTTypeReference;
var
  TypeRef: TNPC_ASTTypeReference;
begin
  TypeRef := TNPC_ASTTypeReference.Create(AToken.Location);
  TypeRef.Kind := REF_Named;

  TypeRef.BaseSymbol := ParseTypeReference_QualifiedIdent(AToken);

  // GENERICS: <T, U, ...>
  if TokenIsReservedSymbol(Texer.PeekToken, rs_LessThan) then begin // '<'
    Texer.SkipToken; // '<'

    TypeRef.GenericArgs := TObjectList<TNPC_ASTTypeReference>.Create(True);

    repeat
      SkipComments;

      TypeRef.GenericArgs.Add(ParseTypeReference);

      SkipComments;

      if TokenIsReservedSymbol(Texer.PeekToken, rs_Comma) then // ','
        Texer.SkipToken
      else
        Break;

    until False;

    Texer.ExpectReservedSymbol(rs_GreaterThan); // '>'
  end;

  Result := TypeRef;
end;

function TNPCSourceParser.ParseTypeReference_ArrayType(const AToken: TNPCToken): TNPC_ASTTypeReference;
begin
  Texer.ExpectReservedToken(ri_array);
  SkipComments;

  Texer.ExpectReservedToken(ri_of);
  SkipComments;

  Result := TNPC_ASTTypeReference.Create(AToken.Location);
  Result.Kind := REF_Array;
  Result.ElementType := ParseTypeReference;
end;

function TNPCSourceParser.ParseTypeReference_RecordType(const AToken: TNPCToken): TNPC_ASTTypeReference;
var
  token: TNPCToken;
  FieldName: String;
  FieldSym: TNPCSymbol;
  FieldType: TNPC_ASTTypeReference;
begin
  Texer.ExpectReservedToken(ri_record);

  Result := TNPC_ASTTypeReference.Create(AToken.Location);
  Result.Kind := REF_Record;
  Result.Fields := TObjectList<TNPCSymbol>.Create(True);

  while not TokenIsReservedIdent(Texer.PeekToken, ri_end) do begin
    SkipComments;

    token := Texer.ExpectToken(tokIdent, 'field name expected');
    FieldName := token.Value;

    Texer.ExpectReservedSymbol(rs_Colon); // ':'

    FieldType := ParseTypeReference;

    FieldSym := TNPCSymbol.Create(token.Location, FieldName, KIND_RecordField, CurrentScope.ResolveTypeKind(FieldType), False, FieldType, CurrentBlock);
    Result.Fields.Add(FieldSym);

    Texer.ExpectReservedSymbol(rs_Semicolon); // ';'
  end;

  Texer.ExpectReservedToken(ri_end);
end;

function TNPCSourceParser.ParseTypeReference_FunctionType(const AToken: TNPCToken): TNPC_ASTTypeReference;
var
  token: TNPCToken;
  ParamName: string;
  ParamType: TNPC_ASTTypeReference;
  ParamSym: TNPCSymbol;
begin
  Result := TNPC_ASTTypeReference.Create(AToken.Location);
  Result.Kind := REF_Function;
  Result.Params := TObjectList<TNPCSymbol>.Create(True);

  if TokenIsReservedIdent(Texer.PeekToken, ri_function) then begin
    Result.IsProcedure := False;
    Texer.SkipToken;
  end
  else begin
    Result.IsProcedure := True;
    Texer.SkipToken;
  end;

  SkipComments;

  // PARAMETERS
  if TokenIsReservedSymbol(Texer.PeekToken, rs_OParen) then begin
    Texer.SkipToken;

    while not TokenIsReservedSymbol(Texer.PeekToken, rs_CParen) do begin
      SkipComments;

      token := Texer.ExpectToken(tokIdent, 'param name');
      ParamName := token.Value;

      Texer.ExpectReservedSymbol(rs_Colon); // ':'

      ParamType := ParseTypeReference;

      ParamSym := TNPCSymbol.Create(token.Location, ParamName, KIND_Param, CurrentScope.ResolveTypeKind(ParamType), False, ParamType, CurrentBlock);
      Result.Params.Add(ParamSym);

      if TokenIsReservedSymbol(Texer.PeekToken, rs_Comma) then // ','
        Texer.SkipToken
      else
        Break;
    end;

    Texer.ExpectReservedSymbol(rs_CParen);
  end;

  // RETURN TYPE
  if not Result.IsProcedure then begin
    Texer.ExpectReservedSymbol(rs_Colon); // ':'
    Result.ReturnType := ParseTypeReference;
  end;
end;

function TNPCSourceParser.ParseTypeReference_QualifiedIdent(const AToken: TNPCToken): TNPCSymbol;
var
  token: TNPCToken;
  Name: String;
begin
  token := Texer.ExpectToken(tokIdent, 'identifier expected');
  Name := token.Value;

  while TokenIsReservedSymbol(Texer.PeekToken, rs_Dot) do begin
    Texer.SkipToken;
    token := Texer.ExpectToken(tokIdent, 'identifier expected');
    Name := Name + '.' + token.Value; // this if wrong, its not that way, we need to search in scope in order to find what we need, else report error
  end;

  Result := CurrentScope.ResolveSymbol(Name);
  if not Assigned(Result) then
    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnknownIdentIn, [Name, '', 'type reference']));
end;

// ident = (enum1, enum2, ... );
function TNPCSourceParser.ParseEnumTypeDeclaration(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
var
  token: TNPCToken;
  Enum: TNPC_ASTTypeEnum;
  Value: Integer;
  Ident: TNPCToken;
  Lit: TNPC_ASTExpression;
  TypeDef: TNPC_ASTTypeDefinition;
begin
  Enum := TNPC_ASTTypeEnum.Create(ATypeToken.Location);
  Value := 0;
  while True do begin
    Ident := Texer.ExpectToken([tokIdent]);

    // support explicit values: (Red=10, Green, Blue=30)
    token := Texer.PeekToken;
    if TokenIsReservedSymbol(token, rs_Assign) then begin
      // reuse expression parser for assigned value, but expect integer literal only
      Texer.SkipToken;
      token := Texer.PeekToken;
      Lit := ParseExpression(token, 0);
      if not (Lit is TNPC_ASTExpressionLiteral) then
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserExpectedButGot, ['literal', Lit.ToString]));
      Value := StrToInt(TNPC_ASTExpressionLiteral(Lit).Value);
      token := Texer.PeekToken;
    end;

    Enum.Members.Add(Ident.Value, Value);

    // add enum member to current scope as constant
    CurrentScope.DefineConst(Ident.Location, Ident.Value, TYPE_EnumConst, 4, Builtin_Type_Integer, Enum, Value);

    Inc(Value);
    if not TokenIsReservedSymbol(token, rs_Comma) then
      Break;
    Texer.SkipToken;
  end;
  Texer.ExpectReservedSymbol(rs_CParen); // ')'

  // add enum to current scope as type
  CurrentScope.DefineType(Enum.Location, ATypeName, TYPE_Enum, 4, Enum, CurrentBlock);

  TypeDef := TNPC_ASTTypeDefinition.Create(ATypeToken.Location, ATypeName, -1); // size will be determined during type & size checking phase
  TypeDef.DefinitionType := DEF_Enum;
  TypeDef.EnumDescription := Enum;

  Result := TNPC_ASTStatementTypeDeclaration.Create(ATypeToken.Location, ATypeName, TypeDef);
end;

function TNPCSourceParser.ParseSetTypeDeclaration(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
var
  token: TNPCToken;
  SetType: TNPC_ASTTypeSet;
  ElemType: TNPC_ASTType;
  ThisType: TNPC_ASTType;
  Value: Integer;
  Ident: TNPCToken;
  Sym: TNPCSymbol;
//  SetLit: TNPC_ASTExpression;
  TypeDef: TNPC_ASTTypeDefinition;
  Elements: TList<TNPC_ASTExpression>;
  Elem: TNPC_ASTExpression;
  expr: TNPC_ASTExpression;
begin
//  SetType := TNPC_ASTTypeSet.Create(ATypeToken.Location, Nil);
  Elements := TList<TNPC_ASTExpression>.Create;
  try
    token := Texer.PeekToken;
    // collect all elements of a set, later we will check if they are correct and alowed in the set
    if not TokenIsReservedSymbol(token, rs_CBracket) then begin
      repeat
        expr := ParseExpression(token, 0);
        Elements.Add(expr);
        token := Texer.PeekToken;
      until not TokenIsReservedSymbol(token, rs_Comma);
    end;
    Texer.ExpectReservedSymbol(rs_CBracket);

    SetType := Nil;
    ElemType := Nil;

    if Elements.Count > 0 then begin
      // infer element type from first element
      for Elem in Elements do begin
        ThisType := Nil;

        if Elem is TNPC_ASTExpressionEnumConst then
          ThisType := TNPC_ASTExpressionEnumConst(Elem).EnumType
        else if Elem is TNPC_ASTExpressionLiteral then
          ThisType := TNPC_ASTExpressionLiteral(Elem).LiteralType
        else if Elem is TNPC_ASTExpressionVariable then begin
          Sym := CurrentScope.ResolveSymbol(TNPC_ASTExpressionVariable(Elem).Name);
          if not Assigned(Sym) then
            raise NPCSyntaxError.ParserError(Elem.Location, Format(sParserUnknownIdentIn, [TNPC_ASTExpressionVariable(Elem).Name, 'set declaration ', sStatement]));
          ThisType := Sym.TypeRef;
        end;

        if not Assigned(ThisType) then
          raise NPCSyntaxError.ParserError(Elem.Location, Format(sParserExpectedElementsButGot, ['literal', TNPC_ASTExpressionVariable(Elem).Name, 'set declaration ', sStatement]));

        if not Assigned(ElemType) then
          ElemType := ThisType
        else if ElemType <> ThisType then
          raise NPCSyntaxError.ParserError(Elem.Location, Format(sParserTypeMismatchInLiteral, ['set literal', ElemType.ToString, ThisType.ToString]));
      end;

      SetType := TNPC_ASTTypeSet.Create(ATypeToken.Location, ElemType);
    end;

    // For empty set, SetType stays nil (to be filled later from context)
//    SetLit := TNPC_ASTExpressionSetLiteral.Create(ATypeToken.Location, SetType);
//    for Elem in Elements do
//      TNPC_ASTExpressionSetLiteral(SetLit).Elements.Add(Elem);
    if SetType <> Nil then begin
      for Elem in Elements do
        SetType.Elements.Add(Elem);
    end;
  finally
    Elements.Free;
  end;

  // add set to current scope as type
  CurrentScope.DefineType(ATypeToken.Location, ATypeName, TYPE_Set, 4, SetType, CurrentBlock);

  TypeDef := TNPC_ASTTypeDefinition.Create(ATypeToken.Location, ATypeName, -1); // size will be determined during type & size checking phase
  TypeDef.DefinitionType := DEF_Set;
  TypeDef.SetDescription := SetType;

  Result := TNPC_ASTStatementTypeDeclaration.Create(ATypeToken.Location, ATypeName, TypeDef);
end;

function TNPCSourceParser.ParseSetOfTypeDeclaration(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
begin
  NotImplemented;
end;

function TNPCSourceParser.ParseArrayTypeDeclaration(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
var
  token: TNPCToken;
  expr: TNPC_ASTExpression;
  ArrayType: TNPC_ASTTypeArray;
  Sym: TNPCSymbol;
  IndexType: TNPC_ASTType;
  ElemType: TNPC_ASTType;
  TypeDef: TNPC_ASTTypeDefinition;
begin
//    Texer.ExpectReservedToken(ri_of); // 'of'
//    Result := TTypeArray.Create(ParseType);
//    Texer.ExpectReservedSymbol(rs_OBracket); // '['
  IndexType := Nil;
  //Texer.SkipToken;
  token := Texer.PeekToken;
  if TokenIsReservedSymbol(token, rs_OBracket) then begin // '[' expr ']'
    Texer.SkipToken;
    token := Texer.PeekToken;
    expr := ParseExpression(token, 0);

    if expr is TNPC_ASTExpressionEnumConst then
      IndexType := TNPC_ASTExpressionEnumConst(expr).EnumType
    else if expr is TNPC_ASTExpressionLiteral then
      IndexType := TNPC_ASTExpressionLiteral(expr).LiteralType
    else if expr is TNPC_ASTExpressionVariable then begin
      Sym := CurrentScope.ResolveSymbol(TNPC_ASTExpressionVariable(expr).Name);
      if not Assigned(Sym) then
        raise NPCSyntaxError.ParserError(expr.Location, Format(sParserUnknownIdentIn, [TNPC_ASTExpressionVariable(expr).Name, 'array declaration ', sStatement]));
      IndexType := Sym.TypeRef;
    end
    else
      raise NPCSyntaxError.ParserError(expr.Location, Format(sParserTypeMismatchInLiteral, ['array index', '<number>', expr.ToString]));

    Texer.ExpectReservedSymbol(rs_CBracket);
  end;

  Texer.ExpectReservedToken(ri_of); // 'of'
  token := Texer.PeekToken;
  expr := ParseExpression(token, 0);

  if expr is TNPC_ASTExpressionIdent then begin
    if TNPC_ASTExpressionIdent(expr).ResolvedSymbol <> Nil then
      Sym := TNPC_ASTExpressionIdent(expr).ResolvedSymbol
    else begin
      Sym := CurrentScope.ResolveSymbol(TNPC_ASTExpressionIdent(expr).Name);
      if not Assigned(Sym) then
        raise NPCSyntaxError.ParserError(expr.Location, Format(sParserUnknownIdentIn, [TNPC_ASTExpressionVariable(expr).Name, 'array declaration ', sStatement]));
      TNPC_ASTExpressionIdent(expr).ResolvedSymbol := Sym;
    end;
    ElemType := Sym.TypeRef;
  end
  else if expr is TNPC_ASTExpressionEnumConst then
    ElemType := TNPC_ASTExpressionEnumConst(expr).EnumType
  else if expr is TNPC_ASTExpressionLiteral then
    ElemType := TNPC_ASTExpressionLiteral(expr).LiteralType
  else if expr is TNPC_ASTExpressionVariable then begin
    Sym := CurrentScope.ResolveSymbol(TNPC_ASTExpressionVariable(expr).Name);
    if not Assigned(Sym) then
      raise NPCSyntaxError.ParserError(expr.Location, Format(sParserUnknownIdentIn, [TNPC_ASTExpressionVariable(expr).Name, 'array declaration ', sStatement]));
    ElemType := Sym.TypeRef;
  end
  else
    raise NPCSyntaxError.ParserError(expr.Location, Format(sParserTypeMismatchInLiteral, ['array type', '<number>', expr.ToString]));

  ArrayType := TNPC_ASTTypeArray.Create(ATypeToken.Location, ElemType, IndexType);

  // add array to current scope as type
  CurrentScope.DefineType(ATypeToken.Location, ATypeName, TYPE_Array, 4, ArrayType, CurrentBlock);

  TypeDef := TNPC_ASTTypeDefinition.Create(ATypeToken.Location, ATypeName, -1); // size will be determined during type & size checking phase
  TypeDef.DefinitionType := DEF_Array;
  TypeDef.ArrayDescription := ArrayType;

  Result := TNPC_ASTStatementTypeDeclaration.Create(ATypeToken.Location, ATypeName, TypeDef);
end;

//    var Rec := TTypeRecord.Create;
//    while not Match(tkEnd) do
//    begin
//      var FieldName := ExpectIdentifier;
//      Expect(tkColon);
//      Rec.Fields.Add(FieldName, ParseType);
//      Match(tkSemicolon); // optional
//    end;
//    Result := Rec;
//
//    var Rec := TTypeRecord.Create;
//    repeat
//      var FieldName := ExpectIdentifier;
//      Expect(tkColon);
//      var FieldType := ParseType;
//      Rec.Fields.Add(FieldName, FieldType);
//      Expect(tkSemicolon);
//    until not (Current.Kind = tkIdentifier);
//    Expect(tkEnd);
//    Result := Rec;
//
  // create the type symbol and register it early (enables recursive types)
//  typeRef := TNPC_ASTTypeDefinition.Create(loc, typeName, -1);
//  typeSym := TNPCSymbol.Create(typeName, skType, False, typeRef, CurrentBlock);
//  DeclareSymbol(typeName, typeSym); // add to current scope before processing fields
//
//  // allocate the Fields dictionary
//  typeSym.Fields := TDictionary<string, TNPCSymbol>.Create;
//
//  // parse fields: loop until 'end'
//  while FToken.Kind <> tkEnd do begin
//    // simple grammar: field1, field2: TypeName; ...
//    // parse identifiers separated by commas
//    while True do begin
//      if FToken.Kind <> tkIdentifier then
//        raise Exception.Create('Expected field name in record');
//      fldName := FToken.Text; Advance;
//      // expect ':' then type name
//      ExpectKind(tkColon);
//      if FToken.Kind <> tkIdentifier then
//        raise Exception.Create('Expected type name for field');
//      fldTypeName := FToken.Text; Advance;
//
//      // resolve field type symbol
//      fieldSym := LookupSymbol(fldTypeName);
//      if fieldSym = nil then
//        raise Exception.CreateFmt('Unknown type "%s" for field %s', [fldTypeName, fldName]);
//
//      // create a symbol for the field: kind = skVar, Typ = fieldType
//      fieldSym := TNPCSymbol.Create(fldName, skVar, fieldSym);
//      typeSym.Fields.AddOrSetValue(fldName, fieldSym);
//
//      // comma or semicolon?
//      if FToken.Kind = tkComma then Advance
//      else Break;
//    end;
//    ExpectKind(tkSemicolon);
//  end;
//
//  ExpectKind(tkEnd); // 'end'
//  ExpectKind(tkSemicolon); // final ';' after type

function TNPCSourceParser.ParseRecordTypeDeclaration(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
var
  token: TNPCToken;
  expr: TNPC_ASTExpression;
  RecordType: TNPC_ASTTypeRecord;
  Fields: TObjectList<TNPCToken>;
  fieldTypeSym,
  Sym: TNPCSymbol;
  fieldType: TNPC_ASTType;
  TypeDef: TNPC_ASTTypeDefinition;
  fieldName,
  fieldTypeName: TNPCToken;
begin
  RecordType := TNPC_ASTTypeRecord.Create(ATypeToken.Location);

  // create the type symbol and register it early (enables recursive types)
  TypeDef := TNPC_ASTTypeDefinition.Create(ATypeToken.Location, ATypeName, -1); // size will be determined during type & size checking phase
  TypeDef.Flags := 1;
  TypeDef.DefinitionType := DEF_Record;
  TypeDef.RecordDescription := RecordType; // put record to type definition
  // add record to current scope as type, before processing fields
  CurrentScope.DefineType(ATypeToken.Location, ATypeName, TYPE_Record, 4, RecordType, CurrentBlock);
  //Sym := CurrentScope.LastSymbol;

//  // allocate the Fields dictionary
//  typeSym.Fields := TDictionary<string, TNPCSymbol>.Create;

//  token := Texer.PeekToken;
//  while not TokenIsReservedIdent(token, ri_end) do begin
//    fieldName := Texer.ExpectToken([tokIdent]); //   Consume(tkIdentifier, 'Expected field name').Lexeme;
//    Texer.ExpectReservedSymbol(rs_Colon); // Consume(tkColon, 'Expected ":" after field name');
//    fieldTyp := Texer.ExpectToken([tokIdent]); // Consume(tkIdentifier, 'Expected type').Lexeme;
//    if not ResolveType(fieldTyp.Value, fType) then
//      raise NPCSyntaxError.ParserError(fieldTyp.Location, Format(sParserTypeDefNotFound, [fieldTyp.Value]));
//    recDecl.Fields.Add(fieldName.Value, fType);
//    Texer.ExpectReservedSymbol(rs_Semicolon); // ';'
//  end;
//  Texer.ExpectReservedToken(ri_end); // Consume(tkEnd, 'Expected "end" to close record');
//  Texer.ExpectReservedSymbol(rs_Semicolon); // Consume(tkSemicolon, 'Expected ";" after record type');

  Fields := TObjectList<TNPCToken>.Create(False);
  try
    // parse fields: loop until 'end'
    token := Texer.PeekToken;
    while not TokenIsReservedIdent(token, ri_end) do begin
      // simple grammar: field1, field2: TypeName; ...
      // parse identifiers separated by commas
      while True do begin
        if not TokenIsIdent(token) then
          raise NPCSyntaxError.ParserError(token.Location, Format(sParserExpectedElementsButGot, ['field name', token.ToString, 'record ', sDeclaration]));

        Fields.Add(token);
        Texer.SkipToken;

        // check is there is comma ',' or colon ':', if comma then add fields to list until comma is present,
        // than get the type name and apply to all fields in the list
        token := Texer.PeekToken;
        while TokenIsReservedSymbol(token, rs_Comma) do begin
          Fields.Add(token);
          Texer.SkipToken;
          token := Texer.PeekToken;
        end;
        // expect ':' then type name
        Texer.ExpectReservedSymbol(rs_Colon);

        token := Texer.PeekToken;
        if not TokenIsIdent(token) then
          raise NPCSyntaxError.ParserError(token.Location, Format(sParserExpectedElementsButGot, ['type name', token.ToString, 'record ', sDeclaration]));
        fieldTypeName := token;
        Texer.SkipToken;

        // resolve field type symbol
//          if not ResolveType(fieldName.Value, fieldType) then
//            raise NPCSyntaxError.ParserError(fieldTypeName.Location, Format(sParserUnknownTypeFor, [fieldTypeName.ToString, 'field ', ]));
        fieldTypeSym := LookupSymbol(fieldTypeName.Value);
        if fieldTypeSym = Nil then
          raise NPCSyntaxError.ParserError(fieldTypeName.Location, Format(sParserUnknownTypeFor, [fieldTypeName.ToString, 'field ', sDeclaration]));
        fieldType := fieldTypeSym.TypeRef;

        for fieldName in Fields do begin
          // create a symbol for the field: kind = skVar, Typ = fieldType
          //Sym := TNPCSymbol.Create(fieldName.Value, skVar, stRecordField, False, fieldSym.TypeRef, TypeDef);
          CurrentScope.DefineVar(fieldName.Location, fieldName.Value, TYPE_RecordField, -1, fieldType, TypeDef);
          RecordType.Fields.Add(fieldName.Value, fieldType);
        end;

        // semicolon?
        token := Texer.PeekToken;
        if TokenIsReservedSymbol(token, rs_Semicolon) then
          Break;
      end;
      Texer.ExpectReservedSymbol(rs_Semicolon);
    end;
  finally
    Fields.Free;
  end;
  Texer.ExpectReservedToken(ri_end); // Consume(tkEnd, 'Expected "end" to close record');
  Texer.ExpectReservedSymbol(rs_Semicolon); // Consume(tkSemicolon, 'Expected ";" after record type');

  // return declaration
  Result := TNPC_ASTStatementTypeDeclaration.Create(ATypeToken.Location, ATypeName, TypeDef);
end;

// type
//   TestClass = class
//     <visibility sections>
//     <fields>
//     <init procedures>
//     public properties
//       <property declarations>
//   end;


//  type_decl       ::= "type" ident "=" class_decl ";"
//
//  class_decl      ::= "class"
//                      class_section*
//                      "end"
//
//  class_section   ::= visibility_section
//                    | property_section
//                    | field_decl
//                    | method_decl
//
//  visibility_section ::= "private" | "protected" | "public"
//
//  property_section ::= "public" "properties" property_decl*
//
//  property_decl ::= ident ":" type_ident property_spec*
//
//  property_spec ::= "read" ident
//                  | "write" ident
//                  | "init" init_value
//                  | "default" expression
//
//  init_value ::= ident
//               | string_literal
//               | block_expr
//               | expression

function TNPCSourceParser.ParseClassTypeDeclaration(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
var
  token: TNPCToken;
  TypeClass: TNPC_ASTTypeClass;
  TypeDef: TNPC_ASTTypeDefinition;
  VisibilityType: TNPC_ASTClassVisibilityTypeEnum;
  IsPropertySection: Boolean;
  Sym: TNPCSymbol;

//  expr: TNPC_ASTExpression;
//  RecordType: TNPC_ASTTypeRecord;
//  Fields: TObjectList<TNPCToken>;
//  fieldTypeSym,
//  Sym: TNPCSymbol;
//  fieldType: TNPC_ASTTypeExpression;
//  fieldName,
//  fieldTypeName: TNPCToken;
begin
  TypeClass := TNPC_ASTTypeClass.Create(ATypeToken.Location, ATypeName);

  TypeDef := TNPC_ASTTypeDefinition.Create(ATypeToken.Location, ATypeName, -1);
  TypeDef.DefinitionType := DEF_Class;
  TypeDef.ClassDescription := TypeClass;

  Sym := CurrentScope.DefineType(ATypeToken.Location, ATypeName, TYPE_Class, -1, TypeDef, CurrentBlock);

  // return declaration
  Result := TNPC_ASTStatementTypeDeclaration.Create(ATypeToken.Location, ATypeName, TypeDef);

  PushScope(CurrentScope, ATypeName); // new scope for class type declaration
  try
    Sym.Scope := CurrentScope;
//    if TokenIs(tkInit) then begin
//      NextToken;
//      Prop.InitExpr := ParseExpression(0);
//    end;

//    if TokenIs(tkPublic) and NextTokenIs(tkProperties) then begin
//      Mode := cmProperties;
//    end;

//TSymbol (kind = skField or skMethod)
//  RefType := inferred type
//  ValueExpr := parsed expression

    VisibilityType := VIS_Class;
    IsPropertySection := False;
    while Texer.IsNotEmpty do begin
      SkipComments;
      token := Texer.PeekToken;
      if TokenIsReservedIdent(token, ri_private) then begin // 'private' visibility
        Texer.SkipToken;
        VisibilityType := VIS_Private;
        IsPropertySection := False;
        Continue;
      end
      else if TokenIsReservedIdent(token, ri_protected) then begin // 'protected' visibility
        Texer.SkipToken;
        VisibilityType := VIS_Protected;
        IsPropertySection := False;
        Continue;
      end
      else if TokenIsReservedIdent(token, ri_public) then begin // 'public' visibility
        Texer.SkipToken;
        VisibilityType := VIS_Public;
        IsPropertySection := False;
        Continue;
      end
      else if TokenIsReservedIdent(token, ri_end) then begin
        Break;
      end
      else if TokenIsIdent(token) then begin
        if TokenIsReservedIdent(token, ri_property) then begin
          Texer.SkipToken;
          IsPropertySection := True;
          Continue;
        end;
        //
        while not TokenIsReservedIdent(token, [ri_private, ri_protected, ri_public, ri_end]) do begin
          if IsPropertySection then
            ParseClassPropertyDeclaration(TypeClass, VisibilityType)
          else
            ParseClassMemberDeclaration(TypeClass, VisibilityType);
          token := Texer.PeekToken;
        end;
      end
      else
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnknownIdentIn, [token.Value, 'class ', sDeclaration]));
    end;
  finally
    PopScope;
  end;

  Texer.ExpectReservedToken(ri_end); // Consume(tkEnd, 'Expected "end" to close record');
//  Texer.ExpectReservedSymbol(rs_Semicolon); // Consume(tkSemicolon, 'Expected ";" after record type');
end;

procedure TNPCSourceParser.ParseClassPropertyDeclaration(const ATypeClass: TNPC_ASTTypeClass; const AVisibilityType: TNPC_ASTClassVisibilityTypeEnum);
var
  token: TNPCToken;
  Prop: TNPC_ASTTypeClassProperty;
  Name: String;
  Sym: TNPCSymbol;
begin
  if Texer.IsEmpty then
    Exit;

  token := Texer.ExpectToken(tokIdent, Format(sParserExpectedElementsButGot, ['property name', Texer.PeekToken.ToString, 'class ', sDeclaration]));
  Name := token.Value;

  Texer.ExpectReservedSymbol(rs_Colon);

  Prop := TNPC_ASTTypeClassProperty.Create(token.Location, Name, AVisibilityType);
  Prop.PropertyType := ParseTypeReference;

  token := Texer.PeekToken;
  while not TokenIsReservedSymbol(token, rs_Semicolon) do begin
    if TokenIsReservedIdent(token, ri_read) then begin
      Texer.SkipToken;
      token := Texer.ExpectToken(tokIdent, Format(sParserExpectedElementsButGot, ['read symbol name', Texer.PeekToken.ToString, 'class ', sDeclaration]));
      Sym := CurrentScope.ResolveSymbol(token.Value);
      if Sym = Nil then
        raise NPCSyntaxError.ParserError(token.Location, Format(sParseSymbolNotFound, [token.Value, 'class ', sDeclaration]));
      Prop.ReadField := Sym;
    end
    else if TokenIsReservedIdent(token, ri_write) then begin
      Texer.SkipToken;
      token := Texer.ExpectToken(tokIdent, Format(sParserExpectedElementsButGot, ['write symbol name', Texer.PeekToken.ToString, 'class ', sDeclaration]));
      Sym := CurrentScope.ResolveSymbol(token.Value);
      if Sym = Nil then
        raise NPCSyntaxError.ParserError(token.Location, Format(sParseSymbolNotFound, [token.Value, 'class ', sDeclaration]));
      Prop.WriteField := Sym;
    end
    else if TokenIsReservedIdent(token, ri_init) then begin
      Texer.SkipToken;
      token := Texer.PeekToken;
      Prop.InitExpr := ParseExpression(token, 0);
    end
    else if TokenIsReservedIdent(token, ri_default) then begin
      Texer.SkipToken;
      token := Texer.PeekToken;
      Prop.DefaultExpr := ParseExpression(token, 0);
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserInvalidPropertySpecifier, [token.Value, 'class ', sDeclaration, 'read / write / init / default']));
    token := Texer.PeekToken;
  end;

  Texer.ExpectReservedSymbol(rs_Semicolon);

  ATypeClass.Properties.Add(Prop);
end;

procedure TNPCSourceParser.ParseClassMemberDeclaration(const ATypeClass: TNPC_ASTTypeClass; const AVisibilityType: TNPC_ASTClassVisibilityTypeEnum);
var
  token, tokenName: TNPCToken;
  Typ: TNPC_ASTType;
  Sym: TNPCSymbol;
  Expr: TNPC_ASTExpression;
  Method: TNPC_ASTTypeClassMethod;
  ProcDecl: TNPC_ASTStatement;
  flags: TNPCParseDeclarationFlags;
begin
  token := Texer.PeekToken;
  if not TokenIsIdent(token) then
    raise NPCSyntaxError.ParserError(token.Location, Format(sParserInvalidPropertySpecifier, [token.Value, 'class ', sDeclaration, 'read / write / init / default']));
  Texer.SkipToken;

  tokenName := token;

  token := Texer.PeekToken;
  if TokenIsReservedSymbol(token, rs_Colon) then begin // ':' - FIELD
    Texer.SkipToken;
    Typ := ParseTypeReference;
    Texer.ExpectReservedSymbol(rs_Semicolon); // ';'

    CurrentScope.DefineClassField(tokenName.Location, tokenName.Value, TYPE_Class, Typ, ATypeClass);

    if ATypeClass.Fields = Nil then
      ATypeClass.Fields := TDictionary<UTF8String, TNPC_ASTType>.Create;

    ATypeClass.Fields.Add(tokenName.Value, Typ);
  end
  else if TokenIsReservedSymbol(token, rs_Equal) then begin // '=' - Init block or expression member
    Texer.SkipToken;

    token := Texer.PeekToken;
    Expr := ParseExpression(token, 0);
    Texer.ExpectReservedSymbol(rs_Semicolon); // ';'

    Method := TNPC_ASTTypeClassMethod.Create(tokenName.Location, tokenName.Value, AVisibilityType);

    Sym := CurrentScope.DefineClassMethod(tokenName.Location, tokenName.Value, TYPE_Class, Method, ATypeClass);  // or skFieldWithInit
    Sym.ValueExpr := Expr;

    if ATypeClass.Methods = Nil then
      ATypeClass.Methods := TObjectList<TNPC_ASTTypeClassMethod>.Create(True);

    ATypeClass.Methods.Add(Method);
  end
  else if TokenIsReservedSymbol(token, rs_OParen) or TokenIsReservedSymbol(token, rs_Semicolon) then begin // '(' or ';'
    // Example:
    // MyProc(const A: String);
    // MyFunc(A: Integer): String;
    // MyProc(...) { ... };

    Method := TNPC_ASTTypeClassMethod.Create(tokenName.Location, tokenName.Value, AVisibilityType);

    // add method to the scope of class
    Sym := CurrentScope.DefineClassMethod(tokenName.Location, tokenName.Value, TYPE_Class, Method, ATypeClass);  // or skFieldWithInit

    if ATypeClass.Methods = Nil then
      ATypeClass.Methods := TObjectList<TNPC_ASTTypeClassMethod>.Create(True);

    ATypeClass.Methods.Add(Method);

    flags := [decfDoNotAddSymbol, decfBodyMayBeSkipped, decfInlineSymbolDeclaration];
    ProcDecl := ParseProcedureDeclaration(tokenName, Sym, flags); // do not add symbol in this function, it is added beneath in method symbol
    if not (ProcDecl is TNPC_ASTStatementProcedure) then
      raise NPCSyntaxError.ParserError(tokenName.Location, Format(sParserExpectedElementsButGot, ['method declaration', tokenName.Value, 'class ', sDeclaration]));
  end
  else
    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnknown, ['class member', token.Value]));
end;

//1: FileIO.OpenFile(...)
//2: ParseQualifiedName
//3: FindClassMethod
//4: ParseProcedureSignatureOnly
//5: Compare with declared signature
//6: PushScope
//7: RegisterProcedureSymbols (from DECLARATION)
//8: ParseBody
//9: Attach body
function TNPCSourceParser.ParseClassMethodDefinition(const AClassSym: TNPCSymbol; const AMethod: TNPCToken; const AClassMethodDefinitionLocation: TNPCLocation): TNPC_ASTStatementProcedure;
var
  token: TNPCToken;
  MethodSym: TNPCSymbol;
  ProcDecl, ParsedSig: TNPC_ASTStatementProcedure;
  oldBlock: TNPC_ASTStatementBlock;
  Body: TNPC_ASTStatement;
begin
  // FIND METHOD IN CLASS
  MethodSym := FindClassMethod(AClassSym, AMethod.Value, AClassMethodDefinitionLocation);
  if not Assigned(MethodSym) then
    raise NPCSyntaxError.ParserError(AMethod.Location, Format('method "%s" not declared in class "%s"', [AMethod.Value, AClassSym.Name]));

  ParsedSig := ParseProcedureSignatureOnly(CurrentBlock, MethodSym.Name);

  if not CompareProcedureSignatures(MethodSym.ProcDecl, ParsedSig) then
    raise NPCSyntaxError.ParserError(Texer.LastToken.Location, 'Method signature does not match declaration');

  ProcDecl := TNPC_ASTStatementProcedure.Create(AMethod.Location, CurrentBlock, AMethod.Value, False, []);

  // PARSE DEFINITION
//  if MethodSym.Scope = Nil then

  EnterScope(MethodSym.Scope); // new lexical scope for this block
  try
    oldBlock := CurrentBlock;
    CurrentBlock := TNPC_ASTStatementBlock(ProcDecl);
    try
      RegisterProcedureSymbols(MethodSym.ProcDecl);
      token := Texer.PeekToken;
      Body := ParseProcedureDefinitionBody(token, []);
      ProcDecl.Body := Body;
    finally
      CurrentBlock := oldBlock;
    end;
  finally
    LeaveScope;
  end;

  // BIND BODY TO DECLARATION
  MethodSym.ProcDecl.Body := Body;

  Result := ProcDecl;
end;

procedure TNPCSourceParser.ParseVariableDeclaration(const AToken: TNPCToken);
var
  token, next_token: TNPCToken;
  varName: TNPCToken;
  varType: TNPC_ASTType;
  initExpr: TNPC_ASTExpression;
  stmt: TNPC_ASTStatementVariableDeclaration;
  sym: TNPCSymbol;
begin
  // current token is 'var'
  //Texer.ExpectReservedToken(ri_var);
  while Texer.IsNotEmpty do begin
    token := Texer.ExpectToken(tokIdent, Format(sParserExpectedNameAfterKeyword, ['variable', 'var']));
    varName := token;

    Texer.ExpectReservedSymbol(rs_Colon); // ':'

    varType := ParseVariableType(varName, varName.Value);

    initExpr := Nil;
    token := Texer.PeekToken;
    if TokenIsReservedSymbol(token, rs_Equal) then begin // '=' - init value for variable
      Texer.SkipToken;
      token := Texer.PeekToken;
      initExpr := ParseExpression(token, 0);
    end;

    Texer.ExpectReservedSymbol(rs_Semicolon); // ';'

    // declare symbol immediately in current scope
    sym := CurrentScope.DefineVar(varName.Location, varName.Value, TYPE_Literal, -1, varType, CurrentBlock);

    // add declaration
    stmt := TNPC_ASTStatementVariableDeclaration.Create(varName.Location, varName.Value, varType, initExpr);
    stmt.SymbolRef := sym;
    AddStatement(stmt);

    token := Texer.PeekToken; // look for ident and '=' or bail
    next_token := Texer.NextToken; // look for ident and '=' or bail
    if not (TokenIsIdent(token) and TokenIsReservedSymbol(next_token, rs_Colon)) then
      Break;
  end;
end;

function TNPCSourceParser.ParseVariableType(const AVarToken: TNPCToken; const AVarName: String): TNPC_ASTType;
var
  token: TNPCToken;
  sym: TNPCSymbol;
begin
  token := Texer.ExpectToken(tokIdent, Format(sParserExpectedNameAfter, ['type', 'variable ', sDeclaration]));
  sym := LookupSymbol(token.Value);
  if sym = Nil then
    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnknownTypeFor, [token.Value, 'variable ', sDeclaration]));
  Result := sym.TypeRef;
end;

function TNPCSourceParser.ParseBlock(const AToken: TNPCToken): TNPC_ASTStatement;
//procedure TNPCSourceParser.ParseBlock(const AToken: TNPCToken);
var
  token: TNPCToken;
  stmt: TNPC_ASTStatement;
  oldBlock: TNPC_ASTStatementBlock;
  scopeName: String;
begin
  //Texer.ExpectReservedToken(ri_begin);
  if not TokenIsReservedIdent(AToken, ri_begin) and not TokenIsReservedSymbol(AToken, rs_OCurly) then
    raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, '', sStatement]));

  Texer.SkipToken; // consume 'begin' / '{'

  scopeName := 'inner scope name';
  if CurrentBlock is TNPC_ASTStatementProcedure then
    scopeName := 'inner_' + TNPC_ASTStatementProcedure(CurrentBlock).Name
  else if CurrentBlock is TNPC_ASTStatementBlock then begin
    if BLOCK_IsMainProcedure in TNPC_ASTStatementBlock(CurrentBlock).Flags then
      scopeName := 'main block';
  end;

  PushScope(CurrentScope, scopeName); // new lexical scope for this block
  stmt := TNPC_ASTStatementBlock.Create(AToken.Location, CurrentBlock, [BLOCK_ConsistsOfOrderedStatements]);
  //AddStatement(stmt);
  try
    oldBlock := CurrentBlock;
    CurrentBlock := TNPC_ASTStatementBlock(stmt);
    token := Texer.PeekToken;
    while not (TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly)) do begin
      if TokenIsOfType(token, [tokEOF]) then
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserExpectedElementsButGot, ['statement', 'EOF', 'block ', sDeclaration]));
      stmt := ParseStatement([stafEmptyStatementIsAcceptable]);
      if Assigned(stmt) then
        //AddStatement(stmt);
        CurrentBlock.AddStatement(stmt);
      token := Texer.PeekToken;
    end;
    //token := Texer.PeekToken;
    if not (TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly)) then
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, '', sStatement]));
    Texer.SkipToken;
    //Texer.ExpectToken([tokSemicolon]);
    Result := CurrentBlock;
  finally
    CurrentBlock := oldBlock;
    PopScope;
  end;
end;

function TNPCSourceParser.ParseQualifiedName(const AToken: TNPCToken; out AClassSym: TNPCSymbol; out AMethod: TNPCToken; out AClassMethodDefinitionLocation: TNPCLocation): Boolean;
begin
  Result := False;
  AClassSym := Nil;
  AMethod := Nil;
  AClassMethodDefinitionLocation := AToken.Location;

  AClassSym := CurrentScope.ResolveSymbol(AToken.Value);
  if not Assigned(AClassSym) then
    Exit;

  Texer.ExpectReservedSymbol(rs_Dot);

  AMethod := Texer.ExpectToken(tokIdent, 'method name expected');

  Result := True;
end;

// ProcedureDecl = Identifier [ "(" ParameterList ")" ] [ ":" "(" TypesList ")" ] [ Statements ] ";"
function TNPCSourceParser.ParseProcedureDeclaration(const AToken: TNPCToken; const AMethodSymbol: TNPCSymbol; const AFlags: TNPCParseDeclarationFlags): TNPC_ASTStatement;
var
  Proc: TNPC_ASTStatement;
begin
  Proc := ParseProcedureDefinition(AToken, AMethodSymbol, AFlags);
  Result := Proc;
end;

// ProcedureDecl = Identifier [ "(" ParameterList ")" ] [ ":" "(" TypesList ")" ] [ Statements ] ";"
function TNPCSourceParser.ParseProcedureDefinition(const AToken: TNPCToken; const AMethodSymbol: TNPCSymbol; const AFlags: TNPCParseDeclarationFlags): TNPC_ASTStatement;
var
  token: TNPCToken;
//  param: TNPC_ASTParameter;
//  stmt: TNPC_ASTStatement;
  oldBlock: TNPC_ASTStatementBlock;
  Proc: TNPC_ASTStatementProcedure;
begin
  Proc := TNPC_ASTStatementProcedure.Create(AToken.Location, CurrentBlock, AToken.Value, False, [BLOCK_ConsistsOfOrderedStatements]);

  if AMethodSymbol <> Nil then // save procedure/function declaration in method
    AMethodSymbol.ProcDecl := Proc;

  SkipComments;
  Texer.ExpectReservedSymbol(rs_OParen); // '('
  SkipComments;

  PushScope(CurrentScope, AToken.Value); // new lexical scope for this block
  try
    if AMethodSymbol <> Nil then
      AMethodSymbol.Scope := CurrentScope; // save scope in method
    ParseProcedureDefinitionHeader(Proc, AFlags);
    oldBlock := CurrentBlock;
    CurrentBlock := TNPC_ASTStatementBlock(Proc);
    try
      token := Texer.PeekToken;
      Proc.Body := ParseProcedureDefinitionBody(token, AFlags);
    finally
      CurrentBlock := oldBlock;
    end;
  finally
    PopScope;

    if not (decfDoNotAddSymbol in AFlags) then
      CurrentScope.DefineProcedure(Proc.Location, Proc.Name, Proc);
    Result := Proc;
  end;
end;

procedure TNPCSourceParser.ParseProcedureDefinitionHeader(const AProc: TNPC_ASTStatementProcedure; const AFlags: TNPCParseDeclarationFlags);
var
  token: TNPCToken;
  param: TNPC_ASTParameter;
  stmt: TNPC_ASTStatement;
  flags: TNPCParseDeclarationFlags;
begin
  SkipComments;

  // collect parameters, if any available
  token := Texer.PeekToken;
  if Texer.IsNotEmpty and not TokenIsReservedSymbol(token, rs_CParen) then begin // ')'
    flags := [decfParamMayContainModifier];
    if decfDoNotAddSymbol in AFlags then
      flags := [decfDoNotAddSymbol, decfParamMayContainModifier];
    AProc.Parameters := ParseParameterList(AProc.Name, pcParam, flags);
  end;

  Texer.ExpectReservedSymbol(rs_CParen); // ')'
  SkipComments;

  // collect returns, if any available
  token := Texer.PeekToken;
  if Texer.IsNotEmpty and TokenIsReservedSymbol(token, rs_Colon) then begin // ':'
    Texer.SkipToken;
    SkipComments;
    AProc.IsFunction := True;
    //
    token := Texer.PeekToken;
    if TokenIsReservedSymbol(token, rs_OParen) then begin // '(' - tuple
      //Texer.ExpectReservedSymbol(rs_OParen);
      Texer.SkipToken;
      SkipComments;

      flags := [decfParamDeclarationAsTuple];
      if decfDoNotAddSymbol in AFlags then
        flags := [decfDoNotAddSymbol, decfParamDeclarationAsTuple];
      AProc.Returns := ParseParameterList(AProc.Name, pcReturn, flags);

      SkipComments;
      Texer.ExpectReservedSymbol(rs_CParen); // ')'
    end
    else begin
      //SkipComments;
      flags := [];
      if decfDoNotAddSymbol in AFlags then
        flags := [decfDoNotAddSymbol];
      AProc.Returns := TObjectList<TNPC_ASTParameter>.Create(True);
      param := ParseProcedureParameter(token, AProc.Name, pcReturn, flags);
      if Assigned(param) then
        AProc.Returns.Add(param);
    end;
    SkipComments;
    //token := Texer.PeekToken;
  end
  else
    AProc.Flags := AProc.Flags + [BLOCK_DoesNotHaveResult];
end;

//    while not TokenIsReservedSymbol(token, rs_CParen) do begin
//      param := ParseProcedureParameter(token, AProc.Name, pcParam, flags);
//      if Assigned(param) then
//        AProc.Parameters.Add(param);
//
//      token := Texer.PeekToken;
//      if TokenIsReservedSymbol(token, rs_Comma) then begin
//        Texer.SkipToken;
//        SkipComments;
//        token := Texer.PeekToken;
//      end
//      else
//        Break;
//    end;
//  end;

//      token := Texer.PeekToken;
//      while not TokenIsReservedSymbol(token, rs_CParen) do begin
//        flags := [decfParamDeclarationAsTuple];
//        if decfDoNotAddSymbol in AFlags then
//          flags := [decfDoNotAddSymbol, decfParamDeclarationAsTuple];
//        param := ParseProcedureParameter(token, AProc.Name, pcReturn, flags);
//        if Assigned(param) then
//          AProc.Returns.Add(param);
//
//        token := Texer.PeekToken;
//        if TokenIsReservedSymbol(token, rs_Comma) then begin
//          Texer.SkipToken;
//          SkipComments;
//          token := Texer.PeekToken;
//        end
//        else
//          Break;
//      end;
//      SkipComments;
//      Texer.ExpectReservedSymbol(rs_CParen); // ')'


  //    token := Texer.PeekToken;
  //    while not (TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly)) do begin
  //      if TokenIsOfType(token, [tokEOF]) then
  //        raise NPCSyntaxError.ParserError(token.Location, Format(sParserExpectedElementsButGot, ['statement', 'EOF', 'block ', sDeclaration]));
  //      stmt := ParseStatement([stafEmptyStatementIsAcceptable]);
  //      if Assigned(stmt) then
  //        Proc.AddStatement(stmt);
  //      token := Texer.PeekToken;
  //    end;
  //    //token := Texer.PeekToken;
  //    if not (TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly)) then
  //      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.Value, '', sStatement]));
  //    Texer.SkipToken;
  //    //Texer.ExpectToken([tokSemicolon]);

  //  token := Texer.PeekToken;
  //  if not (TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly)) then
  //    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.Value, '', sStatement]));

function TNPCSourceParser.ParseProcedureDefinitionBody(const AToken: TNPCToken; const AFlags: TNPCParseDeclarationFlags): TNPC_ASTStatement;
begin
  // body
  Result := Nil;
  if TokenIsReservedSymbol(AToken, rs_Semicolon) and (decfBodyMayBeSkipped in AFlags) and (decfInlineSymbolDeclaration in AFlags) then begin // ';' - no body
    // declaration only
    Texer.SkipToken;
  end
  else begin
    if not TokenIsReservedIdent(AToken, ri_begin) and not TokenIsReservedSymbol(AToken, rs_OCurly) then
      raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserUnexpectedTokenTypeIn, [AToken.TokenToString, AToken.Value, '', sStatement]));

    Result := ParseBlock(AToken);

    Texer.ExpectReservedSymbol(rs_Semicolon);
  end;
end;

function TNPCSourceParser.ParseParameterList(const AProcedureName: UTF8String; AContext: TNPC_ASTParamContext; const AFlags: TNPCParseDeclarationFlags): TObjectList<TNPC_ASTParameter>;
var
  token: TNPCToken;
  param: TNPC_ASTParameter;
  flags: TNPCParseDeclarationFlags;
begin
  Result := TObjectList<TNPC_ASTParameter>.Create(True);

  token := Texer.PeekToken;
  while not TokenIsReservedSymbol(token, rs_CParen) do begin
    SkipComments;

    flags := AFlags;
    param := ParseProcedureParameter(token, AProcedureName, AContext, flags);

    if Assigned(param) then
      Result.Add(param);

    token := Texer.PeekToken;
    if TokenIsReservedSymbol(token, rs_Comma) then begin
      Texer.SkipToken;
      SkipComments;
      token := Texer.PeekToken;
    end
    else
      Break;
  end;
end;

function TNPCSourceParser.ParseProcedureParameter(const AToken: TNPCToken; const AProcedureName: UTF8String; AContext: TNPC_ASTParamContext; const AFlags: TNPCParseDeclarationFlags): TNPC_ASTParameter;
const
  sContext: Array[TNPC_ASTParamContext] of String = ('', 'return ');
var
  token, next_token: TNPCToken;
  Modifier: TNPC_ASTParamModifier;
  Name: UTF8String;
  TypeRef: TNPC_ASTTypeReference;
  VarSym: TNPCSymbol;
  Expr, Init: TNPC_ASTExpression;
  Value: TNPC_ASTValue;
  LastToken: TNPCToken;
begin
  Modifier := pmNone;
  Name := '';

  if AContext = pcParam then begin
    if decfParamMayContainModifier in AFlags then begin // const / var / out
      if TokenIsReservedIdent(AToken, ri_const) then begin
        Modifier := pmConst;
        Texer.SkipToken;
        SkipComments;
        Name := Texer.ExpectToken(tokIdent, Format(sParserExpectedElementsButGot, ['param name', AToken.Value, 'procedure/function ', sContext[AContext] + 'parameter definition'])).Value;
      end
      else if TokenIsReservedIdent(AToken, ri_var) then begin
        Modifier := pmVar;
        Texer.SkipToken;
        SkipComments;
        Name := Texer.ExpectToken(tokIdent, Format(sParserExpectedElementsButGot, ['param name', AToken.Value, 'procedure/function ', sContext[AContext] + 'parameter definition'])).Value;
      end
      else if TokenIsReservedIdent(AToken, ri_out) then begin
        Modifier := pmOut;
        Texer.SkipToken;
        SkipComments;
        Name := Texer.ExpectToken(tokIdent, Format(sParserExpectedElementsButGot, ['param name', AToken.Value, 'procedure/function ', sContext[AContext] + 'parameter definition'])).Value;
      end
      else if TokenIsIdent(AToken) then begin // no const / var / out, just param name
        Name := AToken.Value;
        Texer.SkipToken;
      end
      else
        raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserExpectedElementsButGot, ['const/var/out or param name', AToken.Value, 'procedure/function ', sContext[AContext] + 'parameter definition']));
    end
    else begin
      if TokenIsIdent(AToken) then begin // no const / var / out, just param name
        Name := AToken.Value;
        Texer.SkipToken;
      end
      else
        raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserExpectedElementsButGot, ['const/var/out or param name', AToken.Value, 'procedure/function ', sContext[AContext] + 'parameter definition']));
    end;
  end
  else if decfParamDeclarationAsTuple in AFlags then begin
    if TokenIsIdent(AToken) then begin // no const / var / out, just param name
      Name := AToken.Value;
      Texer.SkipToken;
    end
    else
      raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserExpectedElementsButGot, ['const/var/out or param name', AToken.Value, 'procedure/function ', sContext[AContext] + 'parameter definition']));
  end;

  SkipComments;

  if Length(Name) > 0 then
    Texer.ExpectReservedSymbol(rs_Colon); // ':'

  SkipComments;

  // get type
  TypeRef := ParseTypeReference;
  LastToken := Texer.LastToken;

  // get init value
  Init := Nil;
  next_token := Texer.PeekToken;
  if TokenIsReservedSymbol(next_token, rs_Equal) then begin // '='
    token := next_token;
    Texer.SkipToken;
    SkipComments;
    Init := ParseExpression(token, 0);
    LastToken := Init.EndToken;
  end;

  AToken.Location.SetEndRowCol(LastToken.Location.EndRow, LastToken.Location.EndCol);

  Value := Unassigned;
  VarSym := Nil;

  // if decfDoNotAddSymbol in AFlags then skip symbol creation, we only need parameter name and type for signature comparision
  if not (decfDoNotAddSymbol in AFlags) then begin
    if AContext = pcParam then begin
      case Modifier of
        //pmNone: ;
        pmConst: begin
          if Init <> Nil then begin
            if Init is TNPC_ASTExpressionNumber then begin
              case TNPC_ASTExpressionNumber(Init).LiteralType.&Type of
                AST_UNKNOWN: Value := StrToIntDef(TNPC_ASTExpressionNumber(Init).Value, -1);
              end;
            end
            else if Init is TNPC_ASTExpressionString then begin // allow string
              Value := TNPC_ASTExpressionString(Init).Value;
            end
            else
              raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserExpectedElementsButGot, ['number or string', token.Value, 'function', ' return variable']));

            VarSym := TNPCSymbol.Create(AToken.Location, Name, KIND_Param, CurrentScope.ResolveTypeKind(TypeRef), True, TypeRef, CurrentBlock);
            VarSym.ConstValue := Value;
          end
          else
            VarSym := TNPCSymbol.Create(AToken.Location, Name, KIND_Param, CurrentScope.ResolveTypeKind(TypeRef), True, TypeRef, CurrentBlock);
        end;
        //pmVar: ;
        //pmOut: ;
      else
        VarSym := TNPCSymbol.Create(AToken.Location, Name, KIND_Param, CurrentScope.ResolveTypeKind(TypeRef), False, TypeRef, CurrentBlock);
      end;

      if VarSym <> Nil then begin
        CurrentScope.DefineSymbol(VarSym);
        VarSym.Size := ResolveTypeSize(TypeRef);
      end;
    end
    else begin // RETURN VALUES
      if Length(Name) = 0 then
        Name := AProcedureName;

      VarSym := TNPCSymbol.Create(TypeRef.Location, Name, KIND_Return, CurrentScope.ResolveTypeKind(TypeRef), False, TypeRef, CurrentBlock);
      VarSym.Size := ResolveTypeSize(TypeRef);

      // optional: attach to function scope later
      CurrentScope.DefineSymbol(VarSym);
    end;
  end;

  Result := TNPC_ASTParameter.Create(AToken.Location, Name, TypeRef, Modifier, Init);
end;

function TNPCSourceParser.ParseProcedureSignatureOnly(const AParent: TNPC_ASTStatementBlock; const AName: String): TNPC_ASTStatementProcedure;
begin
  Result := TNPC_ASTStatementProcedure.Create(Texer.LastToken.Location, AParent, AName, False, [BLOCK_DescribesOnlyProcedureSignature]);

  Texer.ExpectReservedSymbol(rs_OParen);

  // ( ... ): ident / ()
  ParseProcedureDefinitionHeader(Result, [decfDoNotAddSymbol, decfParamDeclarationAsTuple, decfParamMayContainModifier]);
end;

//  token := Texer.GetToken;
//  if (not TokenIsIdent(token) and not TokenIsReservedIdent(token)) or TokenIsLiteral(token) then
//    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnknownTypeFor, [token.Value, 'procedure/function ', sContext[AContext] + 'parameter definition']));
//  LastToken := token;
//
//  TypeSym := CurrentScope.ResolveSymbol(token.Value);
//  if not Assigned(TypeSym) then
//    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnknownIdentIn, [token.Value, sContext[AContext], 'parameter definition']));


// assignment = ident [param_list] ":=" expr
//
// param_list = ('(' | '[') ident { ',' ident } (')' | ']')

function TNPCSourceParser.ParseAssignment(const ALeftToken: TNPCToken): TNPC_ASTStatement;
//var
//  assign_type: TNPCToken;
//  exp: TNPC_ASTExpression;
//begin
//  Result := Nil;
//
//  assign_type := Texer.NextToken;
//  // ':=', '+=', '-=', '*=', '/=', '%='
//  if TokenIsOfType(assign_type, [tokAssign]) then begin // ':='
//    Texer.SkipToken; // skip ident
//    Texer.SkipToken; // skip ':='
////    Result := TNPC_ASTAssign.Create;
////    Result.Location := AToken.Location.Copy;
////    TNPC_ASTAssign(Result).Ident := AToken.Value; // ident to assign to; @TODO: check if it exists in declared variables
////    TNPC_ASTAssign(Result).Expression := ParseExpression(Texer.PeekToken); // get everything until ';'
////    Texer.ExpectToken([tokSemicolon], True);
//  end
//  else if TokenIsOfType(assign_type, [tokPlusEqual]) then begin // '+='
//    Texer.SkipToken;
//  end
//  else if TokenIsOfType(assign_type, [tokMinusEqual]) then begin // '-='
//    Texer.SkipToken;
//  end
//  else if TokenIsOfType(assign_type, [tokMulEqual]) then begin // '*='
//    Texer.SkipToken;
//  end
//  else if TokenIsOfType(assign_type, [tokDivEqual]) then begin // '/='
//    Texer.SkipToken;
//  end
//  else if TokenIsOfType(assign_type, [tokModEqual]) then begin // '%='
//    Texer.SkipToken;
//  end
//  else begin
//    Result := ParsePrimaryExpression(AToken);
//  end;
//end;
var
  token: TNPCToken;
  Left: TNPC_ASTExpression;
  Right: TNPC_ASTExpression;
  Sym: TNPCSymbol;
begin
//      token := Texer.PeekToken;
//      rhs := ParseExpression(token, 0);
//      Texer.ExpectToken([tokSemicolon]);
//    end

  Left := ParseExpression(ALeftToken, 0);
  // Ensure Left is variable
  if not (Left is TNPC_ASTExpressionVariable) then
    raise NPCSyntaxError.ParserError(ALeftToken.Location, Format(sParserLeftSideOfMustBe, ['assignment', 'variable']));

  Sym := CurrentScope.ResolveSymbol(TNPC_ASTExpressionVariable(Left).Name);
  if not Assigned(Sym) then
//    raise Exception.Create('Unknown variable: ' + TExprVariable(Left).Name);
    raise NPCSyntaxError.ParserError(TNPC_ASTExpressionVariable(Left).Location, Format(sParserUnknown, ['variable', TNPC_ASTExpressionVariable(Left).Name]));

  Texer.ExpectReservedSymbol(rs_Assign);

  token := Texer.PeekToken;
  Right := ParseExpression(token);

  // Special handling for empty set
  if (Right is TNPC_ASTExpressionSetLiteral) and (TNPC_ASTExpressionSetLiteral(Right).SetType = Nil) then begin
    if not (Sym.TypeRef is TNPC_ASTTypeSet) then
//      raise Exception.Create('Empty set assignment requires a set variable');
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserTypeMismatchInLiteral, ['set assign statement', '<set>', Left.ToString]));
    TNPC_ASTExpressionSetLiteral(Right).SetType := TNPC_ASTTypeSet(Sym.TypeRef);
  end;

//  Result := TStmtAssign.Create(Left, Right));
  Result := TNPC_ASTStatementAssign.Create(ALeftToken.Location, Left, Right);
end;

// assignment := identifier (',' identifier)* ':=' expression
function TNPCSourceParser.ParseAssignmentNew(const AToken: TNPCToken): TNPC_ASTStatement;
var
  token, next_token: TNPCToken;
//  name: String;
//  stmt: TNPC_ASTStatement;
  targets: TObjectList<TNPC_ASTExpression>;
  target: TNPC_ASTExpression;
  rhs: TNPC_ASTExpression;
//  expr: TNPC_ASTExpression;
//  block: TNPC_ASTStatementBlock;
begin
  targets := Nil;
  // expression or assignment statement
  target := ParseExpression(AToken, 0);
  if not (target is TNPC_ASTExpressionIdent) then
    raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserLeftSideOfMustBe, ['assignment', 'identifier']));

  token := Texer.PeekToken;
  if TokenIsReservedSymbol(token, rs_Comma) then begin // ',' - multi-assign = tuple
    Texer.SkipToken;
    targets := TObjectList<TNPC_ASTExpression>.Create(True);
    targets.Add(target);

    token := Texer.PeekToken;
    repeat
      target := ParseExpression(token, 0);
      if not (target is TNPC_ASTExpressionIdent) then
        raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserLeftSideOfMustBe, ['assignment', 'identifier']));
      targets.Add(target);
      //
      token := Texer.PeekToken;
    until not TokenIsReservedSymbol(token, rs_Comma);

    // if next token is assign ':=' -> assignment
    Texer.ExpectReservedSymbol(rs_Assign);

//    if TokenIsReservedSymbol(token, rs_Assign) then begin
      // left must be an lvalue; we keep it as expression for now
      //Texer.SkipToken; // consume :=
      token := Texer.PeekToken;
      rhs := ParseExpression(token, 0);
      Texer.ExpectToken([tokSemicolon]);
      Result := TNPC_ASTStatementMultiAssign.Create(AToken.Location, targets, rhs);
//    end
//    else begin
      // expression statement
//      Texer.ExpectToken([tokSemicolon]);
//      Result := TNPC_ASTStatementExpression.Create(AToken.Location, expr);
//      raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserExpectedButGot, [':=', token.Value]));
//    end;
    Exit;
  end;

  // single-assign
  // if next token is assign ':=' -> assignment
  token := Texer.PeekToken;
  if TokenIsReservedSymbol(token, rs_Assign) then begin
    // left must be an lvalue; we keep it as expression for now
    Texer.SkipToken; // consume :=
    token := Texer.PeekToken;
    rhs := ParseExpression(token, 0);
    Texer.ExpectToken([tokSemicolon]);
    Result := TNPC_ASTStatementAssign.Create(target.Location, target, rhs);
  end
  else begin
    // expression statement
    Texer.ExpectToken([tokSemicolon]);
    Result := TNPC_ASTStatementExpression.Create(target.Location, target);
  end;
end;

function TNPCSourceParser.ParseAssignmentOrMulti: TNPC_ASTExpression;
var
  Targets: TObjectList<TNPC_ASTExpressionVariable>;
  Expr: TNPC_ASTExpression;
begin
  Targets := TObjectList<TNPC_ASTExpressionVariable>.Create(True);

  // first target
  Targets.Add(ParseVariableExpression);

  // detect multiple
  while TokenIsReservedSymbol(Texer.PeekToken, rs_Comma) do begin
    Texer.SkipToken;
    Targets.Add(ParseVariableExpression);
  end;

  // must be :=
  Texer.ExpectReservedSymbol(rs_Assign);

  Expr := ParseExpression(Texer.PeekToken, 0);

  if Targets.Count = 1 then
    Result := TNPC_ASTExpressionAssign.Create(Targets[0].Location, Targets[0], Expr)
  else
    Result := TNPC_ASTExpressionMultiAssign.Create(Targets[0].Location, Targets, Expr);
end;

function TNPCSourceParser.ParseIdentifier(const AToken: TNPCToken): TNPC_ASTExpression;
var
  token: TNPCToken;
  Name: String;
  Sym: TNPCSymbol;
  Arg: TNPC_ASTExpression;
  Value: Integer;
begin
  Name := AToken.Value;
  Texer.SkipToken;

  if SameText(Name, 'ord') then begin
    Texer.ExpectReservedSymbol(rs_OParen); // '('
    token := Texer.PeekToken;
    Arg := ParseExpression(token, 0);
    Texer.ExpectReservedSymbol(rs_CParen); // ')'

    if Arg is TNPC_ASTExpressionEnumConst then
      Result := TNPC_ASTExpressionLiteral.Create(AToken.Location, IntToStr(TNPC_ASTExpressionEnumConst(Arg).Value), Builtin_Type_Integer)
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserIntrinsicFuncExpects, ['ord()', 'enum const', 'input param']));
  end
  else if SameText(Name, 'succ') then begin
    Texer.ExpectReservedSymbol(rs_OParen); // '('
    token := Texer.PeekToken;
    Arg := ParseExpression(token, 0);
    Texer.ExpectReservedSymbol(rs_CParen); // ')'

    if Arg is TNPC_ASTExpressionEnumConst then begin
      Value := TNPC_ASTExpressionEnumConst(Arg).Value + 1;
      Result := TNPC_ASTExpressionEnumConst.Create(AToken.Location, '<succ>', Value, TNPC_ASTExpressionEnumConst(Arg).EnumType);
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserIntrinsicFuncExpects, ['succ()', 'enum const', 'input param']));
  end
  else if SameText(Name, 'pred') then begin
    Texer.ExpectReservedSymbol(rs_OParen); // '('
    token := Texer.PeekToken;
    Arg := ParseExpression(token, 0);
    Texer.ExpectReservedSymbol(rs_CParen); // ')'

    if Arg is TNPC_ASTExpressionEnumConst then begin
      Value := TNPC_ASTExpressionEnumConst(Arg).Value - 1;
      Result := TNPC_ASTExpressionEnumConst.Create(AToken.Location, '<pred>', Value, TNPC_ASTExpressionEnumConst(Arg).EnumType);
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserIntrinsicFuncExpects, ['pred()', 'enum const', 'input param']));
  end
  else begin
//    Result := inheritedParseIdentifier(Name); // your old handling
    Sym := CurrentScope.ResolveSymbol(Name);
    if Assigned(Sym) then begin
      case Sym.Kind of
        //skConst:
        KIND_Var  : begin
          if Sym.IsConst then
            Result := TNPC_ASTExpressionEnumConst.Create(AToken.Location, Sym.Name, Sym.ConstValue, Sym.TypeRef)
          else
            Result := TNPC_ASTExpressionVariable.Create(AToken.Location, Sym.Name, Sym);
        end
      else
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserIdentifierNotValidInExpr, [Sym.Name]));
      end;
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnknownIdentIn, [Name, sExpression, '']));
    Texer.SkipToken;
  end;
end;

function TNPCSourceParser.ParseLiteral(const AToken: TNPCToken): TNPC_ASTExpression;
//function TNPCSourceParser.ParseLiteral(const ALeftToken: TNPCToken; const AToken: TNPCToken): TNPC_ASTExpression;
var
  token: TNPCToken;
  Sym: TNPCSymbol;
begin
  // assume integer literals for now
  case AToken.&Type of
    tokIdent: begin
      Texer.SkipToken;
      // an identifier may be followed by '(' (call) or '[' (index) and will be handled in infix
      Sym := CurrentScope.ResolveSymbol(AToken.Value);
      if not Assigned(Sym) then begin
        token := Texer.PeekToken;
        if TokenIsReservedSymbol(token, rs_Assign) then
          Sym := CurrentScope.DefineVar(AToken.Location, AToken.Value, TYPE_Unresolved, -1, Nil, CurrentBlock) // we don't know what we are declaring for now
        else
          raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserUnknown, ['identifier', AToken.Value]));
      end;

//      if Sym.Kind = skProc then
//        Result := TNPC_ASTExpressionCall.Create(AToken.Location, AToken.Value) //, Sym.TypeRef);
//      else
      Result := TNPC_ASTExpressionIdent.Create(AToken.Location, AToken.Value); //, Sym.TypeRef);
    end;
    tokNumber: begin
      Texer.SkipToken;
      Sym := CurrentScope.ResolveSymbol(AToken.Value);
      if not Assigned(Sym) then
        raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserUnknown, ['variable', AToken.Value]));
      Result := TNPC_ASTExpressionNumber.Create(AToken.Location, AToken.Value, Sym.TypeRef);
    end;
    tokString: begin
      Texer.SkipToken;
//      Sym := CurrentScope.Resolve(ALeftToken.Value);
//      if not Assigned(Sym) then
//        raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserUnknown, ['variable', AToken.Value]));
      Result := TNPC_ASTExpressionLiteral.Create(AToken.Location, AToken.Value, Builtin_Type_String);
    end;
  else
    raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserUnknownIdentIn, [AToken.Value, sExpression, '']));
  end;
end;

function TNPCSourceParser.ParseTupleExpression(const AToken: TNPCToken): TNPC_ASTExpression;
var
  token, next_token: TNPCToken;
  Tuple: TNPC_ASTExpressionTuple;
  Item: TNPC_ASTTupleItem;
  Name: UTF8String;
  Expr: TNPC_ASTExpression;
begin
  Texer.ExpectReservedSymbol(rs_OCurly); // {

  Tuple := TNPC_ASTExpressionTuple.Create(AToken.Location);
//  Tuple.Items := TObjectList<TNPC_ASTTupleItem>.Create(True);

  if Texer.IsEmpty then
    Exit(Tuple);

  repeat
    SkipComments;

    token := Texer.PeekToken;
    next_token := Texer.NextToken;

    Item := TNPC_ASTTupleItem.Create(token.Location);
    Name := '';

    // CASE 1: named value - ok := expr
    if TokenIsIdent(token) and TokenIsReservedSymbol(next_token, rs_Assign) then begin
      Name := token.Value;
      Texer.SkipToken; // ident
      Texer.SkipToken; // :=

      SkipComments;

      Expr := ParseExpression(token, 0);


      Item.Name := Name;
      Item.Expr := Expr;
    end
    else begin // CASE 2: positional value
      Expr := ParseExpression(token, 0);

      Item.Name := ''; // positional
      Item.Expr := Expr;
    end;

    Tuple.Items.Add(Item);

    SkipComments;

    token := Texer.PeekToken;
    if TokenIsReservedSymbol(token, rs_Comma) then
    begin
      Texer.SkipToken;
      SkipComments;
    end
    else
      Break;

  until Texer.IsEmpty;

  Texer.ExpectReservedSymbol(rs_CCurly); // }
  SkipComments;

  Result := Tuple;
end;

function TNPCSourceParser.ParseReturn(const AToken: TNPCToken): TNPC_ASTStatement;
var
  token: TNPCToken;
  Stmt: TNPC_ASTStatementReturn;
  Expr: TNPC_ASTExpression;
begin
  Texer.SkipToken;
  token := Texer.PeekToken;
  Expr := Nil;
  if not TokenIsReservedSymbol(token, rs_Semicolon) then // ';'
    Expr := ParseExpression(token, 0);

  Stmt := TNPC_ASTStatementReturn.Create(AToken.Location, Expr);

  Result := Stmt;
end;

function TNPCSourceParser.ParseResult(const AToken: TNPCToken): TNPC_ASTStatement;
var
  token: TNPCToken;
  Stmt: TNPC_ASTStatementResult;
  Expr: TNPC_ASTExpression;
begin
  Texer.SkipToken;
  token := Texer.PeekToken;
  Expr := ParseExpression(token, 0);
  Texer.ExpectReservedSymbol(rs_Semicolon); // ';'

  Stmt := TNPC_ASTStatementResult.Create(AToken.Location, Expr);

  Result := Stmt;
end;

function TNPCSourceParser.ParseDefer(const AToken: TNPCToken): TNPC_ASTStatement;
var
  token: TNPCToken;
  Expr: TNPC_ASTExpression;
begin
  Texer.SkipToken;
  SkipComments;

  // parse ANY expression after defer
  token := Texer.PeekToken;
  Expr := ParseExpression(token, 0);
  if not (Expr is TNPC_ASTExpressionCall) then
    raise NPCSemanticError.SemanticError(Expr.Location, 'defer expects callable expression');
  Texer.ExpectReservedSymbol(rs_Semicolon); // ';'

  Result := TNPC_ASTStatementDefer.Create(AToken.Location, Expr);
end;

function TNPCSourceParser.ParseSet(const AToken: TNPCToken): TNPC_ASTExpression;
var
  token: TNPCToken;
  Elements: TList<TNPC_ASTExpression>;
  expr: TNPC_ASTExpression;
  SetType: TNPC_ASTTypeSet;
  ElemType: TNPC_ASTType;
  Elem: TNPC_ASTExpression;
  ThisType: TNPC_ASTType;
  Sym: TNPCSymbol;
begin
  Texer.SkipToken; // '['
  Elements := TList<TNPC_ASTExpression>.Create;
  try
    token := Texer.PeekToken;
    // collect all elements of a set, later we will check if they are correct and alowed in the set
    if not TokenIsReservedSymbol(token, rs_CBracket) then begin
      repeat
        expr := ParseExpression(token, 0);
        Elements.Add(expr);
        token := Texer.PeekToken;
      until not TokenIsReservedSymbol(token, rs_Comma);
    end;
    Texer.ExpectReservedSymbol(rs_CBracket);

    SetType := Nil;
    ElemType := Nil;

    if Elements.Count > 0 then begin
      // infer element type from first element
      for Elem in Elements do begin
        ThisType := Nil;

        if Elem is TNPC_ASTExpressionEnumConst then
          ThisType := TNPC_ASTExpressionEnumConst(Elem).EnumType
        else if Elem is TNPC_ASTExpressionLiteral then
          ThisType := TNPC_ASTExpressionLiteral(Elem).LiteralType
        else if Elem is TNPC_ASTExpressionVariable then begin
          Sym := CurrentScope.ResolveSymbol(TNPC_ASTExpressionVariable(Elem).Name);
          if not Assigned(Sym) then
            raise NPCSyntaxError.ParserError(Elem.Location, Format(sParserUnknownIdentIn, [TNPC_ASTExpressionVariable(Elem).Name, 'set declaration ', sStatement]));
          ThisType := Sym.TypeRef;
        end;

        if not Assigned(ThisType) then
          raise NPCSyntaxError.ParserError(Elem.Location, Format(sParserExpectedElementsButGot, ['literal', TNPC_ASTExpressionVariable(Elem).Name, 'set declaration ', sStatement]));

        if not Assigned(ElemType) then
          ElemType := ThisType
        else if ElemType <> ThisType then
          raise NPCSyntaxError.ParserError(Elem.Location, Format(sParserTypeMismatchInLiteral, ['set literal', ElemType.ToString, ThisType.ToString]));
      end;

      SetType := TNPC_ASTTypeSet.Create(AToken.Location, ElemType);
    end;

    // For empty set, SetType stays nil (to be filled later from context)
//    var SetLit := TExprSetLiteral.Create(SetType);
//    for var E in Elements do
//      SetLit.Elements.Add(E);
    if SetType <> Nil then begin
      for Elem in Elements do
        SetType.Elements.Add(Elem);
    end;
  finally
    Elements.Free;
  end;

  Result := TNPC_ASTExpressionSet.Create(AToken.Location, SetType);
  if SetType <> Nil then begin
    for Elem in SetType.Elements do
      TNPC_ASTExpressionSet(Result).Elements.Add(Elem);
  end;
end;

function TNPCSourceParser.ParseIndex(const AToken: TNPCToken; const ALeft: TNPC_ASTExpression): TNPC_ASTExpression;
var
  IndexExpr: TNPC_ASTExpression;
  VarSym: TNPCSymbol;
  ArrType: TNPC_ASTTypeArray;
  array_ident:  UTF8String;
begin
//        // indexing: [expr]
//        Advance;
//        idx := TIndexExpr.Create(Left, ParseExpression(0));
//        ExpectKind(tkRBracket);
//        Result := idx;

  Texer.SkipToken;

  if not (ALeft is TNPC_ASTExpressionVariable) then
    raise NPCSyntaxError.ParserError(ALeft.Location, Format(sParserCantAccessFieldOnNonVar, [AToken.Value, ALeft.ToString]));

  array_ident := TNPC_ASTExpressionVariable(ALeft).Name;

  VarSym := CurrentScope.ResolveSymbol(array_ident);
  if not Assigned(VarSym) or not (VarSym.TypeRef is TNPC_ASTTypeArray) then
    raise NPCSyntaxError.ParserError(ALeft.Location, Format(sParserTypeRequiredForIdent, ['array', array_ident, TNPC_ASTExpressionVariable(ALeft).ToString]));

  IndexExpr := ParseExpression(AToken, 0);
  Texer.ExpectReservedSymbol(rs_CBracket);

  ArrType := TNPC_ASTTypeArray(VarSym.TypeRef);
  Result := TNPC_ASTExpressionArray.Create(ALeft.Location, ALeft, IndexExpr, ArrType.ElementType);
end;

function TNPCSourceParser.ParseRecordMember(const AToken: TNPCToken; const ALeft: TNPC_ASTExpression): TNPC_ASTExpression;
var
  token: TNPCToken;
  Sym: TNPCSymbol;
  RecType: TNPC_ASTTypeRecord;
  record_ident:  UTF8String;
  Field: UTF8String;
  FieldType: TNPC_ASTType;
begin
  Texer.SkipToken;
  token := Texer.ExpectToken(tokIdent, Format(sParserExpectedNameAfter, ['identifier', 'record member', ''])); // expect identifier
  Field := token.Value;

  if not (ALeft is TNPC_ASTExpressionIdent) then
    raise NPCSyntaxError.ParserError(ALeft.Location, Format(sParserCantAccessFieldOnNonVar, [Field, ALeft.ToString]));

  record_ident := TNPC_ASTExpressionIdent(ALeft).Name;

  Sym := CurrentScope.ResolveSymbol(record_ident);
  if not Assigned(Sym) or not (Sym.TypeRef is TNPC_ASTTypeRecord) then
    raise NPCSyntaxError.ParserError(ALeft.Location, Format(sParserTypeRequiredForIdent, ['record', record_ident, TNPC_ASTExpressionVariable(ALeft).ToString]));

  AToken.Location.SetEndRowCol(token.Location.EndRow, token.Location.EndCol);

  RecType := TNPC_ASTTypeRecord(Sym.TypeRef);
  if not RecType.Fields.ContainsKey(Field) then
    raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserFieldNotFoundInRecord, [Field, record_ident]));

  FieldType := RecType.Fields[Field];
  Result := TNPC_ASTExpressionMember.Create(ALeft.Location, ALeft, Field, Sym, FieldType);
end;

function TNPCSourceParser.ParseClassMember(const AToken: TNPCToken; const ALeft: TNPC_ASTExpression): TNPC_ASTExpression;
var
  token: TNPCToken;
  Sym: TNPCSymbol;
  ClassType: TNPC_ASTTypeClass;
  class_ident:  UTF8String;
  Member: UTF8String;
  TypeDef: TNPC_ASTTypeDefinition;
//  Field: TNPC_ASTType;
//  Method: TNPC_ASTTypeClassMethod;
//  Propert: TNPC_ASTTypeClassProperty;
  MemberSymbol: TNPCSymbol;
  MemberType: TNPC_ASTType;
begin
  Texer.SkipToken;
  token := Texer.ExpectToken(tokIdent, Format(sParserExpectedNameAfter, ['identifier', 'class member', ''])); // expect identifier
  Member := token.Value;

  if not (ALeft is TNPC_ASTExpressionIdent) then
    raise NPCSyntaxError.ParserError(ALeft.Location, Format(sParserCantAccessFieldOnNonVar, [Member, ALeft.ToString]));

  class_ident := TNPC_ASTExpressionIdent(ALeft).Name;

  Sym := CurrentScope.ResolveSymbol(class_ident);
  if not Assigned(Sym) or not ((Sym.TypeRef is TNPC_ASTTypeDefinition) and (Sym.Kind = KIND_Type) and (Sym.&Type = TYPE_Class)) then
    raise NPCSyntaxError.ParserError(ALeft.Location, Format(sParserTypeRequiredForIdent, ['class', class_ident, TNPC_ASTExpressionVariable(ALeft).ToString]));

  AToken.Location.SetEndRowCol(token.Location.EndRow, token.Location.EndCol);

  TypeDef := TNPC_ASTTypeDefinition(Sym.TypeRef);
  Assert(TypeDef.DefinitionType = DEF_Class);
  ClassType := TNPC_ASTTypeClass(TypeDef.ClassDescription);

  // find member
  if Assigned(ClassType.Fields) and ClassType.Fields.ContainsKey(Member) then begin
    MemberType := ClassType.Fields[Member];
    Result := TNPC_ASTExpressionMember.Create(ALeft.Location, ALeft, Member, Sym, MemberType);
  end
  else if Assigned(ClassType.Methods) and ClassContainsMethod(ClassType, Member, MemberType, MemberSymbol) then begin
    Result := TNPC_ASTExpressionMember.Create(ALeft.Location, ALeft, Member, Sym, MemberType);
  end
  else if Assigned(ClassType.Properties) and ClassContainsProperty(ClassType, Member, MemberType, MemberSymbol) then begin
    Result := TNPC_ASTExpressionMember.Create(ALeft.Location, ALeft, Member, Sym, MemberType);
  end
  else
    raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserFieldNotFoundInRecord, [Member, class_ident]));
end;

function TNPCSourceParser.ParseIfExpression(const AToken: TNPCToken): TNPC_ASTExpressionIf;
begin
//  if not IsBoolean(Expr.Condition.Typ) then
//    Error('Condition in if-expression must be Boolean');
//  Expr.Typ := ResolveCommonType(Expr.ThenExpr.Typ, Expr.ElseExpr.Typ);


end;

// if_stmt    = 'if' expr 'then' stmt ['else' stmt] ';' .
//
// expr        = simp_expr [ ('<' | '<=' | '=' | '>' | '>=' | ('<>' | '!=')) simp_expr ] [ ';' ] .
//
// simp_expr   = term { ('+' | '-' | 'or') term } .
//
// term        = exp { ('*' | '/' | 'div' | 'mod' | 'and') factor } .
//
// exp         = factor { [ '^' ] exp } .
//
// call_params = '(' expr { ',' expr } ')' .
//
// call       = ident [ call_params ] .
//
// factor     = number | ident | call | '(' expr ')' | ('+' | '-') factor .
//
// stmt       = ident ['(' params ')'] | ident ':=' (expr | string | '%' ident '%') | if_stmt | cmpnd_stmt .
//
// params     = ident ['(' params ')'] | number | string | { ',' params } .
//
// cmpnd_stmt = ('begin'| '{') [stmt ';' {stmt ';'}] ('end' | '}') [';'] .

//var
//  cond: TNPC_ASTExpression;
//  thenS, elseS: TStmt;
//begin
//  ExpectKind(tkIf);
//  cond := ParseExpression(0);
//  ExpectKind(tkThen);
//  thenS := ParseStatement;
//  elseS := nil;
//  if TokenIs(tkElse) then
//  begin
//    Advance;
//    elseS := ParseStatement;
//  end;
//  Result := TIfStmt.Create(cond, thenS, elseS);
//end;

function TNPCSourceParser.ParseIfStatement(const AToken: TNPCToken): TNPC_ASTStatementIf;
var
  token: TNPCToken;
  cond: TNPC_ASTExpression;
  then_block: TNPC_ASTStatement;
  else_block: TNPC_ASTStatement;
begin
//  AToken := Texer.PeekToken;

  Texer.SkipToken; // skip 'if' keyword
  if Texer.IsEmpty then
    Exit;

  token := Texer.PeekToken;
  cond := ParseExpression(token); // get everything until 'then'
  Texer.ExpectReservedToken(ri_then);
  then_block := ParseStatement([stafStartNewScope, stafEmptyStatementIsAcceptable]);
  //
  SkipComments;
  token := Texer.PeekToken;
  if TokenIsReservedIdent(token, ri_else) then begin
    if (Texer.LastToken <> Nil) and TokenIsReservedSymbol(Texer.LastToken, rs_Semicolon) then
      raise NPCSyntaxError.ParserError(Texer.LastToken.Location, Format(sParserUnexpectedTokenTypeIn, [Texer.LastToken.TokenToString, Texer.LastToken.Value, 'if ', sStatement]));
//    AddToken(token);
    Texer.SkipToken;
    SkipComments;
//    token := Texer.PeekToken;
//    if TokenIsReservedIdent(token, ri_if) then begin
//      Result := ParseIf(token);
//      Exit;
//    end;
    else_block := ParseStatement([stafStartNewScope, stafEmptyStatementIsAcceptable]);
  end;
  if (token <> Nil) and not TokenIsReservedSymbol(token, rs_Semicolon) then
//    raise NPCSyntaxError.ParserError(LastToken.Location, Format(sParserUnexpectedTokenIn, [LastToken.TokenToString, 'if ', sStatement]));
    raise NPCSyntaxError.ParserError(Texer.LastToken.Location.After, Format(sParserExpectedButGot, [NPCReservedSymbolToString(rs_Semicolon), token.TokenToString]));
  Texer.ExpectToken([tokSemicolon]);
  //
  Result := TNPC_ASTStatementIf.Create(AToken.Location, cond, then_block, else_block);
//  Result.Block := ABlock;
end;

//  // case         = 'case' expression 'of' case_element { ';' [ 'else' $NOREAD | 'end' $NOREAD | case_element ] } [ 'else' instruction_list ] 'end' [ ';' ] .
//  //
//  // case_element = case_label ':' instruction .
//  //
//  // case_label   = constant_expression { ( ',' constant_expression | '..' constant_expression ) } .


// CaseStatement = [ 'case' SelectorExpression 'of' CaseElement ';' { CaseElement ';' } [ 'else' StatementList ';' ] 'end' |
//                   'case' SelectorExpression '{'  CaseElement ';' { CaseElement ';' } [ 'else' StatementList ';' ] '}' ] .
//
// SelectorExpression = Expresion |
//                      .
//
// CaseElement = 'if' CaseLabel { ',' CaseLabel } ':' [ '{@next' [ ':' CaseElement ] '}' ] Statement .
//
// CaseLabel = ConstExpression [ '..' ConstExpression ] .
//
// ConstExpression = Expression .

function TNPCSourceParser.ParseCaseExpression(const AToken: TNPCToken): TNPC_ASTExpressionCase;
var
//  AToken: TNPCToken;
  token: TNPCToken;
  case_selector: TNPC_ASTExpression;
  case_of: Boolean;
  case_branches: TNPC_ASTExpressionCaseBranches;
  case_default: TNPC_ASTExpression;
begin
//  AToken := Texer.PeekToken;
  Texer.SkipToken;
  token := Texer.PeekToken;
  case_selector := ParseExpression(token, 0); // get everything until 'of' or '{'
  //
  token := Texer.GetToken;
  case_of := TokenIsReservedIdent(token, ri_of); // and not TokenIsReservedSymbol(token, rs_OCurly);
  if not TokenIsReservedIdent(token, ri_of) and not TokenIsReservedSymbol(token, rs_OCurly) then
    raise NPCSyntaxError.ParserError(Texer.LastToken.Location, Format(sParserUnexpectedTokenTypeIn, [Texer.LastToken.TokenToString, Texer.LastToken.Value, 'case ', sStatement]));
  // parse if branches
  case_branches := ParseCaseExpressionBranches(case_selector, case_of, case_default);
  //
  token := Texer.GetToken;
  if (case_of and not TokenIsReservedIdent(token, ri_end)) or (not case_of and not TokenIsReservedSymbol(token, rs_CCurly)) then
    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, 'case ', sStatement]));
  //
  token := Texer.GetToken;
  if not TokenIsReservedSymbol(token, rs_Semicolon) then
//    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'case ', sStatement]));
    raise NPCSyntaxError.ParserError(Texer.LastToken.Location.After, Format(sParserExpectedButGot, [NPCReservedSymbolToString(rs_Semicolon), token.TokenToString]));

  Result := TNPC_ASTExpressionCase.Create(AToken.Location, case_selector, case_branches, case_default);
end;

// CaseElement = 'if' CaseLabel { ',' CaseLabel } ':' [ '{@next' [ ':' CaseElement ] '}' ] Statement .
//
// CaseLabel = ConstExpression [ '..' ConstExpression ] .
//
// ConstExpression = Expression .

function TNPCSourceParser.ParseCaseExpressionBranches(const ASelector: TNPC_ASTExpression; const ACaseOf: Boolean; out ADefaultBranch: TNPC_ASTExpression): TNPC_ASTExpressionCaseBranches;
type
  // Ordinal types are the predefined types Integer, Char, WideChar, Boolean, and declared enumerated types
  TCaseSelectorType = (
    cstUnknown,
    cstLiteral, // ident, number, string, char
    cstEnum, // enum1, enum2, enum3, ...
    cstSet, // se1..setN
    cstType, // selector is type          // @not_supported: add TNPC_ASTExpressionType definition
    cstPointer // pointer, class pointer  // @not_supported: add TNPC_ASTExpressionPointer definition
  );

var
  token: TNPCToken;
  Branch: TNPC_ASTExpressionCaseBranch;
  expr: TNPC_ASTExpression;
  selector_type: TCaseSelectorType;
  branch_type: TCaseSelectorType;
begin
  ADefaultBranch := Nil;

  selector_type := cstUnknown;
  if ASelector is TNPC_ASTExpressionLiteral then
    selector_type := cstLiteral
  else if ASelector is TNPC_ASTExpressionIdent then
    selector_type := cstLiteral
  else if ASelector is TNPC_ASTExpressionNumber then
    selector_type := cstLiteral
  else if ASelector is TNPC_ASTExpressionString then
    selector_type := cstLiteral
  else if ASelector is TNPC_ASTExpressionEnumConst then
    selector_type := cstEnum
  else if ASelector is TNPC_ASTExpressionSet then
    selector_type := cstSet;

  if selector_type = cstUnknown then
    raise NPCSyntaxError.ParserError(ASelector.Location, Format(sParserExpectedElementsButGot, ['selector to be one of {<literal>, <enum>, <set>, <type>, <pointer>}', ASelector.ToString, 'case statement', '']));

  Result := TNPC_ASTExpressionCaseBranches.Create(True);

  token := Texer.PeekToken;
  while not (ACaseOf and TokenIsReservedIdent(token, ri_end)) and not TokenIsReservedIdent(token, ri_else) do begin
    token := Texer.ExpectReservedToken(ri_if);
    Branch := TNPC_ASTExpressionCaseBranch.Create(token.Location);
    repeat
      token := Texer.PeekToken;
      expr := ParseExpression(token, 0);
      branch_type := cstUnknown;
      if ASelector is TNPC_ASTExpressionLiteral then
        branch_type := cstLiteral
      else if ASelector is TNPC_ASTExpressionIdent then
        branch_type := cstLiteral
      else if ASelector is TNPC_ASTExpressionNumber then
        branch_type := cstLiteral
      else if ASelector is TNPC_ASTExpressionString then
        branch_type := cstLiteral
      else if ASelector is TNPC_ASTExpressionEnumConst then
        branch_type := cstEnum
      else if ASelector is TNPC_ASTExpressionSet then
        branch_type := cstSet;

      if branch_type <> selector_type then
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserExpectedElementsButGot, ['case if to be same type as selector "' + ASelector.ToString + '"', expr.ToString, 'case statement', '']));

      Branch.IfValues.Add(expr); // constant values
      token := Texer.PeekToken;
    until not TokenIsReservedSymbol(token, rs_Comma);
    Texer.ExpectReservedSymbol(rs_Colon); // ':'
    //
    token := Texer.PeekToken;
    Branch.ResultExpr := ParseExpression(token, 0);
    Result.Add(Branch);

    token := Texer.PeekToken;
    if not TokenIsReservedSymbol(token, rs_Semicolon) then // ';'
      Break;
  end;

  // optional ELSE
  token := Texer.PeekToken;
  if TokenIsReservedIdent(token, ri_else) then begin
    Texer.SkipToken;
    token := Texer.PeekToken;
    ADefaultBranch := ParseExpression(token, 0);
  end;

  if ACaseOf then
    Texer.ExpectReservedToken(ri_end) // 'end'
  else
    Texer.ExpectReservedSymbol(rs_CCurly); // '}'
  Texer.ExpectReservedSymbol(rs_Semicolon); // ';'
end;

function TNPCSourceParser.ParseCaseStatement(const AToken: TNPCToken): TNPC_ASTStatementCase;
begin

end;
//var
//  CaseStmt: TStmtCase;
//  Branch: TCaseBranchStmt;
//begin
//  Expect(tkCase);
//
//  CaseStmt := TStmtCase.Create;
//  CaseStmt.Selector := ParseExpression(0);
//  Expect(tkOf);
//
//  while not Check(tkEnd) and not Check(tkElse) do
//  begin
//    Branch := TCaseBranchStmt.Create;
//
//    // one or more constant values
//    repeat
//      Branch.Values.Add(ParseExpression(0));
//    until not Match(tkComma);
//
//    Expect(tkColon);
//    Branch.Stmt := ParseStatement; // <---- difference here
//    CaseStmt.Branches.Add(Branch);
//
//    Match(tkSemicolon); // optional
//  end;
//
//  if Match(tkElse) then
//    CaseStmt.ElseStmt := ParseStatement;
//
//  Expect(tkEnd);
//  Result := CaseStmt;
//end;

function TNPCSourceParser.ParseCaseStatementBranches(const ASelector: TNPC_ASTExpression; const ACaseOf: Boolean; out ADefaultBranch: TNPC_ASTStatement): TNPC_ASTStatementCaseBranches;
begin

end;

// for_loop   = 'for' loop_cntrl ( 'do' statement ';' | '{' statement ';' [ statement ';' ] '}' ) .
//
// loop_cntrl = [ loop_init ';' ] [ loop_cond ';' ] [ loop_incr ] .
//
// loop_init  = ( null_init | init_loop_assignment ) .
// null_init  = .
// loop_cond  = ( null_cond | expr ) .
// null_cond  = .
// loop_incr  = ( null_incr | assignment ) .
// null_incr  = .
//
// init_loop_assignment = ( 'var' loop_assignment | loop_assignment ) { ',' loop_assignment } .
//
// loop_assignment = ident ':=' expr .


// for var i := 0; i < 10; i += 1 {
// for i := 0; i < 10; i += 1 {
// for var i := 0, j := 0; i < 10; i += 1, j += 2 {
//
// decalaration on variable before for loop
// var i: Int;
// i := 0;
//
// for ; i < 10; i += 1 {
// for ; i < 10; {
// for i < 10 {
// for {
//
// range loop
// for i in 0..10 {
//
// for item value in
// for i in String/Array/Slice/Set/Map {
//
// for item value, index in
// for value, idx in String/Array/Slice/Set/Map {

function TNPCSourceParser.ParseFor(const AToken: TNPCToken): TNPC_ASTStatementFor;
//var
//  AToken: TNPCToken;
//  token: TNPCToken;
//begin
//  AToken := Texer.PeekToken;
////  AddToken(AToken); // 'for'
//  Texer.SkipToken;
//  ParseForParams; // get everything until 'do' or '{'
//  token := Texer.GetToken;
//  for_do := TokenIsReservedIdent(token, ri_do); // and not TokenIsReservedSymbol(token, rs_OCurly);
//  if not TokenIsReservedIdent(token, ri_of) and not TokenIsReservedSymbol(token, rs_OCurly) then
//    raise NPCSyntaxError.ParserError(Texer.LastToken.Location, Format(sParserUnexpectedTokenIn, [Texer.LastToken.TokenToString, 'for ', sStatement]));
////  AddToken(token);
////  ParseForStatements(for_do);
//  //
//  token := Texer.GetToken;
//  if (for_do and not TokenIsReservedIdent(token, ri_end)) or (not for_do and not TokenIsReservedSymbol(token, rs_CCurly)) then
//    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'for ', sStatement]));
////  AddToken(token);
//  token := Texer.GetToken;
//  if not TokenIsReservedSymbol(token, rs_Semicolon) then
//    raise NPCSyntaxError.ParserError(Texer.LastToken.Location.After, Format(sParserExpectedButGot, [NPCReservedSymbolToString(rs_Semicolon), token.TokenToString]));
////  AddToken(Texer.ExpectToken([tokSemicolon]));
////  AddToken(token);
//end;
var
  // This is a simplified for ... to/downto ... do parser that expects: for i := expr to expr do stmt
  token: TNPCToken;
  sect_init: TNPC_ASTStatement;
  sect_cond: TNPC_ASTExpression;
  sect_post: TNPC_ASTExpression;
  body: TNPC_ASTStatement;
  for_do: Boolean;

//  varName: string;
//  assignTok: TToken;
//  startExpr, endExpr: TNPC_ASTExpression;
//  body: TStmt;
//  dummyAssign: TAssignStmt;
//  beginStmt: TBlockStmt;
begin
  Texer.SkipToken; // skip 'for'
  SkipComments;
  // determine if there is 3 sections of parameters
  sect_init := Nil;
  sect_cond := Nil;
  sect_post := Nil;
  body := Nil;

  //
  if Texer.IsNotEmpty then begin
    token := Texer.PeekToken;
    // check if semicolon was first
    if not TokenIsReservedSymbol(token, rs_Semicolon) then begin // loop_init ';'
      sect_init := ParseStatement([stafStatementIsRequired]);
      SkipComments;
      token := Texer.PeekToken;
    end
    else begin // ';'
      Texer.SkipToken;
      SkipComments;
      token := Texer.PeekToken;
    end;
    // check if semicolon was second
    if not TokenIsReservedSymbol(token, rs_Semicolon) then begin // loop_cond ';'
      sect_cond := ParseExpression(token, 0);
      SkipComments;
      token := Texer.PeekToken;
    end
    else begin // ';'
      Texer.SkipToken;
      SkipComments;
      token := Texer.PeekToken;
    end;
    sect_post := ParseExpression(token, 0); // loop_incr
    SkipComments;
    token := Texer.PeekToken;
    if TokenIsReservedIdent(token, ri_do) or TokenIsReservedSymbol(token, rs_OCurly) then begin
      for_do := TokenIsReservedIdent(token, ri_do); // and not TokenIsReservedSymbol(token, rs_OCurly);
      body := ParseStatement([stafStartNewScope, stafEmptyStatementIsAcceptable]);
      if not for_do then
        Texer.ExpectReservedSymbol(rs_CCurly);
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, 'for ', sStatement]));

  Result := TNPC_ASTStatementFor.Create(AToken.Location, sect_init, sect_cond, sect_post, False, body);

//    if TokenIsReservedIdent(token, ri_var) or
//       (not token.ReservedWord and not token.ReservedSymbol and (token.&Type in [tokIdent..tokChar])) or
//       (token.ReservedSymbol and (token.&Type in [tokMinus..tokDiv, tokDoubleDot..tokModEqual])) then begin
////      AddToken(token);
//    end
//    else if TokenIsReservedSymbol(token, rs_Semicolon) then begin
////      AddToken(token);
//      Inc(sect_cnt);
//    end
//    if TokenIsReservedIdent(token, ri_do) or TokenIsReservedSymbol(token, rs_OCurly) then
//      Exit
  end;

//  token := Texer.PeekToken;
//  if FToken.Kind <> tkIdentifier then
//    raise Exception.Create('Expected identifier after for');
//  varName := FToken.Text;
//  Advance;
//  Eat(tkAssign);
//  startExpr := ParseExpression(0);
//  if FToken.Kind = tkTo then Advance
//  else if FToken.Kind = tkDownto then Advance
//  else raise Exception.Create('Expected to or downto in for');
//  endExpr := ParseExpression(0);
//  Eat(tkDo);
//  body := ParseStatement;
//  // represent as a block: var i := start; while i <= end do begin body; inc(i); end
//  beginStmt := TBlockStmt.Create;
//  beginStmt.Stmts.Add(TVarDeclStmt.Create(varName, startExpr));
//  // condition expression:
//  var cond: TNPC_ASTExpression := TBinaryExpr.Create(TIdentExpr.Create(varName), '<=', endExpr);
//  var whileBody := TBlockStmt.Create;
//  whileBody.Stmts.Add(body);
//  // inc: i := i + 1
//  var incExpr := TBinaryExpr.Create(TIdentExpr.Create(varName), '+', TNumberExpr.Create('1'));
//  whileBody.Stmts.Add(TAssignStmt.Create(TIdentExpr.Create(varName), incExpr));
//  beginStmt.Stmts.Add(TWhileStmt.Create(cond, whileBody));
//  Result := beginStmt;
end;

function TNPCSourceParser.ParseWhile(const AToken: TNPCToken): TNPC_ASTStatementWhile;
//var
//  cond: TNPC_ASTExpression;
//  body: TStmt;
begin
//  ExpectKind(tkWhile);
//  cond := ParseExpression(0);
//  ExpectKind(tkDo);
//  body := ParseStatement;
//  Result := TWhileStmt.Create(cond, body);
end;

function TNPCSourceParser.ParseCall(const AToken: TNPCToken; const ALeft: TNPC_ASTExpression): TNPC_ASTExpressionCall;
var
  token: TNPCToken;
  right: TNPC_ASTExpression;
begin
  Texer.SkipToken;
  // call: Left is callee
  Result := TNPC_ASTExpressionCall.Create(ALeft.Location, ALeft);
  token := Texer.PeekToken;
  if Texer.IsNotEmpty and not TokenIsReservedSymbol(token, rs_CParen) then begin // ')'
    while not TokenIsReservedSymbol(token, rs_CParen) do begin
      right := ParseExpression(token, 0);
      if Assigned(right) then
        Result.Args.Add(right);
      token := Texer.PeekToken;
      if TokenIsReservedSymbol(token, rs_Comma) then begin
        Texer.SkipToken;
        token := Texer.PeekToken;
      end
      else
        Break;
    end;
  end;
  Texer.ExpectReservedSymbol(rs_CParen); // ')'
end;

procedure TNPCSourceParser.ParseCallParams(const AToken: TNPCToken);
var
  token: TNPCToken;
begin
//  AddToken(AToken); // '('
  Texer.SkipToken;
  //
  while Texer.IsNotEmpty do begin
    SkipComments;
    token := Texer.PeekToken;
    if not token.ReservedWord and not token.ReservedSymbol and (token.&Type in [tokIdent..tokChar]) then begin
//      AddToken(token);
      Texer.SkipToken; // move forward
      Continue;
    end
    else if TokenIsReservedIdent(token) then begin
      if TokenIsReservedIdent(token, ri_asm) then begin
        //IncLevel;
      end
      else if TokenIsReservedIdent(token, ri_case) then begin
        //IncLevel;
//        ParseCase(token);
        Continue;
      end
      else if TokenIsReservedIdent(token, ri_if) then begin
        //IncLevel;
//        ParseIf(token);
        Continue;
      end
      else if TokenIsReservedIdent(token, ri_inherited) then begin
      end
      else
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, 'call params ', sSection]));
    end
    else if TokenIsReservedSymbol(token, rs_CParen) then begin
//      AddToken(token);
      Texer.SkipToken; // move forward
      token := Texer.PeekToken; // just peek a token
      if TokenIsReservedSymbol(token, rs_Semicolon) then
//        AddToken(Texer.GetToken);
        Texer.SkipToken; // move forward
      Break;
    end
//    else
//      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sStatement]));
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, 'call params ', sStatement]));
    //
    Texer.SkipToken;
  end;
end;

function SearchInFiles(const Parser: TNPCSourceParser; Path: String; const FileName: String; const Settings: TNPCProjectSettings; var FoundInPath: String; const SearchForText: String): Boolean;
var
  SearchRec: TSearchRec;
  iResult: Integer;
  ext: String;
begin
  Result := False;
  if Path[Length(Path)] <> '\' then
    Path := Path + '\';
  if SysUtils.FindFirst(Path + FileName, faAnyFile - faDirectory, SearchRec) = 0 then begin
    repeat
      if (SearchRec.Attr and faDirectory = 0) and (SearchRec.Size > 0) then begin
        ext := ExtractFileExt(SearchRec.Name);
        Result:=not ((ext = '.exe') or
                     (ext = '.bat') or
                     (ext = '.manifest') or
                     (ext = '.bpl') or
                     (ext = '.dll') or
                     (ext = '.jar') or
                     // configs
                     (ext = '.ini') or
                     (ext = '.json') or
                     // program data
                     (ext = '.xml') or
                     (ext = '.xsd') or
                     (ext = '.xsl') or
                     (ext = '.otf') or
                     (ext = '.ttf') or
                     // apps
                     (ext = '.pdf') or
                     (ext = '.doc') or
                     (ext = '.docx') or
                     (ext = '.xls') or
                     (ext = '.xlsx') or
                     (ext = '.txt') or
                     (ext = '.csv') or
                     (ext = '.npe') or
                     (ext = '.tokens') or
                     // certs
                     (ext = '.crt') or
                     (ext = '.pem'));
        if Result then begin
          Result := Parser.IsCodeFileNamedAs(Path + SearchRec.Name, SearchForText);
          if Result then begin
            FoundInPath := Path + SearchRec.Name;
            Break;
          end;
        end;
      end;
      iResult := SysUtils.FindNext(SearchRec);
    until iResult <> 0;
    SysUtils.FindClose(SearchRec);
  end;
end;

function SearchSubDirectoriesForCodeName(const Parser: TNPCSourceParser; Path: String; const CodeName: String; var FoundInPath: String): Boolean;
var
  SearchRec: TSearchRec;
  checked_path{, checked_file}, found_path: String;
  iResult: Integer;
begin
  Result := False;
  if Path[Length(Path)] <> '\' then
    Path := Path + '\';
  if SysUtils.FindFirst(Path + '*.*', faDirectory, SearchRec) = 0 then begin
    repeat
      if SearchRec.Attr and faDirectory = faDirectory then begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then begin
          checked_path := Path + SearchRec.Name + '\';
          ConsoleWriteln('Searching import path: "' + checked_path + '"');
          Result := SearchInFiles(Parser, checked_path, '*.*', TNPCProjectSettings(Parser.Settings^), found_path, CodeName);
          if Result then begin
            FoundInPath := found_path;
            Break;
          end;
          Result := SearchSubDirectoriesForCodeName(Parser, checked_path, CodeName, FoundInPath);
          if Result then
            Break;
        end;
      end;
      iResult:=SysUtils.FindNext(SearchRec);
    until iResult <> 0;
    SysUtils.FindClose(SearchRec);
  end;
end;

function SearchSubDirectories(Path: String; const FileName: String; var FoundInPath: String): Boolean;
var
  SearchRec: TSearchRec;
  checked_path, checked_file: String;
  iResult: Integer;
begin
  Result := False;
  if Path[Length(Path)] <> '\' then
    Path := Path + '\';
  if SysUtils.FindFirst(Path + '*.*', faDirectory, SearchRec) = 0 then begin
    repeat
      if SearchRec.Attr and faDirectory = faDirectory then begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then begin
          checked_path := Path + SearchRec.Name + '\';
          ConsoleWriteln('Searching import path: "' + checked_path + '"');
          checked_file := checked_path + FileName;
          Result := FileExists(checked_file);
          if Result then begin
            FoundInPath := checked_file;
            Break;
          end;
          Result := SearchSubDirectories(checked_path, FileName, FoundInPath);
          if Result then
            Break;
        end;
      end;
      iResult:=SysUtils.FindNext(SearchRec);
    until iResult <> 0;
    SysUtils.FindClose(SearchRec);
  end;
end;

procedure TNPCSourceParser.SearchImportByCodeName(const AToken: TNPCToken; const SearchPath: String; const SearchFileName: String);
var
  path, checked_path, checked_file, found_path: String;
  recursive, import_found: Boolean;
  i: Integer;
  import_source: TNPCSourceParser;
begin
  checked_path := SearchPath;
  ConsoleWriteln('Searching import path: "' + checked_path + '"');
  import_found := SearchInFiles(Self, checked_path, '*.*', TNPCProjectSettings(SettingsPtr^), found_path, SearchFileName);
  if import_found then begin
    checked_file := found_path;
//    AddToken(AToken);
    AddImport(itCode, SearchFileName, checked_file);
    ConsoleWriteln('Import found_________: "' + SearchFileName + '" at: "' + checked_file  + '"');
    //
    // @TODO: make it parallel
    // @TODO: create new source parser class, because we now have critical error in file processing, we clear self properties with newly loaded file, which is wrong
    if NPC_CompileImport(PChar(checked_file), Compiler, Self, import_source) then begin

      import_source.Free;
    end
    else begin // errors

    end;
  end
  else begin
    for i := 0 to High(TNPCProjectSettings(SettingsPtr^).ProjectSearchPaths) do begin
      path := TNPCProjectSettings(SettingsPtr^).ProjectSearchPaths[i]; // check for absolute or relative path and {*} for recursive directories
      recursive := False;
      if EndsText('{*}', path) then begin // check if we should search inside subdirectories if any exists
        recursive := True;
        path := LeftStr(path, Length(path) - 3);
      end;
      if StartsText('.\', path) or StartsText('..\', path) then // relative path
        checked_path := SearchPath + path
      else
        checked_path := path;
      checked_path := ExpandFileName(checked_path);
      //
      ConsoleWriteln('Searching import path: "' + checked_path + '"');
      import_found := SearchInFiles(Self, checked_path, '*.*', TNPCProjectSettings(SettingsPtr^), found_path, SearchFileName);;
      if not import_found and recursive then begin
        found_path := '';
        import_found := SearchSubDirectoriesForCodeName(Self, checked_path, SearchFileName, found_path);
        if import_found then
          checked_file := found_path;
        found_path := '';
      end;
      if import_found then
        Break;
    end;
    if import_found then begin
//      AddToken(AToken);
      AddImport(itCode, AToken.Value, checked_file);
      ConsoleWriteln('Import found_________: "' + AToken.Value + '" at: "' + checked_file  + '"');
      // @TODO: make it parallel
      if NPC_CompileImport(PChar(checked_file), Compiler, Self, import_source) then begin

        import_source.Free;
      end
      else begin // errors

      end;
    end
    else
      //ConsoleWriteln('Import not found_____: "' + token.Value + '"');
      raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserImportNotFound, [AToken.Value]));
  end;
end;

procedure TNPCSourceParser.SearchImportByFileName(const AToken: TNPCToken; const SearchPath: String; const SearchFileName: String);
var
  path, checked_path, checked_file, found_path: String;
  recursive, import_found: Boolean;
  i: Integer;
  import_source: TNPCSourceParser;
begin
  checked_path := SearchPath;
  ConsoleWriteln('Searching import path: "' + checked_path + '"');
  checked_file := checked_path + SearchFileName;
  import_found := FileExists(checked_file);
  if import_found then begin
//    AddToken(AToken);
    AddImport(itCode, AToken.Value, checked_file);
    ConsoleWriteln('Import found_________: "' + AToken.Value + '" at: "' + checked_file  + '"');
    // @TODO: make it parallel
    if NPC_CompileImport(PChar(checked_file), Compiler, Self, import_source) then begin

      import_source.Free;
    end
    else begin // errors

    end;
  end
  else begin
    for i := 0 to High(TNPCProjectSettings(SettingsPtr^).ProjectSearchPaths) do begin
      path := TNPCProjectSettings(SettingsPtr^).ProjectSearchPaths[i]; // check for absolute or relative path and {*} for recursive directories
      recursive := False;
      if EndsText('{*}', path) then begin // check if we should search inside subdirectories if any exists
        recursive := True;
        path := LeftStr(path, Length(path) - 3);
      end;
      if StartsText('.\', path) or StartsText('..\', path) then // relative path
        checked_path := SearchPath + path
      else
        checked_path := path;
      checked_path := ExpandFileName(checked_path);
      //
      ConsoleWriteln('Searching import path: "' + checked_path + '"');
      checked_file := checked_path + SearchFileName;
      import_found := FileExists(checked_file);
      if not import_found and recursive then begin
        found_path := '';
        import_found := SearchSubDirectories(checked_path, SearchFileName, found_path);
        if import_found then
          checked_file := found_path;
        found_path := '';
      end;
      if import_found then
        Break;
    end;
    if import_found then begin
//      AddToken(AToken);
      AddImport(itCode, AToken.Value, checked_file);
      ConsoleWriteln('Import found_________: "' + AToken.Value + '" at: "' + checked_file  + '"');
      // @TODO: make it parallel
      if NPC_CompileImport(PChar(checked_file), Compiler, Self, import_source) then begin

        import_source.Free;
      end
      else begin // errors

      end;
    end
    else
      //ConsoleWriteln('Import not found_____: "' + token.Value + '"');
      raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserImportNotFound, [AToken.Value]));
  end;
end;

procedure TNPCSourceParser.ParseImports(const ABlock: TNPC_AST);
var
  token: TNPCToken;
  project_path: String;
//  decl: TNPC_ASTDeclaration;
//  import_source: TNPCSourceParser;
begin
  project_path := ExtractFilePath(TNPCProjectSettings(SettingsPtr^).InputPath);
  if Length(project_path) = 0 then
    project_path := GetCurrentDir;
  project_path := IncludeTrailingPathDelimiter(project_path);
  try
    while Texer.IsNotEmpty do begin
      SkipComments;
      token := Texer.GetToken;
      if TokenIsReservedSymbol(token, rs_Comma) then begin
//        AddToken(token);
        Continue;
      end
      else if TokenIsReservedSymbol(token, rs_Semicolon) then begin
//        AddToken(token);
        Break;
      end
      else if (token.&Type = tokIdent) or (token.&Type = tokString) then begin // search for import in $search-path if any is defined
//        decl := TNPC_ASTDeclaration.Create;
//        decl.Location := token.Location.Copy;
//        decl.Identifier := TNPC_ASTIdentifier.Create;
//        decl.Identifier.Location := decl.Location;
//        //decl.Identifier.ResolvedDeclaration
//        decl.Identifier.ParentDeclaration := TNPC_ASTDeclaration(ABlock);
//        decl.Identifier.Name := UTF8String(token.Value);
        //
        if token.&Type = tokString then // @TODO: search all .npc files for code name specified in token.Value
          //raise NPCSyntaxError.NotSupportedError(token.Location, Format(sParserImportNotFound, [token.Value]));
          SearchImportByCodeName(token, project_path, token.Value)
        else
          SearchImportByFileName(token, project_path, token.Value + '.npc');
        //
        //
//        import_source.Free;
      end
      else
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, '', sProjectFile]));
    end;
  finally
    project_path := '';
  end;
end;

procedure TNPCSourceParser.ParseExports(const AToken: TNPCToken);
begin

end;

procedure TNPCSourceParser.ParseTypes(const AToken: TNPCToken);
var
  token: TNPCToken;
  has_body: Boolean;
begin
  has_body := False;
  while Texer.IsNotEmpty do begin
    SkipComments;
    token := Texer.PeekToken; // just peek a token
    if (token.&Type = tokIdent) and not TokenIsReservedIdent(token) then begin
      has_body := True;
      // left side is variable name
//      AddToken(token);
      Texer.SkipToken; // move forward
      token := Texer.ExpectToken([tokComma, tokEqual]);
//      AddToken(token);
      if TokenIsReservedSymbol(token, rs_Comma) then // comma separated variable names
        Continue;
      // we have colon, now get declared type identifier and semicolon at end
      token := Texer.GetToken;
      //if not (token.&Type in [tokIdent..tokString]) then
      if not (token.&Type = tokIdent) then
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, 'type ', sDeclaration]));
//      AddToken(token);
//      AddToken(Texer.ExpectToken([tokSemicolon]));
      Texer.ExpectToken([tokSemicolon]);
//      if (token.&Type = tokIdent) and Texer.IsCurrentSymbol('(') then // might be a function call
//        ParseLambdaParams(token);
    end
//    else if TokenIsReservedSymbol(token, ',') then begin
//      AddToken(token);
//    end
//    else if TokenIsReservedSymbol(token, ';') then begin
//      AddToken(token);
//      Break;
//    end
    else if TokenIsReservedIdent(token) then begin
      if not has_body then
        raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserSectionHasNoBody, [NPCReservedIdentifiers[ri_type].Ident]));
      Break;
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, '', sProjectFile]));
    //
    Texer.SkipToken;
  end;
end;

procedure TNPCSourceParser.ParseConsts(const AToken: TNPCToken);
var
  token: TNPCToken;
  has_body: Boolean;
begin
  has_body := False;
  while Texer.IsNotEmpty do begin
    SkipComments;
    token := Texer.PeekToken; // just peek a token
    if (token.&Type = tokIdent) and not TokenIsReservedIdent(token) then begin
      has_body := True;
      // left side is variable name
//      AddToken(token);
      Texer.SkipToken; // move forward
      token := Texer.ExpectToken([tokComma, tokColon]);
//      AddToken(token);
      if TokenIsReservedSymbol(token, rs_Comma) then // comma separated variable names
        Continue;
      // we have colon, now get declared type identifier and semicolon at end
      token := Texer.GetToken;
      //if not (token.&Type in [tokIdent..tokString]) then
      if not (token.&Type = tokIdent) then
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, 'const ', sDeclaration]));
//      AddToken(token);
//      AddToken(Texer.ExpectToken([tokSemicolon]));
      Texer.ExpectToken([tokSemicolon]);
//      if (token.&Type = tokIdent) and Texer.IsCurrentSymbol('(') then // might be a function call
//        ParseLambdaParams(token);
    end
//    else if TokenIsReservedSymbol(token, ',') then begin
//      AddToken(token);
//    end
//    else if TokenIsReservedSymbol(token, ';') then begin
//      AddToken(token);
//      Break;
//    end
    else if TokenIsReservedIdent(token) then begin
      if not has_body then
        raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserSectionHasNoBody, [NPCReservedIdentifiers[ri_const].Ident]));
      Break;
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, '', sProjectFile]));
    //
    Texer.SkipToken;
  end;
end;

procedure TNPCSourceParser.ParseVariables(const AToken: TNPCToken);
var
  token: TNPCToken;
  has_body: Boolean;
begin
  has_body := False;
  while Texer.IsNotEmpty do begin
    SkipComments;
    token := Texer.PeekToken; // just peek a token
    if (token.&Type = tokIdent) and not TokenIsReservedIdent(token) then begin
      has_body := True;
      // left side is variable name
//      AddToken(token);
      Texer.SkipToken; // move forward
      token := Texer.ExpectToken([tokComma, tokColon]);
//      AddToken(token);
      if TokenIsReservedSymbol(token, rs_Comma) then // comma separated variable names
        Continue;
      // we have colon, now get declared type identifier and semicolon at end
      token := Texer.GetToken;
      //if not (token.&Type in [tokIdent..tokString]) then
      if not (token.&Type = tokIdent) then
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, 'var ', sDeclaration]));
//      AddToken(token);
      token := Texer.ExpectToken([tokSemicolon, tokEqual]);
//      AddToken(token);
      if TokenIsReservedSymbol(token, rs_Equal) then begin // variable is initiated with compile-time value
//        AddToken(Texer.ExpectToken([tokIdent, tokNumber, tokString, tokChar]));
//        AddToken(Texer.ExpectToken([tokSemicolon]));
        Texer.ExpectToken([tokIdent, tokNumber, tokString, tokChar]);
        Texer.ExpectToken([tokSemicolon]);
      end;
//      if (token.&Type = tokIdent) and Texer.IsCurrentSymbol('(') then // might be a function call
//        ParseLambdaParams(token);
      Continue;
    end
//    else if TokenIsReservedSymbol(token, ',') then begin
//      AddToken(token);
//    end
//    else if TokenIsReservedSymbol(token, ';') then begin
//      AddToken(token);
//      Break;
//    end
    else if TokenIsReservedIdent(token) then begin
      if not has_body then
        raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserSectionHasNoBody, [NPCReservedIdentifiers[ri_var].Ident]));
      Break;
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, '', sProjectFile]));
    //
    Texer.SkipToken;
  end;
end;

procedure TNPCSourceParser.ParseInitialization(const AToken: TNPCToken);
var
  token: TNPCToken;
  has_body: Boolean;
begin
  has_body := False;
  while Texer.IsNotEmpty do begin
    SkipComments;
    token := Texer.PeekToken; // just peek a token
//    if ParseStatements(token, [ri_finalization, ri_begin], FLevel + 1) then begin
//      // if statements ware parsed than do nothing
//      has_body := True;
//    end;
    //
    if TokenIsReservedIdent(token, ri_finalization) or TokenIsReservedIdent(token, ri_begin) then begin
      if not has_body then
        raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserSectionHasNoBody, [NPCReservedIdentifiers[ri_initialization].Ident]));
      Break;
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, 'initialization ', sSection]));
    //
    Texer.SkipToken;
  end;
end;

procedure TNPCSourceParser.ParseFinalization(const AToken: TNPCToken);
var
  token: TNPCToken;
  has_body: Boolean;
begin
  has_body := False;
  while Texer.IsNotEmpty do begin
    SkipComments;
    token := Texer.PeekToken; // just peek a token
//    if ParseStatements(token, [ri_begin], FLevel + 1) then begin
//      // if statements ware parsed than do nothing
//      has_body := True;
//    end;
    //
    if TokenIsReservedIdent(token, ri_begin) then begin
      if not has_body then
        raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserSectionHasNoBody, [NPCReservedIdentifiers[ri_initialization].Ident]));
      Break;
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenTypeIn, [token.TokenToString, token.Value, 'finalization ', sSection]));
    //
    Texer.SkipToken;
  end;
end;

//procedure TNPCSourceParser.ParseFinalization(const AToken: TNPCToken);
//var
//  token: TNPCToken;
//  has_body: Boolean;
//begin
//  has_body := False;
//  while Texer.IsNotEmpty do begin
//    SkipComments;
//    token := Texer.PeekToken; // just peek a token
//    if TokenIsReservedSymbol(token, rs_Comma) then begin
//      AddToken(token);
//    end
//    else if TokenIsReservedSymbol(token, rs_Semicolon) then begin
//      AddToken(token);
//      Break;
//    end
//    else if token.&Type in [tokIdent..tokString] then begin
//      if TokenIsReservedIdent(token, ri_begin) then begin
//        if not has_body then
//          raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserSectionHasNoBody, [NPCReservedIdentifiers[ri_finalization].Ident]));
//        Break;
//      end;
//      // add finalization section body
//      has_body := True;
//
//    end
//    else
//      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
//    //
//    Texer.SkipToken;
//  end;
//end;

procedure TNPCSourceParser.ParseBegin(const AToken: TNPCToken);
var
  token: TNPCToken;
//  has_body: Boolean;
  project_main: TNPC_ASTStatementBlock;
  statement: TNPC_ASTStatement;
begin
//  has_body := False;

  project_main := TNPC_ASTStatementBlock.Create(AToken.Location, Nil, [BLOCK_IsMainProcedure, BLOCK_DoesNotHaveResult]);
  TNPC_ASTStatementBlock(ASTree).AddStatement(project_main);
  while Texer.IsNotEmpty do begin
    SkipComments;
    //
    statement := ParseStatement([stafEmptyStatementIsAcceptable]);
    project_main.AddStatement(statement);
    //
    token := Texer.PeekToken; // just peek a token
//    if ParseStatements(token, [ri_end], FLevel + 1) then begin
//      // if statements ware parsed than do nothing
////      has_body := True;
//    end;
    //
    if TokenIsReservedIdent(token, ri_end) then begin
//      Texer.SkipToken;
//      if not has_body then
//        raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserSectionHasNoBody, [NPCReservedIdentifiers[ri_begin].Ident]));
      Break;
    end;
//    else
//      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'finalization ', sSection]));
  end;
end;

procedure TNPCSourceParser.ParseEnd(const AToken: TNPCToken);
begin
//  AddToken(Texer.ExpectToken([tokDot]));
//  AddToken(Texer.GetToken);
  Texer.ExpectToken([tokDot]);
  Texer.SkipToken;
end;

(*
function TNPCSourceParser.ParseDeclarations(const AToken: TNPCToken): Boolean;
begin
  Result := False;
end;

function TNPCSourceParser.ParseStatements(var AToken: TNPCToken; const AExitOnReservedIdents: Array of TNPCReservedIdents; const ALevel: Integer): Boolean;
var
  token: TNPCToken;
  reserved_ident: TNPCReservedIdents;
begin
  Result := False;
  while Texer.IsNotEmpty do begin
    SkipComments;
    token := Texer.PeekToken; // just peek a token
    // check if token is reserved ident on which we wnat to stop parsing statements
    if Length(AExitOnReservedIdents) > 0 then begin
      for reserved_ident in AExitOnReservedIdents do begin
        if TokenIsReservedIdent(token, reserved_ident) then begin
          AToken := token;
          Exit;
        end;
      end;
    end;
    //
    if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then begin // or TokenIsReservedSymbol(token, rs_Semicolon) then begin
      AToken := token;
      Break;
    end
    else if TokenIsReservedIdent(token) then begin
      Result := True;
      if TokenIsReservedIdent(token, ri_asm) then begin
        //IncLevel;
      end
      else if TokenIsReservedIdent(token, ri_begin) then begin
        //IncLevel;
      end
      else if TokenIsReservedIdent(token, ri_break) then begin
        //DecLevel;
      end
      else if TokenIsReservedIdent(token, ri_case) then begin
        //IncLevel;
        //ParseCase(token);
        if FLevel > ALevel then begin
          //DecLevel;
//          Break;
        end;
        Continue;
      end
//      else if TokenIsReservedIdent(token, ri_end) then begin
//        DecLevel;
//      end
      else if TokenIsReservedIdent(token, ri_for) then begin
        //IncLevel;
        //ParseFor(token);
        if FLevel > ALevel then begin
          //DecLevel;
//          Break;
        end;
        Continue;
      end
      else if TokenIsReservedIdent(token, ri_goto) then begin
      end
      else if TokenIsReservedIdent(token, ri_if) then begin
        //IncLevel;
        //ParseIf(token);
        if FLevel > ALevel then begin
          //DecLevel;
//          Break;
        end;
        Continue;
      end
      else if TokenIsReservedIdent(token, ri_inherited) then begin
      end
      else if TokenIsReservedIdent(token, ri_label) then begin
      end
      else if TokenIsReservedIdent(token, ri_raise) then begin
      end
      else if TokenIsReservedIdent(token, ri_repeat) then begin
        //IncLevel;
      end
      else if TokenIsReservedIdent(token, ri_try) then begin
        //IncLevel;
      end
      else if TokenIsReservedIdent(token, ri_while) then begin
        //IncLevel;
      end
//      else if TokenIsReservedIdent(token, ri_else) then begin
//        AToken := token;
//        Break;
//      end
      else
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'initialization ', sSection]));
    end
    else if TokenIsLiteral(token) or TokenIsBiLiteral(token) then begin
      Result := True;
      if TokenIsReservedSymbol(token, rs_Assign) then begin
        //ParseAssignment(''{token});
        Continue;
      end
      else if TokenIsReservedSymbol(token, rs_OParen) then begin // parse function call params
        ParseCallParams(token);
        Continue;
      end
      else begin
//        AddToken(token);
        Texer.SkipToken; // move forward
        Continue;
      end;
    end
    else if not token.ReservedWord and not token.ReservedSymbol and (token.&Type in [tokIdent..tokChar]) then begin
      Result := True;
//      AddToken(token);
      Texer.SkipToken; // move forward
      Continue;
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sStatement]));
    //
    Texer.SkipToken;
  end;
end;

procedure TNPCSourceParser.ParseLambdaParams(const AToken: TNPCToken);
begin
//  AddToken(Texer.ExpectToken([tokOParen]));
  Texer.ExpectToken([tokOParen]);
  if Texer.IsCurrentSymbol(')') then // function without parameters
//    AddToken(Texer.ExpectToken([tokCParen]))
    Texer.ExpectToken([tokCParen])
  else begin // collect function params

  end;

//      AddToken(TNPCToken.Create(tokSetting, AToken.Location.Copy, False, False, '$program-type', EmptyTokenMD5));

end;
*)

function TNPCSourceParser.ParseImportFile(const ASourceFile: String): Boolean;
var
  source_file: TStringStream;
begin
  source_file := TStringStream.Create('', TNPCProjectSettings(SettingsPtr^).ProjectEncoding, False);
  try
    source_file.LoadFromFile(ASourceFile);
    Result := ParseImportFile(source_file, ASourceFile);
  finally
    source_file.Free;
  end;
end;

function TNPCSourceParser.ParseImportFile(const ASource: TStringStream; const ASourceFile: String): Boolean;
begin
  ASource.Position := 0;
  Result := Parse(ASource, ASourceFile, True);
end;

function TNPCSourceParser.ParseSourceCode(const ASourceCode: String): Boolean;
var
  source_file: TStringStream;
begin
  source_file := TStringStream.Create(ASourceCode, TNPCProjectSettings(SettingsPtr^).ProjectEncoding, False);
  try
    Result := ParseSourceCode(source_file, '');
  finally
    source_file.Free;
  end;
end;

function TNPCSourceParser.ParseSourceCode(const ASource: TStringStream; const ASourceFile: String): Boolean;
begin
  ASource.Position := 0;
  Result := Parse(ASource, ASourceFile);
end;

procedure TNPCSourceParser.OutputAST(const AFileName: String);
var
  tf: TStreamWriter;

  procedure Indent(const Level: Integer = 0);
  begin
    tf.Write(StringOfChar(' ', Level * 2));
  end;

  function LiteralTypeToString(const Expr: TNPC_ASTExpression): String;
  begin
    Result := '<--->';
    if Expr is TNPC_ASTExpressionIdent then begin
      Exit(TNPC_ASTExpressionIdent(Expr).Name);
    end
    else if Expr is TNPC_ASTExpressionNumber then begin
      Exit(TNPC_ASTExpressionNumber(Expr).Value);
    end
    else if Expr is TNPC_ASTExpressionString then begin
      Exit(TNPC_ASTExpressionString(Expr).Value);
    end
    else if Expr is TNPC_ASTTypeDefinition then begin
      Exit(TNPC_ASTTypeDefinition(Expr).Name);
    end;
  end;

  procedure PrintType(Typ: TNPC_ASTType; Level: Integer = 0);
  var
    Pair: TPair<UTF8String, Integer>;
    Field: TPair<UTF8String, TNPC_ASTType>;
    idx: Integer;
  begin
    if Typ is TNPC_ASTTypeDefinition then begin
      Indent(Level);
      tf.WriteLine('Type: ' + TNPC_ASTTypeDefinition(Typ).Name);
    end
    else if Typ is TNPC_ASTTypeEnum then begin
      Indent(Level);
      tf.WriteLine('Enum(' + IntToStr(TNPC_ASTTypeEnum(Typ).Members.Count) + '):');
      idx := 0;
      for Pair in TNPC_ASTTypeEnum(Typ).Members do begin
        Indent(Level+1);
        tf.WriteLine('[' + IntToStr(idx) + ']: ' + Pair.Key + ' = ' + Pair.Value.ToString);
        Inc(idx);
      end;
    end
    else if Typ is TNPC_ASTTypeArray then begin
      Indent(Level);
      tf.WriteLine('ArrayOf:');
      PrintType(TNPC_ASTTypeArray(Typ).ElementType, Level+1);
    end
    else if Typ is TNPC_ASTTypeRecord then begin
      Indent(Level);
      tf.WriteLine('Record:');
      idx := 0;
      for Field in TNPC_ASTTypeRecord(Typ).Fields do begin
        Indent(Level+1);
        tf.WriteLine('[' + IntToStr(idx) + ']: ' + Field.Key + ':');
        PrintType(Field.Value, Level+2);
        Inc(idx);
      end;
    end
    else
      Assert(False, 'PrintType unknown type: ' + Typ.ToString);
  end;

  procedure PrintExpr(const Expr: TNPC_ASTExpression; Level: Integer = 0);
  var
    Elem: TNPC_ASTExpression;
    Branch: TNPC_ASTExpressionCaseBranch;
  begin
    if Expr is TNPC_ASTExpressionLiteral then begin
      Indent(Level);
      tf.WriteLine('Literal:');
      Indent(Level+1);
      tf.WriteLine('Type: ' + LiteralTypeToString(TNPC_ASTExpressionLiteral(Expr).LiteralType));
      Indent(Level+1);
      tf.WriteLine('Value: "' + TNPC_ASTExpressionLiteral(Expr).Value + '"');
    end
    else if Expr is TNPC_ASTExpressionIdent then begin
      Indent(Level);
      tf.WriteLine('Ident:');
      Indent(Level+1);
      tf.WriteLine('Name: ' + TNPC_ASTExpressionIdent(Expr).Name);
    end
    else if Expr is TNPC_ASTExpressionEnumConst then begin
      Indent(Level);
      tf.WriteLine('EnumConst(' + TNPC_ASTExpressionEnumConst(Expr).Name + '=' + TNPC_ASTExpressionEnumConst(Expr).Value.ToString + ')');
    end
    else if Expr is TNPC_ASTExpressionArray then begin
      Indent(Level);
      tf.WriteLine('Array');
      PrintExpr(TNPC_ASTExpressionArray(Expr).Base, Level+1);
      PrintExpr(TNPC_ASTExpressionArray(Expr).Index, Level+1);
    end
    else if Expr is TNPC_ASTExpressionRecord then begin
      Indent(Level);
      tf.WriteLine('RecordField(' + TNPC_ASTExpressionRecord(Expr).FieldName + ')');
      PrintExpr(TNPC_ASTExpressionRecord(Expr).Base, Level+1);
    end
  //  else if Expr is TExprSetLiteral then begin
  //    Indent(Level);
  //    tf.WriteLine('SetLiteral');
  //    for var Elem in TExprSetLiteral(Expr).Elements do
  //      PrintExpr(Elem, Level+1);
  //  end
    else if Expr is TNPC_ASTExpressionSetLiteral then begin
      Indent(Level);
      if Assigned(TNPC_ASTExpressionSetLiteral(Expr).SetType) then
        tf.WriteLine('SetLiteral of ' + LiteralTypeToString(TNPC_ASTExpressionSetLiteral(Expr).SetType.ElementType))
      else
        tf.WriteLine('SetLiteral (untyped, empty)');
      for Elem in TNPC_ASTExpressionSetLiteral(Expr).Elements do
        PrintExpr(Elem, Level+1);
    end
    else if Expr is TNPC_ASTExpressionInOp then begin
      Indent(Level);
      tf.WriteLine('InOp');
      PrintExpr(TNPC_ASTExpressionInOp(Expr).Left, Level+1);
      PrintExpr(TNPC_ASTExpressionInOp(Expr).Right, Level+1);
    end
    else if Expr is TNPC_ASTExpressionVariable then begin
      Indent(Level);
      tf.WriteLine('Var(' + TNPC_ASTExpressionVariable(Expr).Name + ':' + TypeToString(TNPC_ASTExpressionVariable(Expr).Symbol) + ')');
    end
    else if Expr is TNPC_ASTExpressionBinary then begin
      Indent(Level);
      tf.WriteLine('Binary(' + TNPC_ASTExpressionBinary(Expr).Op + ')');
      PrintExpr(TNPC_ASTExpressionBinary(Expr).Left, Level+1);
      PrintExpr(TNPC_ASTExpressionBinary(Expr).Right, Level+1);
    end
    else if Expr is TNPC_ASTExpressionUnary then begin
      Indent(Level);
      tf.WriteLine('Unary(' + TNPC_ASTExpressionUnary(Expr).Op + ')');
      PrintExpr(TNPC_ASTExpressionUnary(Expr).Right, Level+1);
    end
    else if Expr is TNPC_ASTExpressionIf then begin
      Indent(Level);
      tf.WriteLine('IfExpr');
      Indent(Level+1);
      tf.WriteLine('Condition:');
      PrintExpr(TNPC_ASTExpressionIf(Expr).Cond, Level+2);
      Indent(Level+1);
      tf.WriteLine('Then:');
      PrintExpr(TNPC_ASTExpressionIf(Expr).ThenExpr, Level+2);
      if Assigned(TNPC_ASTExpressionIf(Expr).ElseExpr) then begin
        Indent(Level+1);
        tf.WriteLine('Else:');
        PrintExpr(TNPC_ASTExpressionIf(Expr).ElseExpr, Level+2);
      end;
    end
    else if Expr is TNPC_ASTExpressionCase then begin
      Indent(Level);
      tf.WriteLine('CaseExpr');
      Indent(Level+1);
      tf.WriteLine('Selector:');
      PrintExpr(TNPC_ASTExpressionCase(Expr).Selector, Level+2);

      for Branch in TNPC_ASTExpressionCase(Expr).Branches do begin
        Indent(Level+1);
        tf.WriteLine('Branch:');
        for Elem in Branch.IfValues do
          PrintExpr(Elem, Level+2);
        Indent(Level+2);
        tf.WriteLine('Result:');
        PrintExpr(Branch.ResultExpr, Level+3);
      end;

      if Assigned(TNPC_ASTExpressionCase(Expr).DefaultExpr) then begin
        Indent(Level+1);
        tf.WriteLine('Else:');
        PrintExpr(TNPC_ASTExpressionCase(Expr).DefaultExpr, Level+2);
      end;
    end
    else if Expr is TNPC_ASTExpressionCall then begin
      Indent(Level);
      tf.WriteLine('Call');
      Indent(Level+1);
      tf.WriteLine('Callee:');
      PrintExpr(TNPC_ASTExpressionCall(Expr).Callee, Level+2);
      Indent(Level+1);
      tf.WriteLine('Params:');
      for Elem in TNPC_ASTExpressionCall(Expr).Args do
        PrintExpr(Elem, Level+2);
    end;
  end;

  procedure PrintStmt(const Prefix: String; Stmt: TNPC_ASTStatement; Level: Integer = 0);
  var
    i: Integer;
    Param: TNPC_ASTParameter;
    TypeDef: TNPC_ASTTypeDefinition;
    Elem: TNPC_ASTExpression;
    Branch: TNPC_ASTStatementCaseBranch;
    idx: Integer;
  begin
    if (Stmt is TNPC_ASTStatementBlock) and not (Stmt is TNPC_ASTStatementProcedure) then begin
      Indent(Level);
      tf.WriteLine(Prefix + 'Block(' + IntToStr(TNPC_ASTStatementBlock(Stmt).Statements.Count) + ')');
      for i := 0 to TNPC_ASTStatementBlock(Stmt).Statements.Count-1 do begin
        PrintStmt('[' + IntToStr(i) + ']: ', TNPC_ASTStatementBlock(Stmt).Statements[i], Level+1);
        tf.WriteLine;
      end;
    end
    else if Stmt is TNPC_ASTStatementProcedure then begin
      Indent(Level);
      tf.WriteLine(Prefix + IfThen(TNPC_ASTStatementProcedure(Stmt).IsFunction, 'Function', 'Procedure' ));
      Indent(Level+1);
      tf.WriteLine('Name: ' + TNPC_ASTStatementProcedure(Stmt).Name);
      //
      Indent(Level+1);
      tf.WriteLine('Params(' + IntToStr(TNPC_ASTStatementProcedure(Stmt).Parameters.Count) + '):');
      idx := 0;
      for Param in TNPC_ASTStatementProcedure(Stmt).Parameters do begin
        Indent(Level+2);
        tf.WriteLine('[' + IntToStr(idx) + ']: Param:');
        TypeDef := TNPC_ASTTypeDefinition(Param.DeclaredType);
        Indent(Level+3);
        tf.WriteLine('Name: ' + Param.Name);
        Indent(Level+3);
        tf.WriteLine('Type: ' + TypeDef.Name);
        Indent(Level+3);
        tf.Write('Modifier:');
        case Param.Modifier of
          pmConst: tf.WriteLine(' const');
          pmVar  : tf.WriteLine(' var');
          pmOut  : tf.WriteLine(' out');
        else
          tf.WriteLine;
        end;
        Indent(Level+3);
        tf.WriteLine('Init:');
        PrintExpr(Param.Init, Level+4);
      end;
      //
      if TNPC_ASTStatementProcedure(Stmt).IsFunction and (TNPC_ASTStatementProcedure(Stmt).Returns <> Nil) then begin
        for Param in TNPC_ASTStatementProcedure(Stmt).Returns do begin
          Indent(Level+1);
          TypeDef := TNPC_ASTTypeDefinition(Param.DeclaredType);
          tf.WriteLine('Return(' + Param.Name + ': ' + TypeDef.Name + ')');
          //
          Indent(Level+2);
          tf.Write('Modifier:');
          case Param.Modifier of
            pmConst: tf.WriteLine(' const');
            pmVar  : tf.WriteLine(' var');
            pmOut  : tf.WriteLine(' out');
          else
            tf.WriteLine;
          end;
          Indent(Level+2);
          tf.WriteLine('Init:');
          PrintExpr(Param.Init, Level+3);
        end;
      end;
      //
      if TNPC_ASTStatementProcedure(Stmt).Body <> Nil then begin
        Indent(Level+1);
        tf.WriteLine('Body:');
        PrintStmt('', TNPC_ASTStatementProcedure(Stmt).Body, Level+2);
      end;
    end
    else if Stmt is TNPC_ASTStatementExpression then begin
//      Indent(Level);
//      tf.WriteLine('Expression');
      PrintExpr(TNPC_ASTStatementExpression(Stmt).Expression, Level);
    end
    else if Stmt is TNPC_ASTStatementTypeDeclaration then begin
      Indent(Level);
      tf.WriteLine(Prefix + 'TypeDecl(' + TNPC_ASTStatementTypeDeclaration(Stmt).Name + ')');
      PrintType(TNPC_ASTStatementTypeDeclaration(Stmt).DeclaredType, Level+1);
    end
    else if Stmt is TNPC_ASTStatementVariableDeclaration then begin
      //Indent(Level);
      //WriteLine('VarDecl(', TStmtVar(Stmt).Name, ':', TStmtVar(Stmt).TypeName, ')');
      Indent(Level);
      tf.WriteLine(Prefix + 'VarDecl');
      Indent(Level+1);
      tf.WriteLine('Name: ' + TNPC_ASTStatementVariableDeclaration(Stmt).Name);
      PrintType(TNPC_ASTStatementVariableDeclaration(Stmt).DeclaredType, Level+1);
      if Assigned(TNPC_ASTStatementVariableDeclaration(Stmt).Init) then begin
        Indent(Level+1);
        tf.WriteLine('Init:');
        PrintExpr(TNPC_ASTStatementVariableDeclaration(Stmt).Init, Level+2);
      end;
    end
    else if Stmt is TNPC_ASTStatementAssign then begin
      Indent(Level);
      tf.WriteLine(Prefix + 'Assign');
      Indent(Level+1);
      tf.WriteLine('Variable: ' + TypeToName(TNPC_ASTStatementAssign(Stmt).Target));
      PrintExpr(TNPC_ASTStatementAssign(Stmt).Value, Level+1);
    end
    else if Stmt is TNPC_ASTStatementIf then begin
      Indent(Level);
      tf.WriteLine(Prefix + 'IfStmt');
      Indent(Level+1);
      tf.WriteLine('Cond:');
      PrintExpr(TNPC_ASTStatementIf(Stmt).Cond, Level+2);
      Indent(Level+1);
      tf.WriteLine('Then:');
      PrintStmt('', TNPC_ASTStatementIf(Stmt).ThenStmt, Level+2);
      if Assigned(TNPC_ASTStatementIf(Stmt).ElseStmt) then begin
        Indent(Level+1);
        tf.WriteLine('Else:');
        PrintStmt('', TNPC_ASTStatementIf(Stmt).ElseStmt, Level+2);
      end;
    end
    else if Stmt is TNPC_ASTStatementCase then begin
      Indent(Level);
      //tf.WriteLine('Case<not-implemented>');
      tf.WriteLine(Prefix + 'CaseStmt');
      Indent(Level+1);
      tf.WriteLine('Selector:');
      PrintExpr(TNPC_ASTStatementCase(Stmt).Selector, Level+2);

      for Branch in TNPC_ASTStatementCase(Stmt).Branches do begin
        Indent(Level+1);
        tf.WriteLine('Branch:');
        for Elem in Branch.IfValues do
          PrintExpr(Elem, Level+2);
        Indent(Level+2);
        tf.WriteLine('Stmt:');
        PrintStmt('', Branch.Stmt, Level+3);
      end;

      if Assigned(TNPC_ASTStatementCase(Stmt).DefaultStmt) then begin
        Indent(Level+1);
        tf.WriteLine('Else:');
        PrintStmt('', TNPC_ASTStatementCase(Stmt).DefaultStmt, Level+2);
      end;
    end
    else if Stmt is TNPC_ASTStatementWhile then begin
      Indent(Level);
      tf.WriteLine(Prefix + 'WhileStmt');
      PrintExpr(TNPC_ASTStatementWhile(Stmt).Cond, Level+1);
      PrintStmt('', TNPC_ASTStatementWhile(Stmt).Body, Level+1);
    end
    else if Stmt is TNPC_ASTStatementFor then begin
//      Indent(Level);
//      if TNPC_ASTStatementFor(Stmt).Reverse then
//        tf.WriteLine('For ' + TNPC_ASTStatementFor(Stmt).VarName + ' downto')
//      else
//        tf.WriteLine('For ' + TNPC_ASTStatementFor(Stmt).VarName + ' to');
      Indent(Level);
      tf.WriteLine(Prefix + 'ForStmt');
      Indent(Level+1);
      tf.WriteLine('Init:');
      PrintStmt('', TNPC_ASTStatementFor(Stmt).InitStmt, Level+2);
      Indent(Level+1);
      tf.WriteLine('Cond:');
      PrintExpr(TNPC_ASTStatementFor(Stmt).CondExpr, Level+2);
      Indent(Level+1);
      tf.WriteLine('End:');
      PrintExpr(TNPC_ASTStatementFor(Stmt).EndExpr, Level+2);
      Indent(Level+1);
      tf.WriteLine('Body:');
      PrintStmt('', TNPC_ASTStatementFor(Stmt).Body, Level+2);
    end;
//    else if Stmt is TNPC_ASTStatemenWhile then begin
//      Indent(Level);
//      tf.WriteLine('Cond:');
//      PrintExpr(TNPC_ASTStatementWhile(Stmt).Cond, Level+1);
//      Indent(Level);
//      tf.WriteLine('Body:');
//      PrintStmt(TNPC_ASTStatementWhile(Stmt).Body, Level+1);
//    end;
  end;

begin
  try
    tf := TStreamWriter.Create(AFileName + '.ast', False, TEncoding.UTF8, 32768);
    try
      tf.BaseStream.Position := 0;
      tf.BaseStream.Size := 0;
      //
      if AST = Nil then begin
        tf.WriteLine('No AST for this file!');
        Exit;
      end;

      tf.WriteLine(Format('%s (%d:%d):', [AST.Location.FileName, AST.Location.StartRow, AST.Location.StartCol]));
      PrintStmt('', TNPC_ASTStatement(AST));
    finally
      tf.Free;
    end;
  except
  end;
end;

end.

