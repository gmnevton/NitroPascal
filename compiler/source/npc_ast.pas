//
// Nitro Pascal Compiler
// version 1.0
//
// Abstract Syntax Tree
//

unit npc_ast;

interface

uses
  SysUtils,
  Classes,
  Generics.Collections,
  npc_location;

type
  TNPC_ASTTypeExpression = class;

  TNPCSymbolKind = (
    skType,
    skDecl, // const, var
    skProc  // procedure, function
  );

  TNPCSymbol = class
  public
    Name: UTF8String;
    Kind: TNPCSymbolKind;
    IsConst: Boolean;
    TypeRef: TNPC_ASTTypeExpression;
    DeclNode: TObject; // pointer/reference to the declaration AST node (optional)
    ConstValue: Integer; // used if Kind=skDecl and IsConst=True
    //
    constructor Create(const AName: UTF8String; AKind: TNPCSymbolKind; AConst: Boolean; AType: TNPC_ASTTypeExpression; ADecl: TObject; AValue: Integer = 0);
  end;

//  TNPCListEntry = packed record
//    Name: UTF8String;
//    Symbol: TNPCSymbol;
//  end;

  TNPCScope = class // TDictionary<UTF8String, TSymbol>;
  private
//    List: Array of TNPCListEntry;
//    Size: Integer;
//    Capacity: Integer;
    Table: TDictionary<UTF8String, TNPCSymbol>;
    Parent: TNPCScope;
    //
//    procedure Init;
  public
    constructor Create(AParent: TNPCScope);
    destructor Destroy; override;
    //
//    procedure Clear;
//    function Exists(const AName: UTF8String): Boolean;
//    procedure Add(const AName: UTF8String; const ASymbol: TNPCSymbol);
//    function TryGetValue(const AName: UTF8String; out ASymbol: TNPCSymbol): Boolean;
    //
    function Resolve(const AName: UTF8String): TNPCSymbol;
    //
    procedure DefineType(const AName: UTF8String; AType: TNPC_ASTTypeExpression; ADecl: TObject);
    procedure DefineConst(const AName: UTF8String; AType: TNPC_ASTTypeExpression; ADecl: TObject; AValue: Integer);
    procedure DefineVar(const AName: UTF8String; AType: TNPC_ASTTypeExpression; ADecl: TObject);
  end;

  TNPC_ASTType = (
    AST_UNKNOWN,

    AST_BLOCK,
    AST_EXPRESSION,
    AST_STATEMENT,
    AST_STATEMENTS,
    AST_COMPOUND_STATEMENT,
    AST_SCOPE_STATEMENT,
    AST_EXPRESSION_STATEMENT,

    AST_IDENTIFIER,
    AST_LITERAL,
    AST_DECLARATION,

    AST_UNARY,
    AST_BINARY,
    AST_TERNARY,

    AST_TYPE_DEFINITION,
    AST_TYPE_DECLARATION,
    AST_RECORD_DESCRIPTION,
    AST_ENUM,
    AST_SET,
    AST_BITSET,
    AST_ARRAY,
    AST_ARRAY_SLICE,
    AST_RECORD,
    AST_EXPRESSION_LITERAL, // AST_LITERAL ???
    AST_ENUM_CONST,
    AST_SET_LITERAL, // AST_LITERAL ???
//    AST_IDENT, // AST_LITERAL ???
//    AST_NUMBER, // AST_LITERAL ???
    AST_STRING, // AST_LITERAL ???
    AST_IN_OP,
    AST_STRUCT_MEMBER,
    AST_ARRAY_INDEX,
    AST_STATEMENT_TYPE_DECL, // same as AST_TYPE_DECLARATION ???
    AST_STATEMENT_VAR_DECL, // same as AST_TYPE_DECLARATION ??

    AST_PROCEDURE,
    AST_CALL,
    AST_EXTERNAL_CALL,

    AST_LABEL,
    AST_ASSIGN,
    AST_IF,
    AST_CASE,
    AST_BREAK,
    AST_CONTINUE,
    AST_FOR,
    AST_WHILE,
    AST_REPEAT_UNTIL,

    AST_DIRECTIVE_IF,
    AST_DIRECTIVE_ENSURE
  );

  TNPC_ByteCode = TBytes;

  TNPC_AstFlags = (
    AST_Parenthesized = $1,
    AST_ForEachBinaryOperation = $2
  );

  TNPC_AST = class
  public
    &Type: TNPC_ASTType;
    ASTFlags: LongWord; // TNPC_AstFlags
    Location: TNPCLocation; // copy of TNPCTokensParser.Location
    //
    constructor Create(const ALocation: TNPCLocation); virtual;
    destructor Destroy; override;
  end;

  TNPC_ASTExpression = class(TNPC_AST)
  public
    // &Type = AST_EXPRESSION
    Flags: LongWord;
    InferredType: TNPC_ASTTypeExpression; //TNPC_ASTTypeDefinition;
    //
    constructor Create(const ALocation: TNPCLocation); override;
    destructor Destroy; override;
  end;

  // Types

  TNPC_ASTTypeDefinition = class(TNPC_ASTExpression)
  public
    // &Type = AST_TYPE_DEFINITION
    //
    constructor Create(const ALocation: TNPCLocation); override;
    destructor Destroy; override;
  end;

  TNPC_ASTTypeExpression = class(TNPC_ASTExpression)
  public
    // &Type = AST_TYPE_DECLARATION
    //
    constructor Create(const ALocation: TNPCLocation); override;
    destructor Destroy; override;
  end;

//  TNPC_ASTTypeName = class(TNPC_ASTTypeExpression)
//  public
//    Name: UTF8String;
//    constructor Create(const AName: UTF8String);
//  end;

  TNPC_ASTTypeEnum = class(TNPC_ASTTypeExpression)
  public
    // &Type = AST_ENUM
    Members: TDictionary<UTF8String, Integer>;
    //
    constructor Create(const ALocation: TNPCLocation); override;
    destructor Destroy; override;
  end;

  TNPC_ASTTypeSet = class(TNPC_ASTTypeExpression)
  public
    // &Type = AST_SET
    ElementType: TNPC_ASTTypeExpression;
    //
    constructor Create(const ALocation: TNPCLocation); override;
    destructor Destroy; override;
  end;

  TNPC_ASTTypeArray = class(TNPC_ASTTypeExpression)
  public
    // &Type = AST_ARRAY
    ElementType: TNPC_ASTTypeExpression;
    IndexType: TNPC_ASTTypeExpression;
    //
    constructor Create(const ALocation: TNPCLocation; AElementType, AIndexType: TNPC_ASTTypeExpression); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTTypeRecord = class(TNPC_ASTTypeExpression)
  public
    // &Type = AST_RECORD_DESCRIPTION
    Fields: TDictionary<UTF8String, TNPC_ASTTypeExpression>;
    //
    constructor Create(const ALocation: TNPCLocation); override;
    destructor Destroy; override;
  end;

  // Expressions

  TNPC_ASTExpressionLiteral = class(TNPC_ASTExpression)
  public
    // &Type = AST_EXPRESSION_LITERAL
    Value: UTF8String;
    LiteralType: TNPC_ASTTypeExpression;
    //
    constructor Create(const ALocation: TNPCLocation; const AValue: UTF8String; AType: TNPC_ASTTypeExpression); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTExpressionEnumConst = class(TNPC_ASTExpression)
  public
    // &Type = AST_ENUM_CONST
    Name: UTF8String;
    Value: Integer;
    EnumType: TNPC_ASTTypeExpression;
    //
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String; AValue: Integer; AType: TNPC_ASTTypeExpression); reintroduce;
    destructor Destroy; override;
  end;

  // set literal: [Red, Green, Blue]
  TNPC_ASTExpressionSetLiteral = class(TNPC_ASTExpression)
  public
    // &Type = AST_SET_LITERAL
    Elements: TObjectList<TNPC_ASTExpression>;
    SetType: TNPC_ASTTypeSet;
    //
    constructor Create(const ALocation: TNPCLocation; AType: TNPC_ASTTypeSet); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTExpressionVariable = class(TNPC_ASTExpression)
  public
    // &Type = AST_DECLARATION
    Name: UTF8String;
    Symbol: TNPCSymbol; // resolved symbol during parsing / semantic analysis
    //
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String; ASymbol: TNPCSymbol); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTExpressionIdent = class(TNPC_ASTExpression)
  public
    // &Type = AST_IDENT
    Name: UTF8String;
    ResolvedSymbol: TNPCSymbol; // filled by resolution pass
    //
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTExpressionNumber = class(TNPC_ASTExpression)
  public
    // &Type = AST_LITERAL
    Value: UTF8String;
    LiteralType: TNPC_ASTTypeExpression;
    //
    constructor Create(const ALocation: TNPCLocation; const AValue: UTF8String; AType: TNPC_ASTTypeExpression); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTExpressionString = class(TNPC_ASTExpression)
  public
    // &Type = AST_STRING
    Value: UTF8String;
    //
    constructor Create(const ALocation: TNPCLocation; const AValue: UTF8String); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTExpressionUnary = class(TNPC_ASTExpression)
  public
    // &Type = AST_UNARY
    Op: UTF8String;
    Right: TNPC_ASTExpression;
    //
    constructor Create(const ALocation: TNPCLocation; const AOp: UTF8String; ARight: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTExpressionBinary = class(TNPC_ASTExpression)
  public
    // &Type = AST_BINARY
    Op: UTF8String;
    Left,
    Right: TNPC_ASTExpression;
    //
    constructor Create(const ALocation: TNPCLocation; ALeft: TNPC_ASTExpression; const AOp: UTF8String; ARight: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
  end;

  // membership test: X in [..]
  TNPC_ASTExpressionInOp = class(TNPC_ASTExpressionBinary)
  public
    // &Type = AST_IN_OP
    //Left: TNPC_ASTExpression;
    //Right: TNPC_ASTExpression; // must be set
    //
    constructor Create(const ALocation: TNPCLocation; ALeft, ARight: TNPC_ASTExpression); reintroduce;
  end;

  TNPC_ASTExpressionArray = class(TNPC_ASTExpression)
  public
    // &Type = AST_ARRAY
    Base: TNPC_ASTExpression;
    Index: TNPC_ASTExpression;
    ElemType: TNPC_ASTTypeExpression;
    //
    constructor Create(const ALocation: TNPCLocation; ABase, AIndex: TNPC_ASTExpression; AElemType: TNPC_ASTTypeExpression); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTExpressionRecord = class(TNPC_ASTExpression)
  public
    // &Type = AST_RECORD
    Base: TNPC_ASTExpression;
    FieldName: UTF8String;
    FieldType: TNPC_ASTTypeExpression;
    //
    constructor Create(const ALocation: TNPCLocation; ABase: TNPC_ASTExpression; const AFieldName: UTF8String; AFieldType: TNPC_ASTTypeExpression); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTExpressionCall = class(TNPC_ASTExpression)
  public
    // &Type = AST_CALL
    Callee: TNPC_ASTExpression;
    Args: TObjectList<TNPC_ASTExpression>;
    //
    constructor Create(const ALocation: TNPCLocation; ACallee: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTExpressionMember = class(TNPC_ASTExpression) // e.g. a.b
  public
    // &Type = AST_STRUCT_MEMBER
    Owner: TNPC_ASTExpression;
    Member: UTF8String;
    //
    constructor Create(const ALocation: TNPCLocation; AOwner: TNPC_ASTExpression; const AMember: UTF8String); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTExpressionIndex = class(TNPC_ASTExpression) // e.g. a[0]
  public
    // &Type = AST_ARRAY_INDEX
    Target: TNPC_ASTExpression;
    Index: TNPC_ASTExpression;
    //
    constructor Create(const ALocation: TNPCLocation; ATarget, AIndex: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
  end;

  // Statements

  TNPC_ASTStatementBlock = class;

  TNPC_ASTStatement = class(TNPC_AST)
  public
    // &Type = AST_STATEMENT
    Flags: LongWord;
    Block: TNPC_ASTStatementBlock;
    TypeDefinition: TNPC_ASTTypeDefinition;
    Expression: TNPC_ASTExpression;
    //
    constructor Create(const ALocation: TNPCLocation); override;
    destructor Destroy; override;
  end;

  TNPC_BlockFlag = (
    BLOCK_ConsistsOfOrderedStatements,
    BLOCK_DoesNotHaveResult,
    BLOCK_BelongsToLoop,
    BLOCK_BelongsToRecord,
    BLOCK_HasBeenInferred,
    BLOCK_HasBeenByteCoded,
    BLOCK_IsMainProcedure,
    BLOCK_IsInitProcedure,
    BLOCK_IsDeinitProcedure
  );
  TNPC_BlockFlags = set of TNPC_BlockFlag;

  TNPC_ASTStatementBlock = class(TNPC_ASTStatement)
  public
    // &Type = AST_BLOCK
    Flags: TNPC_BlockFlags;
    ParentBlock: TNPC_ASTStatementBlock;
    Stmts: TObjectList<TNPC_ASTStatement>;
    //
    constructor Create(const ALocation: TNPCLocation; AParent: TNPC_ASTStatementBlock; const AFlags: TNPC_BlockFlags = []); reintroduce;
    destructor Destroy; override;
    //
    procedure AddStatement(const AStatement: TNPC_ASTStatement);
  end;

  TNPC_ASTStatementExpression = class(TNPC_ASTStatement)
  public
    // &Type = AST_EXPRESSION_STATEMENT
    Expr: TNPC_ASTExpression;
    //
    constructor Create(const ALocation: TNPCLocation; AExpr: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTStatementTypeDecl = class(TNPC_ASTStatement)
  public
    // &Type = AST_STATEMENT_TYPE_DECL
    Name: UTF8String;
    DeclaredType: TNPC_ASTTypeExpression;
    //
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPC_ASTTypeExpression); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTStatementVarDecl = class(TNPC_ASTStatement)
  public
    // &Type = AST_STATEMENT_VAR_DECL
    Name: UTF8String;
    DeclaredType: TNPC_ASTTypeExpression;   // used instead of String TypeName
    Init: TNPC_ASTExpression; // optional initializer
    SymbolRef: TNPCSymbol; // symbol created when parsing
    //
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPC_ASTTypeExpression; AInit: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTStatementLabel = class(TNPC_ASTStatement)
  public
    // &Type = AST_LABEL
    Ident: UTF8String;
    Statement: TNPC_ASTStatement;
//    TryBody: TNPC_ASTStatement; // set to TryCatchStatement or TryFinallyStatement if in _body portion
//    TryFinally: TNPC_ASTTryFinallyStatement;
//    ScopeGuard: TNPC_ASTScopeGuardStatement;
//    LastVar: TNPC_ASTVarDeclaration;
//    GotoTarget: TNPC_ASTStatement; // interpret
    //
    constructor Create(const ALocation: TNPCLocation; const AIdent: UTF8String; AStmt: TNPC_ASTStatement); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTStatementAssign = class(TNPC_ASTStatement)
  public
    // &Type = AST_ASSIGN
    Target: TNPC_ASTExpression; // usually TIdentExpr or member/index
    Value: TNPC_ASTExpression;
    //
    constructor Create(const ALocation: TNPCLocation; ATarget, AValue: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTStatementIf = class(TNPC_ASTStatement)
  public
    // &Type = AST_IF
    Cond: TNPC_ASTExpression;
    ThenStmt,
    ElseStmt: TNPC_ASTStatement;
    //
    constructor Create(const ALocation: TNPCLocation; ACond: TNPC_ASTExpression; AThen, AElse: TNPC_ASTStatement); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTStatementCase = class(TNPC_ASTStatement)
  public
    // &Type = AST_CASE
//    Cond: TNPC_ASTExpression;
//    ThenStmt,
//    ElseStmt: TNPC_ASTStatement;
    //
    //constructor Create(const ALocation: TNPCLocation; ACond: TNPC_ASTExpression; AThen, AElse: TNPC_ASTStatement); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTStatementFor = class(TNPC_ASTStatement)
  public
    // &Type = AST_FOR
    VarName: UTF8String;
    InitExpr: TNPC_ASTExpression;
    EndExpr: TNPC_ASTExpression;
    Reverse: Boolean;
    Body: TNPC_ASTStatement;
    //
    constructor Create(const ALocation: TNPCLocation; const AVarName: UTF8String; AInitExpr, AEndExpr: TNPC_ASTExpression; const ADownTo: Boolean; ABody: TNPC_ASTStatement); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTStatementWhile = class(TNPC_ASTStatement)
  public
    // &Type = AST_WHILE
    Cond: TNPC_ASTExpression;
    Body: TNPC_ASTStatement;
    //
    constructor Create(const ALocation: TNPCLocation; ACond: TNPC_ASTExpression; ABody: TNPC_ASTStatement); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TNPCSymbol }

constructor TNPCSymbol.Create(const AName: UTF8String; AKind: TNPCSymbolKind; AConst: Boolean; AType: TNPC_ASTTypeExpression; ADecl: TObject; AValue: Integer = 0);
begin
  inherited Create;
  Name := AName;
  Kind := AKind;
  IsConst := AConst;
  TypeRef := AType;
  DeclNode := ADecl;
  ConstValue := AValue;
end;

{ TNPCScope }

constructor TNPCScope.Create(AParent: TNPCScope);
begin
  Table := TDictionary<UTF8String, TNPCSymbol>.Create;
  Parent := AParent;
end;

destructor TNPCScope.Destroy;
var
  Symbol: TNPCSymbol;
begin
  for Symbol in Table.Values do
    Symbol.Free;
  Table.Free;
  inherited;
end;

function TNPCScope.Resolve(const AName: UTF8String): TNPCSymbol;
begin
  if Table.TryGetValue(AName, Result) then
    Exit;
  if Assigned(Parent) then
    Exit(Parent.Resolve(AName));
  Result := Nil;
end;

procedure TNPCScope.DefineType(const AName: UTF8String; AType: TNPC_ASTTypeExpression; ADecl: TObject);
begin
  Table.Add(AName, TNPCSymbol.Create(AName, skType, True, AType, ADecl));
end;

procedure TNPCScope.DefineConst(const AName: UTF8String; AType: TNPC_ASTTypeExpression; ADecl: TObject; AValue: Integer);
begin
  Table.Add(AName, TNPCSymbol.Create(AName, skDecl, True, AType, ADecl, AValue));
end;

procedure TNPCScope.DefineVar(const AName: UTF8String; AType: TNPC_ASTTypeExpression; ADecl: TObject);
begin
  Table.Add(AName, TNPCSymbol.Create(AName, skDecl, False, AType, ADecl));
end;

{ TNPC_AST }

constructor TNPC_AST.Create(const ALocation: TNPCLocation);
begin
  &Type := AST_UNKNOWN;
  ASTFlags := 0;
  Location := ALocation.Copy;
end;

destructor TNPC_AST.Destroy;
begin
  &Type := AST_UNKNOWN;
  ASTFlags := 0;
  if Location <> Nil then
    FreeAndNil(Location);
  inherited;
end;

{ TNPC_ASTExpression }

constructor TNPC_ASTExpression.Create;
begin
  inherited;
  &Type := AST_EXPRESSION;
  Flags := 0;
  //
  InferredType := Nil;
end;

destructor TNPC_ASTExpression.Destroy;
begin
  InferredType := Nil;
  inherited;
end;

{ TNPC_ASTTypeDefinition }

constructor TNPC_ASTTypeDefinition.Create;
begin
  inherited;
  &Type := AST_TYPE_DEFINITION;
end;

destructor TNPC_ASTTypeDefinition.Destroy;
begin
  inherited;
end;

{ TNPC_ASTTypeExpression }

constructor TNPC_ASTTypeExpression.Create;
begin
  inherited;
  &Type := AST_TYPE_DECLARATION;
end;

destructor TNPC_ASTTypeExpression.Destroy;
begin
  inherited;
end;

{ TNPC_ASTTypeEnum }

constructor TNPC_ASTTypeEnum.Create;
begin
  inherited;
  &Type := AST_ENUM;
  Members := TDictionary<UTF8String, Integer>.Create;
end;

destructor TNPC_ASTTypeEnum.Destroy;
//var
//  Member: Integer;
begin
//  for Member in Members.Values do
//    Member.Free;
  Members.Free;
  inherited;
end;

{ TNPC_ASTTypeSet }

constructor TNPC_ASTTypeSet.Create;
begin
  inherited;
  &Type := AST_SET;
  ElementType := Nil;
end;

destructor TNPC_ASTTypeSet.Destroy;
begin
  ElementType := Nil;
  inherited;
end;

{ TNPC_ASTTypeArray }

constructor TNPC_ASTTypeArray.Create(const ALocation: TNPCLocation; AElementType, AIndexType: TNPC_ASTTypeExpression);
begin
  inherited Create(ALocation);
  &Type := AST_ARRAY;
  ElementType := AElementType;
  IndexType := AIndexType;
end;

destructor TNPC_ASTTypeArray.Destroy;
begin
  ElementType := Nil;
  IndexType := Nil;
  inherited;
end;

{ TNPC_ASTTypeRecord }

constructor TNPC_ASTTypeRecord.Create;
begin
  inherited;
  &Type := AST_RECORD_DESCRIPTION;
  Fields := TDictionary<UTF8String, TNPC_ASTTypeExpression>.Create;
end;

destructor TNPC_ASTTypeRecord.Destroy;
var
  Field: TNPC_ASTTypeExpression;
begin
  for Field in Fields.Values do
    Field.Free;
  Fields.Free;
  inherited;
end;

{ TNPC_ASTExpressionLiteral }

constructor TNPC_ASTExpressionLiteral.Create(const ALocation: TNPCLocation; const AValue: UTF8String; AType: TNPC_ASTTypeExpression);
begin
  inherited Create(ALocation);
  &Type := AST_EXPRESSION_LITERAL;
  Value := AValue;
  LiteralType := AType;
end;

destructor TNPC_ASTExpressionLiteral.Destroy;
begin
  Value := '';
  if LiteralType <> Nil then
    LiteralType.Free;
  inherited;
end;

{ TNPC_ASTExpressionEnumConst }

constructor TNPC_ASTExpressionEnumConst.Create(const ALocation: TNPCLocation; const AName: UTF8String; AValue: Integer; AType: TNPC_ASTTypeExpression);
begin
  inherited Create(ALocation);
  &Type := AST_ENUM_CONST;
  Name := AName;
  Value := AValue;
  EnumType := AType;
end;

destructor TNPC_ASTExpressionEnumConst.Destroy;
begin
  Name := '';
  if EnumType <> Nil then
    EnumType.Free;
  inherited;
end;

{ TNPC_ASTExpressionSetLiteral }

constructor TNPC_ASTExpressionSetLiteral.Create(const ALocation: TNPCLocation; AType: TNPC_ASTTypeSet);
begin
  inherited Create(ALocation);
  &Type := AST_SET_LITERAL;
  Elements := TObjectList<TNPC_ASTExpression>.Create(True);
  SetType := AType;
end;

destructor TNPC_ASTExpressionSetLiteral.Destroy;
begin
  Elements.Free;
  inherited;
end;

{ TNPC_ASTExpressionVariable }

constructor TNPC_ASTExpressionVariable.Create(const ALocation: TNPCLocation; const AName: UTF8String; ASymbol: TNPCSymbol);
begin
  inherited Create(ALocation);
  &Type := AST_DECLARATION;
  Name := AName;
  Symbol := ASymbol; // resolved symbol during parsing / semantic analysis
end;

destructor TNPC_ASTExpressionVariable.Destroy;
begin
  Name := '';
  if Symbol <> Nil then
    Symbol.Free;
  inherited;
end;

{ TNPC_ASTExpressionIdent }

constructor TNPC_ASTExpressionIdent.Create(const ALocation: TNPCLocation; const AName: UTF8String);
begin
  inherited Create(ALocation);
  &Type := AST_IDENTIFIER;
  Name := AName;
  ResolvedSymbol := Nil; // filled by resolution pass
end;

destructor TNPC_ASTExpressionIdent.Destroy;
begin
  Name := '';
  if ResolvedSymbol <> Nil then
    ResolvedSymbol.Free;
  inherited;
end;

{ TNPC_ASTExpressionNumber }

constructor TNPC_ASTExpressionNumber.Create(const ALocation: TNPCLocation; const AValue: UTF8String; AType: TNPC_ASTTypeExpression);
begin
  inherited Create(ALocation);
  &Type := AST_LITERAL;
  Value := AValue;
  LiteralType := AType;
end;

destructor TNPC_ASTExpressionNumber.Destroy;
begin
  Value := '';
  if LiteralType <> Nil then
    LiteralType.Free;
  inherited;
end;

{ TNPC_ASTExpressionString }

constructor TNPC_ASTExpressionString.Create(const ALocation: TNPCLocation; const AValue: UTF8String);
begin
  inherited Create(ALocation);
  &Type := AST_STRING;
  Value := AValue;
end;

destructor TNPC_ASTExpressionString.Destroy;
begin
  Value := '';
  inherited;
end;

{ TNPC_ASTExpressionUnary }

constructor TNPC_ASTExpressionUnary.Create(const ALocation: TNPCLocation; const AOp: UTF8String; ARight: TNPC_ASTExpression);
begin
  inherited Create(ALocation);
  &Type := AST_UNARY;
  Op := AOp;
  Right := ARight;
end;

destructor TNPC_ASTExpressionUnary.Destroy;
begin
  Op := '';
  if Right <> Nil then
    Right.Free;
  inherited;
end;

{ TNPC_ASTExpressionBinary }

constructor TNPC_ASTExpressionBinary.Create(const ALocation: TNPCLocation; ALeft: TNPC_ASTExpression; const AOp: UTF8String; ARight: TNPC_ASTExpression);
begin
  inherited Create(ALocation);
  &Type := AST_BINARY;
  Op := AOp;
  Left := ALeft;
  Right := ARight;
end;

destructor TNPC_ASTExpressionBinary.Destroy;
begin
  Op := '';
  if Left <> Nil then
    Left.Free;
  if Right <> Nil then
    Right.Free;
  inherited;
end;

{ TNPC_ASTExpressionInOp }

constructor TNPC_ASTExpressionInOp.Create(const ALocation: TNPCLocation; ALeft, ARight: TNPC_ASTExpression);
begin
  inherited Create(ALocation, ALeft, 'in', ARight);
  &Type := AST_IN_OP;
end;

{ TNPC_ASTExpressionArray }

constructor TNPC_ASTExpressionArray.Create(const ALocation: TNPCLocation; ABase, AIndex: TNPC_ASTExpression; AElemType: TNPC_ASTTypeExpression);
begin
  inherited Create(ALocation);
  &Type := AST_ARRAY;
  Base := ABase;
  Index := AIndex;
  ElemType := AElemType;
end;

destructor TNPC_ASTExpressionArray.Destroy;
begin
  if Base <> Nil then
    Base.Free;
  if Index <> Nil then
    Index.Free;
  if ElemType <> Nil then
    ElemType.Free;
  inherited;
end;

{ TNPC_ASTExpressionRecord }

constructor TNPC_ASTExpressionRecord.Create(const ALocation: TNPCLocation; ABase: TNPC_ASTExpression; const AFieldName: UTF8String; AFieldType: TNPC_ASTTypeExpression);
begin
  inherited Create(ALocation);
  &Type := AST_RECORD;
  Base := ABase;
  FieldName := AFieldName;
  FieldType := AFieldType;
end;

destructor TNPC_ASTExpressionRecord.Destroy;
begin
  if Base <> Nil then
    Base.Free;
  FieldName := '';
  if FieldType <> Nil then
    FieldType.Free;
  inherited;
end;

{ TNPC_ASTExpressionCall }

constructor TNPC_ASTExpressionCall.Create(const ALocation: TNPCLocation; ACallee: TNPC_ASTExpression);
begin
  inherited Create(ALocation);
  &Type := AST_CALL;
  Callee := ACallee;
  Args := TObjectList<TNPC_ASTExpression>.Create(True);
end;

destructor TNPC_ASTExpressionCall.Destroy;
begin
  if Callee <> Nil then
    Callee.Free;
  Args.Free;
  inherited;
end;

{ TNPC_ASTExpressionMember }

constructor TNPC_ASTExpressionMember.Create(const ALocation: TNPCLocation; AOwner: TNPC_ASTExpression; const AMember: UTF8String);
begin
  inherited Create(ALocation);
  &Type := AST_STRUCT_MEMBER;
  Owner := AOwner;
  Member := AMember;
end;

destructor TNPC_ASTExpressionMember.Destroy;
begin
  if Owner <> Nil then
    Owner.Free;
  Member := '';
  inherited;
end;

{ TNPC_ASTExpressionIndex }

constructor TNPC_ASTExpressionIndex.Create(const ALocation: TNPCLocation; ATarget, AIndex: TNPC_ASTExpression);
begin
  inherited Create(ALocation);
  &Type := AST_ARRAY_INDEX;
  Target := ATarget;
  Index := AIndex;
end;

destructor TNPC_ASTExpressionIndex.Destroy;
begin
  if Target <> Nil then
    Target.Free;
  if Index <> Nil then
    Index.Free;
  inherited;
end;

{ TNPC_ASTStatement }

constructor TNPC_ASTStatement.Create;
begin
  inherited;
  &Type := AST_STATEMENT;
  Flags := 0;
  Block := Nil;
  TypeDefinition := Nil;
  Expression := Nil;
end;

destructor TNPC_ASTStatement.Destroy;
begin
  Block := Nil;
  TypeDefinition := Nil;
  Expression := Nil;
  inherited;
end;

{ TNPC_ASTStatementBlock }

constructor TNPC_ASTStatementBlock.Create(const ALocation: TNPCLocation; AParent: TNPC_ASTStatementBlock; const AFlags: TNPC_BlockFlags = []);
begin
  inherited Create(ALocation);
  &Type := AST_BLOCK;
  ParentBlock := AParent;
  Stmts := TObjectList<TNPC_ASTStatement>.Create(True);
end;

destructor TNPC_ASTStatementBlock.Destroy;
begin
  Stmts.Free;
  inherited;
end;

procedure TNPC_ASTStatementBlock.AddStatement(const AStatement: TNPC_ASTStatement);
begin
  if AStatement is TNPC_ASTStatementBlock then
    TNPC_ASTStatementBlock(AStatement).ParentBlock := Self;
  Stmts.Add(AStatement);
end;

{ TNPC_ASTStatementExpression }

constructor TNPC_ASTStatementExpression.Create(const ALocation: TNPCLocation; AExpr: TNPC_ASTExpression);
begin
  inherited Create(ALocation);
  &Type := AST_EXPRESSION_STATEMENT;
  Expr := AExpr;
end;

destructor TNPC_ASTStatementExpression.Destroy;
begin
  if Expr <> Nil then
    Expr.Free;
  inherited;
end;

{ TNPC_ASTStatementTypeDecl }

constructor TNPC_ASTStatementTypeDecl.Create(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPC_ASTTypeExpression);
begin
  inherited Create(ALocation);
  &Type := AST_STATEMENT_TYPE_DECL;
  Name := AName;
  DeclaredType := AType;
end;

destructor TNPC_ASTStatementTypeDecl.Destroy;
begin
  Name := '';
  if DeclaredType <> Nil then
    DeclaredType.Free;
  inherited;
end;

{ TNPC_ASTStatementVarDecl }

constructor TNPC_ASTStatementVarDecl.Create(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPC_ASTTypeExpression; AInit: TNPC_ASTExpression);
begin
  inherited Create(ALocation);
  &Type := AST_STATEMENT_VAR_DECL;
  Name := AName;
  DeclaredType := AType; // used instead of String TypeName
  Init := AInit; // optional initializer
  SymbolRef := Nil; // symbol created when parsing
end;

destructor TNPC_ASTStatementVarDecl.Destroy;
begin
  Name := '';
  if DeclaredType <> Nil then
    DeclaredType.Free;
  if Init <> Nil then
    Init.Free;
  if SymbolRef <> Nil then
    SymbolRef.Free;
  inherited;
end;

{ TNPC_ASTStatementLabel }

constructor TNPC_ASTStatementLabel.Create(const ALocation: TNPCLocation; const AIdent: UTF8String; AStmt: TNPC_ASTStatement);
begin
  inherited Create(ALocation);
  &Type := AST_LABEL;
  Ident := AIdent;
  Statement := AStmt;
end;

destructor TNPC_ASTStatementLabel.Destroy;
begin
  Ident := '';
  if Statement <> Nil then
    Statement.Free;
  inherited;
end;

{ TNPC_ASTStatementAssign }

constructor TNPC_ASTStatementAssign.Create(const ALocation: TNPCLocation; ATarget, AValue: TNPC_ASTExpression);
begin
  inherited Create(ALocation);
  &Type := AST_ASSIGN;
  Target := ATarget; // usually TIdentExpr or member/index
  Value := AValue;
end;

destructor TNPC_ASTStatementAssign.Destroy;
begin
  if Target <> Nil then
    Target.Free;
  if Value <> Nil then
    Value.Free;
  inherited;
end;

{ TNPC_ASTStatementIf }

constructor TNPC_ASTStatementIf.Create(const ALocation: TNPCLocation; ACond: TNPC_ASTExpression; AThen, AElse: TNPC_ASTStatement);
begin
  inherited Create(ALocation);
  &Type := AST_IF;
  Cond := ACond;
  ThenStmt := AThen;
  ElseStmt := AElse;
end;

destructor TNPC_ASTStatementIf.Destroy;
begin
  if Cond <> Nil then
    Cond.Free;
  if ThenStmt <> Nil then
    ThenStmt.Free;
  if ElseStmt <> Nil then
    ElseStmt.Free;
  inherited;
end;

{ TNPC_ASTStatementCase }

destructor TNPC_ASTStatementCase.Destroy;
begin

  inherited;
end;

{ TNPC_ASTStatementFor }

constructor TNPC_ASTStatementFor.Create(const ALocation: TNPCLocation; const AVarName: UTF8String; AInitExpr, AEndExpr: TNPC_ASTExpression; const ADownTo: Boolean;
  ABody: TNPC_ASTStatement);
begin
  inherited Create(ALocation);
  &Type := AST_FOR;
  VarName := AVarName;
  InitExpr := AInitExpr;
  EndExpr := AEndExpr;
  Reverse := ADownTo;
  Body := ABody;
end;

destructor TNPC_ASTStatementFor.Destroy;
begin
  VarName := '';
  if InitExpr <> Nil then
    InitExpr.Free;
  if EndExpr <> Nil then
    EndExpr.Free;
  if Body <> Nil then
    Body.Free;
  inherited;
end;

{ TNPC_ASTStatementWhile }

constructor TNPC_ASTStatementWhile.Create(const ALocation: TNPCLocation; ACond: TNPC_ASTExpression; ABody: TNPC_ASTStatement);
begin
  inherited Create(ALocation);
  &Type := AST_WHILE;
  Cond := ACond;
  Body := ABody;
end;

destructor TNPC_ASTStatementWhile.Destroy;
begin
  if Cond <> Nil then
    Cond.Free;
  if Body <> Nil then
    Body.Free;
  inherited;
end;

end.

