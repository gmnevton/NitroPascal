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
  Variants,
  Generics.Collections,
  npc_location;

type
  TNPC_ASTValue = Variant; // for simplicity

  TNPC_ASTTypeExpression = class;
  TNPC_ASTStatementProcedure = class;

  TNPCSymbolKind = (
    skBuiltinType,
    skType,
    skVar, // const
    skProc
  );

  TNPCSymbolType = (
    stUnresolved,
    stEnum, // ( e1, e2, ... )
    stEnumConst, // e1, e2, ...
    stSet, // [ s1, s2, s3..s4, ... ]
    stSetConst, // s1, s2, ...
    stBitSet, // 64 bit int
    stArray, // array[] of ...
    stRecord, // record ... end
    stRecordField, // ident: ident;
    stPointer, // 32 / 64 bit value
    stLiteral, // ident / number / string / char
    stProcedure, // pointer
    stForeignFunction // pointer
  );

  TNPCSymbol = class
  public
    Name: UTF8String;
    Kind: TNPCSymbolKind;
    &Type: TNPCSymbolType;
    TypeRef: TNPC_ASTTypeExpression;
    DeclNode: TObject; // pointer/reference to the declaration AST node (optional)
    Size: Integer;
    IsConst: Boolean;
    ConstValue: TNPC_ASTValue; // used if Kind=skDecl and IsConst=True
    //
    constructor Create(const AName: UTF8String; AKind: TNPCSymbolKind; AType: TNPCSymbolType; AConst: Boolean; ATypeRef: TNPC_ASTTypeExpression; ADecl: TObject; ASize: Integer = -1; AValue: Integer = 0);
    destructor Destroy; override;
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
    procedure AddOrSetValue(const AName: UTF8String; ASymbol: TNPCSymbol);
    function TryGetValue(const AName: UTF8String; out ASymbol: TNPCSymbol): Boolean;
    function Resolve(const AName: UTF8String): TNPCSymbol;
    //
    function DefineBuiltinType(const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTTypeExpression): TNPCSymbol;
    function DefineType(const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTTypeExpression; ADecl: TObject): TNPCSymbol;
    function DefineConst(const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTTypeExpression; ADecl: TObject; AValue: Integer): TNPCSymbol;
    function DefineVar(const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTTypeExpression; ADecl: TObject): TNPCSymbol;
    function DefineProcedure(const AName: UTF8String; const AProcedure: TNPC_ASTStatementProcedure): TNPCSymbol;
  end;

  TNPC_ASTType = (
    AST_UNKNOWN,

    // nodes
    AST_BLOCK,
    AST_EXPRESSION,
    AST_STATEMENT,
    AST_STATEMENTS,
    AST_COMPOUND_STATEMENT,
    AST_SCOPE_STATEMENT,
    AST_EXPRESSION_STATEMENT,

    // subnodes
    AST_IDENTIFIER,
    AST_LITERAL,
    AST_DECLARATION,

    // ops
    AST_UNARY,
    AST_BINARY,
    AST_TERNARY,

    // types
    AST_TYPE_DEFINITION,
    AST_TYPE_NAME,
    AST_TYPE_DECLARATION,
    AST_TYPE_RECORD_DESCRIPTION,
    AST_TYPE_ENUM,
    AST_TYPE_SET,
    AST_TYPE_BITSET,
    AST_TYPE_ARRAY,
    AST_TYPE_ARRAY_SLICE,
    AST_TYPE_RECORD,

    // expressions
    AST_EXPRESSION_LITERAL, // AST_LITERAL ???
//    AST_IDENT, // AST_LITERAL ???
//    AST_NUMBER, // AST_LITERAL ???
    AST_EXPRESSION_STRING, // AST_LITERAL ???
    AST_EXPRESSION_ENUM,
    AST_EXPRESSION_ENUM_CONST,
    AST_EXPRESSION_SET,
    AST_EXPRESSION_SET_LITERAL, // AST_LITERAL ???
    AST_EXPRESSION_ARRAY,
    AST_EXPRESSION_ARRAY_SLICE,
    AST_EXPRESSION_RECORD,
    AST_EXPRESSION_ASSIGN,
    AST_EXPRESSION_IF, // ternary, eg: x := if cond then true_expr else false_expr;
    AST_EXPRESSION_CASE, // ternary, eg: x := case selector of branches_expr else default_expr end;
    AST_EXPRESSION_IN_OP,
    AST_EXPRESSION_STRUCT_MEMBER,
    AST_EXPRESSION_ARRAY_INDEX,

    AST_PROCEDURE,
    AST_CALL,
    AST_EXTERNAL_CALL,

    // statements
    AST_STATEMENT_LABEL,
    AST_STATEMENT_BITSET,
    AST_STATEMENT_ASSIGN,
    AST_STATEMENT_IF,
    AST_STATEMENT_CASE,
    AST_STATEMENT_BREAK,
    AST_STATEMENT_CONTINUE,
    AST_STATEMENT_FOR,
    AST_STATEMENT_WHILE,
    AST_STATEMENT_REPEAT_UNTIL,
    AST_STATEMENT_TYPE_DECL, // same as AST_TYPE_DECLARATION ???
    AST_STATEMENT_VAR_DECL, // same as AST_TYPE_DECLARATION ??

    // directives
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
//    InferredType: TNPC_ASTTypeExpression; //TNPC_ASTTypeDefinition;
    //
    constructor Create(const ALocation: TNPCLocation); override;
    destructor Destroy; override;
    //
    function Eval(Scope: TNPCScope): TNPC_ASTValue; virtual; abstract;
    function ToString: String; virtual;
  end;

  // Types

  TNPC_ASTTypeExpression = class(TNPC_ASTExpression)
  public
    // &Type = AST_TYPE_DECLARATION
    //
    constructor Create(const ALocation: TNPCLocation); override;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTTypeExpressionClassType = class of TNPC_ASTTypeExpression;

  TNPC_ASTTypeEnum = class;
  TNPC_ASTTypeSet = class;
  TNPC_ASTTypeArray = class;
  TNPC_ASTTypeRecord = class;

  TNPC_ASTExpressionSetLiteral = class;

  TNPC_TypeDefinitionType = (
    DEF_Unknown,
    DEF_Type,
    DEF_Enum,
    DEF_Set,
    DEF_BitSet,
    DEF_Array,
    DEF_Record,
    DEF_Pointer,
    DEF_Literal,
    DEF_Procedure,
    DEF_ProcedureArguments,
    DEF_ProcedureResult,
    DEF_ForeignFunction
  );

  TNPC_ASTTypeDefinition = class(TNPC_ASTTypeExpression)
  public
    // &Type = AST_TYPE_DEFINITION
    Flags: LongWord;
    DefinitionType: TNPC_TypeDefinitionType;
    Name: UTF8String;
    SizeInBytes: Integer;
    EnumDescription: TNPC_ASTTypeEnum;
    SetDescription: TNPC_ASTTypeSet; // TNPC_ASTExpressionSetLiteral; // TNPC_ASTTypeSet;
    ArrayDescription: TNPC_ASTTypeArray;
    RecordDescription: TNPC_ASTTypeRecord;
    //
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String; ASizeInBytes: Integer); reintroduce;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTTypeName = class(TNPC_ASTTypeExpression)
  public
    // &Type = AST_TYPE_NAME
    Name: UTF8String;
    //
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String); reintroduce;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTTypeEnum = class(TNPC_ASTTypeExpression)
  public
    // &Type = AST_TYPE_ENUM
    Members: TDictionary<UTF8String, Integer>;
    //
    constructor Create(const ALocation: TNPCLocation); override;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTTypeSet = class(TNPC_ASTTypeExpression)
  public
    // &Type = AST_TYPE_SET
    ElementType: TNPC_ASTTypeExpression;
    Elements: TObjectList<TNPC_ASTExpression>;
    //
    constructor Create(const ALocation: TNPCLocation; const AElementType: TNPC_ASTTypeExpression); reintroduce;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTTypeArray = class(TNPC_ASTTypeExpression)
  public
    // &Type = AST_TYPE_ARRAY
    ElementType: TNPC_ASTTypeExpression;
    IndexType: TNPC_ASTTypeExpression;
    //
    constructor Create(const ALocation: TNPCLocation; AElementType, AIndexType: TNPC_ASTTypeExpression); reintroduce;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTTypeRecord = class(TNPC_ASTTypeExpression)
  public
    // &Type = AST_TYPE_RECORD_DESCRIPTION
    Fields: TDictionary<UTF8String, TNPC_ASTTypeExpression>;
    //
    constructor Create(const ALocation: TNPCLocation); override;
    destructor Destroy; override;
    //
    function ToString: String; override;
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
    //
    function ToString: String; override;
  end;

//  TNPC_ASTStatementEnum = class(TNPC_ASTStatement)
//  public
//    // &Type = AST_ENUM
//    ElementType: TNPC_ASTTypeExpression;
//    Elements: TObjectList<TNPC_ASTExpression>;
//    //
//    constructor Create(const ALocation: TNPCLocation; const AElementType: TNPC_ASTTypeExpression); reintroduce;
//    destructor Destroy; override;
//    //
//    function ToString: String; override;
//  end;

  TNPC_ASTExpressionEnumConst = class(TNPC_ASTExpression)
  public
    // &Type = AST_ENUM_CONST
    Name: UTF8String;
    Value: Integer;
    EnumType: TNPC_ASTTypeExpression;
    //
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String; AValue: Integer; AType: TNPC_ASTTypeExpression); reintroduce;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTExpressionSet = class(TNPC_ASTExpression)
  public
    // &Type = AST_EXPRESSION_SET
    ElementType: TNPC_ASTTypeExpression;
    Elements: TObjectList<TNPC_ASTExpression>;
    //
    constructor Create(const ALocation: TNPCLocation; const AElementType: TNPC_ASTTypeExpression); reintroduce;
    destructor Destroy; override;
    //
    //function ToString: String; override;
  end;

  // set literal: [Red, Green, Blue]
  TNPC_ASTExpressionSetLiteral = class(TNPC_ASTExpression)
  public
    // &Type = AST_SET_LITERAL
    SetType: TNPC_ASTTypeSet;
    Elements: TObjectList<TNPC_ASTExpression>;
    //
    constructor Create(const ALocation: TNPCLocation; AType: TNPC_ASTTypeSet); reintroduce;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTExpressionVariable = class(TNPC_ASTExpression)
  public
    // &Type = AST_DECLARATION
    Name: UTF8String;
    Symbol: TNPCSymbol; // resolved symbol during parsing / semantic analysis
    //
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String; ASymbol: TNPCSymbol); reintroduce;
    destructor Destroy; override;
    //
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
    function ToString: String; override;
  end;

  TNPC_ASTExpressionIdent = class(TNPC_ASTExpression)
  public
    // &Type = AST_IDENT
    Name: UTF8String;
    ResolvedSymbol: TNPCSymbol; // filled by resolution pass
    //
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String); reintroduce;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTExpressionNumber = class(TNPC_ASTExpression)
  public
    // &Type = AST_LITERAL
    Value: UTF8String;
    LiteralType: TNPC_ASTTypeExpression;
    //
    constructor Create(const ALocation: TNPCLocation; const AValue: UTF8String; AType: TNPC_ASTTypeExpression); reintroduce;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTExpressionString = class(TNPC_ASTExpression)
  public
    // &Type = AST_STRING
    Value: UTF8String;
    //
    constructor Create(const ALocation: TNPCLocation; const AValue: UTF8String); reintroduce;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTExpressionUnary = class(TNPC_ASTExpression)
  public
    // &Type = AST_UNARY
    Op: UTF8String;
    Right: TNPC_ASTExpression;
    //
    constructor Create(const ALocation: TNPCLocation; const AOp: UTF8String; ARight: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
    //
    function ToString: String; override;
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
    //
    function ToString: String; override;
  end;

  TNPC_ASTExpressionAssign = class(TNPC_ASTExpression)
  public
    // &Type = AST_EXPRESSION_ASSIGN
    Target: TNPC_ASTExpressionVariable; // usually TIdentExpr or member/index
    ValueExpr: TNPC_ASTExpression;
    //
    constructor Create(const ALocation: TNPCLocation; ATarget: TNPC_ASTExpressionVariable; AValue: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
    //
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
    function ToString: String; override;
  end;

  TNPC_ASTExpressionIf = class(TNPC_ASTExpression)
    // &Type = AST_EXPRESSION_IF
    Cond: TNPC_ASTExpression;
    ThenExpr,
    ElseExpr: TNPC_ASTExpression;
    //
    constructor Create(const ALocation: TNPCLocation; ACond: TNPC_ASTExpression; AThen, AElse: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
    //
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
    function ToString: String; override;
  end;

  TNPC_ASTExpressionCaseBranch = class
  public
    IfValues: TObjectList<TNPC_ASTExpression>; // list of constant expressions like 1, 2, 3
    ResultExpr: TNPC_ASTExpression; // expression for this branch
    //
    constructor Create;
    destructor Destroy; override;
  end;

  TNPC_ASTExpressionCaseBranches = TObjectList<TNPC_ASTExpressionCaseBranch>;

  TNPC_ASTExpressionCase = class(TNPC_ASTExpression)
  public
    // &Type = AST_EXPRESSION_CASE
    Selector: TNPC_ASTExpression;
    Branches: TNPC_ASTExpressionCaseBranches;
    DefaultExpr: TNPC_ASTExpression;
    //
    constructor Create(const ALocation: TNPCLocation; ASelector: TNPC_ASTExpression; ABranches: TNPC_ASTExpressionCaseBranches; ADefault: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
    //
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
    function ToString: String; override;
  end;

  // membership test: X in [..]
  TNPC_ASTExpressionInOp = class(TNPC_ASTExpressionBinary)
  public
    // &Type = AST_IN_OP
    Left: TNPC_ASTExpression;
    Right: TNPC_ASTExpression; // must be set
    //
    constructor Create(const ALocation: TNPCLocation; ALeft, ARight: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
    //
    function ToString: String; override;
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
    //
    function ToString: String; override;
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
    //
    function ToString: String; override;
  end;

  TNPC_ASTExpressionCall = class(TNPC_ASTExpression)
  public
    // &Type = AST_CALL
    Callee: TNPC_ASTExpression;
    Args: TObjectList<TNPC_ASTExpression>;
    //
    constructor Create(const ALocation: TNPCLocation; ACallee: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTExpressionMember = class(TNPC_ASTExpression) // e.g. a.b
  public
    // &Type = AST_STRUCT_MEMBER
    Owner: TNPC_ASTExpression;
    Member: UTF8String;
    MemberType: TNPC_ASTTypeExpression;
    //
    constructor Create(const ALocation: TNPCLocation; AOwner: TNPC_ASTExpression; const AMember: UTF8String; const AMemberType: TNPC_ASTTypeExpression); reintroduce;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTExpressionIndex = class(TNPC_ASTExpression) // e.g. a[0]
  public
    // &Type = AST_ARRAY_INDEX
    Target: TNPC_ASTExpression;
    Index: TNPC_ASTExpression;
    //
    constructor Create(const ALocation: TNPCLocation; ATarget, AIndex: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
    //
    function ToString: String; override;
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
    Statements: TObjectList<TNPC_ASTStatement>;
    //
    constructor Create(const ALocation: TNPCLocation; AParent: TNPC_ASTStatementBlock; const AFlags: TNPC_BlockFlags = []); reintroduce;
    destructor Destroy; override;
    //
    procedure AddStatement(const AStatement: TNPC_ASTStatement);
  end;

  TNPC_ASTStatementExpression = class(TNPC_ASTStatement)
  public
    // &Type = AST_EXPRESSION_STATEMENT
    Expression: TNPC_ASTExpression;
    //
    constructor Create(const ALocation: TNPCLocation; AExpr: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
  end;

  // type
  //   ident = type_definition
  TNPC_ASTStatementTypeDeclaration = class(TNPC_ASTStatement)
  public
    // &Type = AST_STATEMENT_TYPE_DECL
    Name: UTF8String;
    DeclaredType: TNPC_ASTTypeExpression;
    //
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPC_ASTTypeExpression); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTStatementVariableDeclaration = class(TNPC_ASTStatement)
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

  TNPC_ASTParamModifier = (pmNone, pmConst, pmVar, pmOut);

  TNPC_ASTParameter = class(TNPC_ASTStatementVariableDeclaration)
  public
    Modifier: TNPC_ASTParamModifier;
    //
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPC_ASTTypeExpression; AModifier: TNPC_ASTParamModifier; AInit: TNPC_ASTExpression); reintroduce;
  end;

  TNPC_ASTStatementProcedure = class(TNPC_ASTStatementBlock)
  public
    Name: UTF8String;
    IsFunction: Boolean;
    Parameters: TObjectList<TNPC_ASTParameter>;
    Returns: TObjectList<TNPC_ASTParameter>;
    Body: TNPC_ASTStatement;
    //
    constructor Create(const ALocation: TNPCLocation; AParent: TNPC_ASTStatementBlock; const AName: UTF8String; AIsFunction: Boolean; const AFlags: TNPC_BlockFlags = []); reintroduce;
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
    Target: TNPC_ASTExpression; // TNPC_ASTExpressionVariable // usually TIdentExpr or member/index
    Value: TNPC_ASTExpression;
    //
    constructor Create(const ALocation: TNPCLocation; ATarget: TNPC_ASTExpression{Variable}; AValue: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTStatementIf = class(TNPC_ASTStatement)
  public
    // &Type = AST_STATEMENT_IF
    Cond: TNPC_ASTExpression;
    ThenStmt,
    ElseStmt: TNPC_ASTStatement;
    //
    constructor Create(const ALocation: TNPCLocation; ACond: TNPC_ASTExpression; AThen, AElse: TNPC_ASTStatement); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTStatementCaseBranch = class
  public
    IfValues: TObjectList<TNPC_ASTExpression>; // list of constant expressions like 1, 2, 3
    Stmt: TNPC_ASTStatement; // body statement for this branch
    //
    constructor Create;
    destructor Destroy; override;
  end;

  TNPC_ASTStatementCaseBranches = TObjectList<TNPC_ASTStatementCaseBranch>;

  TNPC_ASTStatementCase = class(TNPC_ASTStatement)
  public
    // &Type = AST_CASE
    Selector: TNPC_ASTExpression;
    Branches: TNPC_ASTStatementCaseBranches;
    DefaultStmt: TNPC_ASTStatement;
    //
    constructor Create(const ALocation: TNPCLocation; ASelector: TNPC_ASTExpression; ABranches: TNPC_ASTStatementCaseBranches; ADefault: TNPC_ASTStatement); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTStatementFor = class(TNPC_ASTStatement)
  public
    // &Type = AST_STATEMENT_FOR
    //VarName: UTF8String;
    InitStmt: TNPC_ASTStatement;
    CondExpr: TNPC_ASTExpression;
    EndExpr: TNPC_ASTExpression;
    Reverse: Boolean;
    Body: TNPC_ASTStatement;
    //
    //constructor Create(const ALocation: TNPCLocation; const AVarName: UTF8String; AInitExpr, AEndExpr: TNPC_ASTExpression; const ADownTo: Boolean; ABody: TNPC_ASTStatement); reintroduce;
    constructor Create(const ALocation: TNPCLocation; AInitStmt: TNPC_ASTStatement; ACondExpr, AEndExpr: TNPC_ASTExpression; const ADownTo: Boolean; ABody: TNPC_ASTStatement); reintroduce;
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

constructor TNPCSymbol.Create(const AName: UTF8String; AKind: TNPCSymbolKind; AType: TNPCSymbolType; AConst: Boolean; ATypeRef: TNPC_ASTTypeExpression; ADecl: TObject; ASize: Integer = -1; AValue: Integer = 0);
begin
  inherited Create;
  Name := AName;
  Kind := AKind;
  &Type := AType;
  TypeRef := ATypeRef;
  DeclNode := ADecl;
  Size := ASize;
  IsConst := AConst;
  ConstValue := AValue;
end;

destructor TNPCSymbol.Destroy;
begin
  Name := '';
  TypeRef := Nil;
//  if TypeRef <> Nil then
//    TypeRef.Free;
  DeclNode := Nil;
  ConstValue := Unassigned;
  inherited;
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

procedure TNPCScope.AddOrSetValue(const AName: UTF8String; ASymbol: TNPCSymbol);
begin
  Table.AddOrSetValue(AName, ASymbol);
end;

function TNPCScope.TryGetValue(const AName: UTF8String; out ASymbol: TNPCSymbol): Boolean;
begin
  Result := Table.TryGetValue(AName, ASymbol);
end;

function TNPCScope.Resolve(const AName: UTF8String): TNPCSymbol;
begin
  if Table.TryGetValue(AName, Result) then
    Exit;
  if Assigned(Parent) then
    Exit(Parent.Resolve(AName));
  Result := Nil;
end;

function TNPCScope.DefineBuiltinType(const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTTypeExpression): TNPCSymbol;
begin
  Result := TNPCSymbol.Create(AName, skBuiltinType, AType, False, ATypeRef, Nil, Asize);
  Table.Add(AName, Result);
end;

function TNPCScope.DefineType(const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTTypeExpression; ADecl: TObject): TNPCSymbol;
begin
  Result := TNPCSymbol.Create(AName, skType, AType, False, ATypeRef, ADecl, ASize);
  Table.Add(AName, Result);
end;

function TNPCScope.DefineConst(const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTTypeExpression; ADecl: TObject; AValue: Integer): TNPCSymbol;
begin
  Result := TNPCSymbol.Create(AName, skVar, AType, True, ATypeRef, ADecl, ASize, AValue);
  Table.Add(AName, Result);
end;

function TNPCScope.DefineVar(const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTTypeExpression; ADecl: TObject): TNPCSymbol;
begin
  Result := TNPCSymbol.Create(AName, skVar, AType, False, ATypeRef, ADecl, ASize);
  Table.Add(AName, Result);
end;

function TNPCScope.DefineProcedure(const AName: UTF8String; const AProcedure: TNPC_ASTStatementProcedure): TNPCSymbol;
begin
  Result := TNPCSymbol.Create(AName, skProc, stProcedure, False, Nil, AProcedure);
  Table.Add(AName, Result);
end;

{ TNPC_AST }

constructor TNPC_AST.Create(const ALocation: TNPCLocation);
begin
  &Type := AST_UNKNOWN;
  ASTFlags := 0;
  Location := Nil;
  if ALocation <> Nil then
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
//  //
//  InferredType := Nil;
end;

destructor TNPC_ASTExpression.Destroy;
begin
//  InferredType := Nil;
  inherited;
end;

function TNPC_ASTExpression.ToString: String;
begin
  Result := '<???>';
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

function TNPC_ASTTypeExpression.ToString: String;
begin
  Result := '<type>';
end;

{ TNPC_ASTTypeDefinition }

constructor TNPC_ASTTypeDefinition.Create(const ALocation: TNPCLocation; const AName: UTF8String; ASizeInBytes: Integer);
begin
  inherited Create(ALocation);
  &Type := AST_TYPE_DEFINITION;
  Name := AName;
  SizeInBytes := ASizeInBytes;
  EnumDescription := Nil;
  SetDescription := Nil;
  ArrayDescription := Nil;
  RecordDescription := Nil;
end;

destructor TNPC_ASTTypeDefinition.Destroy;
begin
  Name := '';
  if EnumDescription <> Nil then
    EnumDescription.Free;
  if SetDescription <> Nil then
    SetDescription.Free;
  if ArrayDescription <> Nil then
    ArrayDescription.Free;
  if RecordDescription <> Nil then
    RecordDescription.Free;
  inherited;
end;

function TNPC_ASTTypeDefinition.ToString: String;
begin
  Result := '<type-def>';
end;

{ TNPC_ASTTypeName }

constructor TNPC_ASTTypeName.Create(const ALocation: TNPCLocation; const AName: UTF8String);
begin
  inherited Create(ALocation);
  &Type := AST_TYPE_NAME;
  Name := AName;
end;

destructor TNPC_ASTTypeName.Destroy;
begin
  Name := '';
  inherited;
end;

function TNPC_ASTTypeName.ToString: String;
begin
  Result := '<type-name>';
end;

{ TNPC_ASTTypeEnum }

constructor TNPC_ASTTypeEnum.Create;
begin
  inherited;
  &Type := AST_TYPE_ENUM;
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

function TNPC_ASTTypeEnum.ToString: String;
begin
  Result := '<enum>';
end;

{ TNPC_ASTTypeSet }

constructor TNPC_ASTTypeSet.Create(const ALocation: TNPCLocation; const AElementType: TNPC_ASTTypeExpression);
begin
  inherited Create(ALocation);
  &Type := AST_TYPE_SET;
  ElementType := AElementType;
  Elements := TObjectList<TNPC_ASTExpression>.Create(False);
end;

destructor TNPC_ASTTypeSet.Destroy;
var
  i: Integer;
begin
  if ElementType <> Nil then
    ElementType.Free;
  for i := 0 to Elements.Count - 1 do
    Elements.Items[i].Free;
  Elements.Free;
  inherited;
end;

function TNPC_ASTTypeSet.ToString: String;
begin
  Result := '<set>';
end;

{ TNPC_ASTTypeArray }

constructor TNPC_ASTTypeArray.Create(const ALocation: TNPCLocation; AElementType, AIndexType: TNPC_ASTTypeExpression);
begin
  inherited Create(ALocation);
  &Type := AST_TYPE_ARRAY;
  ElementType := AElementType;
  IndexType := AIndexType;
end;

destructor TNPC_ASTTypeArray.Destroy;
begin
  ElementType := Nil;
  IndexType := Nil;
  inherited;
end;

function TNPC_ASTTypeArray.ToString: String;
begin
  Result := '<array>';
end;

{ TNPC_ASTTypeRecord }

constructor TNPC_ASTTypeRecord.Create;
begin
  inherited;
  &Type := AST_TYPE_RECORD_DESCRIPTION;
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

function TNPC_ASTTypeRecord.ToString: String;
begin
  Result := '<record>';
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

function TNPC_ASTExpressionLiteral.ToString: String;
begin
  Result := '<literal>';
end;

{ TNPC_ASTExpressionEnumConst }

constructor TNPC_ASTExpressionEnumConst.Create(const ALocation: TNPCLocation; const AName: UTF8String; AValue: Integer; AType: TNPC_ASTTypeExpression);
begin
  inherited Create(ALocation);
  &Type := AST_EXPRESSION_ENUM_CONST;
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

function TNPC_ASTExpressionEnumConst.ToString: String;
begin
  Result := '<enum-const>';
end;

{ TNPC_ASTExpressionSet }

constructor TNPC_ASTExpressionSet.Create(const ALocation: TNPCLocation; const AElementType: TNPC_ASTTypeExpression);
begin
  inherited Create(ALocation);
  &Type := AST_EXPRESSION_SET;
  ElementType := AElementType;
  Elements := TObjectList<TNPC_ASTExpression>.Create(False);
end;

destructor TNPC_ASTExpressionSet.Destroy;
var
  i: Integer;
begin
  if ElementType <> Nil then
    ElementType.Free;
  for i := 0 to Elements.Count - 1 do
    Elements.Items[i].Free;
  Elements.Free;
  inherited;
end;

{ TNPC_ASTExpressionSetLiteral }

constructor TNPC_ASTExpressionSetLiteral.Create(const ALocation: TNPCLocation; AType: TNPC_ASTTypeSet);
begin
  inherited Create(ALocation);
  &Type := AST_EXPRESSION_SET_LITERAL;
  SetType := AType;
  Elements := TObjectList<TNPC_ASTExpression>.Create(False);
end;

destructor TNPC_ASTExpressionSetLiteral.Destroy;
var
  i: Integer;
begin
  if SetType <> Nil then
    SetType.Free;
  for i := 0 to Elements.Count - 1 do
    Elements.Items[i].Free;
  Elements.Free;
  inherited;
end;

function TNPC_ASTExpressionSetLiteral.ToString: String;
begin
  Result := '<set-literal>';
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

function TNPC_ASTExpressionVariable.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  if not Assigned(Symbol) then
    Symbol := Scope.Resolve(Name);
  Result := Symbol.ConstValue;
end;

function TNPC_ASTExpressionVariable.ToString: String;
begin
  Result := '<variable>';
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

function TNPC_ASTExpressionIdent.ToString: String;
begin
  Result := '<identifier>';
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

function TNPC_ASTExpressionNumber.ToString: String;
begin
  Result := '<number>';
end;

{ TNPC_ASTExpressionString }

constructor TNPC_ASTExpressionString.Create(const ALocation: TNPCLocation; const AValue: UTF8String);
begin
  inherited Create(ALocation);
  &Type := AST_EXPRESSION_STRING;
  Value := AValue;
end;

destructor TNPC_ASTExpressionString.Destroy;
begin
  Value := '';
  inherited;
end;

function TNPC_ASTExpressionString.ToString: String;
begin
  Result := '<string>';
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

function TNPC_ASTExpressionUnary.ToString: String;
begin
  Result := '<unary>';
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

function TNPC_ASTExpressionBinary.ToString: String;
begin
  Result := '<binary>';
end;

{ TNPC_ASTExpressionAssign }

constructor TNPC_ASTExpressionAssign.Create(const ALocation: TNPCLocation; ATarget: TNPC_ASTExpressionVariable; AValue: TNPC_ASTExpression);
begin
  inherited Create(ALocation);
  &Type := AST_EXPRESSION_ASSIGN;
  Target := ATarget; // usually TIdentExpr or member/index
  ValueExpr := AValue;
end;

destructor TNPC_ASTExpressionAssign.Destroy;
begin
  if Target <> Nil then
    Target.Free;
  if ValueExpr <> Nil then
    ValueExpr.Free;
  inherited;
end;

function TNPC_ASTExpressionAssign.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  Result := ValueExpr.Eval(Scope);
  Target.Symbol.ConstValue := Result;
end;

function TNPC_ASTExpressionAssign.ToString: String;
begin
  Result := '<assign-expr>';
end;

{ TNPC_ASTExpressionIf }

constructor TNPC_ASTExpressionIf.Create(const ALocation: TNPCLocation; ACond, AThen, AElse: TNPC_ASTExpression);
begin
  inherited Create(ALocation);
  &Type := AST_EXPRESSION_IF;
  Cond := ACond;
  ThenExpr := AThen;
  ElseExpr := AElse;
end;

destructor TNPC_ASTExpressionIf.Destroy;
begin
  if Cond <> Nil then
    Cond.Free;
  if ThenExpr <> Nil then
    ThenExpr.Free;
  if ElseExpr <> Nil then
    ElseExpr.Free;
  inherited;
end;

function TNPC_ASTExpressionIf.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  if Boolean(Cond.Eval(Scope)) then
    Result := ThenExpr.Eval(Scope)
  else if Assigned(ElseExpr) then
    Result := ElseExpr.Eval(Scope)
  else
    Result := Null; // or raise an error if missing
end;

function TNPC_ASTExpressionIf.ToString: String;
begin
  Result := '<if-expr>';
end;

{ TNPC_ASTExpressionCaseBranch }

constructor TNPC_ASTExpressionCaseBranch.Create;
begin
  IfValues := TObjectList<TNPC_ASTExpression>.Create(False);
  ResultExpr := Nil;
end;

destructor TNPC_ASTExpressionCaseBranch.Destroy;
var
  i: Integer;
begin
  for i := 0 to IfValues.Count - 1 do
    IfValues.Items[i].Free;
  IfValues.Free;
  if ResultExpr <> Nil then
    ResultExpr.Free;
  inherited;
end;

{ TNPC_ASTExpressionCase }

constructor TNPC_ASTExpressionCase.Create(const ALocation: TNPCLocation; ASelector: TNPC_ASTExpression; ABranches: TNPC_ASTExpressionCaseBranches; ADefault: TNPC_ASTExpression);
begin
  inherited Create(ALocation);
  &Type := AST_EXPRESSION_CASE;
  Selector := ASelector;
  Branches := ABranches; // TNPC_ASTCaseBranches.Create(True);
  DefaultExpr := ADefault;
end;

destructor TNPC_ASTExpressionCase.Destroy;
begin
  if Selector <> Nil then
    Selector.Free;
  if Branches <> Nil then
    Branches.Free;
  if DefaultExpr <> Nil then
    DefaultExpr.Free;
  inherited;
end;

function TNPC_ASTExpressionCase.Eval(Scope: TNPCScope): TNPC_ASTValue;
var
  SelValue, Val: TNPC_ASTValue;
  B: TNPC_ASTExpressionCaseBranch;
  MatchFound: Boolean;
  V: TNPC_ASTExpression;
begin
  SelValue := Selector.Eval(Scope);
  MatchFound := False;

  for B in Branches do begin
    for V in B.IfValues do begin
      Val := V.Eval(Scope);
      if Val = SelValue then begin
        Result := B.ResultExpr.Eval(Scope);
        Exit;
      end;
    end;
  end;

  // Else branch
  if Assigned(DefaultExpr) then
    Result := DefaultExpr.Eval(Scope)
  else
    Result := Null;
end;

function TNPC_ASTExpressionCase.ToString: String;
begin
  Result := '<case-expr>';
end;

{ TNPC_ASTExpressionInOp }

constructor TNPC_ASTExpressionInOp.Create(const ALocation: TNPCLocation; ALeft, ARight: TNPC_ASTExpression);
begin
  inherited Create(ALocation, ALeft, 'in', ARight);
  &Type := AST_EXPRESSION_IN_OP;
  Left := ALeft;
  Right := ARight;
end;

destructor TNPC_ASTExpressionInOp.Destroy;
begin
  if Left <> Nil then
    Left.Free;
  if Right <> Nil then
    Right.Free;
  inherited;
end;

function TNPC_ASTExpressionInOp.ToString: String;
begin
  Result := '<in-op>';
end;

{ TNPC_ASTExpressionArray }

constructor TNPC_ASTExpressionArray.Create(const ALocation: TNPCLocation; ABase, AIndex: TNPC_ASTExpression; AElemType: TNPC_ASTTypeExpression);
begin
  inherited Create(ALocation);
  &Type := AST_EXPRESSION_ARRAY;
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

function TNPC_ASTExpressionArray.ToString: String;
begin
  Result := '<array>';
end;

{ TNPC_ASTExpressionRecord }

constructor TNPC_ASTExpressionRecord.Create(const ALocation: TNPCLocation; ABase: TNPC_ASTExpression; const AFieldName: UTF8String; AFieldType: TNPC_ASTTypeExpression);
begin
  inherited Create(ALocation);
  &Type := AST_EXPRESSION_RECORD;
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

function TNPC_ASTExpressionRecord.ToString: String;
begin
  Result := '<record>';
end;

{ TNPC_ASTExpressionCall }

constructor TNPC_ASTExpressionCall.Create(const ALocation: TNPCLocation; ACallee: TNPC_ASTExpression);
begin
  inherited Create(ALocation);
  &Type := AST_CALL;
  Callee := ACallee;
  Args := TObjectList<TNPC_ASTExpression>.Create(False);
end;

destructor TNPC_ASTExpressionCall.Destroy;
var
  i: Integer;
begin
  if Callee <> Nil then
    Callee.Free;
  for i := 0 to Args.Count - 1 do
    Args.Items[i].Free;
  Args.Free;
  inherited;
end;

function TNPC_ASTExpressionCall.ToString: String;
begin
  Result := '<call>';
end;

{ TNPC_ASTExpressionMember }

constructor TNPC_ASTExpressionMember.Create(const ALocation: TNPCLocation; AOwner: TNPC_ASTExpression; const AMember: UTF8String; const AMemberType: TNPC_ASTTypeExpression);
begin
  inherited Create(ALocation);
  &Type := AST_EXPRESSION_STRUCT_MEMBER;
  Owner := AOwner;
  Member := AMember;
  MemberType := AMemberType;
end;

destructor TNPC_ASTExpressionMember.Destroy;
begin
  if Owner <> Nil then
    Owner.Free;
  Member := '';
  if MemberType <> Nil then
    MemberType.Free;
  inherited;
end;

function TNPC_ASTExpressionMember.ToString: String;
begin
  Result := '<member>';
end;

{ TNPC_ASTExpressionIndex }

constructor TNPC_ASTExpressionIndex.Create(const ALocation: TNPCLocation; ATarget, AIndex: TNPC_ASTExpression);
begin
  inherited Create(ALocation);
  &Type := AST_EXPRESSION_ARRAY_INDEX;
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

function TNPC_ASTExpressionIndex.ToString: String;
begin
  Result := '<index>';
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
  Statements := TObjectList<TNPC_ASTStatement>.Create(False);
end;

destructor TNPC_ASTStatementBlock.Destroy;
var
  i: Integer;
begin
  ParentBlock := Nil;
  for i := 0 to Statements.Count - 1 do
    Statements.Items[i].Free;
  Statements.Free;
  inherited;
end;

procedure TNPC_ASTStatementBlock.AddStatement(const AStatement: TNPC_ASTStatement);
begin
  if AStatement is TNPC_ASTStatementBlock then
    TNPC_ASTStatementBlock(AStatement).ParentBlock := Self;
  Statements.Add(AStatement);
end;

{ TNPC_ASTStatementExpression }

constructor TNPC_ASTStatementExpression.Create(const ALocation: TNPCLocation; AExpr: TNPC_ASTExpression);
begin
  inherited Create(ALocation);
  &Type := AST_EXPRESSION_STATEMENT;
  Expression := AExpr;
end;

destructor TNPC_ASTStatementExpression.Destroy;
begin
  if Expression <> Nil then
    Expression.Free;
  inherited;
end;

{ TNPC_ASTStatementTypeDeclaration }

constructor TNPC_ASTStatementTypeDeclaration.Create(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPC_ASTTypeExpression);
begin
  inherited Create(ALocation);
  &Type := AST_STATEMENT_TYPE_DECL;
  Name := AName;
  DeclaredType := AType;
end;

destructor TNPC_ASTStatementTypeDeclaration.Destroy;
begin
  Name := '';
  if DeclaredType <> Nil then
    DeclaredType.Free;
  inherited;
end;

{ TNPC_ASTStatementVariableDeclaration }

constructor TNPC_ASTStatementVariableDeclaration.Create(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPC_ASTTypeExpression; AInit: TNPC_ASTExpression);
begin
  inherited Create(ALocation);
  &Type := AST_STATEMENT_VAR_DECL;
  Name := AName;
  DeclaredType := AType; // used instead of String TypeName
  Init := AInit; // optional initializer
  SymbolRef := Nil; // symbol created when parsing
end;

destructor TNPC_ASTStatementVariableDeclaration.Destroy;
begin
  Name := '';
  if DeclaredType <> Nil then
    DeclaredType.Free;
  if Init <> Nil then
    Init.Free;
  SymbolRef := Nil;
//  if SymbolRef <> Nil then
//    SymbolRef.Free;
  inherited;
end;

{ TNPC_ASTParameter }

constructor TNPC_ASTParameter.Create(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPC_ASTTypeExpression; AModifier: TNPC_ASTParamModifier;
  AInit: TNPC_ASTExpression);
begin
  inherited Create(ALocation, AName, AType, AInit);
  Modifier := AModifier;
end;

{ TNPC_ASTStatementProcedure }

constructor TNPC_ASTStatementProcedure.Create(const ALocation: TNPCLocation; AParent: TNPC_ASTStatementBlock; const AName: UTF8String; AIsFunction: Boolean; const AFlags: TNPC_BlockFlags = []);
begin
  inherited Create(ALocation, AParent, AFlags);
  Name := AName;
  IsFunction := AIsFunction;
  Parameters := TObjectList<TNPC_ASTParameter>.Create(False);
  Returns := Nil;
  Body := Nil;
  if IsFunction then
    Returns := TObjectList<TNPC_ASTParameter>.Create(False);
end;

destructor TNPC_ASTStatementProcedure.Destroy;
var
  i: Integer;
begin
  Name := '';
  for i := 0 to Parameters.Count - 1 do
    Parameters.Items[i].Free;
  Parameters.Free;
  if Returns <> Nil then begin
    for i := 0 to Returns.Count - 1 do
      Returns.Items[i].Free;
    Returns.Free;
  end;
  Body := Nil;
//  if Body <> Nil then
//    Body.Free;
  inherited;
end;

{ TNPC_ASTStatementLabel }

constructor TNPC_ASTStatementLabel.Create(const ALocation: TNPCLocation; const AIdent: UTF8String; AStmt: TNPC_ASTStatement);
begin
  inherited Create(ALocation);
  &Type := AST_STATEMENT_LABEL;
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

constructor TNPC_ASTStatementAssign.Create(const ALocation: TNPCLocation; ATarget: TNPC_ASTExpression{Variable}; AValue: TNPC_ASTExpression);
begin
  inherited Create(ALocation);
  &Type := AST_STATEMENT_ASSIGN;
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
  &Type := AST_STATEMENT_IF;
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

{ TNPC_ASTStatementCaseBranch }

constructor TNPC_ASTStatementCaseBranch.Create;
begin
  IfValues := TObjectList<TNPC_ASTExpression>.Create(False);
  Stmt := Nil;
end;

destructor TNPC_ASTStatementCaseBranch.Destroy;
var
  i: Integer;
begin
  for i := 0 to IfValues.Count - 1 do
    IfValues.Items[i].Free;
  IfValues.Free;
  if Stmt <> Nil then
    Stmt.Free;
  inherited;
end;

{ TNPC_ASTStatementCase }

constructor TNPC_ASTStatementCase.Create(const ALocation: TNPCLocation; ASelector: TNPC_ASTExpression; ABranches: TNPC_ASTStatementCaseBranches; ADefault: TNPC_ASTStatement);
begin
  inherited Create(ALocation);
  &Type := AST_STATEMENT_CASE;
  Selector := ASelector;
  Branches := ABranches; // TNPC_ASTCaseBranches.Create(True);
  DefaultStmt := ADefault;
end;

destructor TNPC_ASTStatementCase.Destroy;
begin
  if Selector <> Nil then
    Selector.Free;
  if Branches <> Nil then
    Branches.Free;
  if DefaultStmt <> Nil then
    DefaultStmt.Free;
  inherited;
end;

{ TNPC_ASTStatementFor }

constructor TNPC_ASTStatementFor.Create(const ALocation: TNPCLocation; AInitStmt: TNPC_ASTStatement; ACondExpr, AEndExpr: TNPC_ASTExpression; const ADownTo: Boolean; ABody: TNPC_ASTStatement);
begin
  inherited Create(ALocation);
  &Type := AST_STATEMENT_FOR;
  //VarName := AVarName;
  InitStmt := AInitStmt;
  CondExpr := ACondExpr;
  EndExpr := AEndExpr;
  Reverse := ADownTo;
  Body := ABody;
end;

destructor TNPC_ASTStatementFor.Destroy;
begin
//  VarName := '';
  if InitStmt <> Nil then
    InitStmt.Free;
  if CondExpr <> Nil then
    CondExpr.Free;
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
  &Type := AST_STATEMENT_WHILE;
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

