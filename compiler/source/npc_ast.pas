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
  System.Generics.Collections,
  npc_location;

type
  TNPC_ASTTypeExpression = class;

  TNPCSymbolKind = (
    skType,
    skDecl, // const, var
    skProc  // function, procedure
  );

  TNPCSymbol = class
  public
    Name: String;
    Kind: TNPCSymbolKind;
    TypeRef: TNPC_ASTTypeExpression;
    DeclNode: TObject; // pointer/reference to the declaration AST node (optional)
    ConstValue: Integer; // used if Kind=skConst
    //
    constructor Create(const AName: string; AKind: TNPCSymbolKind; AType: TNPC_ASTTypeExpression; ADecl: TObject; AValue: Integer = 0);
  end;

  TSymbol = class
  public
    Name: string;
    Kind: TNPCSymbolKind;
    TypeRef: TNPC_ASTTypeExpression;
    DeclNode: TObject; // pointer/reference to the declaration AST node (optional)
    ConstValue: Integer; // used if Kind=skConst
//    constructor Create(const AName: string; AKind: TSymbolKind; ADecl: TObject);
    constructor Create(const AName: string; AKind: TNPCSymbolKind; AType: TNPC_ASTTypeExpression; ADecl: TObject; AValue: Integer = 0);
  end;

  TNPCListEntry = packed record
    Name: String;
    Symbol: TNPCSymbol;
  end;

  TNPCScope = class // TDictionary<string, TSymbol>;
  private
//    List: Array of TNPCListEntry;
//    Size: Integer;
//    Capacity: Integer;
    Table: TDictionary<String, TSymbol>;
    Parent: TNPCScope;
    //
//    procedure Init;
  public
    constructor Create(AParent: TNPCScope);
    destructor Destroy; override;
    //
//    procedure Clear;
//    function Exists(const AName: String): Boolean;
//    procedure Add(const AName: String; const ASymbol: TNPCSymbol);
//    function TryGetValue(const AName: String; out ASymbol: TNPCSymbol): Boolean;
    //
    function Resolve(const Name: string): TSymbol;
    //
    procedure DefineType(const Name: string; AType: TNPC_ASTTypeExpression);
    procedure DefineConst(const Name: string; AValue: Integer; AType: TNPC_ASTTypeExpression);
    procedure DefineVar(const Name: string; AType: TNPC_ASTTypeExpression);
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

    AST_ENUM,
    AST_TYPE_DEFINITION,
    AST_TYPE_DECLARATION,
    AST_RECORD_DESCRIPTION,

    AST_PROCEDURE,
    AST_PROCEDURE_CALL,

    AST_LABEL,
    AST_ASSIGN,
    AST_IF,
    AST_CASE,
    AST_BREAK,
    AST_CONTINUE,

    AST_FOR,
    AST_WHILE,
    AST_REPEAT_UNTIL,

    AST_ARRAY,
    AST_ARRAY_SLICE,

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
    constructor Create; virtual;
    destructor Destroy; override;
    //
    function ToString: String; virtual;
  end;

  TNPC_ASTExpression = class(TNPC_AST)
  public
    // &Type = AST_EXPRESSION
    InferredType: TNPC_ASTTypeExpression; //TNPC_ASTTypeDefinition;
    //
    constructor Create; override;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTTypeDefinition = class(TNPC_ASTExpression)
  public
    // &Type = AST_TYPE_DEFINITION
    Flags: LongWord;
    //
    constructor Create;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  // Types

  TNPC_ASTTypeExpression = class(TNPC_ASTExpression)
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

//  TTypeName = class(TNPC_ASTTypeExpression)
//  public
//    Name: string;
//    constructor Create(const AName: string);
//  end;

  TNPC_ASTTypeEnum = class(TNPC_ASTTypeExpression)
  public
    Members: TDictionary<string, Integer>;
    //
    constructor Create;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTTypeSet = class(TNPC_ASTTypeExpression)
  public
    ElementType: TNPC_ASTTypeExpression;
    //
    constructor Create;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTTypeArray = class(TNPC_ASTTypeExpression)
  public
    ElementType: TNPC_ASTTypeExpression;
    IndexType: TNPC_ASTTypeExpression;
    //
    constructor Create(AElem: TNPC_ASTTypeExpression);
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTTypeRecord = class(TNPC_ASTTypeExpression)
  public
    Fields: TDictionary<string, TNPC_ASTTypeExpression>;
    //
    constructor Create;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  // Expressions

  TNPC_ASTExpressionLiteral = class(TNPC_ASTExpression)
  public
    Value: string;
    LiteralType: TNPC_ASTTypeExpression;
    //
    constructor Create(const AValue: string; AType: TNPC_ASTTypeExpression);
    //
    function ToString: String; override;
  end;

  TNPC_ASTExpressionEnumConst = class(TNPC_ASTExpression)
  public
    Name: string;
    Value: Integer;
    EnumType: TNPC_ASTTypeExpression;
    //
    constructor Create(const AName: string; AVal: Integer; AType: TNPC_ASTTypeExpression);
    //
    function ToString: String; override;
  end;

  // set literal: [Red, Green, Blue]
  TNPC_ASTExpressionSetLiteral = class(TNPC_ASTExpression)
  public
    Elements: TList<TNPC_ASTExpression>;
    SetType: TNPC_ASTTypeSet;
    //
    constructor Create(AType: TNPC_ASTTypeSet);
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTExpressionIdent = class(TNPC_ASTExpression)
  public
    Name: string;
    ResolvedSymbol: TSymbol; // filled by resolution pass
    //
    constructor Create(const AName: string);
    //
    function ToString: string; override;
  end;

  TNPC_ASTExpressionNumber = class(TNPC_ASTExpression)
  public
    Value: string;
    //
    constructor Create(const AValue: string);
    //
    function ToString: string; override;
  end;

  TNPC_ASTExpressionString = class(TNPC_ASTExpression)
  public
    Value: string;
    //
    constructor Create(const AValue: string);
    //
    function ToString: string; override;
  end;

  TNPC_ASTExpressionUnary = class(TNPC_ASTExpression)
  public
    Op: string;
    Right: TNPC_ASTExpression;
    //
    constructor Create(const AOp: string; ARight: TNPC_ASTExpression);
    destructor Destroy; override;
    //
    function ToString: string; override;
  end;

  TNPC_ASTExpressionBinary = class(TNPC_ASTExpression)
  public
    Op: string;
    Left,
    Right: TNPC_ASTExpression;
    //
    constructor Create(ALeft: TNPC_ASTExpression; const AOp: string; ARight: TNPC_ASTExpression);
    destructor Destroy; override;
    //
    function ToString: string; override;
  end;

  // membership test: X in [..]
  TNPC_ASTExpressionInOp = class(TNPC_ASTExpressionBinary)
  public
    //Left: TNPC_ASTExpression;
    //Right: TNPC_ASTExpression; // must be set
    //
    //constructor Create(ALeft, ARight: TNPC_ASTExpression);
    //
    function ToString: String; override;
  end;

  TNPC_ASTExpressionCall = class(TNPC_ASTExpression)
  public
    Callee: TNPC_ASTExpression;
    Args: TObjectList<TNPC_ASTExpression>;
    //
    constructor Create(ACallee: TNPC_ASTExpression);
    destructor Destroy; override;
    //
    function ToString: string; override;
  end;

  TNPC_ASTExpressionMember = class(TNPC_ASTExpression) // e.g. a.b
  public
    Owner: TNPC_ASTExpression;
    Member: string;
    //
    constructor Create(AOwner: TNPC_ASTExpression; const AMember: string);
    destructor Destroy; override;
    //
    function ToString: string; override;
  end;

  TNPC_ASTExpressionIndex = class(TNPC_ASTExpression) // e.g. a[0]
  public
    Target: TNPC_ASTExpression;
    Index: TNPC_ASTExpression;
    //
    constructor Create(ATarget, AIndex: TNPC_ASTExpression);
    destructor Destroy; override;
    //
    function ToString: string; override;
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
    constructor Create; override;
    destructor Destroy; override;
    //
    function ToString: string; override;
  end;

  TNPC_ASTStatementBlock = class(TNPC_ASTStatement)
  public
    Stmts: TObjectList<TNPC_ASTStatement>;
    //
    constructor Create;
    destructor Destroy; override;
    //
    function ToString: string; override;
  end;

  TNPC_ASTStatementExpression = class(TNPC_ASTStatement)
  public
    Expr: TNPC_ASTExpression;
    //
    constructor Create(AExpr: TNPC_ASTExpression);
    destructor Destroy; override;
    //
    function ToString: string; override;
  end;

  TNPC_ASTStatementTypeDecl = class(TNPC_ASTStatement)
  public
    Name: string;
    DeclType: TNPC_ASTTypeExpression;
    //
    constructor Create(const AName: string; AType: TNPC_ASTTypeExpression);
    //
    function ToString: string; override;
  end;

  TNPC_ASTStatementVarDecl = class(TNPC_ASTStatement)
  public
    Name: string;
    DeclaredType: TNPC_ASTTypeExpression;   // used instead of string TypeName
    Init: TNPC_ASTExpression; // optional initializer
    SymbolRef: TSymbol; // symbol created when parsing
    //
    constructor Create(const AName: string; AType: TNPC_ASTTypeExpression; AInit: TNPC_ASTExpression);
    destructor Destroy; override;
    //
    function ToString: string; override;
  end;

  TNPC_ASTStatementAssign = class(TNPC_ASTStatement)
  public
    Target: TNPC_ASTExpression; // usually TIdentExpr or member/index
    Value: TNPC_ASTExpression;
    //
    constructor Create(ATarget, AValue: TNPC_ASTExpression);
    destructor Destroy; override;
    //
    function ToString: string; override;
  end;

  TNPC_ASTStatementIf = class(TNPC_ASTStatement)
  public
    Cond: TNPC_ASTExpression;
    ThenStmt,
    ElseStmt: TNPC_ASTStatement;
    //
    constructor Create(ACond: TNPC_ASTExpression; AThen, AElse: TNPC_ASTStatement);
    destructor Destroy; override;
    //
    function ToString: string; override;
  end;

  TNPC_ASTStatementWhile = class(TNPC_ASTStatement)
  public
    Cond: TNPC_ASTExpression;
    Body: TNPC_ASTStatement;
    //
    constructor Create(ACond: TNPC_ASTExpression; ABody: TNPC_ASTStatement);
    destructor Destroy; override;
    //
    function ToString: string; override;
  end;

implementation

{ TNPC_AST }

constructor TNPC_AST.Create;
begin
  &Type := AST_UNKNOWN;
  ASTFlags := 0;
  Location := Nil;
end;

destructor TNPC_AST.Destroy;
begin
  &Type := AST_UNKNOWN;
  ASTFlags := 0;
  if Location <> Nil then
    FreeAndNil(Location);
  inherited;
end;

end.

