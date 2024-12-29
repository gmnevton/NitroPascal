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
  npc_location;

type
  TNPC_ASTType = (
    AST_UNKNOWN,

    AST_BLOCK,
    AST_EXPRESSION,
    AST_STATEMENT,

    AST_IDENTIFIER,
    AST_LITERAL,
    AST_DECLARATION,

    AST_UNARY,
    AST_BINARY,
//    AST_TERNARY,

    AST_ENUM,
    AST_TYPE_DEFINITION,
    AST_TYPE_DECLARATION,
    AST_RECORD_DESCRIPTION,

    AST_PROCEDURE,
    AST_PROCEDURE_CALL,

//    AST_WHILE,
    AST_IF,
    AST_BREAK,
    AST_CONTINUE,

    AST_FOR,
    AST_CASE,

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
  end;

  TNPC_ASTProcedure = class;
  TNPC_ASTDeclaration = class;
  TNPC_ASTEnum = class;
  TNPC_ASTIdentifier = class;
  TNPC_ASTStatement = class;
  TNPC_ASTExpression = class;
  TNPC_ASTTypeDefinition = class;
  TNPC_ASTTypeDeclaration = class;
  TNPC_ASTDirectiveENSURE = class;

  TNPC_BlockFlags = (
    BLOCK_ConsistsOfOrderedStatements = $1,
    BLOCK_DoesNotHaveResult = $2,
    BLOCK_BelongsToLoop = $4,
    BLOCK_BelongsToRecord = $8,
    BLOCK_HasBeenInferred = $10,
    BLOCK_HasBeenByteCoded = $20
  );

  TNPC_ASTBlock = class(TNPC_AST)
  public
    // &Type = AST_BLOCK
    Flags: LongWord; // TNPC_BlockFlags = BLOCK_ConsistsOfOrderedStatements
    Parent: TNPC_ASTBlock; // = null
    ParentProcedure: TNPC_ASTProcedure; // = null
    Declarations: Array of TNPC_ASTDeclaration;
    Enums: Array of TNPC_ASTEnum;
    Statements: Array of TNPC_ASTStatement;
    ImplicitDestructors: Array of TNPC_ASTDeclaration;
    ChildScopes: Array of TNPC_ASTBlock;
    OwningStatement: TNPC_ASTStatement; // = null  // see in parent
    EndingStatement: TNPC_ASTStatement; // = null
    // for bytecode
//    bss_size: Int64; // = 0;
//    bss_memory: PChar; // = null
    //
    constructor Create; override;
    destructor Destroy; override;
  end;

  TNPC_ASTExpression = class(TNPC_AST)
  public
    // &Type = AST_EXPRESSION
    InferredType: TNPC_ASTTypeDefinition;
    //
    constructor Create; override;
    destructor Destroy; override;
  end;

  TNPC_ASTIdentifier = class(TNPC_ASTExpression)
  public
    // &Type = AST_IDENTIFIER
    Flags: LongWord;
    Name: UTF8String;
    RecordDereferenceExpression: TNPC_ASTExpression; // = null
    ResolvedDeclaration: TNPC_ASTDeclaration; // = null
    ParentDeclaration: TNPC_ASTDeclaration; // = null
    Block: TNPC_ASTBlock; // = null
    //
    constructor Create; override;
    destructor Destroy; override;
  end;

  TNPC_ASTUnary = class(TNPC_ASTExpression)
  public
    // &Type = AST_UNARY
    OperatorTokenType: Integer;
    Subexpression: TNPC_ASTExpression; // = null
    //
    constructor Create; override;
    destructor Destroy; override;
  end;

  TNPC_ASTBinary = class(TNPC_ASTExpression)
  public
    // &Type = AST_BINARY
    Flags: LongWord;
    OperatorTokenType: Integer;
    Left: TNPC_ASTExpression; // = null
    Right: TNPC_ASTExpression; // = null
    IdentifierToDereference: TNPC_ASTIdentifier; // = null // only for '.'
    //
    constructor Create; override;
    destructor Destroy; override;
  end;

  TNPC_LiteralValueType = (
    LITERAL_Uninitialized,
    LITERAL_Null,
    LITERAL_True,
    LITERAL_False,
    LITERAL_Number,
    LITERAL_String
  );

  TNPC_ASTLiteral = class(TNPC_ASTExpression)
  public
    // &Type = AST_LITERAL
    ValueType: TNPC_LiteralValueType; // = LITERAL_Uninitialized
    StringValue: UTF8String;
    IntegerValue: Int64;
    FloatValue: Double;
    NumberFlags: LongWord;
    //
    constructor Create; override;
    destructor Destroy; override;
  end;

  TNPC_DeclarationFlags = (
    DECLARATION_Unknown = $1
  );

  TNPC_ASTDeclaration = class(TNPC_ASTExpression)
  public
    // &Type = AST_DECLARATION
    Flags: LongWord;
    Identifier: TNPC_ASTIdentifier;
    DeclType: TNPC_ASTTypeDefinition; // TNPC_ASTTypeDeclaration;
    Expression: TNPC_ASTExpression;
    ParentEnum: TNPC_ASTEnum;
    ParentEnumIndex: LongWord;
    Scope: TNPC_ASTBlock;
    SubstituteName: UTF8String;
    RecursionProxyForDeclaration: TNPC_ASTDeclaration;
    DefferedStatement: TNPC_ASTStatement;
    UnresolvedIdents: Array of TNPC_ASTIdentifier;
    Dependencies: Array of TNPC_ASTDeclaration;
    ByteCodeDependencies: Array of TNPC_ASTDeclaration;
    InferDependencies: Array of TNPC_ASTDeclaration;
    EnsureCalls: Array of TNPC_ASTDirectiveENSURE;
    IndexInRecord: LongWord;
    EnsureDirective: TNPC_ASTDirectiveENSURE;
    //
    constructor Create; override;
    destructor Destroy; override;
  end;

  TNPC_ASTEnum = class(TNPC_ASTExpression)
  public
    // &Type = AST_ENUM
    MembersCompleted: Integer; // = 0
    MembersTotal: Integer; // = 0
    NextEnumValue: Int64; // = -1
    HighestValue: Integer; // = -1
    LowestValue: Integer; // = -1
    ExpressionType: TNPC_ASTTypeDefinition;
    InternalType: TNPC_ASTTypeDefinition;
    Scope: TNPC_ASTBlock;
    //Declaration: TNPC_ASTDeclaration;
    //
    constructor Create; override;
    destructor Destroy; override;
  end;

  TNPC_StatementFlags = (
    STATEMENT_Returns = $1,
    STATEMENT_Breaks = $2,
    STSTEMENT_Continues = $4,
    STATEMENT_IsLoop = $8
  );

  TNPC_ASTStatement = class(TNPC_AST)
  public
    // &Type = AST_STATEMENT
    Flags: LongWord;
    Block: TNPC_AstBlock;
    TypeDefinition: TNPC_ASTTypeDefinition;
    Expression: TNPC_ASTExpression;
    //
    constructor Create; override;
    destructor Destroy; override;
  end;

  TNPC_ProcedureFlags = (
    PROCEDURE_BelongsToRunDirective = 1,
    PROCEDURE_HasImplicitResultValue = 2
  );

  TNPC_ASTProcedure = class(TNPC_ASTExpression)
  public
    // &Type = AST_PROCEDURE
    FLags: LongWord;
    Block: TNPC_ASTBlock;
    ForeignFunctionName: UTF8String;
    ArgumentDeclarations: Array of TNPC_ASTDeclaration;
    NonVarargsArguments: Integer;
    ResultType: TNPC_ASTTypeDefinition;
    Header: TNPC_ASTDeclaration;
    Body: TNPC_ASTDeclaration;
    //ByteCode: TNPC_ByteCode;
    //
    constructor Create; override;
    destructor Destroy; override;
  end;

  TNPC_RecordDescriptionFlags = (
    RECORD_HasImplicitConstructor = 1,
    RECORD_HasImplicitDestructor = 2,
    RECORD_IsPacked,
    RECORD_IsUnion
  );

  TNPC_ASTRecordDescription = class(TNPC_ASTExpression)
  public
    // &Type = AST_RECORD_DESCRIPTION
    Flags: LongWord;
    Name: UTF8String;
    Scope: TNPC_ASTBlock;
    DeclarationsThatOwnMemeory: Array of TNPC_ASTDeclaration;
    //
    constructor Create; override;
    destructor Destroy; override;
  end;

  TNPC_ASTIf = class(TNPC_ASTExpression)
  public
    // &Type = AST_IF
    Condition: TNPC_ASTExpression; // = null
    ThenBlock: TNPC_ASTBlock; // = null
    ElseBlock: TNPC_ASTBlock; // = null
    //
    constructor Create; override;
    destructor Destroy; override;
  end;

  TNPC_ASTBreak = class(TNPC_ASTExpression)
  public
    // &Type = AST_BREAK
    //
    constructor Create; override;
    destructor Destroy; override;
  end;

  TNPC_ASTContinue = class(TNPC_ASTExpression)
  public
    // &Type = AST_CONTINUE
    //
    constructor Create; override;
    destructor Destroy; override;
  end;

  TNPC_ASTTypeDeclaration = class(TNPC_ASTExpression)
  public
    // &Type = AST_TYPE_DECLARATION
    Flags: LongWord;
    TypeName: TNPC_ASTIdentifier; // = null
    ArgumentList: TNPC_ASTExpression; // = null
    ExpressionArrayDimensionList: Array of TNPC_ASTExpression;
    ArrayDimensionList: Array of Integer;
    ArrayDimensionProduct: Integer; // = 0
    IsVARArgs: Boolean;
    ArrayCanBeVariableLength: Boolean;
    //
    constructor Create; override;
    destructor Destroy; override;
  end;

  TNPC_TypeDefinitionFlags = (
    TYPE_DefinitionOwnsMemory = $1
  );

  TNPC_ASTTypeDefinition = class(TNPC_ASTExpression)
  public
    // &Type = AST_TYPE_DEFINITION
    Flags: LongWord;
    RecordDescription: TNPC_ASTRecordDescription; // = null
    TypeDeclaration{Instantiation}: TNPC_ASTTypeDeclaration{Instantiation}; // = null
    EnumDescription: TNPC_ASTEnum; // = null
    ArrayElementType: TNPC_ASTTypeDefinition; // = null
    PointerLevel: Integer; // = 0
    PointerTo: TNPC_ASTTypeDefinition; // = null
    LiteralName: UTF8String;
    NumberFlags: LongWord; // = 0
    NumberLiteralLow: TNPC_ASTLiteral; // = null
    NumberLiteralHigh: TNPC_ASTLiteral; // = null
    ProcedureResultType: TNPC_ASTTypeDefinition; // = null
    ProcedureArgumentTypes: Array of TNPC_ASTTypeDefinition;
    ForeignFunctionName: UTF8String;
    ForeignFunctionResolvedPointer: Pointer; // = null
    ProcedureIsVARArgs: Boolean;
    ByteSize: Integer; // -1 // size in bytes of storage for this type; set and used in bytecode-builder
    //
    constructor Create; override;
    destructor Destroy; override;
  end;

  TNPC_ASTDirectiveIF = class(TNPC_ASTExpression)
  public
    // &Type = AST_DIRECTIVE_IF
    TestIdentifier: TNPC_ASTIdentifier; // = null
    Expression: TNPC_ASTExpression; // = null
    //
    constructor Create; override;
    destructor Destroy; override;
  end;

  TNPC_ASTDirectiveENSURE = class(TNPC_ASTExpression)
  public
    // &Type = AST_DIRECTIVE_ENSURE
    IdentifierToCheck: TNPC_ASTIdentifier; // = null
    IdentifierOfEvaluator: TNPC_ASTIdentifier; // = null
    //
    constructor Create; override;
    destructor Destroy; override;
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

{ TNPC_ASTBlock }

constructor TNPC_ASTBlock.Create;
begin
  inherited;
  &Type := AST_BLOCK;
  //
  Flags := LongWord(BLOCK_ConsistsOfOrderedStatements);
  Parent := Nil;
  ParentProcedure := Nil;
  SetLength(Declarations, 0);
  SetLength(Enums, 0);
  SetLength(Statements, 0);
  SetLength(ImplicitDestructors, 0);
  SetLength(ChildScopes, 0);
  OwningStatement := Nil;
  EndingStatement := Nil;
end;

destructor TNPC_ASTBlock.Destroy;
begin
  Parent := Nil;
  ParentProcedure := Nil;
  SetLength(Declarations, 0);
  SetLength(Enums, 0);
  SetLength(Statements, 0);
  SetLength(ImplicitDestructors, 0);
  SetLength(ChildScopes, 0);
  OwningStatement := Nil;
  EndingStatement := Nil;
  inherited;
end;

{ TNPC_ASTExpression }

constructor TNPC_ASTExpression.Create;
begin
  inherited;
  &Type := AST_EXPRESSION;
  //
  InferredType := Nil;
end;

destructor TNPC_ASTExpression.Destroy;
begin
  InferredType := Nil;
  inherited;
end;

{ TNPC_ASTIdentifier }

constructor TNPC_ASTIdentifier.Create;
begin
  inherited;
  &Type := AST_IDENTIFIER;
  //
  Name := '';
  RecordDereferenceExpression := Nil;
  ResolvedDeclaration := Nil;
  ParentDeclaration := Nil;
  Block := Nil;
end;

destructor TNPC_ASTIdentifier.Destroy;
begin
  Name := '';
  RecordDereferenceExpression := Nil;
  ResolvedDeclaration := Nil;
  ParentDeclaration := Nil;
  Block := Nil;
  inherited;
end;

{ TNPC_ASTUnary }

constructor TNPC_ASTUnary.Create;
begin
  inherited;
  &Type := AST_UNARY;
  //
  OperatorTokenType := 0;
  Subexpression := Nil;
end;

destructor TNPC_ASTUnary.Destroy;
begin
  OperatorTokenType := 0;
  Subexpression := Nil;
  inherited;
end;

{ TNPC_ASTBinary }

constructor TNPC_ASTBinary.Create;
begin
  inherited;
  &Type := AST_BINARY;
  //
  OperatorTokenType := 0;
  Left := Nil;
  Right  := Nil;
  IdentifierToDereference := Nil;
end;

destructor TNPC_ASTBinary.Destroy;
begin
  OperatorTokenType := 0;
  Left := Nil;
  Right  := Nil;
  IdentifierToDereference := Nil;
  inherited;
end;

{ TNPC_ASTLiteral }

constructor TNPC_ASTLiteral.Create;
begin
  inherited;
  &Type := AST_LITERAL;
  //
  ValueType := LITERAL_Uninitialized;
  StringValue := '';
  IntegerValue := 0;
  FloatValue := 0;
  NumberFlags := 0;
end;

destructor TNPC_ASTLiteral.Destroy;
begin
  ValueType := LITERAL_Uninitialized;
  StringValue := '';
  IntegerValue := 0;
  FloatValue := 0;
  NumberFlags := 0;
  inherited;
end;

{ TNPC_ASTDeclaration }

constructor TNPC_ASTDeclaration.Create;
begin
  inherited;
  &Type := AST_DECLARATION;
  //
  Identifier := Nil;
  DeclType := Nil;
  Expression := Nil;
  ParentEnum := Nil;
  ParentEnumIndex := 0;
  Scope := Nil;
  SubstituteName := '';
  RecursionProxyForDeclaration := Nil;
  DefferedStatement := Nil;
  SetLength(UnresolvedIdents, 0);
  SetLength(Dependencies, 0);
  SetLength(ByteCodeDependencies, 0);
  SetLength(InferDependencies, 0);
  SetLength(EnsureCalls, 0);
  IndexInRecord := 0;
  //RunDirective: TNPC_ASTDirectiveRun;
  EnsureDirective := Nil;
end;

destructor TNPC_ASTDeclaration.Destroy;
begin
  Identifier := Nil;
  DeclType := Nil;
  Expression := Nil;
  ParentEnum := Nil;
  ParentEnumIndex := 0;
  Scope := Nil;
  SubstituteName := '';
  RecursionProxyForDeclaration := Nil;
  DefferedStatement := Nil;
  SetLength(UnresolvedIdents, 0);
  SetLength(Dependencies, 0);
  SetLength(ByteCodeDependencies, 0);
  SetLength(InferDependencies, 0);
  SetLength(EnsureCalls, 0);
  IndexInRecord := 0;
  //RunDirective := Nil;
  EnsureDirective := Nil;
  inherited;
end;

{ TNPC_ASTEnum }

constructor TNPC_ASTEnum.Create;
begin
  inherited;
  &Type := AST_ENUM;
  //
  MembersCompleted := 0;
  MembersTotal := 0;
  NextEnumValue := -1;
  HighestValue := -1;
  LowestValue := -1;
  ExpressionType := Nil;
  InternalType := Nil;
  Scope := Nil;
  //Declaration := Nil;
end;

destructor TNPC_ASTEnum.Destroy;
begin
  MembersCompleted := 0;
  MembersTotal := 0;
  NextEnumValue := -1;
  HighestValue := -1;
  LowestValue := -1;
  ExpressionType := Nil;
  InternalType := Nil;
  Scope := Nil;
  //Declaration := Nil;
  inherited;
end;

{ TNPC_ASTStatement }

constructor TNPC_ASTStatement.Create;
begin
  inherited;
  &Type := AST_STATEMENT;
  //
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

{ TNPC_ASTProcedure }

constructor TNPC_ASTProcedure.Create;
begin
  inherited;
  &Type := AST_PROCEDURE;
  //
  FLags := 0;
  Block := Nil;
  ForeignFunctionName := '';
  SetLength(ArgumentDeclarations, 0);
  NonVarargsArguments := 0;
  ResultType := Nil;
  Header := Nil;
  Body := Nil;
end;

destructor TNPC_ASTProcedure.Destroy;
begin
  Block := Nil;
  ForeignFunctionName := '';
  SetLength(ArgumentDeclarations, 0);
  NonVarargsArguments := 0;
  ResultType := Nil;
  Header := Nil;
  Body := Nil;
  inherited;
end;

{ TNPC_ASTRecordDescription }

constructor TNPC_ASTRecordDescription.Create;
begin
  inherited;
  &Type := AST_RECORD_DESCRIPTION;
  //
  Flags := 0;
  Name := '';
  Scope := Nil;
  SetLength(DeclarationsThatOwnMemeory, 0);
end;

destructor TNPC_ASTRecordDescription.Destroy;
begin
  Name := '';
  Scope := Nil;
  SetLength(DeclarationsThatOwnMemeory, 0);
  inherited;
end;

{ TNPC_ASTIf }

constructor TNPC_ASTIf.Create;
begin
  inherited;
  &Type := AST_IF;
  //
  Condition := Nil;
  ThenBlock := Nil;
  ElseBlock := Nil;
end;

destructor TNPC_ASTIf.Destroy;
begin
  Condition := Nil;
  ThenBlock := Nil;
  ElseBlock := Nil;
  inherited;
end;

{ TNPC_ASTBreak }

constructor TNPC_ASTBreak.Create;
begin
  inherited;
  &Type := AST_BREAK;
end;

destructor TNPC_ASTBreak.Destroy;
begin
  inherited;
end;

{ TNPC_ASTContinue }

constructor TNPC_ASTContinue.Create;
begin
  inherited;
  &Type := AST_CONTINUE;
end;

destructor TNPC_ASTContinue.Destroy;
begin
  inherited;
end;

{ TNPC_ASTTypeDeclaration }

constructor TNPC_ASTTypeDeclaration.Create;
begin
  inherited;
  &Type := AST_TYPE_DECLARATION;
  //
  Flags := 0;
  TypeName := Nil;
  ArgumentList := Nil;
  SetLength(ExpressionArrayDimensionList, 0);
  SetLength(ArrayDimensionList, 0);
  ArrayDimensionProduct := 0;
  IsVARArgs := False;
  ArrayCanBeVariableLength := False;
end;

destructor TNPC_ASTTypeDeclaration.Destroy;
begin
  TypeName := Nil;
  ArgumentList := Nil;
  SetLength(ExpressionArrayDimensionList, 0);
  SetLength(ArrayDimensionList, 0);
  ArrayDimensionProduct := 0;
  IsVARArgs := False;
  ArrayCanBeVariableLength := False;
  inherited;
end;

{ TNPC_ASTTypeDefinition }

constructor TNPC_ASTTypeDefinition.Create;
begin
  inherited;
  &Type := AST_TYPE_DEFINITION;
  //
  Flags := 0;
  RecordDescription := Nil;
  TypeDeclaration := Nil;
  EnumDescription := Nil;
  ArrayElementType := Nil;
  PointerLevel := 0;
  PointerTo := Nil;
  LiteralName := '';
  NumberFlags := 0;
  NumberLiteralLow := Nil;
  NumberLiteralHigh := Nil;
  ProcedureResultType := Nil;
  SetLength(ProcedureArgumentTypes, 0);
  ForeignFunctionName := '';
  ForeignFunctionResolvedPointer := Nil;
  ProcedureIsVARArgs := False;
  ByteSize := -1; // size in bytes of storage for this type; set and used in bytecode-builder
end;

destructor TNPC_ASTTypeDefinition.Destroy;
begin
  RecordDescription := Nil;
  TypeDeclaration := Nil;
  EnumDescription := Nil;
  ArrayElementType := Nil;
  PointerLevel := 0;
  PointerTo := Nil;
  LiteralName := '';
  NumberFlags := 0;
  NumberLiteralLow := Nil;
  NumberLiteralHigh := Nil;
  ProcedureResultType := Nil;
  SetLength(ProcedureArgumentTypes, 0);
  ForeignFunctionName := '';
  ForeignFunctionResolvedPointer := Nil;
  ProcedureIsVARArgs := False;
  ByteSize := -1; // size in bytes of storage for this type; set and used in bytecode-builder
  inherited;
end;

{ TNPC_ASTDirectiveIF }

constructor TNPC_ASTDirectiveIF.Create;
begin
  inherited;
  &Type := AST_DIRECTIVE_IF;
  //
  TestIdentifier := Nil;
  Expression := Nil;
end;

destructor TNPC_ASTDirectiveIF.Destroy;
begin
  TestIdentifier := Nil;
  Expression := Nil;
  inherited;
end;

{ TNPC_ASTDirectiveENSURE }

constructor TNPC_ASTDirectiveENSURE.Create;
begin
  inherited;
  &Type := AST_DIRECTIVE_ENSURE;
  //
  IdentifierToCheck := Nil;
  IdentifierOfEvaluator := Nil;
end;

destructor TNPC_ASTDirectiveENSURE.Destroy;
begin
  IdentifierToCheck := Nil;
  IdentifierOfEvaluator := Nil;
  inherited;
end;

end.

