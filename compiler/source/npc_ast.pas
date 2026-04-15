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
  npc_location,
  npc_lexer;

type
  TNPC_ASTValue = Variant; // for simplicity

  TNPCScope = class;

  TNPC_ASTType = class;
  TNPC_ASTTypeReference = class;
  TNPC_ASTTypeClassProperty = class;

  TNPC_ASTExpression = class;

  TNPC_ASTParameter = class;
  TNPC_ASTStatementProcedure = class;

  TNPCSymbolKind = (
    KIND_BuiltinType,
    KIND_BuiltinTypeAlias, // builtin-type alias
    KIND_Type,
    KIND_TypeAlias, // type alias
    KIND_Var, // const
    KIND_Param,
    KIND_Return,
    KIND_Tuple,
    //KIND_ReturnTuple,
    KIND_Record,
    KIND_RecordField,
    KIND_Proc,
    // class
    KIND_Field,
    KIND_Method,
    KIND_Property
  );

  TNPCSymbolType = (
    TYPE_Unresolved,
    TYPE_Enum, // ( e1, e2, ... )
    TYPE_EnumConst, // e1, e2, ...
    TYPE_Set, // [ s1, s2, s3..s4, ... ]
    TYPE_SetConst, // s1, s2, ...
    TYPE_BitSet, // 64 bit int
    TYPE_Array, // array[] of ...
    TYPE_Record, // record ... end
    TYPE_RecordField, // ident: ident
    TYPE_Class, // class ... end
//    TYPE_ClassField, // class.field
//    TYPE_ClassMethod, // class.method
//    TYPE_ClassProperty, // class.property
    TYPE_Pointer, // 32 / 64 bit value
    TYPE_Literal, // ident / number / string / char
    TYPE_Procedure, // pointer
    TYPE_ForeignFunction // pointer
  );

  TNPCSymbol = class
  public
    Location: TNPCLocation;
    Name: UTF8String;
    Kind: TNPCSymbolKind;
    &Type: TNPCSymbolType;
    Scope: TNPCScope;
    TypeRef: TNPC_ASTType;
    PropInfo: TNPC_ASTTypeClassProperty;
    DeclNode: TObject; // pointer/reference to the declaration AST node (optional)
    Size: Integer;
    Offset: Integer; // class/record field offset
    IsConst: Boolean;
    ConstValue: TNPC_ASTValue; // used if Kind=skDecl and IsConst=True
    Value: TNPC_ASTValue; // used for evaluation
    //
    // for inline methods: Name = { ... }
    ValueExpr: TNPC_ASTExpression;
    // for declared procedures
    ProcDecl: TNPC_ASTStatementProcedure;
    //
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String; AKind: TNPCSymbolKind; AType: TNPCSymbolType; AConst: Boolean; ATypeRef: TNPC_ASTType; ADecl: TObject); overload;
    constructor Create(AParam: TNPC_ASTParameter; const AKind: TNPCSymbolKind; AType: TNPCSymbolType; ADecl: TObject); overload;
    destructor Destroy; override;
  end;

//  TSymbol.Kind := skProperty;
//  TSymbol.RefType := StringType;
//  TSymbol.PropInfo := TClassProperty;

//  TNPCListEntry = packed record
//    Name: UTF8String;
//    Symbol: TNPCSymbol;
//  end;

  TNPCScope = class // TDictionary<UTF8String, TSymbol>;
  private
//    List: Array of TNPCListEntry;
//    Size: Integer;
//    Capacity: Integer;
    Name: UTF8String;
    Table: TDictionary<UTF8String, TNPCSymbol>; // no overloads/polymorphism for now
    //Table: TDictionary<UTF8String, TObjectList<TNPCSymbol>>; // enable overloads/polymorphism
    Deferred: TObjectList<TNPC_ASTExpression>;
    Parent: TNPCScope;
    //
//    procedure Init;
    procedure DuplicateSymbolError(const ASymbol, AExisting: TNPCSymbol);
  public
    //Memory: PByte; // Pointer; // stack frame base
  public
    constructor Create(AParent: TNPCScope; const AName: UTF8String);
    destructor Destroy; override;
    //
    property ParentScope: TNPCScope read Parent;
//    procedure Clear;
//    function Exists(const AName: UTF8String): Boolean;
//    procedure Add(const AName: UTF8String; const ASymbol: TNPCSymbol);
//    function TryGetValue(const AName: UTF8String; out ASymbol: TNPCSymbol): Boolean;
    //
    procedure AddSymbol(const AName: UTF8String; ASymbol: TNPCSymbol);
    procedure AddOrSetSymbol(const AName: UTF8String; ASymbol: TNPCSymbol);
    procedure AddDefer(const Expr: TNPC_ASTExpression);
    //
    function  FindLocal(const AName: UTF8String): TNPCSymbol;
    function  TryGetSymbol(const AName: UTF8String; out ASymbol: TNPCSymbol): Boolean;
    function  ResolveSymbol(const AName: UTF8String): TNPCSymbol;
    procedure ExecuteDeferred;
    //
    function  ResolveTypeKind(const ATypeRef: TNPC_ASTTypeReference): TNPCSymbolType;
    function  ResolveNullValue(TypeRef: TNPC_ASTType): Variant;
    procedure SetSymbolValue(Sym: TNPCSymbol; const Value: TNPC_ASTValue);
    procedure WriteValueToMemory(Dest: PByte; const Value: TNPC_ASTValue; TypeRef: TNPC_ASTType; Size: Integer);

    //
    //
    function DefineBuiltinType(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTType): TNPCSymbol;
    function DefineBuiltinTypeAlias(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTType): TNPCSymbol;
    function DefineType(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTType; ADecl: TObject): TNPCSymbol;
    function DefineTypeAlias(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTType; ADecl: TObject): TNPCSymbol;
    function DefineConst(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTType; ADecl: TObject; AValue: Integer): TNPCSymbol;
    function DefineVar(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTType; ADecl: TObject): TNPCSymbol;
    function DefineSymbol(const ASymbol: TNPCSymbol): TNPCSymbol;
    function DefineTupleItem(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTType; ADecl: TObject): TNPCSymbol;
    function DefineRecordItem(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTType; ADecl: TObject): TNPCSymbol;
    function DefineProcedure(const ALocation: TNPCLocation; const AName: UTF8String; const AProcedure: TNPC_ASTStatementProcedure): TNPCSymbol;
    function DefineClassField(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPCSymbolType; ATypeRef: TNPC_ASTType; ADecl: TObject): TNPCSymbol;
    function DefineClassMethod(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPCSymbolType; ATypeRef: TNPC_ASTType; ADecl: TObject): TNPCSymbol;
    function DefineClassProperty(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPCSymbolType; ATypeRef: TNPC_ASTType; ADecl: TObject): TNPCSymbol;
  end;

  TNPC_ASTTypes = (
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
    AST_TYPE_REFERENCE,
    AST_TYPE_DECLARATION, // type
    AST_TYPE_DEFINITION, // ident = type_definition
    AST_TYPE_NAME,
    AST_TYPE_ENUM,
    AST_TYPE_SET,
    AST_TYPE_BITSET,
    AST_TYPE_ARRAY,
    AST_TYPE_ARRAY_SLICE,
    AST_TYPE_RECORD,
    AST_TYPE_RECORD_DESCRIPTION,
    AST_TYPE_TUPLE,
    AST_TYPE_TUPLE_DESCRIPTION,
    AST_TYPE_CLASS,
    AST_TYPE_CLASS_DESCRIPTION,
    AST_TYPE_CLASS_METHOD,
    AST_TYPE_CLASS_PROPERTY,

    // expressions
    AST_EXPRESSION_LITERAL, // AST_LITERAL ???
//    AST_IDENT, // AST_LITERAL ???
//    AST_NUMBER, // AST_LITERAL ???
    AST_EXPRESSION_STRING, // AST_LITERAL ???
    AST_EXPRESSION_PARAMETER,
    AST_EXPRESSION_ENUM,
    AST_EXPRESSION_ENUM_CONST,
    AST_EXPRESSION_SET,
    AST_EXPRESSION_SET_LITERAL, // AST_LITERAL ???
    AST_EXPRESSION_ARRAY,
    AST_EXPRESSION_ARRAY_SLICE,
    AST_EXPRESSION_RECORD,
    AST_EXPRESSION_TUPLE,
    AST_EXPRESSION_TUPLE_ITEM,
    //
    AST_EXPRESSION_ASSIGN, // a := expression returning single value
    AST_EXPRESSION_MULTI_ASSIGN, // a, b := expression returning tuple values
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
    AST_STATEMENT_ASSIGN, // a := expression returning single value
    AST_STATEMENT_MULTI_ASSIGN, // a, b := expression returning tuple values
    AST_STATEMENT_RETURN,
    AST_STATEMENT_RESULT,
    AST_STATEMENT_DEFER, // at scope exit
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
    FLAG_Parenthesized = $1,
    FLAG_ForEachBinaryOperation = $2
  );

  TNPC_AST = class
  public
    &Type: TNPC_ASTTypes;
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
    ResolvedSymbol: TNPCSymbol;
    InferredType: TNPC_ASTType; //TNPC_ASTTypeDefinition;
    IsAssignable: Boolean; // means, if can be done like this: func() := value - no; but a := 10 - ok
    EndToken: TNPCToken;
    //
    constructor Create(const ALocation: TNPCLocation); override;
    destructor Destroy; override;
    //
    function Eval(Scope: TNPCScope): TNPC_ASTValue; virtual; abstract;
    function ToString: String; virtual;
  end;

  // Types

  TNPC_ASTType = class(TNPC_ASTExpression)
  public
    // &Type = AST_TYPE_DECLARATION
    //
    constructor Create(const ALocation: TNPCLocation); override;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTTypeClassType = class of TNPC_ASTType;

  TNPCTypeReferenceKind = (
    REF_Unknown,
    REF_Named,   // String, MyType, List<T>
    REF_Array,   // Array of T
    REF_Record,  // record ... end
    REF_Function // function/procedure
  );

  TNPC_ASTTypeReference = class(TNPC_ASTType)
  public
    // &Type = AST_TYPE_REFERENCE
    //
    Kind: TNPCTypeReferenceKind;
    //
    // REF_Named
    BaseSymbol: TNPCSymbol; // e.g. Array / Map / String
    GenericArgs: TObjectList<TNPC_ASTTypeReference>; // nested types for generics
    // REF_Array
    ElementType: TNPC_ASTTypeReference;
    // REF_Record
    Fields: TObjectList<TNPCSymbol>;
    // REF_Function
    Params: TObjectList<TNPCSymbol>;
    ReturnType: TNPC_ASTTypeReference;
    IsFunction: Boolean;
    //
    constructor Create(const ALocation: TNPCLocation); override;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTTypeEnum = class;
  TNPC_ASTTypeSet = class;
  TNPC_ASTTypeArray = class;
  TNPC_ASTTypeRecord = class;
  TNPC_ASTTypeClass = class;

  TNPC_ASTExpressionSetLiteral = class;

  TNPC_TypeDefinitionType = (
    DEF_Unknown,
    DEF_Type,
    DEF_Literal,
    DEF_Enum,
    DEF_Set,
    DEF_BitSet,
    DEF_Array,
    DEF_Record,
    DEF_Class,
    DEF_Pointer,
    DEF_Procedure,
    DEF_ProcedureArguments,
    DEF_ProcedureResult,
    DEF_ForeignFunction
  );

  TNPC_ASTTypeDefinition = class(TNPC_ASTType)
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
    ClassDescription: TNPC_ASTTypeClass;
    //
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String; ADefinitionType: TNPC_TypeDefinitionType; ASizeInBytes: Integer); reintroduce;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTTypeName = class(TNPC_ASTType)
  public
    // &Type = AST_TYPE_NAME
    Name: UTF8String;
    //
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String); reintroduce;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTTypeEnum = class(TNPC_ASTType)
  public
    // &Type = AST_TYPE_ENUM
    Members: TDictionary<UTF8String, Integer>;
    //
    constructor Create(const ALocation: TNPCLocation); override;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTTypeSet = class(TNPC_ASTType)
  public
    // &Type = AST_TYPE_SET
    ElementType: TNPC_ASTType;
    Elements: TObjectList<TNPC_ASTExpression>;
    //
    constructor Create(const ALocation: TNPCLocation; const AElementType: TNPC_ASTType); reintroduce;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTTypeArray = class(TNPC_ASTType)
  public
    // &Type = AST_TYPE_ARRAY
    ElementType: TNPC_ASTType;
    IndexType: TNPC_ASTType;
    //
    constructor Create(const ALocation: TNPCLocation; AElementType, AIndexType: TNPC_ASTType); reintroduce;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTTypeTuple = class(TNPC_ASTType)
  public
    // &Type = AST_TYPE_TUPLE
    Elements: TDictionary<UTF8String, TNPC_ASTType>;
    //
    constructor Create(const ALocation: TNPCLocation); override;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTTypeRecord = class(TNPC_ASTType)
  public
    // &Type = AST_TYPE_RECORD_DESCRIPTION
    Fields: TDictionary<UTF8String, TNPC_ASTType>;
    //
    constructor Create(const ALocation: TNPCLocation); override;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTClassVisibilityTypeEnum = (
    VIS_Class,
    VIS_Private,
    VIS_Protected,
    VIS_Public
  );

  TNPC_ASTTypeClassMethod = class(TNPC_ASTType)
  public
    // &Type = AST_TYPE_CLASS_METHOD
    Name: UTF8String;
    VisibilityType: TNPC_ASTClassVisibilityTypeEnum;
    IsFunction: Boolean;
    //
    Expr: TNPC_ASTExpression; // unified representation
    //
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String; const AVisibilityType: TNPC_ASTClassVisibilityTypeEnum); reintroduce; overload;
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String; const AVisibilityType: TNPC_ASTClassVisibilityTypeEnum; AExpr: TNPC_ASTExpression); overload;
    destructor Destroy; override;
    //
    function ToString: String; override;
    function VisibilityToString: String;
  end;

  TNPC_ASTTypeClassProperty = class(TNPC_ASTType)
  public
    // &Type = AST_TYPE_CLASS_PROPERTY
    Name: UTF8String;
    VisibilityType: TNPC_ASTClassVisibilityTypeEnum;
    PropertyType: TNPC_ASTType;
    //
    ReadField: TNPCSymbol;
    WriteField: TNPCSymbol;
    //
    InitExpr: TNPC_ASTExpression;    // unified representation
    DefaultExpr: TNPC_ASTExpression; // optional
    //
//    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String; APropertyType: TNPC_ASTType; const AVisibilityType: TNPC_ASTClassVisibilityTypeEnum); reintroduce; overload;
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String; const AVisibilityType: TNPC_ASTClassVisibilityTypeEnum); reintroduce; overload;
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String; APropertyType: TNPC_ASTType; const AVisibilityType: TNPC_ASTClassVisibilityTypeEnum; AReadField, AWriteField: TNPCSymbol; AInitExpr, ADefaultExpr: TNPC_ASTExpression); overload;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  TNPC_ASTTypeClass = class(TNPC_ASTType)
  public
    // &Type = AST_TYPE_CLASS
    Name: UTF8String;
    //Scope: TNPCScope;
    Fields: TDictionary<UTF8String, TNPC_ASTType>; // TClassField
    Methods: TObjectList<TNPC_ASTTypeClassMethod>; // TClassMethod     TNPC_ASTExpressionCall = class(TNPC_ASTExpression)
    //Methods: TDictionary<UTF8String, TNPC_ASTExpression>;
    Properties: TObjectList<TNPC_ASTTypeClassProperty>;
    //
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String); reintroduce; overload;
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String; AFields: TDictionary<UTF8String, TNPC_ASTType>; AMethods: TObjectList<TNPC_ASTTypeClassMethod>; AProperties: TObjectList<TNPC_ASTTypeClassProperty>); reintroduce; overload;
    //constructor Create(const ALocation: TNPCLocation; AFields: TDictionary<UTF8String, TNPC_ASTType>; AMethods: TDictionary<UTF8String, TNPC_ASTExpression>; TObjectList<TNPC_ASTTypeClassProperty>); reintroduce; overload;
    destructor Destroy; override;
    //
    function ToString: String; override;
  end;

  // Expressions

  TNPC_ASTExpressionLiteral = class(TNPC_ASTExpression)
  public
    // &Type = AST_EXPRESSION_LITERAL
    Value: UTF8String;
    LiteralType: TNPC_ASTType;
    //
    constructor Create(const ALocation: TNPCLocation; const AValue: UTF8String; AType: TNPC_ASTType); reintroduce;
    destructor Destroy; override;
    //
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
    function ToString: String; override;
  end;

//  TNPC_ASTStatementEnum = class(TNPC_ASTStatement)
//  public
//    // &Type = AST_ENUM
//    ElementType: TNPC_ASTType;
//    Elements: TObjectList<TNPC_ASTExpression>;
//    //
//    constructor Create(const ALocation: TNPCLocation; const AElementType: TNPC_ASTType); reintroduce;
//    destructor Destroy; override;
//    //
//    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
//    function ToString: String; override;
//  end;

  TNPC_ASTExpressionEnumConst = class(TNPC_ASTExpression)
  public
    // &Type = AST_ENUM_CONST
    Name: UTF8String;
    Value: Integer;
    EnumType: TNPC_ASTType;
    //
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String; AValue: Integer; AType: TNPC_ASTType); reintroduce;
    destructor Destroy; override;
    //
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
    function ToString: String; override;
  end;

  TNPC_ASTExpressionSet = class(TNPC_ASTExpression)
  public
    // &Type = AST_EXPRESSION_SET
    ElementType: TNPC_ASTType;
    Elements: TObjectList<TNPC_ASTExpression>;
    //
    constructor Create(const ALocation: TNPCLocation; const AElementType: TNPC_ASTType); reintroduce;
    destructor Destroy; override;
    //
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
    function ToString: String; override;
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
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
    function ToString: String; override;
  end;

  TNPC_ASTExpressionVariable = class(TNPC_ASTExpression)
  public
    // &Type = AST_DECLARATION
    Name: UTF8String;
    Symbol: TNPCSymbol; // resolved symbol during parsing / semantic analysis
    Base: TNPC_ASTExpression; // set by children, nil for root
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
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
    function ToString: String; override;
  end;

  TNPC_ASTExpressionNumber = class(TNPC_ASTExpression)
  public
    // &Type = AST_LITERAL
    Value: UTF8String;
    LiteralType: TNPC_ASTType;
    //
    constructor Create(const ALocation: TNPCLocation; const AValue: UTF8String; AType: TNPC_ASTType); reintroduce;
    destructor Destroy; override;
    //
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
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
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
    function ToString: String; override;
  end;

  TNPC_ASTExpressionNull = class(TNPC_ASTExpression)
  public
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
    function ToString: String; override;
  end;

  TNPC_ASTExpressionParameter = class(TNPC_ASTExpression) // call argument
  public
    // &Type = AST_EXPRESSION_PARAMETER
    //Declaration: TNPC_ASTExpression;
    TypeRef: TNPC_ASTTypeReference;
    Name: UTF8String;
    ParameterType: TNPC_ASTType;
    ParamValue: TNPC_ASTExpression;
    //
    //constructor Create(const ALocation: TNPCLocation; ADeclaration: TNPC_ASTExpression; const AName: UTF8String; const AParameterType: TNPC_ASTType; const AParamValue: TNPC_ASTExpression); reintroduce;
    constructor Create(const ALocation: TNPCLocation; ATypeRef: TNPC_ASTTypeReference; const AName: UTF8String; const AParameterType: TNPC_ASTType; const AParamValue: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
    //
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
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
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
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
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
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

  TNPC_ASTExpressionMultiAssign = class(TNPC_ASTExpression)
  public
    // &Type = AST_EXPRESSION_MULTI_ASSIGN
    Targets: TObjectList<TNPC_ASTExpressionVariable>;
    ValueExpr: TNPC_ASTExpression; // usually tuple or function call
    //
    constructor Create(const ALocation: TNPCLocation; ATargets: TObjectList<TNPC_ASTExpressionVariable>; AValue: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
    //
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
    function ToString: String; override;
  end;

  TNPC_ASTExpressionIf = class(TNPC_ASTExpression)
  public
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

  TNPC_ASTExpressionCaseBranch = class(TNPC_ASTExpression)
  public
    // &Type = AST_EXPRESSION_CASE
    IfValues: TObjectList<TNPC_ASTExpression>; // list of constant expressions like 1, 2, 3
    ResultExpr: TNPC_ASTExpression; // expression for this branch
    //
    constructor Create(const ALocation: TNPCLocation); override;
    destructor Destroy; override;
    //
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
    function ToString: String; override;
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
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
    function ToString: String; override;
  end;

  TNPC_ASTExpressionArray = class(TNPC_ASTExpression)
  public
    // &Type = AST_ARRAY
    Base: TNPC_ASTExpression;
    Index: TNPC_ASTExpression;
    ElemType: TNPC_ASTType;
    //
    constructor Create(const ALocation: TNPCLocation; ABase, AIndex: TNPC_ASTExpression; AElemType: TNPC_ASTType); reintroduce;
    destructor Destroy; override;
    //
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
    function ToString: String; override;
  end;

  TNPC_ASTExpressionRecord = class(TNPC_ASTExpression)
  public
    // &Type = AST_RECORD
    Base: TNPC_ASTExpression;
    FieldName: UTF8String;
    FieldType: TNPC_ASTType;
    //
    constructor Create(const ALocation: TNPCLocation; ABase: TNPC_ASTExpression; const AFieldName: UTF8String; AFieldType: TNPC_ASTType); reintroduce;
    destructor Destroy; override;
    //
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
    function ToString: String; override;
  end;

  TNPC_ASTTupleItem = class(TNPC_ASTExpression)
  public
    // &Type = AST_EXPRESSION_TUPLE_ITEM
    Name: UTF8String; // '' if positional
    Expr: TNPC_ASTExpression;
    //
    constructor Create(const ALocation: TNPCLocation); reintroduce;
    destructor Destroy; override;
    //
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
    function ToString: String; override;
  end;

  TNPC_ASTExpressionTuple = class(TNPC_ASTExpression) // tuple value
  public
    // &Type = AST_EXPRESSION_TUPLE
    //Values: TObjectList<TNPC_ASTExpression>;
    //Values: TDictionary<UTF8String, TNPC_ASTExpression>;
    Items: TObjectList<TNPC_ASTTupleItem>;
    //
    constructor Create(const ALocation: TNPCLocation); reintroduce;
    destructor Destroy; override;
    //
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
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
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
    function ToString: String; override;
  end;

  TNPC_ASTExpressionMember = class(TNPC_ASTExpressionVariable) // e.g. a.b
  public
    // &Type = AST_STRUCT_MEMBER
    MemberType: TNPC_ASTType;
    //
    constructor Create(const ALocation: TNPCLocation; ABase: TNPC_ASTExpression; const AMember: UTF8String; const AMemberSymbol: TNPCSymbol; const AMemberType: TNPC_ASTType); reintroduce;
    destructor Destroy; override;
    //
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
    function ToString: String; override;
  end;

  TNPC_ASTExpressionIndex = class(TNPC_ASTExpressionVariable) // e.g. a[0]
  public
    // &Type = AST_ARRAY_INDEX
    Index: TNPC_ASTExpression;
    //
    constructor Create(const ALocation: TNPCLocation; const ABase, AIndex: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
    //
    function Eval(Scope: TNPCScope): TNPC_ASTValue; override;
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
    BLOCK_DescribesOnlyProcedureSignature,
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
    DeclaredType: TNPC_ASTType;
    //
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPC_ASTType); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTStatementVariableDeclaration = class(TNPC_ASTStatement)
  public
    // &Type = AST_STATEMENT_VAR_DECL
    Name: UTF8String;
    DeclaredType: TNPC_ASTType;   // used instead of String TypeName
    Init: TNPC_ASTExpression; // optional initializer
    SymbolRef: TNPCSymbol; // symbol created when parsing
    //
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPC_ASTType; AInit: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTParamContext = (pcParam, pcReturn);
  TNPC_ASTParamModifier = (pmNone, pmConst, pmVar, pmOut);

  TNPC_ASTParameter = class(TNPC_ASTStatementVariableDeclaration) // declaration param
  public
    Modifier: TNPC_ASTParamModifier;
    //
    constructor Create(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPC_ASTType; AModifier: TNPC_ASTParamModifier; AInit: TNPC_ASTExpression); reintroduce;
  end;

  TNPC_ASTStatementProcedure = class(TNPC_ASTStatementBlock)
  public
    // &Type = AST_PROCEDURE
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

  TNPC_ASTStatementMultiAssign = class(TNPC_ASTStatement)
  public
    // &Type = AST_MULTI_ASSIGN
    Targets: TObjectList<TNPC_ASTExpression>; // array of TNPC_ASTExpressionVariable // usually TIdentExpr or member/index
    Value: TNPC_ASTExpression;
    //
    constructor Create(const ALocation: TNPCLocation; ATargets: TObjectList<TNPC_ASTExpression>; AValue: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTStatementReturn = class(TNPC_ASTStatement) // store function return values and exit
  public
    // &Type = AST_STATEMENT_RETURN
    Expr: TNPC_ASTExpression; // tuple or single value
    //
    constructor Create(const ALocation: TNPCLocation; AExpression: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTStatementResult = class(TNPC_ASTStatement) // store function return values
  public
    // &Type = AST_STATEMENT_RESULT
    Expr: TNPC_ASTExpression; // tuple or single value
    //
    constructor Create(const ALocation: TNPCLocation; AExpression: TNPC_ASTExpression); reintroduce;
    destructor Destroy; override;
  end;

  TNPC_ASTStatementDefer = class(TNPC_ASTStatement)
  public
    // &Type = AST_STATEMENT_DEFER
    Expr: TNPC_ASTExpression;
    //
    constructor Create(const ALocation: TNPCLocation; AExpr: TNPC_ASTExpression); reintroduce;
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

function SymbolKindToString(Kind: TNPCSymbolKind): String;

implementation

uses
  npc_error;

function SymbolKindToString(Kind: TNPCSymbolKind): String;
begin
  case Kind of
    KIND_Param : Result := 'parameter';
    KIND_Return: Result := 'return value';
    KIND_Field : Result := 'field';
    KIND_Method: Result := 'method';
  else
    Result := 'symbol';
  end;
end;

{ TNPCSymbol }

constructor TNPCSymbol.Create(const ALocation: TNPCLocation; const AName: UTF8String; AKind: TNPCSymbolKind; AType: TNPCSymbolType; AConst: Boolean; ATypeRef: TNPC_ASTType; ADecl: TObject);
begin
  inherited Create;
  Location := ALocation;
  Name := AName;
  Kind := AKind;
  &Type := AType;
  Scope := Nil;
  TypeRef := ATypeRef;
  PropInfo := Nil;
  DeclNode := ADecl;
  Size := -1;
  IsConst := AConst;
  ConstValue := Unassigned; // used if Kind=skDecl and IsConst=True
  Value := Unassigned;
  ValueExpr := Nil;
  ProcDecl := Nil;
end;

constructor TNPCSymbol.Create(AParam: TNPC_ASTParameter; const AKind: TNPCSymbolKind; AType: TNPCSymbolType; ADecl: TObject);
begin
  Create(AParam.Location, AParam.Name, AKind, AType, AParam.Modifier = pmConst, AParam.DeclaredType, ADecl);
end;

destructor TNPCSymbol.Destroy;
begin
  Location := Nil;
  Name := '';
  Scope := Nil;
  TypeRef := Nil;
//  if TypeRef <> Nil then
//    FreeAndNil(TypeRef);
  PropInfo := Nil;
//  if PropInfo <> Nil then
//    FreeAndNil(PropInfo);
  DeclNode := Nil;
  ConstValue := Unassigned;
  Value := Unassigned;
  if ValueExpr <> Nil then
    FreeAndNil(ValueExpr);
  if ProcDecl <> Nil then
    FreeAndNil(ProcDecl);
  inherited;
end;

{ TNPCScope }

constructor TNPCScope.Create(AParent: TNPCScope; const AName: UTF8String);
begin
  Name := AName;
  Table := TDictionary<UTF8String, TNPCSymbol>.Create; // no overloads/polymorphism for now
//  Table := TDictionary<UTF8String, TObjectList<TNPCSymbol>>.Create; // enable overloads/polymorphism
  Deferred := TObjectList<TNPC_ASTExpression>.Create(True);
  Parent := AParent;
end;

destructor TNPCScope.Destroy;
var
  Symbol: TNPCSymbol;
begin
  for Symbol in Table.Values do
    Symbol.Free;
  FreeAndNil(Table);
  FreeAndNil(Deferred);
  inherited;
end;

procedure TNPCScope.DuplicateSymbolError(const ASymbol, AExisting: TNPCSymbol);
var
  Msg: String;
begin
  if AExisting <> Nil then begin
    Msg := Format('duplicate identifier "%s". Previously declared at line %d, col %d', [ASymbol.Name, AExisting.Location.StartRow, AExisting.Location.StartCol]);
  end
  else begin
    Msg := Format('duplicate identifier "%s"', [ASymbol.Name]);
  end;

  raise TNPCError.SemanticError(ASymbol.Location, Msg) at ReturnAddress;
end;

procedure TNPCScope.AddSymbol(const AName: UTF8String; ASymbol: TNPCSymbol);
//var
//  List: TObjectList<TNPCSymbol>;
begin
//  if not Table.TryGetValue(AName, List) then begin
//    List := TObjectList<TNPCSymbol>.Create(True);
//    Table.Add(AName, List);
//  end;

  // Optional: check signature uniqueness
//  for Existing in List do
//    if SameSignature(Existing, Result) then
//      raise Exception.CreateFmt('Duplicate overload "%s"', [AName]);

//  List.Add(Result);
  Table.Add(AName, ASymbol);
end;

procedure TNPCScope.AddOrSetSymbol(const AName: UTF8String; ASymbol: TNPCSymbol);
begin
  Table.AddOrSetValue(AName, ASymbol);
end;

procedure TNPCScope.AddDefer(const Expr: TNPC_ASTExpression);
begin
  Deferred.Add(Expr);
end;

function TNPCScope.FindLocal(const AName: UTF8String): TNPCSymbol;
var
  i: Integer;
  Keys: TArray<UTF8String>;
  Key: UTF8String;
  Symbols: TArray<TNPCSymbol>;
//  Symbol: TNPCSymbol;
begin
  Keys := Table.Keys.ToArray;
  Symbols := Table.Values.ToArray;
  try
    for i := Length(Keys) - 1 downto 0 do begin
      Key := Keys[i];
      // Case sensitivity depends on your language
      if SameText(Key, AName) then
        Exit(Symbols[i]);
    end;
  finally
    SetLength(Keys, 0);
    SetLength(Symbols, 0);
  end;

  Result := Nil;

// Other version - possibly faster
//  if not Table.TryGetValue(AName, Result) then
//    Result := Nil;
end;

function TNPCScope.TryGetSymbol(const AName: UTF8String; out ASymbol: TNPCSymbol): Boolean;
begin
  Result := Table.TryGetValue(AName, ASymbol);
end;

function TNPCScope.ResolveSymbol(const AName: UTF8String): TNPCSymbol;
begin
  // non recurent version, maybe faster one ???
//  Scope := Self;
//  while Scope <> nil do begin
//    if Scope.Table.TryGetValue(Name, Result) then
//      Exit;
//    Scope := Scope.Parent;
//  end;
//  Result := Nil;

  if Table.TryGetValue(AName, Result) then
    Exit;
  if Assigned(Parent) then
    Exit(Parent.ResolveSymbol(AName));
  Result := Nil;
end;

procedure TNPCScope.ExecuteDeferred;
var
  i: Integer;
begin
  for i := Deferred.Count - 1 downto 0 do
    Deferred[i].Eval(Self); // LIFO execution
end;

function TNPCScope.ResolveTypeKind(const ATypeRef: TNPC_ASTTypeReference): TNPCSymbolType;
begin
  case ATypeRef.Kind of
    REF_Named: begin
      if not Assigned(ATypeRef.BaseSymbol) then
        Exit(TYPE_Unresolved);

      Result := ATypeRef.BaseSymbol.&Type;
    end;

    REF_Array: begin
      Result := TYPE_Array;
    end;

    REF_Record: begin
      Result := TYPE_Record;
    end;

    REF_Function: begin
      Result := TYPE_Procedure;
    end;
  else
    Result := TYPE_Unresolved;
  end;
end;

function TNPCScope.ResolveNullValue(TypeRef: TNPC_ASTType): Variant;
var
  Ref: TNPC_ASTTypeReference;
begin
  if not (TypeRef is TNPC_ASTTypeReference) then
    raise NPCSemanticError.SemanticError(TypeRef.Location, 'multi-assignment requires tuple');

  Ref := TNPC_ASTTypeReference(TypeRef);

  case ResolveTypeKind(Ref) of
    TYPE_Literal  : begin // other simple types are stored here
      case Ref.Kind of
        REF_Named   : begin   // String, MyType, List<T>
          if not Assigned(Ref.BaseSymbol) then
            Exit(Null);

          //Ref.BaseSymbol.TypeRef
//          AST_BOOLEAN : Result := 0;
//          AST_STRING  : Result := '';
        end;

        REF_Array   : begin // Array of T
          Result := Null;
        end;

        REF_Record  : begin // record ... end
//          Result := CreateEmptyRecord(TypeRef);
        end;

        REF_Function: begin // function/procedure
          Result := Null;
        end;
      end;
    end;

    TYPE_Class    : begin
      Result := Null;
    end;

    TYPE_Pointer  : begin
      Result := Null;
    end;

    TYPE_Procedure: begin // procedure / function
      Result := Null;
    end;
  else
    Result := Null;
  end;
end;

procedure TNPCScope.SetSymbolValue(Sym: TNPCSymbol; const Value: TNPC_ASTValue);
var
  Dest: PByte;
begin
  if Sym = Nil then
    raise NPCCompilerError.Create('internal error: symbol not set');

  if Sym.IsConst then
    raise NPCSemanticError.SemanticError(Sym.Location, Format('cannot assign to const "%s"', [Sym.Name]));

  // Resolve memory location
  if Sym.Scope = Nil then
    raise NPCCompilerError.CreateFmt('symbol "%s" has no scope', [Sym.Name]);

//  if Sym.Scope.Memory = Nil then
//    raise NPCCompilerError.CreateFmt('scope for "%s" has no memory', [Sym.Name]);

//  Dest := Sym.Scope.Memory;
//  Inc(Dest, Sym.Offset);
//
//  // Write value
//  WriteValueToMemory(Dest, Value, Sym.TypeRef, Sym.Size);
  Sym.Value := Value;
end;

procedure TNPCScope.WriteValueToMemory(Dest: PByte; const Value: TNPC_ASTValue; TypeRef: TNPC_ASTType; Size: Integer);
begin
//  case ResolveTypeKind(TypeRef) of
//    // INTEGER / BOOLEAN
//    AST_INT,
//    AST_BOOLEAN : PInteger(Dest)^ := Value.AsInteger;
//
//    // STRING
//    AST_STRING  : PPointer(Dest)^ := Pointer(Value.AsString);
//
//    // ARRAY (reference)
//    AST_ARRAY   : PPointer(Dest)^ := Value.AsPointer;
//
//    // RECORD (value copy)
//    AST_RECORD  : Move(Value.AsRecord^, Dest^, Size);
//
//    // FUNCTION (reference)
//    AST_FUNCTION: PPointer(Dest)^ := Value.AsPointer;
//  else
//    raise NPCCompilerError.Create('unsupported type in WriteValueToMemory');
//  end;
end;

function TNPCScope.DefineBuiltinType(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTType): TNPCSymbol;
begin
  Result := TNPCSymbol.Create(ALocation, AName, KIND_BuiltinType, AType, False, ATypeRef, Nil);
  Result.Size := ASize;
  Table.Add(AName, Result);
end;

function TNPCScope.DefineBuiltinTypeAlias(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTType): TNPCSymbol;
begin
  Result := TNPCSymbol.Create(ALocation, AName, KIND_BuiltinTypeAlias, AType, False, ATypeRef, Nil);
  Result.Size := ASize;
  Table.Add(AName, Result);
end;

function TNPCScope.DefineType(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTType; ADecl: TObject): TNPCSymbol;
begin
  Result := TNPCSymbol.Create(ALocation, AName, KIND_Type, AType, False, ATypeRef, ADecl);
  Result.Size := ASize;
  Table.Add(AName, Result);
end;

function TNPCScope.DefineTypeAlias(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTType; ADecl: TObject): TNPCSymbol;
begin
  Result := TNPCSymbol.Create(ALocation, AName, KIND_TypeAlias, AType, False, ATypeRef, ADecl);
  Result.Size := ASize;
  Table.Add(AName, Result);
end;

function TNPCScope.DefineConst(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTType; ADecl: TObject; AValue: Integer): TNPCSymbol;
begin
  Result := TNPCSymbol.Create(ALocation, AName, KIND_Var, AType, True, ATypeRef, ADecl);
  Result.Size := ASize;
  Result.ConstValue := AValue;
  Table.Add(AName, Result);
end;

function TNPCScope.DefineVar(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTType; ADecl: TObject): TNPCSymbol;
begin
  Result := TNPCSymbol.Create(ALocation, AName, KIND_Var, AType, False, ATypeRef, ADecl);
  Result.Size := ASize;
  Table.Add(AName, Result);
end;

function TNPCScope.DefineSymbol(const ASymbol: TNPCSymbol): TNPCSymbol;
var
  Existing: TNPCSymbol;
begin
  Existing := FindLocal(ASymbol.Name);

  if Existing <> Nil then begin
    if not ((Existing.Kind = KIND_Param) and (ASymbol.Kind = KIND_Return)) or ((Existing.Kind = KIND_Return) and (ASymbol.Kind = KIND_Param)) then
      DuplicateSymbolError(ASymbol, Existing);
  end;

  AddSymbol(ASymbol.Name, ASymbol);
  Result := ASymbol;
end;

function TNPCScope.DefineTupleItem(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTType; ADecl: TObject): TNPCSymbol;
begin
  Result := TNPCSymbol.Create(ALocation, AName, KIND_Tuple, AType, False, ATypeRef, ADecl);
  Result.Size := ASize;
  Table.Add(AName, Result);
end;

function TNPCScope.DefineRecordItem(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPCSymbolType; ASize: Integer; ATypeRef: TNPC_ASTType; ADecl: TObject): TNPCSymbol;
begin
  Result := TNPCSymbol.Create(ALocation, AName, KIND_Record, AType, False, ATypeRef, ADecl);
  Result.Size := ASize;
  Table.Add(AName, Result);
end;

function TNPCScope.DefineProcedure(const ALocation: TNPCLocation; const AName: UTF8String; const AProcedure: TNPC_ASTStatementProcedure): TNPCSymbol;
begin
  if Table.ContainsKey(AName) then
    raise TNPCError.SemanticError(ALocation, Format('duplicate identifier "%s"', [AName]));

  Result := TNPCSymbol.Create(ALocation, AName, KIND_Proc, TYPE_Procedure, False, Nil, AProcedure);
  Table.Add(AName, Result);
end;

function TNPCScope.DefineClassField(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPCSymbolType; ATypeRef: TNPC_ASTType; ADecl: TObject): TNPCSymbol;
begin
  Result := TNPCSymbol.Create(ALocation, AName, KIND_Field, AType, False, ATypeRef, ADecl);
//  Result.Size := ASize;
  Table.Add(AName, Result);
end;

function TNPCScope.DefineClassMethod(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPCSymbolType; ATypeRef: TNPC_ASTType; ADecl: TObject): TNPCSymbol;
begin
  Result := TNPCSymbol.Create(ALocation, AName, KIND_Method, AType, False, ATypeRef, ADecl);
//  Result.Size := ASize;
  Table.Add(AName, Result);
end;

function TNPCScope.DefineClassProperty(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPCSymbolType; ATypeRef: TNPC_ASTType; ADecl: TObject): TNPCSymbol;
begin
  Result := TNPCSymbol.Create(ALocation, AName, KIND_Property, AType, False, ATypeRef, ADecl);
//  Result.Size := ASize;
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
  //
  ResolvedSymbol := Nil;
  InferredType := Nil;
  IsAssignable := True; // default, not possible for: func() := value
  EndToken := Nil;
end;

destructor TNPC_ASTExpression.Destroy;
begin
  ResolvedSymbol := Nil;
  FreeAndNil(InferredType);
  EndToken := Nil;
  inherited;
end;

function TNPC_ASTExpression.ToString: String;
begin
  Result := '<???>';
end;

{ TNPC_ASTType }

constructor TNPC_ASTType.Create;
begin
  inherited;
  &Type := AST_TYPE_DECLARATION;
end;

destructor TNPC_ASTType.Destroy;
begin
  inherited;
end;

function TNPC_ASTType.ToString: String;
begin
  Result := '<type>';
end;

{ TNPC_ASTTypeReference }

constructor TNPC_ASTTypeReference.Create(const ALocation: TNPCLocation);
begin
  inherited Create(ALocation);
  &Type := AST_TYPE_REFERENCE;
  //
  Kind := REF_Unknown;
  // REF_Named
  BaseSymbol := Nil;  // e.g. Array / Map / String
  GenericArgs := Nil; // nested types for generics
  // REF_Array
  ElementType := Nil;
  // REF_Record
  Fields := Nil;
  // REF_Function
  Params := Nil;
  ReturnType := Nil;
  IsFunction := False;
end;

destructor TNPC_ASTTypeReference.Destroy;
begin
  BaseSymbol := Nil;
  if GenericArgs <> Nil then
    FreeAndNil(GenericArgs);
  ElementType := Nil;
  // REF_Record
  if Fields <> Nil then
    FreeAndNil(Fields);
  // REF_Function
  if Params <> Nil then
    FreeAndNil(Params);
  if ReturnType <> Nil then
    FreeAndNil(ReturnType);
  inherited;
end;

function TNPC_ASTTypeReference.ToString: String;
begin
  Result := '<type-ref>';
end;

{ TNPC_ASTTypeDefinition }

constructor TNPC_ASTTypeDefinition.Create(const ALocation: TNPCLocation; const AName: UTF8String; ADefinitionType: TNPC_TypeDefinitionType; ASizeInBytes: Integer);
begin
  inherited Create(ALocation);
  &Type := AST_TYPE_DEFINITION;
  Flags := 0;
  DefinitionType := ADefinitionType;
  Name := AName;
  SizeInBytes := ASizeInBytes;
  // descriptions depends of DefinitionType set
  EnumDescription := Nil;
  SetDescription := Nil;
  ArrayDescription := Nil;
  RecordDescription := Nil;
  ClassDescription := Nil;
end;

destructor TNPC_ASTTypeDefinition.Destroy;
begin
  Name := '';
  if EnumDescription <> Nil then
    FreeAndNil(EnumDescription);
  if SetDescription <> Nil then
    FreeAndNil(SetDescription);
  if ArrayDescription <> Nil then
    FreeAndNil(ArrayDescription);
  if RecordDescription <> Nil then
    FreeAndNil(RecordDescription);
  if ClassDescription <> Nil then
    FreeAndNil(ClassDescription);
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
//    FreeAndNil(Member);
  FreeAndNil(Members);
  inherited;
end;

function TNPC_ASTTypeEnum.ToString: String;
begin
  Result := '<enum>';
end;

{ TNPC_ASTTypeSet }

constructor TNPC_ASTTypeSet.Create(const ALocation: TNPCLocation; const AElementType: TNPC_ASTType);
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
    FreeAndNil(ElementType);
  for i := 0 to Elements.Count - 1 do
    Elements.Items[i].Free;
  FreeAndNil(Elements);
  inherited;
end;

function TNPC_ASTTypeSet.ToString: String;
begin
  Result := '<set>';
end;

{ TNPC_ASTTypeArray }

constructor TNPC_ASTTypeArray.Create(const ALocation: TNPCLocation; AElementType, AIndexType: TNPC_ASTType);
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

{ TNPC_ASTTypeTuple }

constructor TNPC_ASTTypeTuple.Create(const ALocation: TNPCLocation);
begin
  inherited Create(ALocation);
  &Type := AST_TYPE_TUPLE;
  Elements := TDictionary<UTF8String, TNPC_ASTType>.Create;
end;

destructor TNPC_ASTTypeTuple.Destroy;
var
  Element: TNPC_ASTType;
begin
  for Element in Elements.Values do
    Element.Free;
  FreeAndNil(Elements);
  inherited;
end;

function TNPC_ASTTypeTuple.ToString: String;
begin
  Result := '<tuple>';
end;

{ TNPC_ASTTypeRecord }

constructor TNPC_ASTTypeRecord.Create(const ALocation: TNPCLocation);
begin
  inherited Create(ALocation);
  &Type := AST_TYPE_RECORD_DESCRIPTION;
  Fields := TDictionary<UTF8String, TNPC_ASTType>.Create;
end;

destructor TNPC_ASTTypeRecord.Destroy;
var
  Field: TNPC_ASTType;
begin
  for Field in Fields.Values do
    Field.Free;
  FreeAndNil(Fields);
  inherited;
end;

function TNPC_ASTTypeRecord.ToString: String;
begin
  Result := '<record>';
end;

{ TNPC_ASTTypeClassMethod }

constructor TNPC_ASTTypeClassMethod.Create(const ALocation: TNPCLocation; const AName: UTF8String; const AVisibilityType: TNPC_ASTClassVisibilityTypeEnum);
begin
  inherited Create(ALocation);
  &Type := AST_TYPE_CLASS_METHOD;
  //
  Name := AName;
  VisibilityType := AVisibilityType;
  IsFunction := False;
  //
  Expr := Nil; // unified representation
end;

constructor TNPC_ASTTypeClassMethod.Create(const ALocation: TNPCLocation; const AName: UTF8String; const AVisibilityType: TNPC_ASTClassVisibilityTypeEnum; AExpr: TNPC_ASTExpression);
begin
  Create(ALocation, AName, AVisibilityType);
  Expr := AExpr; // unified representation
end;

destructor TNPC_ASTTypeClassMethod.Destroy;
begin
  Name := '';
  if Expr <> Nil then
    FreeAndNil(Expr);
  inherited;
end;

function TNPC_ASTTypeClassMethod.ToString: String;
begin
  Result := '<method>';
end;

function TNPC_ASTTypeClassMethod.VisibilityToString: String;
begin
  case VisibilityType of
    VIS_Class    : Result := 'class';
    VIS_Private  : Result := 'private';
    VIS_Protected: Result := 'protected';
    VIS_Public   : Result := 'public';
  end;
end;

{ TNPC_ASTTypeClassProperty }

//constructor TNPC_ASTTypeClassProperty.Create(const ALocation: TNPCLocation; const AName: UTF8String; APropertyType: TNPC_ASTType; const AVisibilityType: TNPC_ASTClassVisibilityTypeEnum);
constructor TNPC_ASTTypeClassProperty.Create(const ALocation: TNPCLocation; const AName: UTF8String; const AVisibilityType: TNPC_ASTClassVisibilityTypeEnum);
begin
  inherited Create(ALocation);
  &Type := AST_TYPE_CLASS_PROPERTY;
  //
  Name := AName;
  VisibilityType := AVisibilityType;
  PropertyType := Nil;
  //
  ReadField := Nil;
  WriteField := Nil;
  //
  InitExpr := Nil;    // unified representation
  DefaultExpr := Nil; // optional
end;

constructor TNPC_ASTTypeClassProperty.Create(const ALocation: TNPCLocation; const AName: UTF8String; APropertyType: TNPC_ASTType;
  const AVisibilityType: TNPC_ASTClassVisibilityTypeEnum; AReadField, AWriteField: TNPCSymbol; AInitExpr, ADefaultExpr: TNPC_ASTExpression);
begin
  Create(ALocation, AName, AVisibilityType);
  PropertyType := APropertyType;
  //
  ReadField := AReadField;
  WriteField := AWriteField;
  //
  InitExpr := AInitExpr;       // unified representation
  DefaultExpr := ADefaultExpr; // optional
end;

destructor TNPC_ASTTypeClassProperty.Destroy;
begin
  Name := '';
  if PropertyType <> Nil then
    FreeAndNil(PropertyType);
  //
  ReadField := Nil;;
  WriteField := Nil;
  //
  FreeAndNil(InitExpr);    // unified representation
  FreeAndNil(DefaultExpr); // optional
  inherited;
end;

function TNPC_ASTTypeClassProperty.ToString: String;
begin
  Result := '<property>';
end;

{ TNPC_ASTTypeClass }

constructor TNPC_ASTTypeClass.Create(const ALocation: TNPCLocation; const AName: UTF8String);
begin
  inherited Create(ALocation);
  &Type := AST_TYPE_CLASS;
  Name := AName;
  //Scope := Nil;
  Fields := Nil;
  Methods := Nil;
  Properties := Nil;
end;

constructor TNPC_ASTTypeClass.Create(const ALocation: TNPCLocation; const AName: UTF8String; AFields: TDictionary<UTF8String, TNPC_ASTType>; AMethods: TObjectList<TNPC_ASTTypeClassMethod>; AProperties: TObjectList<TNPC_ASTTypeClassProperty>);
begin
  Create(ALocation, AName);
  Fields := AFields;
  Methods := AMethods;
  Properties := AProperties;
end;

destructor TNPC_ASTTypeClass.Destroy;
begin
  //Scope := Nil;
  FreeAndNil(Fields);
  FreeAndNil(Methods);
  FreeAndNil(Properties);
  inherited;
end;

function TNPC_ASTTypeClass.ToString: String;
begin
  Result := '<class>';
end;

{ TNPC_ASTExpressionLiteral }

constructor TNPC_ASTExpressionLiteral.Create(const ALocation: TNPCLocation; const AValue: UTF8String; AType: TNPC_ASTType);
begin
  inherited Create(ALocation);
  &Type := AST_EXPRESSION_LITERAL;
  Value := AValue;
  LiteralType := AType;
end;

destructor TNPC_ASTExpressionLiteral.Destroy;
begin
  Value := '';
  LiteralType := Nil;
//  if (LiteralType <> Nil) and (TNPC_ASTTypeDefinition(LiteralType). = ) then
//    FreeAndNil(LiteralType);
  inherited;
end;

function TNPC_ASTExpressionLiteral.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  Result := Value;
end;

function TNPC_ASTExpressionLiteral.ToString: String;
begin
  Result := '<literal>';
end;

{ TNPC_ASTExpressionEnumConst }

constructor TNPC_ASTExpressionEnumConst.Create(const ALocation: TNPCLocation; const AName: UTF8String; AValue: Integer; AType: TNPC_ASTType);
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
    FreeAndNil(EnumType);
  inherited;
end;

function TNPC_ASTExpressionEnumConst.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  Result := Value;
end;

function TNPC_ASTExpressionEnumConst.ToString: String;
begin
  Result := '<enum-const>';
end;

{ TNPC_ASTExpressionSet }

constructor TNPC_ASTExpressionSet.Create(const ALocation: TNPCLocation; const AElementType: TNPC_ASTType);
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
    FreeAndNil(ElementType);
  for i := 0 to Elements.Count - 1 do
    Elements.Items[i].Free;
  FreeAndNil(Elements);
  inherited;
end;

function TNPC_ASTExpressionSet.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  Result := '';
end;

function TNPC_ASTExpressionSet.ToString: String;
begin
  Result := '<set>';
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
    FreeAndNil(SetType);
  for i := 0 to Elements.Count - 1 do
    Elements.Items[i].Free;
  FreeAndNil(Elements);
  inherited;
end;

function TNPC_ASTExpressionSetLiteral.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  Result := '';
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
  Base := Nil;
end;

destructor TNPC_ASTExpressionVariable.Destroy;
begin
  Name := '';
  Symbol := Nil;
//  if Symbol <> Nil then
//    FreeAndNil(Symbol);
  if Base <> Nil then
    FreeAndNil(Base);
  inherited;
end;

function TNPC_ASTExpressionVariable.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  if not Assigned(Symbol) then
    Symbol := Scope.ResolveSymbol(Name);
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
    FreeAndNil(ResolvedSymbol);
  inherited;
end;

function TNPC_ASTExpressionIdent.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  Result := '';
end;

function TNPC_ASTExpressionIdent.ToString: String;
begin
  Result := '<identifier>';
end;

{ TNPC_ASTExpressionNumber }

constructor TNPC_ASTExpressionNumber.Create(const ALocation: TNPCLocation; const AValue: UTF8String; AType: TNPC_ASTType);
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
    FreeAndNil(LiteralType);
  inherited;
end;

function TNPC_ASTExpressionNumber.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  Result := Value;
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

function TNPC_ASTExpressionString.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  Result := Value;
end;

function TNPC_ASTExpressionString.ToString: String;
begin
  Result := '<string>';
end;

{ TNPC_ASTExpressionNull }

function TNPC_ASTExpressionNull.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  Result := 0;
end;

function TNPC_ASTExpressionNull.ToString: String;
begin
  Result := '<null>';
end;

{ TNPC_ASTExpressionParameter }

//constructor TNPC_ASTExpressionParameter.Create(const ALocation: TNPCLocation; ADeclaration: TNPC_ASTExpression; const AName: UTF8String; const AParameterType: TNPC_ASTType; const AParamValue: TNPC_ASTExpression);
constructor TNPC_ASTExpressionParameter.Create(const ALocation: TNPCLocation; ATypeRef: TNPC_ASTTypeReference; const AName: UTF8String; const AParameterType: TNPC_ASTType; const AParamValue: TNPC_ASTExpression);begin
  inherited Create(ALocation);
  &Type := AST_EXPRESSION_PARAMETER;
  //Declaration := ADeclaration;
  TypeRef := TypeRef;
  Name := AName;
  ParameterType := AParameterType;
  ParamValue := AParamValue;
end;

destructor TNPC_ASTExpressionParameter.Destroy;
begin
  //Declaration := Nil;
  FreeAndNil(TypeRef);
  Name := '';
  ParameterType := Nil;
  FreeAndNil(ParamValue);
  inherited;
end;

function TNPC_ASTExpressionParameter.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  if Assigned(ParamValue) then
    Result := ParamValue.Eval(Scope)
  else
    Result := '';
end;

function TNPC_ASTExpressionParameter.ToString: String;
begin
  Result := '<param>';
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
    FreeAndNil(Right);
  inherited;
end;

function TNPC_ASTExpressionUnary.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  Result := '';
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
    FreeAndNil(Left);
  if Right <> Nil then
    FreeAndNil(Right);
  inherited;
end;

function TNPC_ASTExpressionBinary.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  Result := '';
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
    FreeAndNil(Target);
  if ValueExpr <> Nil then
    FreeAndNil(ValueExpr);
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

{ TNPC_ASTExpressionMultiAssign }

constructor TNPC_ASTExpressionMultiAssign.Create(const ALocation: TNPCLocation; ATargets: TObjectList<TNPC_ASTExpressionVariable>; AValue: TNPC_ASTExpression);
begin
  inherited Create(ALocation);
  &Type := AST_EXPRESSION_MULTI_ASSIGN;
  Targets := ATargets; // TObjectList<TNPC_ASTExpressionVariable>;
  ValueExpr := AValue; // TNPC_ASTExpression; // usually tuple or function call
end;

destructor TNPC_ASTExpressionMultiAssign.Destroy;
begin
  if Targets <> Nil then
    FreeAndNil(Targets);
  if ValueExpr <> Nil then
    FreeAndNil(ValueExpr);
  inherited;
end;

function TNPC_ASTExpressionMultiAssign.Eval(Scope: TNPCScope): TNPC_ASTValue;
var
  Tuple: TNPC_ASTExpressionTuple;
  Values: Array of TNPC_ASTExpression;
  i: Integer;
  Sym: TNPCSymbol;
  Val: TNPC_ASTValue;
begin
  // RHS must be tuple or function returning tuple
  if not (ValueExpr is TNPC_ASTExpressionTuple) then
    raise NPCSemanticError.SemanticError(Location, 'multi-assignment requires tuple');

  Tuple := TNPC_ASTExpressionTuple(ValueExpr);

  // bind tuple -> array
  SetLength(Values, Targets.Count);
  for i := 0 to Targets.Count - 1 do
    Values[i] := Tuple.Items[i].Expr;

  // assign
  for i := 0 to Targets.Count - 1 do begin
    Sym := Targets[i].ResolvedSymbol;

    // type validation
//    Scope.Parser.ValidateTypeCompatibility(Sym.TypeRef, Values[i]);

    // evaluate value
//    if Values[i] is TNPC_ASTExpressionNull then
//      Val := Scope.Parser.ResolveNullValue(Sym.TypeRef)
//    else
//      Val := Values[i].Eval(Scope);
//
//    Scope.Parser.SetSymbolValue(Sym, Val);
  end;

  Result := Default(TNPC_ASTValue);
end;

function TNPC_ASTExpressionMultiAssign.ToString: String;
begin
  Result := '<multi-assign-expr>';
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
    FreeAndNil(Cond);
  if ThenExpr <> Nil then
    FreeAndNil(ThenExpr);
  if ElseExpr <> Nil then
    FreeAndNil(ElseExpr);
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

constructor TNPC_ASTExpressionCaseBranch.Create(const ALocation: TNPCLocation);
begin
  inherited Create(ALocation);
  &Type := AST_EXPRESSION_CASE;
  IfValues := TObjectList<TNPC_ASTExpression>.Create(False);
  ResultExpr := Nil;
end;

destructor TNPC_ASTExpressionCaseBranch.Destroy;
var
  i: Integer;
begin
  for i := 0 to IfValues.Count - 1 do
    IfValues.Items[i].Free;
  FreeAndNil(IfValues);
  if ResultExpr <> Nil then
    FreeAndNil(ResultExpr);
  inherited;
end;

function TNPC_ASTExpressionCaseBranch.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  Result := '';
end;

function TNPC_ASTExpressionCaseBranch.ToString: String;
begin
  Result := '<case-if>';
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
    FreeAndNil(Selector);
  if Branches <> Nil then
    FreeAndNil(Branches);
  if DefaultExpr <> Nil then
    FreeAndNil(DefaultExpr);
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
    FreeAndNil(Left);
  if Right <> Nil then
    FreeAndNil(Right);
  inherited;
end;

function TNPC_ASTExpressionInOp.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  Result := '';
end;

function TNPC_ASTExpressionInOp.ToString: String;
begin
  Result := '<in-op>';
end;

{ TNPC_ASTExpressionArray }

constructor TNPC_ASTExpressionArray.Create(const ALocation: TNPCLocation; ABase, AIndex: TNPC_ASTExpression; AElemType: TNPC_ASTType);
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
    FreeAndNil(Base);
  if Index <> Nil then
    FreeAndNil(Index);
  if ElemType <> Nil then
    FreeAndNil(ElemType);
  inherited;
end;

function TNPC_ASTExpressionArray.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  Result := '';
end;

function TNPC_ASTExpressionArray.ToString: String;
begin
  Result := '<array>';
end;

{ TNPC_ASTExpressionRecord }

constructor TNPC_ASTExpressionRecord.Create(const ALocation: TNPCLocation; ABase: TNPC_ASTExpression; const AFieldName: UTF8String; AFieldType: TNPC_ASTType);
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
    FreeAndNil(Base);
  FieldName := '';
  if FieldType <> Nil then
    FreeAndNil(FieldType);
  inherited;
end;

function TNPC_ASTExpressionRecord.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  Result := '';
end;

function TNPC_ASTExpressionRecord.ToString: String;
begin
  Result := '<record>';
end;

{ TNPC_ASTTupleItem }

constructor TNPC_ASTTupleItem.Create(const ALocation: TNPCLocation);
begin
  inherited Create(ALocation);
  &Type := AST_EXPRESSION_TUPLE_ITEM;
  Name := ''; // '' if positional
  Expr := Nil;
end;

destructor TNPC_ASTTupleItem.Destroy;
begin
  Name := ''; // '' if positional
  FreeAndNil(Expr);
  inherited;
end;

function TNPC_ASTTupleItem.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  Result := '';
end;

function TNPC_ASTTupleItem.ToString: String;
begin
  Result := '<tuple-item>';
end;

{ TNPC_ASTExpressionTuple }

constructor TNPC_ASTExpressionTuple.Create(const ALocation: TNPCLocation);
begin
  inherited Create(ALocation);
  &Type := AST_EXPRESSION_TUPLE;
  //Values := TObjectList<TNPC_ASTExpression>;
  //Values := TDictionary<UTF8String, TNPC_ASTExpression>.Create;
  Items := TObjectList<TNPC_ASTTupleItem>.Create(True);
end;

destructor TNPC_ASTExpressionTuple.Destroy;
begin
  //FreeAndNil(Values);
  FreeAndNil(Items);
  inherited;
end;

function TNPC_ASTExpressionTuple.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  Result := '';
end;

function TNPC_ASTExpressionTuple.ToString: String;
begin
  Result := '<tuple>';
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
    FreeAndNil(Callee);
  for i := 0 to Args.Count - 1 do
    Args.Items[i].Free;
  FreeAndNil(Args);
  inherited;
end;

function TNPC_ASTExpressionCall.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  Result := '';
end;

function TNPC_ASTExpressionCall.ToString: String;
begin
  Result := '<call>';
end;

{ TNPC_ASTExpressionMember }

constructor TNPC_ASTExpressionMember.Create(const ALocation: TNPCLocation; ABase: TNPC_ASTExpression; const AMember: UTF8String; const AMemberSymbol: TNPCSymbol; const AMemberType: TNPC_ASTType);
begin
  inherited Create(ALocation, AMember, AMemberSymbol);
  &Type := AST_EXPRESSION_STRUCT_MEMBER;
  Base := ABase;
  MemberType := AMemberType;
end;

destructor TNPC_ASTExpressionMember.Destroy;
begin
  if MemberType <> Nil then
    FreeAndNil(MemberType);
  inherited;
end;

function TNPC_ASTExpressionMember.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  Result := '';
end;

function TNPC_ASTExpressionMember.ToString: String;
begin
  Result := '<member>';
end;

{ TNPC_ASTExpressionIndex }

constructor TNPC_ASTExpressionIndex.Create(const ALocation: TNPCLocation; const ABase, AIndex: TNPC_ASTExpression);
begin
  inherited Create(ALocation, '', Nil);
  &Type := AST_EXPRESSION_ARRAY_INDEX;
  Base := ABase;
  Index := AIndex;
end;

destructor TNPC_ASTExpressionIndex.Destroy;
begin
  if Index <> Nil then
    FreeAndNil(Index);
  inherited;
end;

function TNPC_ASTExpressionIndex.Eval(Scope: TNPCScope): TNPC_ASTValue;
begin
  Result := '';
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
  Flags := AFlags;
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
  FreeAndNil(Statements);
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
    FreeAndNil(Expression);
  inherited;
end;

{ TNPC_ASTStatementTypeDeclaration }

constructor TNPC_ASTStatementTypeDeclaration.Create(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPC_ASTType);
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
    FreeAndNil(DeclaredType);
  inherited;
end;

{ TNPC_ASTStatementVariableDeclaration }

constructor TNPC_ASTStatementVariableDeclaration.Create(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPC_ASTType; AInit: TNPC_ASTExpression);
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
  DeclaredType := Nil;
//  if DeclaredType <> Nil then
//    FreeAndNil(DeclaredType);
  if Init <> Nil then
    FreeAndNil(Init);
  SymbolRef := Nil;
//  if SymbolRef <> Nil then
//    FreeAndNil(SymbolRef);
  inherited;
end;

{ TNPC_ASTParameter }

constructor TNPC_ASTParameter.Create(const ALocation: TNPCLocation; const AName: UTF8String; AType: TNPC_ASTType; AModifier: TNPC_ASTParamModifier;
  AInit: TNPC_ASTExpression);
begin
  inherited Create(ALocation, AName, AType, AInit);
  Modifier := AModifier;
end;

{ TNPC_ASTStatementProcedure }

constructor TNPC_ASTStatementProcedure.Create(const ALocation: TNPCLocation; AParent: TNPC_ASTStatementBlock; const AName: UTF8String; AIsFunction: Boolean; const AFlags: TNPC_BlockFlags = []);
begin
  inherited Create(ALocation, AParent, AFlags);
  &Type := AST_PROCEDURE;
  Name := AName;
  IsFunction := AIsFunction;
  Parameters := Nil; // TObjectList<TNPC_ASTParameter>.Create(True);
  Returns := Nil;
  Body := Nil;
//  if IsFunction then
//    Returns := TObjectList<TNPC_ASTParameter>.Create(True);
end;

destructor TNPC_ASTStatementProcedure.Destroy;
var
  i: Integer;
begin
  Name := '';
  if Parameters <> Nil then begin
//    for i := 0 to Parameters.Count - 1 do
//      Parameters.Items[i].Free;
    FreeAndNil(Parameters);
  end;
  if Returns <> Nil then begin
//    for i := 0 to Returns.Count - 1 do
//      Returns.Items[i].Free;
    FreeAndNil(Returns);
  end;
  Body := Nil;
//  if Body <> Nil then
//    FreeAndNil(Body);
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
    FreeAndNil(Statement);
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
    FreeAndNil(Target);
  if Value <> Nil then
    FreeAndNil(Value);
  inherited;
end;

{ TNPC_ASTStatementMultiAssign }

constructor TNPC_ASTStatementMultiAssign.Create(const ALocation: TNPCLocation; ATargets: TObjectList<TNPC_ASTExpression>; AValue: TNPC_ASTExpression);
begin
  inherited Create(ALocation);
  &Type := AST_STATEMENT_MULTI_ASSIGN;
  Targets := ATargets; // usually TIdentExpr or member/index
  Value := AValue;
end;

destructor TNPC_ASTStatementMultiAssign.Destroy;
begin
  if Targets <> Nil then
    FreeAndNil(Targets);
  if Value <> Nil then
    FreeAndNil(Value);
  inherited;
end;

{ TNPC_ASTStatementReturn }

constructor TNPC_ASTStatementReturn.Create(const ALocation: TNPCLocation; AExpression: TNPC_ASTExpression);
begin
  inherited Create(ALocation);
  &Type := AST_STATEMENT_RETURN;
  Expr := AExpression; // tuple or single value
end;

destructor TNPC_ASTStatementReturn.Destroy;
begin
  if Expr <> Nil then
    FreeAndNil(Expr);
  inherited;
end;

{ TNPC_ASTStatementResult }

constructor TNPC_ASTStatementResult.Create(const ALocation: TNPCLocation; AExpression: TNPC_ASTExpression);
begin
  inherited Create(ALocation);
  &Type := AST_STATEMENT_RESULT;
  Expr := AExpression; // tuple or single value
end;

destructor TNPC_ASTStatementResult.Destroy;
begin
  if Expr <> Nil then
    FreeAndNil(Expr);
  inherited;
end;

{ TNPC_ASTStatementDefer }

constructor TNPC_ASTStatementDefer.Create(const ALocation: TNPCLocation; AExpr: TNPC_ASTExpression);
begin
  inherited Create(ALocation);
  &Type := AST_STATEMENT_DEFER;
  Expr := AExpr;
end;

destructor TNPC_ASTStatementDefer.Destroy;
begin
  if Expr <> Nil then
    FreeAndNil(Expr);
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
    FreeAndNil(Cond);
  if ThenStmt <> Nil then
    FreeAndNil(ThenStmt);
  if ElseStmt <> Nil then
    FreeAndNil(ElseStmt);
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
  FreeAndNil(IfValues);
  if Stmt <> Nil then
    FreeAndNil(Stmt);
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
    FreeAndNil(Selector);
  if Branches <> Nil then
    FreeAndNil(Branches);
  if DefaultStmt <> Nil then
    FreeAndNil(DefaultStmt);
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
    FreeAndNil(InitStmt);
  if CondExpr <> Nil then
    FreeAndNil(CondExpr);
  if EndExpr <> Nil then
    FreeAndNil(EndExpr);
  if Body <> Nil then
    FreeAndNil(Body);
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
    FreeAndNil(Cond);
  if Body <> Nil then
    FreeAndNil(Body);
  inherited;
end;

end.

