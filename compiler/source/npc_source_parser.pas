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
  Generics.Collections,
  npc_lexer,
  npc_reserved_words,
  npc_reserved_symbols,
  npc_tokenizer,
  npc_tokens,
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
    decfNone
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
    CurrentBlock: TNPC_ASTStatementBlock;
  private
    Imports: TNPCImportArray;
    Builtin_IntegerType: TNPC_ASTTypeDefinition;
    Builtin_RealType: TNPC_ASTTypeDefinition;
    Builtin_BooleanType: TNPC_ASTTypeDefinition;
  private
    FLevel: Integer;
    //
    procedure AddProjectImport(const ASourceFile, ACodeName: String);
    function  TypeToString(const Sym: TNPCSymbol): String;
    function  TypeToName(const Typ: TNPC_ASTExpression): String;
  protected
    ParsingType: TNPCParsingType;
    //
    function Unescape(const Value: String): String; inline;
    function  GetPrecedence(const AToken: TNPCToken): Integer;
    procedure EnterScope(const AParentScope: TNPCScope); //inline;
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
    function  ResolveType(const ATypeName: String; out AType: TNPC_ASTTypeExpression): Boolean;
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

    function  ParseStatement(const AFlags: TNPCParseStatementFlags): TNPC_ASTStatement;
    function  ParseExpression(const AToken: TNPCToken; Precedence: Integer = 0): TNPC_ASTExpression;
    function  ParsePrimaryExpression(const AToken: TNPCToken): TNPC_ASTExpression;
    function  ParseSimpleExpression(const AToken: TNPCToken; const ALeft: TNPC_ASTExpression): TNPC_ASTExpression;

    procedure ParseTypeDeclaration(const AToken: TNPCToken; const AFlags: TNPCParseDeclarationFlags);
    function  ParseTypeDefinition(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
    function  ParseEnumType(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
    function  ParseSetType(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
    function  ParseSetOfType(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
    function  ParseArrayType(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
    function  ParseRecordType(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
    //
    procedure ParseVariableDeclaration(const AToken: TNPCToken);
    function  ParseVariableType(const AVarToken: TNPCToken; const AVarName: String): TNPC_ASTTypeExpression;

    procedure ParseBlock(const AToken: TNPCToken);
    function  ParseAssignment(const ALeftToken: TNPCToken): TNPC_ASTStatement;
    function  ParseIdentifier(const AToken: TNPCToken): TNPC_ASTExpression;
    function  ParseLiteral(const AToken: TNPCToken): TNPC_ASTExpression;
    function  ParseSet(const AToken: TNPCToken): TNPC_ASTExpression;
    function  ParseIndex(const AToken: TNPCToken; const ALeft: TNPC_ASTExpression): TNPC_ASTExpression;
    function  ParseRecordMember(const AToken: TNPCToken; const ALeft: TNPC_ASTExpression): TNPC_ASTExpression;
    function  ParseIf(const AToken: TNPCToken): TNPC_ASTStatementIf;
    function  ParseCase(const AToken: TNPCToken): TNPC_ASTStatementCase;
    function  ParseCaseBranches(const ASelector: TNPC_ASTExpression; const ACaseOf: Boolean; out ADefaultBranch: TNPC_ASTExpression): TNPC_ASTCaseBranches;
    function  ParseFor(const AToken: TNPCToken): TNPC_ASTStatementFor;
    procedure ParseForParams;
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

    function  ParseDeclarations(const AToken: TNPCToken): Boolean;
    function  ParseStatements(var AToken: TNPCToken; const AExitOnReservedIdents: Array of TNPCReservedIdents; const ALevel: Integer): Boolean;
    procedure ParseLambdaParams(const AToken: TNPCToken);
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

function  NPC_CompileImport(const ASourceFileName: PChar; const ACompiler: TNPCCompiler; const AParentParser: TNPCSourceParser; out ASource: TNPCSourceParser): Boolean; stdcall; overload;
function  NPC_CompileImport(const ASourceStream: TStringStream; const ASourceFileName: PChar; const ACompiler: TNPCCompiler; const AParentParser: TNPCSourceParser; out ASource: TNPCSourceParser): Boolean; stdcall; overload;

function  NPC_CompileSource(const ASourceCode: PChar; const ACompiler: TNPCCompiler; const AParentParser: TNPCSourceParser; out ASource: TNPCSourceParser): Boolean; stdcall; overload;
function  NPC_CompileSource(const ASourceStream: TStringStream; const ACompiler: TNPCCompiler; const AParentParser: TNPCSourceParser; out ASource: TNPCSourceParser): Boolean; stdcall; overload;

procedure NPC_CompilerFree(var ASource: TNPCSourceParser); stdcall;

implementation

uses
  Math,
  StrUtils,
  npc_consts,
  npc_project,
  npc_project_settings,
  npc_md5,
  npc_location,
  npc_types;

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
  ScopeStack := TObjectList<TNPCScope>.Create(False);
  CurrentScope := Nil;
end;

destructor TNPCSourceParser.Destroy;
begin
  Clear;
  // clear scopes
  ScopeStack.Free;
  inherited;
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
    Result := TNPC_ASTExpressionMember(Typ).Member
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
  else if TokenIsReservedSymbol(AToken, [rs_Dot, rs_OBracket]) then // '.', '[' // member access very high / indexing high
    Exit(9)
  else
    Result := 0;
end;

procedure TNPCSourceParser.EnterScope(const AParentScope: TNPCScope);
begin
  ScopeStack.Add(TNPCScope.Create(AParentScope));
  CurrentScope := ScopeStack.Last;
  if AParentScope = Nil then
    InitBuiltins(CurrentScope);
end;

procedure TNPCSourceParser.LeaveScope;
var
  s: TNPCScope;
begin
  if ScopeStack.Count = 0 then
    Exit;
  //
  s := ScopeStack.Last;
  ScopeStack.Delete(ScopeStack.Count - 1);
  s.Free;
  //
  if ScopeStack.Count > 0 then
    CurrentScope := ScopeStack.Last
  else
    CurrentScope := Nil;
end;

procedure TNPCSourceParser.InitBuiltins(const AScope: TNPCScope);
begin
  // register built-in types
  Builtin_IntegerType := TNPC_ASTTypeDefinition.Create(Nil, 'Integer', 4);
  AScope.DefineBuiltinType('Integer', stLiteral, 4, Builtin_IntegerType);

  Builtin_IntegerType := TNPC_ASTTypeDefinition.Create(Nil, 'Real', 8);
  AScope.DefineBuiltinType('Real', stLiteral, 8, Builtin_RealType);

  Builtin_BooleanType := TNPC_ASTTypeDefinition.Create(Nil, 'Boolean', 1);
  AScope.DefineBuiltinType('Boolean', stLiteral, 1, Builtin_BooleanType);

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
end;

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
  // clear scopes
  while ScopeStack.Count > 0 do
    LeaveScope;
  if ASTree <> Nil then
    FreeAndNil(ASTree);
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
  scope.AddOrSetValue(AName, ASym);
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
    if scope.TryGetValue(AName, sym) then
      Exit(sym);
  end;
end;

function TNPCSourceParser.ResolveType(const ATypeName: String; out AType: TNPC_ASTTypeExpression): Boolean;
var
  VarSym: TNPCSymbol;
begin
  AType := Nil;
  VarSym := CurrentScope.Resolve(ATypeName);
  if not Assigned(VarSym) then
    Exit(False);

  AType := VarSym.TypeRef;
  Result := True;
end;

function TNPCSourceParser.Parse(const ASource: TStringStream; const ASourceFile: String; const ParsingImport: Boolean = False): Boolean;
var
  token: TNPCToken;
//  idx: Integer;
begin
  Result := False;

  EnterScope(CurrentScope);

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
        token := Texer.ExpectToken(tokIdent, '');
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
  token: TNPCToken;
begin
  while Texer.IsNotEmpty do begin
    SkipComments;
    token := Texer.GetToken; // add relevant tokens
    if TokenIsReservedSymbol(token, rs_OCurly) then begin
      if Texer.IsCurrentSymbol('$') then
        ParseSettingDefineCondition(ABlock)
      else if Texer.IsCurrentSymbol('@') then
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]))
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
    else if TokenIsReservedIdent(token, ri_begin) then begin
      ParseBegin(token);
    end
    else if TokenIsReservedIdent(token, ri_end) then begin
      ParseEnd(token);
      Break;
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
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
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sCodeFile]))
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
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sCodeFile]));
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
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
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
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
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
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
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
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
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
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
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
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectSetting]));
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
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
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
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
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
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
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
  token := Texer.PeekToken;
  //
  if TokenIsIdent(token) then begin // might be: label, assignment, expression-statement
    next_token := Texer.NextToken;
    if TokenIsReservedSymbol(next_token, rs_Colon) then begin // ':' - label
      Texer.SkipToken;
      Texer.SkipToken;
      stmt := ParseStatement([stafEmptyStatementIsAcceptable]);
      Result := TNPC_ASTStatementLabel.Create(token.Location, token.Value, stmt);
      Exit;
    end
    else if TokenIsReservedSymbol(next_token, rs_Assign) then begin // ':=' - assignment
      Texer.SkipToken;
      Result := ParseAssignment(token);
      Exit;
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
    ParseBlock(token);
    Result := Nil;
  end
  else if TokenIsReservedIdent(token, ri_if) then begin
    Result := ParseIf(token);
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
  left := ParsePrimaryExpression(token); // nud
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
      left := ParseSimpleExpression(token, left); // led
    end;
    Result := left;
  except
    left.Free;
    raise;
  end;
end;

function TNPCSourceParser.ParsePrimaryExpression(const AToken: TNPCToken): TNPC_ASTExpression;
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
  token: TNPCToken;
  op: UTF8String;
  inner: TNPC_ASTExpression;
begin
  if TokenIsIdent(AToken) or TokenIsNumber(AToken) or TokenIsString(AToken) or TokenIsChar(AToken) then begin
    Result := ParseLiteral(AToken);
  end
  else if TokenIsReservedSymbol(AToken, [rs_Minus, rs_Plus]) then begin
    Texer.SkipToken;
    token := Texer.PeekToken;
    op := AToken.Value;
    inner := ParseExpression(token, GetPrecedence(token)); // unary - bind tighter than additive
    Result := TNPC_ASTExpressionUnary.Create(AToken.Location, op, inner);
  end
  else if TokenIsReservedIdent(token, ri_not) or TokenIsReservedSymbol(token, rs_Exclamation) then begin // 'not' / '!'
    Texer.SkipToken;
    token := Texer.PeekToken;
    op := token.Value;
    inner := ParseExpression(token, 7);
    Result := TNPC_ASTExpressionUnary.Create(AToken.Location, op, inner);
  end
  else if TokenIsReservedSymbol(token, rs_OParen) then begin // '('
    Texer.SkipToken;
    token := Texer.PeekToken;
    inner := ParseExpression(token, 0);
    Texer.ExpectReservedSymbol(rs_CParen); // ')'
    Result := inner;
  end
  else if TokenIsReservedSymbol(token, rs_OBracket) then begin // '['
    Texer.SkipToken;
    inner := ParseSet(AToken);
    Texer.ExpectReservedSymbol(rs_CBracket); // ']'
    Result := inner;
  end
  else
    raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserUnexpectedTokenIn, [AToken.TokenToString, '', sStatement]));
end;

function TNPCSourceParser.ParseSimpleExpression(const AToken: TNPCToken; const ALeft: TNPC_ASTExpression): TNPC_ASTExpression;
var
  token: TNPCToken;
  op: UTF8String;
  right: TNPC_ASTExpression;
  idx: TNPC_ASTExpressionIndex;
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
    Result := ParseRecordMember(AToken, ALeft);
  end
  else if TokenIsReservedSymbol(AToken, rs_OParen) then begin // call: ident '(' expr (, expr) ')'
    Result := ParseCall(AToken, ALeft);
  end
  else if TokenIsReservedSymbol(AToken, rs_OBracket) then begin // indexing: '[' expr ']'
    Result := ParseIndex(AToken, ALeft);
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
    raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserUnexpectedTokenIn, [AToken.TokenToString, '', sStatement]));
end;

procedure TNPCSourceParser.ParseTypeDeclaration(const AToken: TNPCToken; const AFlags: TNPCParseDeclarationFlags);
var
  token, next_token: TNPCToken;
  stmt: TNPC_ASTStatementTypeDeclaration;
begin
  // assume current token at 'type'
  Texer.ExpectReservedToken(ri_type);
  while Texer.IsNotEmpty do begin
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
    Result := ParseEnumType(ATypeToken, ATypeName);
  end
  else if TokenIsReservedSymbol(token, rs_OBracket) then begin // '[' - set
    Texer.SkipToken;
    Result := ParseSetType(ATypeToken, ATypeName);
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
    Result := ParseSetOfType(ATypeToken, ATypeName);
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
    token := Texer.PeekToken;
    Result := ParseArrayType(ATypeToken, ATypeName);
//    Result := TNPC_ASTTypeDefinition.Create(ATypeToken.Location, ATypeName, -1); // size will be determined during type & size checking phase
//    Result.DefinitionType := DEF_Array;
//    Result.RecordDescription := TNPC_ASTTypeRecord(typeRef);
  end
  else if TokenIsReservedIdent(token, ri_record) then begin // 'record'
    _record:
    Texer.SkipToken;
    token := Texer.PeekToken;
    Result := ParseRecordType(ATypeToken, ATypeName);
//    Result := TNPC_ASTTypeDefinition.Create(ATypeToken.Location, ATypeName, -1); // size will be determined during type & size checking phase
//    Result.DefinitionType := DEF_Record;
//    Result.RecordDescription := TNPC_ASTTypeRecord(typeRef);
  end
  else if TokenIsReservedIdent(token, ri_class) then begin // 'class'
    Abort;
  end
  else if TokenIsReservedIdent(token, ri_object) then begin // 'object'
    Abort;
  end
  else if TokenIsReservedIdent(token, ri_type) then begin // 'type' ident
    Abort;
  end
  else if TokenIsIdent(token) then begin // procedure / function
    Abort;
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

// ident = (enum1, enum2, ... );
function TNPCSourceParser.ParseEnumType(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
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
  repeat
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
    end;

    Enum.Members.Add(Ident.Value, Value);

    // add enum member to current scope as constant
    CurrentScope.DefineConst(Ident.Value, stEnumConst, 4, Builtin_IntegerType, Enum, Value);

    Inc(Value);
  until not TokenIsReservedSymbol(token, rs_Comma);
  Texer.ExpectReservedSymbol(rs_CParen); // ')'

  // add enum to current scope as type
  CurrentScope.DefineType(ATypeName, stEnum, 4, Enum, CurrentBlock);

  TypeDef := TNPC_ASTTypeDefinition.Create(ATypeToken.Location, ATypeName, -1); // size will be determined during type & size checking phase
  TypeDef.DefinitionType := DEF_Enum;
  TypeDef.EnumDescription := Enum;

  Result := TNPC_ASTStatementTypeDeclaration.Create(ATypeToken.Location, ATypeName, TypeDef);
end;

function TNPCSourceParser.ParseSetType(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
var
  token: TNPCToken;
  SetType: TNPC_ASTTypeSet;
  ElemType: TNPC_ASTTypeExpression;
  ThisType: TNPC_ASTTypeExpression;
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
          Sym := CurrentScope.Resolve(TNPC_ASTExpressionVariable(Elem).Name);
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
  CurrentScope.DefineType(ATypeName, stSet, 4, SetType, CurrentBlock);

  TypeDef := TNPC_ASTTypeDefinition.Create(ATypeToken.Location, ATypeName, -1); // size will be determined during type & size checking phase
  TypeDef.DefinitionType := DEF_Set;
  TypeDef.SetDescription := SetType;

  Result := TNPC_ASTStatementTypeDeclaration.Create(ATypeToken.Location, ATypeName, TypeDef);
end;

function TNPCSourceParser.ParseSetOfType(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
begin
  Abort;
end;

function TNPCSourceParser.ParseArrayType(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
var
  token: TNPCToken;
  expr: TNPC_ASTExpression;
  ArrayType: TNPC_ASTTypeArray;
  Sym: TNPCSymbol;
  IndexType: TNPC_ASTTypeExpression;
  ElemType: TNPC_ASTTypeExpression;
  TypeDef: TNPC_ASTTypeDefinition;
begin
//    Texer.ExpectReservedToken(ri_of); // 'of'
//    Result := TTypeArray.Create(ParseType);
//    Texer.ExpectReservedSymbol(rs_OBracket); // '['
  IndexType := Nil;
  Texer.SkipToken;
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
      Sym := CurrentScope.Resolve(TNPC_ASTExpressionVariable(expr).Name);
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

  if expr is TNPC_ASTExpressionEnumConst then
    ElemType := TNPC_ASTExpressionEnumConst(expr).EnumType
  else if expr is TNPC_ASTExpressionLiteral then
    ElemType := TNPC_ASTExpressionLiteral(expr).LiteralType
  else if expr is TNPC_ASTExpressionVariable then begin
    Sym := CurrentScope.Resolve(TNPC_ASTExpressionVariable(expr).Name);
    if not Assigned(Sym) then
      raise NPCSyntaxError.ParserError(expr.Location, Format(sParserUnknownIdentIn, [TNPC_ASTExpressionVariable(expr).Name, 'array declaration ', sStatement]));
    ElemType := Sym.TypeRef;
  end
  else
    raise NPCSyntaxError.ParserError(expr.Location, Format(sParserTypeMismatchInLiteral, ['array type', '<number>', expr.ToString]));

  ArrayType := TNPC_ASTTypeArray.Create(ATypeToken.Location, ElemType, IndexType);

  // add array to current scope as type
  CurrentScope.DefineType(ATypeName, stArray, 4, ArrayType, CurrentBlock);

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

function TNPCSourceParser.ParseRecordType(const ATypeToken: TNPCToken; const ATypeName: String): TNPC_ASTStatementTypeDeclaration;
var
  token: TNPCToken;
  expr: TNPC_ASTExpression;
  RecordType: TNPC_ASTTypeRecord;
  Fields: TObjectList<TNPCToken>;
  fieldTypeSym,
  Sym: TNPCSymbol;
  fieldType: TNPC_ASTTypeExpression;
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
  CurrentScope.DefineType(ATypeName, stRecord, 4, RecordType, CurrentBlock);
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
          CurrentScope.DefineVar(fieldName.Value, stRecordField, -1, fieldType, TypeDef);
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

procedure TNPCSourceParser.ParseVariableDeclaration(const AToken: TNPCToken);
var
  token, next_token: TNPCToken;
  varName: TNPCToken;
  varType: TNPC_ASTTypeExpression;
  initExpr: TNPC_ASTExpression;
  stmt: TNPC_ASTStatementVariableDeclaration;
  sym: TNPCSymbol;
begin
  // current token is 'var'
  Texer.ExpectReservedToken(ri_var);
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
    sym := CurrentScope.DefineVar(varName.Value, stLiteral, -1, varType, CurrentBlock);

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

function TNPCSourceParser.ParseVariableType(const AVarToken: TNPCToken; const AVarName: String): TNPC_ASTTypeExpression;
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

procedure TNPCSourceParser.ParseBlock(const AToken: TNPCToken);
var
  token: TNPCToken;
  stmt: TNPC_ASTStatement;
  oldBlock: TNPC_ASTStatementBlock;
begin
  Texer.ExpectReservedToken(ri_begin);
  EnterScope(CurrentScope); // new lexical scope for this block
  stmt := TNPC_ASTStatementBlock.Create(AToken.Location, CurrentBlock, [BLOCK_ConsistsOfOrderedStatements]);
  AddStatement(stmt);
  try
    oldBlock := CurrentBlock;
    CurrentBlock := TNPC_ASTStatementBlock(stmt);
    token := Texer.PeekToken;
    while not (TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly)) do begin
      if TokenIsOfType(token, [tokEOF]) then
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserExpectedElementsButGot, ['statement', 'EOF', 'block ', sDeclaration]));
      stmt := ParseStatement([stafEmptyStatementIsAcceptable]);
      if Assigned(stmt) then
        AddStatement(stmt);
    end;
    token := Texer.PeekToken;
    if not (TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly)) then
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.Value, '', sStatement]));
  finally
    CurrentBlock := oldBlock;
    LeaveScope;
  end;
end;

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

  Sym := CurrentScope.Resolve(TNPC_ASTExpressionVariable(Left).Name);
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
      Result := TNPC_ASTExpressionLiteral.Create(AToken.Location, IntToStr(TNPC_ASTExpressionEnumConst(Arg).Value), Builtin_IntegerType)
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
    Sym := CurrentScope.Resolve(Name);
    if Assigned(Sym) then begin
      case Sym.Kind of
        //skConst:
        skVar  : begin
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
var
  Sym: TNPCSymbol;
begin
  // assume integer literals for now
  case AToken.&Type of
    tokIdent: begin
      Texer.SkipToken;
      // an identifier may be followed by '(' (call) or '[' (index) and will be handled in infix
      Sym := CurrentScope.Resolve(Atoken.Value);
      if not Assigned(Sym) then
        raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserUnknown, ['variable', AToken.Value]));
      Result := TNPC_ASTExpressionLiteral.Create(AToken.Location, AToken.Value, Sym.TypeRef);
    end;
    tokNumber: begin
      Texer.SkipToken;
      Sym := CurrentScope.Resolve(Atoken.Value);
      if not Assigned(Sym) then
        raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserUnknown, ['variable', AToken.Value]));
      Result := TNPC_ASTExpressionLiteral.Create(AToken.Location, AToken.Value, Sym.TypeRef);
    end;
    tokString: begin
      Texer.SkipToken;
      Sym := CurrentScope.Resolve(Atoken.Value);
      if not Assigned(Sym) then
        raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserUnknown, ['variable', AToken.Value]));
      Result := TNPC_ASTExpressionLiteral.Create(AToken.Location, AToken.Value, Sym.TypeRef);
    end;
  else
    raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserUnknownIdentIn, [AToken.Value, sExpression, '']));
  end;
end;

function TNPCSourceParser.ParseSet(const AToken: TNPCToken): TNPC_ASTExpression;
var
  token: TNPCToken;
  Elements: TList<TNPC_ASTExpression>;
  expr: TNPC_ASTExpression;
  SetType: TNPC_ASTTypeSet;
  ElemType: TNPC_ASTTypeExpression;
  Elem: TNPC_ASTExpression;
  ThisType: TNPC_ASTTypeExpression;
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
          Sym := CurrentScope.Resolve(TNPC_ASTExpressionVariable(Elem).Name);
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

  VarSym := CurrentScope.Resolve(array_ident);
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
  VarSym: TNPCSymbol;
  RecType: TNPC_ASTTypeRecord;
  record_ident:  UTF8String;
  Field: UTF8String;
  FieldType: TNPC_ASTTypeExpression;
begin
  Texer.SkipToken;
  token := Texer.ExpectToken(tokIdent, Format(sParserExpectedNameAfter, ['identifier', 'record member', ''])); // expect identifier
  Field := token.Value;

  if not (ALeft is TNPC_ASTExpressionVariable) then
    raise NPCSyntaxError.ParserError(ALeft.Location, Format(sParserCantAccessFieldOnNonVar, [Field, ALeft.ToString]));

  record_ident := TNPC_ASTExpressionVariable(ALeft).Name;

  VarSym := CurrentScope.Resolve(record_ident);
  if not Assigned(VarSym) or not (VarSym.TypeRef is TNPC_ASTTypeRecord) then
    raise NPCSyntaxError.ParserError(ALeft.Location, Format(sParserTypeRequiredForIdent, ['record', record_ident, TNPC_ASTExpressionVariable(ALeft).ToString]));

  AToken.Location.SetEndRowCol(token.Location.EndRow, token.Location.EndCol);

  RecType := TNPC_ASTTypeRecord(VarSym.TypeRef);
  if not RecType.Fields.ContainsKey(Field) then
    raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserFieldNotFoundInRecord, [Field, record_ident]));

  FieldType := RecType.Fields[Field];
  Result := TNPC_ASTExpressionMember.Create(ALeft.Location, ALeft, Field, FieldType);
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

function TNPCSourceParser.ParseIf(const AToken: TNPCToken): TNPC_ASTStatementIf;
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
      raise NPCSyntaxError.ParserError(Texer.LastToken.Location, Format(sParserUnexpectedTokenIn, [Texer.LastToken.TokenToString, 'if ', sStatement]));
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
  if (Texer.LastToken <> Nil) and not TokenIsReservedSymbol(Texer.LastToken, rs_Semicolon) then
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

function TNPCSourceParser.ParseCase(const AToken: TNPCToken): TNPC_ASTStatementCase;
var
//  AToken: TNPCToken;
  token: TNPCToken;
  case_selector: TNPC_ASTExpression;
  case_of: Boolean;
  case_branches: TNPC_ASTCaseBranches;
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
    raise NPCSyntaxError.ParserError(Texer.LastToken.Location, Format(sParserUnexpectedTokenIn, [Texer.LastToken.TokenToString, 'case ', sStatement]));
  // parse if branches
  case_branches := ParseCaseBranches(case_selector, case_of, case_default);
  //
  token := Texer.GetToken;
  if (case_of and not TokenIsReservedIdent(token, ri_end)) or (not case_of and not TokenIsReservedSymbol(token, rs_CCurly)) then
    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'case ', sStatement]));
  //
  token := Texer.GetToken;
  if not TokenIsReservedSymbol(token, rs_Semicolon) then
//    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'case ', sStatement]));
    raise NPCSyntaxError.ParserError(Texer.LastToken.Location.After, Format(sParserExpectedButGot, [NPCReservedSymbolToString(rs_Semicolon), token.TokenToString]));

  Result := TNPC_ASTStatementCase.Create(AToken.Location, case_selector, case_branches, case_default);
end;

// CaseElement = 'if' CaseLabel { ',' CaseLabel } ':' [ '{@next' [ ':' CaseElement ] '}' ] Statement .
//
// CaseLabel = ConstExpression [ '..' ConstExpression ] .
//
// ConstExpression = Expression .

function TNPCSourceParser.ParseCaseBranches(const ASelector: TNPC_ASTExpression; const ACaseOf: Boolean; out ADefaultBranch: TNPC_ASTExpression): TNPC_ASTCaseBranches;
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
  Branch: TNPC_ASTCaseBranch;
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

  Result := TNPC_ASTCaseBranches.Create(True);

  token := Texer.PeekToken;
  while not (ACaseOf and TokenIsReservedIdent(token, ri_end)) and not TokenIsReservedIdent(token, ri_else) do begin
    Branch := TNPC_ASTCaseBranch.Create;
    Texer.ExpectReservedToken(ri_if);
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

// for i := 0; i < 10; i += 1 {
//
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

function TNPCSourceParser.ParseFor(const AToken: TNPCToken): TNPC_ASTStatementFor;
//var
//  AToken: TNPCToken;
//  token: TNPCToken;
//  for_do: Boolean;
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
  varName: string;
  assignTok: TToken;
  startExpr, endExpr: TNPC_ASTExpression;
  body: TStmt;
  dummyAssign: TAssignStmt;
  beginStmt: TBlockStmt;
begin
  Eat(tkFor);
  if FToken.Kind <> tkIdentifier then
    raise Exception.Create('Expected identifier after for');
  varName := FToken.Text;
  Advance;
  Eat(tkAssign);
  startExpr := ParseExpression(0);
  if FToken.Kind = tkTo then Advance
  else if FToken.Kind = tkDownto then Advance
  else raise Exception.Create('Expected to or downto in for');
  endExpr := ParseExpression(0);
  Eat(tkDo);
  body := ParseStatement;
  // represent as a block: var i := start; while i <= end do begin body; inc(i); end
  beginStmt := TBlockStmt.Create;
  beginStmt.Stmts.Add(TVarDeclStmt.Create(varName, startExpr));
  // condition expression:
  var cond: TNPC_ASTExpression := TBinaryExpr.Create(TIdentExpr.Create(varName), '<=', endExpr);
  var whileBody := TBlockStmt.Create;
  whileBody.Stmts.Add(body);
  // inc: i := i + 1
  var incExpr := TBinaryExpr.Create(TIdentExpr.Create(varName), '+', TNumberExpr.Create('1'));
  whileBody.Stmts.Add(TAssignStmt.Create(TIdentExpr.Create(varName), incExpr));
  beginStmt.Stmts.Add(TWhileStmt.Create(cond, whileBody));
  Result := beginStmt;
end;

// var i := 0; i < 10; i += 1 {
// i := 0; i < 10; i += 1 {
// var i := 0, j := 0; i < 10; i += 1, j += 2 {
//
// decalaration on variable before for loop
// var i: Int;
// i := 0;
//
// ; i < 10; i += 1 {
// ; i < 10; {
// i < 10 {
// {
//
// range loop
// i in 0..10 {
//
// item value in
// i in String/Array/Slice/Set/Map {
//
// item value, index in
// value, idx in String/Array/Slice/Set/Map {

procedure TNPCSourceParser.ParseForParams;
var
  token: TNPCToken;
  sect_init: TNPC_ASTStatement;
  sect_cond: TNPC_ASTExpression;
  sect_post: TNPC_ASTExpression;
begin
  SkipComments;
  // determine if there is 3 sections of parameters
  sect_init := Nil;
  sect_cond := Nil;
  sect_post := Nil;
  //
  if Texer.IsNotEmpty then begin
    token := Texer.PeekToken;
    // check if semicolon was first
    if not TokenIsReservedSymbol(token, rs_Semicolon) then begin
      sect_init := ParseStatement([]);
      SkipComments;
      token := Texer.PeekToken;
    end
    else begin
      Texer.SkipToken;
      SkipComments;
      token := Texer.PeekToken;
    end;
    // check if semicolon was second
    if not TokenIsReservedSymbol(token, rs_Semicolon) then begin
      //sect_cond := ParseExpression;
      SkipComments;
      token := Texer.PeekToken;
    end
    else begin
      Texer.SkipToken;
      SkipComments;
      token := Texer.PeekToken;
    end;
    //sect_post := ParseExpression;
    SkipComments;
    token := Texer.PeekToken;
//    if TokenIsReservedIdent(token, ri_var) or
//       (not token.ReservedWord and not token.ReservedSymbol and (token.&Type in [tokIdent..tokChar])) or
//       (token.ReservedSymbol and (token.&Type in [tokMinus..tokDiv, tokDoubleDot..tokModEqual])) then begin
////      AddToken(token);
//    end
//    else if TokenIsReservedSymbol(token, rs_Semicolon) then begin
////      AddToken(token);
//      Inc(sect_cnt);
//    end
    if TokenIsReservedIdent(token, ri_do) or TokenIsReservedSymbol(token, rs_OCurly) then
      Exit
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'for ', sStatement]));
  end;
end;

function TNPCSourceParser.ParseWhile: TNPC_ASTStatementWhile;
var
  cond: TNPC_ASTExpression;
  body: TStmt;
begin
  ExpectKind(tkWhile);
  cond := ParseExpression(0);
  ExpectKind(tkDo);
  body := ParseStatement;
  Result := TWhileStmt.Create(cond, body);
end;

function TNPCSourceParser.ParseCall(const AToken: TNPCToken; const ALeft: TNPC_ASTExpression): TNPC_ASTExpressionCall;
var
  token: TNPCToken;
  right: TNPC_ASTExpression;
begin
  // call: Left is callee
  Result := TNPC_ASTExpressionCall.Create(ALeft.Location, ALeft);
  token := Texer.PeekToken;
  if Texer.IsNotEmpty and not TokenIsReservedSymbol(token, rs_CParen) then begin // ')'
    while not TokenIsReservedSymbol(token, rs_CParen) do begin
      right := ParseExpression(0);
      if Assigned(right) then
        callNode.Args.Add(right);
      token := Texer.PeekToken;
      if TokenIsReservedSymbol(token, rs_Comma) then begin
        Texer.SkipToken
        token := Texer.PeekToken;
      end
      else
        Break;
    end;
  end;
  Texer.ExpectReservedSymbol(rs_CParen); // ')'
  Result := callNode;
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
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'call params ', sSection]));
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
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'call params ', sStatement]));
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
          Result := SearchInFiles(Parser, checked_path, '*.*', TNPCProjectSettings(Parser.SettingsPtr^), found_path, CodeName);
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
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
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
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'type ', sDeclaration]));
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
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
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
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'const ', sDeclaration]));
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
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
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
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'var ', sDeclaration]));
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
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
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
    if ParseStatements(token, [ri_finalization, ri_begin], FLevel + 1) then begin
      // if statements ware parsed than do nothing
      has_body := True;
    end;
    //
    if TokenIsReservedIdent(token, ri_finalization) or TokenIsReservedIdent(token, ri_begin) then begin
      if not has_body then
        raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserSectionHasNoBody, [NPCReservedIdentifiers[ri_initialization].Ident]));
      Break;
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'initialization ', sSection]));
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
    if ParseStatements(token, [ri_begin], FLevel + 1) then begin
      // if statements ware parsed than do nothing
      has_body := True;
    end;
    //
    if TokenIsReservedIdent(token, ri_begin) then begin
      if not has_body then
        raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserSectionHasNoBody, [NPCReservedIdentifiers[ri_initialization].Ident]));
      Break;
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'finalization ', sSection]));
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
      Texer.SkipToken;
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
    end;
  end;

  procedure PrintType(Typ: TNPC_ASTTypeExpression; Level: Integer = 0);
  var
    Pair: TPair<UTF8String, Integer>;
    Field: TPair<UTF8String, TNPC_ASTTypeExpression>;
  begin
//    if Typ is TNPC_ASTTypeDefinition then begin
//      Indent(Level);
//      tf.WriteLine('TypeName(', TNPC_ASTTypeDefinition(Typ).Name, ')');
//    end
//    else
    if Typ is TNPC_ASTTypeEnum then begin
      Indent(Level);
      tf.WriteLine('Enum');
      for Pair in TNPC_ASTTypeEnum(Typ).Members do begin
        Indent(Level+1);
        tf.WriteLine(Pair.Key + ' = ' + Pair.Value.ToString);
      end;
    end
    else if Typ is TNPC_ASTTypeArray then begin
      Indent(Level);
      tf.WriteLine('ArrayOf');
      PrintType(TNPC_ASTTypeArray(Typ).ElementType, Level+1);
    end
    else if Typ is TNPC_ASTTypeRecord then begin
      Indent(Level);
      tf.WriteLine('Record');
      for Field in TNPC_ASTTypeRecord(Typ).Fields do begin
        Indent(Level+1);
        tf.WriteLine(Field.Key + ':');
        PrintType(Field.Value, Level+2);
      end;
    end
    else
      Assert(False);
  end;

  procedure PrintExpr(const Expr: TNPC_ASTExpression; Level: Integer = 0);
  var
    Elem: TNPC_ASTExpression;
    Branch: TNPC_ASTCaseBranch;
  begin
    if Expr is TNPC_ASTExpressionLiteral then begin
      Indent(Level);
      tf.WriteLine('Literal(' + TNPC_ASTExpressionLiteral(Expr).Value + ':' + LiteralTypeToString(TNPC_ASTExpressionLiteral(Expr).LiteralType) + ')');
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
    end;
  end;

  procedure PrintStmt(Stmt: TNPC_ASTStatement; Level: Integer = 0);
  var
    i: Integer;
  begin
    if Stmt is TNPC_ASTStatementBlock then begin
      Indent(Level);
      tf.WriteLine('Block');
      for i := 0 to TNPC_ASTStatementBlock(Stmt).Stmts.Count-1 do
        PrintStmt(TNPC_ASTStatementBlock(Stmt).Stmts[i], Level+1);
    end
    else if Stmt is TNPC_ASTStatementTypeDecl then begin
      Indent(Level);
      tf.WriteLine('TypeDecl(' + TNPC_ASTStatementTypeDecl(Stmt).Name + ')');
      PrintType(TNPC_ASTStatementTypeDecl(Stmt).DeclaredType, Level+1);
    end
    else if Stmt is TNPC_ASTStatementVarDecl then begin
      //Indent(Level);
      //Writeln('VarDecl(', TStmtVar(Stmt).Name, ':', TStmtVar(Stmt).TypeName, ')');
      Indent(Level);
      tf.WriteLine('VarDecl(' + TNPC_ASTStatementVarDecl(Stmt).Name + ')');
      PrintType(TNPC_ASTStatementVarDecl(Stmt).DeclaredType, Level+1);
      if Assigned(TNPC_ASTStatementVarDecl(Stmt).Init) then
        PrintExpr(TNPC_ASTStatementVarDecl(Stmt).Init, Level+1);
    end
    else if Stmt is TNPC_ASTStatementAssign then begin
      Indent(Level);
      tf.WriteLine('Assign(' + TypeToName(TNPC_ASTStatementAssign(Stmt).Target) + ')');
      PrintExpr(TNPC_ASTStatementAssign(Stmt).Value, Level+1);
    end
    else if Stmt is TNPC_ASTStatementIf then begin
      Indent(Level);
      tf.WriteLine('If');
      PrintExpr(TNPC_ASTStatementIf(Stmt).Cond, Level+1);
      Indent(Level);
      tf.WriteLine('Then');
      PrintStmt(TNPC_ASTStatementIf(Stmt).ThenStmt, Level+1);
      if Assigned(TNPC_ASTStatementIf(Stmt).ElseStmt) then begin
        Indent(Level);
        tf.WriteLine('Else');
        PrintStmt(TNPC_ASTStatementIf(Stmt).ElseStmt, Level+1);
      end;
    end
    else if Stmt is TNPC_ASTStatementCase then begin
      Indent(Level);
      tf.WriteLine('Case<not-implemented>');
    end
    else if Stmt is TNPC_ASTStatementWhile then begin
      Indent(Level);
      tf.WriteLine('While');
      PrintExpr(TNPC_ASTStatementWhile(Stmt).Cond, Level+1);
      PrintStmt(TNPC_ASTStatementWhile(Stmt).Body, Level+1);
    end
    else if Stmt is TNPC_ASTStatementFor then begin
      Indent(Level);
      if TNPC_ASTStatementFor(Stmt).Reverse then
        tf.WriteLine('For ' + TNPC_ASTStatementFor(Stmt).VarName + ' downto')
      else
        tf.WriteLine('For ' + TNPC_ASTStatementFor(Stmt).VarName + ' to');
      Indent(Level+1);
      tf.WriteLine('Init');
      PrintExpr(TNPC_ASTStatementFor(Stmt).InitExpr, Level+2);
      Indent(Level+1);
      tf.WriteLine('End');
      PrintExpr(TNPC_ASTStatementFor(Stmt).EndExpr, Level+2);
      Indent(Level+1);
      tf.WriteLine('Body');
      PrintStmt(TNPC_ASTStatementFor(Stmt).Body, Level+2);
    end
    else if Stmt is TNPC_ASTStatementWhile then begin
      Indent(Level);
      tf.WriteLine('Cond');
      PrintExpr(TNPC_ASTStatementWhile(Stmt).Cond, Level+1);
      Indent(Level);
      tf.WriteLine('Body');
      PrintStmt(TNPC_ASTStatementWhile(Stmt).Body, Level+1);
    end;
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
      PrintStmt(TNPC_ASTStatement(AST));
    finally
      tf.Free;
    end;
  except
  end;
end;

end.

