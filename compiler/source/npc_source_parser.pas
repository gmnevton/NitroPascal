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
  private
    Compiler: TNPCCompiler;
    ParentParser: TNPCSourceParser;
    Tokenizer: TNPCTokenizer;
    Texer: TNPCTokensParser;
    //
    SettingsPtr: Pointer;
    ASTree: TNPC_AST;
    ScopeStack: TObjectList<TNPCScope>;
    Imports: TNPCImportArray;
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
    procedure EnterScope; //inline;
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
    function  ParseExpression(const AToken: TNPCToken; Prec: Integer = 0): TNPC_ASTExpression;
    function  ParsePrimaryExpression(const AToken: TNPCToken): TNPC_ASTExpression;
    function  ParseSimpleExpression(const AToken: TNPCToken; const Left: TNPC_ASTExpression): TNPC_ASTExpression;

    function  ParseType: TNPC_ASTTypeExpression;
    function  ParseTypeDeclaration(const AFlags: TNPCParseDeclarationFlags): TNPC_ASTStatementTypeDecl;
    function  ParseVariableDeclaration: TNPC_ASTStatementVarDecl;

    function  ParseBlock: TNPC_ASTStatementBlock;
    function  ParseAssignment(const AToken: TNPCToken): TNPC_ASTExpression;
    function  ParseIdentifier: TNPC_ASTExpression;
    function  ParseLiteral: TNPC_ASTExpression;
    function  ParseIf: TNPC_ASTStatementIf;
    function  ParseCase: TNPC_ASTStatementCase;
    function  ParseFor: TNPC_ASTStatement;
    procedure ParseForParams;
    function  ParseWhile: TNPC_ASTStatementWhile;
    procedure ParseCall(const AToken: TNPCToken);
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
end;

destructor TNPCSourceParser.Destroy;
begin
  Clear;
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

//  if Sym.Typ <> Nil then
//    Result := Sym.Typ.Name
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
  case AToken.&Type of
    tokOr: Result := 1;
    tkXor: Result := 2;
    tkAnd: Result := 3;
    tkEq,
    tkNeq,
    tkLT,
    tkLTE,
    tkGT,
    tkGTE: Result := 4;
    tkPlus,
    tkMinus: Result := 5;
    tkStar,
    tkSlash,
    tkPercent,
    tkMod,
    tkShl,
    tkShr: Result := 6;
    tkAssign: Result := 0; // assignment handled as statement-level
    tkDot: Result := 9; // member access very high
    tkLBracket: Result := 9; // indexing high
  else
    Result := 0;
  end;
end;

procedure TNPCSourceParser.EnterScope;
begin
  FScopeStack.Add(TScope.Create(16));
end;

procedure TNPCSourceParser.LeaveScope;
var
  s: TScope;
begin
  if FScopeStack.Count = 0 then
    Exit;
  //
  s := FScopeStack.Last;
  FScopeStack.Delete(FScopeStack.Count - 1);
  s.Free;
end;

procedure TNPCSourceParser.InitBuiltins(const AScope: TNPCScope);
begin
  // register built-in types
  BuiltinIntegerType := TTypeName.Create;
  TTypeName(BuiltinIntegerType).Name := 'Integer';
  CurrentScope.DefineType('Integer', BuiltinIntegerType);

  BuiltinRealType := TTypeName.Create;
  TTypeName(BuiltinRealType).Name := 'Real';
  CurrentScope.DefineType('Real', BuiltinRealType);

  BuiltinBooleanType := TTypeName.Create;
  TTypeName(BuiltinBooleanType).Name := 'Boolean';
  CurrentScope.DefineType('Boolean', BuiltinBooleanType);

//  Scope.DefineType('Integer', TTypeName.Create('Integer'));
//  Scope.DefineType('Real', TTypeName.Create('Real'));
//  Scope.DefineType('Boolean', TTypeName.Create('Boolean'));

//var
//  intSym, boolSym, strSym: TSymbol;
//  globalScope: TScope; // adjust to your scope variable name
//begin
//  // assume you have a top-level scope created already (FScopeStack or similar)
//  // create the type symbols
//  intSym := TSymbol.Create('Integer', skType, nil);
//  boolSym := TSymbol.Create('Boolean', skType, nil);
//  strSym := TSymbol.Create('String', skType, nil);
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
//  FIndex := 0;
//  if ASTree <> Nil then
//    FreeAndNil(ASTree);
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
//        auto t = pt;
//        //printf("isCDeclaration() %s\n", t.toChars());
//        if (!isDeclarationSpecifiers(t))
//            return false;
//
//        while (1)
//        {
//            if (t.value == TOK.semicolon)
//            {
//                t = peek(t);
//                pt = t;
//                return true;
//            }
//            if (!isCDeclarator(t, DTR.xdirect))
//                return false;
//            if (t.value == TOK.asm_)
//            {
//                t = peek(t);
//                if (t.value != TOK.leftParenthesis || !skipParens(t, &t))
//                    return false;
//            }
//            if (t.value == TOK.__attribute__)
//            {
//                t = peek(t);
//                if (t.value != TOK.leftParenthesis || !skipParens(t, &t))
//                    return false;
//            }
//            if (t.value == TOK.assign)
//            {
//                t = peek(t);
//                if (!isInitializer(t))
//                    return false;
//            }
//            switch (t.value)
//            {
//                case TOK.comma:
//                    t = peek(t);
//                    break;
//
//                case TOK.semicolon:
//                    t = peek(t);
//                    pt = t;
//                    return true;
//
//                default:
//                    return false;
//            }
//        }
end;

procedure TNPCSourceParser.AddStatement(const AStmt: TNPC_ASTStatement);
begin
  if Assigned(CurrentBlock) then
    CurrentBlock.Statements.Add(S)
  else
    raise Exception.Create('No current block to add statement to');
end;

procedure TNPCSourceParser.DeclareSymbol(const AName: String; const ASym: TNPCSymbol);
var
  scope: TScope;
begin
  if FScopeStack.Count = 0 then
    Exit;
  //
  scope := FScopeStack.Last;
  // allow shadowing: put symbol in current scope
  scope.AddOrSetValue(Name, Sym);
end;

function TNPCSourceParser.LookupSymbol(const AName: String): TNPCSymbol;
var
  i: Integer;
  scope: TScope;
  sym: TSymbol;
begin
  Result := Nil;
  for i := FScopeStack.Count - 1 downto 0 do begin
    scope := FScopeStack[i];
    if scope.TryGetValue(Name, sym) then
      Exit(sym);
  end;
end;

function TNPCSourceParser.Parse(const ASource: TStringStream; const ASourceFile: String; const ParsingImport: Boolean = False): Boolean;
var
  token: TNPCToken;
//  idx: Integer;
begin
  Result := False;
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
        token := Texer.ExpectToken([tokIdent]);
        if not TokenIsReservedIdent(token, ri_project) then begin
          token.Free;
          raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedType, [token.TokenToString, NPCReservedIdentifiers[ri_project].Ident]));
        end;
        //
        // ok we are inside project file
        //
//        AddToken(token);
//        ASTree := TNPC_AST.Create;
//        TNPC_AST(ASTree).Location := token.Location.Copy;
//        TNPC_AST(ASTree).Flags := LongWord(BLOCK_ConsistsOfOrderedStatements);

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
//        ASTree := TNPC_AST.Create;
//        TNPC_AST(ASTree).Location := token.Location.Copy;
//        TNPC_AST(ASTree).Flags := LongWord(BLOCK_ConsistsOfOrderedStatements);

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
  name: string;
  target: TExpr;
  rhs: TExpr;
  expr: TExpr;
  block: TBlockStmt;
begin
  case FToken.Kind of
    tkIdentifier: begin
      // might be assignment
      Result := ParseAssignment;
    end;
    tkType: begin
      Result := ParseTypeDecl;
      Exit;
    end;
    tkVar: begin
      Result := ParseVarDecl;
      Exit;
    end;
    tkBegin: begin
      Result := ParseBlock;
      Exit;
    end;
    tkIf: begin
      Result := ParseIf;
      Exit;
    end;
    tkWhile: begin
      Result := ParseWhile;
      Exit;
    end;
    tkFor: begin
      Result := ParseFor;
      Exit;
    end;
    else
      begin
        // expression or assignment statement
        expr := ParseExpression(0);
        // if next token is assign ':=' -> assignment
        if (FToken.Kind = tkAssign) then
        begin
          // left must be an lvalue; we keep it as expression for now
          target := expr;
          Advance; // consume :=
          rhs := ParseExpression(0);
          Eat(tkSemicolon);
          Result := TAssignStmt.Create(target, rhs);
          Exit;
        end
        else
        begin
          // expression statement
          Eat(tkSemicolon);
          Result := TExprStmt.Create(expr);
          Exit;
        end;
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

function TNPCSourceParser.ParseExpression(const AToken: TNPCToken; Prec: Integer = 0): TNPC_ASTExpression;
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
  left: TExpr;
  curPrec: Integer;
  opTok: TToken;
begin
  left := ParsePrimaryExpression; // nud
  try
    while True do begin
      curPrec := GetPrecedence(FToken);
      if (curPrec = 0) or (curPrec <= Prec) then
        Break;
      opTok := FToken;
      Advance;
      left := ParseSimpleExpression(left, opTok); // led
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
  t: TToken;
  inner: TExpr;
begin
  t := FToken;
  case t.Kind of
    tkInteger: begin
      Result := ParseLiteral;
    end;
    tkIdentifier: begin
      Advance;
      // an identifier may be followed by '(' (call) or '[' (index) and will be handled in infix
      Result := TIdentExpr.Create(t.Text);
    end;
    tkNumber: begin
      Advance;
      Result := TNumberExpr.Create(t.Text);
    end;
    tkString: begin
      Advance;
      Result := TStringExpr.Create(t.Text);
    end;
    tkMinus: begin
      Advance;
      inner := ParseExpression(GetPrecedence(t)); // unary - bind tighter than additive
      Result := TUnaryExpr.Create('-', inner);
    end;
    tkPlus: begin
      Advance;
      expr := ParseExpression(GetPrecedence(t));
      Result := TUnaryExpr.Create('+', expr);
    end;
    tkNot: begin
      Advance;
      expr := ParseExpression(7);
      Result := TUnaryExpr.Create('not', expr);
    end;
    tkLParen: begin
      Advance;
      inner := ParseExpression(0);
      ExpectKind(tkRParen);
      Result := inner;
    end
    tkLBracket: begin
      Advance;
      var Elements := TList<TExpr>.Create;
      try
        if Current.Kind <> tkRBracket then
        repeat
          Elements.Add(ParseExpression);
        until not Match(tkComma);
        Expect(tkRBracket);

        var SetType: TTypeSet := nil;
        var ElemType: TTypeExpr := nil;

        if Elements.Count > 0 then
        begin
          // infer element type from first element
          for var E in Elements do
          begin
            var ThisType: TTypeExpr := nil;

            if E is TExprEnumConst then
              ThisType := TExprEnumConst(E).EnumType
            else if E is TExprLiteral then
              ThisType := TExprLiteral(E).LiteralType
            else if E is TExprVariable then
            begin
              var Sym := CurrentScope.Resolve(TExprVariable(E).Name);
              if not Assigned(Sym) then
                raise Exception.Create('Unknown identifier: ' + TExprVariable(E).Name);
              ThisType := Sym.TypeRef;
            end;

            if not Assigned(ThisType) then
              raise Exception.Create('Unsupported element in set literal');

            if not Assigned(ElemType) then
              ElemType := ThisType
            else if ElemType <> ThisType then
              raise Exception.CreateFmt('Type mismatch in set literal: expected %s, found %s',
                [ElemType.Name, ThisType.Name]);
          end;

          SetType := TTypeSet.Create;
          SetType.ElementType := ElemType;
        end;

        // For empty set, SetType stays nil (to be filled later from context)
        var SetLit := TExprSetLiteral.Create(SetType);
        for var E in Elements do
          SetLit.Elements.Add(E);
        Result := SetLit;
      finally
        Elements.Free;
      end;
    end;
  else
    raise Exception.CreateFmt('Unexpected token in prefix: %s', [t.Text]);
  end;
end;

function TNPCSourceParser.ParseSimpleExpression(const AToken: TNPCToken; const Left: TNPC_ASTExpression): TNPC_ASTExpression;
var
  opTxt: string;
  right: TExpr;
  callNode: TCallExpr;
  idx: TIndexExpr;
begin
  opTxt := OpTok.Text;
  case OpTok.Kind of
    tkPlus, tkMinus, tkStar, tkSlash, tkPercent, tkAnd, tkOr, tkXor, tkShl, tkShr, tkEq, tkNeq, tkLT, tkLTE, tkGT, tkGTE, tkMod: begin
      right := ParseExpression(GetPrecedence(OpTok));
      Result := TBinaryExpr.Create(Left, opText, right);
    end;
    tkDot: begin
//        // member access: expect identifier
//        if FToken.Kind <> tkIdentifier then
//          raise Exception.Create('Expected member identifier after .');
//        memberName := FToken.Text;
//        Advance;
//        Result := TMemberExpr.Create(Left, memberName);

      Advance;
      var Field := ExpectIdentifier;

      if not (Left is TExprVariable) then
        raise Exception.Create('Field access must be on a variable');

      var VarSym := CurrentScope.Resolve(TExprVariable(Left).Name);
      if not Assigned(VarSym) or not (VarSym.TypeRef is TTypeRecord) then
        raise Exception.Create('Variable is not a record: ' + TExprVariable(Left).Name);

      var RecType := TTypeRecord(VarSym.TypeRef);
      if not RecType.Fields.ContainsKey(Field) then
        raise Exception.CreateFmt('Unknown field "%s" in record "%s"', [Field, TExprVariable(Left).Name]);

      var FieldType := RecType.Fields[Field];
      Left := TExprRecordField.Create(Left, Field, FieldType);
      Result := Left;
    end;
    tkLParen: begin
      // call: Left is callee
      callNode := TCallExpr.Create(Left);
      if not TokenIs(tkRParen) then
      begin
        while not TokenIs(tkRParen) do
        begin
          callNode.Args.Add(ParseExpression(0));
          if TokenIs(tkComma) then Advance else Break;
        end;
      end;
      ExpectKind(tkRParen);
      Result := callNode;
    end;
    tkLBracket: begin
//        // indexing: [expr]
//        Advance;
//        idx := TIndexExpr.Create(Left, ParseExpression(0));
//        ExpectKind(tkRBracket);
//        Result := idx;

      Advance;
      var IndexExpr := ParseExpression;
      Expect(tkRBracket);

      if not (Left is TExprVariable) then
        raise Exception.Create('Array access must be on a variable');

      var VarSym := CurrentScope.Resolve(TExprVariable(Left).Name);
      if not Assigned(VarSym) or not (VarSym.TypeRef is TTypeArray) then
        raise Exception.Create('Variable is not an array: ' + TExprVariable(Left).Name);

      var ArrType := TTypeArray(VarSym.TypeRef);
      Left := TExprArrayAccess.Create(Left, IndexExpr, ArrType.ElementType);
      Result := Left;
    end;
    tkIn: begin
      var RightExpr := ParseExpression;

      if not (RightExpr is TExprSetLiteral) then
        raise Exception.Create('Right side of "in" must be a set');

      var SetLit := TExprSetLiteral(RightExpr);
      var SetType := SetLit.SetType;

      // Determine type of left operand
      var LeftType: TTypeExpr := nil;
      if Left is TExprVariable then begin
        var Sym := CurrentScope.Resolve(TExprVariable(Left).Name);
        if Assigned(Sym) then
          LeftType := Sym.TypeRef;
      end
      else if Left is TExprEnumConst then
        LeftType := TExprEnumConst(Left).EnumType
      else if Left is TExprLiteral then
        LeftType := TExprLiteral(Left).LiteralType;

      if not Assigned(LeftType) then
        raise Exception.Create('Cannot determine type of left operand in "in" expression');

      // If RHS is empty set -> infer type from LHS
      if (not Assigned(SetType)) then begin
        SetType := TTypeSet.Create;
        SetType.ElementType := LeftType;
        SetLit.SetType := SetType;
      end
      else if SetType.ElementType <> LeftType then
        raise Exception.CreateFmt(
          'Type mismatch in "in": left operand is %s, but set element type is %s',
          [LeftType.Name, SetType.ElementType.Name]);

      Left := TExprInOp.Create(Left, RightExpr);
      Result := Left;
    end;
  else
    raise Exception.CreateFmt('Unknown infix %s', [OpTok.Text]);
  end;
end;

function TNPCSourceParser.ParseType: TNPC_ASTTypeExpression;
begin
  if Current.Kind = tkIdentifier then
  begin
    // Simple type name
    Result := TTypeName.Create(Current.Lexeme);
    Advance;
  end
  else if Match(tkLParen) then
  begin
    var Enum := TTypeEnum.Create;
    var Value := 0;
    repeat
      var Ident := ExpectIdentifier;

      // support explicit values: (Red=10, Green, Blue=30)
      if Match(tkAssign) then
      begin
        // reuse expression parser for assigned value, but expect integer literal only
        var Lit := ParseExpression;
        if not (Lit is TExprLiteral) then
          raise Exception.Create('Enum value must be integer literal');
        Value := StrToInt(TExprLiteral(Lit).Value);
      end;

      Enum.Members.Add(Ident, Value);

      // add enum member to current scope as constant
      CurrentScope.DefineConst(Ident, Value, Enum);

      Inc(Value);
    until not Match(tkComma);
    Expect(tkRParen);
    Result := Enum;
  end
  else if Match(tkArray) then
  begin
//    Expect(tkOf);
//    Result := TTypeArray.Create(ParseType);
    Expect(tkLBracket);
    var IndexType := ParseType;
    Expect(tkRBracket);
    Expect(tkOf);
    var ElemType := ParseType;
    var Arr := TTypeArray.Create;
    Arr.IndexType := IndexType;
    Arr.ElementType := ElemType;
    Result := Arr;
  end
  else if Match(tkRecord) then
  begin
//    var Rec := TTypeRecord.Create;
//    while not Match(tkEnd) do
//    begin
//      var FieldName := ExpectIdentifier;
//      Expect(tkColon);
//      Rec.Fields.Add(FieldName, ParseType);
//      Match(tkSemicolon); // optional
//    end;
//    Result := Rec;

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

    recDecl := TRecordTypeDecl.Create(typeName);
    while not Check(tkEnd) do
    begin
      fieldName := Consume(tkIdentifier, 'Expected field name').Lexeme;
      Consume(tkColon, 'Expected ":" after field name');
      fieldTyp := ResolveType(Consume(tkIdentifier, 'Expected type').Lexeme);
      recDecl.Fields.Add(TRecordField.Create);
      recDecl.Fields.Last.Name := fieldName;
      recDecl.Fields.Last.Typ := fieldTyp;
      Match(tkSemicolon); // optional semicolon
    end;
    Consume(tkEnd, 'Expected "end" to close record');
    Consume(tkSemicolon, 'Expected ";" after record type');
    // Add symbol to current scope
    CurrentScope.Define(typeName, recDecl);
    //Exit(recDecl);
    Result := Rec;
  end
  else if Match(tkSet) then
  begin
    Expect(tkOf);
    var ElemType := ParseType;
    var S := TTypeSet.Create;
    S.ElementType := ElemType;
    Result := S;
  end
  else
    raise Exception.Create('Unexpected type syntax');
end;

function TNPCSourceParser.ParseTypeDeclaration(const AFlags: TNPCParseDeclarationFlags): TNPC_ASTStatementTypeDecl;
//begin
//  Result := Nil;
//  if Texer.IsEmpty then
//    Exit;
//
//end;
//var
//  Name: string;
//  Typ: TTypeExpr;
//begin
////  Consume(tkType, 'Expected "type" keyword');
////  typeName := Consume(tkIdentifier, 'Expected type name').Lexeme;
////  Consume(tkEq, 'Expected "=" after type name');
//
//  Name := ExpectIdentifier;
//  Expect(tkAssign);
//  Typ := ParseType;
//  Match(tkSemicolon);
//  Result := TStmtTypeDecl.Create(Name, Typ);
//end;
var
  typeName: string;
  typeSym, fieldSym: TSymbol;
  fldName, fldTypeName: string;
begin
  // assume current token at 'type'
  ExpectKind(tkType);
  if FToken.Kind <> tkIdentifier then
    raise Exception.Create('Expected type name after "type"');
  typeName := FToken.Text; Advance;

  ExpectKind(tkEq); // '='
  ExpectKind(tkRecord); // 'record'

  // create the type symbol and register it early (enables recursive types)
  typeSym := TSymbol.Create(typeName, skType, nil);
  DeclareSymbol(typeName, typeSym); // add to current scope before processing fields

  // allocate the Fields dictionary
  typeSym.Fields := TDictionary<string, TSymbol>.Create;

  // parse fields: loop until 'end'
  while FToken.Kind <> tkEnd do
  begin
    // simple grammar: field1, field2: TypeName; ...
    // parse identifiers separated by commas
    while True do
    begin
      if FToken.Kind <> tkIdentifier then
        raise Exception.Create('Expected field name in record');
      fldName := FToken.Text; Advance;
      // expect ':' then type name
      ExpectKind(tkColon);
      if FToken.Kind <> tkIdentifier then
        raise Exception.Create('Expected type name for field');
      fldTypeName := FToken.Text; Advance;

      // resolve field type symbol
      fieldSym := LookupSymbol(fldTypeName);
      if fieldSym = nil then
        raise Exception.CreateFmt('Unknown type "%s" for field %s', [fldTypeName, fldName]);

      // create a symbol for the field: kind = skVar, Typ = fieldType
      fieldSym := TSymbol.Create(fldName, skVar, fieldSym);
      typeSym.Fields.AddOrSetValue(fldName, fieldSym);

      // comma or semicolon?
      if FToken.Kind = tkComma then Advance
      else Break;
    end;
    ExpectKind(tkSemicolon);
  end;

  ExpectKind(tkEnd); // 'end'
  ExpectKind(tkSemicolon); // final ';' after type

  // Optionally return an AST node representing the type decl; otherwise just registered the symbol
end;

function TNPCSourceParser.ParseVariableDeclaration: TNPC_ASTStatementVarDecl;
var
  name: string;
  Typ: TTypeExpr;
  initExpr: TExpr;
  sym: TSymbol;
begin
  // current token is 'var'
  ExpectKind(tkVar);
  if FToken.Kind <> tkIdentifier then
    raise Exception.Create('Expected identifier after var');
  name := FToken.Text;
  Advance;
  Expect(tkColon);
  Typ := ParseType;
  initExpr := nil;
  if TokenIs(tkAssign) then
  begin
    Advance;
    initExpr := ParseExpression(0);
  end;
  ExpectKind(tkSemicolon);
//  Result := TStmtVar.Create(Name, Typ, Init);
  Result := TVarDeclStmt.Create(name, Typ, initExpr);
  // declare symbol immediately in current scope
  sym := TSymbol.Create(name, skVar, Result);
  Result.SymbolRef := sym;
  DeclareSymbol(name, sym);
end;

function TNPCSourceParser.ParseBlock: TNPC_ASTStatementBlock;
begin
  ExpectKind(tkBegin);
  EnterScope; // new lexical scope for this block
  Result := TBlockStmt.Create;
  while not TokenIs(tkEnd) do
  begin
    if TokenIs(tkEOF) then
      raise Exception.Create('Unexpected EOF inside block');
//    var Stmt := ParseStatement;
//    if Assigned(Stmt) then
//      AddStatement(Stmt);
    Result.Stmts.Add(ParseStatement);
  end;
  ExpectKind(tkEnd);
  LeaveScope;
end;

// assignment = ident [param_list] ":=" expr
//
// param_list = ('(' | '[') ident { ',' ident } (')' | ']')

function TNPCSourceParser.ParseAssignment(const AToken: TNPCToken): TNPC_ASTExpression;
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
  Right: TExpr;
  Sym: TSymbol;
begin
  Expect(tkAssign);
  Right := ParseExpression;

  // Ensure Left is variable
  if not (Left is TExprVariable) then
    raise Exception.Create('Left side of assignment must be a variable');

  Sym := CurrentScope.Resolve(TExprVariable(Left).Name);
  if not Assigned(Sym) then
    raise Exception.Create('Unknown variable: ' + TExprVariable(Left).Name);

  // Special handling for empty set
  if (Right is TExprSetLiteral) and (TExprSetLiteral(Right).SetType = nil) then
  begin
    if not (Sym.TypeRef is TTypeSet) then
      raise Exception.Create('Empty set assignment requires a set variable');
    TExprSetLiteral(Right).SetType := TTypeSet(Sym.TypeRef);
  end;

  AddStatement(TStmtAssign.Create(Left, Right));
end;

function TNPCSourceParser.ParseIdentifier: TNPC_ASTExpression;
var
  Name: String;
  Sym: TSymbol;
begin
  Name := Current.Lexeme;
  Advance;

  if SameText(Name, 'ord') then begin
    Expect(tkLParen);
    var Arg := ParseExpression;
    Expect(tkRParen);

    if Arg is TExprEnumConst then
      Result := TExprLiteral.Create(IntToStr(TExprEnumConst(Arg).Value), BuiltinIntegerType)
    else
      raise Exception.Create('ord() expects enum constant');
  end
  else if SameText(Name, 'succ') then begin
    Expect(tkLParen);
    var Arg := ParseExpression;
    Expect(tkRParen);

    if Arg is TExprEnumConst then begin
      var V := TExprEnumConst(Arg).Value + 1;
      Result := TExprEnumConst.Create('<succ>', V, TExprEnumConst(Arg).EnumType);
    end
    else
      raise Exception.Create('succ() expects enum constant');
  end
  else if SameText(Name, 'pred') then begin
    Expect(tkLParen);
    var Arg := ParseExpression;
    Expect(tkRParen);

    if Arg is TExprEnumConst then begin
      var V := TExprEnumConst(Arg).Value - 1;
      Result := TExprEnumConst.Create('<pred>', V, TExprEnumConst(Arg).EnumType);
    end
    else
      raise Exception.Create('pred() expects enum constant');
  end
  else begin
//    Result := inheritedParseIdentifier(Name); // your old handling
    Sym := CurrentScope.Resolve(Name);
    if Assigned(Sym) then begin
      case Sym.Kind of
        skConst: Result := TExprEnumConst.Create(Sym.Name, Sym.ConstValue, Sym.TypeRef);
        skVar  : Result := TExprVariable.Create(Sym.Name, Sym.TypeRef);
      else
        raise Exception.Create('Identifier not valid in expression: ' + Sym.Name);
      end;
    end
    else
      raise Exception.Create('Unknown identifier: ' + Name);
    Advance;
  end;
end;

function TNPCSourceParser.ParseLiteral: TNPC_ASTExpression;
begin
  // assume integer literals for now
  Result := TExprLiteral.Create(Current.Lexeme, BuiltinIntegerType);
  Advance;
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

function TNPCSourceParser.ParseIf: TNPC_ASTStatementIf;
//var
//  AToken: TNPCToken;
//  token: TNPCToken;
//  cond: TNPC_ASTExpression;
//  then_block: TNPC_ASTStatement;
//  else_block: TNPC_ASTStatement;
//begin
//  Result := Nil;
//
//  AToken := Texer.PeekToken;
//
//  Texer.SkipToken; // skip 'if' keyword
//  if Texer.IsEmpty then
//    Exit;
//
//  token := Texer.PeekToken;
//  cond := ParseExpression(token); // get everything until 'then'
//  Texer.ExpectReservedToken(ri_then);
//  then_block := ParseStatement([stafStartNewScope, stafEmptyStatementIsAcceptable]);
//  //
//  SkipComments;
//  token := Texer.PeekToken;
//  if TokenIsReservedIdent(token, ri_else) then begin
//    if (Texer.LastToken <> Nil) and TokenIsReservedSymbol(Texer.LastToken, rs_Semicolon) then
//      raise NPCSyntaxError.ParserError(Texer.LastToken.Location, Format(sParserUnexpectedTokenIn, [Texer.LastToken.TokenToString, 'if ', sStatement]));
////    AddToken(token);
//    Texer.SkipToken;
//    SkipComments;
//    token := Texer.PeekToken;
//    if TokenIsReservedIdent(token, ri_if) then begin
//      Result := ParseIf;
//      Exit;
//    end;
//    else_block := ParseStatement([stafStartNewScope]);
//  end;
//  if (Texer.LastToken <> Nil) and not TokenIsReservedSymbol(Texer.LastToken, rs_Semicolon) then
////    raise NPCSyntaxError.ParserError(LastToken.Location, Format(sParserUnexpectedTokenIn, [LastToken.TokenToString, 'if ', sStatement]));
//    raise NPCSyntaxError.ParserError(Texer.LastToken.Location.After, Format(sParserExpectedButGot, [NPCReservedSymbolToString(rs_Semicolon), token.TokenToString]));
//  Texer.ExpectToken([tokSemicolon]);
//  //
//  Result := TNPC_ASTStatementIf.Create(AToken.Location, cond, then_block, else_block);
////  Result.Block := ABlock;
//end;
var
  cond: TExpr;
  thenS, elseS: TStmt;
begin
  ExpectKind(tkIf);
  cond := ParseExpression(0);
  ExpectKind(tkThen);
  thenS := ParseStatement;
  elseS := nil;
  if TokenIs(tkElse) then
  begin
    Advance;
    elseS := ParseStatement;
  end;
  Result := TIfStmt.Create(cond, thenS, elseS);
end;

// case         = 'case' expression 'of' case_element { ';' [ 'else' $NOREAD | 'end' $NOREAD | case_element ] } [ 'else' instruction_list ] 'end' [ ';' ] .
//
// case_element = case_label ':' instruction .
//
// case_label   = constant_expression { ( ',' constant_expression | '..' constant_expression ) } .


// CaseStatement = [ 'case' Expression 'of' CaseElement ';' { CaseElement ';' } [ 'else' StatementList ';' ] 'end' |
//                   'case' Expression '{'  CaseElement ';' { CaseElement ';' } [ 'else' StatementList ';' ] '}' ] .
//
// CaseElement = 'if' CaseLabel { ',' CaseLabel } ':' [ '{@next' [ ':' CaseElement ] '}' ] Statement .
//
// CaseLabel = ConstExpression [ '..' ConstExpression ] .
//
// ConstExpression = Expression .

function TNPCSourceParser.ParseCase: TNPC_ASTStatementCase;
var
  AToken: TNPCToken;
  token: TNPCToken;
  case_of: Boolean;
begin
  AToken := Texer.PeekToken;
//  AddToken(AToken); // 'case'
  Texer.SkipToken;
//  ParseCaseExpression; // get everything until 'of' or '{'
  token := Texer.GetToken;
  case_of := TokenIsReservedIdent(token, ri_of); // and not TokenIsReservedSymbol(token, rs_OCurly);
  if not TokenIsReservedIdent(token, ri_of) and not TokenIsReservedSymbol(token, rs_OCurly) then
    raise NPCSyntaxError.ParserError(Texer.LastToken.Location, Format(sParserUnexpectedTokenIn, [Texer.LastToken.TokenToString, 'case ', sStatement]));
//  AddToken(token);
//  ParseCaseElements(case_of);
  //
//  SkipComments;
//    if (LastToken <> Nil) and TokenIsReservedSymbol(LastToken, rs_Semicolon) then
//  token := LastToken;
  token := Texer.GetToken;
  if (case_of and not TokenIsReservedIdent(token, ri_end)) or (not case_of and not TokenIsReservedSymbol(token, rs_CCurly)) then
    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'case ', sStatement]));
//  AddToken(token);
  token := Texer.GetToken;
  if not TokenIsReservedSymbol(token, rs_Semicolon) then
//    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'case ', sStatement]));
    raise NPCSyntaxError.ParserError(Texer.LastToken.Location.After, Format(sParserExpectedButGot, [NPCReservedSymbolToString(rs_Semicolon), token.TokenToString]));
//  AddToken(Texer.ExpectToken([tokSemicolon]));
//  AddToken(token);
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

function TNPCSourceParser.ParseFor: TNPC_ASTStatement;
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
  startExpr, endExpr: TExpr;
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
  var cond: TExpr := TBinaryExpr.Create(TIdentExpr.Create(varName), '<=', endExpr);
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
  cond: TExpr;
  body: TStmt;
begin
  ExpectKind(tkWhile);
  cond := ParseExpression(0);
  ExpectKind(tkDo);
  body := ParseStatement;
  Result := TWhileStmt.Create(cond, body);
end;

procedure TNPCSourceParser.ParseCall(const AToken: TNPCToken);
begin

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

