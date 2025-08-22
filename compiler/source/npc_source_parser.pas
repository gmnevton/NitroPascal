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

  TNPCSourceParser = class
  private
    Compiler: TNPCCompiler;
    ParentParser: TNPCSourceParser;
    Tokenizer: TNPCTokenizer;
    Texer: TNPCTokensParser;
    //
    SettingsPtr: Pointer;
    ASTree: TNPC_ASTBlock;
    Imports: TNPCImportArray;
  private
    FLevel: Integer;
    //
    procedure AddProjectImport(const ASourceFile, ACodeName: String);
  protected
    ParsingType: TNPCParsingType;
    //
    function Unescape(const Value: String): String; inline;
    procedure IncLevel; inline;
    procedure DecLevel; inline;
    procedure Clear;
    //
    procedure AddImport(const AImportType: TNPCImportType; const AImportName: String; const AImportPath: String);
    function TokenIsOfType(const AToken: TNPCToken; const ATokenTypes: Array of TNPCTokenType): Boolean;
    function TokenIsIdent(const AToken: TNPCToken): Boolean;// inline;
    function TokenIsNumber(const AToken: TNPCToken): Boolean;// inline;
    function TokenIsString(const AToken: TNPCToken): Boolean;// inline;
    function TokenIsChar(const AToken: TNPCToken): Boolean;// inline;
    function TokenIsReserved(const AToken: TNPCToken): Boolean;// inline;
    function TokenIsReservedIdent(const AToken: TNPCToken): Boolean; overload;// inline;
    function TokenIsReservedIdent(const AToken: TNPCToken; const AReservedIdent: TNPCReservedIdents): Boolean; overload;// inline;
    function TokenIsReservedIdent(const AToken: TNPCToken; const AReservedIdents: Array of TNPCReservedIdents): Boolean; overload;
    function TokenIsLiteral(const AToken: TNPCToken): Boolean;// inline;
    function TokenIsBiLiteral(const AToken: TNPCToken): Boolean;// inline;
    function TokenIsReservedSymbol(const AToken: TNPCToken): Boolean; overload;// inline;
    function TokenIsReservedSymbol(const AToken: TNPCToken; const AReservedSymbol: TNPCReservedSymbols): Boolean; overload;// inline;
    function TokenIsReservedSymbol(const AToken: TNPCToken; const AReservedSymbols: Array of TNPCReservedSymbols): Boolean; overload;
    procedure SkipComments;// inline;
    function SkipComment(const AToken: TNPCToken; const ConsumeToken: Boolean = False): Boolean;// inline;
    function IsCodeFileNamedAs(const FileName: String; const CodeName: String): Boolean;
    function LookupTypeDefinition(const AToken: TNPCToken): TNPC_ASTTypeDefinition;
    function IsDeclaration(var AToken: TNPCToken): Boolean;
    //
    function Parse(const ASource: TStringStream; const ASourceFile: String; const ParsingImport: Boolean = False): Boolean;
    //
    procedure ParseProjectBody(const ABlock: TNPC_ASTBlock);
    procedure ParseCodeBody(const ABlock: TNPC_ASTBlock);
    procedure ParseSettingDefineCondition(const ABlock: TNPC_ASTBlock);
    procedure ParseSettingOrDefineDirective(const ABlock: TNPC_ASTBlock);
    procedure ParseSettings(const AToken: TNPCToken; const ASettingType: TNPCSettingType; const ABlock: TNPC_ASTBlock);
    procedure ParseSettingProgram(const AToken: TNPCToken; const ABlock: TNPC_ASTBlock);
    procedure ParseSettingProgramType(const AToken: TNPCToken; const ABlock: TNPC_ASTBlock);
    procedure ParseSearchPath(const AToken: TNPCToken; const ABlock: TNPC_ASTBlock);

    procedure ParseDefines(const AToken: TNPCToken; const ABlock: TNPC_ASTBlock);
    procedure ParseDefinition(const ABlock: TNPC_ASTBlock);
    procedure ParseDirectives(const ABlock: TNPC_ASTBlock);
    procedure ParseComment;

    function  ParseStatement(const AFlags: TNPCParseStatementFlags; const ABlock: TNPC_ASTBlock): TNPC_ASTStatement; // ; var AStatement: TNPC_ASTStatement): Boolean;
    function  ParseExpression(const AToken: TNPCToken): TNPC_ASTExpression;
    function  ParsePrimaryExpression(const AToken: TNPCToken): TNPC_ASTExpression;

    function  ParseAssignment(const AToken: TNPCToken): TNPC_ASTExpression;


//    procedure ParseExpressionSimpleExpression;
//    procedure ParseExpressionTerm;
//    procedure ParseExpressionFactor;

    procedure ParseCallParams(const AToken: TNPCToken);

    procedure ParseIf(const AToken: TNPCToken);
    procedure ParseIfExpression;
    procedure ParseIfSimpleExpression;
    procedure ParseIfTerm;
    procedure ParseIfFactor;
    procedure ParseIfStatement;
    procedure ParseIfStatementFuncParams(const AToken: TNPCToken);

    procedure ParseCase(const AToken: TNPCToken);
    procedure ParseCaseExpression;
    procedure ParseCaseSimpleExpression;
    procedure ParseCaseTerm;
    procedure ParseCaseFactor;
    procedure ParseCaseElements(const case_of: Boolean);
    procedure ParseCaseElseStatements;
    procedure ParseCaseIfExpression;
    procedure ParseCaseIfSimpleExpression;
    procedure ParseCaseIfTerm;
    procedure ParseCaseIfFactor;
    procedure ParseCaseIfStatement;
    procedure ParseCaseIfStatementFuncParams(const AToken: TNPCToken);

    procedure ParseFor(const AToken: TNPCToken);
    procedure ParseForParams;
    procedure ParseForStatements(const for_do: Boolean);
    procedure ParseForExpression;

    procedure SearchImportByCodeName(const AToken: TNPCToken; const SearchPath: String; const SearchFileName: String; const ADecl: TNPC_ASTDeclaration);
    procedure SearchImportByFileName(const AToken: TNPCToken; const SearchPath: String; const SearchFileName: String; const ADecl: TNPC_ASTDeclaration);
    procedure ParseImports(const ABlock: TNPC_ASTBlock);
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
    property AST: TNPC_ASTBlock read ASTree;
  end;

function NPC_CompileImport(const ASourceFileName: PChar; const ACompiler: TNPCCompiler; const AParentParser: TNPCSourceParser; var Block: TNPC_ASTBlock): Boolean; stdcall; overload;
function NPC_CompileImport(const ASourceStream: TStringStream; const ASourceFileName: PChar; const ACompiler: TNPCCompiler; const AParentParser: TNPCSourceParser; var Block: TNPC_ASTBlock): Boolean; stdcall; overload;

function NPC_CompileSource(const ASourceCode: PChar; const ACompiler: TNPCCompiler; const AParentParser: TNPCSourceParser; var Block: TNPC_ASTBlock): Boolean; stdcall; overload;
function NPC_CompileSource(const ASourceStream: TStringStream; const ACompiler: TNPCCompiler; const AParentParser: TNPCSourceParser; var Block: TNPC_ASTBlock): Boolean; stdcall; overload;

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

function NPC_CompileImport(const ASourceFileName: PChar; const ACompiler: TNPCCompiler; const AParentParser: TNPCSourceParser; var Block: TNPC_ASTBlock): Boolean;
var
  Source: TNPCSourceParser;
  idx: Integer;
begin
  Source := TNPCSourceParser.Create(ACompiler, AParentParser);
  try
    Result := Source.ParseImportFile(ASourceFileName);
    if Result then begin
      Block := Source.AST;
      if AParentParser <> Nil then begin
        AParentParser.Imports[High(AParentParser.Imports)].Resolved := True;
      end;
    end;
  finally
    Source.Free;
  end;
end;

function NPC_CompileImport(const ASourceStream: TStringStream; const ASourceFileName: PChar; const ACompiler: TNPCCompiler; const AParentParser: TNPCSourceParser; var Block: TNPC_ASTBlock): Boolean;
begin

end;

function NPC_CompileSource(const ASourceCode: PChar; const ACompiler: TNPCCompiler; const AParentParser: TNPCSourceParser; var Block: TNPC_ASTBlock): Boolean;
var
  Source: TNPCSourceParser;
begin
  Source := TNPCSourceParser.Create(ACompiler, AParentParser);
  try
    Result := Source.ParseSourceCode(ASourceCode);
  finally
    Source.Free;
  end;
end;

function NPC_CompileSource(const ASourceStream: TStringStream; const ACompiler: TNPCCompiler; const AParentParser: TNPCSourceParser; var Block: TNPC_ASTBlock): Boolean;
var
  Source: TNPCSourceParser;
begin
  Source := TNPCSourceParser.Create(ACompiler, AParentParser);
  try
    Result := Source.ParseSourceCode(ASourceStream, '');
  finally
    Source.Free;
  end;
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

procedure TNPCSourceParser.IncLevel;
begin
  Inc(FLevel);
end;

procedure TNPCSourceParser.DecLevel;
begin
  Dec(FLevel);
  if FLevel < 0 then
    FLevel := 0;
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

function TNPCSourceParser.SkipComment(const AToken: TNPCToken; const ConsumeToken: Boolean = False): Boolean;
var
  token: TNPCToken;
begin
  Result := False;
  if TokenIsReservedSymbol(Atoken, rs_CommentSL) then begin // single-line comment
    if ConsumeToken then
      Texer.SkipToken;
    Exit(True);
  end;
  if TokenIsReservedSymbol(Atoken, rs_CommentMLB) then begin // multi-line comment /. ... ./
    if ConsumeToken then
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
    if ConsumeToken then
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
    if ConsumeToken then
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

procedure TNPCSourceParser.ParseProjectBody(const ABlock: TNPC_ASTBlock);
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

procedure TNPCSourceParser.ParseCodeBody(const ABlock: TNPC_ASTBlock);
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

procedure TNPCSourceParser.ParseSettingDefineCondition(const ABlock: TNPC_ASTBlock);
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

procedure TNPCSourceParser.ParseSettingOrDefineDirective(const ABlock: TNPC_ASTBlock);
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

procedure TNPCSourceParser.ParseSettings(const AToken: TNPCToken; const ASettingType: TNPCSettingType; const ABlock: TNPC_ASTBlock);
begin
  case ASettingType of
    setProgram    : ParseSettingProgram(AToken, ABlock);
    setProgramType: ParseSettingProgramType(AToken, ABlock);
    setSearchPath : ParseSearchPath(AToken, ABlock);
    setDefines    : ParseDefines(AToken, ABlock);
    setResources  : ;
  end;
end;

procedure TNPCSourceParser.ParseSettingProgram(const AToken: TNPCToken; const ABlock: TNPC_ASTBlock);
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

procedure TNPCSourceParser.ParseSettingProgramType(const AToken: TNPCToken; const ABlock: TNPC_ASTBlock);

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

procedure TNPCSourceParser.ParseSearchPath(const AToken: TNPCToken; const ABlock: TNPC_ASTBlock);

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

procedure TNPCSourceParser.ParseDefines(const AToken: TNPCToken; const ABlock: TNPC_ASTBlock);
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

function TNPCSourceParser.ParseStatement(const AFlags: TNPCParseStatementFlags; const ABlock: TNPC_ASTBlock): TNPC_ASTStatement; // ; var AStatement: TNPC_ASTStatement): Boolean;
var
  token, next_token, next_next_token: TNPCToken;
  loc: TNPCLocation;
  ident: String;
  statement: TNPC_ASTStatement;
  expression: TNPC_ASTExpression;
  idx: Integer;
  type_cast: TNPC_ASTTypeDefinition;
label
  _Declaration,
  _Expression;
begin
  Result := Nil;
//  Result := False;
//  AStatement := Nil;
  if Texer.IsNotEmpty then begin
    token := Texer.GetToken;
    loc := token.Location;

    // if we have:
    // ident
    // then check if next token is:
    if TokenIsIdent(token) then begin
      next_token := Texer.PeekToken;

      // ident + ':'
      if TokenIsReservedSymbol(next_token, rs_Colon) then begin // it's a label
        ident := token.Value;
        Texer.SkipToken; // skip after ':'
        next_next_token := Texer.PeekToken;
        if next_next_token.&Type = tokCCurly then // '}' block close
          statement := Nil
        else if next_next_token.&Type = tokOCurly then // '{' new block open
          statement := ParseStatement([stafStartNewScope, stafStatementIsRequired], ABlock) //, statement)
        else if stafStatementIsRequiredWithNewScope in AFlags then
          statement := ParseStatement([stafStatementIsRequired, stafEmptyStatementIsAcceptable], ABlock) //, statement)
        else
          statement := ParseStatement([stafEmptyStatementIsAcceptable], ABlock); //, statement);
        //
        Result{AStatement} := TNPC_ASTLabel.Create; // LabelStatement(Texer. loc, ident, s);
        Result.Location := loc.Copy;
        ABlock.AddStatement(Result);
//        AStatement.Expression := // never TNPC_ASTExpression.Create;, must be somthing derived from it
//        AStatement.Expression.
        TNPC_ASTLabel(Result).Ident := ident;
        TNPC_ASTLabel(Result).Statement := statement; // never TNPC_ASTExpression.Create;, must be somthing derived from it
//        Result := True;
        Exit;
      end;

// case
// TOK.question: '?'
// TOK.andAssign: '&='
// TOK.orAssign:  '|='
// TOK.xorAssign: '^='
// TOK.leftShiftAssign: '<<='
// TOK.rightShiftAssign: '>>='

      // if we have:
      // ident + '.'
      // ident + '['
      // ident + '++'
      // ident + '--'
      // ident + ':='
      // ident + '+='
      // ident + '-='
      // ident + '*='
      // ident + '/='
      // ident + '%='
      // then eveluate as expression
      if TokenIsReservedSymbol(next_token, [rs_Dot,
                                            rs_OBracket,
                                            rs_DoublePlus,
                                            rs_DoubleMinus,
                                            rs_Assign,
                                            rs_PlusEqual,
                                            rs_MinusEqual,
                                            rs_MulEqual,
                                            rs_DivEqual,
                                            rs_ModEqual]) then
        goto _Expression;

      // ident + '('
      if TokenIsReservedSymbol(next_token, rs_OParen) then begin // it may be a type_cast for variable declaration or function call
        type_cast := LookupTypeDefinition(token);
        if (type_cast <> Nil) and (type_cast.DefinitionOfType <> DEF_Unknown) then
            goto _Declaration;
        goto _Expression;  // function call
      end;

      // ident + ';'
      if TokenIsReservedSymbol(next_token, rs_Semicolon) then // it may be a type_cast for variable declaration or function call
        goto _Expression;

      // else it must be declaration
      if IsDeclaration(token) then
        goto _Declaration;

      goto _Expression;
    end

// case
// TOK.Literal:
// TOK.leftParenthesis:
// TOK.and:
// TOK.mul:
// TOK.min:
// TOK.add:
// TOK.tilde:
// TOK.not:
// TOK.plusPlus:
// TOK.minusMinus:
// TOK.sizeof_:
// TOK._Generic:
// TOK._assert:

    // if we have:
    // ident
    // '('
    // '*'
    // '-'
    // '+'
    // '++'
    // '--'
    // 'and'
    // 'not' or '!'
    // then eveluate as expression
    else if TokenIsIdent(token) or // not reserved ident
            TokenIsReservedSymbol(token, [rs_OParen, rs_Asterisk, rs_Minus, rs_Plus,{ rs_Tilda,} rs_DoublePlus, rs_DoubleMinus]) or
            TokenIsReservedIdent(token, [ri_and, ri_not]) then begin
//      _Declaration:
      _Expression:
      expression := ParseExpression(token);
      if TokenIsIdent(token) and (expression is TNPC_ASTIdentifier) then begin
        //error(token.loc, "found `%s` when expecting `;` or `=`, did you mean `%s %s = %s`?", peek(&token).toChars(), exp.toChars(), token.toChars(), peek(peek(&token)).toChars());
        Texer.SkipToken;
      end
      else
//        check(TOK.semicolon, "statement");
        Texer.ExpectToken([tokSemicolon]);

      Result := TNPC_ASTExpressionStatement.Create; // ExpStatement(loc, exp);
      Result.Location := loc.Copy;
      Result.Block := ABlock;
      Result.Expression := expression;
      Exit;
    end

//  // type-specifiers
// case
//  TOK.void_:
//  TOK.char_:
//  TOK.int16:
//  TOK.int32:
//  TOK.int64:
//  TOK.__int128:
//  TOK.float32:
//  TOK.float64:
//  TOK.signed:
//  TOK.unsigned:
//  TOK._Bool:
//  //TOK._Imaginary:
//  TOK._Complex:
//  TOK.struct_:
//  TOK.union_:
//  TOK.enum_:
//  TOK.typeof_:
//
//  // storage-class-specifiers
//  TOK.typedef_:
//  TOK.extern_:
//  TOK.static_:
//  TOK._Thread_local:
//  TOK.__thread:
//  TOK.auto_:
//  TOK.register:
//
//  // function-specifiers
//  TOK.inline:
//  TOK._Noreturn:
//
//  // type-qualifiers
//  TOK.const_:
//  TOK.volatile:
//  TOK.restrict:
//  TOK.__stdcall:
//
//  // alignment-specifier
//  TOK._Alignas:
//
//  // atomic-type-specifier or type_qualifier
//  TOK._Atomic:
//
//  TOK.__attribute__:
//  TOK.__declspec:

//    else if TokenIsIdent(token) then begin
//      _Declaration:
//            cparseDeclaration(LVL.local);
//            if (symbols.length > 1)
//            {
//                auto as = new AST.Statements();
//                as.reserve(symbols.length);
//                foreach (d; (*symbols)[])
//                {
//                    s = new AST.ExpStatement(loc, d);
//                    as.push(s);
//                }
//                s = new AST.CompoundDeclarationStatement(loc, as);
//                symbols.setDim(0);
//            }
//            else if (symbols.length == 1)
//            {
//                auto d = (*symbols)[0];
//                s = new AST.ExpStatement(loc, d);
//                symbols.setDim(0);
//            }
//            else
//                s = new AST.ExpStatement(loc, cast(AST.Expression)null);
//            if (flags & ParseStatementFlags.scope_)
//                s = new AST.ScopeStatement(loc, s, token.loc);
//            break;
//    end


//    if TokenIsReservedSymbol(token, rs_Dollar) then begin
//    end
//    else if TokenIsReservedSymbol(token, rs_At) then begin
//    end
//    else if TokenIsReservedSymbol(token, rs_CCurly) then begin
//      Break;
//    end
////    else if TokenIsReservedIdent(token, ri_imports) then begin
////      ParseImports(token);
////    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
  end;
  _Declaration:
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

function TNPCSourceParser.ParseExpression(const AToken: TNPCToken): TNPC_ASTExpression;
var
  token: TNPCToken;
  loc: TNPCLocation;
  exp: TNPC_ASTExpression;
begin
  Result := Nil;
  if Texer.IsEmpty then
    Exit;

  loc := AToken.Location;

  exp := ParseAssignment(AToken); // maybe this is assignment

  token := Texer.PeekToken;
  while Texer.IsNotEmpty and TokenIsReservedSymbol(token, rs_Comma) do begin
    token := Texer.GetToken;
    exp := ParseAssignment(token);
//    Result := TNPC_ASTCommaExpression(loc, Result, exp, False);
    loc := token.Location;
  end;
  Result := exp;
end;

function TNPCSourceParser.ParsePrimaryExpression(const AToken: TNPCToken): TNPC_ASTExpression;
var
  token: TNPCToken;
  loc: TNPCLocation;
  exp: TNPC_ASTExpression;
  temp: String;
begin
  Result := Nil;
  if Texer.IsEmpty then
    Exit;

  loc := AToken.Location;

  case AToken.&Type of
    tokIdent: begin

    end;
    tokNumber: begin

    end;
    tokString: begin
      temp := AToken.Value;
      Texer.SkipToken;
      while Texer.IsNotEmpty do begin
        token := Texer.PeekToken;
        if token.&Type = tokChar then begin
          temp := temp + token.Value;
          loc.SetEndRowCol(token.Location.EndRow, token.Location.EndCol);
          Texer.SkipToken;
        end
        else
          Break;
      end;
      Result := TNPC_ASTLiteral.Create;
      Result.Location := loc.Copy;
      TNPC_ASTLiteral(Result).ValueType := LITERAL_String;
      TNPC_ASTLiteral(Result).StringValue := temp;
    end;
    tokChar: begin

    end;
  end;


//  ParseExpressionSimpleExpression;
//  while Texer.IsNotEmpty do begin
//    token := Texer.PeekToken;
//    if token.ReservedSymbol and (token.&Type in [tokEqual, tokNotEqual, tokLessThan, tokLessEqual, tokGreaterThan, tokGreaterEqual]) then begin
////      AddToken(token);
//      Texer.SkipToken;
//      ParseExpressionSimpleExpression;
//      Continue;
//    end
//    else if TokenIsReservedSymbol(token, rs_Semicolon) then
//      Break
//    else
//      Break;
////      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'expression ', sStatement]));
//    //
//    Texer.SkipToken;
//  end;
end;

// assignment = ident [param_list] ":=" expr
//
// param_list = ('(' | '[') ident { ',' ident } (')' | ']')

function TNPCSourceParser.ParseAssignment(const AToken: TNPCToken): TNPC_ASTExpression;
var
  assign_type: TNPCToken;
  exp: TNPC_ASTExpression;
begin
  Result := Nil;

  assign_type := Texer.PeekToken;
  if TokenIsOfType(assign_type, [tokAssign]) then begin
    Texer.SkipToken; // ':=', '+=', '-=', '*=', '/=', '%='
    Result := TNPC_ASTAssign.Create;
    Result.Location := AToken.Location.Copy;
    TNPC_ASTAssign(Result).Ident := AToken.Value; // ident to assign to; @TODO: check if it exists in declared variables
    TNPC_ASTAssign(Result).Expression := ParseExpression(Texer.PeekToken); // get everything until ';'
    Texer.ExpectToken([tokSemicolon], True);
  end
  else if TokenIsOfType(assign_type, [tokPlusEqual]) then begin
    Texer.SkipToken; // ':=', '+=', '-=', '*=', '/=', '%='
  end
  else if TokenIsOfType(assign_type, [tokMinusEqual]) then begin
    Texer.SkipToken; // ':=', '+=', '-=', '*=', '/=', '%='
  end
  else if TokenIsOfType(assign_type, [tokMulEqual]) then begin
    Texer.SkipToken; // ':=', '+=', '-=', '*=', '/=', '%='
  end
  else if TokenIsOfType(assign_type, [tokDivEqual]) then begin
    Texer.SkipToken; // ':=', '+=', '-=', '*=', '/=', '%='
  end
  else if TokenIsOfType(assign_type, [tokModEqual]) then begin
    Texer.SkipToken; // ':=', '+=', '-=', '*=', '/=', '%='
  end
  else begin
    Result := ParsePrimaryExpression(AToken);
  end;
end;

//procedure TNPCSourceParser.ParseExpressionSimpleExpression;
//var
//  token: TNPCToken;
//begin
//  ParseExpressionTerm;
//  while Texer.IsNotEmpty do begin
//    token := Texer.PeekToken;
//    if (token.ReservedSymbol and (token.&Type in [tokPlus, tokMinus])) or TokenIsReservedIdent(token, ri_or) or TokenIsReservedIdent(token, ri_xor) then begin
////      AddToken(token);
//      Texer.SkipToken;
//      ParseExpressionTerm;
//      Continue;
//    end
//    else if TokenIsReservedSymbol(token, rs_Semicolon) then
//      Break
//    else
//      Break;
////      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'expression ', sStatement]));
//    //
//    Texer.SkipToken;
//  end;
//end;
//
//procedure TNPCSourceParser.ParseExpressionTerm;
//var
//  token: TNPCToken;
//begin
//  ParseExpressionFactor;
//  while Texer.IsNotEmpty do begin
//    token := Texer.PeekToken;
//    if (token.ReservedSymbol and (token.&Type in [tokAsterisk, tokDiv])) or
//       TokenIsReservedIdent(token, ri_div) or
//       TokenIsReservedIdent(token, ri_mod) or
//       TokenIsReservedIdent(token, ri_and) or
//       TokenIsReservedIdent(token, ri_shl) or
//       TokenIsReservedIdent(token, ri_shr) then begin
////      AddToken(token);
//      Texer.SkipToken;
//      ParseExpressionFactor;
//      Continue;
//    end
//    else if TokenIsReservedSymbol(token, rs_Semicolon) then
//      Break
//    else
//      Break;
////      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'expression ', sStatement]));
//    //
//    Texer.SkipToken;
//  end;
//end;
//
//procedure TNPCSourceParser.ParseExpressionFactor;
//var
//  token: TNPCToken;
//begin
//  token := Texer.PeekToken;
//  if not token.ReservedWord and (token.&Type in [tokIdent..tokChar]) then begin
////    AddToken(token);
//    Texer.SkipToken;
//  end
//  else if TokenIsReservedIdent(token, ri_nil) then begin
////    AddToken(token);
//    Texer.SkipToken;
//  end
//  else if TokenIsReservedIdent(token, ri_not) or TokenIsReservedIdent(token, ri_inherited) then begin
////    AddToken(token);
//    Texer.SkipToken;
//    ParseExpressionFactor;
//  end
//  else if TokenIsReservedSymbol(token, rs_At) then begin
////    AddToken(token);
//    Texer.SkipToken;
//    ParseExpressionFactor;
//  end
//  else if TokenIsReservedSymbol(token, rs_Dash) then begin
////    AddToken(token);
//    Texer.SkipToken;
////    AddToken(Texer.ExpectToken([tokIdent]));
//    Texer.ExpectToken([tokIdent]);
//  end
//  else if TokenIsReservedSymbol(token, rs_OBracket) then begin // set
////    AddToken(token);
//    Texer.SkipToken;
//    while Texer.IsNotEmpty do begin
//      token := Texer.PeekToken;
//      //
//      if not token.ReservedWord and (token.&Type = tokIdent) then begin
////        AddToken(token);
//      end
//      else if TokenIsReservedSymbol(token, rs_Comma) or TokenIsReservedSymbol(token, rs_DoubleDot) then begin
////        AddToken(token);
//      end
//      else if TokenIsReservedSymbol(token, rs_CBracket) then
//        Break
//      else
//        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sExpression]));
//      //
//      Texer.SkipToken;
//    end;
////    AddToken(Texer.ExpectToken([tokCBracket]));
//    Texer.ExpectToken([tokCBracket]);
//  end
//  else if TokenIsReservedSymbol(token, rs_OParen) then begin // call params
////    AddToken(token);
//    Texer.SkipToken;
//    ParseExpression;
//    while Texer.IsNotEmpty do begin
//      token := Texer.PeekToken;
//      //
//      if not token.ReservedWord and (token.&Type = tokIdent) then begin
////        AddToken(token);
//      end
//      else if TokenIsReservedSymbol(token, rs_Comma) or TokenIsReservedSymbol(token, rs_Dot) or TokenIsReservedSymbol(token, rs_Dash) then begin
////        AddToken(token);
//        Texer.SkipToken;
//        ParseExpression;
//        Continue;
//      end
//      else if TokenIsReservedIdent(token, ri_as) then begin
////        AddToken(token);
////        AddToken(Texer.ExpectToken([tokIdent]));
//        Texer.ExpectToken([tokIdent]);
//        Continue;
//      end
//      else if TokenIsReservedSymbol(token, rs_CParen) then
//        Break
//      else
//        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sExpression]));
//      //
//      Texer.SkipToken;
//    end;
////    AddToken(Texer.ExpectToken([tokCParen]));
//    Texer.ExpectToken([tokCParen]);
//  end
//  else if (token.ReservedSymbol and (token.&Type in [tokPlus, tokMinus])) then begin
////    AddToken(token);
//    Texer.SkipToken;
//    ParseExpressionFactor;
//  end
//  else if TokenIsReservedSymbol(token, rs_Semicolon) then
//    Exit
//  else
//    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'expression ', sStatement]));
//end;

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
        IncLevel;
      end
      else if TokenIsReservedIdent(token, ri_case) then begin
        IncLevel;
        ParseCase(token);
        Continue;
      end
      else if TokenIsReservedIdent(token, ri_if) then begin
        IncLevel;
        ParseIf(token);
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

procedure TNPCSourceParser.ParseIf(const AToken: TNPCToken);
var
  token: TNPCToken;
begin
//  AddToken(AToken); // 'if'
  Texer.SkipToken;
  {cond := }ParseIfExpression; // get everything until 'then'
  Texer.ExpectReservedToken(ri_then);
  ParseIfStatement;
  //
  SkipComments;
  token := Texer.PeekToken;
  if TokenIsReservedIdent(token, ri_else) then begin
    if (Texer.LastToken <> Nil) and TokenIsReservedSymbol(Texer.LastToken, rs_Semicolon) then
      raise NPCSyntaxError.ParserError(Texer.LastToken.Location, Format(sParserUnexpectedTokenIn, [Texer.LastToken.TokenToString, 'if ', sStatement]));
//    AddToken(token);
    Texer.SkipToken;
    SkipComments;
    token := Texer.PeekToken;
    if TokenIsReservedIdent(token, ri_if) then begin
      ParseIf(token);
      Exit;
    end;
    ParseIfStatement;
  end;
  if (Texer.LastToken <> Nil) and not TokenIsReservedSymbol(Texer.LastToken, rs_Semicolon) then
//    raise NPCSyntaxError.ParserError(LastToken.Location, Format(sParserUnexpectedTokenIn, [LastToken.TokenToString, 'if ', sStatement]));
    raise NPCSyntaxError.ParserError(Texer.LastToken.Location.After, Format(sParserExpectedButGot, [NPCReservedSymbolToString(rs_Semicolon), token.TokenToString]));
//  AddToken(Texer.ExpectToken([tokSemicolon]));
end;

procedure TNPCSourceParser.ParseIfExpression;
var
  token: TNPCToken;
begin
  SkipComments;
  ParseIfSimpleExpression;
  token := Texer.PeekToken;
  if token.ReservedSymbol and (token.&Type in [tokEqual, tokNotEqual, tokLessThan, tokLessEqual, tokGreaterThan, tokGreaterEqual]) then begin
//    AddToken(token);
    Texer.SkipToken;
    ParseIfSimpleExpression;
    Exit;
  end
  else if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then
    Exit
  else if TokenIsReservedSymbol(token, rs_Semicolon) then
    Exit
  else if TokenIsReservedIdent(token, ri_then) then
    Exit
  else
    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
  //
  Texer.SkipToken;
end;

procedure TNPCSourceParser.ParseIfSimpleExpression;
var
  token: TNPCToken;
begin
  ParseIfTerm;
  while Texer.IsNotEmpty do begin
    SkipComments;
    token := Texer.PeekToken;
    if (token.ReservedSymbol and (token.&Type in [tokPlus, tokMinus])) or TokenIsReservedIdent(token, ri_or) or TokenIsReservedIdent(token, ri_xor) then begin
//      AddToken(token);
      Texer.SkipToken;
      ParseIfTerm;
      Continue;
    end
    else if TokenIsReservedSymbol(token, rs_Semicolon) then
      Break
    else if TokenIsReservedIdent(token, ri_then) then
      Break
    else
      Break;
//      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
    //
    Texer.SkipToken;
  end;
end;

procedure TNPCSourceParser.ParseIfTerm;
var
  token: TNPCToken;
begin
  ParseIfFactor;
  while Texer.IsNotEmpty do begin
    SkipComments;
    token := Texer.PeekToken;
    if (token.ReservedSymbol and (token.&Type in [tokAsterisk, tokDiv])) or
       TokenIsReservedIdent(token, ri_div) or
       TokenIsReservedIdent(token, ri_mod) or
       TokenIsReservedIdent(token, ri_and) then begin
//      AddToken(token);
      Texer.SkipToken;
      ParseIfFactor;
      Continue;
    end
    else if TokenIsReservedSymbol(token, rs_Semicolon) then
      Break
    else if TokenIsReservedIdent(token, ri_then) then
      Break
    else
      Break;
//      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
    //
    Texer.SkipToken;
  end;
end;

procedure TNPCSourceParser.ParseIfFactor;
var
  token: TNPCToken;
begin
  token := Texer.PeekToken;
  if not token.ReservedWord and (token.&Type in [tokIdent, tokNumber]) then begin
//    AddToken(token);
    Texer.SkipToken;
  end
  else if TokenIsReservedIdent(token, ri_not) then begin
//    AddToken(token);
    Texer.SkipToken;
    ParseIfFactor;
  end
  else if TokenIsReservedSymbol(token, rs_Percent) then begin
//    AddToken(token);
    Texer.SkipToken;
//    AddToken(Texer.ExpectToken([tokIdent], True));
//    AddToken(Texer.ExpectToken([tokPercent]));
    Texer.ExpectToken([tokIdent], True);
    Texer.ExpectToken([tokPercent]);
  end
  else if (token.ReservedSymbol and (token.&Type in [tokPlus, tokMinus])) then begin
//    AddToken(token);
    Texer.SkipToken;
    ParseIfFactor;
  end
  else if TokenIsReservedSymbol(token, rs_OParen) then begin
    if (Texer.LastToken <> Nil) and not Texer.LastToken.ReservedWord and (Texer.LastToken.&Type = tokIdent) then
      ParseCallParams(token)
    else begin
//      AddToken(token);
      Texer.SkipToken;
      while Texer.IsNotEmpty do begin
        //ParseExpression;
        token := Texer.PeekToken;
        if TokenIsReservedSymbol(token, rs_Comma) then begin
//          AddToken(token);
          Texer.SkipToken;
        end
        else if TokenIsReservedSymbol(token, rs_CParen) then begin
          Break;
        end;
//        else
//          raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
      end;
    end;
//    AddToken(Texer.ExpectToken([tokCParen]));
    Texer.ExpectToken([tokCParen]);
  end
  else if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then
    Exit
  else if TokenIsReservedIdent(token, ri_then) then
    Exit
  else
    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
end;

procedure TNPCSourceParser.ParseIfStatement;
var
  token: TNPCToken;
begin
  SkipComments;
  token := Texer.PeekToken;
  if not token.ReservedWord and (token.&Type = tokIdent) then begin
//    AddToken(token);
    Texer.SkipToken;
    token := Texer.PeekToken;
    if TokenIsReservedSymbol(token, rs_CParen) then begin
      ParseIfStatementFuncParams(token);
//      AddToken(Texer.ExpectToken([tokCParen]));
//      AddToken(Texer.ExpectToken([tokSemicolon]));
      Texer.ExpectToken([tokCParen]);
      Texer.ExpectToken([tokSemicolon]);
    end
    else if TokenIsReservedSymbol(token, rs_Assign) then begin // assignment
//      AddToken(token);
      Texer.SkipToken;
      ParseIfExpression;
//      AddToken(Texer.ExpectToken([tokSemicolon]));
      Texer.ExpectToken([tokSemicolon]);
    end
    else if TokenIsReservedIdent(token, ri_if) then
      ParseIf(token)
    else if TokenIsReservedIdent(token, ri_begin) or TokenIsReservedSymbol(token, rs_OCurly) then begin
//      AddToken(token);
      Texer.SkipToken;
      while Texer.IsNotEmpty do begin
        ParseIfStatement;
        token := Texer.PeekToken;
        if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then begin
//          AddToken(token);
          Texer.SkipToken;
          Break;
        end
        else
          raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
      end;
//      AddToken(Texer.ExpectToken([tokSemicolon]));
      Texer.ExpectToken([tokSemicolon]);
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
  end
  else if TokenIsReservedIdent(token, ri_begin) or TokenIsReservedSymbol(token, rs_OCurly) then begin
//    AddToken(token);
    Texer.SkipToken;
    while Texer.IsNotEmpty do begin
      token := Texer.PeekToken;
      if not ParseStatements(token, [], FLevel + 1) then begin
        if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then begin
//          AddToken(token);
          Texer.SkipToken;
          token := Texer.PeekToken;
          if TokenIsReservedSymbol(token, rs_Semicolon) then
//            AddToken(Texer.GetToken);
            Texer.SkipToken;
          Break;
        end
        else if TokenIsReservedSymbol(token, rs_Semicolon) then begin
//          AddToken(token);
//          Texer.SkipToken;
          Break;
        end
        else
          raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
      end
      else if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then begin
//        AddToken(token);
        Texer.SkipToken;
        token := Texer.PeekToken;
        if TokenIsReservedSymbol(token, rs_Semicolon) then
//          AddToken(Texer.GetToken);
          Texer.SkipToken;
        Break;
      end;
    end;
    //AddToken(Texer.ExpectToken([tokSemicolon]));
  end
  else if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then
    Exit
  else
    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
end;

procedure TNPCSourceParser.ParseIfStatementFuncParams(const AToken: TNPCToken);
var
  token: TNPCToken;
begin
//  AddToken(AToken);
  Texer.SkipToken;
  while Texer.IsNotEmpty do begin
    token := Texer.PeekToken;
    if not token.ReservedWord and (token.&Type = tokIdent) then begin
//      AddToken(token);
      Texer.SkipToken;
      token := Texer.PeekToken;
      if TokenIsReservedSymbol(token, rs_CParen) then begin
        ParseIfStatementFuncParams(token);
//        AddToken(Texer.ExpectToken([tokCParen]));
//        AddToken(Texer.ExpectToken([tokSemicolon]));
        Texer.ExpectToken([tokCParen]);
        Texer.ExpectToken([tokSemicolon]);
      end
      else if token.&Type in [tokNumber, tokString] then begin
//        AddToken(token);
        Texer.SkipToken;
      end
      else if TokenIsReservedSymbol(token, rs_Comma) then begin
//        AddToken(token);
        Texer.SkipToken;
      end
      else
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
    end
    else if TokenIsReservedSymbol(token, rs_CParen) then
      Break
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
    //
    Texer.SkipToken;
  end;
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

procedure TNPCSourceParser.ParseCase(const AToken: TNPCToken);
var
  token: TNPCToken;
  case_of: Boolean;
begin
//  AddToken(AToken); // 'case'
  Texer.SkipToken;
  ParseCaseExpression; // get everything until 'of' or '{'
  token := Texer.GetToken;
  case_of := TokenIsReservedIdent(token, ri_of); // and not TokenIsReservedSymbol(token, rs_OCurly);
  if not TokenIsReservedIdent(token, ri_of) and not TokenIsReservedSymbol(token, rs_OCurly) then
    raise NPCSyntaxError.ParserError(Texer.LastToken.Location, Format(sParserUnexpectedTokenIn, [Texer.LastToken.TokenToString, 'case ', sStatement]));
//  AddToken(token);
  ParseCaseElements(case_of);
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

procedure TNPCSourceParser.ParseCaseExpression;
var
  token: TNPCToken;
begin
  SkipComments;
  ParseCaseSimpleExpression;
  token := Texer.PeekToken;
  if token.ReservedSymbol and (token.&Type in [tokEqual, tokNotEqual, tokLessThan, tokLessEqual, tokGreaterThan, tokGreaterEqual]) then begin
//    AddToken(token);
    Texer.SkipToken;
    ParseCaseSimpleExpression;
    Exit;
  end
  else if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then
    Exit
  else if TokenIsReservedSymbol(token, rs_Semicolon) then
    Exit
  else if TokenIsReservedIdent(token, ri_of) or TokenIsReservedSymbol(token, rs_OCurly) then
    Exit
  else
    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'case ', sStatement]));
  //
  Texer.SkipToken;
end;

procedure TNPCSourceParser.ParseCaseSimpleExpression;
var
  token: TNPCToken;
begin
  ParseCaseTerm;
  while Texer.IsNotEmpty do begin
    SkipComments;
    token := Texer.PeekToken;
    if (token.ReservedSymbol and (token.&Type in [tokPlus, tokMinus])) or TokenIsReservedIdent(token, ri_or) or TokenIsReservedIdent(token, ri_xor) then begin
//      AddToken(token);
      Texer.SkipToken;
      ParseCaseTerm;
      Continue;
    end
    else if TokenIsReservedSymbol(token, rs_Semicolon) then
      Break
    else if TokenIsReservedIdent(token, ri_of) then
      Break
    else
      Break;
    //
    Texer.SkipToken;
  end;
end;

procedure TNPCSourceParser.ParseCaseTerm;
var
  token: TNPCToken;
begin
  ParseCaseFactor;
  while Texer.IsNotEmpty do begin
    SkipComments;
    token := Texer.PeekToken;
    if (token.ReservedSymbol and (token.&Type in [tokAsterisk, tokDiv])) or
       TokenIsReservedIdent(token, ri_div) or
       TokenIsReservedIdent(token, ri_mod) or
       TokenIsReservedIdent(token, ri_and) then begin
//      AddToken(token);
      Texer.SkipToken;
      ParseCaseFactor;
    end
    else if TokenIsReservedSymbol(token, rs_Semicolon) then
      Break
    else if TokenIsReservedIdent(token, ri_of) then
      Break
    else
      Break;
//      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
    //
    Texer.SkipToken;
  end;
end;

procedure TNPCSourceParser.ParseCaseFactor;
var
  token: TNPCToken;
begin
  token := Texer.PeekToken;
  if not token.ReservedWord and (token.&Type in [tokIdent, tokNumber]) then begin
//    AddToken(token);
    Texer.SkipToken;
  end
  else if TokenIsReservedIdent(token, ri_not) then begin
//    AddToken(token);
    Texer.SkipToken;
    ParseCaseFactor;
  end
  else if TokenIsReservedSymbol(token, rs_Percent) then begin
//    AddToken(token);
    Texer.SkipToken;
//    AddToken(Texer.ExpectToken([tokIdent], True));
//    AddToken(Texer.ExpectToken([tokPercent]));
    Texer.ExpectToken([tokIdent], True);
    Texer.ExpectToken([tokPercent]);
  end
  else if (token.ReservedSymbol and (token.&Type in [tokPlus, tokMinus])) then begin
//    AddToken(token);
    Texer.SkipToken;
    ParseCaseFactor;
  end
  else if TokenIsReservedSymbol(token, rs_CParen) then begin
    if (Texer.LastToken <> Nil) and not Texer.LastToken.ReservedWord and (Texer.LastToken.&Type = tokIdent) then
      ParseCallParams(token)
    else begin
//      AddToken(token);
      Texer.SkipToken;
      while Texer.IsNotEmpty do begin
        //ParseExpression;
        token := Texer.PeekToken;
        if TokenIsReservedSymbol(token, rs_Comma) then begin
//          AddToken(token);
          Texer.SkipToken;
        end
        else if TokenIsReservedSymbol(token, rs_CParen) then begin
          Break;
        end;
      end;
    end;
//    AddToken(Texer.ExpectToken([tokCParen]));
    Texer.ExpectToken([tokCParen]);
  end
  else if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then
    Exit
  else if TokenIsReservedIdent(token, ri_of) then
    Exit
  else
    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'case ', sStatement]));
end;

// CaseElement = 'if' CaseLabel { ',' CaseLabel } ':' [ '{@next' [ ':' CaseElement ] '}' ] Statement .
//
// CaseLabel = ConstExpression [ '..' ConstExpression ] .

procedure TNPCSourceParser.ParseCaseElements(const case_of: Boolean);
var
  token: TNPCToken;
begin
  while Texer.IsNotEmpty do begin
    SkipComments;
    token := Texer.PeekToken;
    if TokenIsReservedIdent(token, ri_if) then begin
//      AddToken(token); // 'if'
      Texer.SkipToken;
      ParseCaseIfExpression; // get everything until ':'
//      AddToken(Texer.ExpectReservedSymbol(rs_Colon));
      Texer.ExpectReservedSymbol(rs_Colon);
      ParseCaseIfStatement;
//      raise Exception.Create('Error Message');
    end
    else if TokenIsReservedSymbol(token, rs_Semicolon) then
      Break
    else if (case_of and not TokenIsReservedIdent(token, ri_end)) or (not case_of and not TokenIsReservedSymbol(token, rs_CCurly)) then
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'case ', sStatement]))
    else
      Break;
    //
    Texer.SkipToken;
  end;

//  AddToken(AToken); // 'if'
//  Texer.SkipToken;
//  ParseIfExpression; // get everything until 'then'
//  AddToken(Texer.ExpectReservedToken(ri_then));
//  ParseIfStatement;
//  //
//  SkipComments;
//  token := Texer.PeekToken;
//  if TokenIsReservedIdent(token, ri_else) then begin
//    if (LastToken <> Nil) and TokenIsReservedSymbol(LastToken, rs_Semicolon) then
//      raise NPCSyntaxError.ParserError(LastToken.Location, Format(sParserUnexpectedTokenIn, [LastToken.TokenToString, 'if ', sStatement]));
//    AddToken(token);
//    Texer.SkipToken;
//    ParseIfStatement;
//  end;
//  if (LastToken <> Nil) and not TokenIsReservedSymbol(LastToken, rs_Semicolon) then
//    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
//    raise NPCSyntaxError.ParserError(LastToken.Location.After, Format(sParserExpectedButGot, [NPCReservedSymbolToString(rs_Semicolon), token.TokenToString]));
//  AddToken(Texer.ExpectToken([tokSemicolon]));
end;

procedure TNPCSourceParser.ParseCaseElseStatements;
begin

end;

procedure TNPCSourceParser.ParseCaseIfExpression;
var
  token: TNPCToken;
begin
  SkipComments;
  ParseCaseIfSimpleExpression;
  token := Texer.PeekToken;
  if token.ReservedSymbol and (token.&Type in [tokEqual, tokNotEqual, tokLessThan, tokLessEqual, tokGreaterThan, tokGreaterEqual]) then begin
//    AddToken(token);
    Texer.SkipToken;
    ParseCaseIfSimpleExpression;
    Exit;
  end
  else if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then
    Exit
  else if TokenIsReservedSymbol(token, rs_Semicolon) then
    Exit
  else if TokenIsReservedSymbol(token, rs_Colon) then
    Exit
  else
    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
  //
  Texer.SkipToken;
end;

procedure TNPCSourceParser.ParseCaseIfSimpleExpression;
var
  token: TNPCToken;
begin
  ParseCaseIfTerm;
  while Texer.IsNotEmpty do begin
    SkipComments;
    token := Texer.PeekToken;
    if (token.ReservedSymbol and (token.&Type in [tokPlus, tokMinus])) or TokenIsReservedIdent(token, ri_or) or TokenIsReservedIdent(token, ri_xor) then begin
//      AddToken(token);
      Texer.SkipToken;
      ParseCaseIfTerm;
      Continue;
    end
    else if TokenIsReservedSymbol(token, rs_Semicolon) then
      Break
    else if TokenIsReservedSymbol(token, rs_Colon) then
      Break
    else
      Break;
//      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
    //
    Texer.SkipToken;
  end;
end;

procedure TNPCSourceParser.ParseCaseIfTerm;
var
  token: TNPCToken;
begin
  ParseCaseIfFactor;
  while Texer.IsNotEmpty do begin
    SkipComments;
    token := Texer.PeekToken;
    if (token.ReservedSymbol and (token.&Type in [tokAsterisk, tokDiv])) or
       TokenIsReservedIdent(token, ri_div) or
       TokenIsReservedIdent(token, ri_mod) or
       TokenIsReservedIdent(token, ri_and) then begin
//      AddToken(token);
      Texer.SkipToken;
      ParseCaseIfFactor;
    end
    else if TokenIsReservedSymbol(token, rs_Semicolon) then
      Break
    else if TokenIsReservedSymbol(token, rs_Colon) then
      Break
    else
      Break;
//      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
    //
    Texer.SkipToken;
  end;
end;

procedure TNPCSourceParser.ParseCaseIfFactor;
var
  token: TNPCToken;
begin
  token := Texer.PeekToken;
  if not token.ReservedWord and (token.&Type in [tokIdent, tokNumber]) then begin
//    AddToken(token);
    Texer.SkipToken;
  end
  else if TokenIsReservedSymbol(token, rs_Dot) then begin
//    AddToken(token);
    Texer.SkipToken;
    ParseCaseIfFactor;
  end
  else if TokenIsReservedIdent(token, ri_not) then begin
//    AddToken(token);
    Texer.SkipToken;
    ParseCaseIfFactor;
  end
  else if TokenIsReservedSymbol(token, rs_Percent) then begin
//    AddToken(token);
    Texer.SkipToken;
//    AddToken(Texer.ExpectToken([tokIdent], True));
//    AddToken(Texer.ExpectToken([tokPercent]));
    Texer.ExpectToken([tokIdent], True);
    Texer.ExpectToken([tokPercent]);
  end
  else if (token.ReservedSymbol and (token.&Type in [tokPlus, tokMinus])) then begin
//    AddToken(token);
    Texer.SkipToken;
    ParseCaseIfFactor;
  end
  else if TokenIsReservedSymbol(token, rs_OParen) then begin
    if (Texer.LastToken <> Nil) and not Texer.LastToken.ReservedWord and (Texer.LastToken.&Type = tokIdent) then
      ParseCallParams(token)
    else begin
//      AddToken(token);
      Texer.SkipToken;
      while Texer.IsNotEmpty do begin
        //ParseExpression;
        token := Texer.PeekToken;
        if TokenIsReservedSymbol(token, rs_Comma) then begin
//          AddToken(token);
          Texer.SkipToken;
        end
        else if TokenIsReservedSymbol(token, rs_CParen) then begin
          Break;
        end;
//        else
//          raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
      end;
    end;
//    AddToken(Texer.ExpectToken([tokCParen]));
    Texer.ExpectToken([tokCParen]);
  end
  else if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then
    Exit
  else if TokenIsReservedIdent(token, ri_then) then
    Exit
  else
    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
end;

procedure TNPCSourceParser.ParseCaseIfStatement;
var
  token: TNPCToken;
begin
  SkipComments;
  token := Texer.PeekToken;
  if not token.ReservedWord and (token.&Type = tokIdent) then begin
//    AddToken(token);
    Texer.SkipToken;
    token := Texer.PeekToken;
    if TokenIsReservedSymbol(token, rs_CParen) then begin
      ParseCaseIfStatementFuncParams(token);
//      AddToken(Texer.ExpectToken([tokCParen]));
//      AddToken(Texer.ExpectToken([tokSemicolon]));
      Texer.ExpectToken([tokCParen]);
      Texer.ExpectToken([tokSemicolon]);
    end
    else if TokenIsReservedSymbol(token, rs_Assign) then begin // assignment
//      AddToken(token);
      Texer.SkipToken;
      ParseCaseIfExpression;
//      AddToken(Texer.ExpectToken([tokSemicolon]));
      Texer.ExpectToken([tokSemicolon]);
    end
    else if TokenIsReservedIdent(token, ri_if) then
      ParseIf(token)
    else if TokenIsReservedIdent(token, ri_begin) or TokenIsReservedSymbol(token, rs_OCurly) then begin
//      AddToken(token);
      Texer.SkipToken;
      while Texer.IsNotEmpty do begin
        ParseCaseIfStatement;
        token := Texer.PeekToken;
        if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then begin
//          AddToken(token);
          Texer.SkipToken;
          Break;
        end
        else
          raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
      end;
//      AddToken(Texer.ExpectToken([tokSemicolon]));
      Texer.ExpectToken([tokSemicolon]);
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
  end
  else if TokenIsReservedIdent(token, ri_begin) or TokenIsReservedSymbol(token, rs_OCurly) then begin
//    AddToken(token);
    Texer.SkipToken;
    while Texer.IsNotEmpty do begin
      token := Texer.PeekToken;
      if not ParseStatements(token, [], FLevel + 1) then begin
        if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then begin
//          AddToken(token);
          Texer.SkipToken;
          token := Texer.PeekToken;
          if TokenIsReservedSymbol(token, rs_Semicolon) then
//            AddToken(token);
            Texer.SkipToken;
          Break;
        end
        else if TokenIsReservedSymbol(token, rs_Semicolon) then begin
//          AddToken(token);
          Texer.SkipToken;
          Break;
        end
        else
          raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
      end
      else if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then begin
//        AddToken(token);
        Texer.SkipToken;
        token := Texer.PeekToken;
        if TokenIsReservedSymbol(token, rs_Semicolon) then
//          AddToken(token);
          Texer.SkipToken;
        Break;
      end;
    end;
    //AddToken(Texer.ExpectToken([tokSemicolon]));
  end
  else if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then
    Exit
  else
    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
end;

procedure TNPCSourceParser.ParseCaseIfStatementFuncParams(const AToken: TNPCToken);
var
  token: TNPCToken;
begin
//  AddToken(AToken);
  Texer.SkipToken;
  while Texer.IsNotEmpty do begin
    token := Texer.PeekToken;
    if not token.ReservedWord and (token.&Type = tokIdent) then begin
//      AddToken(token);
      Texer.SkipToken;
      token := Texer.PeekToken;
      if TokenIsReservedSymbol(token, rs_CParen) then begin
        ParseCaseIfStatementFuncParams(token);
//        AddToken(Texer.ExpectToken([tokCParen]));
//        AddToken(Texer.ExpectToken([tokSemicolon]));
        Texer.ExpectToken([tokCParen]);
        Texer.ExpectToken([tokSemicolon]);
      end
      else if token.&Type in [tokNumber, tokString] then begin
//        AddToken(token);
        Texer.SkipToken;
      end
      else if TokenIsReservedSymbol(token, rs_Comma) then begin
//        AddToken(token);
        Texer.SkipToken;
      end
      else
        raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
    end
    else if TokenIsReservedSymbol(token, rs_CParen) then
      Break
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
    //
    Texer.SkipToken;
  end;
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

procedure TNPCSourceParser.ParseFor(const AToken: TNPCToken);
var
  token: TNPCToken;
  for_do: Boolean;
begin
//  AddToken(AToken); // 'for'
  Texer.SkipToken;
  ParseForParams; // get everything until 'do' or '{'
  token := Texer.GetToken;
  for_do := TokenIsReservedIdent(token, ri_do); // and not TokenIsReservedSymbol(token, rs_OCurly);
  if not TokenIsReservedIdent(token, ri_of) and not TokenIsReservedSymbol(token, rs_OCurly) then
    raise NPCSyntaxError.ParserError(Texer.LastToken.Location, Format(sParserUnexpectedTokenIn, [Texer.LastToken.TokenToString, 'for ', sStatement]));
//  AddToken(token);
  ParseForStatements(for_do);
  //
  token := Texer.GetToken;
  if (for_do and not TokenIsReservedIdent(token, ri_end)) or (not for_do and not TokenIsReservedSymbol(token, rs_CCurly)) then
    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'for ', sStatement]));
//  AddToken(token);
  token := Texer.GetToken;
  if not TokenIsReservedSymbol(token, rs_Semicolon) then
    raise NPCSyntaxError.ParserError(Texer.LastToken.Location.After, Format(sParserExpectedButGot, [NPCReservedSymbolToString(rs_Semicolon), token.TokenToString]));
//  AddToken(Texer.ExpectToken([tokSemicolon]));
//  AddToken(token);
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
      sect_init := ParseStatement([], AST);
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

procedure TNPCSourceParser.ParseForStatements(const for_do: Boolean);
var
  token: TNPCToken;
//  has_body: Boolean;
begin
//  has_body := False;
  while Texer.IsNotEmpty do begin
    SkipComments;
    token := Texer.PeekToken; // just peek a token
    if ParseStatements(token, [ri_begin], FLevel + 1) then begin
      // if statements ware parsed than do nothing
//      has_body := True;
    end;
    //
    if TokenIsReservedIdent(token, ri_begin) or TokenIsReservedSymbol(token, rs_CCurly) then begin
//      if not has_body then
//        raise NPCSyntaxError.ParserError(token.Location, Format(sParserSectionHasNoBody, [NPCReservedIdentifiers[ri_initialization].Ident]));
      Break;
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'finalization ', sSection]));
    //
    Texer.SkipToken;
  end;
end;

procedure TNPCSourceParser.ParseForExpression;
var
  token: TNPCToken;
begin
  SkipComments;
  ParseIfSimpleExpression;
  token := Texer.PeekToken;
  if token.ReservedSymbol and (token.&Type in [tokEqual, tokNotEqual, tokLessThan, tokLessEqual, tokGreaterThan, tokGreaterEqual]) then begin
//    AddToken(token);
    Texer.SkipToken;
    ParseCaseSimpleExpression;
    Exit;
  end
  else if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then
    Exit
  else if TokenIsReservedSymbol(token, rs_Semicolon) then
    Exit
  else if TokenIsReservedIdent(token, ri_of) or TokenIsReservedSymbol(token, rs_OCurly) then
    Exit
  else
    raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'case ', sStatement]));
  //
  Texer.SkipToken;
end;

function TNPCSourceParser.IsCodeFileNamedAs(const FileName: String; const CodeName: String): Boolean;
var
  LTokenizer: TNPCTokenizer;
  LTexer: TNPCTokensParser;
  token: TNPCToken;
begin
  Result := False;
  LTokenizer := TNPCTokenizer.Create(TNPCProjectSettings(SettingsPtr^).ProjectFormatSettings^);
  try
    LTokenizer.TokenizeFile(FileName, TNPCProjectSettings(SettingsPtr^).ProjectEncoding);
    LTexer := TNPCTokensParser.Create(FileName, LTokenizer.Tokens);
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

      if not SameText(token.Value, CodeName) then
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

function TNPCSourceParser.LookupTypeDefinition(const AToken: TNPCToken): TNPC_ASTTypeDefinition;
begin

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

procedure TNPCSourceParser.SearchImportByCodeName(const AToken: TNPCToken; const SearchPath: String; const SearchFileName: String; const ADecl: TNPC_ASTDeclaration);
var
  path, checked_path, checked_file, found_path: String;
  recursive, import_found: Boolean;
  i: Integer;
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
    if NPC_CompileImport(PChar(checked_file), Compiler, Self, ADecl.Identifier.Block) then begin

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
      if NPC_CompileImport(PChar(checked_file), Compiler, Self, ADecl.Identifier.Block) then begin

      end
      else begin // errors

      end;
    end
    else
      //ConsoleWriteln('Import not found_____: "' + token.Value + '"');
      raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserImportNotFound, [AToken.Value]));
  end;
end;

procedure TNPCSourceParser.SearchImportByFileName(const AToken: TNPCToken; const SearchPath: String; const SearchFileName: String; const ADecl: TNPC_ASTDeclaration);
var
  path, checked_path, checked_file, found_path: String;
  recursive, import_found: Boolean;
  i: Integer;
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
    if NPC_CompileImport(PChar(checked_file), Compiler, Self, ADecl.Identifier.Block) then begin

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
      if NPC_CompileImport(PChar(checked_file), Compiler, Self, ADecl.Identifier.Block) then begin

      end
      else begin // errors

      end;
    end
    else
      //ConsoleWriteln('Import not found_____: "' + token.Value + '"');
      raise NPCSyntaxError.ParserError(AToken.Location, Format(sParserImportNotFound, [AToken.Value]));
  end;
end;

procedure TNPCSourceParser.ParseImports(const ABlock: TNPC_ASTBlock);
var
  token: TNPCToken;
  project_path: String;
  decl: TNPC_ASTDeclaration;
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
        decl := TNPC_ASTDeclaration.Create;
        decl.Location := token.Location.Copy;
        decl.Identifier := TNPC_ASTIdentifier.Create;
        decl.Identifier.Location := decl.Location;
        //decl.Identifier.ResolvedDeclaration
        decl.Identifier.ParentDeclaration := TNPC_ASTDeclaration(ABlock);
        decl.Identifier.Name := UTF8String(token.Value);
        //
        if token.&Type = tokString then // @TODO: search all .npc files for code name specified in token.Value
          //raise NPCSyntaxError.NotSupportedError(token.Location, Format(sParserImportNotFound, [token.Value]));
          SearchImportByCodeName(token, project_path, token.Value, decl)
        else
          SearchImportByFileName(token, project_path, token.Value + '.npc', decl);
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
  project_main: TNPC_ASTBlock;
begin
//  has_body := False;

  project_main := TNPC_ASTBlock.Create;
  project_main.Location := AToken.Location.Copy;
  project_main.Flags := project_main.Flags or LongWord(BLOCK_IsMainProcedure) or LongWord(BLOCK_DoesNotHaveResult);

  ASTree.AddBlock(project_main);
  while Texer.IsNotEmpty do begin
    SkipComments;
    //
    project_main.AddStatement(ParseStatement([stafEmptyStatementIsAcceptable], project_main));
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
    end
    else
      raise NPCSyntaxError.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'finalization ', sSection]));
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
        IncLevel;
      end
      else if TokenIsReservedIdent(token, ri_begin) then begin
        IncLevel;
      end
      else if TokenIsReservedIdent(token, ri_break) then begin
        DecLevel;
      end
      else if TokenIsReservedIdent(token, ri_case) then begin
        IncLevel;
        ParseCase(token);
        if FLevel > ALevel then begin
          DecLevel;
//          Break;
        end;
        Continue;
      end
//      else if TokenIsReservedIdent(token, ri_end) then begin
//        DecLevel;
//      end
      else if TokenIsReservedIdent(token, ri_for) then begin
        IncLevel;
        ParseFor(token);
        if FLevel > ALevel then begin
          DecLevel;
//          Break;
        end;
        Continue;
      end
      else if TokenIsReservedIdent(token, ri_goto) then begin
      end
      else if TokenIsReservedIdent(token, ri_if) then begin
        IncLevel;
        ParseIf(token);
        if FLevel > ALevel then begin
          DecLevel;
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
        IncLevel;
      end
      else if TokenIsReservedIdent(token, ri_try) then begin
        IncLevel;
      end
      else if TokenIsReservedIdent(token, ri_while) then begin
        IncLevel;
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
        ASTree := TNPC_ASTBlock.Create;
        TNPC_ASTBlock(ASTree).Location := token.Location.Copy;
        TNPC_ASTBlock(ASTree).Flags := LongWord(BLOCK_ConsistsOfOrderedStatements);

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
        ParseProjectBody(TNPC_ASTBlock(ASTree));
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
        ASTree := TNPC_ASTBlock.Create;
        TNPC_ASTBlock(ASTree).Location := token.Location.Copy;
        TNPC_ASTBlock(ASTree).Flags := LongWord(BLOCK_ConsistsOfOrderedStatements);

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
        ParseCodeBody(TNPC_ASTBlock(ASTree));
        Result := True;
      end;
    finally
      FreeAndNil(Texer);
    end;
  finally
    FreeAndNil(Tokenizer);
  end;
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

end.

