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
  npc_utils,
  npc_error;

type
  TNPCSettingType = (
    setProgram,
    setProgramType,
    setResources
  );

  TNPCImportType = (itCode, itExternalObject, itDLL);

  TNPCImport = packed record
    &Type: TNPCImportType;
    Name: String;
    Path: String;
    Resolved: Boolean;
  end;
  TNPCImportArray = Array of TNPCImport;

  NPCSourceParserException = class(TNPCError);

  TNPCSourceParser = class
  private
    Tokenizer: TNPCTokenizer;
    Texer: TNPCTokensParser;
    TokensArray: TNPCTokens;
    Settings: Pointer;
    Imports: TNPCImportArray;
    FIndex: UInt64;
    FLevel: Integer;
  protected
    function Unescape(const Value: String): String;
    procedure IncLevel; inline;
    procedure DecLevel; inline;
    procedure Clear;
    procedure Grow;
    procedure Trim;
    procedure AddToken(const AToken: TNPCToken);
    procedure AddImport(const AImportType: TNPCImportType; const AImportName: String; const AImportPath: String);
    function TokenIsReservedIdent(const AToken: TNPCToken; const AReservedIdent: TNPCReservedIdents): Boolean; overload;
    function TokenIsReservedIdent(const AToken: TNPCToken): Boolean; overload;
    function TokenIsLiteral(const AToken: TNPCToken): Boolean;
    function TokenIsBiLiteral(const AToken: TNPCToken): Boolean;
    function TokenIsReservedSymbol(const AToken: TNPCToken; const AReservedSymbol: TNPCReservedSymbols): Boolean; overload;
    function TokenIsReservedSymbol(const AToken: TNPCToken): Boolean; overload;
    function LastToken: TNPCToken;
    procedure SkipComments;
    function SkipComment(const AToken: TNPCToken; const ConsumeToken: Boolean = False): Boolean;
    //
    procedure ParseProjectBody;
    procedure ParseSettingDefineCondition;
    procedure ParseSettingOrDefineDirective;
    procedure ParseSettings(const AToken: TNPCToken; const ASettingType: TNPCSettingType);
    procedure ParseSettingProgram(const AToken: TNPCToken);
    procedure ParseSettingProgramType(AToken: TNPCToken);

    procedure ParseDefines();
    procedure ParseDirectives();
    procedure ParseComment;

    procedure ParseAssignment(const AToken: TNPCToken);
    procedure ParseExpression;
    procedure ParseExpressionSimpleExpression;
    procedure ParseExpressionTerm;
    procedure ParseExpressionFactor;
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

    procedure ParseImports(const AToken: TNPCToken);
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
    constructor Create(const PSettings: Pointer);
    destructor Destroy; override;
    //
    procedure ParseProject;
    procedure ParseImportFile(const ASourceFile: String); overload;
    procedure ParseImportFile(const ASourceFile: TStringStream); overload;
    procedure ParseSourceCode(const ASourceCode: String); overload;
    procedure ParseSourceCode(const ASourceCode: TStringStream); overload;
  end;

implementation

uses
  StrUtils,
  npc_consts,
  npc_project,
  npc_md5,
  npc_tokens,
  npc_types;

{ TNPCSourceParser }

constructor TNPCSourceParser.Create(const PSettings: Pointer);
begin
  Settings := PSettings;
  Tokenizer := Nil;
  Texer := Nil;
  SetLength(TokensArray, 0);
  FIndex := 0;
  FLevel := 0;
end;

destructor TNPCSourceParser.Destroy;
begin
  Clear;
//  FreeAndNil(Texer);
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
  SetLength(TokensArray, 0);
  FIndex := 0;
end;

procedure TNPCSourceParser.Grow;
var
  size: UInt64;
begin
  size := Length(TokensArray);
  SetLength(TokensArray, size + 100);
end;

procedure TNPCSourceParser.Trim;
begin
  SetLength(TokensArray, FIndex);
end;

procedure TNPCSourceParser.AddToken(const AToken: TNPCToken);
var
  size: UInt64;
begin
  size := Length(TokensArray);
  if FIndex = size then
    Grow;
  //
  TokensArray[FIndex] := AToken;
  Inc(FIndex);
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

function TNPCSourceParser.TokenIsReservedIdent(const AToken: TNPCToken; const AReservedIdent: TNPCReservedIdents): Boolean;
begin
  Result := (AToken.&Type = tokIdent) and AToken.ReservedWord and SameText(AToken.Value, NPCReservedIdentifiers[AReservedIdent].Ident);
end;

function TNPCSourceParser.TokenIsReservedIdent(const AToken: TNPCToken): Boolean;
begin
  Result := (AToken.&Type = tokIdent) and AToken.ReservedWord;
end;

function TNPCSourceParser.TokenIsLiteral(const AToken: TNPCToken): Boolean;
begin
  Result := (AToken.&Type in [tokOParen..tokDiv]) and AToken.ReservedSymbol;
end;

function TNPCSourceParser.TokenIsBiLiteral(const AToken: TNPCToken): Boolean;
begin
  Result := (AToken.&Type in [tokAssign..tokNotEqual]) and AToken.ReservedSymbol;
end;

function TNPCSourceParser.TokenIsReservedSymbol(const AToken: TNPCToken; const AReservedSymbol: TNPCReservedSymbols): Boolean;
begin
  Result := (AToken.&Type in [tokOParen..tokDiv]) and AToken.ReservedSymbol and (AToken.Value = NPCReservedSymbolToString(AReservedSymbol));
  if not Result then
    Result := (AToken.&Type in [tokCommentSL..tokAssign]) and AToken.ReservedSymbol and StartsStr(NPCReservedSymbolToString(AReservedSymbol), AToken.Value);
end;

function TNPCSourceParser.TokenIsReservedSymbol(const AToken: TNPCToken): Boolean;
begin
  Result := (AToken.&Type = tokIdent) and AToken.ReservedSymbol;
end;

function TNPCSourceParser.LastToken: TNPCToken;
var
  cnt: Integer;
begin
  Result := Nil;
  cnt := Length(TokensArray);
  if FIndex < cnt then
    Result := TokensArray[FIndex - 1];
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
  if TokenIsReservedSymbol(Atoken, rs_CommentMLB2) then begin // multi-line comment
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
  if TokenIsReservedSymbol(Atoken, rs_CommentMLB1) then begin // multi-line comment
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
end;

procedure TNPCSourceParser.ParseProjectBody;
var
  token: TNPCToken;
begin
  while Texer.IsNotEmpty do begin
    SkipComments;
    token := Texer.GetToken; // add relevant tokens
    AddToken(token);
    if TokenIsReservedSymbol(token, rs_OCurly) then begin
      if Texer.IsCurrentSymbol('$') then
        ParseSettingDefineCondition
      else if Texer.IsCurrentSymbol('@') then
        raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]))
      else
        ParseComment;
    end
    else if TokenIsReservedIdent(token, ri_imports) then begin
      ParseImports(token);
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
      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
  end;
end;

procedure TNPCSourceParser.ParseSettingDefineCondition;
var
  token: TNPCToken;
begin
  while Texer.IsNotEmpty do begin
    token := Texer.PeekToken;
    if TokenIsReservedSymbol(token, rs_Dollar) then begin
      Texer.SkipToken;
      ParseSettingOrDefineDirective;
    end
    else if TokenIsReservedSymbol(token, rs_CCurly) then begin
      AddToken(token);
      Texer.SkipToken;
      Break;
    end
    else
      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
  end;
end;

procedure TNPCSourceParser.ParseSettingOrDefineDirective;
var
  token: TNPCToken;
begin
  while Texer.IsNotEmpty do begin
    token := Texer.GetTokenWithMinus;
    try
      if (token.&Type = tokIdent) and SameText(token.Value, 'program-type') then begin
        ParseSettings(token, setProgramType);
        Break;
      end
//      else if TokenIsReservedSymbol(token, '}') then begin
//        Break;
//      end
      else
        raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
    finally
      token.Free;
    end;
  end;
end;

procedure TNPCSourceParser.ParseSettings(const AToken: TNPCToken; const ASettingType: TNPCSettingType);
begin
  case ASettingType of
    setProgram: ParseSettingProgram(AToken);
    setProgramType: ParseSettingProgramType(AToken);
    setResources: ;
  end;
end;

procedure TNPCSourceParser.ParseSourceCode(const ASourceCode: TStringStream);
begin

end;

procedure TNPCSourceParser.ParseSourceCode(const ASourceCode: String);
begin

end;

procedure TNPCSourceParser.ParseSettingProgram(const AToken: TNPCToken);
var
  token: TNPCToken;
begin
  token := Texer.GetToken;
  try
//    if (token.&Type = tokIdent) and SameText(token.Value, 'type') then begin
//      ParseSettingProgramType(AToken);
//    end
//    else
      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
  finally
    token.Free;
  end;
end;

procedure TNPCSourceParser.ParseSettingProgramType(AToken: TNPCToken);

  function EnsureTypeIsNotSet(BToken: TNPCToken; const CurrentProjectTypes: TNPCProjectTypes; ProjectTypes: TNPCProjectTypes): Boolean;
  begin
    Result := (CurrentProjectTypes * ProjectTypes = []);
    if Result then
      Exit;
    //
    raise NPCSourceParserException.ParserError(BToken.Location, Format(sParserUnexpectedTokenIn, [BToken.TokenToString, '', sProjectFile]));
  end;

  procedure AddProjectType(Settings: TNPCProjectSettings; const OutputPath: String; const ProjectTypes: TNPCProjectTypes; const ProjectExtension: String);
  var
    idx: Integer;
  begin
    idx := Length(Settings.OutputTypes);
    SetLength(Settings.OutputTypes, idx + 1);
    //
    Settings.OutputTypes[idx].OutputPath := OutputPath;
    Settings.OutputTypes[idx].OutputStream := Nil;
    Settings.OutputTypes[idx].ProjectTypes := ProjectTypes;
    Settings.OutputTypes[idx].ProjectExtension := ProjectExtension;
  end;

  function ProjectTypeToString(const ProjectTypes: TNPCProjectTypes): String;
  begin
    Result := '';
    if ptWindows in ProjectTypes then
      Result := Result + 'Windows'
    else if ptLinux in ProjectTypes then
      Result := Result + 'Linux'
    else if ptAndroid in ProjectTypes then
      Result := Result + 'Android'
    else if ptWebAssembly in ProjectTypes then
      Result := Result + 'WebAssembly';

    if Length(Result) > 0 then
      Result := Result + ' ';

    if pt32Bit in ProjectTypes then
      Result := Result + '32 bit'
    else if pt64Bit in ProjectTypes then
      Result := Result + '64 bit';

    if Length(Result) > 0 then
      Result := Result + ' ';

    if ptCONSOLE in ProjectTypes then
      Result := Result + 'CONSOLE'
    else if ptGUI in ProjectTypes then
      Result := Result + 'GUI'
    else if ptDLL in ProjectTypes then
      Result := Result + 'DLL'
    else if ptTEXT in ProjectTypes then
      Result := Result + 'TEXT';

    Result := SysUtils.Trim(Result);
  end;

  function ProjectTypeToIdent(const ProjectTypes: TNPCProjectTypes; const ProjectExtension: String; const OutputPath: String): String;
  begin
    Result := '';
    if ptWindows in ProjectTypes then
      Result := Result + 'Windows'
    else if ptLinux in ProjectTypes then
      Result := Result + 'Linux'
    else if ptAndroid in ProjectTypes then
      Result := Result + 'Android'
    else if ptWebAssembly in ProjectTypes then
      Result := Result + 'WebAssembly';

    Result := Result + ';';

    if pt32Bit in ProjectTypes then
      Result := Result + '32'
    else if pt64Bit in ProjectTypes then
      Result := Result + '64';

    Result := Result + ';';

    if ptCONSOLE in ProjectTypes then
      Result := Result + 'CONSOLE'
    else if ptGUI in ProjectTypes then
      Result := Result + 'GUI'
    else if ptDLL in ProjectTypes then
      Result := Result + 'DLL'
    else if ptTEXT in ProjectTypes then
      Result := Result + 'TEXT';

    Result := Result + ';';
    Result := Result + ProjectExtension;
    Result := Result + ';';
    Result := Result + OutputPath;
  end;

var
  token: TNPCToken;
  stemp: String;
  //
  OutputPath: String;
  ProjectTypes: TNPCProjectTypes;
  ProjectExtension: String;
begin
  Assert(Assigned(AToken), 'no token passed');
  AddToken(TNPCToken.Create(tokSetting, AToken.Location.Copy, False, False, '$program-type', EmptyTokenMD5));
  //
  OutputPath := '';
  ProjectTypes := [];
  ProjectExtension := '';
  //
  stemp := '';
  //
  while Texer.IsNotEmpty do begin
    token := Texer.PeekToken;
    if (token.&Type = tokIdent) then begin
      Texer.SkipToken;
      if SameText(token.Value, 'Windows32') and EnsureTypeIsNotSet(token, ProjectTypes, [ptWindows, ptLinux, ptAndroid, ptWebAssembly, pt32Bit, pt64Bit]) then begin
        ProjectTypes := ProjectTypes + [ptWindows];
        ProjectTypes := ProjectTypes + [pt32Bit];
      end
      else if SameText(token.Value, 'Windows64') and EnsureTypeIsNotSet(token, ProjectTypes, [ptWindows, ptLinux, ptAndroid, ptWebAssembly, pt32Bit, pt64Bit]) then begin
        ProjectTypes := ProjectTypes + [ptWindows];
        ProjectTypes := ProjectTypes + [pt64Bit];
      end
      else if SameText(token.Value, 'Linux32') and EnsureTypeIsNotSet(token, ProjectTypes, [ptWindows, ptLinux, ptAndroid, ptWebAssembly, pt32Bit, pt64Bit]) then begin
        ProjectTypes := ProjectTypes + [ptLinux];
        ProjectTypes := ProjectTypes + [pt32Bit];
      end
      else if SameText(token.Value, 'Linux64') and EnsureTypeIsNotSet(token, ProjectTypes, [ptWindows, ptLinux, ptAndroid, ptWebAssembly, pt32Bit, pt64Bit]) then begin
        ProjectTypes := ProjectTypes + [ptLinux];
        ProjectTypes := ProjectTypes + [pt64Bit];
      end
      else if SameText(token.Value, 'Android32') and EnsureTypeIsNotSet(token, ProjectTypes, [ptWindows, ptLinux, ptAndroid, ptWebAssembly, pt32Bit, pt64Bit]) then begin
        ProjectTypes := ProjectTypes + [ptAndroid];
        ProjectTypes := ProjectTypes + [pt32Bit];
      end
      else if SameText(token.Value, 'Android64') and EnsureTypeIsNotSet(token, ProjectTypes, [ptWindows, ptLinux, ptAndroid, ptWebAssembly, pt32Bit, pt64Bit]) then begin
        ProjectTypes := ProjectTypes + [ptAndroid];
        ProjectTypes := ProjectTypes + [pt64Bit];
      end
      else if SameText(token.Value, 'WebAssembly') and EnsureTypeIsNotSet(token, ProjectTypes, [ptWindows, ptLinux, ptAndroid, ptWebAssembly, pt32Bit, pt64Bit]) then begin
        ProjectTypes := ProjectTypes + [ptWebAssembly];
        ProjectTypes := ProjectTypes + [pt32Bit];
      end
//      else if SameText(token.Value, 'WebAssembly64') and EnsureTypeIsNotSet(token, ProjectTypes, [ptWindows, ptLinux, ptAndroid, ptWebAssembly, pt32Bit, pt64Bit]) then begin
//        ProjectTypes := ProjectTypes + [ptWebAssembly];
//        ProjectTypes := ProjectTypes + [pt64Bit];
//      end
      else if SameText(token.Value, 'CONSOLE') and EnsureTypeIsNotSet(token, ProjectTypes, [ptCONSOLE, ptGUI, ptDLL, ptTEXT]) then
        ProjectTypes := ProjectTypes + [ptCONSOLE]
      else if SameText(token.Value, 'GUI') and EnsureTypeIsNotSet(token, ProjectTypes, [ptCONSOLE, ptGUI, ptDLL, ptTEXT]) then
        ProjectTypes := ProjectTypes + [ptGUI]
      else if SameText(token.Value, 'DLL') and EnsureTypeIsNotSet(token, ProjectTypes, [ptCONSOLE, ptGUI, ptDLL, ptTEXT]) then
        ProjectTypes := ProjectTypes + [ptDLL]
      else if SameText(token.Value, 'TEXT') and EnsureTypeIsNotSet(token, ProjectTypes, [ptCONSOLE, ptGUI, ptDLL, ptTEXT]) then
        ProjectTypes := ProjectTypes + [ptTEXT]
      else
        raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
    end
    else if (token.&Type = tokString) then begin
      Texer.SkipToken;
      if (Length(token.Value) > 1) and (token.Value[1] = '.') and (Length(ProjectExtension) = 0) then begin // extension
        ProjectExtension := token.Value;
      end
      else if (Length(token.Value) > 0) and (Length(OutputPath) = 0) then begin
        OutputPath := token.Value;
      end
      else
        raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
    end
    else if TokenIsReservedSymbol(token, rs_CCurly) then begin
      AddProjectType(TNPCProjectSettings(Settings^), OutputPath, ProjectTypes, ProjectExtension);
      AddToken(TNPCToken.Create(tokIdent, AToken.Location.Copy, False, False, ProjectTypeToIdent(ProjectTypes, ProjectExtension, OutputPath), EmptyTokenMD5));
//      AddToken(token);
//      Texer.SkipToken;
      stemp := ProjectTypeToString(ProjectTypes);
      ConsoleWriteln('Compilation target : ' + stemp + IfThen(Length(ProjectExtension) > 0, ' (extension: ' + ProjectExtension + ')'));
      ConsoleWriteln('        output path: ' + OutputPath);
      stemp := '';
      OutputPath := '';
      ProjectTypes := [];
      ProjectExtension := '';
      Break;
    end
    else
      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
  end;
end;

procedure TNPCSourceParser.ParseDefines;
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
    AddToken(token);
    if TokenIsReservedSymbol(token, rs_Dollar) then begin
    end
    else if TokenIsReservedSymbol(token, rs_At) then begin
    end
    else if TokenIsReservedSymbol(token, rs_CCurly) then begin
      Break;
    end
    else if TokenIsReservedIdent(token, ri_imports) then begin
      ParseImports(token);
    end
    else
      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
  end;
end;

// assignment = ident [param_list] ":=" expr
//
// param_list = ('(' | '[') ident { ',' ident } (')' | ']')

procedure TNPCSourceParser.ParseAssignment(const AToken: TNPCToken);
var
  token: TNPCToken;
begin
  AddToken(AToken); // ':='
  Texer.SkipToken;
  ParseExpression; // get everything until ';'
  AddToken(Texer.ExpectToken([tokSemicolon]));
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

procedure TNPCSourceParser.ParseExpression;
var
  token: TNPCToken;
begin
  ParseExpressionSimpleExpression;
  while Texer.IsNotEmpty do begin
    token := Texer.PeekToken;
    if token.ReservedSymbol and (token.&Type in [tokEqual, tokNotEqual, tokLessThan, tokLessEqual, tokGreaterThan, tokGreaterEqual]) then begin
      AddToken(token);
      Texer.SkipToken;
      ParseExpressionSimpleExpression;
      Continue;
    end
    else if TokenIsReservedSymbol(token, rs_Semicolon) then
      Break
    else
      Break;
//      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'expression ', sStatement]));
    //
    Texer.SkipToken;
  end;
end;

procedure TNPCSourceParser.ParseExpressionSimpleExpression;
var
  token: TNPCToken;
begin
  ParseExpressionTerm;
  while Texer.IsNotEmpty do begin
    token := Texer.PeekToken;
    if (token.ReservedSymbol and (token.&Type in [tokPlus, tokMinus])) or TokenIsReservedIdent(token, ri_or) or TokenIsReservedIdent(token, ri_xor) then begin
      AddToken(token);
      Texer.SkipToken;
      ParseExpressionTerm;
      Continue;
    end
    else if TokenIsReservedSymbol(token, rs_Semicolon) then
      Break
    else
      Break;
//      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'expression ', sStatement]));
    //
    Texer.SkipToken;
  end;
end;

procedure TNPCSourceParser.ParseExpressionTerm;
var
  token: TNPCToken;
begin
  ParseExpressionFactor;
  while Texer.IsNotEmpty do begin
    token := Texer.PeekToken;
    if (token.ReservedSymbol and (token.&Type in [tokAsterisk, tokDiv])) or
       TokenIsReservedIdent(token, ri_div) or
       TokenIsReservedIdent(token, ri_mod) or
       TokenIsReservedIdent(token, ri_and) or
       TokenIsReservedIdent(token, ri_shl) or
       TokenIsReservedIdent(token, ri_shr) then begin
      AddToken(token);
      Texer.SkipToken;
      ParseExpressionFactor;
      Continue;
    end
    else if TokenIsReservedSymbol(token, rs_Semicolon) then
      Break
    else
      Break;
//      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'expression ', sStatement]));
    //
    Texer.SkipToken;
  end;
end;

procedure TNPCSourceParser.ParseExpressionFactor;
var
  token: TNPCToken;
begin
  token := Texer.PeekToken;
  if not token.ReservedWord and (token.&Type in [tokIdent..tokChar]) then begin
    AddToken(token);
    Texer.SkipToken;
  end
  else if TokenIsReservedIdent(token, ri_nil) then begin
    AddToken(token);
    Texer.SkipToken;
  end
  else if TokenIsReservedIdent(token, ri_not) or TokenIsReservedIdent(token, ri_inherited) then begin
    AddToken(token);
    Texer.SkipToken;
    ParseExpressionFactor;
  end
  else if TokenIsReservedSymbol(token, rs_At) then begin
    AddToken(token);
    Texer.SkipToken;
    ParseExpressionFactor;
  end
  else if TokenIsReservedSymbol(token, rs_Dash) then begin
    AddToken(token);
    Texer.SkipToken;
    AddToken(Texer.ExpectToken([tokIdent]));
  end
  else if TokenIsReservedSymbol(token, rs_OBracket) then begin // set
    AddToken(token);
    Texer.SkipToken;
    while Texer.IsNotEmpty do begin
      token := Texer.PeekToken;
      //
      if not token.ReservedWord and (token.&Type = tokIdent) then begin
        AddToken(token);
      end
      else if TokenIsReservedSymbol(token, rs_Comma) or TokenIsReservedSymbol(token, rs_DoubleDot) then begin
        AddToken(token);
      end
      else if TokenIsReservedSymbol(token, rs_CBracket) then
        Break
      else
        raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sExpression]));
      //
      Texer.SkipToken;
    end;
    AddToken(Texer.ExpectToken([tokCBracket]));
  end
  else if TokenIsReservedSymbol(token, rs_OParen) then begin // call params
    AddToken(token);
    Texer.SkipToken;
    ParseExpression;
    while Texer.IsNotEmpty do begin
      token := Texer.PeekToken;
      //
      if not token.ReservedWord and (token.&Type = tokIdent) then begin
        AddToken(token);
      end
      else if TokenIsReservedSymbol(token, rs_Comma) or TokenIsReservedSymbol(token, rs_Dot) or TokenIsReservedSymbol(token, rs_Dash) then begin
        AddToken(token);
        Texer.SkipToken;
        ParseExpression;
        Continue;
      end
      else if TokenIsReservedIdent(token, ri_as) then begin
        AddToken(token);
        AddToken(Texer.ExpectToken([tokIdent]));
        Continue;
      end
      else if TokenIsReservedSymbol(token, rs_CParen) then
        Break
      else
        raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sExpression]));
      //
      Texer.SkipToken;
    end;
    AddToken(Texer.ExpectToken([tokCParen]));
  end
  else if (token.ReservedSymbol and (token.&Type in [tokPlus, tokMinus])) then begin
    AddToken(token);
    Texer.SkipToken;
    ParseExpressionFactor;
  end
  else if TokenIsReservedSymbol(token, rs_Semicolon) then
    Exit
  else
    raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'expression ', sStatement]));
end;

procedure TNPCSourceParser.ParseCallParams(const AToken: TNPCToken);
var
  token: TNPCToken;
begin
  AddToken(AToken); // '('
  Texer.SkipToken;
  //
  while Texer.IsNotEmpty do begin
    SkipComments;
    token := Texer.PeekToken;
    if not token.ReservedWord and not token.ReservedSymbol and (token.&Type in [tokIdent..tokChar]) then begin
      AddToken(token);
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
        raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'call params ', sSection]));
    end
    else if TokenIsReservedSymbol(token, rs_CParen) then begin
      AddToken(token);
      Texer.SkipToken; // move forward
      token := Texer.PeekToken; // just peek a token
      if TokenIsReservedSymbol(token, rs_Semicolon) then
        AddToken(Texer.GetToken);
      Break;
    end
//    else
//      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sStatement]));
    else
      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'call params ', sStatement]));
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
  AddToken(AToken); // 'if'
  Texer.SkipToken;
  ParseIfExpression; // get everything until 'then'
  AddToken(Texer.ExpectReservedToken(ri_then));
  ParseIfStatement;
  //
  SkipComments;
  token := Texer.PeekToken;
  if TokenIsReservedIdent(token, ri_else) then begin
    if (LastToken <> Nil) and TokenIsReservedSymbol(LastToken, rs_Semicolon) then
      raise NPCSourceParserException.ParserError(LastToken.Location, Format(sParserUnexpectedTokenIn, [LastToken.TokenToString, 'if ', sStatement]));
    AddToken(token);
    Texer.SkipToken;
    ParseIfStatement;
  end;
  if (LastToken <> Nil) and not TokenIsReservedSymbol(LastToken, rs_Semicolon) then
    raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
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
    AddToken(token);
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
    raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
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
      AddToken(token);
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
//      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
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
      AddToken(token);
      Texer.SkipToken;
      ParseIfFactor;
    end
    else if TokenIsReservedSymbol(token, rs_Semicolon) then
      Break
    else if TokenIsReservedIdent(token, ri_then) then
      Break
    else
      Break;
//      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
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
    AddToken(token);
    Texer.SkipToken;
  end
  else if TokenIsReservedIdent(token, ri_not) then begin
    AddToken(token);
    Texer.SkipToken;
    ParseIfFactor;
  end
  else if TokenIsReservedSymbol(token, rs_Percent) then begin
    AddToken(token);
    Texer.SkipToken;
    AddToken(Texer.ExpectToken([tokIdent], True));
    AddToken(Texer.ExpectToken([tokPercent]));
  end
  else if (token.ReservedSymbol and (token.&Type in [tokPlus, tokMinus])) then begin
    AddToken(token);
    Texer.SkipToken;
    ParseIfFactor;
  end
  else if TokenIsReservedSymbol(token, rs_OParen) then begin
    if (LastToken <> Nil) and not LastToken.ReservedWord and (LastToken.&Type = tokIdent) then
      ParseCallParams(token)
    else begin
      AddToken(token);
      Texer.SkipToken;
      while Texer.IsNotEmpty do begin
        ParseExpression;
        token := Texer.PeekToken;
        if TokenIsReservedSymbol(token, rs_Comma) then begin
          AddToken(token);
          Texer.SkipToken;
        end
        else if TokenIsReservedSymbol(token, rs_CParen) then begin
          Break;
        end;
//        else
//          raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
      end;
    end;
    AddToken(Texer.ExpectToken([tokCParen]));
  end
  else if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then
    Exit
  else if TokenIsReservedIdent(token, ri_then) then
    Exit
  else
    raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
end;

procedure TNPCSourceParser.ParseIfStatement;
var
  token: TNPCToken;
begin
  SkipComments;
  token := Texer.PeekToken;
  if not token.ReservedWord and (token.&Type = tokIdent) then begin
    AddToken(token);
    Texer.SkipToken;
    token := Texer.PeekToken;
    if TokenIsReservedSymbol(token, rs_CParen) then begin
      ParseIfStatementFuncParams(token);
      AddToken(Texer.ExpectToken([tokCParen]));
      AddToken(Texer.ExpectToken([tokSemicolon]));
    end
    else if TokenIsReservedSymbol(token, rs_Assign) then begin // assignment
      AddToken(token);
      Texer.SkipToken;
      ParseIfExpression;
      AddToken(Texer.ExpectToken([tokSemicolon]));
    end
    else if TokenIsReservedIdent(token, ri_if) then
      ParseIf(token)
    else if TokenIsReservedIdent(token, ri_begin) or TokenIsReservedSymbol(token, rs_OCurly) then begin
      AddToken(token);
      Texer.SkipToken;
      while Texer.IsNotEmpty do begin
        ParseIfStatement;
        token := Texer.PeekToken;
        if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then begin
          AddToken(token);
          Texer.SkipToken;
          Break;
        end
        else
          raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
      end;
      AddToken(Texer.ExpectToken([tokSemicolon]));
    end
    else
      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
  end
  else if TokenIsReservedIdent(token, ri_begin) or TokenIsReservedSymbol(token, rs_OCurly) then begin
    AddToken(token);
    Texer.SkipToken;
    while Texer.IsNotEmpty do begin
      token := Texer.PeekToken;
      if not ParseStatements(token, [], FLevel + 1) then begin
        if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then begin
          AddToken(token);
          Texer.SkipToken;
          token := Texer.PeekToken;
          if TokenIsReservedSymbol(token, rs_Semicolon) then
            AddToken(token);
          Break;
        end
        else if TokenIsReservedSymbol(token, rs_Semicolon) then begin
          AddToken(token);
//          Texer.SkipToken;
          Break;
        end
        else
          raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
      end
      else if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then begin
        AddToken(token);
        Texer.SkipToken;
        token := Texer.PeekToken;
        if TokenIsReservedSymbol(token, rs_Semicolon) then
          AddToken(token);
        Break;
      end;
    end;
    //AddToken(Texer.ExpectToken([tokSemicolon]));
  end
  else if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then
    Exit
  else
    raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
end;

procedure TNPCSourceParser.ParseIfStatementFuncParams(const AToken: TNPCToken);
var
  token: TNPCToken;
begin
  AddToken(token);
  Texer.SkipToken;
  while Texer.IsNotEmpty do begin
    token := Texer.PeekToken;
    if not token.ReservedWord and (token.&Type = tokIdent) then begin
      AddToken(token);
      Texer.SkipToken;
      token := Texer.PeekToken;
      if TokenIsReservedSymbol(token, rs_CParen) then begin
        ParseIfStatementFuncParams(token);
        AddToken(Texer.ExpectToken([tokCParen]));
        AddToken(Texer.ExpectToken([tokSemicolon]));
      end
      else if token.&Type in [tokNumber, tokString] then begin
        AddToken(token);
        Texer.SkipToken;
      end
      else if TokenIsReservedSymbol(token, rs_Comma) then begin
        AddToken(token);
        Texer.SkipToken;
      end
      else
        raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
    end
    else if TokenIsReservedSymbol(token, rs_CParen) then
      Break
    else
      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
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
  AddToken(AToken); // 'case'
  Texer.SkipToken;
  ParseCaseExpression; // get everything until 'of'
  token := Texer.GetToken;
  case_of := TokenIsReservedIdent(token, ri_of); // and not TokenIsReservedSymbol(token, rs_OCurly);
  if not TokenIsReservedIdent(token, ri_of) and not TokenIsReservedSymbol(token, rs_OCurly) then
    raise NPCSourceParserException.ParserError(LastToken.Location, Format(sParserUnexpectedTokenIn, [LastToken.TokenToString, 'case ', sStatement]));
  AddToken(token);
  ParseCaseElements(case_of);
  //
//  SkipComments;
//    if (LastToken <> Nil) and TokenIsReservedSymbol(LastToken, rs_Semicolon) then
//  token := LastToken;
  token := Texer.GetToken;
  if (case_of and not TokenIsReservedIdent(token, ri_end)) or (not case_of and not TokenIsReservedSymbol(token, rs_CCurly)) then
    raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'case ', sStatement]));
  AddToken(token);
  token := Texer.GetToken;
  if not TokenIsReservedSymbol(token, rs_Semicolon) then
    raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'case ', sStatement]));
//  AddToken(Texer.ExpectToken([tokSemicolon]));
  AddToken(token);
end;

procedure TNPCSourceParser.ParseCaseExpression;
var
  token: TNPCToken;
begin
  SkipComments;
  ParseCaseSimpleExpression;
  token := Texer.PeekToken;
  if token.ReservedSymbol and (token.&Type in [tokEqual, tokNotEqual, tokLessThan, tokLessEqual, tokGreaterThan, tokGreaterEqual]) then begin
    AddToken(token);
    Texer.SkipToken;
    ParseCaseSimpleExpression;
    Exit;
  end
  else if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then
    Exit
  else if TokenIsReservedSymbol(token, rs_Semicolon) then
    Exit
  else if TokenIsReservedIdent(token, ri_of) then
    Exit
  else
    raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'case ', sStatement]));
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
      AddToken(token);
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
      AddToken(token);
      Texer.SkipToken;
      ParseCaseFactor;
    end
    else if TokenIsReservedSymbol(token, rs_Semicolon) then
      Break
    else if TokenIsReservedIdent(token, ri_of) then
      Break
    else
      Break;
//      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
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
    AddToken(token);
    Texer.SkipToken;
  end
  else if TokenIsReservedIdent(token, ri_not) then begin
    AddToken(token);
    Texer.SkipToken;
    ParseCaseFactor;
  end
  else if TokenIsReservedSymbol(token, rs_Percent) then begin
    AddToken(token);
    Texer.SkipToken;
    AddToken(Texer.ExpectToken([tokIdent], True));
    AddToken(Texer.ExpectToken([tokPercent]));
  end
  else if (token.ReservedSymbol and (token.&Type in [tokPlus, tokMinus])) then begin
    AddToken(token);
    Texer.SkipToken;
    ParseCaseFactor;
  end
  else if TokenIsReservedSymbol(token, rs_CParen) then begin
    if (LastToken <> Nil) and not LastToken.ReservedWord and (LastToken.&Type = tokIdent) then
      ParseCallParams(token)
    else begin
      AddToken(token);
      Texer.SkipToken;
      while Texer.IsNotEmpty do begin
        ParseExpression;
        token := Texer.PeekToken;
        if TokenIsReservedSymbol(token, rs_Comma) then begin
          AddToken(token);
          Texer.SkipToken;
        end
        else if TokenIsReservedSymbol(token, rs_CParen) then begin
          Break;
        end;
      end;
    end;
    AddToken(Texer.ExpectToken([tokCParen]));
  end
  else if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then
    Exit
  else if TokenIsReservedIdent(token, ri_of) then
    Exit
  else
    raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'case ', sStatement]));
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
      AddToken(token); // 'if'
      Texer.SkipToken;
      ParseCaseIfExpression; // get everything until ':'
      AddToken(Texer.ExpectReservedSymbol(rs_Colon));
      ParseCaseIfStatement;
//      raise Exception.Create('Error Message');
    end
    else if TokenIsReservedSymbol(token, rs_Semicolon) then
      Break
    else if (case_of and not TokenIsReservedIdent(token, ri_end)) or (not case_of and not TokenIsReservedSymbol(token, rs_CCurly)) then
      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'case ', sStatement]))
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
//      raise NPCSourceParserException.ParserError(LastToken.Location, Format(sParserUnexpectedTokenIn, [LastToken.TokenToString, 'if ', sStatement]));
//    AddToken(token);
//    Texer.SkipToken;
//    ParseIfStatement;
//  end;
//  if (LastToken <> Nil) and not TokenIsReservedSymbol(LastToken, rs_Semicolon) then
//    raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
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
    AddToken(token);
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
    raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
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
      AddToken(token);
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
//      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
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
      AddToken(token);
      Texer.SkipToken;
      ParseCaseIfFactor;
    end
    else if TokenIsReservedSymbol(token, rs_Semicolon) then
      Break
    else if TokenIsReservedSymbol(token, rs_Colon) then
      Break
    else
      Break;
//      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
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
    AddToken(token);
    Texer.SkipToken;
  end
  else if TokenIsReservedIdent(token, ri_not) then begin
    AddToken(token);
    Texer.SkipToken;
    ParseCaseIfFactor;
  end
  else if TokenIsReservedSymbol(token, rs_Percent) then begin
    AddToken(token);
    Texer.SkipToken;
    AddToken(Texer.ExpectToken([tokIdent], True));
    AddToken(Texer.ExpectToken([tokPercent]));
  end
  else if (token.ReservedSymbol and (token.&Type in [tokPlus, tokMinus])) then begin
    AddToken(token);
    Texer.SkipToken;
    ParseCaseIfFactor;
  end
  else if TokenIsReservedSymbol(token, rs_OParen) then begin
    if (LastToken <> Nil) and not LastToken.ReservedWord and (LastToken.&Type = tokIdent) then
      ParseCallParams(token)
    else begin
      AddToken(token);
      Texer.SkipToken;
      while Texer.IsNotEmpty do begin
        ParseExpression;
        token := Texer.PeekToken;
        if TokenIsReservedSymbol(token, rs_Comma) then begin
          AddToken(token);
          Texer.SkipToken;
        end
        else if TokenIsReservedSymbol(token, rs_CParen) then begin
          Break;
        end;
//        else
//          raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
      end;
    end;
    AddToken(Texer.ExpectToken([tokCParen]));
  end
  else if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then
    Exit
  else if TokenIsReservedIdent(token, ri_then) then
    Exit
  else
    raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
end;

procedure TNPCSourceParser.ParseCaseIfStatement;
var
  token: TNPCToken;
begin
  SkipComments;
  token := Texer.PeekToken;
  if not token.ReservedWord and (token.&Type = tokIdent) then begin
    AddToken(token);
    Texer.SkipToken;
    token := Texer.PeekToken;
    if TokenIsReservedSymbol(token, rs_CParen) then begin
      ParseCaseIfStatementFuncParams(token);
      AddToken(Texer.ExpectToken([tokCParen]));
      AddToken(Texer.ExpectToken([tokSemicolon]));
    end
    else if TokenIsReservedSymbol(token, rs_Assign) then begin // assignment
      AddToken(token);
      Texer.SkipToken;
      ParseCaseIfExpression;
      AddToken(Texer.ExpectToken([tokSemicolon]));
    end
    else if TokenIsReservedIdent(token, ri_if) then
      ParseIf(token)
    else if TokenIsReservedIdent(token, ri_begin) or TokenIsReservedSymbol(token, rs_OCurly) then begin
      AddToken(token);
      Texer.SkipToken;
      while Texer.IsNotEmpty do begin
        ParseCaseIfStatement;
        token := Texer.PeekToken;
        if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then begin
          AddToken(token);
          Texer.SkipToken;
          Break;
        end
        else
          raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
      end;
      AddToken(Texer.ExpectToken([tokSemicolon]));
    end
    else
      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
  end
  else if TokenIsReservedIdent(token, ri_begin) or TokenIsReservedSymbol(token, rs_OCurly) then begin
    AddToken(token);
    Texer.SkipToken;
    while Texer.IsNotEmpty do begin
      token := Texer.PeekToken;
      if not ParseStatements(token, [], FLevel + 1) then begin
        if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then begin
          AddToken(token);
          Texer.SkipToken;
          token := Texer.PeekToken;
          if TokenIsReservedSymbol(token, rs_Semicolon) then
            AddToken(token);
          Break;
        end
        else if TokenIsReservedSymbol(token, rs_Semicolon) then begin
          AddToken(token);
          Texer.SkipToken;
          Break;
        end
        else
          raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
      end
      else if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then begin
        AddToken(token);
        Texer.SkipToken;
        token := Texer.PeekToken;
        if TokenIsReservedSymbol(token, rs_Semicolon) then
          AddToken(token);
        Break;
      end;
    end;
    //AddToken(Texer.ExpectToken([tokSemicolon]));
  end
  else if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) then
    Exit
  else
    raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
end;

procedure TNPCSourceParser.ParseCaseIfStatementFuncParams(const AToken: TNPCToken);
var
  token: TNPCToken;
begin
  AddToken(token);
  Texer.SkipToken;
  while Texer.IsNotEmpty do begin
    token := Texer.PeekToken;
    if not token.ReservedWord and (token.&Type = tokIdent) then begin
      AddToken(token);
      Texer.SkipToken;
      token := Texer.PeekToken;
      if TokenIsReservedSymbol(token, rs_CParen) then begin
        ParseCaseIfStatementFuncParams(token);
        AddToken(Texer.ExpectToken([tokCParen]));
        AddToken(Texer.ExpectToken([tokSemicolon]));
      end
      else if token.&Type in [tokNumber, tokString] then begin
        AddToken(token);
        Texer.SkipToken;
      end
      else if TokenIsReservedSymbol(token, rs_Comma) then begin
        AddToken(token);
        Texer.SkipToken;
      end
      else
        raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
    end
    else if TokenIsReservedSymbol(token, rs_CParen) then
      Break
    else
      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'if ', sStatement]));
    //
    Texer.SkipToken;
  end;
end;

(*
  while Texer.IsNotEmpty do begin
    token := Texer.PeekToken;


    if token.&Type in [tokIdent..tokString, tokExclamation..tokDash, tokAsterisk..tokDiv, tokAssign..tokNotEqual] then begin
      AddToken(token);
      Texer.SkipToken;
//      if True then
    end
    else if TokenIsReservedSymbol(token, ';') then begin
      AddToken(token);
      Texer.SkipToken;
      Break;
    end
    else
      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenInProject, [token.TokenToString]));
    //
    Texer.SkipToken;
  end;



  while Texer.IsNotEmpty do begin
    case token.&Type of
      tokIdent: begin

      end;
      tokNumber: begin

      end;
      tokPlus: begin

      end;
      tokMinus: begin

      end;
      tokOParen: begin
        AddToken(token);
        Texer.SkipToken;
        ParseExpression;
        AddToken(Texer.ExpectToken([tokCParen]));
      end;
    end;


    if token.&Type in [tokIdent..tokString, tokExclamation..tokDash, tokAsterisk..tokDiv, tokAssign..tokNotEqual] then begin
      AddToken(token);
      Texer.SkipToken;
//      if True then


    end
    else if TokenIsReservedSymbol(token, ';') then begin
      AddToken(token);
      Texer.SkipToken;
      Break;
    end
//    else if TokenIsReservedSymbol(token, '}') then begin
//      Break;
//    end
//    else if TokenIsReservedIdent(token) then begin
//      Break;
//    end
    else
      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenInProject, [token.TokenToString]));
    //
    Texer.SkipToken;
  end;
*)

procedure TNPCSourceParser.ParseImportFile(const ASourceFile: String);
begin

end;

procedure TNPCSourceParser.ParseImportFile(const ASourceFile: TStringStream);
begin

end;

procedure TNPCSourceParser.ParseImports(const AToken: TNPCToken);
var
  token: TNPCToken;
  path: String;
begin
  path := ExtractFilePath(TNPCProjectSettings(Settings^).InputPath);
  try
    while Texer.IsNotEmpty do begin
      SkipComments;
      token := Texer.GetToken;
      if TokenIsReservedSymbol(token, rs_Comma) then begin
        AddToken(token);
      end
      else if TokenIsReservedSymbol(token, rs_Semicolon) then begin
        AddToken(token);
        Break;
      end
      else if (token.&Type = tokIdent) or (token.&Type = tokString) then begin
        if FileExists(path + IfThen(Pos('.', token.Value) = 0, token.Value + '.npc', token.Value)) then begin
          AddToken(token);
          AddImport(itCode, token.Value, path);
          //ConsoleWriteln('Target project type: ' + stemp);
          ConsoleWriteln('Import_____________: "' + token.Value + '"');
        end
        else
          ConsoleWriteln('Import not found___: "' + token.Value + '"');
         //TNPCProjectSettings(Settings^).ProjectName := token.Value;
      end
      else
        raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
    end;
  finally
    path := '';
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
      AddToken(token);
      Texer.SkipToken; // move forward
      token := Texer.ExpectToken([tokComma, tokEqual]);
      AddToken(token);
      if TokenIsReservedSymbol(token, rs_Comma) then // comma separated variable names
        Continue;
      // we have colon, now get declared type identifier and semicolon at end
      token := Texer.GetToken;
      //if not (token.&Type in [tokIdent..tokString]) then
      if not (token.&Type = tokIdent) then
        raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'type ', sDeclaration]));
      AddToken(token);
      AddToken(Texer.ExpectToken([tokSemicolon]));
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
        raise NPCSourceParserException.ParserError(AToken.Location, Format(sParserSectionHasNoBody, [NPCReservedIdentifiers[ri_type].Ident]));
      Break;
    end
    else
      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
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
      AddToken(token);
      Texer.SkipToken; // move forward
      token := Texer.ExpectToken([tokComma, tokColon]);
      AddToken(token);
      if TokenIsReservedSymbol(token, rs_Comma) then // comma separated variable names
        Continue;
      // we have colon, now get declared type identifier and semicolon at end
      token := Texer.GetToken;
      //if not (token.&Type in [tokIdent..tokString]) then
      if not (token.&Type = tokIdent) then
        raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'const ', sDeclaration]));
      AddToken(token);
      AddToken(Texer.ExpectToken([tokSemicolon]));
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
        raise NPCSourceParserException.ParserError(AToken.Location, Format(sParserSectionHasNoBody, [NPCReservedIdentifiers[ri_const].Ident]));
      Break;
    end
    else
      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
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
      AddToken(token);
      Texer.SkipToken; // move forward
      token := Texer.ExpectToken([tokComma, tokColon]);
      AddToken(token);
      if TokenIsReservedSymbol(token, rs_Comma) then // comma separated variable names
        Continue;
      // we have colon, now get declared type identifier and semicolon at end
      token := Texer.GetToken;
      //if not (token.&Type in [tokIdent..tokString]) then
      if not (token.&Type = tokIdent) then
        raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'var ', sDeclaration]));
      AddToken(token);
      token := Texer.ExpectToken([tokSemicolon, tokEqual]);
      AddToken(token);
      if TokenIsReservedSymbol(token, rs_Equal) then begin // variable is initiated with compile-time value
        AddToken(Texer.ExpectToken([tokIdent, tokNumber, tokString, tokChar]));
        AddToken(Texer.ExpectToken([tokSemicolon]));
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
        raise NPCSourceParserException.ParserError(AToken.Location, Format(sParserSectionHasNoBody, [NPCReservedIdentifiers[ri_var].Ident]));
      Break;
    end
    else
      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
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
        raise NPCSourceParserException.ParserError(AToken.Location, Format(sParserSectionHasNoBody, [NPCReservedIdentifiers[ri_initialization].Ident]));
      Break;
    end
    else
      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'initialization ', sSection]));

//    else if TokenIsLiteral(token) or TokenIsBiLiteral(token) then begin
//      if TokenIsReservedSymbol(token, ':=') then begin
//        ParseAssignment(token);
//        Continue;
//      end
//      else begin
//        raise NPCSourceParserException.Create('not supported');
//      end;
//    end
//    else if not token.ReservedWord and not token.ReservedSymbol and (token.&Type = tokIdent) then begin
//      AddToken(token);
//      Texer.SkipToken; // move forward
//    end

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
    if TokenIsReservedSymbol(token, rs_Comma) then begin
      AddToken(token);
    end
    else if TokenIsReservedSymbol(token, rs_Semicolon) then begin
      AddToken(token);
      Break;
    end
    else if token.&Type in [tokIdent..tokString] then begin
      if TokenIsReservedIdent(token, ri_begin) then begin
        if not has_body then
          raise NPCSourceParserException.ParserError(AToken.Location, Format(sParserSectionHasNoBody, [NPCReservedIdentifiers[ri_finalization].Ident]));
        Break;
      end;
      // add finalization section body
      has_body := True;

    end
    else
      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sProjectFile]));
    //
    Texer.SkipToken;
  end;
end;

procedure TNPCSourceParser.ParseBegin(const AToken: TNPCToken);
begin

end;

procedure TNPCSourceParser.ParseEnd(const AToken: TNPCToken);
begin
  AddToken(Texer.ExpectToken([tokDot]));
  AddToken(Texer.GetToken);
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
    if TokenIsReservedIdent(token, ri_end) or TokenIsReservedSymbol(token, rs_CCurly) or TokenIsReservedSymbol(token, rs_Semicolon) then begin
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
        raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, 'initialization ', sSection]));
    end
    else if TokenIsLiteral(token) or TokenIsBiLiteral(token) then begin
      Result := True;
      if TokenIsReservedSymbol(token, rs_Assign) then begin
        ParseAssignment(token);
        Continue;
      end
      else if TokenIsReservedSymbol(token, rs_OParen) then begin // parse function call params
        ParseCallParams(token);
        Continue;
      end
      else begin
        AddToken(token);
        Texer.SkipToken; // move forward
        Continue;
      end;
    end
    else if not token.ReservedWord and not token.ReservedSymbol and (token.&Type in [tokIdent..tokChar]) then begin
      Result := True;
      AddToken(token);
      Texer.SkipToken; // move forward
      Continue;
    end
    else
      raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedTokenIn, [token.TokenToString, '', sStatement]));
    //
    Texer.SkipToken;
  end;
end;

procedure TNPCSourceParser.ParseLambdaParams(const AToken: TNPCToken);
begin
  AddToken(Texer.ExpectToken([tokOParen]));
  if Texer.IsCurrentSymbol(')') then // function without parameters
    AddToken(Texer.ExpectToken([tokCParen]))
  else begin // collect function params

  end;

//      AddToken(TNPCToken.Create(tokSetting, AToken.Location.Copy, False, False, '$program-type', EmptyTokenMD5));

end;

procedure TNPCSourceParser.ParseProject;
var
  token: TNPCToken;
begin
  Tokenizer := TNPCTokenizer.Create(TNPCProjectSettings(Settings^).ProjectFormatSettings^);
  try
    Tokenizer.TokenizeFile(TNPCProjectSettings(Settings^).InputPath, TNPCProjectSettings(Settings^).ProjectEncoding);
    if TNPCProjectSettings(Settings^).OutputTokens in [otProjectOnly, otProjectAndSources] then
      Tokenizer.OutputTokens;
    //
    if Assigned(Texer) then
      FreeAndNil(Texer);
    Texer := TNPCTokensParser.Create(TNPCProjectSettings(Settings^).InputPath, Tokenizer.Tokens);
    try
      token := Texer.ExpectToken([tokIdent]);
      if not TokenIsReservedIdent(token, ri_project) then begin
        token.Free;
        raise NPCSourceParserException.ParserError(token.Location, Format(sParserUnexpectedType, [token.TokenToString, NPCReservedIdentifiers[ri_project].Ident]));
      end;
      //
      // ok we are inside project file
      //
      AddToken(token);

      token := Texer.ExpectToken([tokString]);
      TNPCProjectSettings(Settings^).ProjectName := token.Value;
      ConsoleWriteln('Compiling project__: ' + token.Value);
      AddToken(token);

      AddToken(Texer.ExpectToken([tokSemicolon]));
      //
      // we have collected basic info about project, its name
      // go collect the rest of the project body
      //
      ParseProjectBody;
    finally
      FreeAndNil(Texer);
    end;
  finally
    FreeAndNil(Tokenizer);
  end;
end;

end.

