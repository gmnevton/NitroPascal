//
// Nitro Pascal Compiler
// version 1.0
//
// Parser
//

unit npc_parser;

interface

uses
  SysUtils,
  Classes,
  npc_lexer,
  npc_reserved_words,
  npc_utils,
  npc_error;

type
  TNPCSettingType = (
    setProgram,
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

  NPCParserException = class(TNPCError);

  TNPCParser = class
  private
    Lexer: TNPCLexer;
    Settings: Pointer;
    TokensArray: TNPCTokens;
    Imports: TNPCImportArray;
  protected
    procedure Clear;
    procedure AddToken(const AToken: TNPCToken);
    procedure AddImport(const AImportType: TNPCImportType; const AImportName: String; const AImportPath: String);
    function TokenIsReservedIdent(const AToken: TNPCToken; const AReservedIdent: TNPCReservedIdents): Boolean; overload;
    function TokenIsReservedIdent(const AToken: TNPCToken): Boolean; overload;
    function TokenIsReservedSymbol(const AToken: TNPCToken; const AReservedSymbol: Char): Boolean; overload;
    function TokenIsReservedSymbol(const AToken: TNPCToken; const AReservedSymbol: String): Boolean; overload;
    function SkipComments(const AToken: TNPCToken): Boolean;
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
    procedure ParseImports(const AToken: TNPCToken);
    procedure ParseExports(const AToken: TNPCToken);
    procedure ParseTypes(const AToken: TNPCToken);
    procedure ParseConsts(const AToken: TNPCToken);
    procedure ParseVariables(const AToken: TNPCToken);
    procedure ParseInitialization(const AToken: TNPCToken);
    procedure ParseFinalization(const AToken: TNPCToken);
    procedure ParseBegin(const AToken: TNPCToken);
    procedure ParseEnd(const AToken: TNPCToken);
    procedure ParseLambdaParams(const AToken: TNPCToken);
  public
    constructor Create(const ALexer: TNPCLexer; const PSettings: Pointer);
    destructor Destroy; override;
    //
    procedure ParseProject;
    procedure ParseImportFile(const ASourceFile: String); overload;
    procedure ParseImportFile(const ASourceFile: TStringStream); overload;
    procedure ParseSourceCode(const ASourceCode: String); overload;
    procedure ParseSourceCode(const ASourceCode: TStringStream); overload;
    procedure OutputTokens;
    //
    property Tokens: TNPCTokens read TokensArray;
  end;

implementation

uses
  StrUtils,
  npc_consts,
  npc_project,
  npc_md5,
  npc_types;

{ TNPCParser }

constructor TNPCParser.Create(const ALexer: TNPCLexer; const PSettings: Pointer);
begin
  Lexer := ALexer;
  Settings := PSettings;
  SetLength(TokensArray, 0);
  //Imports := TFastStringList.Create;
  SetLength(Imports, 0);
end;

destructor TNPCParser.Destroy;
begin
//  Imports.Clear;
//  Imports.Free;
  Clear;
  inherited;
end;

procedure TNPCParser.Clear;
var
  i: Integer;
begin
  for i:=0 to High(TokensArray) do
    FreeAndNil(TokensArray[i]);
  //
  SetLength(TokensArray, 0);
  //
  //
  for i:=0 to High(Imports) do begin
    Imports[i].Name := '';
    Imports[i].Path := '';
  end;
  //
  SetLength(Imports, 0);
end;

procedure TNPCParser.AddToken(const AToken: TNPCToken);
var
  idx: Integer;
begin
  idx := Length(TokensArray);
  SetLength(TokensArray, idx + 1);
  TokensArray[idx] := AToken;
end;

procedure TNPCParser.AddImport(const AImportType: TNPCImportType; const AImportName, AImportPath: String);
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

function TNPCParser.TokenIsReservedIdent(const AToken: TNPCToken; const AReservedIdent: TNPCReservedIdents): Boolean;
begin
  Result := (AToken.&Type = tokIdent) and AToken.ReservedWord and SameText(AToken.Value, NPCReservedIdentifiers[AReservedIdent].Ident);
end;

function TNPCParser.TokenIsReservedIdent(const AToken: TNPCToken): Boolean;
begin
  Result := (AToken.&Type = tokIdent) and AToken.ReservedWord;
end;

function TNPCParser.TokenIsReservedSymbol(const AToken: TNPCToken; const AReservedSymbol: Char): Boolean;
begin
  Result := (AToken.&Type in [tokOParen..tokDiv]) and AToken.ReservedSymbol and (AToken.Value = AReservedSymbol);
end;

function TNPCParser.TokenIsReservedSymbol(const AToken: TNPCToken; const AReservedSymbol: String): Boolean;
begin
  Result := (AToken.&Type in [tokCommentSL..tokAssign]) and AToken.ReservedSymbol and StartsStr(AReservedSymbol, AToken.Value);
end;

function TNPCParser.SkipComments(const AToken: TNPCToken): Boolean;
var
  token: TNPCToken;
begin
  Result := False;
  if TokenIsReservedSymbol(Atoken, '//') then // single-line comment
    Exit(True);
  if TokenIsReservedSymbol(Atoken, '(*') then begin // multi-line comment
    while Lexer.IsNotEmpty do begin
      token := Lexer.NextToken;
      if TokenIsReservedSymbol(token, '*)') then // skip comments until closing section
        Break;
    end;
    Exit(True);
  end;
  if TokenIsReservedSymbol(Atoken, '{.') then begin // multi-line comment
    while Lexer.IsNotEmpty do begin
      token := Lexer.NextToken;
      if TokenIsReservedSymbol(token, '.}') then // skip comments until closing section
        Break;
    end;
    Exit(True);
  end;
end;

procedure TNPCParser.ParseProjectBody;
var
  token: TNPCToken;
begin
  while Lexer.IsNotEmpty do begin
    token := Lexer.NextToken;
    // skip comments
    if SkipComments(token) then
      Continue;
    //
    // add relevant tokens
    AddToken(token);
    if TokenIsReservedSymbol(token, '{') then begin
      if Lexer.IsCurrentSymbol('$') then
        ParseSettingDefineCondition
      else if Lexer.IsCurrentSymbol('@') then
        raise NPCParserException.ParserError(token.Location, Format(sParserUnexpectedTokenInProject, [token.Value]))
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
      raise NPCParserException.ParserError(token.Location, Format(sParserUnexpectedTokenInProject, [token.Value]));
  end;
end;

procedure TNPCParser.ParseSettingDefineCondition;
var
  token: TNPCToken;
  free_token: Boolean;
begin
  while Lexer.IsNotEmpty do begin
    token := Lexer.NextToken;
    free_token := True;
    try
      //AddToken(token);
      if TokenIsReservedSymbol(token, '$') then begin
        ParseSettingOrDefineDirective;
      end
      else if TokenIsReservedSymbol(token, '}') then begin
        AddToken(token);
        free_token := False;
        Break;
      end
      else
        raise NPCParserException.ParserError(token.Location, Format(sParserUnexpectedTokenInProject, [token.Value]));
    finally
      if free_token then
        token.Free;
    end;
  end;
end;

procedure TNPCParser.ParseSettingOrDefineDirective;
var
  token: TNPCToken;
begin
  while Lexer.IsNotEmpty do begin
    token := Lexer.NextToken;
    try
      if (token.&Type = tokIdent) and SameText(token.Value, 'program') then begin
        ParseSettings(token, setProgram);
        Break;
      end
//      else if TokenIsReservedSymbol(token, '}') then begin
//        Break;
//      end
      else
        raise NPCParserException.ParserError(token.Location, Format(sParserUnexpectedTokenInProject, [token.Value]));
    finally
      token.Free;
    end;
  end;
end;

procedure TNPCParser.ParseSettings(const AToken: TNPCToken; const ASettingType: TNPCSettingType);
begin
  case ASettingType of
    setProgram: ParseSettingProgram(AToken);
    setResources: ;
  end;
end;

procedure TNPCParser.ParseSettingProgram(const AToken: TNPCToken);
var
  token: TNPCToken;
begin
  Lexer.ExpectToken([tokMinus]).Free;
  token := Lexer.NextToken;
  try
    if (token.&Type = tokIdent) and SameText(token.Value, 'type') then begin
      ParseSettingProgramType(AToken);
    end
    else
      raise NPCParserException.ParserError(token.Location, Format(sParserUnexpectedTokenInProject, [token.Value]));
  finally
    token.Free;
  end;
end;

procedure TNPCParser.ParseSettingProgramType(AToken: TNPCToken);
var
  token: TNPCToken;
  stemp: String;
begin
  Assert(Assigned(AToken), 'no token passed');
  TNPCProjectSettings(Settings^).ProjectType := [];
  AddToken(TNPCToken.Create(tokSetting, AToken.Location.Copy, False, False, '$program-type', EmptyTokenMD5));
  stemp := '';
  //
  token := Lexer.ExpectToken([tokIdent]);
  AToken := token;
  try
    if SameText(token.Value, 'GUI') then
      TNPCProjectSettings(Settings^).ProjectType := TNPCProjectSettings(Settings^).ProjectType + [ptGUI]
    else if SameText(token.Value, 'CONSOLE') then
      TNPCProjectSettings(Settings^).ProjectType := TNPCProjectSettings(Settings^).ProjectType + [ptCONSOLE]
    else if SameText(token.Value, 'DLL') then
      TNPCProjectSettings(Settings^).ProjectType := TNPCProjectSettings(Settings^).ProjectType + [ptDLL]
    else
      raise NPCParserException.ParserError(token.Location, Format(sParserUnexpectedTokenInProject, [token.Value]));
    stemp := stemp + token.Value;
    //
    Lexer.ExpectToken([tokPlus]).Free;
    stemp := stemp + '+';
    token := Lexer.ExpectToken([tokIdent]);
    try
      if SameText(token.Value, 'Windows32') then
      begin
        TNPCProjectSettings(Settings^).ProjectType := TNPCProjectSettings(Settings^).ProjectType + [ptWindows];
        TNPCProjectSettings(Settings^).ProjectType := TNPCProjectSettings(Settings^).ProjectType + [pt32Bit];
      end
      else if SameText(token.Value, 'Windows64') then
      begin
        TNPCProjectSettings(Settings^).ProjectType := TNPCProjectSettings(Settings^).ProjectType + [ptWindows];
        TNPCProjectSettings(Settings^).ProjectType := TNPCProjectSettings(Settings^).ProjectType + [pt64Bit];
      end
      else if SameText(token.Value, 'Linux32') then
      begin
        TNPCProjectSettings(Settings^).ProjectType := TNPCProjectSettings(Settings^).ProjectType + [ptLinux];
        TNPCProjectSettings(Settings^).ProjectType := TNPCProjectSettings(Settings^).ProjectType + [pt32Bit];
      end
      else if SameText(token.Value, 'Linux64') then
      begin
        TNPCProjectSettings(Settings^).ProjectType := TNPCProjectSettings(Settings^).ProjectType + [ptLinux];
        TNPCProjectSettings(Settings^).ProjectType := TNPCProjectSettings(Settings^).ProjectType + [pt64Bit];
      end
      else if SameText(token.Value, 'Android32') then
      begin
        TNPCProjectSettings(Settings^).ProjectType := TNPCProjectSettings(Settings^).ProjectType + [ptAndroid];
        TNPCProjectSettings(Settings^).ProjectType := TNPCProjectSettings(Settings^).ProjectType + [pt32Bit];
      end
      else if SameText(token.Value, 'Android64') then
      begin
        TNPCProjectSettings(Settings^).ProjectType := TNPCProjectSettings(Settings^).ProjectType + [ptAndroid];
        TNPCProjectSettings(Settings^).ProjectType := TNPCProjectSettings(Settings^).ProjectType + [pt64Bit];
      end
      else
        raise NPCParserException.ParserError(token.Location, Format(sParserUnexpectedTokenInProject, [token.Value]));
      stemp := stemp + token.Value;
    finally
      token.Free;
    end;
    AddToken(TNPCToken.Create(tokIdent, AToken.Location.Copy, False, False, stemp, EmptyTokenMD5));
    ConsoleWriteln('Target project type: ' + stemp);
    stemp := '';
  finally
    AToken.Free;
  end;
end;

procedure TNPCParser.ParseDefines;
begin

end;

procedure TNPCParser.ParseDirectives;
begin

end;

procedure TNPCParser.ParseComment;
var
  token: TNPCToken;
begin
  while Lexer.IsNotEmpty do begin
    token := Lexer.NextToken;
    AddToken(token);
    if TokenIsReservedSymbol(token, '$') then begin
    end
    else if TokenIsReservedSymbol(token, '@') then begin
    end
    else if TokenIsReservedSymbol(token, '}') then begin
      Break;
    end
    else if TokenIsReservedIdent(token, ri_imports) then begin
      ParseImports(token);
    end
    else
      raise NPCParserException.ParserError(token.Location, Format(sParserUnexpectedTokenInProject, [token.Value]));
  end;
end;

procedure TNPCParser.ParseImports(const AToken: TNPCToken);
var
  token: TNPCToken;
  path: String;
begin
  path := ExtractFilePath(TNPCProjectSettings(Settings^).InputPath);
  try
    while Lexer.IsNotEmpty do begin
      token := Lexer.NextToken;
      if TokenIsReservedSymbol(token, ',') then begin
        AddToken(token);
      end
      else if TokenIsReservedSymbol(token, ';') then begin
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
        raise NPCParserException.ParserError(token.Location, Format(sParserUnexpectedTokenInProject, [token.Value]));
    end;
  finally
    path := '';
  end;
end;

procedure TNPCParser.ParseExports(const AToken: TNPCToken);
begin

end;

procedure TNPCParser.ParseTypes(const AToken: TNPCToken);
begin

end;

procedure TNPCParser.ParseConsts(const AToken: TNPCToken);
begin

end;

procedure TNPCParser.ParseVariables(const AToken: TNPCToken);
var
  token: TNPCToken;
  has_body: Boolean;
begin
  has_body := False;
  while Lexer.IsNotEmpty do begin
    token := Lexer.NextToken;
    // skip comments
    if SkipComments(token) then
      Continue;
    //
    if (token.&Type = tokIdent) and not TokenIsReservedIdent(token) then begin
      has_body := True;
      // left side is variable name
      AddToken(token);
      token := Lexer.ExpectToken([tokComma, tokColon]);
      AddToken(token);
      if TokenIsReservedSymbol(token, ',') then // comma separated variable names
        Continue;
      // we have colon, now get declared type identifier and semicolon at end
      token := Lexer.NextToken;
      //if not (token.&Type in [tokIdent..tokString]) then
      if not (token.&Type = tokIdent) then
        raise NPCParserException.ParserError(token.Location, Format(sParserUnexpectedTokenInDeclaration, [NPCTokensType[Ord(token.&Type)], 'type']));
      AddToken(token);
      AddToken(Lexer.ExpectToken([tokSemicolon]));
//      if (token.&Type = tokIdent) and Lexer.IsCurrentSymbol('(') then // might be a function call
//        ParseLambdaParams(token);
    end
    else if TokenIsReservedSymbol(token, ',') then begin
      AddToken(token);
    end
    else if TokenIsReservedSymbol(token, ';') then begin
      AddToken(token);
      Break;
    end
    else if TokenIsReservedIdent(token) then begin
      if not has_body then
        raise NPCParserException.ParserError(AToken.Location, Format(sParserSectionHasNoBody, [NPCReservedIdentifiers[ri_initialization].Ident]));
      Break;
    end
    else
      raise NPCParserException.ParserError(token.Location, Format(sParserUnexpectedTokenInProject, [token.Value]));
  end;
end;

procedure TNPCParser.ParseInitialization(const AToken: TNPCToken);
var
  token: TNPCToken;
  has_body: Boolean;
begin
  has_body := False;
  while Lexer.IsNotEmpty do begin
    token := Lexer.NextToken;
    if TokenIsReservedSymbol(token, ',') then begin
      AddToken(token);
    end
    else if TokenIsReservedSymbol(token, ';') then begin
      AddToken(token);
      Break;
    end
    else if token.&Type in [tokIdent..tokString] then begin
      if TokenIsReservedIdent(token, ri_finalization) or TokenIsReservedIdent(token, ri_begin) then begin
        if not has_body then
          raise NPCParserException.ParserError(AToken.Location, Format(sParserSectionHasNoBody, [NPCReservedIdentifiers[ri_initialization].Ident]));
        Break;
      end;
      // add initialization section body
      has_body := True;

    end
    else
      raise NPCParserException.ParserError(token.Location, Format(sParserUnexpectedTokenInProject, [token.Value]));
  end;
end;

procedure TNPCParser.ParseFinalization(const AToken: TNPCToken);
var
  token: TNPCToken;
  has_body: Boolean;
begin
  has_body := False;
  while Lexer.IsNotEmpty do begin
    token := Lexer.NextToken;
    if TokenIsReservedSymbol(token, ',') then begin
      AddToken(token);
    end
    else if TokenIsReservedSymbol(token, ';') then begin
      AddToken(token);
      Break;
    end
    else if token.&Type in [tokIdent..tokString] then begin
      if TokenIsReservedIdent(token, ri_begin) then begin
        if not has_body then
          raise NPCParserException.ParserError(AToken.Location, Format(sParserSectionHasNoBody, [NPCReservedIdentifiers[ri_finalization].Ident]));
        Break;
      end;
      // add finalization section body
      has_body := True;

    end
    else
      raise NPCParserException.ParserError(token.Location, Format(sParserUnexpectedTokenInProject, [token.Value]));
  end;
end;

procedure TNPCParser.ParseBegin(const AToken: TNPCToken);
begin

end;

procedure TNPCParser.ParseEnd(const AToken: TNPCToken);
begin
  AddToken(Lexer.ExpectToken([tokDot]));
  AddToken(Lexer.NextToken);
end;

procedure TNPCParser.ParseLambdaParams(const AToken: TNPCToken);
begin
  AddToken(Lexer.ExpectToken([tokOParen]));
  if Lexer.IsCurrentSymbol(')') then // function without parameters
    AddToken(Lexer.ExpectToken([tokCParen]))
  else begin // collect function params

  end;

//      AddToken(TNPCToken.Create(tokSetting, AToken.Location.Copy, False, False, '$program-type', EmptyTokenMD5));

end;

procedure TNPCParser.ParseProject;
var
  token: TNPCToken;
begin
  token := Lexer.ExpectToken([tokIdent]);
  if not TokenIsReservedIdent(token, ri_project) then begin
    token.Free;
    raise NPCParserException.ParserError(token.Location, Format(sParserUnexpectedType, [token.Value, NPCReservedIdentifiers[ri_project].Ident]));
  end;
  //
  // ok we are inside project file
  //
  AddToken(token);

  token := Lexer.ExpectToken([tokString]);
  TNPCProjectSettings(Settings^).ProjectName := token.Value;
  ConsoleWriteln('Compiling project__: ' + token.Value);
  AddToken(token);

  AddToken(Lexer.ExpectToken([tokSemicolon]));
  //
  // we have collected basic info about project, its name
  // go collect the rest of the project body
  //
  ParseProjectBody;
end;

procedure TNPCParser.ParseImportFile(const ASourceFile: String);
begin

end;

procedure TNPCParser.ParseImportFile(const ASourceFile: TStringStream);
begin

end;

procedure TNPCParser.ParseSourceCode(const ASourceCode: String);
begin

end;

procedure TNPCParser.ParseSourceCode(const ASourceCode: TStringStream);
begin

end;

procedure TNPCParser.OutputTokens;
var
  i: Integer;
  tf: TStreamWriter;
  token: TNPCToken;
begin
  try
    tf := TStreamWriter.Create(TNPCProjectSettings(Settings^).ProjectName + '.tokens', False, TEncoding.UTF8, 32768);
    try
      tf.BaseStream.Position := 0;
      tf.BaseStream.Size := 0;
      //
      for i:=0 to Length(TokensArray) - 1 do begin
        token := TokensArray[i];
        if Assigned(token) then
          tf.WriteLine(Format('%s (%d:%d) - %s: "%s"', [token.Location.FileName, token.Location.StartRow, token.Location.StartCol, NPCTokensType[Ord(token.&Type)], token.Value]));
      end;
    finally
      tf.Free;
    end;
  except
  end;
end;

end.
