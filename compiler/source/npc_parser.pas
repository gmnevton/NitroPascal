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
  npc_reserved_words;

type
  TNPCSettingType = (
    setProgram,
    setResources
  );

  NPCParserException = class(Exception);

  TNPCParser = class
  private
    Lexer: TNPCLexer;
    Settings: Pointer;
    TokensArray: TNPCTokens;
  protected
    procedure Clear;
    procedure AddToken(const AToken: TNPCToken);
    function TokenIsReservedIdent(const AToken: TNPCToken; const AReservedIdent: TNPCReservedIdents): Boolean;
    function TokenIsReservedSymbol(const AToken: TNPCToken; const AReservedSymbol: Char): Boolean;
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
    procedure ParseImports;
    procedure ParseExports;
    procedure ParseInitialization;
    procedure ParseFinalization;
    procedure ParseBegin;
    procedure ParseEnd;
  public
    constructor Create(const ALexer: TNPCLexer; const PSettings: Pointer);
    destructor Destroy; override;
    //
    procedure ParseProject;
    procedure OutputTokens;
    //
    property Tokens: TNPCTokens read TokensArray;
  end;

implementation

uses
  npc_consts,
  npc_project,
  npc_md5;

{ TNPCParser }

constructor TNPCParser.Create(const ALexer: TNPCLexer; const PSettings: Pointer);
begin
  Lexer := ALexer;
  Settings := PSettings;
  SetLength(TokensArray, 0);
end;

destructor TNPCParser.Destroy;
begin
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
end;

procedure TNPCParser.AddToken(const AToken: TNPCToken);
var
  idx: Integer;
begin
  idx := Length(TokensArray);
  SetLength(TokensArray, idx + 1);
  TokensArray[idx] := AToken;
end;

function TNPCParser.TokenIsReservedIdent(const AToken: TNPCToken; const AReservedIdent: TNPCReservedIdents): Boolean;
begin
  Result := (AToken.&Type = tokIdent) and AToken.ReservedWord and SameText(AToken.Value, NPCReservedIdentifiers[AReservedIdent].Ident);
end;

function TNPCParser.TokenIsReservedSymbol(const AToken: TNPCToken; const AReservedSymbol: Char): Boolean;
begin
  Result := (AToken.&Type in [tokOParen..tokDiv]) and AToken.ReservedSymbol and (AToken.Value = AReservedSymbol);
end;

procedure TNPCParser.ParseProjectBody;
var
  token: TNPCToken;
begin
  while Lexer.IsNotEmpty do begin
    token := Lexer.NextToken;
    AddToken(token);
    if TokenIsReservedSymbol(token, '{') then begin
      if Lexer.IsCurrentSymbol('$') then
        ParseSettingDefineCondition
      else if Lexer.IsCurrentSymbol('@') then
        raise NPCParserException.CreateFmt(sParserUnexpectedTokenInProject, [token.Location.ToString, token.Value])
      else
        ParseComment;
    end
    else if TokenIsReservedIdent(token, ri_imports) then begin
      ParseImports;
    end
    else if TokenIsReservedIdent(token, ri_exports) then begin
      ParseExports;
    end
    else if TokenIsReservedIdent(token, ri_initialization) then begin
      ParseInitialization;
    end
    else if TokenIsReservedIdent(token, ri_finalization) then begin
      ParseFinalization;
    end
    else if TokenIsReservedIdent(token, ri_begin) then begin
      ParseBegin;
    end
    else if TokenIsReservedIdent(token, ri_end) then begin
      ParseEnd;
      Break;
    end
    else
      raise NPCParserException.CreateFmt(sParserUnexpectedTokenInProject, [token.Location.ToString, token.Value]);
  end;
end;

procedure TNPCParser.ParseSettingDefineCondition;
var
  token: TNPCToken;
begin
  while Lexer.IsNotEmpty do begin
    token := Lexer.NextToken;
    try
      //AddToken(token);
      if TokenIsReservedSymbol(token, '$') then begin
        ParseSettingOrDefineDirective;
      end
      else if TokenIsReservedSymbol(token, '}') then begin
        Break;
      end
      else
        raise NPCParserException.CreateFmt(sParserUnexpectedTokenInProject, [token.Location.ToString, token.Value]);
    finally
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
      end
      else if TokenIsReservedSymbol(token, '}') then begin
        Break;
      end
      else
        raise NPCParserException.CreateFmt(sParserUnexpectedTokenInProject, [token.Location.ToString, token.Value]);
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
      raise NPCParserException.CreateFmt(sParserUnexpectedTokenInProject, [AToken.Location.ToString, AToken.Value + '-' + token.Value]);
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
      raise NPCParserException.CreateFmt(sParserUnexpectedTokenInProject, [token.Location.ToString, token.Value]);
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
        raise NPCParserException.CreateFmt(sParserUnexpectedTokenInProject, [token.Location.ToString, token.Value]);
      stemp := stemp + token.Value;
    finally
      token.Free;
    end;
    AddToken(TNPCToken.Create(tokIdent, AToken.Location.Copy, False, False, stemp, EmptyTokenMD5));
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
      ParseImports;
    end
    else
      raise NPCParserException.CreateFmt(sParserUnexpectedTokenInProject, [token.Location.ToString, token.Value]);
  end;
end;

procedure TNPCParser.ParseImports;
begin

end;

procedure TNPCParser.ParseExports;
begin

end;

procedure TNPCParser.ParseInitialization;
begin

end;

procedure TNPCParser.ParseFinalization;
begin

end;

procedure TNPCParser.ParseBegin;
begin

end;

procedure TNPCParser.ParseEnd;
begin

end;

procedure TNPCParser.ParseProject;
var
  token: TNPCToken;
begin
  token := Lexer.ExpectToken([tokIdent]);
  if not TokenIsReservedIdent(token, ri_project) then begin
    raise NPCParserException.CreateFmt(sParserUnexpectedType, [token.Location.ToString, token.Value, NPCReservedIdentifiers[ri_project].Ident]);
  end;
  //
  // ok we are inside project file
  //
  AddToken(token);
  token := Lexer.ExpectToken([tokString]);
  TNPCProjectSettings(Settings^).ProjectName := token.Value;
  AddToken(token);
  AddToken(Lexer.ExpectToken([tokSemicolon]));
  //
  // we have collected basic info about project, its name
  // go collect the rest of the project body
  //
  ParseProjectBody;
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
          tf.WriteLine(Format('%s (%d:%d) - %s: "%s"', [ExtractFileName(token.Location.FileName), token.Location.Row, token.Location.Col, NPCTokensType[Ord(token.&Type)], token.Value ]));
      end;
    finally
      tf.Free;
    end;
  except
  end;
end;

end.
