//
// Nitro Pascal Compiler
// version 1.0
//
// Project file parser and compiler entry point
//

unit npc_project;

interface

uses
  SysUtils,
  Classes,
  npc_lexer,
  npc_parser,
  npc_project_settings,
  npc_compiler;

type
  TNPCProject = class
  private
    FCompiler: TNPCCompiler;
  protected
    procedure ReportErrors;
  public
    constructor Create(const AInputPath, AOutputPath: String; const ASettings: TNPCCommandLineSettings); overload;
    constructor Create(const AInputPath, AOutputPath: String; const ASettings: TNPCCommandLineSettings; AEncoding: TEncoding; AFormatSettings: PFormatSettings); overload;
    constructor Create(const AInput, AOutput: TMemoryStream; const ASettings: TNPCCommandLineSettings; AEncoding: TEncoding; AFormatSettings: PFormatSettings); overload;
    destructor Destroy; override;
    //
    function Compile: Boolean;
  end;

//------------------------------------------------------------------------------------------------------------------------------
// this calls are exported in a library

procedure NPC_SetProjectEncoding(const AEncoding: TEncoding); stdcall;
procedure NPC_SetProjectFormatSettings(const AFormatSettings: PFormatSettings); stdcall;

procedure NPC_InitCompiler(const AParams: String; AParamsCount: Byte); stdcall;
function NPC_CompileProject(const AProjectFileName, AProjectOutputPath: PChar): Boolean; stdcall;
function NPC_ReportErrors: PChar; stdcall;

//------------------------------------------------------------------------------------------------------------------------------
// this is for our internal use

function ProjectTypeToString(const ProjectTypes: TNPCProjectTypes): String;
function ProjectTypeToIdent(const ProjectTypes: TNPCProjectTypes; const ProjectExtension: String; const OutputPath: String): String;
function EnsureTypeIsNotSet(Token: TNPCToken; const CurrentProjectTypes: TNPCProjectTypes; ProjectTypes: TNPCProjectTypes): Boolean;

implementation

uses
  StrUtils,
  npc_tokenizer,
  npc_source_parser,
  npc_consts,
  npc_utils,
  npc_error;

var
  gEncoding: TEncoding;
  gFormatSettings: PFormatSettings;
  gReportedErrors: String;
  //
  commandLineSettings: TNPCCommandLineSettings;

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

function EnsureTypeIsNotSet(Token: TNPCToken; const CurrentProjectTypes: TNPCProjectTypes; ProjectTypes: TNPCProjectTypes): Boolean;
begin
  Result := (CurrentProjectTypes * ProjectTypes = []);
  if Result then
    Exit;
  //
  raise NPCSyntaxError.ParserError(Token.Location, Format(sParserUnexpectedTokenIn, [Token.TokenToString, '', sProjectFile]));
end;

//------------------------------------------------------------------------------------------------------------------------------

procedure NPC_SetProjectEncoding(const AEncoding: TEncoding);
begin
  if Assigned(gEncoding) then
    gEncoding.Free;
  //
  gEncoding := AEncoding;
end;

procedure NPC_SetProjectFormatSettings(const AFormatSettings: PFormatSettings);
begin
  gFormatSettings := AFormatSettings;
end;

procedure NPC_InitCompiler(const AParams: String; AParamsCount: Byte);
var
  Params: TStringArray;
  Param, Value: String;
  i, j, k: Integer;
begin
  FillChar(commandLineSettings, SizeOf(TNPCCommandLineSettings), 0);
  if AParamsCount < 1 then
    Exit;
  //
  Params := explode_quotes(' ', AParams);
  try
    for k:=0 to Length(Params) - 1 do begin
      Param := Params[k];
      if (Length(Param) > 1) and StartsText('-e:', Param) then begin // ASCII or ANSI or UTF8

      end;
      if (Length(Param) > 1) and StartsText('-o:', Param) then begin
        i := Pos(':', Param);
        if i > 0 then
          Delete(Param, 1, i);
        //
        if Length(Param) > 1 then begin
          //format_settings{}
          //output_tokens[]
          if StartsText('format_settings', Param) then begin

          end
          else if StartsText('output_tokens', Param) then begin
            i := Pos('[', Param);
            if i > 0 then begin
              j := Pos(']', Param);
              if j > 0 then begin
                Value := Copy(Param, i + 1, j - i - 1);
                if SameText(Value, 'P') then begin // ProjectOnly
                  commandLineSettings.OutputTokens := otProjectOnly;
                  commandLineSettings.SetOutputTokens := True;
                end
                else if SameText(Value, 'S') then begin // SourcesOnly
                  commandLineSettings.OutputTokens := otSourcesOnly;
                  commandLineSettings.SetOutputTokens := True;
                end
                else
                if SameText(Value, 'A') then begin // All
                  commandLineSettings.OutputTokens := otProjectAndSources;
                  commandLineSettings.SetOutputTokens := True;
                end;
              end
              else
                Continue;
            end
            else
              Continue;
          end;
        end;
      end;
    end;
  finally
    SetLength(Params, 0);
  end;
end;

function NPC_CompileProject(const AProjectFileName, AProjectOutputPath: PChar): Boolean;
var
  Project: TNPCProject;
begin
  Project := TNPCProject.Create(AProjectFileName, AProjectOutputPath, commandLineSettings);
  try
    Result := Project.Compile;
    if not Result then
      Project.ReportErrors;
  finally
    Project.Free;
  end;
end;

function NPC_ReportErrors: PChar;
begin
  Result := PChar(gReportedErrors);
end;

{ TNPCProject }

constructor TNPCProject.Create(const AInputPath, AOutputPath: String; const ASettings: TNPCCommandLineSettings);
begin
  Create(AInputPath, AOutputPath, ASettings, gEncoding, gFormatSettings);
end;

constructor TNPCProject.Create(const AInputPath, AOutputPath: String; const ASettings: TNPCCommandLineSettings; AEncoding: TEncoding; AFormatSettings: PFormatSettings);
begin
  if AEncoding = Nil then
    AEncoding := TEncoding.UTF8;
  if AFormatSettings = Nil then
    AFormatSettings := @FormatSettings;
  //
  FCompiler := TNPCCompiler.Create;
  //
  GetSettings.InputPath := AInputPath; // main project path
  GetSettings.ProjectName := '';
  GetSettings.ProjectEncoding := AEncoding;
  GetSettings.ProjectFormatSettings := AFormatSettings;
  SetLength(GetSettings.ProjectSearchPaths, 0);
  SetLength(GetSettings.Imports, 0);
  SetLength(GetSettings.Defines, 0);
  SetLength(GetSettings.OutputTypes, 0);
  GetSettings.OutputTokens := otNone;
  //
  if ASettings.SetFormatSettings then begin
    GetSettings.ProjectFormatSettings := @ASettings.FormatSettings;
  end;
  if ASettings.SetOutputTokens then begin
    GetSettings.OutputTokens := ASettings.OutputTokens;
  end;
  //
  GetSettings.Errors.Clear;
end;

constructor TNPCProject.Create(const AInput, AOutput: TMemoryStream; const ASettings: TNPCCommandLineSettings; AEncoding: TEncoding; AFormatSettings: PFormatSettings);
begin
  if AEncoding = Nil then
    if gEncoding <> Nil then
      AEncoding := gEncoding
    else
      AEncoding := TEncoding.UTF8;
  if AFormatSettings = Nil then
    if gFormatSettings <> Nil then
      AFormatSettings := @gFormatSettings
    else
      AFormatSettings := @FormatSettings;
  //
  FCompiler := TNPCCompiler.Create;
  //
  GetSettings.InputPath := '';
  GetSettings.ProjectName := '';
  GetSettings.ProjectEncoding := AEncoding;
  GetSettings.ProjectFormatSettings := AFormatSettings;
  SetLength(GetSettings.ProjectSearchPaths, 0);
  SetLength(GetSettings.Imports, 0);
  SetLength(GetSettings.Defines, 0);
  SetLength(GetSettings.OutputTypes, 1);
  GetSettings.OutputTokens := otNone;
  //
  if ASettings.SetFormatSettings then begin
    GetSettings.ProjectFormatSettings := @ASettings.FormatSettings;
  end;
  if ASettings.SetOutputTokens then begin
    GetSettings.OutputTokens := ASettings.OutputTokens;
  end;
  //
  GetSettings.OutputTypes[0].OutputPath := '';
  //Settings.OutputTypes[0].InputStream := AInput;
  GetSettings.OutputTypes[0].OutputStream := AOutput;
  GetSettings.OutputTypes[0].CompilationType := [];
  GetSettings.OutputTypes[0].CompilationExtension := '';
  SetLength(GetSettings.OutputTypes[0].CompilationSearchPaths, 0);
  //
  GetSettings.Errors.Clear;
end;

destructor TNPCProject.Destroy;
var
  i, j: Integer;
begin
  //FreeAndNil(GetSettings.Errors);
  GetSettings.Errors.Clear;
  //
  GetSettings.InputPath := '';
  GetSettings.ProjectName := '';
  //Settings.ProjectEncoding := Nil;
  GetSettings.ProjectFormatSettings := Nil;
  for i:=0 to High(GetSettings.OutputTypes) do begin
    GetSettings.OutputTypes[i].OutputPath := '';
    GetSettings.OutputTypes[i].OutputStream := Nil;
    GetSettings.OutputTypes[i].CompilationType := [];
    GetSettings.OutputTypes[i].CompilationExtension := '';
    //
    for j:=0 to High(GetSettings.OutputTypes[i].CompilationSearchPaths) do
      GetSettings.OutputTypes[i].CompilationSearchPaths[j] := '';
    SetLength(GetSettings.OutputTypes[i].CompilationSearchPaths, 0);
  end;
  SetLength(GetSettings.OutputTypes, 0);
  //
  for i:=0 to High(GetSettings.ProjectSearchPaths) do
    GetSettings.ProjectSearchPaths[i] := '';
  SetLength(GetSettings.ProjectSearchPaths, 0);
  //
  for i:=0 to High(GetSettings.Imports) do begin
    GetSettings.Imports[i].InputPath := '';
    GetSettings.Imports[i].CodeName := '';
    SetLength(GetSettings.Imports[i].Imports, 0);
  end;
  SetLength(GetSettings.Imports, 0);
  //
  for i:=0 to High(GetSettings.Defines) do begin
    //Settings.Defines[i]. := '';
  end;
  SetLength(GetSettings.Defines, 0);
  //
  FCompiler.Free;
  inherited;
end;

function TNPCProject.Compile: Boolean;
var
  Parser: TNPCProjectParser;
begin
  Result := True;
  try
    FCompiler.Clear;
    Parser := TNPCProjectParser.Create(FCompiler, Nil);
    try
      Parser.ParseProject;
    finally
      Parser.Free;
    end;
  except
    on E: NPCCompilerError do begin
      Result := False;
      GetSettings.Errors.Add(E.Message);
    end;
    on E: NPCProjectError do begin
      Result := False;
      GetSettings.Errors.Add(E.Message);
    end;
    on E: NPCSyntaxError do begin
      Result := False;
      GetSettings.Errors.Add(E.Message);
    end;
    on E: Exception do begin
      Result := False;
      GetSettings.Errors.Add(Format(sProjectError, [ExtractFileName(GetSettings.InputPath), E.ClassName, E.Message]));
      GetSettings.Errors.Add(GetExceptionStackTrace(E));
    end;
  end;
end;

procedure TNPCProject.ReportErrors;
begin
  gReportedErrors := #13#10 + Trim(GetSettings.Errors.Text);
  GetSettings.Errors.Clear;
end;

initialization
  gEncoding := Nil;
  gFormatSettings := Nil;
  FillChar(commandLineSettings, SizeOf(TNPCCommandLineSettings), 0);

finalization
//  gEncoding := Nil;
//  if Assigned(gEncoding) then
//    gEncoding.Free;
  gFormatSettings := Nil;

end.
