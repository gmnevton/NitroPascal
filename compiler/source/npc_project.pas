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
  npc_parser;

type
  TNPCProjectType = (
    ptWindows,
    ptLinux,
    ptAndroid,
    ptWebAssembly,

    pt32Bit,
    pt64Bit,

    ptCONSOLE,
    ptGUI,
    ptDLL,
    ptTEXT,
    ptSCRIPT // no runtime, executing during compilation; * no rumtime - means that project main begin...end is not fired and thus not needed
  );
  TNPCProjectTypes = set of TNPCProjectType;

  PFormatSettings = ^TFormatSettings;

  TNPCProjectOutputType = packed record
    OutputPath: String;
    //InputStream: TMemoryStream;
    OutputStream: TMemoryStream;
    //
    CompilationType: TNPCProjectTypes;
    CompilationExtension: String;
    CompilationSearchPaths: Array of String;
  end;

  TNPCProjectOutputTokensType = (otNone, otProjectOnly, otSourcesOnly, otProjectAndSources);

  TNPCCommandLineSettings = packed record
    FormatSettings: TFormatSettings;
    SetFormatSettings: Boolean;
    //
    OutputTokens: TNPCProjectOutputTokensType;
    SetOutputTokens: Boolean;
  end;

  TNPCImportInfo = packed record
    InputPath: String;
    //
    CodeName: String;
    //
    Imports: Array of TNPCImportInfo;
  end;

  TNPCProjectSettings = packed record
    InputPath: String;
    //
    ProjectName: String;
    //
    ProjectEncoding: TEncoding;
    ProjectFormatSettings: PFormatSettings;
    ProjectSearchPaths: Array of String;
    //
    Imports: Array of TNPCImportInfo;
    //
    OutputTypes: Array of TNPCProjectOutputType;
    OutputTokens: TNPCProjectOutputTokensType;
  end;

  TNPCProject = class
  private
    Settings: TNPCProjectSettings;
    Errors: TStringList;
    //
    Parser: TNPCProjectParser;
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

procedure NPC_SetProjectEncoding(const AEncoding: TEncoding); stdcall;
procedure NPC_SetProjectFormatSettings(const AFormatSettings: PFormatSettings); stdcall;

procedure NPC_InitCompiler(const AParams: String; AParamsCount: Byte); stdcall;
function NPC_CompileProject(const AProjectFileName, AProjectOutputPath: PChar): Boolean; stdcall;
function NPC_ReportErrors: PChar; stdcall;

//------------------------------------------------------------------------------------------------------------------------------

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
    Project.Parser.OutputTokens;
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
//  TNPCProjectSettings = packed record
//    InputPath: String;
//    ProjectName: String;
//    ProjectEncoding: TEncoding;
//    ProjectFormatSettings: PFormatSettings;
//    OutputTypes: Array of TNPCProjectOutputType;
  //
  Settings.InputPath := AInputPath; // main project path
  Settings.ProjectName := '';
  Settings.ProjectEncoding := AEncoding;
  Settings.ProjectFormatSettings := AFormatSettings;
  SetLength(Settings.ProjectSearchPaths, 0);
  SetLength(Settings.Imports, 0);
  SetLength(Settings.OutputTypes, 0);
  Settings.OutputTokens := otNone;
  //
  if ASettings.SetFormatSettings then begin
    Settings.ProjectFormatSettings := @ASettings.FormatSettings;
  end;
  if ASettings.SetOutputTokens then begin
    Settings.OutputTokens := ASettings.OutputTokens;
  end;
  //
  Errors := TStringList.Create;
  //
  Parser := TNPCProjectParser.Create(@Settings);
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
  Settings.InputPath := '';
  Settings.ProjectName := '';
  Settings.ProjectEncoding := AEncoding;
  Settings.ProjectFormatSettings := AFormatSettings;
  SetLength(Settings.ProjectSearchPaths, 0);
  SetLength(Settings.Imports, 0);
  SetLength(Settings.OutputTypes, 1);
  Settings.OutputTokens := otNone;
  //
  if ASettings.SetFormatSettings then begin
    Settings.ProjectFormatSettings := @ASettings.FormatSettings;
  end;
  if ASettings.SetOutputTokens then begin
    Settings.OutputTokens := ASettings.OutputTokens;
  end;
  //
  Settings.OutputTypes[0].OutputPath := '';
  //Settings.OutputTypes[0].InputStream := AInput;
  Settings.OutputTypes[0].OutputStream := AOutput;
  Settings.OutputTypes[0].CompilationType := [];
  Settings.OutputTypes[0].CompilationExtension := '';
  SetLength(Settings.OutputTypes[0].CompilationSearchPaths, 0);
  //
  Errors := TStringList.Create;
  //
  Parser := TNPCProjectParser.Create(@Settings);
end;

destructor TNPCProject.Destroy;
var
  i, j: Integer;
begin
  FreeAndNil(Parser);
  FreeAndNil(Errors);
  //
  Settings.InputPath := '';
  Settings.ProjectName := '';
  //Settings.ProjectEncoding := Nil;
  Settings.ProjectFormatSettings := Nil;
  for i:=0 to High(Settings.OutputTypes) do begin
    Settings.OutputTypes[i].OutputPath := '';
    Settings.OutputTypes[i].OutputStream := Nil;
    Settings.OutputTypes[i].CompilationType := [];
    Settings.OutputTypes[i].CompilationExtension := '';
    //
    for j:=0 to High(Settings.ProjectSearchPaths) do
      Settings.ProjectSearchPaths[j] := '';
    SetLength(Settings.ProjectSearchPaths, 0);
    //
    for j:=0 to High(Settings.Imports) do begin
      Settings.Imports[j].InputPath := '';
      Settings.Imports[j].CodeName := '';
      SetLength(Settings.Imports[j].Imports, 0);
    end;
    SetLength(Settings.Imports, 0);
    //
    for j:=0 to High(Settings.OutputTypes[i].CompilationSearchPaths) do
      Settings.OutputTypes[i].CompilationSearchPaths[j] := '';
    SetLength(Settings.OutputTypes[i].CompilationSearchPaths, 0);
  end;
  SetLength(Settings.OutputTypes, 0);
  inherited;
end;

function TNPCProject.Compile: Boolean;
begin
  Result := True;
  try
    Parser.ParseProject;
  except
    on E: NPCCompilerError do begin
      Result := False;
      Errors.Add(E.Message);
    end;
    on E: NPCProjectError do begin
      Result := False;
      Errors.Add(E.Message);
    end;
    on E: NPCSyntaxError do begin
      Result := False;
      Errors.Add(E.Message);
    end;
    on E: Exception do begin
      Result := False;
      Errors.Add(Format(sProjectError, [ExtractFileName(Settings.InputPath), E.ClassName, E.Message]));
      Errors.Add(GetExceptionStackTrace(E));
    end;
  end;
end;

procedure TNPCProject.ReportErrors;
begin
  gReportedErrors := #13#10 + Trim(Errors.Text);
  Errors.Clear;
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
