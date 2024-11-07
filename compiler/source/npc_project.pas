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
    ptTEXT
  );
  TNPCProjectTypes = set of TNPCProjectType;

  PFormatSettings = ^TFormatSettings;

  TNPCProjectOutputType = packed record
    OutputPath: String;
    //InputStream: TMemoryStream;
    OutputStream: TMemoryStream;
    //
    ProjectTypes: TNPCProjectTypes;
    ProjectExtension: String;
  end;

  TNPCProjectOutputTokensType = (otNone, otProjectOnly, otSourcesOnly, otProjectAndSources);

  TNPCProjectSettings = packed record
    InputPath: String;

    ProjectName: String;

    ProjectEncoding: TEncoding;
    ProjectFormatSettings: PFormatSettings;

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
    constructor Create(const AInputPath, AOutputPath: String); overload;
    constructor Create(const AInputPath, AOutputPath: String; AEncoding: TEncoding; AFormatSettings: PFormatSettings); overload;
    constructor Create(const AInput, AOutput: TMemoryStream; AEncoding: TEncoding; AFormatSettings: PFormatSettings); overload;
    destructor Destroy; override;
    //
    function Compile: Boolean;
  end;

procedure NPC_SetProjectEncoding(const AEncoding: TEncoding); stdcall;
procedure NPC_SetProjectFormatSettings(const AFormatSettings: PFormatSettings); stdcall;

function NPC_CompileProject(const AProjectFileName, AProjectOutputPath: PChar): Boolean; stdcall;
function NPC_ReportErrors: PChar; stdcall;

implementation

uses
  npc_lexer,
  npc_tokenizer,
  npc_source_parser,
  npc_consts,
  npc_error;

var
  gEncoding: TEncoding;
  gFormatSettings: PFormatSettings;
  gReportedErrors: String;

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

function NPC_CompileProject(const AProjectFileName, AProjectOutputPath: PChar): Boolean;
var
  Project: TNPCProject;
begin
  Project := TNPCProject.Create(AProjectFileName, AProjectOutputPath);
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

constructor TNPCProject.Create(const AInputPath, AOutputPath: String);
begin
  Create(AInputPath, AOutputPath, gEncoding, gFormatSettings);
end;

constructor TNPCProject.Create(const AInputPath, AOutputPath: String; AEncoding: TEncoding; AFormatSettings: PFormatSettings);
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
  SetLength(Settings.OutputTypes, 0);
  Settings.OutputTokens := otNone;
  //
  Errors := TStringList.Create;
  //
  Parser := TNPCProjectParser.Create(@Settings);
end;

constructor TNPCProject.Create(const AInput, AOutput: TMemoryStream; AEncoding: TEncoding; AFormatSettings: PFormatSettings);
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
  SetLength(Settings.OutputTypes, 1);
  Settings.OutputTokens := otNone;
  //
  Settings.OutputTypes[0].OutputPath := '';
  //Settings.OutputTypes[0].InputStream := AInput;
  Settings.OutputTypes[0].OutputStream := AOutput;
  Settings.OutputTypes[0].ProjectTypes := [];
  Settings.OutputTypes[0].ProjectExtension := '';
  //
  Errors := TStringList.Create;
  //
  Parser := TNPCProjectParser.Create(@Settings);
end;

destructor TNPCProject.Destroy;
var
  i: Integer;
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
    Settings.OutputTypes[i].ProjectTypes := [];
    Settings.OutputTypes[i].ProjectExtension := '';
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
    on E: NPCLexerException do begin
      Result := False;
      Errors.Add(E.Message);
    end;
//    on E: NPCTokenizerException do begin
//      Result := False;
//      Errors.Add(E.Message);
//    end;
    on E: NPCSourceParserException do begin
      Result := False;
      Errors.Add(E.Message);
    end;
    on E: NPCProjectParserException do begin
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

finalization
//  gEncoding := Nil;
//  if Assigned(gEncoding) then
//    gEncoding.Free;
  gFormatSettings := Nil;

end.