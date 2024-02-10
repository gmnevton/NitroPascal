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
  TNPCProjectType = (ptGUI, ptCONSOLE, ptDLL, ptWindows, ptLinux, ptAndroid, pt32Bit, pt64Bit);
  TNPCProjectTypes = set of TNPCProjectType;

  PFormatSettings = ^TFormatSettings;

  TNPCProjectSettings = packed record
    InputPath: String;
    OutputPath: String;
    //
    ProjectEncoding: TEncoding;
    ProjectFormatSettings: PFormatSettings;
    //
    ProjectName: String;
    ProjectType: TNPCProjectTypes;
    ProjectExtension: String;
  end;

  TNPCProject = class
  private
    Settings: TNPCProjectSettings;
    Errors: TStringList;
    //
    Lexer: TNPCLexer;
    Parser: TNPCParser;
  protected
    procedure ReportErrors;
  public
    constructor Create(const AInputPath, AOutputPath: String); overload;
    constructor Create(const AInputPath, AOutputPath: String; AEncoding: TEncoding; AFormatSettings: PFormatSettings); overload;
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
  npc_consts;

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
  Settings.InputPath := AInputPath;
  Settings.OutputPath := AOutputPath;
  Settings.ProjectEncoding := AEncoding;
  Settings.ProjectFormatSettings := AFormatSettings;
  Settings.ProjectName := '';
  Settings.ProjectType := [];
  Settings.ProjectExtension := '';
  //
  Errors := TStringList.Create;
  //
  Lexer := TNPCLexer.Create(AInputPath, AFormatSettings^, AEncoding);
  Parser := TNPCParser.Create(Lexer, @Settings);
end;

destructor TNPCProject.Destroy;
begin
  FreeAndNil(Parser);
  FreeAndNil(Lexer);
  FreeAndNil(Errors);
  //
  Settings.InputPath := '';
  Settings.OutputPath := '';
  Settings.ProjectEncoding := Nil;
  Settings.ProjectFormatSettings := Nil;
  Settings.ProjectName := '';
  Settings.ProjectType := [];
  Settings.ProjectExtension := '';
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
    on E: NPCParserException do begin
      Result := False;
      Errors.Add(E.Message);
    end;
    on E: Exception do begin
      Result := False;
      Errors.Add(Format(sProjectError, [ExtractFileName(Settings.InputPath), E.ClassName, E.Message]));
    end;
  end;
end;

procedure TNPCProject.ReportErrors;
begin
  gReportedErrors := Errors.Text;
  Errors.Clear;
end;

initialization
  gEncoding := Nil;
  gFormatSettings := Nil;

finalization
  if Assigned(gEncoding) then
    gEncoding.Free;
  gFormatSettings := Nil;

end.