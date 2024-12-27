//
// Nitro Pascal Compiler
// version 1.0
//
// Project settings
//

unit npc_project_settings;

interface

uses
  SysUtils,
  Classes;

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

  TNPCProjectOutputTokensType = (
    otNone,
    otProjectOnly,
    otSourcesOnly,
    otProjectAndSources
  );

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

  TNPCDefines = packed record

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
    Errors: TStringList;
    //
    Imports: Array of TNPCImportInfo;
    Defines: Array of TNPCDefines;
    //
    OutputTypes: Array of TNPCProjectOutputType;
    OutputTokens: TNPCProjectOutputTokensType;
  end;

  PNPCProjectSettings = ^TNPCProjectSettings;

function GetSettings: PNPCProjectSettings;

implementation

var
  Settings: TNPCProjectSettings;

function GetSettings: PNPCProjectSettings;
begin
  Result := @Settings;
end;

procedure InitSettings;
begin
  Settings.InputPath := ''; // main project path
  Settings.ProjectName := '';
  Settings.ProjectEncoding := Nil;
  Settings.ProjectFormatSettings := Nil;
  SetLength(Settings.ProjectSearchPaths, 0);
  SetLength(Settings.Imports, 0);
  SetLength(Settings.Defines, 0);
  SetLength(Settings.OutputTypes, 0);
  Settings.OutputTokens := otNone;
  Settings.Errors := TStringList.Create;
end;

procedure FreeSettings;
begin
  Settings.InputPath := ''; // main project path
  Settings.ProjectName := '';
  Settings.ProjectEncoding := Nil;
  Settings.ProjectFormatSettings := Nil;
  SetLength(Settings.ProjectSearchPaths, 0);
  SetLength(Settings.Imports, 0);
  SetLength(Settings.Defines, 0);
  SetLength(Settings.OutputTypes, 0);
  Settings.OutputTokens := otNone;
  Settings.Errors.Free;
end;

initialization
  InitSettings;

finalization
  FreeSettings;

end.

