//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-07
// All rights reserved.
//

unit ned_workspace_manager;

interface

uses
  SysUtils,
  Classes,
  ned_session_context,
  ned_projects;

type
  TNEDWorkspace = class
  private
//    FEditorSettings: TNEDEditorSettings;
//    FTerminalSettings: TNEDTerminalSettings;
//    FCodeFormatting: TNEDCodeFormatting;
//    FCodeCompletion: TNEDCodeCompletion;
//    FVersioningSettings: TNEDVersioningSettings;
//    FSnippets: TNEDSnippets;
//    FTemplates: TNEDTemplates;
//    FMacros: TNEDMacros;
//    FAISettings: TNEDAISettings;
//    FSearchHistory: TNEDSearchHistory;
//    FReplaceHistory: TNEDReplaceHistroy;
//    //
//    FCompilerSettings: TNEDCompilerSettings;
//    FDebugerSettings: TNEDDebuggerSettings;
//    //
//    FWindowLayout: TNEDWindowLayout;
//    FOpenEditors: TNEDOpenEditors;
//    //
//    FPlugins: TNEDPlugins;
//    FExternalTools: TNEDExternalTools;
  public
    constructor Create;
    destructor Destroy; override;
  end;


  TNEDWorkspaceManager = class
  private
     FWorkspace: TNEDWorkspace;

  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure CreateWorkspace;
    procedure DestroyWorkspace;
    //
    function NewProject(const AName: String): TNEDProject;
    function NewProjectGroup(const AName: String): TNEDProject;
    //
    function AddProject(const AProject: TNEDProject): TNEDProject;
    function AddProjectGroup(const AProject: TNEDProject): TNEDProject;
    //
    function OpenProject(const AProjectPath: String): TNEDProject;
    function OpenProjectGroup(const AProjectPath: String): TNEDProject;
    function OpenFile(const AFilePath: String): TNEDProject;
  end;

var
  NEDWorkspace: TNEDWorkspaceManager;

implementation

{ TNEDWorkspace }

constructor TNEDWorkspace.Create;
begin

end;

destructor TNEDWorkspace.Destroy;
begin

  inherited;
end;

{ TNEDWorkspaceManager }

constructor TNEDWorkspaceManager.Create;
begin

end;

destructor TNEDWorkspaceManager.Destroy;
begin

  inherited;
end;

procedure TNEDWorkspaceManager.CreateWorkspace;
begin

end;

procedure TNEDWorkspaceManager.DestroyWorkspace;
begin

end;

function TNEDWorkspaceManager.NewProject(const AName: String): TNEDProject;
begin

end;

function TNEDWorkspaceManager.NewProjectGroup(const AName: String): TNEDProject;
begin

end;

function TNEDWorkspaceManager.AddProject(const AProject: TNEDProject): TNEDProject;
begin

end;

function TNEDWorkspaceManager.AddProjectGroup(const AProject: TNEDProject): TNEDProject;
begin

end;

function TNEDWorkspaceManager.OpenProject(const AProjectPath: String): TNEDProject;
begin

end;

function TNEDWorkspaceManager.OpenProjectGroup(const AProjectPath: String): TNEDProject;
begin

end;

function TNEDWorkspaceManager.OpenFile(const AFilePath: String): TNEDProject;
begin

end;

end.

