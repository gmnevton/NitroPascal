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
  Controls,
  Messages,
  UITypes,
  uFolders,
  ned_session_context,
  ned_projects,
  ned_source_view,
  ned_splitview_manager,
  ned_dialog_message;

const
  WM_NED_CLEAR_WORKSPACE = WM_USER + 1;

type
  TNEDMessageClearWorkspace = record
    Buttons: TNEDDialogButtons;
    Title: String;
    Question: String;
  end;

  TNEDWorkspaceManager = class;

  TNEDWorkspace = class
  private
    FWorkspaceManager: TNEDWorkspaceManager;
    FEmpty: Boolean;
    FWorkspaceEntries: TEntryView; // owned by TNEDMainForm.boxWorkSpace object
    FViewForm: TNEDViewForm; // owned by TNEDMainForm.pnlWorkSpace object
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
  protected
  public
    constructor Create(const Owner: TNEDWorkspaceManager);
    destructor Destroy; override;
    //
    procedure CreateEntries(const AParentControl: TWinControl);
    procedure CreateViewForm(const AParentControl: TWinControl);
    procedure DestroyControls;
    procedure Clear;
    //
    function CreateProjectGroup(const AGroupName: String): TEntryItem;
    function AddProject(const AProjectGroup: TEntryItem; const AProjectPath: String): TEntryItem;
    function GetProjectGroup: TEntryItem;
    //
    property Empty: Boolean read FEmpty;
    property Entries: TEntryView read FWorkspaceEntries;
    property ViewForm: TNEDViewForm read FViewForm;
  end;

  TNEDWorkspaceManager = class
  private
    FWorkspace: TNEDWorkspace;
    //
    function GetViewForm: TNEDViewForm;
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure CreateWorkspace(const AParentWorkspaceEntriesControl: TWinControl; const AParentWorkspaceViewControl: TWinControl);
    procedure DestroyWorkspace;
    procedure ClearWorkspace;
    //
    function NewProject(const AName: String): TNEDProject;
    function NewProjectGroup(const AName: String): TNEDProject;
    //
    function AddProject(const AProject: TNEDProject): TNEDProject;
    function AddProjectGroup(const AProject: TNEDProject): TNEDProject;
    //
    function Open(const AFilePath: String; const SplitType: TNEDSplitViewTypeEnum = stSplitNone): Boolean;
    function OpenProjectGroup(const AProjectPath: String): TNEDProject;
    function OpenProject(const AProjectPath: String): TNEDProject;
    function OpenFile(const AFilePath: String): TNEDProject;
    //
    property NEDViewForm: TNEDViewForm read GetViewForm;
  end;

//var
//  NEDWorkspace: TNEDWorkspaceManager;

implementation

uses
  Windows,
  Forms,
  Dialogs,
  ned_home_page,
  ned_editor_context;

{ TNEDWorkspace }

constructor TNEDWorkspace.Create(const Owner: TNEDWorkspaceManager);
begin
  FWorkspaceManager := Owner;
  FEmpty := True;
  FWorkspaceEntries := Nil;
  FViewForm := Nil;
end;

destructor TNEDWorkspace.Destroy;
begin

  inherited;
end;

procedure TNEDWorkspace.CreateEntries(const AParentControl: TWinControl);
begin
  if FWorkspaceEntries = Nil then begin
    FWorkspaceEntries := TEntryView.Create(AParentControl);
    FWorkspaceEntries.Parent := AParentControl;
    FWorkspaceEntries.Align := alClient;
    FWorkspaceEntries.ParentColor := True;
    FWorkspaceEntries.BorderStyle := bsNone;
    //FWorkspaceEntries.ActiveColor := ;
    //FWorkspaceEntries.SelectedColor := ;
    //FWorkspaceEntries.EntryImages := ImageList1;
    FWorkspaceEntries.TabStop := True;
    //FWorkspaceEntries.OnChange := FilesViewChanged;
    //FWorkspaceEntries.OnItemGetType := FilesViewItemGetType;
    //FWorkspaceEntries.OnItemSelection := FilesViewItemSelection;
    FWorkspaceEntries.Show;
  end;
end;

procedure TNEDWorkspace.CreateViewForm(const AParentControl: TWinControl);
begin
  if FViewForm = Nil then begin
    FViewForm := TNEDViewForm.Create(AParentControl);
    FViewForm.Parent := AParentControl;
    FViewForm.Align := alClient;
    FViewForm.Show;
  end;
end;

procedure TNEDWorkspace.DestroyControls;
begin
  if FWorkspaceEntries <> Nil then
    FWorkspaceEntries.Free;
  FWorkspaceEntries := Nil;
  //
  if FViewForm <> Nil then
    FViewForm.Free;
  FViewForm := Nil;
  //
  FEmpty := True;
end;

procedure TNEDWorkspace.Clear;
begin
  if FWorkspaceEntries <> Nil then begin
    FWorkspaceEntries.Clear;
  end;
  if FViewForm <> Nil then begin
    FViewForm.CloseEditorViews;
  end;
end;

function TNEDWorkspace.CreateProjectGroup(const AGroupName: String): TEntryItem;
begin
  FWorkspaceEntries.BeginUpdate;
  try
    Result := FWorkspaceEntries.Entries.Add;
    if Length(AGroupName) > 0 then
      Result.Caption := AGroupName
    else
      Result.Caption := 'Unnamed project group';
    FEmpty := False;
  finally
    FWorkspaceEntries.EndUpdate;
  end;
end;

function TNEDWorkspace.AddProject(const AProjectGroup: TEntryItem; const AProjectPath: String): TEntryItem;
var
  Project: TNEDProject;
begin
  FWorkspaceEntries.BeginUpdate;
  try
    Result := AProjectGroup.Items.Add;
    Result.Caption := ExtractFileName(AProjectPath);
    //
    Project := FWorkspaceManager.OpenProject(AProjectPath);
    Result.Data := Project;
    //Result.DataObject := True;
    Project.Touch;
  finally
    FWorkspaceEntries.EndUpdate;
  end;
end;

function TNEDWorkspace.GetProjectGroup: TEntryItem;
begin
  Result := FWorkspaceEntries.GetFirstEntry;
end;

{ TNEDWorkspaceManager }

constructor TNEDWorkspaceManager.Create;
begin
  FWorkspace := TNEDWorkspace.Create(Self);
end;

destructor TNEDWorkspaceManager.Destroy;
begin
  FWorkspace.Free;
  inherited;
end;

function TNEDWorkspaceManager.GetViewForm: TNEDViewForm;
begin
  Result := FWorkspace.ViewForm;
end;

procedure TNEDWorkspaceManager.CreateWorkspace(const AParentWorkspaceEntriesControl: TWinControl; const AParentWorkspaceViewControl: TWinControl);
begin
  FWorkspace.CreateEntries(AParentWorkspaceEntriesControl);
  FWorkspace.CreateViewForm(AParentWorkspaceViewControl);
  //
  NEDViewForm.BringToFront;
end;

procedure TNEDWorkspaceManager.DestroyWorkspace;
begin
  FWorkspace.DestroyControls;
end;

procedure TNEDWorkspaceManager.ClearWorkspace;
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

function TNEDWorkspaceManager.Open(const AFilePath: String; const SplitType: TNEDSplitViewTypeEnum = stSplitNone): Boolean;

  function AskForWorkspaceClearing: Boolean;
  var
    Msg: TNEDMessageClearWorkspace;
  begin
    SetLength(Msg.Buttons, 3);
    FillChar(Msg.Buttons[0], SizeOf(TNEDDialogButtons) * 3, 0);
    Msg.Buttons[0].&Type := mbYes;
    Msg.Buttons[0].Caption := 'Clear';
    Msg.Buttons[1].&Type := mbNo;
    Msg.Buttons[1].Caption := 'Add';
    Msg.Buttons[2].&Type := mbCancel;
    Msg.Buttons[2].Caption := 'Cancel';
    Msg.Title := 'Clear workspace';
    Msg.Question := 'Current workspace is not empty.'#13#10 +
                    'While opening project group "' + ExtractFileName(AFilePath) + '",'#13#10 +
                    'would You like to:'#13#10 +
                    '  [C]lear workspace and open selected project group'#13#10 +
                    'or'#13#10 +
                    '  [A]dd to this workspace ?';
    //
    Result := SendMessage(Application.MainForm.Handle, WM_NED_CLEAR_WORKSPACE, 0, Integer(@Msg)) = 1;
    //
    FillChar(Msg.Buttons[0], SizeOf(TNEDDialogButtons) * 3, 0);
    SetLength(Msg.Buttons, 0);
    Msg.Title := '';
    Msg.Question := '';
  end;

var
  ext: String;
  Project: TNEDProject;
  ProjectGroup, Entry: TEntryItem;
  EditorContext: TNEDEditorContext;
begin
  ext := ExtractFileExt(AFilePath);
  if SameText(ext, '.npe') then begin // NitroPascal project
    if not FWorkspace.Empty then begin
      ProjectGroup := FWorkspace.GetProjectGroup;
      Assert(ProjectGroup <> Nil);
    end
    else begin
      ProjectGroup := FWorkspace.CreateProjectGroup('');
    end;
    Entry := FWorkspace.AddProject(ProjectGroup, AFilePath);
    ProjectGroup.Expand;
    if NEDViewForm.OpenFile(AFilePath, EditorContext, SplitType) then begin
      EditorContext.WorkspaceEntry := Entry;
      TNEDProject(Entry.Data).AddEditor(EditorContext);
    end;
  end
  else if SameText(ext, '.npg') then begin // NitroPascal project group
    if not FWorkspace.Empty and AskForWorkspaceClearing then begin
    end;
    Project := OpenProjectGroup(AFilePath);
    Project := AddProjectGroup(Project);
  end
  else begin
    ProjectGroup := FWorkspace.GetProjectGroup;
    Assert(ProjectGroup <> Nil);
    Entry := FWorkspace.AddProject(ProjectGroup, AFilePath);
    NEDViewForm.OpenFile(AFilePath, EditorContext, SplitType);
    EditorContext.WorkspaceEntry := Entry;
    TNEDProject(Entry.Data).AddEditor(EditorContext);
  end;
end;

function TNEDWorkspaceManager.OpenProjectGroup(const AProjectPath: String): TNEDProject;
begin
  Result := NEDHomeForm.FindProjectGroup(AProjectPath);
  if Result = Nil then
    raise Exception.Create('Not implemented !!!');
//    Result := TNEDProject.Create(ptProjectGroup, AProjectPath);
end;

function TNEDWorkspaceManager.OpenProject(const AProjectPath: String): TNEDProject;
begin
  Result := NEDHomeForm.FindProject(AProjectPath);
  if Result = Nil then
    raise Exception.Create('Not implemented !!!');
    //Result := NEDHomeForm.Profile.  // TNEDProject.Create(ptProject, AProjectPath);
end;

function TNEDWorkspaceManager.OpenFile(const AFilePath: String): TNEDProject;
begin
  Result := TNEDProject.Create(ptFile, AFilePath);
end;

end.

