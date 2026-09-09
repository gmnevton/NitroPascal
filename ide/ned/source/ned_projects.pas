//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-08
// All rights reserved.
//

unit ned_projects;

interface

uses
  SysUtils,
  ExtCtrls,
  Generics.Collections,
  JSON.VerySimple,
  uFolders,
  ned_common_simple_types,
  ned_editor_context;

type
  TNEDProject = class;

  TNEDProjectFile = class
  private
    FProject: TNEDProject;
    //
    FName: String;
    FFileName: String;
    FFilePath: String;
    FFullFilePath: String;
    FCreateDate: TDateTime;
    FModifyDate: TDateTime;
    FLastOpenedDate: TDateTime;
    FTimesOpened: Integer;
    FTimeUsedMinutes: Integer;
    //
    FModified: Boolean;
    FEditorContextReference: TNEDEditorContext;
    FTimer: TTimer;
    //
    procedure TimerTick(Sender: TObject);
  protected
    function GetFullFilePath: String;
  public
    constructor Create(const AOwner: TNEDProject);
    destructor Destroy; override;
    //
    procedure Load(const AStorage: TJSONVerySimple; const ANode: TJSONNode);
    //
    property Name: String read FName;
    property FileName: String read FFileName;
    property FilePath: String read FFilePath;
    property FullFilePath: String read GetFullFilePath;
    property CreateDate: TDateTime read FCreateDate;
    property ModifyDate: TDateTime read FModifyDate;
    property LastOpenedDate: TDateTime read FLastOpenedDate;
    property TimesOpened: Integer read FTimesOpened;
    property TimeUsedMinutes: Integer read FTimeUsedMinutes;
  end;

  TNEDProjectTypeEnum = (
    ptUnknown,
    ptProjectGroup,
    ptProject,
    ptFile
  );

  TNEDProjectType = class
  public
    class function ProjectTypeToString(const AValue: TNEDProjectTypeEnum): String;
    class function StringToProjectType(AValue: String): TNEDProjectTypeEnum;
  end;

  TNEDProject = class
  private
    FID: TNEDUniqueID;
    FType: TNEDProjectTypeEnum;
    FName: String;
    FFileName: String;
    FFilePath: String;
    FFullFilePath: String;
    FDescription: String;
    FCreateDate: TDateTime;
    FModifyDate: TDateTime;
    FLastOpenedDate: TDateTime;
    FTimesOpened: Integer;
    FTimeUsedMinutes: Integer;
    //
    FProjectEntry,
    FConfigEntry,
    FFilesEntry: TEntryItem;
    //
    FModified: Boolean;
    FFiles: TObjectList<TNEDProjectFile>;
    FEditors: TObjectList<TNEDEditorContext>; // owns TNEDEditorContext
    FTimer: TTimer;
    //
    procedure TimerTick(Sender: TObject);
  protected
    procedure ReadFiles(const AStorage: TJSONVerySimple; const ARootNode: TJSONNode);
    procedure ReadFile(const AStorage: TJSONVerySimple; const ANode: TJSONNode; const AList: TObjectList<TNEDProjectFile>);
    procedure ListFiles(const AFilesEntry: TEntryItem);
    function GetFullFilePath: String;
    function GetFilesCount: Integer;
    function GetFile(const Index: Integer): TNEDProjectFile;
    function GetEditorsCount: Integer;
    function GetEditor(const Index: Integer): TNEDEditorContext;
  public
    constructor Create; overload;
    constructor Create(const AStorage: TJSONVerySimple; const ANode: TJSONNode); overload;
    constructor Create(const AType: TNEDProjectTypeEnum; const AProjectPath: String); overload;
    constructor Create(const AType: TNEDProjectTypeEnum; const AProjectPath, AProjectName: String); overload;
    destructor Destroy; override;
    //
    function  CreateWorkspaceEntries(const AProjectGroup: TEntryItem): TEntryItem;
    function  ContainsFile(const AFilePath: String): Boolean;
    procedure AddEditor(const AEditorContext: TNEDEditorContext);
    procedure OpenEditors;
    procedure OpenEditor(const AEditorContext: TNEDEditorContext);
    procedure CloseEditors;
    procedure CloseEditor(const AEditorContext: TNEDEditorContext);
    procedure Close;
    procedure Touch;
    procedure Save;
    //
    property ID: TNEDUniqueID read FID;
    property &Type: TNEDProjectTypeEnum read FType;
    property Name: String read FName;
    property FileName: String read FFileName;
    property FilePath: String read FFilePath;
    property FullFilePath: String read GetFullFilePath;
    property Description: String read FDescription;
    property CreateDate: TDateTime read FCreateDate;
    property ModifyDate: TDateTime read FModifyDate;
    property LastOpenedDate: TDateTime read FLastOpenedDate;
    property TimesOpened: Integer read FTimesOpened;
    property TimeUsedMinutes: Integer read FTimeUsedMinutes;
    //
    property ProjectEntry: TEntryItem read FProjectEntry;
    property ConfigEntry: TEntryItem read FConfigEntry;
    property FilesEntry: TEntryItem read FFilesEntry;
    //
    property FilesCount: Integer read GetFilesCount;
    property &File[const Index: Integer]: TNEDProjectFile read GetFile;
    property EditorsCount: Integer read GetEditorsCount;
    property Editor[const Index: Integer]: TNEDEditorContext read GetEditor;
  end;

implementation

uses
  ned_home_page,
  ned_json_config_utils;

{ TNEDProjectFile }

constructor TNEDProjectFile.Create(const AOwner: TNEDProject);
begin
  FProject := AOwner;
  //
  FEditorContextReference := Nil;
  //
  FTimer := TTimer.Create(Nil);
  FTimer.Enabled := False;
  FTimer.Interval := 60 * 1000; // 1 minute
  FTimer.OnTimer := TimerTick;
  //
  FName := '';
  FFileName := '';
  FFilePath := '';
  FFullFilePath := '';
  FCreateDate := 0;
  FModifyDate := 0;
  FLastOpenedDate := 0;
  FTimesOpened := 0;
  FTimeUsedMinutes := 0;
  //
  FModified := False;
end;

destructor TNEDProjectFile.Destroy;
begin
  FName := '';
  FFileName := '';
  FFilePath := '';
  FFullFilePath := '';
  FProject := Nil;
  FEditorContextReference := Nil;
  FTimer.Free;
  inherited;
end;

procedure TNEDProjectFile.Load(const AStorage: TJSONVerySimple; const ANode: TJSONNode);
begin
//  FID := AStorage.NodeAsString(ANode, 'ID', '');
//  FType := TNEDProjectType.StringToProjectType(AStorage.NodeAsString(ANode, 'Type', 'unknown'));
  FName := AStorage.NodeAsString(ANode, 'Name', '');
  FFileName := AStorage.NodeAsString(ANode, 'FileName', '');
  FFilePath := AStorage.NodeAsString(ANode, 'FilePath', '');
//  FDescription := AStorage.NodeAsString(ANode, 'Description', '');
  FCreateDate := AStorage.NodeAsDateTime(ANode, 'CreateDate', 0);
  FModifyDate := AStorage.NodeAsDateTime(ANode, 'ModifyDate', 0);
  FLastOpenedDate := AStorage.NodeAsDateTime(ANode, 'LastOpenedDate', 0);
  FTimesOpened := AStorage.NodeAsInteger(ANode, 'TimesOpened', 0);
  FTimeUsedMinutes := AStorage.NodeAsInteger(ANode, 'TimeUsedMinutes', 0);
end;

function TNEDProjectFile.GetFullFilePath: String;
begin
  if Length(FFullFilePath) = 0 then
    FFullFilePath := IncludeTrailingPathDelimiter(FFilePath) + FFileName;
  Result := FFullFilePath;
end;

procedure TNEDProjectFile.TimerTick(Sender: TObject);
begin
  Inc(FTimeUsedMinutes);
//  Save;
end;

{ TNEDProjectType }

class function TNEDProjectType.ProjectTypeToString(const AValue: TNEDProjectTypeEnum): String;
begin
  Result := 'unknown';
  if AValue = ptProject then
    Result := 'project'
  else if AValue = ptProjectGroup then
    Result := 'group'
  else if AValue = ptFile then
    Result := 'file';
end;

class function TNEDProjectType.StringToProjectType(AValue: String): TNEDProjectTypeEnum;
begin
  Result := ptUnknown;
  AValue := LowerCase(AValue);
  if AValue = 'project' then
    Result := ptProject
  else if AValue = 'group' then
    Result := ptProjectGroup
  else if AValue = 'file' then
    Result := ptFile;
end;

{ TNEDProject }

constructor TNEDProject.Create;
begin
  FFiles := TObjectList<TNEDProjectFile>.Create(True);
  FEditors := TObjectList<TNEDEditorContext>.Create(True); // owner of TNEDEditorContext
  FTimer := TTimer.Create(Nil);
  FTimer.Enabled := False;
  FTimer.Interval := 60 * 1000; // 1 minute
  FTimer.OnTimer := TimerTick;
  //
  FID := '';
  FType := ptUnknown;
  FName := '';
  FFileName := '';
  FFilePath := '';
  FFullFilePath := '';
  FDescription := '';
  FCreateDate := 0;
  FModifyDate := 0;
  FLastOpenedDate := 0;
  FTimesOpened := 0;
  FTimeUsedMinutes := 0;
  //
  FProjectEntry := Nil;
  FConfigEntry := Nil;
  FFilesEntry := Nil;
  //
  FModified := False;
end;

constructor TNEDProject.Create(const AStorage: TJSONVerySimple; const ANode: TJSONNode);
begin
  Create;
  //
  FID := AStorage.NodeAsString(ANode, 'ID', '');
  FType := TNEDProjectType.StringToProjectType(AStorage.NodeAsString(ANode, 'Type', 'unknown'));
  FName := AStorage.NodeAsString(ANode, 'Name', '');
  FFileName := AStorage.NodeAsString(ANode, 'FileName', '');
  FFilePath := AStorage.NodeAsString(ANode, 'FilePath', '');
  FDescription := AStorage.NodeAsString(ANode, 'Description', '');
  FCreateDate := AStorage.NodeAsDateTime(ANode, 'CreateDate', 0);
  FModifyDate := AStorage.NodeAsDateTime(ANode, 'ModifyDate', 0);
  FLastOpenedDate := AStorage.NodeAsDateTime(ANode, 'LastOpenedDate', 0);
  FTimesOpened := AStorage.NodeAsInteger(ANode, 'TimesOpened', 0);
  FTimeUsedMinutes := AStorage.NodeAsInteger(ANode, 'TimeUsedMinutes', 0);
  //
  ReadFiles(AStorage, ANode.FindNode('Files', [jtObject]));
end;

constructor TNEDProject.Create(const AType: TNEDProjectTypeEnum; const AProjectPath: String);
begin
  Create;
  //
  FID := NewGUID;
  FType := AType;
  FFileName := ExtractFileName(AProjectPath);
  FFilePath := ExtractFilePath(AProjectPath);
  FCreateDate := Now;
  FModifyDate := FCreateDate;
  FLastOpenedDate := FCreateDate;
  FTimesOpened := 0;
  //
  FName := '';
  FDescription := '';
end;

constructor TNEDProject.Create(const AType: TNEDProjectTypeEnum; const AProjectPath, AProjectName: String);
begin
  Create;
  //
  FID := NewGUID;
  FType := AType;
  FFileName := ExtractFileName(AProjectPath);
  FFilePath := ExtractFilePath(AProjectPath);
  FCreateDate := Now;
  FModifyDate := FCreateDate;
  FLastOpenedDate := FCreateDate;
  FTimesOpened := 0;
  //
  FName := AProjectName;
  FDescription := '';
end;

destructor TNEDProject.Destroy;
begin
  FID := '';
  FName := '';
  FFileName := '';
  FFilePath := '';
  FFullFilePath := '';
  FTimer.Enabled := False;
  FTimer.Free;
  FFiles.Free;
  FEditors.Free;
  inherited;
end;

procedure TNEDProject.TimerTick(Sender: TObject);
begin
  Inc(FTimeUsedMinutes);
  Save;
  NEDHomeForm.RefreshProjects;
end;

procedure TNEDProject.ReadFiles(const AStorage: TJSONVerySimple; const ARootNode: TJSONNode);
var
  List, Node: TJSONNode;
  LCount: Integer;
begin
  if ARootNode <> Nil then begin
    LCount := AStorage.NodeAsInteger(ARootNode, 'Count', 0);
    List := ARootNode.FindNode('List', [jtArray]);
    if List <> Nil then begin
      if List.HasChildNodes and (List.ChildNodes.Count = LCount) then begin
        Node := List.FirstChild;
        while Node <> Nil do begin
          ReadFile(AStorage, Node, FFiles);
          Node := Node.NextSibling;
        end;
      end;
    end;
  end;
end;

procedure TNEDProject.ReadFile(const AStorage: TJSONVerySimple; const ANode: TJSONNode; const AList: TObjectList<TNEDProjectFile>);
var
  //
  ProjectFile: TNEDProjectFile;
begin
  if ANode = Nil then
    Exit;
  //
  ProjectFile := Nil; // satisfy compiler
  try
    ProjectFile := TNEDProjectFile.Create(Self);
    ProjectFile.Load(AStorage, ANode);
    AList.Add(ProjectFile);
  except
    if ProjectFile <> Nil then
      ProjectFile.Free;
    raise;
  end;
end;

procedure TNEDProject.ListFiles(const AFilesEntry: TEntryItem);
var
  i: Integer;
  Entry: TEntryItem;
  ProjectFile: TNEDProjectFile;
begin
  AFilesEntry.Items.Clear;
  for i := 0 to Self.FilesCount - 1 do begin
    ProjectFile := Self.&File[i];
    Entry := AFilesEntry.Items.Add;
    Entry.Caption := ProjectFile.FileName;
    if SameText(ExtractFileExt(ProjectFile.FileName), '.npe') then
      Entry.ImageChar := Char($E943) // Code
    else
      Entry.ImageChar := Char($F000); // KnowledgeArticle
    Entry.Data := ProjectFile;
  end;
  AFilesEntry.Expand;
end;

function TNEDProject.GetFullFilePath: String;
begin
  if Length(FFullFilePath) = 0 then
    FFullFilePath := IncludeTrailingPathDelimiter(FFilePath) + FFileName;
  Result := FFullFilePath;
end;

function TNEDProject.GetFilesCount: Integer;
begin
  Result := FFiles.Count;
end;

function TNEDProject.GetFile(const Index: Integer): TNEDProjectFile;
begin
  Result := Nil;
  if (FFiles.Count > 0) and (Index >= 0) and (Index < FFiles.Count) then
    Result := FFiles.Items[Index];
end;

function TNEDProject.GetEditorsCount: Integer;
begin
  Result := FEditors.Count;
end;

function TNEDProject.GetEditor(const Index: Integer): TNEDEditorContext;
begin
  Result := Nil;
  if (FEditors.Count > 0) and (Index >= 0) and (Index < FEditors.Count) then
    Result := FEditors.Items[Index];
end;

function TNEDProject.CreateWorkspaceEntries(const AProjectGroup: TEntryItem): TEntryItem;
begin
  Assert(AProjectGroup <> Nil);
  if FProjectEntry = Nil then begin
    FProjectEntry := AProjectGroup.Items.Add;
    FProjectEntry.Caption := 'Project ' + Self.FileName + ' - ' + Self.Name; // ExtractFileName(AProjectPath)
    FProjectEntry.ImageChar := Char($E9F9); // ReportDocument
    FProjectEntry.Data := Self;
    //Result.DataObject := True;
  end;
  Result := FProjectEntry;
  //
  if FConfigEntry = Nil then begin
    FConfigEntry := FProjectEntry.Items.Add;
    FConfigEntry.Caption := 'Config';
    FConfigEntry.ImageChar := Char($E713); // Setting
    FConfigEntry.Data := Self;
  end;
  //
  if FFilesEntry = Nil then begin
    FFilesEntry := FProjectEntry.Items.Add;
    FFilesEntry.Caption := 'Files';
    FFilesEntry.ImageChar := Char($F12B); // FolderHorizontal
    FFilesEntry.Data := Self;
  end;
  ListFiles(FFilesEntry);
end;

function TNEDProject.ContainsFile(const AFilePath: String): Boolean;
var
  i: Integer;
  ProjectFile: TNEDProjectFile;
begin
  Result := False;
  for i := 0 to FFiles.Count - 1 do begin
    ProjectFile := FFiles.Items[i];
    if SameText(ProjectFile.FullFilePath, AFilePath) then begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TNEDProject.AddEditor(const AEditorContext: TNEDEditorContext);
begin
  FEditors.Add(AEditorContext);
end;

procedure TNEDProject.OpenEditors;
var
  i: Integer;
begin
  for i := 0 to FEditors.Count - 1 do begin
    OpenEditor(FEditors.Items[i]);
  end;
end;

procedure TNEDProject.OpenEditor(const AEditorContext: TNEDEditorContext);
begin

end;

procedure TNEDProject.CloseEditors;
var
  i: Integer;
begin
  for i := 0 to FEditors.Count - 1 do begin
    CloseEditor(FEditors.Items[i]);
  end;
end;

procedure TNEDProject.CloseEditor(const AEditorContext: TNEDEditorContext);
begin

end;

procedure TNEDProject.Close;
begin
  FTimer.Enabled := False;
  CloseEditors;
end;

procedure TNEDProject.Touch;
begin
  FLastOpenedDate := Now;
  Inc(FTimesOpened);
  FTimer.Enabled := True;
  Save;
  NEDHomeForm.RefreshProjects;
end;

procedure TNEDProject.Save;
begin
  try

  except

  end;
end;

end.

