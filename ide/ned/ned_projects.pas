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
  Generics.Collections,
  JSON.VerySimple,
  ned_common_simple_types,
  ned_editor_context;

type
  TNEDProjectTypeEnum = (
    ptUnknown,
    ptProject,
    ptProjectGroup,
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
    FDescription: String;
    FCreateDate: TDateTime;
    FModifyDate: TDateTime;
    FLastOpenedDate: TDateTime;
    FTimesOpened: Integer;
    FTimeUsedMinutes: Integer;
    //
    FEditors: TObjectList<TNEDEditorContext>;
  protected
    function GetEditorsCount: Integer;
    function GetEditor(const Index: Integer): TNEDEditorContext;
  public
    constructor Create; overload;
    constructor Create(const AStorage: TJSONVerySimple; const ANode: TJSONNode); overload;
    constructor Create(const AType: TNEDProjectTypeEnum; const AProjectPath: String); overload;
    destructor Destroy; override;
    //
    procedure AddEditor(const AEditorContext: TNEDEditorContext);
    procedure OpenEditors;
    procedure OpenEditor(const AEditorContext: TNEDEditorContext);
    procedure CloseEditors;
    procedure CloseEditor(const AEditorContext: TNEDEditorContext);
    procedure Touch;
    //
    property ID: TNEDUniqueID read FID;
    property &Type: TNEDProjectTypeEnum read FType;
    property Name: String read FName;
    property FileName: String read FFileName;
    property FilePath: String read FFilePath;
    property Description: String read FDescription;
    property CreateDate: TDateTime read FCreateDate;
    property ModifyDate: TDateTime read FModifyDate;
    property LastOpenedDate: TDateTime read FLastOpenedDate;
    property TimesOpened: Integer read FTimesOpened;
    property TimeUsedMinutes: Integer read FTimeUsedMinutes;
    //
    property EditorsCount: Integer read GetEditorsCount;
    property Editor[const Index: Integer]: TNEDEditorContext read GetEditor;
  end;

implementation

uses
  ned_json_config_utils;

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
  FEditors := TObjectList<TNEDEditorContext>.Create(True);
  //
  FID := '';
  FType := ptUnknown;
  FName := '';
  FFileName := '';
  FFilePath := '';
  FDescription := '';
  FCreateDate := 0;
  FModifyDate := 0;
  FLastOpenedDate := 0;
  FTimesOpened := 0;
  FTimeUsedMinutes := 0;
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
  FTimesOpened := 1;
  //
  FName := '';
  FDescription := '';
end;

destructor TNEDProject.Destroy;
begin
  FEditors.Free;
  inherited;
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

procedure TNEDProject.Touch;
begin
  FLastOpenedDate := Now;
  Inc(FTimesOpened);
end;

end.

