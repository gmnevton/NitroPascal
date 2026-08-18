//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-07
// All rights reserved.
//

unit ned_projects;

interface

uses
  SysUtils,
  Generics.Collections,
  JSON.VerySimple,
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
    FID: String;
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
  public
    constructor Create(const AStorage: TJSONVerySimple; const ANode: TJSONNode);
    destructor Destroy; override;
    //
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

constructor TNEDProject.Create(const AStorage: TJSONVerySimple; const ANode: TJSONNode);
var
  pID: String;
  pType: TNEDProjectTypeEnum;
  pName: String;
  pFileName: String;
  pFilePath: String;
  pDescription: String;
  pCreateDate: TDateTime;
  pModifyDate: TDateTime;
  pLastOpenedDate: TDateTime;
  pTimesOpened: Integer;
  pTimeUsedMinutes: Integer;
begin
  FEditors := TObjectList<TNEDEditorContext>.Create(True);
  //
  pID := AStorage.NodeAsString(ANode, 'ID', '');
  pType := TNEDProjectType.StringToProjectType(AStorage.NodeAsString(ANode, 'Type', 'unknown'));
  pName := AStorage.NodeAsString(ANode, 'Name', '');
  pFileName := AStorage.NodeAsString(ANode, 'FileName', '');
  pFilePath := AStorage.NodeAsString(ANode, 'FilePath', '');
  pDescription := AStorage.NodeAsString(ANode, 'Description', '');
  pCreateDate := AStorage.NodeAsDateTime(ANode, 'CreateDate', 0);
  pModifyDate := AStorage.NodeAsDateTime(ANode, 'ModifyDate', 0);
  pLastOpenedDate := AStorage.NodeAsDateTime(ANode, 'LastOpenedDate', 0);
  pTimesOpened := AStorage.NodeAsInteger(ANode, 'TimesOpened', 0);
  pTimeUsedMinutes := AStorage.NodeAsInteger(ANode, 'TimeUsedMinutes', 0);
  //
  FID := pID;
  FType := pType;
  FName := pName;
  FFileName := pFileName;
  FFilePath := pFilePath;
  FDescription := pDescription;
  FCreateDate := pCreateDate;
  FModifyDate := pModifyDate;
  FLastOpenedDate := pLastOpenedDate;
  FTimesOpened := pTimesOpened;
  FTimeUsedMinutes := pTimeUsedMinutes;
  //
  pID := '';
  pName := '';
  pFileName := '';
  pFilePath := '';
  pDescription := '';
end;

destructor TNEDProject.Destroy;
begin
  FEditors.Free;
  inherited;
end;

end.

