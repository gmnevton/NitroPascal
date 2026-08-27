//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-07
// All rights reserved.
//

unit ned_session_context;

interface

uses
  SysUtils,
  Generics.Collections,
  JSON.VerySimple,
  ned_projects;

type
  TNEDLayout = class
  private
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Clear;
  end;

  TNEDSettings = class
  private
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Clear;
  end;

  TNEDSessionContext = class
  private
    FContextID: String; // same as TNEDProfile.ID
    FName: String; // same as TNEDProfile.Name
    FCopiedFrom: String; // same as TNEDProfile.OriginateFrom
    FCreateDate: TDateTime;
    FModifyDate: TDateTime;
    FLastOpenedDate: TDateTime;
    FTimesOpened: Integer;
    FTimeUsedMinutes: Integer;
    //
    FStorage: TJSONVerySimple;
    FStorageFileName: String;
    FLoaded: Boolean;
    FError: Boolean;
    FErrorMsg: String;
  private
    FFavorites: TObjectList<TNEDProject>;
    FRecents: TObjectList<TNEDProject>;
    FLayout: TNEDLayout;
    FSettings: TNEDSettings;
    FProjects: TObjectList<TNEDProject>;
    //
    function GetFavorite(const AIndex: Integer): TNEDProject;
    function GetRecent(const AIndex: Integer): TNEDProject;
    function GetProject(const AIndex: Integer): TNEDProject;
  private
    procedure ReadProfileHeader;
    procedure ReadContext;
    procedure ReadFavorites(const ARootNode: TJSONNode);
    procedure ReadRecents(const ARootNode: TJSONNode);
    procedure ReadWorkspace;
    procedure ReadProject(const ANode: TJSONNode; const AList: TObjectList<TNEDProject>);
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure ContextClear;
    function ContextFavoritesCount: Integer;
    function ContextRecentsCount: Integer;
    function ContextAdd(const AProjectID, AName: String): TNEDProject;
    function ContextAddNew(const AName: String): TNEDProject;
    function ContextRemove(const AIndex: Integer): Boolean;
    function ContextMove(const AFromIndex, AToIndex: Integer): Boolean;
    function FindFavorite(const AType: TNEDProjectTypeEnum; const AFilePath: String): TNEDProject;
    function FindRecent(const AType: TNEDProjectTypeEnum; const AFilePath: String): TNEDProject;
    //
    procedure LoadContext(const AProfilePath: String); // read profile favorites and recents, IDE layout and settings
    procedure LoadSession(const ASessionPath: String); // read what was opened
    procedure LoadProject(const AProjectPath: String); // read opened projects
    procedure LoadFile(const AFilePath: String); // read opened file that was not part of a project
    //
    property ContextID: String read FContextID;
    property Name: String read FName;
    property OriginateFrom: String read FCopiedFrom;
    property CreateDate: TDateTime read FCreateDate;
    property ModifyDate: TDateTime read FModifyDate;
    property LastOpenedDate: TDateTime read FLastOpenedDate;
    property TimesOpened: Integer read FTimesOpened;
    property TimeUsedMinutes: Integer read FTimeUsedMinutes;
    //
    property Loaded: Boolean read FLoaded;
    property Error: Boolean read FError;
    property ErrorMsg: String read FErrorMsg;
    //
    property Favorite[const AIndex: Integer]: TNEDProject read GetFavorite;
    property Recent[const AIndex: Integer]: TNEDProject read GetRecent;
    property Project[const AIndex: Integer]: TNEDProject read GetProject;
  end;

implementation

uses
  ned_json_config_utils;

{ TNEDLayout }

constructor TNEDLayout.Create;
begin

end;

destructor TNEDLayout.Destroy;
begin

  inherited;
end;

procedure TNEDLayout.Clear;
begin

end;

{ TNEDSettings }

constructor TNEDSettings.Create;
begin

end;

destructor TNEDSettings.Destroy;
begin

  inherited;
end;

procedure TNEDSettings.Clear;
begin

end;

{ TNEDSessionContext }

constructor TNEDSessionContext.Create;
begin
  FStorage := TJSONVerySimple.Create;
  FStorageFileName := '';
  FLoaded := False;
  FError := False;
  FErrorMsg := '';
  //
  FFavorites := TObjectList<TNEDProject>.Create(True);
  FRecents := TObjectList<TNEDProject>.Create(True);
  FLayout := TNEDLayout.Create;
  FSettings := TNEDSettings.Create;
end;

destructor TNEDSessionContext.Destroy;
begin
  FErrorMsg := '';
  //
  FFavorites.Free;
  FRecents.Free;
  FLayout.Free;
  FSettings.Free;
  //
  if FProjects <> Nil then
    FProjects.Free;
  FStorageFileName := '';
  FStorage.Free;
  inherited;
end;

function TNEDSessionContext.GetFavorite(const AIndex: Integer): TNEDProject;
begin
  Result := Nil;
  if (FFavorites.Count > 0) and (AIndex >= 0) and (AIndex < FFavorites.Count) then
    Result := FFavorites.Items[AIndex];
end;

function TNEDSessionContext.GetRecent(const AIndex: Integer): TNEDProject;
begin
  Result := Nil;
  if (FRecents.Count > 0) and (AIndex >= 0) and (AIndex < FRecents.Count) then
    Result := FRecents.Items[AIndex];
end;

function TNEDSessionContext.GetProject(const AIndex: Integer): TNEDProject;
begin
  Result := Nil;
  if (FProjects.Count > 0) and (AIndex >= 0) and (AIndex < FProjects.Count) then
    Result := FProjects.Items[AIndex];
end;

procedure TNEDSessionContext.ReadProfileHeader;
var
  DocRoot: TJSONNode;
begin
  DocRoot := FStorage.DocumentElement;
  if DocRoot <> Nil then begin
    FContextID := FStorage.NodeAsString(DocRoot, 'ID', '');
    FName := FStorage.NodeAsString(DocRoot, 'Name', '');
    FCopiedFrom := FStorage.NodeAsString(DocRoot, 'OriginateFrom', '');
    FCreateDate := FStorage.NodeAsInteger(DocRoot, 'CreateDate', 0);
    FModifyDate := FStorage.NodeAsInteger(DocRoot, 'ModifyDate', 0);
    FLastOpenedDate := FStorage.NodeAsInteger(DocRoot, 'LastOpenedDate', 0);
    FTimesOpened := FStorage.NodeAsInteger(DocRoot, 'TimesOpened', 0);
    FTimeUsedMinutes := FStorage.NodeAsInteger(DocRoot, 'TimeUsedMinutes', 0);
  end;
end;

procedure TNEDSessionContext.ReadContext;
var
  DocRoot, Root, SubRoot: TJSONNode;
begin
  DocRoot := FStorage.DocumentElement;
  if DocRoot <> Nil then begin
    Root := DocRoot.FindNode('Context', [jtObject]);
    if Root <> Nil then begin
      SubRoot := Root.FindNode('Favorites', [jtObject]);
      ReadFavorites(SubRoot);
      //
      SubRoot := Root.FindNode('Recents', [jtObject]);
      ReadRecents(SubRoot);
      //
      SubRoot := Root.FindNode('Layout', [jtObject]);
      //
      SubRoot := Root.FindNode('Settings', [jtObject]);
    end;
  end;
end;

procedure TNEDSessionContext.ReadFavorites(const ARootNode: TJSONNode);
var
  List, Node: TJSONNode;
  LCount: Integer;
begin
  if ARootNode <> Nil then begin
    LCount := FStorage.NodeAsInteger(ARootNode, 'Count', 0);
    List := ARootNode.FindNode('List', [jtArray]);
    if List <> Nil then begin
      if List.HasChildNodes and (List.ChildNodes.Count <> LCount) then begin // list not complete
        FError := True;
        FErrorMsg := Trim(FErrorMsg + #13#10 + Format('Favorites list elements count mismatch, expected: %d, but got: %d.', [LCount, List.ChildNodes.Count]));
        //
        Exit;
      end;
      //
      Node := List.FirstChild;
      while Node <> Nil do begin
        ReadProject(Node, FFavorites);
        Node := Node.NextSibling;
      end;
    end;
  end;
end;

procedure TNEDSessionContext.ReadRecents(const ARootNode: TJSONNode);
var
  List, Node: TJSONNode;
  LCount: Integer;
begin
  if ARootNode <> Nil then begin
    LCount := FStorage.NodeAsInteger(ARootNode, 'Count', 0);
    List := ARootNode.FindNode('List', [jtArray]);
    if List <> Nil then begin
      if List.HasChildNodes and (List.ChildNodes.Count <> LCount) then begin // list not complete
        FError := True;
        FErrorMsg := Trim(FErrorMsg + #13#10 + Format('Recents list elements count mismatch, expected: %d, but got: %d.', [LCount, List.ChildNodes.Count]));
        //
        Exit;
      end;
      //
      Node := List.FirstChild;
      while Node <> Nil do begin
        ReadProject(Node, FRecents);
        Node := Node.NextSibling;
      end;
    end;
  end;
end;

procedure TNEDSessionContext.ReadWorkspace;
begin

end;

procedure TNEDSessionContext.ReadProject(const ANode: TJSONNode; const AList: TObjectList<TNEDProject>);
var
  //
  Project: TNEDProject;
begin
  if ANode = Nil then
    Exit;
  //
  try
    Project := TNEDProject.Create(FStorage, ANode);
    AList.Add(Project);
  except
    if Project <> Nil then
      Project.Free;
    raise;
  end;
end;

procedure TNEDSessionContext.ContextClear;
begin
  FFavorites.Clear;
  FRecents.Clear;
  FLayout.Clear;
  FSettings.Clear;
  //FProjects.Clear;
end;

//Result := FProjects.Count;

function TNEDSessionContext.ContextFavoritesCount: Integer;
begin
  Result := FFavorites.Count;
end;

function TNEDSessionContext.ContextRecentsCount: Integer;
begin
  Result := FRecents.Count;
end;

function TNEDSessionContext.ContextAdd(const AProjectID, AName: String): TNEDProject;
begin
  try
    Result := Nil;
//    Result := TNEDProject.Create(FStorage, ANode);
//    Result.FID := AID;
//    Result.FIndex :=
//    FProjects.Add(Result);
  except
    if Result <> Nil then
      Result.Free;
    Result := Nil;
  end;
end;

function TNEDSessionContext.ContextAddNew(const AName: String): TNEDProject;
begin
  try
    Result := Nil;
//    Result := TNEDProject.Create(FStorage, ANode); // (AName, True);
//    Result.FIndex :=
//    FProjects.Add(Result);
//    if ASetAsDefault then
//      FDefaultIndex := Result.FIndex;
  except
    if Result <> Nil then
      Result.Free;
    Result := Nil;
  end;
end;

function TNEDSessionContext.ContextRemove(const AIndex: Integer): Boolean;
begin
  Result := False;
  if (FProjects.Count > 0) and (AIndex >= 0) and (AIndex < FProjects.Count) then begin
    FProjects.Delete(AIndex);
    Result := True;
  end;
end;

function TNEDSessionContext.ContextMove(const AFromIndex, AToIndex: Integer): Boolean;
begin
  Result := False;
  if (FProjects.Count > 0) and
     (AFromIndex <> AToIndex) and
     (AFromIndex >= 0) and (AFromIndex < FProjects.Count) and
     (AToIndex >= 0) and (AToIndex < FProjects.Count)
  then begin
    FProjects.Move(AFromIndex, AToIndex);
    Result := True;
  end;
end;

function TNEDSessionContext.FindFavorite(const AType: TNEDProjectTypeEnum; const AFilePath: String): TNEDProject;
var
  i: Integer;
  temp_file_path: String;
begin
  Result := Nil;
  for i := 0 to FFavorites.Count - 1 do begin
    temp_file_path := IncludeTrailingPathDelimiter(FFavorites.Items[i].FilePath) + FFavorites.Items[i].FileName;
    try
      if (FFavorites.Items[i].&Type = AType) and SameText(temp_file_path, AFilePath) then begin
        Result := FFavorites.Items[i];
        Exit;
      end;
    finally
      temp_file_path := '';
    end;
  end;
end;

function TNEDSessionContext.FindRecent(const AType: TNEDProjectTypeEnum; const AFilePath: String): TNEDProject;
var
  i: Integer;
  temp_file_path: String;
begin
  Result := Nil;
  for i := 0 to FRecents.Count - 1 do begin
    temp_file_path := IncludeTrailingPathDelimiter(FRecents.Items[i].FilePath) + FRecents.Items[i].FileName;
    try
      if (FRecents.Items[i].&Type = AType) and SameText(temp_file_path, AFilePath) then begin
        Result := FRecents.Items[i];
        Exit;
      end;
    finally
      temp_file_path := '';
    end;
  end;
end;

//    FProjects: TObjectList<TNEDProject>;
//    //
//    FFavorites: TObjectList<TNEDProject>;
//    FRecent: TObjectList<TNEDProject>;
//    FLayout: TNEDLayout;
//    FSettings: TNEDSettings;

procedure TNEDSessionContext.LoadContext(const AProfilePath: String);
begin
  try
    FError := False;
    FErrorMsg := '';
    ContextClear;
    FStorage.Clear;
    FStorageFileName := IncludeTrailingPathDelimiter(AProfilePath) + 'profile.jcfg';
    FStorage.LoadFromFile(FStorageFileName);
    FLoaded := FStorage.Empty;
    if FLoaded then begin
      ReadProfileHeader;
      ReadContext;
      ReadWorkspace;
    end;
  except
    // no error here
    on E: Exception do begin
      FError := True;
      FErrorMsg := E.ClassName + ': ' + E.Message;
    end;
  end;
end;

procedure TNEDSessionContext.LoadSession(const ASessionPath: String);
begin

end;

procedure TNEDSessionContext.LoadProject(const AProjectPath: String);
begin
  FProjects := TObjectList<TNEDProject>.Create(True);

end;

procedure TNEDSessionContext.LoadFile(const AFilePath: String);
begin

end;

end.

