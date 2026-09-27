//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-07
// All rights reserved.
//

unit ned_config;

interface

uses
  SysUtils,
  Classes,
  Forms,
  Types,
  MultiMon, // HMONITOR type
  JSON.VerySimple,
  ned_profiles;

type
  TNEDConfig = class
  private
    FMonitor: HMONITOR;
    FPosition: TPoint;
    FSize: TSize;
    FMaximize: Boolean;
    FShowHomePage: Boolean;
    FColorSchema: String;
    FScreenSnap: Boolean;
    FScreenSnapBuffer: Word;
    //
    FShowProfileSelection: Boolean;
    FProfilesCount: Integer;
    FProfiles: TNEDProfiles;
    FLastProfile: String;
    FUseLastProfile: Boolean;
    FLoadProfileSession: Boolean;
  private
    FStorage: TJSONVerySimple;
    FLoaded: Boolean;
    FError: Boolean;
    FErrorMsg: String;
    FLocked: Integer;
  private
    procedure Init;
    procedure FieldsToJson;
    procedure JsonToFields;
    procedure ProfilesToJson(const ARootNode: TJSONNode);
    procedure JsonToProfiles(const ARootNode: TJSONNode);
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure LoadConfig;
    procedure SaveConfig;
    //
    procedure Lock;
    procedure Unlock;
    //
    procedure Update;
    //
    property Loaded: Boolean read FLoaded;
    property Error: Boolean read FError;
    property ErrorMsg: String read FErrorMsg;
    //
    property Monitor: HMONITOR read FMonitor write FMonitor;
    property Position: TPoint read FPosition write FPosition;
    property Size: TSize read FSize write FSize;
    property Maximize: Boolean read FMaximize write FMaximize;
    property ShowHomePage: Boolean read FShowHomePage write FShowHomePage;
    property ColorSchema: String read FColorSchema write FColorSchema;
    property ScreenSnap: Boolean read FScreenSnap write FScreenSnap;
    property ScreenSnapBuffer: Word read FScreenSnapBuffer write FScreenSnapBuffer;
    //
    property ShowProfileSelection: Boolean read FShowProfileSelection write FShowProfileSelection;
    property ProfilesCount: Integer read FProfilesCount write FProfilesCount;
    property Profiles: TNEDProfiles read FProfiles;
    property LastProfile: String read FLastProfile write FLastProfile;
    property UseLastProfile: Boolean read FUseLastProfile write FUseLastProfile;
    property LoadProfileSession: Boolean read FLoadProfileSession write FLoadProfileSession;
  end;

var
  NEDConfig: TNEDConfig;

implementation

uses
  ned_json_config_utils;

{ TNEDConfig }

constructor TNEDConfig.Create;
begin
  FStorage := TJSONVerySimple.Create;
  FLoaded := False;
  FError := False;
  FErrorMsg := '';
  FLocked := 0;
  //
  Init;
end;

destructor TNEDConfig.Destroy;
begin
  FErrorMsg := '';
  FColorSchema := '';
  FProfiles.Free;
  FLastProfile := '';
  FStorage.Free;
  inherited;
end;

procedure TNEDConfig.Init;
begin
  FMonitor := 0;
  FPosition := TPoint.Create(0, 0);
  FSize := TSize.Create(-1, -1);
  FMaximize := True;
  FShowHomePage := True;
  FColorSchema := 'system';
  FScreenSnap := False;
  FScreenSnapBuffer := 10;
  //
  FShowProfileSelection := False;
  FProfilesCount := 0;
  FProfiles := TNEDProfiles.Create;
  FLastProfile := '';
  FUseLastProfile := True;
  FLoadProfileSession := True;
end;

procedure TNEDConfig.FieldsToJson;
var
  DocRoot, Root, SubRoot, Node, SubNode: TJSONNode;
begin
  DocRoot := FStorage.DocumentElement;
  if DocRoot <> Nil then begin
    Root := FStorage.EnsureNode(DocRoot, 'NEDConfig', jtObject);
    //
    SubRoot := FStorage.EnsureNode(Root, 'Application', jtObject);
    //
      Node := FStorage.EnsureNode(SubRoot, 'Monitor', jtNumber);
      Node.ValueAsInteger := Integer(FMonitor);
      //
      Node := FStorage.EnsureNode(SubRoot, 'Position', jtObject);
        SubNode := FStorage.EnsureNode(Node, 'X', jtNumber);
        SubNode.ValueAsInteger := FPosition.X;
        SubNode := FStorage.EnsureNode(Node, 'Y', jtNumber);
        SubNode.ValueAsInteger := FPosition.Y;
      //
      Node := FStorage.EnsureNode(SubRoot, 'Size', jtObject);
        SubNode := FStorage.EnsureNode(Node, 'CX', jtNumber);
        SubNode.ValueAsInteger := FSize.cx;
        SubNode := FStorage.EnsureNode(Node, 'CY', jtNumber);
        SubNode.ValueAsInteger := FSize.cy;
      //
      Node := FStorage.EnsureNode(SubRoot, 'Maximize', jtBoolean);
      Node.ValueAsBoolean := FMaximize;
      //
      Node := FStorage.EnsureNode(SubRoot, 'ShowHomePage', jtBoolean);
      Node.ValueAsBoolean := FShowHomePage;
      //
      Node := FStorage.EnsureNode(SubRoot, 'ColorSchema', jtString);
      Node.ValueAsString := FColorSchema;
      //
      Node := FStorage.EnsureNode(SubRoot, 'ScreenSnap', jtBoolean);
      Node.ValueAsBoolean := FScreenSnap;
      //
      Node := FStorage.EnsureNode(SubRoot, 'ScreenSnapBuffer', jtNumber);
      Node.ValueAsInteger := FScreenSnapBuffer;
      //
    SubRoot := FStorage.EnsureNode(Root, 'Profiles', jtObject);
    //
      Node := FStorage.EnsureNode(SubRoot, 'ShowProfileSelection', jtBoolean);
      Node.ValueAsBoolean := FShowProfileSelection;
      //
      Node := FStorage.EnsureNode(SubRoot, 'ProfilesCount', jtNumber);
      Node.ValueAsInteger := FProfilesCount;
      //
      Node := FStorage.EnsureNode(SubRoot, 'DefaultProfile', jtNumber);
      Node.ValueAsInteger := FProfiles.DefaultIndex;
      //
      Node := FStorage.EnsureNode(SubRoot, 'ProfilesList', jtArray);
      ProfilesToJson(Node);
      //
      Node := FStorage.EnsureNode(SubRoot, 'LastProfile', jtString);
      Node.ValueAsString := FLastProfile;
      //
      Node := FStorage.EnsureNode(SubRoot, 'UseLastProfile', jtBoolean);
      Node.ValueAsBoolean := FUseLastProfile;
      //
      Node := FStorage.EnsureNode(SubRoot, 'LoadProfileSession', jtBoolean);
      Node.ValueAsBoolean := FLoadProfileSession;
  end;
end;

procedure TNEDConfig.JsonToFields;
var
  DocRoot, Root, SubRoot, Node, SubNode: TJSONNode;
begin
  DocRoot := FStorage.DocumentElement;
  if DocRoot <> Nil then begin
    Root := DocRoot.FindNode('NEDConfig', [jtObject]);
    if Root <> Nil then begin
      SubRoot := Root.FindNode('Application', [jtObject]);
      if SubRoot <> Nil then begin
        FMonitor := HMONITOR(FStorage.NodeAsInteger(SubRoot, 'Monitor', 0));
        //
        Node := SubRoot.FindNode('Position', [jtObject]);
        if Node <> Nil then begin
          FPosition.X := FStorage.NodeAsInteger(Node, 'X', 0);
          FPosition.Y := FStorage.NodeAsInteger(Node, 'Y', 0);
        end
        else begin
          FPosition := Point(0, 0);
        end;
        //
        Node := SubRoot.FindNode('Size', [jtObject]);
        if Node <> Nil then begin
          FSize.cx := FStorage.NodeAsInteger(Node, 'CX', -1);
          FSize.cy := FStorage.NodeAsInteger(Node, 'CY', -1);
        end
        else begin
          FSize := TSize.Create(-1, -1);
        end;
        //
        FMaximize := FStorage.NodeAsBoolean(SubRoot, 'Maximize', True);
        FShowHomePage := FStorage.NodeAsBoolean(SubRoot, 'ShowHomePage', True);
        FColorSchema := FStorage.NodeAsString(SubRoot, 'ColorSchema', 'system');
        FScreenSnap := FStorage.NodeAsBoolean(SubRoot, 'ScreenSnap', False);
        FScreenSnapBuffer := FStorage.NodeAsInteger(SubRoot, 'ScreenSnapBuffer', 10);
      end;
      SubRoot := Root.FindNode('Profiles', [jtObject]);
      if SubRoot <> Nil then begin
        FShowProfileSelection := FStorage.NodeAsBoolean(SubRoot, 'ShowProfileSelection', False);
        FProfilesCount := FStorage.NodeAsInteger(SubRoot, 'ProfilesCount', 0);
        FProfiles.DefaultIndex := FStorage.NodeAsInteger(SubRoot, 'DefaultProfile', -1);
        //FProfiles := TNEDProfiles;
        Node := SubRoot.FindNode('ProfilesList', [jtArray]);
        if Node <> Nil then begin
          JsonToProfiles(Node); //FProfiles: TNEDProfiles;
        end;
        FLastProfile := FStorage.NodeAsString(SubRoot, 'LastProfile', '');
        FUseLastProfile := FStorage.NodeAsBoolean(SubRoot, 'UseLastProfile', True);
        FLoadProfileSession := FStorage.NodeAsBoolean(SubRoot, 'LoadProfileSession', True);
      end;
    end;
  end;
end;

procedure TNEDConfig.ProfilesToJson(const ARootNode: TJSONNode);
var
  i: Integer;
  SubRoot, Node, SubNode: TJSONNode;
  Profile: TNEDProfile;
begin
  for i := 0 to FProfiles.Count - 1 do begin
    Profile := FProfiles.Profile[i];
    //
    SubRoot := FStorage.EnsureObjectInArray(ARootNode, 'ID', Profile.ID, jtObject);
    //
      Node := FStorage.EnsureNode(SubRoot, 'ID', jtString);
      Node.ValueAsString := Profile.ID;
      //
      Node := FStorage.EnsureNode(SubRoot, 'Name', jtString);
      Node.ValueAsString := Profile.Name;
      //
      Node := FStorage.EnsureNode(SubRoot, 'OriginateFrom', jtString);
      Node.ValueAsString := Profile.OriginateFrom;
      //
      Node := FStorage.EnsureNode(SubRoot, 'OpenedProjects', jtNumber);
      Node.ValueAsInteger := Profile.OpenedProjects;
      //
      Node := FStorage.EnsureNode(SubRoot, 'OpenedFiles', jtNumber);
      Node.ValueAsInteger := Profile.OpenedFiles;
      //
  end;
end;

procedure TNEDConfig.JsonToProfiles(const ARootNode: TJSONNode);
var
  SubRoot: TJSONNode;
  Profile: TNEDProfile;
  pID: String;
  pName: String;
  pOriginateFrom: String;
  pOpenedProjects: Integer;
  pOpenedFiles: Integer;
begin
  SubRoot := ARootNode.FirstChild;
  while SubRoot <> Nil do begin
    pID := FStorage.NodeAsString(SubRoot, 'ID', '');
    pName := FStorage.NodeAsString(SubRoot, 'Name', '');
    pOriginateFrom := FStorage.NodeAsString(SubRoot, 'OriginateFrom', '');
    pOpenedProjects := FStorage.NodeAsInteger(SubRoot, 'OpenedProjects', 0);
    pOpenedFiles := FStorage.NodeAsInteger(SubRoot, 'OpenedFiles', 0);
    //
    Profile := NEDConfig.Profiles.Add(pID, pName);
    Profile.OriginateFrom := pOriginateFrom;
    Profile.OpenedProjects := pOpenedProjects;
    Profile.OpenedFiles := pOpenedFiles;
    //
    SubRoot := SubRoot.NextSibling;
  end;
end;

procedure TNEDConfig.LoadConfig;
begin
  try
    FError := False;
    FErrorMsg := '';
    FStorage.Clear;
    FProfiles.Clear;
    FStorage.LoadFromFile(ChangeFileExt(Application.ExeName, '.jcfg'));
    FLoaded := FStorage.Empty;
    if FLoaded then
      JsonToFields;
  except
    // no error here
    on E: Exception do begin
      FError := True;
      FErrorMsg := E.ClassName + ': ' + E.Message;
    end;
  end;
end;

procedure TNEDConfig.SaveConfig;
begin
  if FLocked > 0 then
    Exit;
  //
  try
    FieldsToJson;
    FStorage.SaveToFile(ChangeFileExt(Application.ExeName, '.jcfg'));
    FError := False;
    FErrorMsg := '';
  except
    // no error here
    on E: Exception do begin
      FError := True;
      FErrorMsg := E.ClassName + ': ' + E.Message;
    end;
  end;
end;

procedure TNEDConfig.Lock;
begin
  Inc(FLocked);
end;

procedure TNEDConfig.Unlock;
begin
  Dec(FLocked);
  if FLocked <= 0 then
    FLocked := 0;
end;

procedure TNEDConfig.Update;
begin
  FProfilesCount := FProfiles.Count;
end;

initialization
  NEDConfig := TNEDConfig.Create;

finalization
  NEDConfig.Free;

end.

