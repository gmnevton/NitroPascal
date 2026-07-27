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
  ned_profiles,
  JSON.VerySimple;

type
  TNEDConfig = class
  private
    FMonitor: HMONITOR;
    FPosition: TPoint;
    FSize: TSize;
    FMaximize: Boolean;
    FShowHomePage: Boolean;
    FColorSchema: String;
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
    function EnsureNode(const ARoot: TJSONNode; const ANodeName: String; const ANodeType: TJSONNodeType): TJSONNode;
    function EnsureObjectInArray(const ARoot: TJSONNode; const ANodeName, ANodeValue: String; const ANodeType: TJSONNodeType): TJSONNode;
    function NodeAsBoolean(const ARoot: TJSONNode; const ANodeName: String; const ADefault: Boolean): Boolean;
    function NodeAsInteger(const ARoot: TJSONNode; const ANodeName: String; const ADefault: Integer): Integer;
    function NodeAsString(const ARoot: TJSONNode; const ANodeName: String; const ADefault: String): String;
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
  //
  FShowProfileSelection := False;
  FProfilesCount := 0;
  FProfiles := TNEDProfiles.Create;
  FLastProfile := '';
  FUseLastProfile := True;
  FLoadProfileSession := True;
end;

function TNEDConfig.EnsureNode(const ARoot: TJSONNode; const ANodeName: String; const ANodeType: TJSONNodeType): TJSONNode;
begin
  Result := ARoot.FindNode(ANodeName, [ANodeType]);
  if Result = Nil then begin
    Result := ARoot.AddChild(ANodeName, ANodeType);
  end;
end;

function TNEDConfig.EnsureObjectInArray(const ARoot: TJSONNode; const ANodeName, ANodeValue: String; const ANodeType: TJSONNodeType): TJSONNode;
begin
  Result := ARoot.FindNode(ANodeName, ANodeValue, [jtString], [jsRecursive]);
  if Result = Nil then begin
    Result := ARoot.AddChild('', ANodeType);
  end
  else begin
    Result := Result.ParentNode; // gets object node
  end;
end;

function TNEDConfig.NodeAsBoolean(const ARoot: TJSONNode; const ANodeName: String; const ADefault: Boolean): Boolean;
var
  Node: TJSONNode;
begin
  Result := ADefault;
  Node := ARoot.FindNode(ANodeName, [jtBoolean]);
  if Node <> Nil then
    Result := Node.ValueAsBoolean;
end;

function TNEDConfig.NodeAsInteger(const ARoot: TJSONNode; const ANodeName: String; const ADefault: Integer): Integer;
var
  Node: TJSONNode;
begin
  Result := ADefault;
  Node := ARoot.FindNode(ANodeName, [jtNumber]);
  if Node <> Nil then
    Result := Node.ValueAsInteger;
end;

function TNEDConfig.NodeAsString(const ARoot: TJSONNode; const ANodeName: String; const ADefault: String): String;
var
  Node: TJSONNode;
begin
  Result := ADefault;
  Node := ARoot.FindNode(ANodeName, [jtString]);
  if Node <> Nil then
    Result := Node.ValueAsString;
end;

procedure TNEDConfig.FieldsToJson;
var
  DocRoot, Root, SubRoot, Node, SubNode: TJSONNode;
begin
  DocRoot := FStorage.DocumentElement;
  if DocRoot <> Nil then begin
    Root := EnsureNode(DocRoot, 'NEDConfig', jtObject);
    //
    SubRoot := EnsureNode(Root, 'Application', jtObject);
    //
      Node := EnsureNode(SubRoot, 'Monitor', jtNumber);
      Node.ValueAsInteger := Integer(FMonitor);
      //
      Node := EnsureNode(SubRoot, 'Position', jtObject);
        SubNode := EnsureNode(Node, 'X', jtNumber);
        SubNode.ValueAsInteger := FPosition.X;
        SubNode := EnsureNode(Node, 'Y', jtNumber);
        SubNode.ValueAsInteger := FPosition.Y;
      //
      Node := EnsureNode(SubRoot, 'Size', jtObject);
        SubNode := EnsureNode(Node, 'CX', jtNumber);
        SubNode.ValueAsInteger := FSize.cx;
        SubNode := EnsureNode(Node, 'CY', jtNumber);
        SubNode.ValueAsInteger := FSize.cy;
      //
      Node := EnsureNode(SubRoot, 'Maximize', jtBoolean);
      Node.ValueAsBoolean := FMaximize;
      //
      Node := EnsureNode(SubRoot, 'ShowHomePage', jtBoolean);
      Node.ValueAsBoolean := FShowHomePage;
      //
      Node := EnsureNode(SubRoot, 'ColorSchema', jtString);
      Node.ValueAsString := FColorSchema;
      //
    SubRoot := EnsureNode(Root, 'Profiles', jtObject);
    //
      Node := EnsureNode(SubRoot, 'ShowProfileSelection', jtBoolean);
      Node.ValueAsBoolean := FShowProfileSelection;
      //
      Node := EnsureNode(SubRoot, 'ProfilesCount', jtNumber);
      Node.ValueAsInteger := FProfilesCount;
      //
      Node := EnsureNode(SubRoot, 'DefaultProfile', jtNumber);
      Node.ValueAsInteger := FProfiles.DefaultIndex;
      //
      Node := EnsureNode(SubRoot, 'ProfilesList', jtArray);
      ProfilesToJson(Node);
      //
      Node := EnsureNode(SubRoot, 'LastProfile', jtString);
      Node.ValueAsString := FLastProfile;
      //
      Node := EnsureNode(SubRoot, 'UseLastProfile', jtBoolean);
      Node.ValueAsBoolean := FUseLastProfile;
      //
      Node := EnsureNode(SubRoot, 'LoadProfileSession', jtBoolean);
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
        FMonitor := HMONITOR(NodeAsInteger(SubRoot, 'Monitor', 0));
        //
        Node := SubRoot.FindNode('Position', [jtObject]);
        if Node <> Nil then begin
          FPosition.X := NodeAsInteger(Node, 'X', 0);
          FPosition.Y := NodeAsInteger(Node, 'Y', 0);
        end
        else begin
          FPosition := Point(0, 0);
        end;
        //
        Node := SubRoot.FindNode('Size', [jtObject]);
        if Node <> Nil then begin
          FSize.cx := NodeAsInteger(Node, 'CX', -1);
          FSize.cy := NodeAsInteger(Node, 'CY', -1);
        end
        else begin
          FSize := TSize.Create(-1, -1);
        end;
        //
        FMaximize := NodeAsBoolean(SubRoot, 'Maximize', True);
        FShowHomePage := NodeAsBoolean(SubRoot, 'ShowHomePage', True);
        FColorSchema := NodeAsString(SubRoot, 'ColorSchema', 'system');
      end;
      SubRoot := Root.FindNode('Profiles', [jtObject]);
      if SubRoot <> Nil then begin
        FShowProfileSelection := NodeAsBoolean(SubRoot, 'ShowProfileSelection', False);
        FProfilesCount := NodeAsInteger(SubRoot, 'ProfilesCount', 0);
        FProfiles.DefaultIndex := NodeAsInteger(SubRoot, 'DefaultProfile', -1);
        //FProfiles := TNEDProfiles;
        Node := SubRoot.FindNode('ProfilesList', [jtArray]);
        if Node <> Nil then begin
          JsonToProfiles(Node); //FProfiles: TNEDProfiles;
        end;
        FLastProfile := NodeAsString(SubRoot, 'LastProfile', '');
        FUseLastProfile := NodeAsBoolean(SubRoot, 'UseLastProfile', True);
        FLoadProfileSession := NodeAsBoolean(SubRoot, 'LoadProfileSession', True);
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
    SubRoot := EnsureObjectInArray(ARootNode, 'ID', Profile.ID, jtObject);
    //
      Node := EnsureNode(SubRoot, 'ID', jtString);
      Node.ValueAsString := Profile.ID;
      //
      Node := EnsureNode(SubRoot, 'Name', jtString);
      Node.ValueAsString := Profile.Name;
      //
      Node := EnsureNode(SubRoot, 'OriginateFrom', jtString);
      Node.ValueAsString := Profile.OriginateFrom;
      //
      Node := EnsureNode(SubRoot, 'OpenedProjects', jtNumber);
      Node.ValueAsInteger := Profile.OpenedProjects;
      //
      Node := EnsureNode(SubRoot, 'OpenedFiles', jtNumber);
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
    pID := NodeAsString(SubRoot, 'ID', '');
    pName := NodeAsString(SubRoot, 'Name', '');
    pOriginateFrom := NodeAsString(SubRoot, 'OriginateFrom', '');
    pOpenedProjects := NodeAsInteger(SubRoot, 'OpenedProjects', 0);
    pOpenedFiles := NodeAsInteger(SubRoot, 'OpenedFiles', 0);
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

