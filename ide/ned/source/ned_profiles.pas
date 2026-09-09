//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-07
// All rights reserved.
//

unit ned_profiles;

interface

uses
  SysUtils,
  Generics.Collections,
  ned_session_context,
  ned_common_simple_types;

type
  // this class is responsible for handling info about profiles that are available to the user,
  // all additional info is handled by session contex, that load/save profile file config
  TNEDProfile = class
  private
    FID: TNEDUniqueID;
    FName: String;
    FCopiedFrom: String; // not sure if this is needed
    FIndex: Integer;
    //
    FOpenedProjects: Integer;
    FOpenedFiles: Integer;
    //
    FSession: TNEDSessionContext;
    FSessionLoaded: Boolean;
    //
    FError: Boolean;
    FErrorMsg: String;
  public
    constructor Create(const AName: String; const ACreateNewUID: Boolean);
    destructor Destroy; override;
    //
    function CopyFrom(const AName: String): Boolean;
    function Remove: Boolean;
    //
    function LoadSessionContext: Boolean;
    function UnloadSessionContext: Boolean;
    //
    property ID: TNEDUniqueID read FID;
    property Name: String read FName;
    property OriginateFrom: String read FCopiedFrom write FCopiedFrom;
    property Index: Integer read FIndex;
    property OpenedProjects: Integer read FOpenedProjects write FOpenedProjects;
    property OpenedFiles: Integer read FOpenedFiles write FOpenedFiles;
    //
    property Error: Boolean read FError;
    property ErrorMsg: String read FErrorMsg;
    //
    property Session: TNEDSessionContext read FSession;
  end;

  TNEDProfiles = class
  private
    FList: TObjectList<TNEDProfile>;
    FDefaultIndex: Integer;
    //
    function GetProfile(const AIndex: Integer): TNEDProfile;
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Clear;
    function Count: Integer;
    function Add(const AID: TNEDUniqueID; const AName: String): TNEDProfile;
    function AddNew(const AName: String; const ASetAsDefault: Boolean): TNEDProfile;
    function Remove(const AIndex: Integer): Boolean;
    function Move(const AFromIndex, AToIndex: Integer): Boolean;
    //
    property Profile[const AIndex: Integer]: TNEDProfile read GetProfile;
    property DefaultIndex: Integer read FDefaultIndex write FDefaultIndex;
  end;

implementation

uses
  Forms;

{ TNEDProfile }

constructor TNEDProfile.Create(const AName: String; const ACreateNewUID: Boolean);
begin
  FID := '';
  if ACreateNewUID then begin
    FID := NewGUID;
  end;
  //
  FName := AName;
  FCopiedFrom := '';
  FIndex := -1;
  FOpenedProjects := 0;
  FOpenedFiles := 0;
  FSession := TNEDSessionContext.Create;
  FSessionLoaded := False;
  FError := False;
  FErrorMsg := '';
end;

destructor TNEDProfile.Destroy;
begin
  FID := '';
  FName := '';
  FCopiedFrom := '';
  FErrorMsg := '';
  FSession.Free;
  inherited;
end;

function TNEDProfile.CopyFrom(const AName: String): Boolean;
begin
  Result := False;
end;

function TNEDProfile.Remove: Boolean;
begin
// @TODO: remove profile directory with all config files in it
end;

function TNEDProfile.LoadSessionContext: Boolean;
var
  FProfilesPath: String;
begin
  try
    FError := False;
    FErrorMsg := '';
    //
    FProfilesPath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
    FProfilesPath := FProfilesPath + 'profiles' + '\' + FName; // + '\profile.jcfg';
    //
    if DirectoryExists(FProfilesPath) then begin
      FSession.LoadContext(FProfilesPath);
      Result := FSession.Loaded;
      if not Result or FSession.Error then begin
        FError := FSession.Error;
        FErrorMsg := FSession.ErrorMsg;
      end;
    end
    else begin
      Result := False;
      FError := True;
      FErrorMsg := Format('Profile "%s" not found.', [FProfilesPath]);
    end;
  except
    // no error here
    on E: Exception do begin
      Result := False;
      FError := True;
      FErrorMsg := E.ClassName + ': ' + E.Message;
    end;
  end;
end;

function TNEDProfile.UnloadSessionContext: Boolean;
begin

end;

{ TNEDProfiles }

constructor TNEDProfiles.Create;
begin
  FList := TObjectList<TNEDProfile>.Create(True);
  FDefaultIndex := -1;
end;

destructor TNEDProfiles.Destroy;
begin
  FList.Free;
  inherited;
end;

function TNEDProfiles.GetProfile(const AIndex: Integer): TNEDProfile;
begin
  Result := Nil;
  if (FList.Count > 0) and (AIndex >= 0) and (AIndex < FList.Count) then
    Result := FList.Items[AIndex];
end;

procedure TNEDProfiles.Clear;
begin
  FList.Clear;
end;

function TNEDProfiles.Count: Integer;
begin
  Result := FList.Count;
end;

function TNEDProfiles.Add(const AID: TNEDUniqueID; const AName: String): TNEDProfile;
begin
  try
    Result := TNEDProfile.Create(AName, False);
    Result.FID := AID;
    Result.FIndex := FList.Add(Result);
  except
    if Result <> Nil then
      Result.Free;
    Result := Nil;
  end;
end;

function TNEDProfiles.AddNew(const AName: String; const ASetAsDefault: Boolean): TNEDProfile;
begin
  try
    Result := TNEDProfile.Create(AName, True);
    Result.FIndex := FList.Add(Result);
    if ASetAsDefault then
      FDefaultIndex := Result.FIndex;
  except
    if Result <> Nil then
      Result.Free;
    Result := Nil;
  end;
end;

function TNEDProfiles.Remove(const AIndex: Integer): Boolean;
begin
  Result := False;
  if (FList.Count > 0) and (AIndex >= 0) and (AIndex < FList.Count) then begin
    FList.Delete(AIndex);
    Result := True;
  end;
end;

function TNEDProfiles.Move(const AFromIndex, AToIndex: Integer): Boolean;
begin
  Result := False;
  if (FList.Count > 0) and
     (AFromIndex <> AToIndex) and
     (AFromIndex >= 0) and (AFromIndex < FList.Count) and
     (AToIndex >= 0) and (AToIndex < FList.Count)
  then begin
    FList.Move(AFromIndex, AToIndex);
    Result := True;
  end;
end;

end.

