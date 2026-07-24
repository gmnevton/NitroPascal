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
  Generics.Collections;

type
  TNEDProfile = class
  private
    FName: String;
    FOpenedProjects: Integer;
    FOpenedFiles: Integer;
  public
    constructor Create(const AName: String);
    destructor Destroy; override;
  end;

  TNEDProfiles = class
  private
    FList: TObjectList<TNEDProfile>;
    //
    function GetProfile(const AIndex: Integer): TNEDProfile;
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Clear;
    function Count: Integer;
    function Add(const AName: String): TNEDProfile;
    function Remove(const AIndex: Integer): Boolean;
    //
    property Profile[const AIndex: Integer]: TNEDProfile read GetProfile;
  end;

implementation

{ TNEDProfile }

constructor TNEDProfile.Create(const AName: String);
begin
  FName := AName;
  FOpenedProjects := 0;
  FOpenedFiles := 0;
end;

destructor TNEDProfile.Destroy;
begin
  FName := '';
  inherited;
end;

{ TNEDProfiles }

constructor TNEDProfiles.Create;
begin
  FList := TObjectList<TNEDProfile>.Create(True);
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

function TNEDProfiles.Add(const AName: String): TNEDProfile;
begin
  try
    Result := TNEDProfile.Create(AName);
    FList.Add(Result);
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

end.

