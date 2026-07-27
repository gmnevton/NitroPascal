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
  Generics.Collections;

type
  TNEDEditorContext = class
  private
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TNEDProject = class
  private
    FEditors: TObjectList<TNEDEditorContext>;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TNEDSessionContext = class
  private
    FProjects: TObjectList<TNEDProject>;
    //
    function GetProject(const AIndex: Integer): TNEDProject;
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Clear;
    function Count: Integer;
    function Add(const AProjectID, AName: String): TNEDProject;
    function AddNew(const AName: String): TNEDProject;
    function Remove(const AIndex: Integer): Boolean;
    function Move(const AFromIndex, AToIndex: Integer): Boolean;
    //
    property Project[const AIndex: Integer]: TNEDProject read GetProject;
  end;

implementation

{ TNEDEditorContext }

constructor TNEDEditorContext.Create;
begin

end;

destructor TNEDEditorContext.Destroy;
begin

  inherited;
end;

{ TNEDProject }

constructor TNEDProject.Create;
begin
  FEditors := TObjectList<TNEDEditorContext>.Create(True);
end;

destructor TNEDProject.Destroy;
begin
  FEditors.Free;
  inherited;
end;

{ TNEDSessionContext }

constructor TNEDSessionContext.Create;
begin
  FProjects := TObjectList<TNEDProject>.Create(True);
end;

destructor TNEDSessionContext.Destroy;
begin
  FProjects.Free;
  inherited;
end;

function TNEDSessionContext.GetProject(const AIndex: Integer): TNEDProject;
begin
  Result := Nil;
  if (FProjects.Count > 0) and (AIndex >= 0) and (AIndex < FProjects.Count) then
    Result := FProjects.Items[AIndex];
end;

procedure TNEDSessionContext.Clear;
begin
  FProjects.Clear;
end;

function TNEDSessionContext.Count: Integer;
begin
  Result := FProjects.Count;
end;

function TNEDSessionContext.Add(const AProjectID, AName: String): TNEDProject;
begin
  try
    Result := TNEDProject.Create;
//    Result.FID := AID;
//    Result.FIndex :=
    FProjects.Add(Result);
  except
    if Result <> Nil then
      Result.Free;
    Result := Nil;
  end;
end;

function TNEDSessionContext.AddNew(const AName: String): TNEDProject;
begin
  try
    Result := TNEDProject.Create; // (AName, True);
//    Result.FIndex :=
    FProjects.Add(Result);
//    if ASetAsDefault then
//      FDefaultIndex := Result.FIndex;
  except
    if Result <> Nil then
      Result.Free;
    Result := Nil;
  end;
end;

function TNEDSessionContext.Remove(const AIndex: Integer): Boolean;
begin
  Result := False;
  if (FProjects.Count > 0) and (AIndex >= 0) and (AIndex < FProjects.Count) then begin
    FProjects.Delete(AIndex);
    Result := True;
  end;
end;

function TNEDSessionContext.Move(const AFromIndex, AToIndex: Integer): Boolean;
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

end.

