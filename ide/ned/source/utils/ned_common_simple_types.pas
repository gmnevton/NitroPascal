//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-08
// All rights reserved.
//

unit ned_common_simple_types;

interface

uses
  SysUtils;

type
  TNEDUniqueID = type String;

function NewGUID: TNEDUniqueID;

implementation

uses
  ActiveX;

function Succeeded(Res: HResult): Boolean;
begin
  Result := Res and $80000000 = 0;
end;

function GUIDToString(const ClassID: TGUID): string;
var
  P: PWideChar;
  OpResult: HResult;
begin
  OpResult := StringFromCLSID(ClassID, P);
  if not Succeeded(OpResult) then
    Result :=  ''
  else begin
    Result := P;
    Result := Result.Replace('{', '').Replace('}', '').Replace('-', '');
    CoTaskMemFree(P);
  end;
end;

function NewGUID: TNEDUniqueID;
var
  G: TGUID;
  Res: HResult;
begin
  Result := '';
  Res := CreateGUID(G);
  if Succeeded(Res) then // @TODO: do something with this, when error occurs
    Result := GUIDToString(G);
end;

end.

