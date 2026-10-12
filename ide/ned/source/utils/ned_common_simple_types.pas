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
  SysUtils,
  Graphics;

type
  TNEDUniqueID = type String;

function NewGUID: TNEDUniqueID;
function FontStylesToString(const Styles: TFontStyles): String;
function StringToFontStyles(Styles: String): TFontStyles;

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

function FontStylesToString(const Styles: TFontStyles): String;
begin
  Result := '';
  if fsBold in Styles then
    Result := Result + 'B';
  if fsItalic in Styles then
    Result := Result + 'I';
  if fsUnderline in Styles then
    Result := Result + 'U';
  if fsStrikeOut in Styles then
    Result := Result + 'S';
end;

function StringToFontStyles(Styles: String): TFontStyles;
begin
  Result := [];
  Styles := UpperCase(Styles);
  if Pos('B', Styles) > 0 then
    Include(Result, fsBold);
  if Pos('I', Styles) > 0 then
    Include(Result, fsItalic);
  if Pos('U', Styles) > 0 then
    Include(Result, fsUnderline);
  if Pos('S', Styles) > 0 then
    Include(Result, fsStrikeOut);
end;

end.

