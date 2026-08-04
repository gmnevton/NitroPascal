//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-07
// All rights reserved.
//

unit ned_json_config_utils;

interface

uses
  JSON.VerySimple;

type
  TJSONVerySimpleHelper = class helper for TJSONVerySimple
  public
    function EnsureNode(const ARoot: TJSONNode; const ANodeName: String; const ANodeType: TJSONNodeType): TJSONNode;
    function EnsureObjectInArray(const ARoot: TJSONNode; const ANodeName, ANodeValue: String; const ANodeType: TJSONNodeType): TJSONNode;
    //
    function NodeAsBoolean(const ARoot: TJSONNode; const ANodeName: String; const ADefault: Boolean): Boolean;
    function NodeAsInteger(const ARoot: TJSONNode; const ANodeName: String; const ADefault: Integer): Integer;
    function NodeAsDateTime(const ARoot: TJSONNode; const ANodeName: String; const ADefault: TDateTime): TDateTime;
    function NodeAsString(const ARoot: TJSONNode; const ANodeName: String; const ADefault: String): String;
  end;

implementation

uses
  DateUtils;

{ TJSONVerySimpleHelper }

function TJSONVerySimpleHelper.EnsureNode(const ARoot: TJSONNode; const ANodeName: String; const ANodeType: TJSONNodeType): TJSONNode;
begin
  Result := ARoot.FindNode(ANodeName, [ANodeType]);
  if Result = Nil then begin
    Result := ARoot.AddChild(ANodeName, ANodeType);
  end;
end;

function TJSONVerySimpleHelper.EnsureObjectInArray(const ARoot: TJSONNode; const ANodeName, ANodeValue: String; const ANodeType: TJSONNodeType): TJSONNode;
begin
  Result := ARoot.FindNode(ANodeName, ANodeValue, [jtString], [jsRecursive]);
  if Result = Nil then begin
    Result := ARoot.AddChild('', ANodeType);
  end
  else begin
    Result := Result.ParentNode; // gets object node
  end;
end;

function TJSONVerySimpleHelper.NodeAsBoolean(const ARoot: TJSONNode; const ANodeName: String; const ADefault: Boolean): Boolean;
var
  Node: TJSONNode;
begin
  Result := ADefault;
  Node := ARoot.FindNode(ANodeName, [jtBoolean]);
  if Node <> Nil then
    Result := Node.ValueAsBoolean;
end;

function TJSONVerySimpleHelper.NodeAsInteger(const ARoot: TJSONNode; const ANodeName: String; const ADefault: Integer): Integer;
var
  Node: TJSONNode;
begin
  Result := ADefault;
  Node := ARoot.FindNode(ANodeName, [jtNumber]);
  if Node <> Nil then
    Result := Node.ValueAsInteger;
end;

function TJSONVerySimpleHelper.NodeAsDateTime(const ARoot: TJSONNode; const ANodeName: String; const ADefault: TDateTime): TDateTime;
var
  Node: TJSONNode;
begin
  Result := ADefault;
  Node := ARoot.FindNode(ANodeName, [jtString]);
  if (Node <> Nil) then
    if not TryISO8601ToDate(Node.ValueAsString, Result, False) then
      Result := ADefault;
end;

function TJSONVerySimpleHelper.NodeAsString(const ARoot: TJSONNode; const ANodeName: String; const ADefault: String): String;
var
  Node: TJSONNode;
begin
  Result := ADefault;
  Node := ARoot.FindNode(ANodeName, [jtString]);
  if Node <> Nil then
    Result := Node.ValueAsString;
end;

end.

