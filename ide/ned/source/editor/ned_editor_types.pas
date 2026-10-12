//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-10
// All rights reserved.
//

unit ned_editor_types;

interface

uses
  SysUtils;

type
  TNEDEditorCommentTypeEnum = (
    ectNotInComment,
    ectSingleLine,
    ectMultiLine,
    ectMixed
  );

  TNEDEditorDelimiterPair = record
  public
    &Type: TNEDEditorCommentTypeEnum;
    StartDelimiter: UTF8String;
    EndDelimiter: UTF8String; // optional, for single-line comment leave empty
    CanNest: Boolean; // can nest other comments
    CanBeNested: Boolean; // can be nested inside other comments
    //
    class function EditorCommentTypeToString(const Value: TNEDEditorCommentTypeEnum): String; static;
    class function StringToEditorCommentType(Value: String): TNEDEditorCommentTypeEnum; static;
  end;

implementation

{ TNEDEditorDelimiterPair }

class function TNEDEditorDelimiterPair.EditorCommentTypeToString(const Value: TNEDEditorCommentTypeEnum): String;
begin
  Result := 'NotInComment';
  if Value = ectSingleLine then
    Result := 'SingleLine'
  else if Value = ectMultiLine then
    Result := 'MultiLine'
  else if Value = ectMixed then
    Result := 'Mixed';
end;

class function TNEDEditorDelimiterPair.StringToEditorCommentType(Value: String): TNEDEditorCommentTypeEnum;
begin
  Value := LowerCase(Value);
  Result := ectNotInComment;
  if Value = 'singleline' then
    Result := ectSingleLine
  else if Value = 'multiline' then
    Result := ectMultiLine
  else if Value = 'mixed' then
    Result := ectMixed;
end;

end.

