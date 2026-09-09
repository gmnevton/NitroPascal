//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-08
// All rights reserved.
//

unit ned_editor_context;

interface

uses
  SysUtils,
  Classes,
  uFolders,
  UCL.SymbolButton,
  ned_editor_buffer,
  ned_source_editor;

type
  TNEDEditorContext = class
  public
    Buffer: TNEDEditorBuffer;
    Info: TNEDEditorInfo;
    //FView: TNEDEditorForm;
    //FThumbstone: TUSymbolButton;
    WorkspaceEntry: TEntryItem;
  public
    constructor Create(const ABuffer: TNEDEditorBuffer; const AInfo: TNEDEditorInfo);
    destructor Destroy; override;
  end;

implementation

{ TNEDEditorContext }

constructor TNEDEditorContext.Create(const ABuffer: TNEDEditorBuffer; const AInfo: TNEDEditorInfo);
begin
  Buffer := ABuffer;
  Info := AInfo;
//  FView := View;
//  FViewHeaderButton := ViewHeaderButton;
  WorkspaceEntry := Nil;
end;

destructor TNEDEditorContext.Destroy;
begin

  inherited;
end;

end.

