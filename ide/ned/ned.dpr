//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-06
// All rights reserved.
//

program ned;

uses
  madExcept,
  madLinkDisAsm,
  madListModules,
  Forms,
  ned_main in 'ned_main.pas' {NEDMainForm},
  ned_source_editor in 'ned_source_editor.pas' {NEDEditorForm},
  ned_home_page in 'ned_home_page.pas' {NEDHomeForm},
  ned_settings in 'ned_settings.pas' {NEDSettingsForm},
  ned_dialog_open in 'ned_dialog_open.pas' {NEDDialogOpen: TFrame},
  ned_editor_buffer in 'ned_editor_buffer.pas',
  ned_editor_view in 'ned_editor_view.pas',
  ned_splitview_manager in 'ned_splitview_manager.pas',
  ned_source_view in 'ned_source_view.pas' {NEDViewForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TNEDMainForm, NEDMainForm);
  Application.Run;
end.
