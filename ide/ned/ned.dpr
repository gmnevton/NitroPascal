//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-07
// All rights reserved.
//

program ned;

uses
  madExcept,
  madLinkDisAsm,
  madListModules,
  Forms,
  ned_common_simple_types in 'source\ned_common_simple_types.pas',
  ned_config in 'source\ned_config.pas',
  ned_profiles in 'source\ned_profiles.pas',
  ned_session_context in 'source\ned_session_context.pas',
  ned_main in 'source\ned_main.pas' {NEDMainForm},
  ned_home_page in 'source\ned_home_page.pas' {NEDHomeForm},
  ned_settings in 'source\ned_settings.pas' {NEDSettingsForm},
  ned_workspace_manager in 'source\ned_workspace_manager.pas',
  ned_projects in 'source\ned_projects.pas',
  ned_editor_buffer in 'source\ned_editor_buffer.pas',
  ned_editor_view in 'source\ned_editor_view.pas',
  ned_splitview_manager in 'source\ned_splitview_manager.pas',
  ned_source_view in 'source\ned_source_view.pas' {NEDViewForm},
  ned_source_editor in 'source\ned_source_editor.pas' {NEDEditorForm},
  ned_dialog_base in 'source\ned_dialog_base.pas' {NEDDialogBase},
  ned_dialog_open in 'source\ned_dialog_open.pas' {NEDDialogOpen},
  ned_dialog_save in 'source\ned_dialog_save.pas' {NEDDialogSave},
  ned_dialog_profiles in 'source\ned_dialog_profiles.pas' {NEDDialogProfiles},
  ned_dialog_message in 'source\ned_dialog_message.pas' {NEDDialogMessage};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  NEDConfig.LoadConfig;
  Application.CreateForm(TNEDMainForm, NEDMainForm);
  Application.Run;
//  NEDConfig.SaveConfig;
end.
