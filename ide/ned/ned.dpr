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
  ned_common_simple_types in 'source\utils\ned_common_simple_types.pas',
  ned_config in 'source\config\ned_config.pas',
  ned_profiles in 'source\config\ned_profiles.pas',
  ned_session_context in 'source\utils\ned_session_context.pas',
  ned_main in 'source\ned_main.pas' {NEDMainForm},
  ned_home_page in 'source\views\ned_home_page.pas' {NEDHomeForm},
  ned_settings in 'source\views\ned_settings.pas' {NEDSettingsForm},
  ned_workspace_manager in 'source\utils\ned_workspace_manager.pas',
  ned_projects in 'source\config\ned_projects.pas',
  ned_editor_buffer in 'source\editor\ned_editor_buffer.pas',
  ned_editor_view in 'source\editor\ned_editor_view.pas',
  ned_splitview_manager in 'source\utils\ned_splitview_manager.pas',
  ned_source_view in 'source\views\ned_source_view.pas' {NEDViewForm},
  ned_source_editor in 'source\views\ned_source_editor.pas' {NEDEditorForm},
  ned_dialog_base in 'source\dialogs\ned_dialog_base.pas' {NEDDialogBase},
  ned_dialog_open in 'source\dialogs\ned_dialog_open.pas' {NEDDialogOpen},
  ned_dialog_save in 'source\dialogs\ned_dialog_save.pas' {NEDDialogSave},
  ned_dialog_profiles in 'source\dialogs\ned_dialog_profiles.pas' {NEDDialogProfiles},
  ned_dialog_message in 'source\dialogs\ned_dialog_message.pas' {NEDDialogMessage},
  ned_editor_lexer in 'source\editor\ned_editor_lexer.pas',
  ned_editor_texer in 'source\editor\ned_editor_texer.pas',
  ned_editor_parser in 'source\editor\ned_editor_parser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  NEDConfig.LoadConfig;
  Application.CreateForm(TNEDMainForm, NEDMainForm);
  Application.Run;
//  NEDConfig.SaveConfig;
end.
