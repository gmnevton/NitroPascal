//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
//

program ned;

uses
  madExcept,
  madLinkDisAsm,
  madListModules,
  Forms,
  ned_main in 'ned_main.pas' {NEDMainForm},
  ned_source_editor in 'ned_source_editor.pas' {NEDEditorForm},
  ned_home_page in 'ned_home_page.pas' {NEDHomeForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TNEDMainForm, NEDMainForm);
  Application.Run;
end.
