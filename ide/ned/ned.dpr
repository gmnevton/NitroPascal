program ned;

uses
  madExcept,
  madLinkDisAsm,
  madListModules,
  Forms,
  ned_main in 'ned_main.pas' {NEDMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TNEDMainForm, NEDMainForm);
  Application.Run;
end.
