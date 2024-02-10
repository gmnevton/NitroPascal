program npc;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  madExcept,
  madLinkDisAsm,
  madListModules,
  SysUtils,
  npc_lexer,
  npc_parser,
  npc_project;

var
  input_path: String;
  output_path: String;

begin
  try
    Writeln('NitroPascal compiler version 1.0');
    Writeln('2024, Grzegorz Molenda');
    Writeln('All rights reserved.');
    Writeln;
    if ParamCount = 0 then begin
      Writeln('npc.exe project_file_path [output_path] [-e:encoding] [-f:format_settings]');
      Writeln('        encoding - ASCII or ANSI or UTF8');
      Writeln('        format_settings - n/a');
      Writeln;
      Exit;
    end;
    //
    if ParamCount > 0 then begin
      input_path := ParamStr(1);
      if ParamCount > 1 then begin
        output_path := ParamStr(2);
      end
      else
        output_path := '';
    end;
    //
    if not NPC_CompileProject(PChar(input_path), PChar(output_path)) then
      Writeln(NPC_ReportErrors);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
