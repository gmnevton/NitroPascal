program npc;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  madExcept,
  madLinkDisAsm,
  madListModules,
  SysUtils,
  Windows,
  npc_lexer,
  npc_parser,
  npc_project,
  npc_utils;

var
  input_path: String;
  output_path: String;
  start_time: Real;
  total_time: Real;

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
    start_time := getTime;
    //
    if not NPC_CompileProject(PChar(input_path), PChar(output_path)) then
      Writeln(NPC_ReportErrors);
    //
    total_time := getTime - start_time;
    //Writeln(total_time:0:8);
    if total_time < 0 then // if hour changed from 23 to less than that
      total_time := total_time + 3600.0 * 24;
    if Round(Frac(Total_time) * 1000) >= 1000 then
      total_time := Trunc(total_time) + 1;
    Writeln;
    Writeln(Format('Total time (m:s.ms): %d:%d.%.3d', [Trunc(total_time) div 60, Trunc(total_time) mod 60, Round(Frac(total_time) * 1000)]));
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
