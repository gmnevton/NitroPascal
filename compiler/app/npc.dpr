//
// Nitro Pascal Compiler
// version 1.0
//
// Creation date (dd-mm-yyyy):  6-02-2024
// Modification date         : ..-..-....
//
// Author: Grzegorz Molenda aka NevTon
//         All rights reserved.
//

program npc;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  madExcept,
  madLinkDisAsm,
  madListModules,
  SysUtils,
  Windows,
  StrUtils,
  Math,
  npc_lexer,
  npc_parser,
  npc_project,
  npc_utils;

var
  input_path: String;
  output_path: String;
  temp: String;
  start_time: Real;
  total_time: Real;
//  h,
  m, s, ms: LongWord;
  i, start_idx, decrement: Integer;
  cmd_params: String;

begin
  try
    Writeln('NitroPascal compiler version 1.0');
    Writeln('2024, Grzegorz Molenda');
    Writeln('All rights reserved.');
    Writeln;
    if ParamCount = 0 then begin
      Writeln('npc.exe project_file_path [output_path] [-e:encoding] [-o:options]');
      Writeln('        encoding - ASCII or ANSI or UTF8');
      Writeln('        options:');
      Writeln('          - format_settings{');
      Writeln('              ''date_sep'': ''-'',');
      Writeln('              ''time_sep'': '':'',');
      Writeln('              ''date_fmt'': ''yyyy-mm-dd'',');
      Writeln('              ''time_fmt'': ''hh:nn:ss'',');
      Writeln('              ''1000_sep'': '''',');
      Writeln('              ''frac_sep'': ''.'',');
      Writeln('              ''curr_dec'': 2,');
      Writeln('              ''curr_fmt'': ''#0.00''');
//      Writeln('              ''curr_str'': ''#0.00'',');
      Writeln('            }');
      Writeln('          - output_tokens[');
      Writeln('              P - for ProjectOnly or');
      Writeln('              S - for SourcesOnly or');
      Writeln('              A - for All');
      Writeln('            ]');
      Writeln('          - output_ast[');
      Writeln('              P - for ProjectOnly or');
      Writeln('              S - for SourcesOnly or');
      Writeln('              A - for All');
      Writeln('            ]');
      Writeln;
      Writeln('Try:    npc.exe ''.\tests\simple_project\first.npe''');
      Writeln;
      Exit;
    end;
    //
    output_path := '';
    if ParamCount > 0 then begin
      input_path := ParamStr(1);
      if (ParamCount > 1) then begin
        temp := ParamStr(2);
        if not StartsText('-e:', temp) and not StartsText('-o:', temp) then begin
          output_path := temp;
          start_idx := 3;
          decrement := 2;
        end
        else begin
          start_idx := 2;
          decrement := 1;
        end;
        temp := '';
      end
      else begin
        start_idx := 2;
        decrement := 1;
      end;
      //
      cmd_params := '';
      for i:=start_idx to ParamCount do
        cmd_params := cmd_params + ParamStr(i) + ' ';
      cmd_params := TrimRight(cmd_params);
      NPC_InitCompiler(cmd_params, ParamCount - decrement);
      cmd_params := '';
    end;
    //
    start_time := getTime;
    //
    if NPC_CompileProject(PChar(input_path), PChar(output_path)) then begin
      Writeln;
      Writeln('Project compiled ' + IfThen(CanUseExtendedConsole, #$1b'[92m') + 'successfully' + IfThen(CanUseExtendedConsole, #$1b'[0m') + '.');
    end
    else begin
      Writeln(NPC_ReportErrors);
      Writeln;
    end;
    //
    total_time := getTime - start_time;
    //Writeln(total_time:0:8);
    if total_time < 0 then // if hour changed from 23 to less than that
      total_time := total_time + 3600.0 * 24;
    if Round(Frac(Total_time) * 1000) >= 1000 then
      total_time := Trunc(total_time) + 1;
    //Writeln(Format('Total time (m:s.ms): %d:%d.%.3d', [Trunc(total_time) div 60, Trunc(total_time) mod 60, Round(Frac(total_time) * 1000)]));
//    h := Trunc(total_time) div 3600;
    m := Trunc(total_time) div 60;
    s := Trunc(total_time) mod 60;
    ms := Round(Frac(total_time) * 1000);
    Writeln('Total time: ' + IfThen(m > 0, IntToStr(m) + 'm : ') + IfThen(s > 0, IfThen(s < 10, '0') + IntToStr(s) + 's') + IntToStr(ms) + 'ms');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

