//
// Nitro Pascal Compiler
// version 1.0
//
// Utilities
//

unit npc_utils;

interface

function getRealTime: Real;
procedure ConsoleWriteln(const Value: String);

implementation

uses
  Windows;

const
  ATTACH_PARENT_PROCESS: DWORD = $FFFFFFFF;

var
  ConsoleAllocated: Boolean;

function AttachConsole(dwProcessId: DWORD): BOOL; stdcall; external 'kernel32.dll';

function getRealTime: Real;
var
  st: TSystemTime;
begin
  GetLocalTime(st); // get local system time
  // in st struct there is:
  // .wHour   - actual hour; not used by us, but needed in case of hour change
  // .wMinute - actual minute
  // .wSecond - actual second
  // .wMilliseconds - how many milisecond in a second there was
  // now we make sum of it, where whole side of the real number is seconds and fractional side is miliseconds
  Result := st.wHour * 3600.0 + st.wMinute * 60.0 + st.wSecond + (st.wMilliseconds / 1000.0);
end;

procedure ConsoleWriteln(const Value: String);
begin
  if ConsoleAllocated then
    Writeln(Value);
end;

initialization
  ConsoleAllocated := False;
  if AttachConsole(ATTACH_PARENT_PROCESS) then
    ConsoleAllocated := True
  else begin
    ConsoleAllocated := (GetLastError = ERROR_ACCESS_DENIED);
  end;

end.