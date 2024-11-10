//
// Nitro Pascal Compiler
// version 1.0
//
// Utilities
//

unit npc_utils;

interface

uses
  SysUtils,
  Classes;

type
  TFastStringList = class(TStringList)
  private
    FAssigning: Boolean;
    FOwnObjects: Boolean;
  protected
    function CompareStrings(const S1, S2: string): Integer; override;
  public
    constructor Create; reintroduce;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Sort; override;
    property OwnObjects: Boolean read FOwnObjects write FOwnObjects default False;
  end;

  TStringArray = Array of String;

function getTime: Real;
function ConsoleAvailable: Boolean;
procedure ConsoleWriteln(const Value: String);
function IsANSIConsoleSupported: Boolean;
procedure SetConsoleANSIMode;

//function ParamValue(P: PChar; Index: Integer): String;
function explode_quotes(const Spliter: Char; const Value: String; const TrimParts: Boolean = True; const PreserveQuotes: Boolean = False): TStringArray; overload;
function explode_quotes(const Spliter: String; const Value: String; const TrimParts: Boolean = True; const PreserveQuotes: Boolean = False): TStringArray; overload;

implementation

uses
  Windows,
  StrUtils;

const
  ATTACH_PARENT_PROCESS: DWORD = $FFFFFFFF;
  ENABLE_VIRTUAL_TERMINAL_INPUT = $0200;
  ENABLE_VIRTUAL_TERMINAL_PROCESSING = $0004;
  ENABLE_PROCESSED_OUTPUT = $0001;

var
  ConsoleAllocated: Boolean;

{ TFastStringList }

constructor TFastStringList.Create;
begin
  inherited Create;
  FOwnObjects:=False;
end;

procedure TFastStringList.Assign(Source: TPersistent);
{ Override the assign to speed up an assign of a Sorted StringList to a Sorted StringList }
begin
  if Sorted and (Source is TStringList) and (TStringList(Source).Sorted) then begin
    BeginUpdate;
    try
      Sorted := False;
      FAssigning := True;
      inherited Assign( Source );
    finally
      Sorted := True;
      FAssigning := False;
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TFastStringList.Clear;
var
  i: Integer;
begin
  if (Self.Count > 0) and FOwnObjects then begin
    for i:=0 to Self.Count-1 do
      if Assigned(Self.Objects[i]) then begin
        Self.Objects[i].Free;
        Self.Objects[i]:=Nil;
      end;
  end;
  inherited Clear;
end;

procedure TFastStringList.Delete(Index: Integer);
begin
  if ((Index >= 0) and (Index <= Self.Count-1)) and FOwnObjects then begin
    if Assigned(Self.Objects[Index]) then begin
      Self.Objects[Index].Free;
      Self.Objects[Index]:=Nil;
    end;
  end;
  inherited Delete(Index);
end;

function TFastStringList.CompareStrings(const S1, S2: string): Integer;
begin
  if CaseSensitive then
    Result := CompareStr(S1, S2)
  else
    Result := CompareText(S1, S2);
end;

procedure TFastStringList.Sort;
begin
  if not FAssigning then
    inherited Sort;
end;

function getTime: Real;
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

function ConsoleAvailable: Boolean;
begin
  Result := ConsoleAllocated;
end;

procedure ConsoleWriteln(const Value: String);
begin
  if ConsoleAllocated then
    Writeln(Value);
end;

function IsANSIConsoleSupported: Boolean;
var
  h: THandle;
  dwOriginalMode: DWORD;
  dwRequestedModes: DWORD;
begin
  Result := False;

  h := GetStdHandle(STD_OUTPUT_HANDLE);
  if h = INVALID_HANDLE_VALUE then
    Exit;

  dwOriginalMode := 0;

  if not GetConsoleMode(h, dwOriginalMode) then
    Exit;

  dwRequestedModes := ENABLE_PROCESSED_OUTPUT or ENABLE_VIRTUAL_TERMINAL_PROCESSING;

  Result := (dwOriginalMode and dwRequestedModes) = dwRequestedModes;
end;

procedure SetConsoleANSIMode;
var
  hIn: THandle;
  hOut: THandle;
  dwOriginalInMode,
  dwRequestedInModes: DWORD;
  dwOriginalOutMode,
  dwRequestedOutModes: DWORD;
  dwInMode,
  dwOutMode: DWORD;
begin
  hIn := GetStdHandle(STD_INPUT_HANDLE);
  if hIn = INVALID_HANDLE_VALUE then
    Exit;

  hOut := GetStdHandle(STD_OUTPUT_HANDLE);
  if hOut = INVALID_HANDLE_VALUE then
    Exit;

  dwOriginalInMode := 0;
  dwOriginalOutMode := 0;

  if not GetConsoleMode(hIn, dwOriginalInMode) then
    Exit;

  if not GetConsoleMode(hOut, dwOriginalOutMode) then
    Exit;

  dwRequestedInModes := ENABLE_VIRTUAL_TERMINAL_INPUT;
  dwRequestedOutModes := ENABLE_PROCESSED_OUTPUT or ENABLE_VIRTUAL_TERMINAL_PROCESSING;

  dwInMode := dwOriginalInMode and dwRequestedInModes;
  if not SetConsoleMode(hIn, dwInMode) then // we failed to set mode
    Exit;

  dwOutMode := dwOriginalOutMode or dwRequestedOutModes;
  if not SetConsoleMode(hOut, dwOutMode) then begin  // we failed to set both modes, try to step down mode gracefully.
    dwRequestedOutModes := ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    dwOutMode := dwOriginalOutMode or dwRequestedOutModes;
    if not SetConsoleMode(hOut, dwOutMode) then // Failed to set any VT mode, can't do anything here.
      Exit;
  end;
end;

//function GetParamValue(P: PChar; var Param: String): PChar;
//var
//  i, Len: Integer;
//  Start, S: PChar;
//begin
//  while True do begin
//    while (P[0] <> #0) and (P[0] <= ' ') do
//      Inc(P);
//    if (P[0] = '"') and (P[1] = '"') then
//      Inc(P, 1)
//    else
//      Break;
//  end;
//  Len := 0;
//  Start := P;
//  while P[0] > ' ' do begin
//    if P[0] = '"' then begin
//      Inc(Len);
//      Inc(P);
//      while (P[0] <> #0) and (P[0] <> '"') do begin
//        Inc(Len);
//        Inc(P);
//      end;
//      if P[0] <> #0 then begin
//        Inc(Len);
//        Inc(P);
//      end;
//    end
//    else begin
//      Inc(Len);
//      Inc(P);
//    end;
//  end;
//
//  SetLength(Param, Len);
//
//  P := Start;
//  S := Pointer(Param);
//  i := 0;
//  while P[0] > ' ' do begin
//    if P[0] = '"' then begin
//      S[i] := P^;
//      Inc(P);
//      Inc(i);
//      while (P[0] <> #0) and (P[0] <> '"') do begin
//        S[i] := P^;
//        Inc(P);
//        Inc(i);
//      end;
//      if P[0] <> #0 then begin
//        S[i] := P^;
//        Inc(P);
//        Inc(i);
//      end;
//    end
//    else begin
//      S[i] := P^;
//      Inc(P);
//      Inc(i);
//    end;
//  end;
//
//  Result := P;
//end;
//
//function ParamValue(P: PChar; Index: Integer): String;
//begin
//  Result := '';
//  while True do begin
//    P := GetParamValue(P, Result);
//    if (Index = 0) or (Result = '') then Break;
//    Dec(Index);
//  end;
//end;

function explode_quotes(const Spliter: Char; const Value: String; const TrimParts: Boolean = True; const PreserveQuotes: Boolean = False): TStringArray;
begin
  Result:=explode_quotes(String(Spliter), Value, TrimParts, PreserveQuotes);
end;

function explode_quotes(const Spliter: String; const Value: String; const TrimParts: Boolean = True; const PreserveQuotes: Boolean = False): TStringArray; overload;
var
  poz, sl: Integer;
  temp_value, temp: String;

  function inQuotes(const Text: String): Boolean;
  var
    C: Char;
  begin
    Result:=False;
    for C in Text do
      if CharInSet(C, ['''', '"']) then
        Result:=not Result;
  end;

  function deQuoted(var Text: String): Boolean;
  var
    len: Integer;
    quote_char: Char;
  begin
    Result:=False;
    if PreserveQuotes then
      Exit;
    if (Pos('''', Text) > 1) or (Pos('"', Text) > 1) then
      Text:=TrimLeft(Text);
    len:=Length(Text);
    if len > 0 then begin
      quote_char:=#0;
      if CharInSet(Text[1], ['''', '"']) then begin
        quote_char:=Text[1];
        Delete(Text, 1, 1);
        Dec(len);
        Result:=True;
      end;
      if (quote_char <> #0) and (len > 0) and (Text[len] = quote_char) then
        Delete(Text, len, 1);
    end;
  end;

begin
  SetLength(Result, 0);
  temp_value:=TrimRight(Value);
  sl:=Length(Spliter);
  if (Length(temp_value) >= sl) and (temp_value[Length(temp_value) - sl + 1] = Spliter) then
    Delete(temp_value, Length(temp_value) - sl + 1, sl)
  else
    temp_value:=Value;

  poz:=1;
  while temp_value <> '' do begin
    poz:=PosEx(Spliter, temp_value, poz);
    if poz > 0 then begin
      temp:=Copy(temp_value, 1, poz - 1);
      if inQuotes(temp) then begin
        Inc(poz);
        Continue;
      end;
      SetLength(Result, Length(Result) + 1);
      if not deQuoted(temp) and TrimParts then
        temp:=Trim(temp);
//      deQuoted(temp);
      Result[High(Result)]:=temp;
      Delete(temp_value, 1, poz + sl - 1);
      poz:=1;
    end
    else begin
      poz:=Length(temp_value) + 1;
      temp:=Copy(temp_value, 1, poz - 1);
      SetLength(Result, Length(Result) + 1);
      if not deQuoted(temp) and TrimParts then
        temp:=Trim(temp);
//      deQuoted(temp);
      Result[High(Result)]:=temp;
      Delete(temp_value, 1, poz);
    end;
  end;
end;

function AttachConsole(dwProcessId: DWORD): BOOL; stdcall; external 'kernel32.dll';

initialization
  ConsoleAllocated := False;
  if AttachConsole(ATTACH_PARENT_PROCESS) then
    ConsoleAllocated := True
  else begin
    ConsoleAllocated := (GetLastError = ERROR_ACCESS_DENIED);
  end;

end.