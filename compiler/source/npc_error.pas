//
// Nitro Pascal Compiler
// version 1.0
//
// Reports errors
//

unit npc_error;

interface

uses
  SysUtils,
  npc_location;

type
  TNPCError = class(Exception)
  public
    Location: TNPCLocation;
//    constructor Create(const ALocation: TNPCLocation; const AError: String); reintroduce;
//    constructor CreateFmt(const ALocation: TNPCLocation; const Msg: String; const Args: Array of const); reintroduce;
    constructor CompilerError(const AError: String);
    constructor LexerError(const ALocation: TNPCLocation; const AError: String);
    constructor ParserError(const ALocation: TNPCLocation; const AError: String);
    constructor ProjectError(const AError: String);
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  StrUtils,
  Math,
  npc_consts,
  npc_utils;

const
  iShowSourceCodeLines = 3;

{ TNPCError }

function GetSourceCodeLines(const ALocation: TNPCLocation; ALines: Integer): String;
const
  BLACK     = #$1b'[30m';
  RED       = #$1b'[31m';
  GREEN     = #$1b'[32m';
  YELLOW    = #$1b'[33m';
  BLUE      = #$1b'[34m';
  MAGENTA   = #$1b'[35m';
  CYAN      = #$1b'[36m';
  LIGHTGRAY = #$1b'[37m';

  BG_BLACK     = #$1b'[40m';
  BG_RED       = #$1b'[41m';
  BG_GREEN     = #$1b'[42m';
  BG_YELLOW    = #$1b'[43m';
  BG_BLUE      = #$1b'[44m';
  BG_MAGENTA   = #$1b'[45m';
  BG_CYAN      = #$1b'[46m';
  BG_LIGHTGRAY = #$1b'[47m';

  DARKGRAY     = #$1b'[90m';
  LIGHTRED     = #$1b'[91m';
  LIGHTGREEN   = #$1b'[92m';
  LIGHTYELLOW  = #$1b'[93m';
  LIGHTBLUE    = #$1b'[94m';
  LIGHTMAGENTA = #$1b'[95m';
  LIGHTCYAN    = #$1b'[96m';
  WHITE        = #$1b'[97m';

  BG_LIGHT_BLACK     = #$1b'[100m';
  BG_LIGHT_RED       = #$1b'[101m';
  BG_LIGHT_GREEN     = #$1b'[102m';
  BG_LIGHT_YELLOW    = #$1b'[103m';
  BG_LIGHT_BLUE      = #$1b'[104m';
  BG_LIGHT_MAGENTA   = #$1b'[105m';
  BG_LIGHT_CYAN      = #$1b'[106m';
  BG_LIGHT_LIGHTGRAY = #$1b'[107m';

//  WHITE   = #$1b'[0;97m';
//  GUTTER  = #$1b'[0;32m';
//  NORMAL  = #$1b'[0;96m';
//  HILIGHT = #$1b'[1;91m';
//  RESET   = #$1b'[0m';

  GUTTER  = GREEN;
  NORMAL  = LIGHTCYAN;
  HILIGHT = LIGHTRED;

  RESET   = #$1b'[0m';
var
  ansi: Boolean;
  in_comment: Boolean;
  comment_end_tag: String;

  function number(var P: PChar): String;
  var
    S: PChar;
  begin
    Result := '';
    S := P;
    while P^ in ['0'..'9', '-', '.'] do Inc(P);
    SetString(Result, S, P - S);
    Result := MAGENTA + Result + RESET;
  end;

  function literal(var P: PChar): String;
  begin
    Result := P^;
    Inc(P);
    Result := YELLOW + Result + RESET;
  end;

  function ident(var P: PChar): String;
  var
    S: PChar;
  begin
    Result := '';
    S := P;
    while P^ in ['a'..'z', 'A'..'Z', '_', '.'] do Inc(P);
    SetString(Result, S, P - S);
    Result := LIGHTCYAN + Result + RESET;
  end;

  function comment(var P: PChar; const EndTag: String): String;
  var
    S: PChar;
    idx: Byte;
  begin
    Result := '';
    S := P;
    if Length(EndTag) = 0 then begin
      while P^ <> #0 do Inc(P);
    end
    else begin
      idx := 0;
      while (P^ <> #0) and not (P^ = EndTag[idx]) do Inc(P);
      Inc(idx);
      if (P^ <> #0) and not (P^ = EndTag[idx]) then begin
        in_comment := True;
        comment_end_tag := EndTag;
      end
      else begin
        in_comment := False;
        comment_end_tag := '';
      end;
    end;
    SetString(Result, S, P - S);
    Result := DARKGRAY + Result + RESET;
  end;

  function norm(var P: PChar): String;
  begin
    Result := P^;
    Inc(P);
    Result := LIGHTGRAY + Result + RESET;
  end;

  function strline(var P: PChar): String;
  var
    S: PChar;
  begin
    Result := '';
    S := P;
    Inc(P);
    while (P^ <> #0) and not (P^ = '''') do Inc(P);
    SetString(Result, S, P - S);
    Result := WHITE + Result + RESET;
  end;

  function syntax_highlight(line: String): String;
  var
    P, S: PChar;
  begin
    if not ansi then begin
      Result := line;
      Exit;
    end;

    Result := '';
    P := PChar(line);
    if in_comment then
      Result := Result + comment(P, comment_end_tag)
    else begin
      while True do begin
        case P^ of
          #0: Break;
          '0'..'9': Result := Result + number(P);
          '-': begin
            S := P;
            Inc(S);
            if S^ in ['0'..'9'] then
              Result := Result + number(P)
            else
              Result := Result + literal(P);
          end;
          '+': Result := Result + literal(P);
          '/': begin
            S := P;
            Inc(S);
            if S^ = '/' then
              Result := Result + comment(P, '')
            else
              Result := Result + literal(P);
          end;
          '{': Result := Result + comment(P, '}');
          '(': begin
            S := P;
            Inc(S);
            if S^ = '*' then
              Result := Result + comment(P, '*)')
            else
              Result := Result + literal(P);
          end;
          'A'..'Z': Result := Result + ident(P);
          'a'..'z': Result := Result + ident(P);
          '''': Result := Result + strline(P);
        else
          Result := Result + norm(P);
        end;
      end;
    end;
  end;

  function PrintGutter(width, num: Integer): String;
  begin
    Result := Format(IfThen(ansi, GUTTER) + ' %.' + IntToStr(width) + 'd' + IfThen(ansi, WHITE) + '|' + IfThen(ansi, RESET) + ' '{ + IfThen(ansi, NORMAL)}, [num]);
  end;

var
  list: TStringList;
  i, j, num_len, line_len, line_start, line_end, ident_start, ident_end: Integer;
  line: String;
begin
  ansi := False;
  if ConsoleAvailable then begin
    ansi := IsANSIConsoleSupported;
    if not ansi then begin
      SetConsoleANSIMode;
      ansi := IsANSIConsoleSupported;
    end;
  end;

  in_comment := False;
  Result := '';
  try
    list := TStringList.Create;
    try
      list.LoadFromFile(ALocation.FilePath + ALocation.FileName);

      line_start := ALocation.StartRow - 1 - ALines;
      if line_start < 0 then
        line_start := 0;

      line_end := ALocation.EndRow - 1 + ALines;
      if line_end > list.Count then
        line_end := list.Count;

      while (line_start < ALocation.StartRow - 1) and (Length(list.Strings[line_start]) = 0) do
        Inc(line_start);

      while (line_end > ALocation.EndRow - 1) and (Length(list.Strings[line_end]) = 0) do
        Dec(line_end);

      num_len := 1;
      while (line_end div Trunc(Power(10, num_len))) > 0 do
        Inc(num_len);

//      if ansi then
//        Result := NORMAL;

      for i:=line_start to line_end do begin
        if i = line_start then
          Result := Result + PrintGutter(num_len, i + 1)
        else
          Result := Result + #13#10 + PrintGutter(num_len, i + 1);

        if ansi then begin
          if i = ALocation.StartRow - 1 then begin
//            Result := Result + Copy(list.Strings[i], 1, ALocation.StartCol - 1);
            line := list.Strings[i];
            line_len := Length(line);
            ident_start := ALocation.StartCol;
            ident_end := IfThen(ALocation.EndCol > 0, ALocation.EndCol, ALocation.StartCol);
            if ident_start <= line_len then begin
              for j:=1 to line_len do begin
                if j = ident_start then
                  Result := Result + HILIGHT;

                Result := Result + line[j];

                if j = ident_end then
                  Result := Result + NORMAL;
              end;
            end
            else begin
              Result := Result + syntax_highlight(list.Strings[i]) + DupeString(' ', ALocation.StartCol - line_len - 1);
              Result := Result + BG_LIGHT_RED + WHITE;
              Result := Result + ' ';
              Result := Result + RESET + NORMAL;
            end;
          end
          else
            Result := Result + syntax_highlight(list.Strings[i]);
        end
        else begin
          Result := Result + list.Strings[i];
          if i = ALocation.StartRow - 1 then
            Result := Result + #13#10 + DupeString(' ', j + 1) + '| ' + DupeString(' ', ALocation.StartCol - 1) + DupeString('~', ALocation.EndCol - ALocation.StartCol);
        end;
      end;

      if ansi then
        Result := Result + RESET;
    finally
      list.Free;
    end;
  except
    // nothing here
  end;
end;

//constructor TNPCError.Create(const ALocation: TNPCLocation; const AError: String);
//begin
//  Location := ALocation;
//  Message := Format(sErrorBase, [Location.ToString, AError]);
//end;
//
//constructor TNPCError.CreateFmt(const ALocation: TNPCLocation; const Msg: String; const Args: Array of const);
//begin
//  Create(ALocation, Format(Msg, Args));
//end;

constructor TNPCError.CompilerError(const AError: String);
begin
  inherited CreateFmt(sErrorBase, ['CompilerError', AError]);
end;

constructor TNPCError.LexerError(const ALocation: TNPCLocation; const AError: String);
var
  temp: String;
begin
  Location := ALocation;
  temp := Format(sErrorBaseEx, [Location.ToString, 'LexerError', AError]);
  temp := temp + #13#10#13#10 + GetSourceCodeLines(Location, iShowSourceCodeLines);
  inherited Create(temp);
end;

constructor TNPCError.ParserError(const ALocation: TNPCLocation; const AError: String);
var
  temp: String;
begin
  Location := ALocation;
  temp := Format(sErrorBaseEx, [Location.ToString, 'ParserError', AError]);
  temp := temp + #13#10#13#10 + GetSourceCodeLines(Location, iShowSourceCodeLines);
  inherited Create(temp);
end;

constructor TNPCError.ProjectError(const AError: String);
begin
  inherited CreateFmt(sErrorBase, ['ProjectError', AError]);
end;

destructor TNPCError.Destroy;
begin
  Location := Nil;
  Message := '';
  inherited;
end;

end.
