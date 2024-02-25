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
  npc_consts;

const
  iShowSourceCodeLines = 3;

{ TNPCError }

function GetSourceCodeLines(const ALocation: TNPCLocation; ALines: Integer): String;
var
  list: TStringList;
  i, line_start, line_end: Integer;
begin
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
      for i:=line_start to line_end - 1 do begin
        if Length(Result) = 0 then begin
          if Length(list.Strings[i]) > 0 then
            Result := '  >  ' + list.Strings[i];
        end
        else begin
          if (i = ALocation.EndRow - 1) then begin
            if (Length(list.Strings[i]) > 0) then
              Result := Result + #13#10 + '  >  ' +list.Strings[i];
          end
          else begin
            Result := Result + #13#10 + '  >  ' +list.Strings[i];
          end;
        end;
        if i = ALocation.StartRow - 1 then
          Result := Result + #13#10 + '  >--' + DupeString('-', ALocation.EndCol - ALocation.StartCol) + '^';
      end;
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
