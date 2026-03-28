//
// Nitro Pascal Compiler
// version 1.0
//
// Source code location info
//

unit npc_location;

interface

uses
  SysUtils,
  Classes;

type
  TNPCLocation = class
  public
    FilePath: String;
    FileName: String;
    StartRow: Integer;
    StartCol: Integer;
    EndRow: Integer;
    EndCol: Integer;
    //
    constructor Create(const AFileName: String; const ARow, ACol: Integer);
    destructor Destroy; override;
    //
    procedure SetEndRowCol(const AEndRow, AEndCol: Integer); overload; inline;
    procedure SetEndRowCol(const AOtherLoc: TNPCLocation); overload; inline;
    procedure IncEndCol(const ASize: Integer); inline;
    procedure SetEndAsStart; inline;
    function GetLocationSize: Integer; inline;
    function Copy: TNPCLocation; inline;
    function After: TNPCLocation; inline;
    function ToString: String; inline;
  end;

implementation

uses
  Math;

{ TNPCLocation }

constructor TNPCLocation.Create(const AFileName: String; const ARow, ACol: Integer);
begin
  FilePath := ExtractFilePath(AFileName);
  FileName := ExtractFileName(AFileName);
  StartRow := ARow;
  StartCol := ACol;
  EndRow   := ARow;
  EndCol   := -1;
end;

destructor TNPCLocation.Destroy;
begin
  FileName := '';
  inherited;
end;

procedure TNPCLocation.SetEndRowCol(const AEndRow, AEndCol: Integer);
begin
  EndRow := AEndRow;
  EndCol := AEndCol;
end;

procedure TNPCLocation.SetEndRowCol(const AOtherLoc: TNPCLocation);
begin
  if not Assigned(AOtherLoc) then
    Exit;
  //
  EndRow := AOtherLoc.EndRow;
  EndCol := AOtherLoc.EndCol;
end;

procedure TNPCLocation.IncEndCol(const ASize: Integer);
begin
  EndCol := EndCol + ASize + IfThen(EndCol = -1, 1);
end;

procedure TNPCLocation.SetEndAsStart;
begin
  StartRow := EndRow;
  StartCol := EndCol;
end;

function TNPCLocation.GetLocationSize: Integer;
begin
  Result := 1;
  if EndCol > -1 then
    Result := EndCol - StartCol;
end;

function TNPCLocation.Copy: TNPCLocation;
begin
  Result := TNPCLocation.Create(FilePath + FileName, StartRow, StartCol);
  Result.EndRow := Self.EndRow;
  Result.EndCol := Self.EndCol;
end;

function TNPCLocation.After: TNPCLocation;
begin
  Result := TNPCLocation.Create(FilePath + FileName, StartRow, StartCol);
  Result.EndRow := Self.EndRow;
  Result.StartCol := IfThen(Self.EndCol > -1, Self.EndCol, Self.StartCol) + 1;
  Result.EndCol := -1;
end;

function TNPCLocation.ToString: String;
begin
  Result := Format('%s (%d:%d)', [FileName, StartRow, StartCol]);
end;

end.

