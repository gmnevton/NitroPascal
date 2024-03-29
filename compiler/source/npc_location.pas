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
    procedure SetEndRowCol(const AEndRow, AEndCol: Integer); inline;
    function Copy: TNPCLocation; inline;
    function ToString: String; inline;
  end;

implementation

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

function TNPCLocation.Copy: TNPCLocation;
begin
  Result := TNPCLocation.Create(FilePath + FileName, StartRow, StartCol);
  Result.EndRow := Self.EndRow;
  Result.EndCol := Self.EndCol;
end;

function TNPCLocation.ToString: String;
begin
  Result := Format('%s (%d:%d)', [FileName, StartRow, StartCol]);
end;

end.
