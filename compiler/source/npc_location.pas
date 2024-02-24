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
    Row: Integer;
    Col: Integer;
    //
    constructor Create(const AFileName: String; const ARow, ACol: Integer);
    destructor Destroy; override;
    //
    function Copy: TNPCLocation;
    function ToString: String;
  end;

implementation

{ TNPCLocation }

constructor TNPCLocation.Create(const AFileName: String; const ARow, ACol: Integer);
begin
  FilePath := ExtractFilePath(AFileName);
  FileName := ExtractFileName(AFileName);
  Row := ARow;
  Col := ACol;
end;

destructor TNPCLocation.Destroy;
begin
  FileName := '';
  inherited;
end;

function TNPCLocation.Copy: TNPCLocation;
begin
  Result := TNPCLocation.Create(FilePath + FileName, Row, Col);
end;

function TNPCLocation.ToString: String;
begin
  Result := Format('%s (%d:%d)', [FileName, Row, Col]);
end;

end.
