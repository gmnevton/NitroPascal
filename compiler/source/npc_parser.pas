//
// Nitro Pascal Compiler
// version 1.0
//
// Parser
//

unit npc_parser;

interface

uses
  SysUtils,
  Classes,
  npc_source_parser,
  npc_utils,
  npc_error;

type
  TNPCProjectParser = class(TNPCSourceParser)
  public
    procedure ParseProject;
  end;

implementation

uses
  npc_project;

{ TNPCProjectParser }

procedure TNPCProjectParser.ParseProject;
begin
  ParsingType := PROJECT;
  Parse(Nil, '');
end;

end.

