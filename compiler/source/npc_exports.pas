//
// Nitro Pascal Compiler
// version 1.0
//
// Exports
//

unit npc_exports;

interface

uses
  SysUtils,
  Classes,
  npc_lexer,
  npc_parser,
  npc_project;

exports
  NPC_CompileProject,
  NPC_ReportErrors,
  NPC_SetProjectEncoding,
  NPC_SetProjectFormatSettings
  ;

implementation

end.
