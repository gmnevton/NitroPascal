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

library npc_lib;

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListModules,
  npc_exports;

{$R *.res}

begin
  IsMultiThread := True;
end.

