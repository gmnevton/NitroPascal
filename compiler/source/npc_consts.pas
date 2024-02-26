//
// Nitro Pascal Compiler
// version 1.0
//
// Consts
//

unit npc_consts;

interface

uses
  Types;

const
  sErrorBase = '%s: %s';
  sErrorBaseEx = '%s - %s: %s';

  sLexerStreamNotSpecified = 'Stream not specified while trying to read from file "%s".';
  sLexerStreamNotSpecifiedStream = 'Stream not specified while trying to read from stream.';

  sParserUnexpectedTokenInProject     = 'unexpected token "%s" in project file';
  sParserUnexpectedTokenInDeclaration = 'unexpected token "%s" in %s declaration';
  sParserUnexpectedTokenInStatement   = 'unexpected token "%s" in %s statement';
  sParserUnexpectedType               = 'unexpected type "%s", expected "%s"';
  sParserSectionHasNoBody             = '"%s" section has no body';

  sProjectError = '%s - ProjectError(%s): %s';


function ArrayOfCharToArrayOfString(const Values: Array of Char): TStringDynArray;

implementation

function ArrayOfCharToArrayOfString(const Values: Array of Char): TStringDynArray;
var
  i: Integer;
begin
  SetLength(Result, Length(Values));
  for i:=0 to High(Values) do
    Result[i] := Values[i];
end;

end.