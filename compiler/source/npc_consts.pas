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

  sProjectFile    = 'project file';
  sProjectSetting = 'project setting';
  sSection        = 'section';
  sDeclaration    = 'declaration';
  sStatement      = 'statement';
  sExpression     = 'expression';

  sParserUnexpectedTokenIn      = 'unexpected token "%s" in %s%s';
  sParserUnexpectedType         = 'unexpected type "%s", expected "%s"';
  sParserSectionHasNoBody       = '"%s" section has no body';
  sParserUnknownIdentIn         = 'unknown identifier "%s" in %s%s';
  sParserUnknownDirectiveNameIn = 'unknown setting or define directive name "%s" in %s%s';
  sParserExpectedButGot         = 'expected "%s", but got "%s"';

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

