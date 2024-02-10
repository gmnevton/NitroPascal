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
  sLexerStreamNotSpecified = 'Stream not specified while trying to read from file "%s".';
  sLexerError = '%s - LexerError: %s';

  sParserErrorBase = '%s - ParserError: ';
  sParserError = sParserErrorBase + '%s';
  sParserUnexpectedTokenInProject = sParserErrorBase + 'unexpected token "%s" in project file';
  sParserUnexpectedType = sParserErrorBase + 'unexpected type "%s", expected "%s"';

  sProjectError = '%s - Error(%s): %s';

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