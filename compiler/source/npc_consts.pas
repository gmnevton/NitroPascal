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
  sErrorBase   = '%s: %s';
  sErrorBaseEx = '%s - %s: %s';

  sLexerStreamNotSpecified       = 'Stream not specified while trying to read from file "%s".';
  sLexerStreamNotSpecifiedStream = 'Stream not specified while trying to read from stream.';

  sProjectFile    = 'project file';
  sCodeFile       = 'code file';
  sProjectSetting = 'project setting';
  sSection        = 'section';
  sDeclaration    = 'declaration';
  sStatement      = 'statement';
  sExpression     = 'expression';

  sParserUnexpectedTokenIn         = 'unexpected token "%s" in %s%s';
  sParserUnexpectedType            = 'unexpected type "%s", expected "%s"';
  sPareserUnexpectedSyntax         = 'unexpected %s syntax, expected %s';
  sParserSectionHasNoBody          = '"%s" section has no body';
  sParserUnknownIdentIn            = 'unknown identifier "%s" in %s%s';
  sParserUnknownDirectiveNameIn    = 'unknown setting or define directive name "%s" in %s%s';
  sParserExpectedButGot            = 'expected "%s", but got "%s"';
  sParserExpectedElementsButGot    = 'expected %s, but got "%s" in %s%s';
  sParserImportNotFound            = 'import not found "%s"';
  sParserCantAccessFieldOnNonVar   = 'can''t access field "%s" on non-variable expression "%s"';
  sParserTypeRequiredForIdent      = '"<%s>" type required for identifier "%s", but got "%s"';
  sParserFieldNotFoundInRecord     = 'field "%s" not found in record "%s"';
  sParserTypeDefNotFound           = 'type definition for "%s" not found';
  sParserExpectedNameAfter         = 'expected %s name after %s%s';
  sParserExpectedNameAfterKeyword  = 'expected %s name after "%s" keyword';
  sParserTypeMismatchInLiteral     = 'type mismatch in %s: expected %s, found %s';
  sParserUnknownTypeFor            = 'unknown type "%s" for %s%s';
  sParserUnknown                   = 'unknown %s: %s';
  sParserLeftSideOfMustBe          = 'left side of %s must be a %s';
//  sParserEmptySetAssignRequiresSet = 'can''t assign empty set to variable "%s"';
  sParserIntrinsicFuncExpects      = 'intrinsic function %s expects %s as %s';
  sParserIdentifierNotValidInExpr  = 'identifier not valid in expression: %s';

  sProjectError = '%s - ProjectError(%s): %s';

  s8low   = -128;
  s8hi    = 127;
  s16low  = -32768;
  s16hi   = 32767;
  s32low  = -2147483648;
  s32hi   = 2147483647;
  s64low  = -9223372036854775808;
  s64hi   = 9223372036854775807;
//  s128low = -170141183460469231731687303715884105728;
//  s128hi  = 170141183460469231731687303715884105727;

  u8low   = 0;
  u8hi    = 255;
  u16low  = 0;
  u16hi   = 65535;
  u32low  = 0;
  u32hi   = 4294967295;
  u64low  = 0;
  u64hi   = 18446744073709551615;
//  u128low = 0;
//  u128hi  = 340282366920938463463374607431768211455;

  f32low  = 1.18e-38;
  f32hi   = 3.40e+38;
  f64low  = 2.23e-308;
  f64hi   = 1.79e+308;
  f128low = 3.37e-4932;
  f128hi  = 1.18e+4932;

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

