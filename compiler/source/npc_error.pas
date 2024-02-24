//
// Nitro Pascal Compiler
// version 1.0
//
// Reports errors
//

unit npc_error;

interface

uses
  SysUtils,
  npc_location;

type
  TNPCError = class(Exception)
  public
    Location: TNPCLocation;
//    constructor Create(const ALocation: TNPCLocation; const AError: String); reintroduce;
//    constructor CreateFmt(const ALocation: TNPCLocation; const Msg: String; const Args: Array of const); reintroduce;
    constructor CompilerError(const AError: String);
    constructor LexerError(const ALocation: TNPCLocation; const AError: String);
    constructor ParserError(const ALocation: TNPCLocation; const AError: String);
    constructor ProjectError(const AError: String);
    destructor Destroy; override;
  end;

implementation

uses
  npc_consts;

{ TNPCError }

//constructor TNPCError.Create(const ALocation: TNPCLocation; const AError: String);
//begin
//  Location := ALocation;
//  Message := Format(sErrorBase, [Location.ToString, AError]);
//end;
//
//constructor TNPCError.CreateFmt(const ALocation: TNPCLocation; const Msg: String; const Args: Array of const);
//begin
//  Create(ALocation, Format(Msg, Args));
//end;

constructor TNPCError.CompilerError(const AError: String);
begin
  inherited CreateFmt(sErrorBase, ['CompilerError', AError]);
end;

constructor TNPCError.LexerError(const ALocation: TNPCLocation; const AError: String);
begin
  Location := ALocation;
  inherited CreateFmt(sErrorBaseEx, [Location.ToString, 'LexerError', AError]);
end;

constructor TNPCError.ParserError(const ALocation: TNPCLocation; const AError: String);
begin
  Location := ALocation;
  inherited CreateFmt(sErrorBaseEx, [Location.ToString, 'ParserError', AError]);
end;

constructor TNPCError.ProjectError(const AError: String);
begin
  inherited CreateFmt(sErrorBase, ['ProjectError', AError]);
end;

destructor TNPCError.Destroy;
begin
  Location := Nil;
  Message := '';
  inherited;
end;

end.
