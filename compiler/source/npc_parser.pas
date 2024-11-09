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
  NPCProjectParserException = class(TNPCError);

  TNPCProjectParser = class
  private
    Source: TNPCSourceParser;
    Settings: Pointer;
  protected
  public
    constructor Create(const PSettings: Pointer);
    destructor Destroy; override;
    //
    procedure ParseProject;
    procedure ParseImportFile(const ASourceFile: String); overload;
    procedure ParseImportFile(const ASourceFile: TStringStream); overload;
    procedure ParseSourceCode(const ASourceCode: String); overload;
    procedure ParseSourceCode(const ASourceCode: TStringStream); overload;
    procedure OutputTokens;
  end;

implementation

uses
  npc_project;

{ TNPCProjectParser }

constructor TNPCProjectParser.Create(const PSettings: Pointer);
begin
  Settings := PSettings;
  Source := TNPCSourceParser.Create(Settings);
end;

destructor TNPCProjectParser.Destroy;
begin
  Settings := Nil;
  FreeAndNil(Source);
  inherited;
end;

procedure TNPCProjectParser.ParseProject;
begin
  Source.ParseProject;
end;

procedure TNPCProjectParser.ParseImportFile(const ASourceFile: String);
begin

end;

procedure TNPCProjectParser.ParseImportFile(const ASourceFile: TStringStream);
begin

end;

procedure TNPCProjectParser.ParseSourceCode(const ASourceCode: String);
begin

end;

procedure TNPCProjectParser.ParseSourceCode(const ASourceCode: TStringStream);
begin

end;

procedure TNPCProjectParser.OutputTokens;
begin

end;

end.

