//
// Nitro Pascal Compiler
// version 1.0
//
// Compiler
//

unit npc_compiler;

interface

uses
  SysUtils,
  Classes,
  Contnrs,
  npc_location;

type
  TNPCDeclarationValueType = (
    // unknown
    value_unknown,

    // unsigned integer
    value_u8,   // 0..255
    value_u16,  // 0..32767
    value_u32,  // 0..
    value_u64,  // 0..
    value_u128, // 0..

    // signed integer
    value_s8,   // -128..127
    value_s16,  // -32768..32767
    value_s32,  // -..
    value_s64,  // -..
    value_s128, // -..

    // float
    value_f32,
    value_f64,
    value_f80,

    // string
    value_string,

    // record
    value_record,

    // class
    value_class
  );

  TNPCDeclaration = class;

  TNPCCompiler = class
  private
    FDeclarations: TObjectList;
    FScope: TObjectList;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Clear;
    //
    function Declare(const ALocation: TNPCLocation; const AName: String; const AValue: String; const AValueType: TNPCDeclarationValueType; const AValueIsConst: Boolean = False; const AValueIsReference: Boolean = False): Integer;
    function FindDeclaration(const AName: String): TNPCDeclaration;
  end;

  TNPCDeclaration = class
  private
    FName: String;
    FValue: String;
    FLocation: TNPCLocation;
    FValueType: TNPCDeclarationValueType;
    FValueIsConst: Boolean;
    FValueIsReference: Boolean;
    FValueIsReferenced: Boolean; // is used or not
  public
    constructor Create;
    destructor Destroy; override;
    //
    property Name: String read FName;
    property Value: String read FValue;
    property Location: TNPCLocation read FLocation;
    property ValueType: TNPCDeclarationValueType read FValueType;
    property ValueIsConst: Boolean read FValueIsConst;
    property ValueIsReference: Boolean read FValueIsReference;
    property ValueIsReferenced: Boolean read FValueIsReferenced;
  end;

implementation

{ TNPCCompiler }

constructor TNPCCompiler.Create;
begin
  FDeclarations := TObjectList.Create(True);
  FScope := TObjectList.Create(True);
end;

destructor TNPCCompiler.Destroy;
begin
  FDeclarations.Free;
  FScope.Free;
  inherited;
end;

procedure TNPCCompiler.Clear;
begin
  FDeclarations.Clear;
  FScope.Clear;
end;

function TNPCCompiler.Declare(const ALocation: TNPCLocation; const AName, AValue: String; const AValueType: TNPCDeclarationValueType; const AValueIsConst, AValueIsReference: Boolean): Integer;
var
  declaration: TNPCDeclaration;
begin
  if FindDeclaration(AName) <> Nil then
    Exit(-1); // declaration exists
  //
  declaration := TNPCDeclaration.Create;
  declaration.FName := AName;
  declaration.FValue := AValue;
  declaration.FLocation := ALocation.Copy;
  declaration.FValueType := AValueType;
  declaration.FValueIsConst := AValueIsConst;
  declaration.FValueIsReference := AValueIsReference;

  Result := FDeclarations.Add(declaration);
end;

function TNPCCompiler.FindDeclaration(const AName: String): TNPCDeclaration;
var
  i: Integer;
  declaration: TNPCDeclaration;
begin
  for i:=0 to FDeclarations.Count - 1 do begin
    declaration := TNPCDeclaration(FDeclarations.Items[i]);
    if SameText(declaration.FName, AName) then begin
      Result := declaration;
      Exit;
    end;
  end;
  Result := Nil;
end;

{ TNPCDeclaration }

constructor TNPCDeclaration.Create;
begin
  FName := '';
  FValue := '';
  FLocation := Nil;
  FValueType := value_unknown;
  FValueIsConst := False;
  FValueIsReference := False;
  FValueIsReferenced := False;
end;

destructor TNPCDeclaration.Destroy;
begin
  FName := '';
  FValue := '';
  FLocation.Free;
  inherited;
end;

end.

