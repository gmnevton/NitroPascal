//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2026-06-26
// Modified: 2026-06
// All rights reserved.
//

unit ned_splitview_manager;

interface

uses
  SysUtils,
  Classes,
  Controls,
  ExtCtrls,
  SplitEx;

type
  TNEDSplitViewTypeEnum = (
    stSplitNone,
    stSplitH,
    stSplitV,
    stSplitLastOpposite
  );

  //TNEDSplitterControlClass = class of TGraphicControl;

  TNEDSplittingManager = class
  private
    FSplittingBaseControl: TWinControl;
    //FSplitterControl: TSplitterControlClass;
    FSplitCols: Byte;
    FSplitRows: Byte;
    FMaxSplitCols: Byte;
    FMaxSplitRows: Byte;
    FLastSplitH: Boolean;
    //
    FBaseControlColArray: Array of TWinControl;
    FBaseControlRowArray: Array of TWinControl;
  protected
    function GetBaseControl(const Horz: Boolean): TWinControl;
    procedure AddCol(const ctrl: TWinControl);
    procedure AddRow(const ctrl: TWinControl);
    function CreateBasePanel(const BaseControl: TWinControl; const Align: TAlign): TPanel;
    function CreateSplitter(const BaseControl, AssignedControl: TWinControl; const Align: TAlign): TSplitterEx;
  public
    constructor Create(const BaseControl: TWinControl);
    destructor Destroy; override;
    //
    function SplitH: TCustomPanel;
    function SplitHV: TCustomPanel;
    function SplitV: TCustomPanel;
    function SplitVH: TCustomPanel;
    function Split(const SplitType: TNEDSplitViewTypeEnum; const DefaultSplitH: Boolean = False): TCustomPanel;
    //
    procedure CloseSplitH;
    procedure CloseSplitHV;
    procedure CloseSplitV;
    procedure CloseSplitVH;
    procedure CloseSplit;
    //
    //property SplitterControlClass: TSplitterControlClass read FSplitterControl write FSplitterControl;
    property MaxSplitCols: Byte read FMaxSplitCols write FMaxSplitCols;
    property MaxSplitRows: Byte read FMaxSplitRows write FMaxSplitRows;
  end;

implementation

uses
  Forms;

//type
//  TWinControlAccess = class(TWinControl);

{ TNEDSplittingManager }

constructor TNEDSplittingManager.Create(const BaseControl: TWinControl);
begin
  Assert(BaseControl <> Nil);
  FSplittingBaseControl := BaseControl;
  //FSplitterControl := TSplitterEx;
  FSplitCols := 0;
  FSplitRows := 0;
  FMaxSplitCols := 3;
  FMaxSplitRows := 3;
  FLastSplitH := False;
  //
  SetLength(FBaseControlColArray, 0);
  SetLength(FBaseControlRowArray, 0);
end;

destructor TNEDSplittingManager.Destroy;
begin
  FSplittingBaseControl := Nil;
  SetLength(FBaseControlColArray, 0);
  SetLength(FBaseControlRowArray, 0);
  inherited;
end;

function TNEDSplittingManager.GetBaseControl(const Horz: Boolean): TWinControl;
begin
  if Horz then begin
    if FSplitRows = 0 then
      Result := FSplittingBaseControl
    else
      Result := FBaseControlRowArray[FSplitRows - 1];
    Exit;
  end;
  //
  if FSplitCols = 0 then
    Result := FSplittingBaseControl
  else
    Result := FBaseControlColArray[FSplitCols - 1];
end;

procedure TNEDSplittingManager.AddCol(const ctrl: TWinControl);
begin
  Inc(FSplitCols);
  SetLength(FBaseControlColArray, FSplitCols);
  FBaseControlColArray[FSplitCols - 1] := ctrl;
end;

procedure TNEDSplittingManager.AddRow(const ctrl: TWinControl);
begin
  Inc(FSplitRows);
  SetLength(FBaseControlRowArray, FSplitRows);
  FBaseControlRowArray[FSplitRows - 1] := ctrl;
end;

function TNEDSplittingManager.CreateBasePanel(const BaseControl: TWinControl; const Align: TAlign): TPanel;
begin
  Result := TPanel.Create(BaseControl);
  Result.Parent := BaseControl;
//  TWinControlAccess(Result).ParentColor := True;
  Result.ParentColor := True;
  Result.BevelEdges := [];
  Result.BevelKind := bkNone;
  Result.BevelInner := bvNone;
  Result.BevelOuter := bvNone;
  Result.BorderStyle := bsNone;
  Result.Align := Align;
  case Result.Align of
    alBottom: Result.Height := BaseControl.Height div 2;
    alRight : Result.Width  := BaseControl.Width  div 2;
  end;
end;

function TNEDSplittingManager.CreateSplitter(const BaseControl, AssignedControl: TWinControl; const Align: TAlign): TSplitterEx;
begin
  //SplitterControl := FSplitterControl.Create(BaseControl);
  Result := TSplitterEx.Create(BaseControl);
  Result.Parent := BaseControl;
  Result.ParentColor := False;
  Result.ParentColor := True;
  Result.Align := Align;
  Result.AssignedControl := AssignedControl;
  Result.AutoSnap := False;
  Result.ResizeStyle := rsUpdate;
  case Result.Align of
    alBottom: Result.Height := 7;
    alRight : Result.Width  := 7;
  end;
end;

function TNEDSplittingManager.SplitH: TCustomPanel;
var
  BaseControl: TWinControl;
  //SplitterControl: TGraphicControl;
  SplitterControl: TSplitterEx;
begin
  if FSplitRows >= FMaxSplitRows then
    Exit(Nil);
  //
  BaseControl := GetBaseControl(True);
  Result := CreateBasePanel(BaseControl, alBottom);
  SplitterControl := CreateSplitter(BaseControl, Result, alBottom);
  Result.Tag := Integer(SplitterControl);
  //
  AddRow(Result);
  //
  SplitterControl.Visible := True;
  Result.Visible := True;
end;

function TNEDSplittingManager.SplitHV: TCustomPanel;
var
  BaseControl: TWinControl;
  //SplitterControl: TGraphicControl;
  SplitterControl: TSplitterEx;
begin
  if FSplitCols >= FMaxSplitCols then
    Exit(Nil);
  //
  if FSplitRows = 0 then
    BaseControl := FSplittingBaseControl
  else
    BaseControl := FBaseControlRowArray[FSplitRows - 1];
  Result := CreateBasePanel(BaseControl, alRight);
  SplitterControl := CreateSplitter(BaseControl, Result, alRight);
  Result.Tag := Integer(SplitterControl);
  //
  AddCol(Result);
  //
  SplitterControl.Visible := True;
  Result.Visible := True;
end;

function TNEDSplittingManager.SplitV: TCustomPanel;
var
  BaseControl: TWinControl;
  //SplitterControl: TGraphicControl;
  SplitterControl: TSplitterEx;
begin
  if FSplitCols >= FMaxSplitCols then
    Exit(Nil);
  //
  BaseControl := GetBaseControl(False);
  Result := CreateBasePanel(BaseControl, alRight);
  SplitterControl := CreateSplitter(BaseControl, Result, alRight);
  Result.Tag := Integer(SplitterControl);
  //
  AddCol(Result);
  //
  SplitterControl.Visible := True;
  Result.Visible := True;
end;

function TNEDSplittingManager.SplitVH: TCustomPanel;
var
  BaseControl: TWinControl;
  //SplitterControl: TGraphicControl;
  SplitterControl: TSplitterEx;
begin
  if FSplitRows >= FMaxSplitRows then
    Exit(Nil);
  //
  if FSplitCols = 0 then
    BaseControl := FSplittingBaseControl
  else
    BaseControl := FBaseControlColArray[FSplitCols - 1];
  Result := CreateBasePanel(BaseControl, alBottom);
  SplitterControl := CreateSplitter(BaseControl, Result, alBottom);
  Result.Tag := Integer(SplitterControl);
  //
  AddRow(Result);
  //
  SplitterControl.Visible := True;
  Result.Visible := True;
end;

function TNEDSplittingManager.Split(const SplitType: TNEDSplitViewTypeEnum; const DefaultSplitH: Boolean): TCustomPanel;
begin
  if SplitType = stSplitH then begin
    Result := SplitH;
    if Result <> Nil then
      FLastSplitH := True;
  end
  else if SplitType = stSplitV then begin
    Result := SplitV;
    if Result <> Nil then
      FLastSplitH := False;
  end
  else if SplitType = stSplitLastOpposite then begin
    if (FSplitCols = 0) and (FSplitRows = 0) then begin
      if DefaultSplitH then begin
        Result := SplitH;
        if Result <> Nil then
          FLastSplitH := True;
      end
      else begin
        Result := SplitV;
        if Result <> Nil then
          FLastSplitH := False;
      end;
    end
    else begin
      if FLastSplitH then begin
        Result := SplitHV;
        if Result <> Nil then
          FLastSplitH := False;
      end
      else begin
        Result := SplitVH;
        if Result <> Nil then
          FLastSplitH := True;
      end;
    end;
  end;
end;

procedure TNEDSplittingManager.CloseSplitH;
var
  BaseControl: TWinControl;
  SplitterControl: TSplitterEx;
begin
  if FSplitRows = 0 then
    Exit;
  //
  BaseControl := FBaseControlRowArray[FSplitRows - 1];
  Dec(FSplitRows);
  SplitterControl := TSplitterEx(Pointer(BaseControl.Tag));
  SplitterControl.Free;
  BaseControl.Free;
end;

procedure TNEDSplittingManager.CloseSplitHV;
var
  BaseControl: TWinControl;
  SplitterControl: TSplitterEx;
begin
  if FSplitCols = 0 then
    Exit;
  //
  BaseControl := FBaseControlColArray[FSplitCols - 1];
  Dec(FSplitCols);
  SplitterControl := TSplitterEx(Pointer(BaseControl.Tag));
  SplitterControl.Free;
  BaseControl.Free;
end;

procedure TNEDSplittingManager.CloseSplitV;
var
  BaseControl: TWinControl;
  SplitterControl: TSplitterEx;
begin
  if FSplitCols = 0 then
    Exit;
  //
  BaseControl := FBaseControlColArray[FSplitCols - 1];
  Dec(FSplitCols);
  SplitterControl := TSplitterEx(Pointer(BaseControl.Tag));
  SplitterControl.Free;
  BaseControl.Free;
end;

procedure TNEDSplittingManager.CloseSplitVH;
var
  BaseControl: TWinControl;
  SplitterControl: TSplitterEx;
begin
  if FSplitRows = 0 then
    Exit;
  //
  BaseControl := FBaseControlRowArray[FSplitRows - 1];
  Dec(FSplitRows);
  SplitterControl := TSplitterEx(Pointer(BaseControl.Tag));
  SplitterControl.Free;
  BaseControl.Free;
end;

procedure TNEDSplittingManager.CloseSplit;
begin
  if FLastSplitH then
    CloseSplitH
  else
    CloseSplitV;
  FLastSplitH := not FLastSplitH;
end;

end.

