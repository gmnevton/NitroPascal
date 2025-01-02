//
// Nitro EDitor
// version 1.0
//
// Splitt Work-space manager
//

unit uSplittingManager;

interface

uses
  SysUtils,
  Classes,
  Controls,
  ExtCtrls,
  SplitEx;

type
  TSplittType = (
    stSplittH,
    stSplittV,
    stSplittLastOpposite
  );

  //TSplitterControlClass = class of TGraphicControl;

  TSplittingManager = class
  private
    FSplittingBaseControl: TWinControl;
    //FSplitterControl: TSplitterControlClass;
    FSplittCols: Byte;
    FSplittRows: Byte;
    FMaxSplittCols: Byte;
    FMaxSplittRows: Byte;
    FLastSplittH: Boolean;
    //
    FBaseControlColArray: Array of TWinControl;
    FBaseControlRowArray: Array of TWinControl;
  protected
    function GetBaseControl(const Horz: Boolean): TWinControl;
    procedure AddCol(const ctrl: TWinControl);
    procedure AddRow(const ctrl: TWinControl);
  public
    constructor Create(const BaseControl: TWinControl);
    destructor Destroy; override;
    //
    function SplittH: TCustomPanel;
    function SplittHV: TCustomPanel;
    function SplittV: TCustomPanel;
    function SplittVH: TCustomPanel;
    function Splitt(const SplittType: TSplittType; const DefaultSplittH: Boolean = False): TCustomPanel;
    //
    procedure CloseSplittH;
    procedure CloseSplittHV;
    procedure CloseSplittV;
    procedure CloseSplittVH;
    procedure CloseSplitt;
    //
    //property SplitterControlClass: TSplitterControlClass read FSplitterControl write FSplitterControl;
    property MaxSplittCols: Byte read FMaxSplittCols write FMaxSplittCols;
    property MaxSplittRows: Byte read FMaxSplittRows write FMaxSplittRows;
  end;

implementation

type
  TWinControlAccess = class(TWinControl);

{ TSplittingManager }

constructor TSplittingManager.Create(const BaseControl: TWinControl);
begin
  Assert(BaseControl <> Nil);
  FSplittingBaseControl := BaseControl;
  //FSplitterControl := TSplitterEx;
  FSplittCols := 0;
  FSplittRows := 0;
  FMaxSplittCols := 3;
  FMaxSplittRows := 3;
  FLastSplittH := False;
  //
  SetLength(FBaseControlColArray, 0);
  SetLength(FBaseControlRowArray, 0);
end;

destructor TSplittingManager.Destroy;
begin
  FSplittingBaseControl := Nil;
  SetLength(FBaseControlColArray, 0);
  SetLength(FBaseControlRowArray, 0);
  inherited;
end;

function TSplittingManager.GetBaseControl(const Horz: Boolean): TWinControl;
begin
  if Horz then begin
    if FSplittRows = 0 then
      Result := FSplittingBaseControl
    else
      Result := FBaseControlRowArray[FSplittRows - 1];
    Exit;
  end;
  //
  if FSplittCols = 0 then
    Result := FSplittingBaseControl
  else
    Result := FBaseControlColArray[FSplittCols - 1];
end;

procedure TSplittingManager.AddCol(const ctrl: TWinControl);
begin
  Inc(FSplittCols);
  SetLength(FBaseControlColArray, FSplittCols);
  FBaseControlColArray[FSplittCols - 1] := ctrl;
end;

procedure TSplittingManager.AddRow(const ctrl: TWinControl);
begin
  Inc(FSplittRows);
  SetLength(FBaseControlRowArray, FSplittRows);
  FBaseControlRowArray[FSplittRows - 1] := ctrl;
end;

function TSplittingManager.SplittH: TCustomPanel;
var
  BaseControl: TWinControl;
  //SplitterControl: TGraphicControl;
  SplitterControl: TSplitterEx;
begin
  if FSplittRows >= FMaxSplittRows then
    Exit(Nil);
  //
  BaseControl := GetBaseControl(True);
  Result := TPanel.Create(BaseControl);
  Result.Parent := BaseControl;
  TWinControlAccess(Result).ParentColor := True;
  Result.Align := alBottom;
  Result.Height := BaseControl.Height div 2;

  //SplitterControl := FSplitterControl.Create(BaseControl);
  SplitterControl := TSplitterEx.Create(BaseControl);
  SplitterControl.Parent := BaseControl;
  SplitterControl.ParentColor := False;
  SplitterControl.ParentColor := True;
  SplitterControl.Align := alBottom;
  SplitterControl.AssignedControl := Result;
  SplitterControl.AutoSnap := False;
  SplitterControl.ResizeStyle := rsUpdate;
  SplitterControl.Height := 7;

  Result.Tag := Integer(SplitterControl);
  //
  AddRow(Result);
  //
  SplitterControl.Visible := True;
  Result.Visible := True;
end;

function TSplittingManager.SplittHV: TCustomPanel;
var
  BaseControl: TWinControl;
  //SplitterControl: TGraphicControl;
  SplitterControl: TSplitterEx;
begin
  if FSplittCols >= FMaxSplittCols then
    Exit(Nil);
  //
  if FSplittRows = 0 then
    BaseControl := FSplittingBaseControl
  else
    BaseControl := FBaseControlRowArray[FSplittRows - 1];
  Result := TPanel.Create(BaseControl);
  Result.Parent := BaseControl;
  TWinControlAccess(Result).ParentColor := True;
  Result.Align := alRight;
  Result.Width := BaseControl.Width div 2;

  //SplitterControl := FSplitterControl.Create(BaseControl);
  SplitterControl := TSplitterEx.Create(BaseControl);
  SplitterControl.Parent := BaseControl;
  SplitterControl.ParentColor := False;
  SplitterControl.ParentColor := True;
  SplitterControl.Align := alRight;
  SplitterControl.AssignedControl := Result;
  SplitterControl.AutoSnap := False;
  SplitterControl.ResizeStyle := rsUpdate;
  SplitterControl.Width := 7;

  Result.Tag := Integer(SplitterControl);
  //
  AddCol(Result);
  //
  SplitterControl.Visible := True;
  Result.Visible := True;
end;

function TSplittingManager.SplittV: TCustomPanel;
var
  BaseControl: TWinControl;
  //SplitterControl: TGraphicControl;
  SplitterControl: TSplitterEx;
begin
  if FSplittCols >= FMaxSplittCols then
    Exit(Nil);
  //
  BaseControl := GetBaseControl(False);
  Result := TPanel.Create(BaseControl);
  Result.Parent := BaseControl;
  TWinControlAccess(Result).ParentColor := True;
  Result.Align := alRight;
  Result.Width := BaseControl.Width div 2;

  //SplitterControl := FSplitterControl.Create(BaseControl);
  SplitterControl := TSplitterEx.Create(BaseControl);
  SplitterControl.Parent := BaseControl;
  SplitterControl.ParentColor := False;
  SplitterControl.ParentColor := True;
  SplitterControl.Align := alRight;
  SplitterControl.AssignedControl := Result;
  SplitterControl.AutoSnap := False;
  SplitterControl.ResizeStyle := rsUpdate;
  SplitterControl.Width := 7;

  Result.Tag := Integer(SplitterControl);
  //
  AddCol(Result);
  //
  SplitterControl.Visible := True;
  Result.Visible := True;
end;

function TSplittingManager.SplittVH: TCustomPanel;
var
  BaseControl: TWinControl;
  //SplitterControl: TGraphicControl;
  SplitterControl: TSplitterEx;
begin
  if FSplittRows >= FMaxSplittRows then
    Exit(Nil);
  //
  if FSplittCols = 0 then
    BaseControl := FSplittingBaseControl
  else
    BaseControl := FBaseControlColArray[FSplittCols - 1];
  Result := TPanel.Create(BaseControl);
  Result.Parent := BaseControl;
  TWinControlAccess(Result).ParentColor := True;
  Result.Align := alBottom;
  Result.Height := BaseControl.Height div 2;

  //SplitterControl := FSplitterControl.Create(BaseControl);
  SplitterControl := TSplitterEx.Create(BaseControl);
  SplitterControl.Parent := BaseControl;
  SplitterControl.ParentColor := False;
  SplitterControl.ParentColor := True;
  SplitterControl.Align := alBottom;
  SplitterControl.AssignedControl := Result;
  SplitterControl.AutoSnap := False;
  SplitterControl.ResizeStyle := rsUpdate;
  SplitterControl.Height := 7;

  Result.Tag := Integer(SplitterControl);
  //
  AddRow(Result);
  //
  SplitterControl.Visible := True;
  Result.Visible := True;
end;

function TSplittingManager.Splitt(const SplittType: TSplittType; const DefaultSplittH: Boolean): TCustomPanel;
begin
  if SplittType = stSplittH then begin
    Result := SplittH;
    if Result <> Nil then
      FLastSplittH := True;
  end
  else if SplittType = stSplittV then begin
    Result := SplittV;
    if Result <> Nil then
      FLastSplittH := False;
  end
  else if SplittType = stSplittLastOpposite then begin
    if (FSplittCols = 0) and (FSplittRows = 0) then begin
      if DefaultSplittH then begin
        Result := SplittH;
        if Result <> Nil then
          FLastSplittH := True;
      end
      else begin
        Result := SplittV;
        if Result <> Nil then
          FLastSplittH := False;
      end;
    end
    else begin
      if FLastSplittH then begin
        Result := SplittHV;
        if Result <> Nil then
          FLastSplittH := False;
      end
      else begin
        Result := SplittVH;
        if Result <> Nil then
          FLastSplittH := True;
      end;
    end;
  end;
end;

procedure TSplittingManager.CloseSplittH;
var
  BaseControl: TWinControl;
  SplitterControl: TSplitterEx;
begin
  if FSplittRows = 0 then
    Exit;
  //
  BaseControl := FBaseControlRowArray[FSplittRows - 1];
  Dec(FSplittRows);
  SplitterControl := TSplitterEx(Pointer(BaseControl.Tag));
  SplitterControl.Free;
  BaseControl.Free;
end;

procedure TSplittingManager.CloseSplittHV;
var
  BaseControl: TWinControl;
  SplitterControl: TSplitterEx;
begin
  if FSplittCols = 0 then
    Exit;
  //
  BaseControl := FBaseControlColArray[FSplittCols - 1];
  Dec(FSplittCols);
  SplitterControl := TSplitterEx(Pointer(BaseControl.Tag));
  SplitterControl.Free;
  BaseControl.Free;
end;

procedure TSplittingManager.CloseSplittV;
var
  BaseControl: TWinControl;
  SplitterControl: TSplitterEx;
begin
  if FSplittCols = 0 then
    Exit;
  //
  BaseControl := FBaseControlColArray[FSplittCols - 1];
  Dec(FSplittCols);
  SplitterControl := TSplitterEx(Pointer(BaseControl.Tag));
  SplitterControl.Free;
  BaseControl.Free;
end;

procedure TSplittingManager.CloseSplittVH;
var
  BaseControl: TWinControl;
  SplitterControl: TSplitterEx;
begin
  if FSplittRows = 0 then
    Exit;
  //
  BaseControl := FBaseControlRowArray[FSplittRows - 1];
  Dec(FSplittRows);
  SplitterControl := TSplitterEx(Pointer(BaseControl.Tag));
  SplitterControl.Free;
  BaseControl.Free;
end;

procedure TSplittingManager.CloseSplitt;
begin
  if FLastSplittH then
    CloseSplittH
  else
    CloseSplittV;
  FLastSplittH := not FLastSplittH;
end;

end.

