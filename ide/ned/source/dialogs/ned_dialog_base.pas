//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-07
// All rights reserved.
//

unit ned_dialog_base;

interface

uses
  SysUtils,
  Windows,
  Classes,
  Controls,
  StdCtrls,
  ExtCtrls,
  Forms,
  UCL.Form,
  UCL.ThemeManager,
  UCL.QuickButton,
  UCL.Classes,
  UCL.TitleBar,
  UCL.Edit,
  UCL.Button,
  UCL.Panel,
  UCL.Text,
  SplitEx,
  uFolders,
  ImageList,
  ImgList,
  ShlObj,
  ShellAPI;

type
  TNEDDialogBase = class(TUForm)
    UTitleBar1: TUTitleBar;
    btnClose: TUQuickButton;
    ImageList1: TImageList;
    UPanel3: TUPanel;
    btnCancel: TUButton;
    btnOK: TUButton;
    //
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    //
    procedure btnCloseClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FCanClose: Boolean;
    FExecutionResult: TModalResult;
    //
    FMainForm: TUForm;
    FOldMainFormResizeEvent: TNotifyEvent;
    //
    procedure MainFormResize(Sender: TObject);
  protected
    function CanDrawBorder: Boolean; override;
    procedure DoDrawBorder; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //
    function Execute: Boolean; dynamic;
  end;

  TNEDDialogBaseClass = class of TNEDDialogBase;

implementation

{$R *.dfm}

uses
  Types,
  Graphics,
  ActiveX,
  StrUtils,
  IOUtils,
  UCL.FormOverlay;

//type
//  TWinControlAccess = class(TWinControl);

{ TNEDDialogBase }

constructor TNEDDialogBase.Create(AOwner: TComponent);
begin
  inherited;
  FFormState := FFormState - [fsVisible];
  FCanClose := False;
  FExecutionResult := mrNone;
end;

destructor TNEDDialogBase.Destroy;
begin
  FCanClose := False;
  FExecutionResult := mrNone;
  inherited;
end;

function TNEDDialogBase.CanDrawBorder: Boolean;
begin
  Result := True;
end;

procedure TNEDDialogBase.DoDrawBorder;
begin
  UpdateBorderColor;
  Canvas.Pen.Color := BorderColor;
  Canvas.MoveTo(0, 1);
  Canvas.LineTo(0, Height - 1);  // left
  Canvas.LineTo(Width - 1, Height - 1); // bottom
  Canvas.LineTo(Width - 1, 0); // right
end;

procedure TNEDDialogBase.FormCreate(Sender: TObject);
begin
  FFormState := FFormState - [fsVisible];
end;

procedure TNEDDialogBase.FormResize(Sender: TObject);
begin
  btnCancel.Width := Self.Width div 2;
end;

procedure TNEDDialogBase.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then begin
    Key := 0; // prevent default handling
    FExecutionResult := mrCancel;
    FCanClose := True;
  end;
end;

//var
//  FocusedControl: TWinControl;
//  NextControl: TWinControl;
//begin
//  if Key = VK_TAB then begin
//    FocusedControl := ActiveControl;
//    if FocusedControl <> Nil then begin
//      NextControl := TWinControlAccess(Self).FindNextControl(FocusedControl, not (ssShift in Shift), True, True);
//
//      if Assigned(NextControl) then
//        NextControl.SetFocus;
//
//      Key := 0; // prevent default handling
//    end;
//  end;
//end;

procedure TNEDDialogBase.MainFormResize(Sender: TObject);
begin
//  Self.Margins.Top := Round(FMainForm.Height * 0.2);
//  Self.Margins.Left := (FMainForm.Width - 600) div 2;
//  Self.Margins.Right := Self.Margins.Left;
//  Self.Margins.Bottom := Self.Margins.Top;
  //
  if Assigned(FOldMainFormResizeEvent) then
    FOldMainFormResizeEvent(Sender);
end;

function TNEDDialogBase.Execute: Boolean;
var
  t, l , w, h: Integer;
begin
  Result := False;
  FMainForm := TUForm(Application.MainForm);

  Self.Hide;
  Application.ProcessMessages;
  FOldMainFormResizeEvent := FMainForm.OnResize;
  try
    // center on main form
    FMainForm.OnResize := MainFormResize;

    FMainForm.DisableAlign;
    try
      Self.Parent := FMainForm;

      t := Round(FMainForm.Height * 0.2);
      l := Round(FMainForm.Width * 0.6) div 2;
      w := FMainForm.Width - Self.Left * 2;
      h := FMainForm.Height - Self.Top * 2;

      Self.SetBounds(l, t, w, h);

      Self.Align := alCustom;
      //
      // @TODO: make TitleBar background color setable by user
      UTitleBar1.BackColors.DarkColor := SelectAccentColor(GetCommonThemeManager, $00404040);
      UTitleBar1.BackColors.LightColor := SelectAccentColor(GetCommonThemeManager, $00404040);
      //
      FMainForm.OverlayType := otTransparent;
      //
      Application.ProcessMessages;
    finally
      FMainForm.EnableAlign;
      Application.ProcessMessages;
    end;
    //
    // it would be good to show FolderView first entry as expanded - done
    //
    try
      Self.Show;
      Self.BringToFront;
      Self.Invalidate;
      //Self.SetFocus;
      //
      // block execution at this point
      FCanClose := False;
      repeat
        Application.HandleMessage;
      until FCanClose or Application.Terminated;
    finally
      Self.Hide;
      FMainForm.OverlayType := otNone;
      Application.ProcessMessages;
      Result := FExecutionResult = mrOk;
    end;
  finally
    FMainForm.OnResize := FOldMainFormResizeEvent;
  end;
end;

procedure TNEDDialogBase.btnCloseClick(Sender: TObject);
begin
  FExecutionResult := mrClose;
  FCanClose := True;
end;

procedure TNEDDialogBase.btnCancelClick(Sender: TObject);
begin
  FExecutionResult := mrCancel;
  FCanClose := True;
end;

procedure TNEDDialogBase.btnOKClick(Sender: TObject);
begin
  FExecutionResult := mrOk;
  FCanClose := True;
end;

end.

