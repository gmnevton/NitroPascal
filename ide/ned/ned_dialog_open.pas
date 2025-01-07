unit ned_dialog_open;

interface

uses
  SysUtils,
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
  SplitEx;

type
  TNEDDialogOpen = class(TFrame)
    UTitleBar1: TUTitleBar;
    btnClose: TUQuickButton;
    UPanel1: TUPanel;
    UPanel2: TUPanel;
    UPanel3: TUPanel;
    UPanel4: TUPanel;
    btnCancel: TUButton;
    btnOpen: TUButton;
    edtPath: TUEdit;
    txtPath: TUText;
    UQuickButton2: TUQuickButton;
    SplitterEx1: TSplitterEx;
    procedure btnCloseClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
  private
    FCanClose: Boolean;
    FExecutionResult: TModalResult;
    //
    FMainForm: TUForm;
    FOldMainFormResizeEvent: TNotifyEvent;
    //
    procedure MainFormResize(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //
    function Execute(const DefaultPath: String = ''; const DefaultFileExt: String = '.*'): Boolean;
  end;

implementation

{$R *.dfm}

uses
  UCL.FormOverlay;

constructor TNEDDialogOpen.Create(AOwner: TComponent);
begin
  inherited;
  FCanClose := False;
  FExecutionResult := mrNone;
end;

destructor TNEDDialogOpen.Destroy;
begin
  FCanClose := False;
  FExecutionResult := mrNone;
  inherited;
end;

procedure TNEDDialogOpen.MainFormResize(Sender: TObject);
begin
  Self.Margins.Top := Round(FMainForm.Height * 0.2);
  Self.Margins.Left := (FMainForm.Width - 600) div 2;
  Self.Margins.Right := Self.Margins.Left;
  Self.Margins.Bottom := Self.Margins.Top;
  //
  if Assigned(FOldMainFormResizeEvent) then
    FOldMainFormResizeEvent(Sender);
end;

function TNEDDialogOpen.Execute(const DefaultPath, DefaultFileExt: String): Boolean;
begin
  Result := False;
  FMainForm := TUForm(Application.MainForm);
  FOldMainFormResizeEvent := FMainForm.OnResize;
  try
    FMainForm.OnResize := MainFormResize;
    Self.Parent := FMainForm;
    //Self.Top := 200;
    //Self.Left := (main.Width - Self.Width) div 2;
    Self.Margins.Top := Round(FMainForm.Height * 0.2);
    Self.Margins.Left := (FMainForm.Width - 600) div 2;
    Self.Margins.Right := Self.Margins.Left;
    Self.Margins.Bottom := Self.Margins.Top;
    Self.AlignWithMargins := True;
    Self.Align := alClient;
    //
    FMainForm.OverlayType := otTransparent;
    try
      Self.Show;
      Self.BringToFront;
      //
      // block execution at this point
      FCanClose := False;
      repeat
        Application.HandleMessage;
      until FCanClose;
    finally
      FMainForm.OverlayType := otNone;
      Result := FExecutionResult = mrOk;
    end;
  finally
    FMainForm.OnResize := FOldMainFormResizeEvent;
  end;
end;

procedure TNEDDialogOpen.btnCloseClick(Sender: TObject);
begin
  FExecutionResult := mrClose;
  FCanClose := True;
end;

procedure TNEDDialogOpen.btnCancelClick(Sender: TObject);
begin
  FExecutionResult := mrCancel;
  FCanClose := True;
end;


procedure TNEDDialogOpen.btnOpenClick(Sender: TObject);
begin
  FExecutionResult := mrOk;
  FCanClose := True;
end;

end.

