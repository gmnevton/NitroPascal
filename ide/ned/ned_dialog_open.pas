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
  UCL.Text;

type
  TNEDDialogOpen = class(TFrame)
    UTitleBar1: TUTitleBar;
    UQuickButton1: TUQuickButton;
    UPanel1: TUPanel;
    UPanel2: TUPanel;
    UPanel3: TUPanel;
    UPanel4: TUPanel;
    UButton1: TUButton;
    UButton2: TUButton;
    UEdit1: TUEdit;
    UText1: TUText;
    UQuickButton2: TUQuickButton;
    procedure UQuickButton1Click(Sender: TObject);
  private
  public
  end;

implementation

{$R *.dfm}

uses
  UCL.FormOverlay;

procedure TNEDDialogOpen.UQuickButton1Click(Sender: TObject);
begin
  TUForm(Application.MainForm).OverlayType := otNone;
  Self.Free;
end;

end.
