//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
//

unit ned_main;

interface

uses
  SysUtils,
  Classes,
  Controls,
  StdCtrls,
  ExtCtrls,
  Forms,
  Menus,
  UCL.Form,
  UCL.ThemeManager,
  UCL.Types,
  UCL.Panel,
  UCL.CaptionBar,
  UCL.ScrollBox,
  UCL.ItemButton,
  UCL.SymbolButton,
  UCL.QuickButton,
  UCL.Separator,
  UCL.PopupMenu,
  UCL.Text,
  UCL.Slider,
  SynEditHighlighter,
  SynHighlighterGeneral,
  SynEdit, UCL.ProgressBar, UCL.HyperLink;

type
  TNEDMainForm = class(TUForm)
    UCaptionBar1: TUCaptionBar;
    UPanel1: TUPanel;
    UPanel2: TUPanel;
    UPanel3: TUPanel;
    btnClose: TUQuickButton;
    btnMax: TUQuickButton;
    btnMin: TUQuickButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    UQuickButton6: TUQuickButton;
    New1: TMenuItem;
    Open1: TMenuItem;
    History1: TMenuItem;
    N1: TMenuItem;
    Save1: TMenuItem;
    Saveas1: TMenuItem;
    Saveall1: TMenuItem;
    N2: TMenuItem;
    Close1: TMenuItem;
    Closeall1: TMenuItem;
    N3: TMenuItem;
    Exit1: TMenuItem;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    N4: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Search1: TMenuItem;
    Find1: TMenuItem;
    Findfile1: TMenuItem;
    Replace1: TMenuItem;
    N5: TMenuItem;
    Repeatsearch1: TMenuItem;
    Project1: TMenuItem;
    Run1: TMenuItem;
    Debug1: TMenuItem;
    ools1: TMenuItem;
    Window1: TMenuItem;
    Help1: TMenuItem;
    empty1: TMenuItem;
    Resources1: TMenuItem;
    Options1: TMenuItem;
    Run2: TMenuItem;
    Runwithoutdebuging1: TMenuItem;
    Parameters1: TMenuItem;
    N6: TMenuItem;
    Stepover1: TMenuItem;
    Stepinto1: TMenuItem;
    N7: TMenuItem;
    erminate1: TMenuItem;
    Inspect1: TMenuItem;
    Evaluate1: TMenuItem;
    Addwatch1: TMenuItem;
    Addbreakpoint1: TMenuItem;
    Options2: TMenuItem;
    Nextwindow1: TMenuItem;
    empty2: TMenuItem;
    N8: TMenuItem;
    AboutNED1: TMenuItem;
    NEDprojectwebsite1: TMenuItem;
    NitroPascalwebsite1: TMenuItem;
    N9: TMenuItem;
    UPanel6: TUPanel;
    UScrollBox2: TUScrollBox;
    UItemButton1: TUItemButton;
    UItemButton2: TUItemButton;
    UItemButton3: TUItemButton;
    UItemButton4: TUItemButton;
    UItemButton5: TUItemButton;
    UItemButton6: TUItemButton;
    UItemButton7: TUItemButton;
    UItemButton8: TUItemButton;
    UItemButton9: TUItemButton;
    UPanel5: TUPanel;
    UQuickButton1: TUQuickButton;
    UQuickButton2: TUQuickButton;
    UQuickButton3: TUQuickButton;
    USeparator1: TUSeparator;
    UQuickButton4: TUQuickButton;
    UQuickButton5: TUQuickButton;
    UPanel7: TUPanel;
    UQuickButton7: TUQuickButton;
    UQuickButton8: TUQuickButton;
    UQuickButton9: TUQuickButton;
    USeparator2: TUSeparator;
    txtFilePath: TUText;
    UPopupMenu1: TUPopupMenu;
    sliFileZoom: TUSlider;
    USeparator3: TUSeparator;
    btnFileZoomIn: TUQuickButton;
    btnFileZoomOut: TUQuickButton;
    txtFileEncoding: TUText;
    USeparator4: TUSeparator;
    txtFileLineBreaks: TUText;
    USeparator5: TUSeparator;
    txtFileEditMode: TUText;
    USeparator6: TUSeparator;
    txtFileEditPosition: TUText;
    barStatus: TUProgressBar;
    sepStatus: TUSeparator;
    txtStatus: TUText;
    UHyperLink1: TUHyperLink;
    //
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
  private
  public
  end;

var
  NEDMainForm: TNEDMainForm;

implementation

{$R *.dfm}

procedure TNEDMainForm.FormCreate(Sender: TObject);
begin
  CaptionBar := UCaptionBar1;
  //
  txtFilePath.Caption := '---';
  txtFileEncoding.Caption := '---';
  txtFileLineBreaks.Caption := '---';
  txtFileEditMode.Caption := '---';
  txtFileEditPosition.Caption := '---';
end;

procedure TNEDMainForm.FormDestroy(Sender: TObject);
begin
//
end;

procedure TNEDMainForm.FormShow(Sender: TObject);
begin
//
end;

procedure TNEDMainForm.FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
//
end;

procedure TNEDMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
//
end;

procedure TNEDMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
//
end;

procedure TNEDMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
//
end;

procedure TNEDMainForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
//
end;

procedure TNEDMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
//
end;

procedure TNEDMainForm.FormResize(Sender: TObject);
begin
//
end;

end.

