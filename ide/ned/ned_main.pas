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
  UCL.FormOverlay,
  UCL.ThemeManager,
  UCL.Types,
  UCL.Panel,
  UCL.CaptionBar,
  UCL.ScrollBox,
  UCL.ItemButton,
  UCL.SymbolButton,
  UCL.QuickButton,
  UCL.Separator,
  UCL.Text,
  UCL.Slider,
  UCL.ProgressBar,
  UCL.HyperLink,
  UCL.TitleBar,
  SynEditHighlighter,
  SynHighlighterGeneral,
  SynEdit,
  VirtualTrees,
  SplitEx;

type
  TNEDMainForm = class(TUForm)
    barCaption: TUCaptionBar;
    pnlShortCuts: TUPanel;
    pnlStatus: TUPanel;
    pnlWorkSpace: TUPanel;
    btnClose: TUQuickButton;
    btnMax: TUQuickButton;
    btnMin: TUQuickButton;
    mnuMain: TMainMenu;
    File1: TMenuItem;
    btnShowHideToolbox: TUQuickButton;
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
    pnlLeft: TUPanel;
    boxProject: TUScrollBox;
    UPanel5: TUPanel;
    btnDebugRun: TUQuickButton;
    btnDebugPause: TUQuickButton;
    btnDebugStop: TUQuickButton;
    USeparator1: TUSeparator;
    btnDebugStepOver: TUQuickButton;
    btnDebugStepInto: TUQuickButton;
    UPanel7: TUPanel;
    btnHome: TUQuickButton;
    btnProject: TUQuickButton;
    btnSearch: TUQuickButton;
    USeparator2: TUSeparator;
    txtFilePath: TUText;
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
    barProject: TUTitleBar;
    vstProject: TVirtualStringTree;
    boxSearch: TUScrollBox;
    barSearch: TUTitleBar;
    vstSearch: TVirtualStringTree;
    splLeft: TSplitterEx;
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
    procedure btnShowHideToolboxClick(Sender: TObject);
    procedure btnHomeClick(Sender: TObject);
  private
  public
  end;

var
  NEDMainForm: TNEDMainForm;

implementation

{$R *.dfm}

uses
  ned_home_page,
  ned_source_editor,
  ned_dialog_open;

var
  NEDHomeForm: TNEDHomeForm;
//  NEDEditorForm: TNEDEditorForm;

procedure TNEDMainForm.FormCreate(Sender: TObject);
begin
  CaptionBar := barCaption;
  //
  NEDHomeForm := TNEDHomeForm.Create(Self);
  NEDHomeForm.Parent := pnlWorkSpace;
  NEDHomeForm.Align := alClient;
  //
  txtFilePath.Caption := '---';
  txtFileEncoding.Caption := '---';
  txtFileLineBreaks.Caption := '---';
  txtFileEditMode.Caption := '---';
  txtFileEditPosition.Caption := '---';
  //
  pnlLeft.Visible := False;
  splLeft.Visible := False;
  boxProject.BringToFront;
  //
end;

procedure TNEDMainForm.FormDestroy(Sender: TObject);
begin
  NEDHomeForm.Free;
end;

procedure TNEDMainForm.FormShow(Sender: TObject);
begin
  NEDHomeForm.Show;
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

procedure TNEDMainForm.btnShowHideToolboxClick(Sender: TObject);
begin
  if pnlLeft.Visible then begin
    pnlLeft.Visible := False;
    splLeft.Visible := False;
  end
  else begin
    splLeft.Visible := True;
    pnlLeft.Visible := True;
  end;
end;

procedure TNEDMainForm.btnHomeClick(Sender: TObject);
var
  open: TNEDDialogOpen;
begin
  open := TNEDDialogOpen.Create(Self);
  try
    if open.Execute() then begin

    end;
  finally
    open.Free;
  end;
end;

end.

