//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-07
// All rights reserved.
//

unit ned_main;

interface

uses
  SysUtils,
  Messages,
  Classes,
  Types,
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
  SplitEx,
  ned_dialog_base,
  ned_editor_view,
  ned_source_view,
  ned_profiles;

const
  WM_AFTERSHOW = $BF00; // max $BFFF

type
  TNEDToolboxTypeEnum = (
    ttWorkspace,
    ttSearch
  );

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
    btnSelectProfile: TUQuickButton;
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
    pnlToolbox: TUPanel;
    boxWorkspace: TUScrollBox;
    UPanel5: TUPanel;
    btnDebugRun: TUQuickButton;
    btnDebugPause: TUQuickButton;
    btnDebugStop: TUQuickButton;
    USeparator1: TUSeparator;
    btnDebugStepOver: TUQuickButton;
    btnDebugStepInto: TUQuickButton;
    UPanel7: TUPanel;
    btnHome: TUQuickButton;
    btnWorkspace: TUQuickButton;
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
    barWorkspace: TUTitleBar;
    vstWorkspace: TVirtualStringTree;
    boxSearch: TUScrollBox;
    barSearch: TUTitleBar;
    vstSearch: TVirtualStringTree;
    splLeft: TSplitterEx;
    txtFileType: TUText;
    USeparator7: TUSeparator;
    View1: TMenuItem;
    Spliteditortoright1: TMenuItem;
    Spliteditortobottom1: TMenuItem;
    Openandsplittoright1: TMenuItem;
    Openandsplittobottom1: TMenuItem;
    txtProfile: TUText;
    USeparator8: TUSeparator;
    //
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormAlignPosition(Sender: TWinControl; Control: TControl; var NewLeft, NewTop, NewWidth, NewHeight: Integer; var AlignRect: TRect; AlignInfo: TAlignInfo);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMove(Sender: TObject);
    procedure FormResize(Sender: TObject);
    //
    procedure btnSelectProfileClick(Sender: TObject);
    procedure btnHomeClick(Sender: TObject);
    procedure btnWorkspaceClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    //
    procedure New1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Saveas1Click(Sender: TObject);
    procedure Saveall1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure Closeall1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure Redo1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure Findfile1Click(Sender: TObject);
    procedure Replace1Click(Sender: TObject);
    procedure Repeatsearch1Click(Sender: TObject);
    procedure Spliteditortoright1Click(Sender: TObject);
    procedure Spliteditortobottom1Click(Sender: TObject);
    procedure NEDprojectwebsite1Click(Sender: TObject);
    procedure NitroPascalwebsite1Click(Sender: TObject);
    procedure AboutNED1Click(Sender: TObject);
  private
    FModalForm: TForm;
    FProfile: TNEDProfile;
    FLastOpenedPath: String;
    FActiveToolbox: TNEDToolboxTypeEnum;
    //
    //
    procedure WMAfterShow(var Msg: TMessage); message WM_AFTERSHOW;
    procedure CMDialogKey(var Msg: TCMDialogKey); message CM_DIALOGKEY; // grab TAB key before delphi can still it and switch it off
    procedure NEDEditorInfoDetails(var Msg: TMessage); message CM_NED_EDITORINFO_DETAILS;
    procedure NEDWindowSwitch(var Msg: TMessage); message CM_NED_WINDOW_SWITCH;
    //
    //
    procedure RestoreWindowPlacement;
    procedure ShowHideToolBox(const AToolbox: TNEDToolboxTypeEnum);
  private
    NEDViewForm: TNEDViewForm;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
  public
    function CreateModalDialogForm(const AFormClass: TNEDDialogBaseClass): TNEDDialogBase;
    function DestroyModalDialogForm(var ADialogForm: TNEDDialogBase): Boolean;
  end;

var
  NEDMainForm: TNEDMainForm;

implementation

{$R *.dfm}

uses
  Windows,
  Dialogs,
  ned_home_page,
  ned_dialog_profiles,
  ned_source_editor,
  ned_dialog_open,
  ned_splitview_manager,
  ned_config;

type
  TWinControlAccess = class(TWinControl);

var
  NEDHomeForm: TNEDHomeForm;
//  NEDDialogProfiles: TNEDDialogProfiles;
//  NEDEditorForm: TNEDEditorForm;

procedure TNEDMainForm.FormCreate(Sender: TObject);
begin
  CaptionBar := barCaption;
  RestoreWindowPlacement;
  OnMove := FormMove;
  //
  NEDHomeForm := TNEDHomeForm.Create(Self);
  NEDHomeForm.Parent := pnlWorkSpace;
  NEDHomeForm.Align := alClient;
  //
  txtProfile.Caption := '---';
  txtFilePath.Caption := '---';
  txtFileType.Caption := '---';
  txtFileEncoding.Caption := '---';
  txtFileLineBreaks.Caption := '---';
  txtFileEditMode.Caption := '---';
  txtFileEditPosition.Caption := '---';
  txtStatus.Caption := '';
  //
  pnlToolbox.Visible := False;
  splLeft.Visible := False;
  //
  FModalForm := Nil;
  FProfile := Nil;
  FLastOpenedPath := '';
  boxWorkspace.BringToFront;
  FActiveToolbox := ttWorkspace;
  //
  if not NEDConfig.Loaded and NEDConfig.Error then
    txtStatus.Caption := NEDConfig.ErrorMsg;
end;

procedure TNEDMainForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  // allow navigation keys to be processed by the form
  Params.ExStyle := Params.ExStyle or WS_EX_CONTROLPARENT;
end;

procedure TNEDMainForm.FormDestroy(Sender: TObject);
begin
//  NEDDialogProfiles.Free;
  NEDHomeForm.Free;
  FProfile := Nil;
end;

procedure TNEDMainForm.FormShow(Sender: TObject);
begin
  if SameText(NEDConfig.ColorSchema, 'system') then begin
    GetCommonThemeManager.Theme := ttSystem;
  end
  else if SameText(NEDConfig.ColorSchema, 'dark') then begin
    GetCommonThemeManager.Theme := ttDark;
  end
  else if SameText(NEDConfig.ColorSchema, 'light') then begin
    GetCommonThemeManager.Theme := ttLight;
  end;
  if NEDConfig.ShowHomePage then
    NEDHomeForm.Show;
  if NEDConfig.Maximize then
    Self.WindowState := wsMaximized;
  if NEDConfig.ShowProfileSelection then
    PostMessage(Self.Handle, WM_AFTERSHOW, 0, 0);
end;

procedure TNEDMainForm.FormAlignPosition(Sender: TWinControl; Control: TControl; var NewLeft, NewTop, NewWidth, NewHeight: Integer; var AlignRect: TRect; AlignInfo: TAlignInfo);
begin
  if Control = FModalForm then begin
    NewTop := Round(Self.Height * 0.2);
    NewLeft := Round(Self.Width * 0.6) div 2;
    NewWidth := Self.Width - NewLeft * 2;
    NewHeight := Self.Height - NewTop * 2;
  end;
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
var
  FocusedControl: TWinControl;
  NextControl: TWinControl;
begin
  if Key = VK_TAB then begin
    FocusedControl := ActiveControl;
    if (FocusedControl <> Nil) and (FModalForm <> Nil) then begin
      NextControl := TWinControlAccess(FModalForm).FindNextControl(FocusedControl, not (ssShift in Shift), True, False);

      if Assigned(NextControl) and ((NextControl = FModalForm) or (GetParentForm(NextControl, False) = FModalForm)) then
        NextControl.SetFocus;
    end;
    Key := 0; // prevent default handling
  end;
end;

procedure TNEDMainForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
//
end;

procedure TNEDMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
//
end;

procedure TNEDMainForm.FormMove(Sender: TObject);
var
  _monitor: TMonitor;
  x_pos, y_pos: Integer;
begin
  _monitor := Screen.MonitorFromWindow(Self. Handle);
  if (_monitor <> Nil) and (_monitor.Handle <> Screen.PrimaryMonitor.Handle) then begin
    NEDConfig.Monitor := _monitor.Handle;
    x_pos := _monitor.WorkareaRect.Left;
    y_pos := _monitor.WorkareaRect.Top;
    x_pos := Self.Left - x_pos;
    y_pos := Self.Top - y_pos;
    NEDConfig.Position := TPoint.Create(x_pos, y_pos);
  end
  else begin
    NEDConfig.Monitor := 0;
    NEDConfig.Position := TPoint.Create(Self.Left, Self.Top);
  end;
  NEDConfig.Size := TSize.Create(Self.Width, Self.Height);
  NEDConfig.SaveConfig;
end;

procedure TNEDMainForm.FormResize(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;
  //
  NEDConfig.Size := TSize.Create(Self.Width, Self.Height);
  NEDConfig.SaveConfig;
end;

procedure TNEDMainForm.WMAfterShow(var Msg: TMessage);
begin
  Msg.Result := 1;
  btnSelectProfileClick(Nil);
end;

//procedure TNEDMainForm.CMDialogKey(var Msg: TCMDialogKey);
//  if GetKeyState(VK_MENU) >= 0 then begin // ALT
//    case Msg.CharCode of
//      VK_TAB: begin
//        if GetKeyState(VK_CONTROL) >= 0 then begin
//          ShowMessage('MAIN CTRL+TAB');
//          SelectNext(FActiveControl, GetKeyState(VK_SHIFT) >= 0, True);
//          Result := 1;
//          Exit;
//        end;
//        Exit;
//      end;
//    end;
//  end;
//  inherited;
//end;
procedure TNEDMainForm.CMDialogKey(var Msg: TCMDialogKey);
begin
  if (Msg.CharCode = VK_TAB) and (FModalForm <> Nil) then
    Exit;

  inherited;
end;

procedure TNEDMainForm.NEDEditorInfoDetails(var Msg: TMessage);
var
  EditorInfoDetails: PNEDEditorInfoDetails;
begin
  if Msg.WParam = 0 then begin
    txtFilePath.Caption := '---';
    txtFileType.Caption := '---';
    txtFileEncoding.Caption := '---';
    txtFileLineBreaks.Caption := '---';
    txtFileEditMode.Caption := '---';
    txtFileEditPosition.Caption := '---';
    txtStatus.Caption := '---';
  end
  else begin
    try
      EditorInfoDetails := PNEDEditorInfoDetails(Msg.LParam);
      //
      txtFilePath.Caption := EditorInfoDetails.FilePath;
      txtFileType.Caption := EditorInfoDetails.FileType;
      txtFileEncoding.Caption := EditorInfoDetails.FileEncodingType;
      txtFileLineBreaks.Caption := EditorInfoDetails.FileLineBreakType;
      txtFileEditMode.Caption := EditorInfoDetails.FileInsertType;
      txtFileEditPosition.Caption := 'Line: ' + IntToStr(EditorInfoDetails.Line) + ' / ' + IntToStr(EditorInfoDetails.Lines) + ', Column: ' + IntToStr(EditorInfoDetails.Column);
      txtStatus.Caption := EditorInfoDetails.Status;
    except
    end;
  end;
  Msg.Result := 1;
end;

procedure TNEDMainForm.NEDWindowSwitch(var Msg: TMessage);
var
  CurrentEditor: TNEDCustomEditorView;
  MoveDirection: Integer;
  i, idx: Integer;
begin
  CurrentEditor := TNEDCustomEditorView(Pointer(Msg.WParam));
  MoveDirection := Msg.LParam;

  if CurrentEditor <> Nil then begin
    idx := -1;
    for i := 0 to NEDEditors.Count - 1 do begin
      if NEDEditors.Items[i].Editor = CurrentEditor then begin
        idx := i;
        Break;
      end;
    end;
    //
    if idx > -1 then begin
      if (MoveDirection = 1) and (idx + 1 < NEDEditors.Count) then begin
        TNEDEditorForm.SelectEditorByIndex(idx + 1);
      end
      else if (MoveDirection = -1) and (idx - 1 >= 0) then begin
        TNEDEditorForm.SelectEditorByIndex(idx - 1);
      end
      else begin // wrap around
        if (MoveDirection = 1) and (idx + 1 = NEDEditors.Count) then begin
          TNEDEditorForm.SelectEditorByIndex(0);
        end
        else if (MoveDirection = -1) and (idx - 1 < 0) then begin
          TNEDEditorForm.SelectEditorByIndex(NEDEditors.Count - 1);
        end;
      end;
    end;
  end;
end;

procedure TNEDMainForm.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  // inform Windows that this form wants navigation keys
  Msg.Result := Msg.Result or DLGC_WANTTAB or DLGC_WANTARROWS or DLGC_WANTALLKEYS;
end;

procedure TNEDMainForm.RestoreWindowPlacement;
var
  i, x_pos, y_pos, x, y: Integer;
  _monitor: TMonitor;
  P: TPoint;
begin
  NEDConfig.Lock;
  try
    x_pos := 0;
    y_pos := 0;
    if NEDConfig.Monitor <> 0 then begin
      for i := 0 to Screen.MonitorCount - 1 do begin
        _monitor := Screen.Monitors[i];
        if _monitor.Handle = NEDConfig.Monitor then begin
          if NEDConfig.Monitor <> Screen.PrimaryMonitor.Handle then begin
            x_pos := _monitor.WorkareaRect.Left + NEDConfig.Position.X;
            y_pos := _monitor.WorkareaRect.Top + NEDConfig.Position.Y;
            if x_pos < 0 then
              x := _monitor.WorkareaRect.Left - x_pos
            else
              x := x_pos;
            if y_pos < 0 then
              y := _monitor.WorkareaRect.Top - y_pos
            else
              y := y_pos;
            P := Point(x, y);
            if not PtInRect(_monitor.WorkareaRect, P) then begin
              x_pos := _monitor.WorkareaRect.Left;
              y_pos := _monitor.WorkareaRect.Top;
            end;
            Break;
          end;
        end;
      end;
    end
    else begin
      x_pos := NEDConfig.Position.X;
      y_pos := NEDConfig.Position.Y;
    end;
    Self.Left := x_pos;
    Self.Top := y_pos;
    x := NEDConfig.Size.cx;
    y := NEDConfig.Size.cy;
    if x > -1 then
      Self.Width := x;
    if y > -1 then
      Self.Height := y;
  finally
    NEDConfig.Unlock;
  end;
end;

procedure TNEDMainForm.ShowHideToolBox(const AToolbox: TNEDToolboxTypeEnum);
begin
  if pnlToolbox.Visible then begin
    if FActiveToolbox <> AToolbox then begin
      FActiveToolbox := AToolbox;
      if AToolbox = ttWorkspace then
        boxWorkspace.BringToFront
      else if AToolbox = ttSearch then
        boxSearch.BringToFront;
    end
    else begin
      pnlToolbox.Visible := False;
      splLeft.Visible := False;
    end;
  end
  else begin
    FActiveToolbox := AToolbox;
    if AToolbox = ttWorkspace then
      boxWorkspace.BringToFront
    else if AToolbox = ttSearch then
      boxSearch.BringToFront;
    splLeft.Visible := True;
    pnlToolbox.Visible := True;
  end;
end;

function TNEDMainForm.CreateModalDialogForm(const AFormClass: TNEDDialogBaseClass): TNEDDialogBase;
begin
  Result := Nil;
  if FModalForm <> Nil then
    Exit;
  //
  Result := AFormClass.Create(Application);
  FModalForm := Result;
end;

function TNEDMainForm.DestroyModalDialogForm(var ADialogForm: TNEDDialogBase): Boolean;
begin
  Result := False;
  if (FModalForm = Nil) and (ADialogForm = Nil) then
    Exit;
  //
  FModalForm := Nil;
  if ADialogForm <> Nil then begin
    ADialogForm.Free;
    ADialogForm := Nil;
  end;
  Result := True;
end;

procedure TNEDMainForm.btnSelectProfileClick(Sender: TObject);
var
  NEDDialogProfiles: TNEDDialogProfiles;
begin
  NEDDialogProfiles := TNEDDialogProfiles(CreateModalDialogForm(TNEDDialogProfiles));
  try
    if NEDDialogProfiles <> Nil then begin
      if NEDDialogProfiles.Execute then begin // load selected profile
        FProfile := NEDDialogProfiles.SelectedProfile;
        txtProfile.Caption := FProfile.Name;
        // @TODO: load context if needed
        if not FProfile.LoadSessionContext then begin
          ShowMessage(FProfile.ErrorMsg);
          Exit;
        end;
        NEDHomeForm.SyncProperties(FProfile);

        btnWorkspace.Enabled := True;
        btnSearch.Enabled := True;
        ShowHideToolBox(ttWorkspace);
      end;
    end;
  finally
    DestroyModalDialogForm(TNEDDialogBase(NEDDialogProfiles));
  end;
end;

procedure TNEDMainForm.btnHomeClick(Sender: TObject);
begin
  btnHome.Enabled := False;
  try
    // empty for now
    if not NEDHomeForm.Visible then
      NEDHomeForm.Show;
    NEDHomeForm.BringToFront;
  finally
    btnHome.Enabled := True;
  end;
end;

procedure TNEDMainForm.btnWorkspaceClick(Sender: TObject);
begin
  ShowHideToolBox(ttWorkspace);
end;

procedure TNEDMainForm.btnSearchClick(Sender: TObject);
begin
  ShowHideToolBox(ttSearch);
end;

procedure TNEDMainForm.New1Click(Sender: TObject);
begin
  //
end;

procedure TNEDMainForm.Open1Click(Sender: TObject);
var
  open: TNEDDialogOpen;
begin
  if FModalForm <> Nil then
    Exit;
  //
  open := TNEDDialogOpen.Create(Application);
  FModalForm := open;
  try
    if open.Execute(FLastOpenedPath) then begin
      FLastOpenedPath := ExtractFilePath(open.FileName);
      //
      if NEDViewForm = Nil then begin
        NEDViewForm := TNEDViewForm.Create(Application);
        NEDViewForm.Parent := pnlWorkSpace;
        NEDViewForm.Align := alClient;
        NEDViewForm.Show;
      end;
      NEDViewForm.BringToFront;
      //
      if Sender = Open1 then
        NEDViewForm.OpenFile(open.FileName)
      else if Sender = Openandsplittoright1 then
        NEDViewForm.OpenFile(open.FileName, True, stSplitV)
      else if Sender = Openandsplittobottom1 then
        NEDViewForm.OpenFile(open.FileName, True, stSplitH);
    end;
  finally
    FModalForm := Nil;
    open.Free;
    open := Nil;
  end;
end;

procedure TNEDMainForm.Save1Click(Sender: TObject);
begin
//
end;

procedure TNEDMainForm.Saveas1Click(Sender: TObject);
begin
//
end;

procedure TNEDMainForm.Saveall1Click(Sender: TObject);
begin
//
end;

procedure TNEDMainForm.Close1Click(Sender: TObject);
begin
//
end;

procedure TNEDMainForm.Closeall1Click(Sender: TObject);
begin
//
end;

procedure TNEDMainForm.Exit1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TNEDMainForm.Undo1Click(Sender: TObject);
begin
//
end;

procedure TNEDMainForm.Redo1Click(Sender: TObject);
begin
//
end;

procedure TNEDMainForm.Cut1Click(Sender: TObject);
begin
//
end;

procedure TNEDMainForm.Copy1Click(Sender: TObject);
begin
//
end;

procedure TNEDMainForm.Paste1Click(Sender: TObject);
begin
//
end;

procedure TNEDMainForm.Find1Click(Sender: TObject);
begin
//
end;

procedure TNEDMainForm.Findfile1Click(Sender: TObject);
begin
//
end;

procedure TNEDMainForm.Replace1Click(Sender: TObject);
begin
//
end;

procedure TNEDMainForm.Repeatsearch1Click(Sender: TObject);
begin
//
end;

procedure TNEDMainForm.Spliteditortoright1Click(Sender: TObject);
begin
//
end;

procedure TNEDMainForm.Spliteditortobottom1Click(Sender: TObject);
begin
//
end;

procedure TNEDMainForm.NEDprojectwebsite1Click(Sender: TObject);
begin
//
end;

procedure TNEDMainForm.NitroPascalwebsite1Click(Sender: TObject);
begin
//
end;

procedure TNEDMainForm.AboutNED1Click(Sender: TObject);
begin
//
end;

end.

