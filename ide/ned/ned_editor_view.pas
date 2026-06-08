unit ned_editor_view;

interface

uses
  SysUtils,
  Messages,
  Classes,
  Controls,
  Graphics,
  Types,
  ned_editor_buffer;

type
  TNEDCustomEditorView = class;

  TNEDEditorGutterNumberingTypeEnum = (
    gnHidden,
    gnEachLine,
    gnEachTenth, // just like in delphi, 10 . . . 20 . . . 30
    gnEachTenthWithDashedFifths // just like in delphi, 10 . - . 20 . - . 30
  );

  TNEDEditorGutter = class(TPersistent)
  private
    FEditorControl: TNEDCustomEditorView;
    FVisible: Boolean;
    FWidth: Integer;
    FAutoSize: Boolean;
    FColor: TColor;
    FParentColor: Boolean;
    FFontColor: TColor;
    FParentFontColor: Boolean;
    FActiveLineFontColor: TColor;
    FSpacer: Boolean;
    FSpacerWidth: Integer;
    FSpacerColor: TColor;
    FSpacerParentColor: Boolean;
    FLineNumberType: TNEDEditorGutterNumberingTypeEnum;
    FLineIndicatorWidth: Integer;
    FLineModifiedIndicatorColor: TColor;
    FLineSavedIndicatorColor: TColor;
    FLineDeletedIndicatorColor: TColor;
    FLineHiddenIndicatorColor: TColor;
  protected
    procedure SetVisible(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
    procedure SetAutoSize(const Value: Boolean);
    procedure SetColor(const Value: TColor);
    procedure SetParentColor(const Value: Boolean);
    procedure SetFontColor(const Value: TColor);
    procedure SetParentFontColor(const Value: Boolean);
    procedure SetActiveLineFontColor(const Value: TColor);
    procedure SetSpacer(const Value: Boolean);
    procedure SetSpacerWidth(const Value: Integer);
    procedure SetSpacerColor(const Value: TColor);
    procedure SetSpacerParentColor(const Value: Boolean);
    procedure SetLineNumberType(const Value: TNEDEditorGutterNumberingTypeEnum);
    procedure SetLineIndicatorWidth(const Value: Integer);
    procedure SetLineModifiedIndicatorColor(const Value: TColor);
    procedure SetLineSavedIndicatorColor(const Value: TColor);
    procedure SetLineDeletedIndicatorColor(const Value: TColor);
    procedure SetLineHiddenIndicatorColor(const Value: TColor);
    //
    function GetWidth: Integer;
    //
    procedure UpdateEditor;
    procedure Paint(const ACanvas: TCanvas; const ARect: TRect);
    procedure DrawLineNumbers(const ACanvas: TCanvas; const ARect: TRect);
    procedure DrawLineStateIndicators(const ACanvas: TCanvas; const ARect: TRect);
    procedure DrawSpacer(const ACanvas: TCanvas; const ARect: TRect);
  public
    constructor Create(const AParentControl: TNEDCustomEditorView); reintroduce;
    destructor Destroy; override;
  published
    property Visible: Boolean read FVisible write SetVisible;
    property Width: Integer read FWidth write SetWidth;
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    property Color: TColor read FColor write SetColor;
    property ParentColor: Boolean read FParentColor write SetParentColor;
    property FontColor: TColor read FFontColor write SetFontColor;
    property ParentFontColor: Boolean read FParentFontColor write SetParentFontColor;
    property ActiveLineFontColor: TColor read FActiveLineFontColor write SetActiveLineFontColor;
    property Spacer: Boolean read FSpacer write SetSpacer;
    property SpacerWidth: Integer read FSpacerWidth write SetSpacerWidth;
    property SpacerColor: TColor read FSpacerColor write SetSpacerColor;
    property SpacerParentColor: Boolean read FSpacerParentColor write SetSpacerParentColor;
    property LineNumberType: TNEDEditorGutterNumberingTypeEnum read FLineNumberType write SetLineNumberType;
    property LineIndicatorWidth: Integer read FLineIndicatorWidth write SetLineIndicatorWidth;
    property LineModifiedIndicatorColor: TColor read FLineModifiedIndicatorColor write SetLineModifiedIndicatorColor;
    property LineSavedIndicatorColor: TColor read FLineSavedIndicatorColor write SetLineSavedIndicatorColor;
    property LineDeletedIndicatorColor: TColor read FLineDeletedIndicatorColor write SetLineDeletedIndicatorColor;
    property LineHiddenIndicatorColor: TColor read FLineHiddenIndicatorColor write SetLineHiddenIndicatorColor;
  end;

  TNEDEditorMinimap = class(TPersistent)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  published
  end;

  TNEDEditorCaret = class(TPersistent)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  published
  end;

  TNEDEditorSelection = class(TPersistent)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  published
  end;

  TNEDEditorScrollbar = class(TPersistent)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  published
  end;

  TNEDEditorOptions = class(TPersistent)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  published
  end;

  TNEDEditorColors = class(TPersistent)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  published
  end;

  TNEDEditorTextEntryModeEnum = (
    teInsert,
    teOverwrite
  );

  // this control will not have any non-client area
  // everything will be drawn into clent area,
  // things like:
  //  - gutter
  //  - text
  //  - minimap
  //  - caret / multi-caret
  //  - selection
  //  - scrollbars
  //
  TNEDCustomEditorView = class(TCustomControl)
  private
    FDocument: TNEDEditorBuffer;
    FObserver: TNEDDocumentObserver;
    FGutter: TNEDEditorGutter;
    FMinimap: TNEDEditorMinimap;
    FCaret: TNEDEditorCaret;
    FSelection: TNEDEditorSelection;
    FHorizontalScrollBar: TNEDEditorScrollbar;
    FVerticalScrollBar: TNEDEditorScrollbar;
    //
    FOptions: TNEDEditorOptions;
    FColors: TNEDEditorColors;
  private
    FEncoding: TEncoding;
    FModified: Boolean;
    FReadOnly: Boolean;
    FTextEntryMode: TNEDEditorTextEntryModeEnum;
    FCommentLineCount: Integer;
    FBlankLineCount: Integer;
    //
    FTopIndex: Integer;
    FVisibleLinesCount: Integer;
    FActiveLineIndex: Integer;
  private
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
//    procedure CMEntryImages(var Msg: TMessage); message CM_ENTRYIMAGES;
//    //procedure CMItemImages(var Msg: TMessage); message CM_ITEMIMAGES;
//    procedure CMItemDetails(var Msg: TMessage); message CM_ITEMDETAILS;
//    procedure CNItemClick(var Msg: TMessage); message CN_ITEMCLICK;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TWMScroll); message WM_HSCROLL;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateHandle; override;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure DrawBackground(const ACanvas: TCanvas; var ARect: TRect); virtual;
    procedure DrawLines(const ACanvas: TCanvas; const ARect: TRect); virtual;
    procedure DrawLine(const ACanvas: TCanvas; const LineIdx: Integer; const ARect: TRect); virtual;
    //
    procedure SetDocument(const DocumentBuffer: TNEDEditorBuffer);
    procedure SetObserver(const DocumentObserver: TNEDDocumentObserver);
    procedure SetActiveLineIndex(Value: Integer);
    //
    function GetGutterArea: TRect;
    function GetLinesArea: TRect;
    function GetMinimapArea: TRect;
    function GetHorizontalScrollbarArea: TRect;
    function GetVerticalScrollbarArea: TRect;
    function GetLineHeight: Integer;
    //
    procedure InvalidateLine(const LineIndex: Integer);
    procedure ScrollToSelection;
    //
    procedure DocumentChanged(const Change: TNEDDocumentChangeInfo);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Document: TNEDEditorBuffer read FDocument write SetDocument;
    property LineHeight: Integer read GetLineHeight;
    property TopIndex: Integer read FTopIndex;
    property VisibleLinesCount: Integer read FVisibleLinesCount;
    property ActiveLineIndex: Integer read FActiveLineIndex write SetActiveLineIndex;
  end;

  TNEDEditorView = class(TNEDCustomEditorView);

implementation

uses
  Windows;

function IfThen(const Cond: Boolean; TrueValue: Integer; FalseValue: Integer = 0): Integer; inline;
begin
  if Cond then
    Result := TrueValue
  else
    Result := FalseValue;
end;

function GetCharFromVirtualKey(Key: Word): Char;
var
  keyboardState: TKeyboardState;
  asciiResult: Integer;
  aResult: String;
begin
  Result := #0;
  GetKeyboardState(keyboardState);
  SetLength(aResult, 2);
  asciiResult := ToAscii(Key, MapVirtualKey(Key, 0), keyboardState, @aResult[1], 0);
  case asciiResult of
    0: aResult := '';
    1: begin
      SetLength(aResult, 1);
      if Length(aResult) > 0 then
        Result := aResult[1];
    end;
    2: begin
      if Length(aResult) > 0 then
        Result := aResult[1];
    end;
  end;
  aResult := '';
end;

{ TNEDEditorGutter }

constructor TNEDEditorGutter.Create(const AParentControl: TNEDCustomEditorView);
begin
  FEditorControl := AParentControl;
end;

destructor TNEDEditorGutter.Destroy;
begin
  FEditorControl := Nil;
  inherited;
end;

procedure TNEDEditorGutter.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then begin
    FVisible := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorGutter.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then begin
    FWidth := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorGutter.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then begin
    FAutoSize := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorGutter.SetColor(const Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorGutter.SetParentColor(const Value: Boolean);
begin
  if FParentColor <> Value then begin
    FParentColor := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorGutter.SetFontColor(const Value: TColor);
begin
  if FFontColor <> Value then begin
    FFontColor := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorGutter.SetParentFontColor(const Value: Boolean);
begin
  if FParentFontColor <> Value then begin
    FParentFontColor := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorGutter.SetActiveLineFontColor(const Value: TColor);
begin
  if FActiveLineFontColor <> Value then begin
    FActiveLineFontColor := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorGutter.SetSpacer(const Value: Boolean);
begin
  if FSpacer <> Value then begin
    FSpacer := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorGutter.SetSpacerWidth(const Value: Integer);
begin
  if FSpacerWidth <> Value then begin
    FSpacerWidth := Value;
    if FSpacerWidth < 1 then
      FSpacerWidth := 1;
    UpdateEditor;
  end;
end;

procedure TNEDEditorGutter.SetSpacerColor(const Value: TColor);
begin
  if FSpacerColor <> Value then begin
    FSpacerColor := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorGutter.SetSpacerParentColor(const Value: Boolean);
begin
  if FSpacerParentColor <> Value then begin
    FSpacerParentColor := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorGutter.SetLineNumberType(const Value: TNEDEditorGutterNumberingTypeEnum);
begin
  if FLineNumberType <> Value then begin
    FLineNumberType := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorGutter.SetLineIndicatorWidth(const Value: Integer);
begin
  if FLineIndicatorWidth <> Value then begin
    FLineIndicatorWidth := Value;
    if FLineIndicatorWidth < 1 then
      FLineIndicatorWidth := 1;
    UpdateEditor;
  end;
end;

procedure TNEDEditorGutter.SetLineModifiedIndicatorColor(const Value: TColor);
begin
  if FLineModifiedIndicatorColor <> Value then begin
    FLineModifiedIndicatorColor := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorGutter.SetLineSavedIndicatorColor(const Value: TColor);
begin
  if FLineSavedIndicatorColor <> Value then begin
    FLineSavedIndicatorColor := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorGutter.SetLineDeletedIndicatorColor(const Value: TColor);
begin
  if FLineDeletedIndicatorColor <> Value then begin
    FLineDeletedIndicatorColor := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorGutter.SetLineHiddenIndicatorColor(const Value: TColor);
begin
  if FLineHiddenIndicatorColor <> Value then begin
    FLineHiddenIndicatorColor := Value;
    UpdateEditor;
  end;
end;

function TNEDEditorGutter.GetWidth: Integer;
begin
  if not AutoSize then
    Result := Width
  else begin
    // calculate gutter width
    Result := 0; // @TODO
  end;
end;

procedure TNEDEditorGutter.UpdateEditor;
begin
  if FEditorControl <> Nil then
    FEditorControl.Repaint;
end;

procedure TNEDEditorGutter.Paint(const ACanvas: TCanvas; const ARect: TRect);
begin
  // clear background
  if ParentColor then
    ACanvas.Brush.Color := FEditorControl.Color
  else
    ACanvas.Brush.Color := Color;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.FillRect(ARect);

  // paint line numbers
  DrawLineNumbers(ACanvas, ARect);

  // paint line state indicators
  DrawLineStateIndicators(ACanvas, ARect);

  // paint spacer
  DrawSpacer(ACanvas, ARect);
end;

procedure TNEDEditorGutter.DrawLineNumbers(const ACanvas: TCanvas; const ARect: TRect);
var
  I: Integer;
  Line: TRect;
begin
  ACanvas.Font.Assign(FEditorControl.Font);
  if not ParentFontColor then
    ACanvas.Font.Color := FontColor;
  //
  for I := 0 to (ARect.Height div (FEditorControl.LineHeight - 1)) do begin
    if I + FEditorControl.TopIndex > FEditorControl.VisibleLinesCount - 1 then
      Break;
    Line := Rect(ARect.Left, ARect.Top + I * FEditorControl.LineHeight, 0, 0);
    Line.Width := ARect.Right - FLineIndicatorWidth - IfThen(FSpacer, FSpacerWidth);
    Line.Height := FEditorControl.LineHeight;
    if FEditorControl.TopIndex + I = FEditorControl.ActiveLineIndex then begin // active line
    end
    else begin // other line
    end;
  end;
end;

procedure TNEDEditorGutter.DrawLineStateIndicators(const ACanvas: TCanvas; const ARect: TRect);
begin

end;

procedure TNEDEditorGutter.DrawSpacer(const ACanvas: TCanvas; const ARect: TRect);
begin

end;

{ TNEDEditorMinimap }

constructor TNEDEditorMinimap.Create;
begin

end;

destructor TNEDEditorMinimap.Destroy;
begin

  inherited;
end;

{ TNEDEditorCaret }

constructor TNEDEditorCaret.Create;
begin

end;

destructor TNEDEditorCaret.Destroy;
begin

  inherited;
end;

{ TNEDEditorSelection }

constructor TNEDEditorSelection.Create;
begin

end;

destructor TNEDEditorSelection.Destroy;
begin

  inherited;
end;

{ TNEDEditorScrollbar }

constructor TNEDEditorScrollbar.Create;
begin

end;

destructor TNEDEditorScrollbar.Destroy;
begin

  inherited;
end;

{ TNEDEditorOptions }

constructor TNEDEditorOptions.Create;
begin

end;

destructor TNEDEditorOptions.Destroy;
begin

  inherited;
end;

{ TNEDEditorColors }

constructor TNEDEditorColors.Create;
begin

end;

destructor TNEDEditorColors.Destroy;
begin

  inherited;
end;

{ TNEDCustomEditorView }

constructor TNEDCustomEditorView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
//  ControlStyle := ControlStyle + [csOpaque, csSetCaption, csNeedsBorderPaint];
{$IF CompilerVersion > 29}
  StyleElements := [];
{$IFEND}
  BevelEdges := [];
  BevelOuter := bvNone;
  BevelInner := bvNone;
//  FullRepaint := False;
  DoubleBuffered := True;
  ParentBackground := False;
  ParentColor := False;
  TabStop := True;
  Cursor := crIBeam;
  //
  FDocument := Nil;
  FObserver := Nil;
  FGutter := TNEDEditorGutter.Create(Self);
  FMinimap := TNEDEditorMinimap.Create;
  FCaret := TNEDEditorCaret.Create;
  FSelection := TNEDEditorSelection.Create;
  FHorizontalScrollBar := TNEDEditorScrollbar.Create;
  FVerticalScrollBar := TNEDEditorScrollbar.Create;
  //
  FOptions := TNEDEditorOptions.Create;
  FColors := TNEDEditorColors.Create;
  //
  //
  FEncoding := TEncoding.UTF8;
  FModified := False;
  FReadOnly := False;
  FTextEntryMode := teInsert;
  FCommentLineCount := 0;
  FBlankLineCount := 0;
  //
  FTopIndex := 0;
  FVisibleLinesCount := 0;
  FActiveLineIndex := 0;
  //
  Color := $001F1F1F;
  Font.Color := clWhite;
end;

destructor TNEDCustomEditorView.Destroy;
begin
  Document := Nil;
  FGutter.Free;
  FMinimap.Free;
  FCaret.Free;
  FSelection.Free;
  FHorizontalScrollBar.Free;
  FVerticalScrollBar.Free;
  FOptions.Free;
  FColors.Free;
  inherited;
end;

procedure TNEDCustomEditorView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  // remove OS (windows) painted scrollbars
  // after this we need our own scrollbars renderer, that will paint them as we want
  Params.WindowClass.style := Params.WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  // don't repaint areas occupied by child controls - not sure for now, if this is usefull
  Params.Style := Params.Style or WS_CLIPCHILDREN;
end;

procedure TNEDCustomEditorView.CreateHandle;
begin
  inherited CreateHandle;
//  if FTextHeight = 0 then
//    FTextHeight := Canvas.TextHeight(' ');
//  HideNativeScrollBars;
//  UpdateScrollRange;
//  ShowModernScrollBar;
end;

procedure TNEDCustomEditorView.Loaded;
begin
  inherited;

end;

procedure TNEDCustomEditorView.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
  inherited;
  Msg.Result := 1;
end;

procedure TNEDCustomEditorView.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  Canvas.Font := Font;
  //FTextHeight := Canvas.TextHeight(' ');
end;

procedure TNEDCustomEditorView.CMMouseEnter(var Msg: TMessage);
begin
//  HideNativeScrollBars;
//  ShowModernScrollBar;
  inherited;
//  CaptureItem := FMouseItem;
//  FMouseItem := Nil;
  Repaint;
end;

procedure TNEDCustomEditorView.CMMouseLeave(var Msg: TMessage);
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);
//  if not PtInRect(ClientRect, P) then
//    HideModernScrollBar;
  inherited;
//  if MouseCapture then
//    FMouseItem := CaptureItem
//  else
//    FMouseItem := Nil;
//  CaptureItem := Nil;
//  FHotIndex := -1;
  Repaint;
end;

procedure TNEDCustomEditorView.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TNEDCustomEditorView.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := DLGC_WANTARROWS or DLGC_WANTTAB or DLGC_WANTALLKEYS or DLGC_WANTCHARS;
//  Msg.Result := Msg.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
//  if fWantTabs then
//    Msg.Result := Msg.Result or DLGC_WANTTAB;
//  if fWantReturns then
//    Msg.Result := Msg.Result or DLGC_WANTALLKEYS;
end;

procedure TNEDCustomEditorView.WMSize(var Msg: TWMSize);
begin
  inherited;
//  HideNativeScrollBars;
//  UpdateScrollRange;
//  ShowModernScrollBar;
end;

procedure TNEDCustomEditorView.WMMouseWheel(var Msg: TWMMouseWheel);
var
  Delta: Integer;
  p: TPoint;
begin
//  if Selected <> Nil then begin
    Delta := -Msg.WheelDelta div 120;
//    if not IsRectEmpty(ButtonRect[fbScrollUp]) and (Delta = -1) then
//      Selected.TopIndex := Selected.TopIndex - 1
//    else if not IsRectEmpty(ButtonRect[fbScrollDown]) and (Delta = 1) then
//      Selected.TopIndex := Selected.TopIndex + 1;
//    if not MouseCapture then begin
//      p := ScreenToClient(Point(Message.XPos, Message.YPos));
//      CaptureItem := ItemFromPoint(p.X, p.Y);
//    end;
//  end;
//  if delta > 0 then
//    SetTopIndex(FTopIndex + 1)
//  else
//    SetTopIndex(FTopIndex - 1);
  inherited;
end;

procedure TNEDCustomEditorView.WMVScroll(var Msg: TWMScroll);
begin
//  HideNativeScrollBars;
//  case Msg.ScrollCode of
//    SB_TOP       : SetTopIndex(0);
//    SB_BOTTOM    : SetTopIndex(VisibleEntriesCount - 1);
//    SB_LINEDOWN  : SetTopIndex(FTopIndex + 1);
//    SB_LINEUP    : SetTopIndex(FTopIndex - 1);
//    SB_PAGEDOWN  : SetTopIndex(FTopIndex + ClientHeight div FEntryHeight);
//    SB_PAGEUP    : SetTopIndex(FTopIndex - ClientHeight div FEntryHeight);
//    SB_THUMBTRACK: SetTopIndex(Msg.Pos);
//  end;
end;

procedure TNEDCustomEditorView.WMHScroll(var Msg: TWMScroll);
begin
  // we do not scroll left/right
//  HideNativeScrollBars;
end;

procedure TNEDCustomEditorView.WMTimer(var Msg: TWMTimer);
begin
//  if FButtons = [] then begin
//    KillTimer(Handle, 1);
//    Exit;
//  end;
//  with Selected do
//    if fbScrollUp in FButtons then begin
//      TopIndex := TopIndex - 1;
//      if IsRectEmpty(ButtonRect[fbScrollUp]) then
//        FButtons := [];
//    end
//    else begin
//      TopIndex := TopIndex + 1;
//      if IsRectEmpty(ButtonRect[fbScrollDown]) then
//        FButtons := [];
//    end;
//  if FButtons = [] then
//    KillTimer(Handle, 1);
end;

procedure TNEDCustomEditorView.WMSetFocus(var Msg: TWMSetFocus);
begin
//  HideNativeScrollBars;
//  ShowModernScrollBar;
  inherited;
  Invalidate;
end;

procedure TNEDCustomEditorView.WMKillFocus(var Msg: TWMKillFocus);
begin
//  HideModernScrollBar;
  inherited;
//  FMouseCapture := False;
//  FDownIndex := -1;
  Invalidate;
end;

procedure TNEDCustomEditorView.WMNCHitTest(var Msg: TWMNCHitTest);
var
  P: TPoint;
  R: TRect;
begin
  inherited;

  P := ScreenToClient(Point(Msg.XPos, Msg.YPos));
  R := ClientRect;

  // Right-side fake scrollbar
//  if (P.X >= R.Right - FVerticalScrollBar.Width) and not FVerticalScrollBar.Visible then begin
//    //Msg.Result := HTVSCROLL;
//    FVerticalScrollBar.ShowAnimated;
//  end;
end;

procedure TNEDCustomEditorView.KeyDown(var Key: Word; Shift: TShiftState);
var
  PressedKey: Char;
  CaretPos: TNEDTextPosition;
begin
  inherited;

  if Key = 0 then
    Exit;

  if FReadOnly then
    Exit;

  case Key of
    VK_UP: begin
      Key := 0;
    end;
    VK_DOWN: begin
      Key := 0;
    end;
    VK_LEFT: begin
      Key := 0;
    end;
    VK_RIGHT: begin
      Key := 0;

    end;
    VK_BACK: begin
      Key := 0;
//      CaretPos := FCaret.GetCaretPosition;
    end;
    VK_DELETE: begin
      Key := 0;
    end;
  end;

  if Key = 0 then
    Exit;

  PressedKey := GetCharFromVirtualKey(Key);
  if PressedKey <> '' then begin
//      CaretPos := FCaret.GetCaretPosition;
    FDocument.Insert(CaretPos, PressedKey);
  end;
end;

procedure TNEDCustomEditorView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

procedure TNEDCustomEditorView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

procedure TNEDCustomEditorView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

procedure TNEDCustomEditorView.DblClick;
begin
  inherited;

end;

procedure TNEDCustomEditorView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent <> Nil) then begin
//    if AComponent = FEntryImages then
//      UpdateImages(FEntryImages, Nil);
//    else if AComponent = FItemImages then
//      UpdateImages(FItemImages, Nil);
  end;
end;

procedure TNEDCustomEditorView.Paint;
var
  LinesR, SB: TRect;
begin
  LinesR := GetLinesArea;
  DrawBackground(Canvas, LinesR); // clear background
//  if FDrawVerticalScrollBar or (not MouseInClient and Focused and not FVerticalScrollBar.Visible) then begin
//    FDrawVerticalScrollBar := False;
//    SB := VerticalScrollBarArea;
//    DrawBackground(Canvas, SB); // clear background
//  end;
  //
  if FGutter.Visible and (FGutter.GetWidth > 0) then
    FGutter.Paint(Canvas, GetGutterArea);
  //
  DrawLines(Canvas, LinesR); //draw items
  //
//  if FMinimap.Visible then
//    FMinimap.Paint(Canvas, GetMinimapArea);
  //
//  if FHorizontalScrollBar.Visible then
//    FHorizontalScrollBar.Paint(Canvas, GetHorizontalScrollbarArea);
//  if FVerticalScrollBar.Visible then
//    FVerticalScrollBar.Paint(Canvas, GetVerticalScrollbarArea);
end;

procedure TNEDCustomEditorView.DrawBackground(const ACanvas: TCanvas; var ARect: TRect);
begin
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := ColorToRGB(Color);
  ACanvas.FillRect(ARect);
  //
//  if FBorderStyle = bsSingle then begin
//    DrawThemeBorder(ACanvas.Handle, Color, ARect, []);
//    InflateRect(ARect, -GetBorder, -GetBorder);
//    //SelectClipRect(DC, Rect, RGN_AND);
//  end;
end;

procedure TNEDCustomEditorView.DrawLines(const ACanvas: TCanvas; const ARect: TRect);
var
  ClipR: TRect;
begin
  ClipR := ACanvas.ClipRect;

end;

procedure TNEDCustomEditorView.DrawLine(const ACanvas: TCanvas; const LineIdx: Integer; const ARect: TRect);
begin

end;

procedure TNEDCustomEditorView.SetDocument(const DocumentBuffer: TNEDEditorBuffer);
begin
  // unbind observer from old document
  if FDocument <> Nil then begin
    FDocument.RemoveObserver(Self.FObserver);
    SetObserver(Nil);
  end;

  FDocument := DocumentBuffer;

  // bind observer to new document
  if DocumentBuffer <> Nil then begin
    SetObserver(TNEDDocumentObserver.Create(FDocument));
    FDocument.AddObserver(Self.FObserver);
  end;

  Invalidate;
end;

procedure TNEDCustomEditorView.SetObserver(const DocumentObserver: TNEDDocumentObserver);
begin
  if FObserver <> Nil then
    FObserver.Free;

  FObserver := DocumentObserver;

  if DocumentObserver <> Nil then
    FObserver.OnDocumentChanged := DocumentChanged;
end;

procedure TNEDCustomEditorView.SetActiveLineIndex(Value: Integer);
var
  PriorIndex: Integer;
begin
  if FActiveLineIndex <> Value then begin
    if Value < 0 then
      Value := 0;
    if Value >= FDocument.LineCount then
      Value := FDocument.LineCount - 1;
    //
    if not HandleAllocated then begin
      FActiveLineIndex := Value;
      Exit;
    end;
    //
    PriorIndex := FActiveLineIndex;
    if PriorIndex > -1 then
      InvalidateLine(PriorIndex);
    //
    FActiveLineIndex := Value;
    ScrollToSelection;
    if PriorIndex <> FActiveLineIndex then begin
      InvalidateLine(FActiveLineIndex);
//      if FMultiSelect and (FItemIndex > -1) then begin
//        if ssShift in FShift then begin
//          FSelectedCount := 1;
//          FShiftIndex := FItemIndex;
//          Invalidate;
//        end
//        else if ssCtrl in FShift then begin
//          if FSelectedCount > 1 then
//            Invalidate;
//        end
//        else begin
//          FSelectedCount := 1;
//          Invalidate;
//        end;
//      end
//      else begin
//        Selected := ItemFromIndex(FItemIndex);
//      end;
    end;
  end
end;

function TNEDCustomEditorView.GetGutterArea: TRect;
begin
  Result := TRect.Empty;
  if not FGutter.Visible then
    Exit;

  Result := Rect(0, 0, FGutter.GetWidth, Self.Height);
//  if FHorizontalScrollBar.Visible then
//    Result.Bottom := Result.Bottom - FHorizontalScrollBar.Size;
end;

function TNEDCustomEditorView.GetLinesArea: TRect;
var
  Gutter: TRect;
begin
  Gutter := GetGutterArea;
  Result := Rect(Gutter.Width, 0, Self.Width, Self.Height);
//  if FVerticalScrollBar.Visible then
//    Result.Right := Result.Right - FVerticalScrollBar.Size;
//  if FHorizontalScrollBar.Visible then
//    Result.Bottom := Result.Bottom - FHorizontalScrollBar.Size;
end;

function TNEDCustomEditorView.GetMinimapArea: TRect;
begin
  // @TODO
end;

function TNEDCustomEditorView.GetHorizontalScrollbarArea: TRect;
begin
  // @TODO
end;

function TNEDCustomEditorView.GetVerticalScrollbarArea: TRect;
begin
  // @TODO
end;

function TNEDCustomEditorView.GetLineHeight: Integer;
begin
  Result := 0; // @TODO
end;

procedure TNEDCustomEditorView.InvalidateLine(const LineIndex: Integer);
begin
  // @TODO
end;

procedure TNEDCustomEditorView.ScrollToSelection;
begin
  // @TODO
end;

procedure TNEDCustomEditorView.DocumentChanged(const Change: TNEDDocumentChangeInfo);
begin
  // @TODO - ??? do something ???


  // and than
  Invalidate;
end;

end.
