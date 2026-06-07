unit ned_editor_view;

interface

uses
  SysUtils,
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
    procedure DrawLine(const ACanvas: TCanvas; const ItemIdx: Integer; const ARect: TRect);
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
    1: SetLength(aResult, 1);
    2: aResult := '';
    else
      aResult := '';
  end;
  if Length(aResult) > 0 then
    Result := aResult[1];
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
  //Params.Style := Params.Style or WS_CLIPCHILDREN;
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
  inherited;

end;

procedure TNEDCustomEditorView.Paint;
begin
  inherited;

end;

procedure TNEDCustomEditorView.DrawBackground(const ACanvas: TCanvas; var ARect: TRect);
begin

end;

procedure TNEDCustomEditorView.DrawLines(const ACanvas: TCanvas; const ARect: TRect);
begin

end;

procedure TNEDCustomEditorView.DrawLine(const ACanvas: TCanvas; const ItemIdx: Integer; const ARect: TRect);
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
end;

function TNEDCustomEditorView.GetLinesArea: TRect;
begin
  // @TODO
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
