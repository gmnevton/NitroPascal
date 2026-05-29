unit uFolders;

{$WRITEABLECONST ON}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  ExtCtrls,
  Forms,
  ImgList,
  CommCtrl,
  Generics.Collections;

const
  CM_FOLDERBASE  = $AA00;
  CM_ENTRYIMAGES = CM_FOLDERBASE + 1;
  //CM_ITEMIMAGES  = CM_ENTRYIMAGES + 1;
  CM_ITEMDETAILS = CM_ENTRYIMAGES + 1;
  CN_ITEMCLICK   = CM_ITEMDETAILS + 1;

type
  TDrawStateItem = (
    dsDisabled,
    dsPressed,
    dsSelected,
    dsHot,
    dsFocused,
    dsChecked,
    dsExpanded,
    dsDefaulted,
    dsThin,
    dsFlat,
    dsBackground,
    dsCustom
  );
  TDrawState = set of TDrawStateItem;

  TDrawArrowDirection = (
    daUp,
    daDown,
    daLeft,
    daRight
  );

  TEntryItem = class;
  TEntryItems = class;
  TCustomEntryView = class;

  TEntryItemDetails = record
    BorderStyle: TborderStyle;
    Selected: TEntryItem;
    ClientRect: TRect;
    EntryHeight: Integer;
    //FolderHeight: Integer;
    TopIndex: Integer;
  end;
  PEntryItemDetails = ^TEntryItemDetails;

  TUserFreeProc = procedure (var AData: Pointer) of object;

  TEntryStateEnum = (esCollapsed, esExpanded);

  TEntryItemPositionEnum = (epAbsolute, epRelative);

  TEntryItemTypeEnum = (etUnknown, etSubDirectory, etFile);

  TEntryItem = class(TCollectionItem)
  private
    // expand/collapse icon (left/right aligned) - entry icon - entry caption - [child entries]
    FIndex: Integer; // absolute entry index; updated while tree state is being changed
    FLevel: Integer; // indentation level
    FCaption: string;
    FEnabled: Boolean;
    FVisible: Boolean;
    FImageIndex: Integer;
    FEntryState: TEntryStateEnum;
    FItems: TEntryItems;
    //
    FData: Pointer;
    FDataObject: Boolean;
    FUserFreeProc: TUserFreeProc;
    //
    FParentEntries: TEntryItems;
    //
    procedure SetCaption(const Value: String);
    procedure SetEnabled(Value: Boolean);
    procedure SetVisible(Value: Boolean);
    procedure SetImageIndex(Value: Integer);
    procedure SetEntryState(Value: TEntryStateEnum);
    procedure SetItems(Value: TEntryItems);
    //
    function GetVisible: Boolean;
    //
    function GetDisplayRect(const AbsoluteOrRelative: TEntryItemPositionEnum): TRect;
//  protected
//    function GetNearestTop: Integer;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    //
    procedure Assign(Source: TPersistent); override;
    procedure Click;
    procedure Collapse;
    procedure Expand;
    function AbsoluteDisplayRect: TRect;
    function RelativeDisplayRect: TRect;
    function HasChildren: Boolean;
    function IsExpanded: Boolean;
    function ParentIsVisible: Boolean;
    function ParentHasChildren: Boolean;
    function ParentIsExpanded: Boolean;
    //
    //property DisplayRect: TRect read GetDisplayRect;
    property Data: Pointer read FData write FData;
    property DataObject: Boolean read FDataObject write FDataObject;
    property ParentEntries: TEntryItems read FParentEntries;
    property TreeIndex: Integer read FIndex;
    property TreeLevel: Integer read FLevel;
  published
    property Caption: string read FCaption write SetCaption;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Visible: Boolean read GetVisible write SetVisible;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property State: TEntryStateEnum read FEntryState write SetEntryState;
    property Items: TEntryItems read FItems write SetItems;
    //
    property UserFreeProc: TUserFreeProc read FUserFreeProc write FUserFreeProc;
  end;

  TEntryItems = class(TCollection)
  private
    FOwnerEntry: TEntryItem;
    FControl: TControl;
  protected
    procedure Update(Item: TCollectionItem); override; // this tells us, that collection changed after EndUpdate
    function Get(Index: Integer): TEntryItem;
    procedure Put(Index: Integer; Value: TEntryItem);
    //
    function GetTreeIndex(Entry: TEntryItem): Integer;
    function GetTreeLevel(Entry: TEntryItem): Integer;
    //
    function GetFirstEntry(const Entry: TEntryItem): TEntryItem; // inline;
    function GetLastEntry(const Entry: TEntryItem): TEntryItem; // inline;
    function GetPrevEntry(Entry: TEntryItem): TEntryItem; // inline;
    function GetNextEntry(Entry: TEntryItem): TEntryItem; // inline;
    //
    function GetFirstEntrySibling(const Entry: TEntryItem): TEntryItem; // inline;
    function GetLastEntrySibling(const Entry: TEntryItem): TEntryItem; // inline;
    function GetPrevEntrySibling(const Entry: TEntryItem): TEntryItem; // inline;
    function GetNextEntrySibling(const Entry: TEntryItem): TEntryItem; // inline;

//    function DisplayRect(const Entry: TEntryItem): TRect;
//    function GetNearestTop: Integer;
  public
    constructor Create(Control: TControl; OwnerEntry: TEntryItem);
    //
    procedure Assign(Source: TPersistent); override;
    function Add: TEntryItem;
    function Insert(Index: Integer): TEntryItem;
    //
    property Items[Index: Integer]: TEntryItem read Get write Put; default;
    property OwnerEntry: TEntryItem read FOwnerEntry;
    property Control: TControl read FControl;
  end;

//  TFolderBar = class(TCollectionItem)
//  private
//    FCaption: string;
//    FVisible: Boolean;
//    FImageIndex: Integer;
//    FData: Pointer;
//    FDataObject: Boolean;
//    FItems: TFolderItems;
//    FSelectedIndex: Integer;
//    FTopIndex: Integer;
//    FUserFreeProc: TUserFreeProc;
//    //
//    procedure SetCaption(const Value: string);
//    procedure SetVisible(Value: Boolean);
//    procedure SetImageIndex(Value: Integer);
//    procedure SetItems(Value: TFolderItems);
//    procedure SetSelectedIndex(Value: Integer);
//    procedure SetTopIndex(Value: Integer);
//    //
//    function GetDisplayRect: TRect;
//  protected
//    function GetNearestTop: Integer;
//  public
//    constructor Create(Collection: TCollection); override;
//    destructor Destroy; override;
//    //
//    procedure Assign(Source: TPersistent); override;
//    property Data: Pointer read FData write FData;
//    property DataObject: Boolean read FDataObject write FDataObject;
//    property DisplayRect: TRect read GetDisplayRect;
//    property TopIndex: Integer read FTopIndex write SetTopIndex;
//  published
//    property Caption: string read FCaption write SetCaption;
//    property Visible: Boolean read FVisible write SetVisible;
//    property ImageIndex: Integer read FImageIndex write SetImageIndex;
//    property Items: TFolderItems read FItems write SetItems;
//    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
//    property UserFreeProc: TUserFreeProc read FUserFreeProc write FUserFreeProc;
//  end;

//  TFolderBars = class(TCollection)
//  private
//    FControl: TControl;
//  protected
//    procedure Update(Item: TCollectionItem); override;
//    function Get(Index: Integer): TFolderBar;
//    procedure Put(Index: Integer; Value: TFolderBar);
//  public
//    constructor Create(Control: TControl);
//    procedure Assign(Source: TPersistent); override;
//    function Add: TFolderBar;
//    function Insert(Index: Integer): TFolderBar;
//    property Items[Index: Integer]: TFolderBar read Get write Put; default;
//    property Control: TControl read FControl;
//  end;

{ TCustomScrollBar }

// we successfully hidden native scrollbars, so now we have to draw our own, using this component bellow, we place it above Entries
// this will allow us to receive OS events directly, so mouse enter/leave, mouse move, clicks and potentially gestures are to handle
// but need to keep small steps, one after another, so handle basics first

  TCustomScrollBarThumbButtonEnum = (tbTop, tbScroll, tbBottom);
  TCustomScrollBarShowingEnum = (
    shHidden,
    shShowing, // shFadeInThin,
    shVisibleThin,
    shExpanding,
    shVisibleFull, //shExpanded,
    shCollapse,
    //shVisibleThin
    shHiding // shFadeOut
  );

  TCustomScrollBarShowingChangeEvent = procedure (Sender: TObject; State: TCustomScrollBarShowingEnum) of object;

  TCustomScrollBar = class(TCustomControl)
  private
    FRange: Integer; // total content size
    FPageSize: Integer; // visible size
    FPosition: Integer; // current offset
    FScrollBarKind: TScrollBarKind;
    FXYScrollSize: TSize;
    FUpdating: Boolean;
    // colors
    FThumbButtonColorInactive: TColor;
    FThumbButtonColorActive: TColor;
    FThumbButtonColorPressed: TColor;
    //
    FScrollColorInactive: TColor;
    FScrollColorActive: TColor;
    FScrollColorPressed: TColor;
    //
    //
    FTimer: TTimer;
    FVisibleState: Boolean;
    FCurrentWidth: Integer;
    FTargetWidth: Integer;
    FCurrentAlpha: Byte;
    FTargetAlpha: Byte;
    FThinWidth: Integer;
    FExpandedWidth: Integer;
    //
    FShowing: TCustomScrollBarShowingEnum;
    FShowingChangeEvent: TCustomScrollBarShowingChangeEvent;
  private
    procedure SetRange(const Value: Integer);
    procedure SetPageSize(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    function GetMaxPosition: Integer;
    procedure DoShowing(const AState: TCustomScrollBarShowingEnum);
    procedure AnimationTimer(Sender: TObject);
    // messages
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMVisibleChanged(var Msg: TMessage); message CM_VISIBLECHANGED;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
  protected
    function  CalculateThumbRect(const DrawRect: TRect; const Button: TCustomScrollBarThumbButtonEnum): TRect;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure DrawBackground(const ACanvas: TCanvas; var ARect: TRect); virtual;
    procedure DrawButtons(const ACanvas: TCanvas; const ARect: TRect); virtual;
  public
    constructor Create(AOwner: TComponent; AKind: TScrollBarKind); reintroduce;
    destructor Destroy; override;
    // methods
    procedure SetInfo(const ARange, APageSize, APosition: Integer);
    procedure ShowAnimated;
    procedure HideAnimated;
    // properties
    property Range: Integer read FRange write SetRange;
    property PageSize: Integer read FPageSize write SetPageSize;
    property Position: Integer read FPosition write SetPosition;
    property MaxPosition: Integer read GetMaxPosition;
    property OnShowingChange: TCustomScrollBarShowingChangeEvent read FShowingChangeEvent write FShowingChangeEvent;
  end;

  TEntryViewScrollBar = class(TCustomScrollBar)
  public
  published
  end;

// i have to go, so for now this is not finished, but i come back to this custom drawn scrollbar probably in the next stream
// there was good background work done, so now it is a matter of finding of what is going on, debugging and fixing stuff
// until it starts to work as intended

{ TCustomEntryView }

  TEntryItemEvent = procedure (Sender: TObject; Item: TEntryItem) of object;
  TEntryItemSelectionEvent = procedure (Sender: TObject; Item: TEntryItem; IsSubDirectory: Boolean) of object;
  TEntryItemGetTypeEvent = procedure (Sender: TObject; Item: TEntryItem; var ItemType: TEntryItemTypeEnum) of object;

  TCustomEntryView = class(TCustomControl)
  private // fileds that will be accesible by user
    FActiveIndex: Integer;
    FBorderStyle: TBorderStyle;
    FCaptureItem: TEntryItem;
    FChangeLink: TChangeLink;
    FEntryHeight: Integer;
    FEntryImages: TCustomImageList;
    FEntries: TEntryItems;
    FHotTrack: Boolean;
    FHotIndex: Integer;
//    FItemImages: TCustomImageList;
//    FItemHeight: Integer;
    FMouseItem: TEntryItem;
    //FMousePoint: TPoint;
    FMultiSelect: Boolean;
    FSelectedCount: Integer;
    FSelected: TEntryItem;
    FSelectedItems: TObjectList<TEntryItem>;
    FOverlayPicture: TPicture;
    FItemIndex: Integer;
    FActiveColor: TColor;
    FSelectedColor: TColor;
    FHotColor: TColor;
    // events
    FOnChange: TNotifyEvent;
    FOnItemClick: TEntryItemEvent;
    FOnItemSelection: TEntryItemSelectionEvent;
    FOnItemGetType: TEntryItemGetTypeEvent;
  private // internal fields
    FTextHeight: Integer;
    FTopIndex: Integer;
    FLocked: Boolean;
    FLockedIndex: Integer;
    FShift: TShiftState;
    FShiftIndex: Integer;
    FEntriesCount: Integer;
    FVisibleEntriesCount: Integer;
    FDrawVerticalScrollBar: Boolean;
    FVerticalScrollBar: TEntryViewScrollBar;
  private // setters
    procedure SetActiveIndex(Value: Integer);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetCaptureItem(Value: TEntryItem);
    procedure SetEntryHeight(Value: Integer);
    procedure SetEntryImages(Value: TCustomImageList);
    procedure SetEntries(Value: TEntryItems);
    procedure SetItemIndex(Value: Integer);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetOverlayPicture(Value: TPicture);
    procedure SetScrollIndex(Value: Integer);
    procedure SetSelected(Value: TEntryItem);
    procedure SetTopIndex(Value: Integer);
    procedure SetActiveColor(Value: TColor);
    procedure SetSelectedColor(Value: TColor);
    procedure SetHotColor(Value: TColor);
  private // getters
    procedure ComponentPropertiesChanged;
    //function GetButtonRect(Button: TFolderScrollButton): TRect;
    procedure EntriesChanged;
    function GetEntriesCount: Integer;
    function GetVisibleEntriesCount: Integer;
    //function GetSelectedRect: TRect;
    //function GetButtonRect(Button: TFolderScrollButton): TRect;
    function GetSelectedRect: TRect;
    //function GetIndentLevel(const Entry: TEntryItem): Integer; // inline;
    function CanShowScrollBar: Boolean;
    procedure UpdateModernScrollBar;
    procedure ShowModernScrollBar;
    procedure HideModernScrollBar;
    procedure InvalidateScrollBarArea;
    procedure VerticalScrollBarShowingChange(Sender: TObject; State: TCustomScrollBarShowingEnum);
  private // internal methods
    procedure ImagesChange(Sender: TObject);
    procedure OverlayChange(Sender: TObject);
    //
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMEntryImages(var Msg: TMessage); message CM_ENTRYIMAGES;
    //procedure CMItemImages(var Msg: TMessage); message CM_ITEMIMAGES;
    procedure CMItemDetails(var Msg: TMessage); message CM_ITEMDETAILS;
    procedure CNItemClick(var Msg: TMessage); message CN_ITEMCLICK;
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
    procedure DoItemClick(Item: TEntryItem); dynamic;
    procedure EnsureItemVisible;
    //function GetClientRect: TRect; override;
    function ClientArea: TRect;
    function VerticalScrollBarArea: TRect;
    function ItemFromPoint(X, Y: Integer): TEntryItem;
    function ItemRect(Item: Integer): TRect;
    function ItemAtPos(const Pos: TPoint; Existing: Boolean = False): Integer;
    function ItemFromIndex(const Index: Integer): TEntryItem;
    function ItemSelected(const Item: TEntryItem): Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure DblClick; override;
    procedure DrawBackground(const ACanvas: TCanvas; var ARect: TRect); virtual;
    procedure DrawItems(const ACanvas: TCanvas; const ARect: TRect); virtual;
    procedure DrawItem(const ACanvas: TCanvas; const ItemIdx: Integer; const ARect: TRect; DrawState: TDrawState);
    //procedure DrawEntry(const ACanvas: TCanvas; const ARect: TRect; const DrawState: TDrawState; const Entry: TEntryItem);
    //procedure DrawFolderItem(const ACanvas: TCanvas; const ARect: TRect; const DrawState: TDrawState; const Item: TFolderItem);
//    procedure DrawEntryItem(const ACanvas: TCanvas; const ARect: TRect; const DrawState: TDrawState; const Folder: TFolderBar);
    procedure DrawEntryItem(const ACanvas: TCanvas; const ARect: TRect; const DrawState: TDrawState; const Item: TEntryItem);
    procedure SelectItem(PriorIndex: Integer; NewIndex: Integer; var CanSelect: Boolean); virtual;
    procedure UpdateImages(var InternalImages: TCustomImageList; ExternalImages: TCustomImageList);
    procedure HideNativeScrollBars;
    procedure UpdateScrollRange;
    procedure InvalidateItem(Item: Integer);
    procedure ScrollToSelection;
    procedure Scroll(Delta: Integer); virtual;
    procedure ScrollBy(DeltaX, DeltaY: Integer); reintroduce;
    // properties
    property ActiveIndex: Integer read FActiveIndex write SetActiveIndex;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
//    property ButtonRect[Button: TFolderScrollButton]: TRect read GetButtonRect;
    property CaptureItem: TEntryItem read FCaptureItem write SetCaptureItem;
    property EntryHeight: Integer read FEntryHeight write SetEntryHeight;
    property EntryImages: TCustomImageList read FEntryImages write SetEntryImages;
    property OverlayPicture: TPicture read FOverlayPicture write SetOverlayPicture;
    property Entries: TEntryItems read FEntries write SetEntries;
//    property ItemImages: TCustomImageList read FItemImages write SetItemImages;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect;
    property Selected: TEntryItem read FSelected write SetSelected;
    property TopIndex: Integer read FTopIndex write SetTopIndex;
    property EntriesCount: Integer read FEntriesCount;
    property VisibleEntriesCount: Integer read FVisibleEntriesCount;
    // colors
    property ActiveColor: TColor read FActiveColor write SetActiveColor;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor;
    property HotColor: TColor read FHotColor write SetHotColor;
    // events
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnItemClick: TEntryItemEvent read FOnItemClick write FOnItemClick;
    property OnItemSelection: TEntryItemSelectionEvent read FOnItemSelection write FOnItemSelection;
    property OnItemGetType: TEntryItemGetTypeEvent read FOnItemGetType write FOnItemGetType;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //
    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Update; override;
    function GetFirstEntry: TEntryItem; // inline;
    function GetLastEntry: TEntryItem; // inline;
    function GetPrevEntry(Entry: TEntryItem): TEntryItem; // inline;
    function GetNextEntry(Entry: TEntryItem): TEntryItem; // inline;
    function GetFirstEntrySibling(const Entry: TEntryItem): TEntryItem; // inline;
    function GetLastEntrySibling(const Entry: TEntryItem): TEntryItem; // inline;
    function GetPrevEntrySibling(const Entry: TEntryItem): TEntryItem; // inline;
    function GetNextEntrySibling(const Entry: TEntryItem): TEntryItem; // inline;
    function EntryFromPoint(X, Y: Integer): TEntryItem;
  end;

  TEntryView = class(TCustomEntryView)
  public
    property Selected;
    property EntryHeight;
    property EntriesCount;
    property VisibleEntriesCount;
  published
    property Align;
    property ActiveColor;
    property ActiveIndex;
    property Anchors;
    property BorderStyle;
    property Color;
    property DesktopFont;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property EntryImages;
    property Entries;
    property HotColor;
//    property ItemImages;
    property MultiSelect;
    property OverlayPicture;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property SelectedColor;
    property TabOrder;
    property TabStop;
    // events
    property OnCanResize;
    property OnDblClick;
    property OnChange;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnItemClick;
    property OnItemGetType;
    property OnItemSelection;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  Math,
  Themes,
  UXTheme;

type
  TWinControlAccess = class(TWinControl);

  TDirection = (drLeft, drUp, drRight, drDown, drCenter, drFill, drWrap);
  //TDirections = set of TDirection;
  TDrawFrameState = (dfFocus, dfFramed, dfHover, dfRaised, dfFlat, dfLowered, dfSunken, dfPressed, dfPushed);

  TFastBitmap = record
    DC: HDC;
    Handle: HBITMAP;
    Bits: Pointer;
    Width: Integer;
    Height: Integer;
  end;

const
  DR_FORMAT = DT_SINGLELINE or DT_END_ELLIPSIS;
  DR_LEFT = DR_FORMAT or DT_VCENTER or DT_LEFT;
  DR_TOP = DR_FORMAT or DT_TOP or DT_LEFT;
  DR_RIGHT = DR_FORMAT or DT_VCENTER or DT_RIGHT;
  DR_BOTTOM = DR_FORMAT or DT_BOTTOM or DT_LEFT;
  DR_CENTER = DR_FORMAT or DT_VCENTER or DT_CENTER;
  DR_FILL = DT_SINGLELINE or DT_VCENTER or DT_CENTER or DT_NOCLIP;
  DR_WRAP = DT_TOP or DT_WORDBREAK or DT_END_ELLIPSIS;
  DR_WRAP_CENTER = DT_VCENTER or DT_CENTER or DT_WORDBREAK or DT_END_ELLIPSIS;

const
  Directions: array[TDirection] of Integer = (DR_LEFT or DT_VCENTER, DR_TOP, DT_RIGHT or DT_VCENTER, DR_BOTTOM, DR_CENTER, DR_FILL, DR_WRAP_CENTER);

var
  CheckeredBitmap: TBitmap = Nil;

//------------------------------------------------------------------------------------------------------------------------------

function RectInRect(const ARect, AInRect: TRect): Boolean; inline;
begin
  Result := (ARect.Top >= AInRect.Top) and
            (ARect.Left >= AInRect.Left) and
            (ARect.Bottom <= AInRect.Bottom) and
            (ARect.Right <= AInRect.Right);
end;

function IfThen(const Cond: Boolean; ValueTrue, ValueFalse: Integer): Integer; inline;
begin
  if Cond then
    Result := ValueTrue
  else
    Result := ValueFalse;
end;

function GetBorder: Integer; inline;
begin
	if StyleServices.Enabled then
  	Result := 1
  else
  	Result := GetSystemMetrics(SM_CXEDGE);
end;

function AverageColor(Color: TColor): Byte; inline;
var
	RGB: TRGBQuad absolute Color;
begin
  Color := ColorToRGB(Color);
  Result := (RGB.rgbBlue + RGB.rgbGreen + RGB.rgbRed) div 3;
end;

function GetTextColor(Background: TColor): TColor; inline;
var
  L, H: TColor;
begin
  if AverageColor(clWindow) > AverageColor(clWindowFrame) then begin
	  L := ColorToRGB(clWindowFrame);
  	H := ColorToRGB(clWindow);
	end
  else begin
	  H := ColorToRGB(clWindowFrame);
  	L := ColorToRGB(clWindow);
	end;
  if AverageColor(Background) > 128 then
  	Result := L
	else
  	Result := H;
end;

function CalculateCaptionSize(DC: HDC; const Text: string): TSize; inline;
begin
  FillChar(Result, SizeOf(TSize), #0);
  GetTextExtentPoint32(DC, PChar(Text), Length(Text), Result);
end;

function GetPoint(X, Y: Integer): TPoint; inline;
begin
  Result.X := X;
  Result.Y := Y;
end;

function GetRect(ALeft, ATop, ARight, ABottom: Integer): TRect; inline;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ARight;
  Result.Bottom := ABottom;
end;

function HeightOf(const Rect: TRect): Integer; inline;
begin
  Result := Rect.Bottom - Rect.Top;
end;

function WidthOf(const Rect: TRect): Integer; inline;
begin
  Result := Rect.Right - Rect.Left;
end;

function CreateFastBitmap(Width, Height: Integer): TFastBitmap; inline;
var
  BitmapInfo: TBitmapinfo;
begin
  Result.DC := CreateCompatibleDC(0);
  FillChar(BitmapInfo, SizeOf(BitmapInfo), #0);
  with BitmapInfo.bmiHeader do begin
    biSize := SizeOf(BitmapInfo.bmiHeader);
    biWidth := Width;
    biHeight := Height;
    biPlanes := 1;
    biBitCount := 24;
    biCompression := BI_RGB;
  end;
  with Result do
    Handle := CreateDIBSection(DC, BitmapInfo, DIB_RGB_COLORS, Bits, 0, 0);
  Result.Width := Width;
  Result.Height := Height;
  with Result do
    SelectObject(DC, Handle);
end;

procedure DestroyFastBitmap(const Bitmap: TFastBitmap); inline;
begin
  DeleteDC(Bitmap.DC);
  DeleteObject(Bitmap.Handle);
end;

//function GetDetails(Widget: TThemedButton): TThemedElementDetails; overload;
//var
//  Base: Integer;
//begin
//  Result.Element := teButton;
//  with Result do begin
//    case Widget of
//      tbPushButtonNormal..tbPushButtonDefaulted: begin
//        Part := BP_PUSHBUTTON;
//        Base := Ord(tbPushButtonNormal);
//      end;
//      tbRadioButtonUncheckedNormal..tbRadioButtonCheckedDisabled: begin
//        Part := BP_RADIOBUTTON;
//        Base := Ord(tbRadioButtonUncheckedNormal);
//      end;
//      tbCheckBoxUncheckedNormal..tbCheckBoxMixedDisabled: begin
//        Part := BP_CHECKBOX;
//        Base := Ord(tbCheckBoxUncheckedNormal);
//      end;
//      tbGroupBoxNormal..tbGroupBoxDisabled: begin
//        Part := BP_GROUPBOX;
//        Base := Ord(tbGroupBoxNormal);
//      end;
//      tbUserButton: begin
//        Part := BP_USERBUTTON;
//        Base := Ord(tbUserButton);
//      end;
//    else
//      Part := 0;
//      Base := 0;
//    end;
//    State := Ord(Widget) - Base + 1;
//  end;
//end;

function GetDetails(Widget: TThemedScrollBar): TThemedElementDetails; overload; inline;
var
  Base: Integer;
begin
  Result.Element := teScrollBar;
  with Result do begin
    case Widget of
      tsArrowBtnUpNormal..tsArrowBtnRightDisabled: begin
        Part := SBP_ARROWBTN;
        Base := Ord(tsArrowBtnUpNormal);
      end;
      tsThumbBtnHorzNormal..tsThumbBtnHorzDisabled: begin
        Part := SBP_THUMBBTNHORZ;
        Base := Ord(tsThumbBtnHorzNormal);
      end;
      tsThumbBtnVertNormal..tsThumbBtnVertDisabled: begin
        Part := SBP_THUMBBTNVERT;
        Base := Ord(tsThumbBtnVertNormal);
      end;
      tsLowerTrackHorzNormal..tsLowerTrackHorzDisabled: begin
        Part := SBP_LOWERTRACKHORZ;
        Base := Ord(tsLowerTrackHorzNormal);
      end;
      tsUpperTrackHorzNormal..tsUpperTrackHorzDisabled: begin
        Part := SBP_UPPERTRACKHORZ;
        Base := Ord(tsUpperTrackHorzNormal);
      end;
      tsLowerTrackVertNormal..tsLowerTrackVertDisabled: begin
        Part := SBP_LOWERTRACKVERT;
        Base := Ord(tsLowerTrackVertNormal);
      end;
      tsUpperTrackVertNormal..tsUpperTrackVertDisabled: begin
        Part := SBP_UPPERTRACKVERT;
        Base := Ord(tsUpperTrackVertNormal);
      end;
      tsGripperHorzNormal..tsGripperHorzDisabled: begin
        Part := SBP_GRIPPERHORZ;
        Base := Ord(tsGripperHorzNormal);
      end;
      tsGripperVertNormal..tsGripperVertDisabled: begin
        Part := SBP_GRIPPERVERT;
        Base := Ord(tsGripperVertNormal);
      end;
      tsSizeBoxRightAlign..tsSizeBoxLeftAlign: begin
        Part := SBP_SIZEBOX;
        Base := Ord(tsSizeBoxRightAlign);
      end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function GetDetails(Widget: TThemedComboBox): TThemedElementDetails; overload; inline;
var
  Base: Integer;
begin
  Result.Element := teComboBox;
  with Result do begin
    case Widget of
      tcDropDownButtonNormal..tcDropDownButtonDisabled: begin
        Part := CP_DROPDOWNBUTTON;
        Base := Ord(tcDropDownButtonNormal);
      end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function GetDetails(Widget: TThemedHeader): TThemedElementDetails; overload; inline;
var
  Base: Integer;
begin
  Result.Element := teHeader;
  with Result do begin
    case Widget of
      thHeaderItemNormal..thHeaderItemPressed: begin
        Part := HP_HEADERITEM;
        Base := Ord(thHeaderItemNormal);
      end;
      thHeaderItemLeftNormal..thHeaderItemLeftPressed: begin
        Part := HP_HEADERITEMLEFT;
        Base := Ord(thHeaderItemLeftNormal);
      end;
      thHeaderItemRightNormal..thHeaderItemRightPressed: begin
        Part := HP_HEADERITEMRIGHT;
        Base := Ord(thHeaderItemRightNormal);
      end;
      thHeaderSortArrowSortedUp..thHeaderSortArrowSortedDown: begin
        Part := HP_HEADERSORTARROW;
        Base := Ord(thHeaderSortArrowSortedUp);
      end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function GetDetails(Widget: TThemedToolBar): TThemedElementDetails; overload; inline;
var
  Base: Integer;
begin
  Result.Element := teToolBar;
  with Result do begin
    case Widget of
      ttbButtonNormal..ttbButtonCheckedHot: begin
        Part := TP_BUTTON;
        Base := Ord(ttbButtonNormal);
      end;
      ttbDropDownButtonNormal..ttbDropDownButtonCheckedHot: begin
        Part := TP_DROPDOWNBUTTON;
        Base := Ord(ttbDropDownButtonNormal);
      end;
      ttbSplitButtonNormal..ttbSplitButtonCheckedHot: begin
        Part := TP_SPLITBUTTON;
        Base := Ord(ttbSplitButtonNormal);
      end;
      ttbSplitButtonDropDownNormal..ttbSplitButtonDropDownCheckedHot: begin
        Part := TP_SPLITBUTTONDROPDOWN;
        Base := Ord(ttbSplitButtonDropDownNormal);
      end;
      ttbSeparatorNormal..ttbSeparatorCheckedHot: begin
        Part := TP_SEPARATOR;
        Base := Ord(ttbSeparatorNormal);
      end;
      ttbSeparatorVertNormal..ttbSeparatorVertCheckedHot: begin
        Part := TP_SEPARATORVERT;
        Base := Ord(ttbSeparatorVertNormal);
      end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

procedure DrawScroll(DC: HDC; Theme: TThemedScrollBar; Rect: TRect); inline;
var
 W, H, X, Y: Integer;
 A: TFastBitmap;
begin
  W := WidthOf(Rect);
  H := HeightOf(Rect);
  if (W < 1) or (H < 1) then
    Exit;
  X := Rect.Left;
  Y := Rect.Top;
  OffsetRect(Rect, -X, -Y);
  A := CreateFastBitmap(W, H);
  StyleServices.DrawElement(A.DC, GetDetails(Theme), Rect);
  StretchBlt(A.DC, 3, 3, W - 6, H - 6, A.DC, 3, 3, 1, H - 6, SRCCOPY);
  BitBlt(DC, X, Y, W, H, A.DC, 0, 0, SRCCOPY);
  DestroyFastBitmap(A);
end;

procedure DrawThemeScroll(DC: HDC; Direction: TDirection; const Rect: TRect; State: TDrawState); inline;
var
	Theme: TThemedScrollBar;
  FrameState: Cardinal;
begin
  if StyleServices.Enabled then begin
  	Theme := tsArrowBtnLeftNormal;
  	case Direction of
      drLeft: Theme := tsArrowBtnLeftNormal;
			drUp: Theme := tsArrowBtnUpNormal;
      drRight: Theme := tsArrowBtnRightNormal;
      drDown: Theme := tsArrowBtnDownNormal;
      drCenter: Theme := tsArrowBtnDownNormal;
		end;
    if dsDisabled in State then
			Inc(Theme, 3)
    else if dsPressed in State then
			Inc(Theme, 2)
    else if dsHot in State then
			Inc(Theme, 1);
    if Direction = drCenter then
      DrawScroll(DC, Theme, Rect)
    else
    	StyleServices.DrawElement(DC, GetDetails(Theme), Rect);
  end
  else begin
		FrameState := 0;
  	case Direction of
      drLeft: FrameState := DFCS_SCROLLLEFT;
			drUp: FrameState := DFCS_SCROLLUP;
      drRight: FrameState := DFCS_SCROLLRIGHT;
      drDown: FrameState := DFCS_SCROLLDOWN;
      drCenter: FrameState := DFCS_BUTTONPUSH;
		end;
    if dsThin in State then
			FrameState := FrameState or DFCS_FLAT;
    if dsDisabled in State then
			FrameState := FrameState or DFCS_INACTIVE
    else if dsPressed in State then
			FrameState := FrameState or DFCS_PUSHED;
    if Direction = drCenter then
	    DrawFrameControl(DC, Rect, DFC_BUTTON, FrameState)
    else
	    DrawFrameControl(DC, Rect, DFC_SCROLL, FrameState);
  end;
end;

procedure OverwriteObject(DC: HDC; Obj: HGDIOBJ); inline;
begin
  DeleteObject(SelectObject(DC, Obj));
end;

procedure SelectClipRect(DC: HDC; const Rect: TRect; Mode: Integer); inline;
var
  Region: HRGN;
begin
  with Rect do
    Region := CreateRectRgn(Left, Top, Right, Bottom);
  ExtSelectClipRgn(DC, Region, Mode);
  DeleteObject(Region);
end;

procedure DrawFrame(DC: HDC; Rect: TRect; State: TDrawFrameState); inline;
var
  PriorPen: HPEN;
  PriorPoint: TPoint;
begin
  if State = dfFlat then
    Exit;
  with Rect do begin
    Dec(Right);
    Dec(Bottom);
    MoveToEx(DC, Left, Top, @PriorPoint);
    PriorPen := SelectObject(DC, CreatePen(PS_SOLID, 0,
      GetSysColor(COLOR_BTNFACE)));
    case State of
      dfFocus: begin
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_WINDOWFRAME)));
        LineTo(DC, Right, Top);
        LineTo(DC, Right, Bottom);
        LineTo(DC, Left, Bottom);
        LineTo(DC, Left, Top);
      end;
      dfFramed: begin
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_3DHILIGHT)));
        LineTo(DC, Right, Top);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_3DDKSHADOW)));
        LineTo(DC, Right, Bottom);
        LineTo(DC, Left, Bottom);
        LineTo(DC, Left, Bottom - 1);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_3DHILIGHT)));
        LineTo(DC, Left, Top);
        InflateRect(Rect, -1, -1);
        MoveToEx(DC, Left, Top, Nil);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_3DLIGHT)));
        LineTo(DC, Right, Top);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_BTNSHADOW)));
        LineTo(DC, Right, Bottom);
        LineTo(DC, Left, Bottom);
        LineTo(DC, Left, Bottom - 1);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_3DLIGHT)));
        LineTo(DC, Left, Top);
      end;
     dfHover: begin
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_BTNFACE)));
        LineTo(DC, Right, Top);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_3DDKSHADOW)));
        LineTo(DC, Right, Bottom);
        LineTo(DC, Left, Bottom);
        LineTo(DC, Left, Bottom - 1);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_BTNFACE)));
        LineTo(DC, Left, Top);
      end;
     dfRaised: begin
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_3DHILIGHT)));
        LineTo(DC, Right, Top);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_BTNSHADOW)));
        LineTo(DC, Right, Bottom);
        LineTo(DC, Left, Bottom);
        LineTo(DC, Left, Bottom - 1);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_3DHILIGHT)));
        LineTo(DC, Left, Top);
      end;
     dfLowered: begin
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_BTNSHADOW)));
        LineTo(DC, Right, Top);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_3DHILIGHT)));
        LineTo(DC, Right, Bottom);
        LineTo(DC, Left, Bottom);
        LineTo(DC, Left, Bottom - 1);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_BTNSHADOW)));
        LineTo(DC, Left, Top);
      end;
     dfPressed: begin
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_3DDKSHADOW)));
        LineTo(DC, Right, Top);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_BTNFACE)));
        LineTo(DC, Right, Bottom);
        LineTo(DC, Left, Bottom);
        LineTo(DC, Left, Bottom - 1);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_3DDKSHADOW)));
        LineTo(DC, Left, Top);
      end;
     dfSunken: begin
        MoveToEx(DC, Left, Bottom, Nil);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_BTNSHADOW)));
        LineTo(DC, Left, Top);
        LineTo(DC, Right, Top);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_3DHILIGHT)));
        LineTo(DC, Right, Bottom);
        LineTo(DC, Left, Bottom);
        LineTo(DC, Left, Bottom - 1);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_BTNSHADOW)));
        LineTo(DC, Left, Top);
        InflateRect(Rect, -1, -1);
        MoveToEx(DC, Left, Bottom, Nil);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_3DDKSHADOW)));
        LineTo(DC, Left, Top);
        LineTo(DC, Right, Top);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_3DLIGHT)));
        LineTo(DC, Right, Bottom);
        LineTo(DC, Left, Bottom);
        LineTo(DC, Left, Bottom - 1);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_BTNSHADOW)));
     end;
     dfPushed: begin
        InflateRect(Rect, -1, -1);
        MoveToEx(DC, Left, Top, Nil);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_BTNSHADOW)));
        LineTo(DC, Right, Top);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_3DLIGHT)));
        LineTo(DC, Right, Bottom);
        LineTo(DC, Left, Bottom);
        LineTo(DC, Left, Bottom - 1);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_BTNSHADOW)));
        LineTo(DC, Left, Top);
        InflateRect(Rect, 1, 1);
        MoveToEx(DC, Left, Top, Nil);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_3DDKSHADOW)));
        LineTo(DC, Right, Top);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_3DHILIGHT)));
        LineTo(DC, Right, Bottom);
        LineTo(DC, Left, Bottom);
        LineTo(DC, Left, Bottom - 1);
        OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_3DDKSHADOW)));
        LineTo(DC, Left, Top);
      end;
    end;
    OverwriteObject(DC, PriorPen);
    with PriorPoint do
      MoveToEx(DC, x, y, Nil);
  end;
end;

procedure DrawThemeBorder(DC: HDC; Color: TColor; const Rect: TRect; State: TDrawState); inline;
var
	Brush: HBRUSH;
  Rgn: HRGN;
  R: TRect;
begin
  Brush := CreateSolidBrush(ColorToRGB(Color));
  FillRect(DC, Rect, Brush);
  with StyleServices do
    if Enabled then begin
      R := Rect;
      Rgn := CreateRectRgn(0, 0, 1, 1);
      if GetClipRgn(DC, Rgn) <> 1 then begin
      	DeleteObject(Rgn);
        Rgn := 0;
      end;
			ExcludeClipRect(DC, Rect.Left + 1, Rect.Top + 1, Rect.Right - 1, Rect.Bottom - 1);
      InflateRect(R, -1, -1);
      DrawElement(DC, GetDetails(tcComboBoxRoot), Rect);
      SelectClipRgn(DC, Rgn);
      if Rgn <> 0 then
      	DeleteObject(Rgn);
    end
    else if dsThin in State then
      DrawFrame(DC, Rect, dfLowered)
    else
      DrawFrame(DC, Rect, dfSunken);
  Brush := SelectObject(DC, Brush);
  {if (Rect.Left + 3 < Rect.Right) and (Rect.Top + 3 < Rect.Bottom) then
	  ExtFloodFill(DC, Rect.Top + 3, Rect.Left + 3, GetSysColor(COLOR_WINDOW),
    	FLOODFILLSURFACE);}
	OverwriteObject(DC, Brush);
end;

function GetBrush(Bitmap: TBitmap): HBRUSH; inline;
var
  LogBrush: TLogBrush;
begin
  LogBrush.lbStyle := BS_PATTERN;
  LogBrush.lbColor := 0;
  LogBrush.lbHatch := Bitmap.Handle;
  Result := CreateBrushIndirect(LogBrush);
end;

procedure DrawCaption(const Canvas: TCanvas; const Caption: String; Rect: TRect; Direction: TDirection; Enabled: Boolean = True); inline;
var
  DC: HDC;
  DrawRect: TRect;
  PriorMode: Integer;
  PriorColor: COLORREF;
begin
  DC := Canvas.Handle;
  DrawRect := Rect;
  PriorMode := SetBkMode(DC, TRANSPARENT);
  if Enabled then
    DrawText(DC, PChar(Caption), -1, DrawRect, Directions[Direction])
  else begin
    OffsetRect(DrawRect, 1, 1);
    PriorColor := Canvas.Font.Color;
    Canvas.Font.Color := clBtnHighlight;
    DrawText(DC, PChar(Caption), -1, DrawRect, Directions[Direction]);
    OffsetRect(DrawRect, -1, -1);
    Canvas.Font.Color := clGrayText;
    DrawText(DC, PChar(Caption), -1, DrawRect, Directions[Direction]);
    Canvas.Font.Color := PriorColor;
  end;
  SetBkMode(DC, PriorMode);
end;

procedure DrawThemeThinButton(DC: HDC; const Rect: TRect; State: TDrawState); inline;
var
  Theme: TThemedToolBar;
begin
  if StyleServices.Enabled then begin
    Theme := ttbButtonNormal;
    if dsDisabled in State then
      Theme := ttbButtonDisabled
    else if dsPressed in State then
      Theme := ttbButtonPressed
    else if dsHot in State then
      Theme := ttbButtonHot;
    StyleServices.DrawElement(DC, GetDetails(Theme), Rect);
  end
  else begin
		if dsDisabled in State then
      Exit;
    if dsPressed in State then
	    DrawFrame(DC, Rect, dfLowered)
    else if dsHot in State then
	    DrawFrame(DC, Rect, dfRaised);
  end;
end;

function GetBitmap(ForeColor: TColor; BackColor: TColor): TBitmap; inline;
var
  PixelColors: array[Boolean] of TColor;
  Col: Integer;
  Row: Integer;
begin
  PixelColors[False] := ForeColor;
  PixelColors[True] := BackColor;
  Result := TBitmap.Create;
  with Result do begin
    Height := 8;
    Width := 8;
    for Col := 0 to Width - 1 do
      for Row := 0 to Height - 1 do
        Canvas.Pixels[Col, Row] := PixelColors[Odd(Col + Row)];
    HandleType := bmDDB;
  end;
end;

procedure DrawBorder(const Canvas: TCanvas; R: TRect; Color: TColor; Thickness: Integer; const Overlay: Boolean = False);
var
  TL, BR: Byte;
  spm: TPenMode;
begin
  if Thickness > 0 then begin
    TL := Thickness div 2;
    if Thickness mod 2 = 0 then
      BR := TL - 1
    else
      BR := TL;

    Canvas.Pen.Color := Color;
    Canvas.Pen.Width := Thickness;
    spm:=pmBlack; // satisfy compiler
    if Overlay then begin
      spm:=Canvas.Pen.Mode; // remember old state
      Canvas.Pen.Mode:=pmXor;
    end;
    //
    // for this rectangle the coordinates are relative to top/left, but for windows it doesn't matter, it draws backwards if it must
    Canvas.Rectangle(Rect(R.Left + TL, R.Top + TL, R.Left + R.Width - BR, R.Top + R.Height - BR));
    //
    if Overlay then
      Canvas.Pen.Mode:=spm; // restore old state
  end;
end;

// up arrow
//   | 0 1 2 X
// -----------
// 0 |   x
// 1 | x   x
// 2 |
// Y |

// down arrow
//   | 0 1 2 X
// -----------
// 0 | x   x
// 1 |   x
// 2 |
// Y |

// left arrow
//   | 0 1 2 X
// -----------
// 0 |   x
// 1 | x
// 2 |   x
// Y |

// right arrow
//   | 0 1 2 X
// -----------
// 0 | x
// 1 |   x
// 2 | x
// Y |

procedure DrawArrow(const Canvas: TCanvas; const R: TRect; ArrowDirection: TDrawArrowDirection; DrawState: TDrawState; Thickness, Size: Integer);
const
  // array points transformed by -1
  ArrowPoints: Array[TDrawArrowDirection, 0..2] of TPoint = (
    ((X: -1; Y:  0), (X:  0; Y: -1), (X:  1; Y:  0)), // up
    ((X: -1; Y: -1), (X:  0; Y:  0), (X:  1; Y: -1)), // down
    ((X:  0; Y: -1), (X: -1; Y:  0), (X:  0; Y:  1)), // left
    ((X: -1; Y: -1), (X:  0; Y:  0), (X: -1; Y:  1))  // right
  );
var
  CX, CY, Offset: Integer;
  I, HalfSize: Integer;
  P: Array[0..2] of TPoint;
begin
  // Clamp values
  if Thickness < 1 then
    Thickness := 1;

  if Size < 2 then
    Size := 2;

  HalfSize := Round(Size * 0.5);
  Offset := Round(Size / 0.5) div 5;

  // Center of drawing rect
  CX := (R.Left + R.Right) div 2;
  CY := (R.Top + R.Bottom) div 2 - Thickness div 2;

  // Pen setup
  Canvas.Pen.Color := clWindow;
  if dsSelected in DrawState then
    Canvas.Pen.Color := clWindowText
  else if dsHot in DrawState then
    Canvas.Pen.Color := clCaptionText;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := Thickness;

  // Build arrow geometry
  for I := 0 to 2 do begin
    P[I] := Point(CX + ArrowPoints[ArrowDirection, I].X * Size, CY + ArrowPoints[ArrowDirection, I].Y * Size);
    if (ArrowDirection = daUp) or (ArrowDirection = daDown) then
      P[I].Offset(-Offset, HalfSize)
    else if ArrowDirection = daLeft then
      P[I].Offset(HalfSize - Offset, 0)
    else if ArrowDirection = daRight then
      P[I].Offset(HalfSize, 0);
  end;

  Canvas.Polyline(P);

  // Fixups
  if Thickness = 1 then begin
    Canvas.Pixels[P[2].X, P[2].Y] := Canvas.Pen.Color;
  end
  else begin
    if ArrowDirection = daDown then
      Canvas.Pixels[P[1].X, P[1].Y + 1] := Canvas.Pen.Color
    else if ArrowDirection > daDown then
      Canvas.Pixels[P[2].X, P[2].Y] := Canvas.Pen.Color;
  end;
end;

{ TEntryItem }

constructor TEntryItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FIndex := -1;
  FLevel := -1;
  FCaption := '';
  FEnabled := True;
  FVisible := True;
  FImageIndex := -1;
  FEntryState := esCollapsed;
  FItems := TEntryItems.Create(TEntryItems(Collection).Control, Self);
  FData := Nil;
  FDataObject := False;
  FUserFreeProc := Nil;
  FParentEntries := Nil;
end;

destructor TEntryItem.Destroy;
begin
  if FDataObject and (FData <> Nil) then
    TObject(FData).Free
  else if (FData <> Nil) and Assigned(FUserFreeProc) then
    FUserFreeProc(FData);
  FItems.Free;
  FParentEntries := Nil;
  FCaption := '';
  inherited Destroy;
end;

procedure TEntryItem.Assign(Source: TPersistent);
var
  Item: TEntryItem;
begin
  if Source is TEntryItem then begin
    Item := TEntryItem(Source);
    //
    FIndex := Item.TreeIndex;
    FLevel := Item.TreeLevel;
    FCaption := Item.Caption;
    FEnabled := Item.Enabled;
    FVisible := Item.Visible;
    FImageIndex := Item.ImageIndex;
    FEntryState := Item.State;
    FItems.Assign(Item.Items);
    FData := Item.Data;
    FDataObject := Item.DataObject;
    FUserFreeProc := Item.UserFreeProc;
//    FParentEntries := ; // ???
  end
  else
    inherited Assign(Source);
end;

procedure TEntryItem.Click;
begin
  (Collection as TEntryItems).Control.Perform(CN_ITEMCLICK, Integer(Self), 0);
end;

procedure TEntryItem.Collapse;
begin
  if FEntryState = esExpanded then begin
    FEntryState := esCollapsed;
    Changed(False);
  end;
end;

procedure TEntryItem.Expand;
begin
  if FEntryState = esCollapsed then begin
    FEntryState := esExpanded;
    Changed(False);
  end;
end;

procedure TEntryItem.SetCaption(const Value: string);
begin
  if Value <> FCaption then begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TEntryItem.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then begin
    FEnabled := Value;
    Changed(False);
  end;
end;

procedure TEntryItem.SetEntryState(Value: TEntryStateEnum);
begin
  if FEntryState <> Value then begin
    FEntryState := Value;
    Changed(False);
  end;
end;

function TEntryItem.GetDisplayRect(const AbsoluteOrRelative: TEntryItemPositionEnum): TRect;
var
  View: TCustomEntryView;
  TopIndex,
  ItemIndex: Integer;
  ControlDetails: TEntryItemDetails;
  VisibleIndex: Integer;
  I: Integer;
  Item: TEntryItem;
begin
  if Collection = Nil then begin
    SetRectEmpty(Result);
    Exit;
  end;
  // this is getting parent render control info for us, to know what are the parameters to use while calculating entry position
  View := TCustomEntryView((Collection as TEntryItems).Control);
  if View.Perform(CM_ITEMDETAILS, Integer(@ControlDetails), 0) = 0 then begin // technically not possible, but ... to be sure
    SetRectEmpty(Result);
    Exit;
  end;
  //
  //Result := ParentEntries.DisplayRect(Self); // this calculates absolute position ??? why ???
  Result.Top := ControlDetails.EntryHeight * (FIndex - IfThen(AbsoluteOrRelative = epRelative, ControlDetails.TopIndex, 0));
  Result.Left := ControlDetails.ClientRect.Left;
  Result.Bottom := Result.Top + ControlDetails.EntryHeight;
  Result.Right := ControlDetails.ClientRect.Right;
  // correct for borders
  if ControlDetails.BorderStyle = bsSingle then begin
    InflateRect(Result, -GetBorder, 0);
    if (ControlDetails.Selected <> Nil) and (Self.TreeIndex > ControlDetails.Selected.TreeIndex) then
      OffsetRect(Result, 0, -GetBorder)
    else
      OffsetRect(Result, 0, GetBorder);
  end;
end;

function TEntryItem.AbsoluteDisplayRect: TRect;
begin
  Result := GetDisplayRect(epAbsolute);
end;

function TEntryItem.RelativeDisplayRect: TRect;
begin
  Result := GetDisplayRect(epRelative);
end;

function TEntryItem.HasChildren: Boolean;
begin
  Result := Items.Count > 0;
end;

function TEntryItem.IsExpanded: Boolean;
begin
  Result := State = esExpanded;
end;

function TEntryItem.ParentIsVisible: Boolean;
var
  Parent: TEntryItem;
begin
  Result := False;
  if (ParentEntries = Nil) or (ParentEntries.OwnerEntry = Nil) then
    Exit;
  //
  Parent := ParentEntries.OwnerEntry;
  Result := Parent.Visible;
end;

function TEntryItem.ParentHasChildren: Boolean;
var
  Parent: TEntryItem;
begin
  Result := False;
  if (ParentEntries = Nil) or (ParentEntries.OwnerEntry = Nil) then
    Exit;
  //
  Parent := ParentEntries.OwnerEntry;
  Result := Parent.HasChildren;
end;

function TEntryItem.ParentIsExpanded: Boolean;
var
  Parent: TEntryItem;
begin
  Result := False;
  if (ParentEntries = Nil) or (ParentEntries.OwnerEntry = Nil) then
    Exit;
  //
  Parent := ParentEntries.OwnerEntry;
  Result := Parent.Visible and Parent.HasChildren and Parent.IsExpanded;
end;

//function TEntryItem.GetNearestTop: Integer;
//var
//  I: Integer;
//begin
//  Result := 0;
//  for I := 0 to Items.Count - 1 do
//    if Items[I].Visible then
//      if Result = TCustomEntryView(TEntryItems(Collection).Control).FTopIndex then begin
//        Result := I;
//        Break;
//      end
//      else
//        Inc(Result);
//end;

procedure TEntryItem.SetImageIndex(Value: Integer);
begin
  if Value <> FImageIndex then begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

procedure TEntryItem.SetItems(Value: TEntryItems);
begin
  if FItems <> Value then begin
    FItems.Assign(Value);
    Changed(False);
  end;
end;

procedure TEntryItem.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then begin
    FVisible := Value;
    Changed(False);
  end;
end;

function TEntryItem.GetVisible: Boolean;
begin
  Result := False;
  if ParentEntries = Nil then
    Exit;
  //
  Result := (ParentIsExpanded or (ParentEntries.OwnerEntry = Nil)) and FVisible;
end;

{ TEntryItems }

constructor TEntryItems.Create(Control: TControl; OwnerEntry: TEntryItem);
begin
  inherited Create(TEntryItem);
  FControl := Control;
  FOwnerEntry := OwnerEntry;
end;

procedure TEntryItems.Update(Item: TCollectionItem);
begin
  if TCustomEntryView(FControl).Entries.UpdateCount > 0 then
    Exit;
  //
  if FOwnerEntry <> Nil then // propagate up
    TEntryItems(FOwnerEntry.Collection).Update(Item)
  else begin
    TCustomEntryView(FControl).EntriesChanged;
    FControl.Invalidate;
  end;
end;

function TEntryItems.Get(Index: Integer): TEntryItem;
begin
  Result := GetItem(Index) as TEntryItem;
end;

function TEntryItems.GetTreeIndex(Entry: TEntryItem): Integer;
begin
  Result := 0;
  Entry := GetPrevEntry(Entry);
  if Entry <> Nil then
    Result := Entry.TreeIndex + 1;
end;

function TEntryItems.GetTreeLevel(Entry: TEntryItem): Integer;
var
  Parent: TEntryItems;
begin
  Result := 0;
  if Entry = Nil then
    Exit;
  //
  Parent := Entry.ParentEntries;
  while Parent <> Nil do begin
    if Parent.OwnerEntry = Nil then
      Exit;
    //
    Parent := Parent.OwnerEntry.ParentEntries;
    if Parent <> Nil then
      Inc(Result);
  end;
end;

function TEntryItems.GetFirstEntry(const Entry: TEntryItem): TEntryItem;
begin
  Result := Entry;
  if Entry = Nil then
    Exit;

  while Assigned(Result.Items) and (Result.Items.Count > 0) do
    Result := Result.Items[0];
end;

function TEntryItems.GetLastEntry(const Entry: TEntryItem): TEntryItem;
begin
  Result := Entry;

  while Assigned(Result.Items) and (Result.Items.Count > 0) do
    Result := Result.Items[Result.Items.Count - 1];
end;

function TEntryItems.GetPrevEntry(Entry: TEntryItem): TEntryItem;
var
  PrevSibling: TEntryItem;
  Parent: TEntryItem;
begin
  Result := Nil;
  if Entry = Nil then
    Exit;

  // go up
  if Entry.Collection <> Nil then
    Parent := TEntryItems(Entry.Collection).OwnerEntry
  else
    Parent := Nil;

  if (Parent <> Nil) and (Entry.Index > 0) then
    Parent := Parent.Items[Entry.Index - 1] // go up
  else begin
    // try previous sibling
    PrevSibling := GetPrevEntrySibling(Entry);
    if PrevSibling <> Nil then begin
      // go to its deepest last child
      Exit(GetLastEntry(PrevSibling));
    end;
  end;

  Result := Parent;
end;

function TEntryItems.GetNextEntry(Entry: TEntryItem): TEntryItem;
var
  Items: TEntryItems;
begin
  Result := Nil;
  if Entry = Nil then
    Exit;

  // go down
  Result := GetFirstEntrySibling(Entry);
  if Result <> Nil then
    Exit;

  // go right or up-right
  while Entry <> Nil do begin
    Result := GetNextEntrySibling(Entry);
    if Result <> Nil then
      Exit;

    if Entry.Items.Count > 0 then
      Entry := Entry.Items[0] // go down
    else
      Entry := Nil;
  end;

  Result := Nil;
end;

function TEntryItems.GetFirstEntrySibling(const Entry: TEntryItem): TEntryItem;
begin
  Result := Nil;
  if Entry = Nil then
    Exit;

  if Entry.Items.Count > 0 then
    Exit(Entry.Items[0]);
end;

function TEntryItems.GetLastEntrySibling(const Entry: TEntryItem): TEntryItem;
begin
  Result := Nil;
  if Entry = Nil then
    Exit;

  if Entry.Items.Count > 0 then
    Exit(Entry.Items[Entry.Items.Count - 1]);
end;

function TEntryItems.GetPrevEntrySibling(const Entry: TEntryItem): TEntryItem;
var
  Items: TEntryItems;
  Index: Integer;
begin
  Result := Nil;
  if Entry = Nil then
    Exit;

  //Items := Entry.Items;
  Items := Entry.ParentEntries;
  Index := Entry.Index;

  if (Index > 0) and (Items <> Nil) and (Items.Count > 0) then
    Result := Items[Index - 1];
end;

function TEntryItems.GetNextEntrySibling(const Entry: TEntryItem): TEntryItem;
var
  Parent: TEntryItems;
  Items: TEntryItems;
  Index: Integer;
begin
  Result := Nil;

  Parent := Entry.ParentEntries; // careful: depends on your design

  if Parent <> Nil then
    Items := Parent
  else
    Exit; // root-level handling needed

  Index := Entry.Index; // TCollectionItem gives me this

  if Index < Items.Count - 1 then
    Result := Items[Index + 1];
end;

(*
function TEntryItems.DisplayRect(const Entry: TEntryItem): TRect;
var
  View: TCustomEntryView;
//  Entry,
  Item: TEntryItem;
  ControlDetails: TEntryItemDetails;
  Rect: TRect;
  I: Integer;
begin
  View := TCustomEntryView(Control);
  if View.Perform(CM_ITEMDETAILS, Integer(@ControlDetails), 0) = 0 then begin // technically not possible
    SetRectEmpty(Result);
    Exit;
  end;
//  Entry := Self.OwnerEntry;
//  if (Entry = Nil) and (Count > 0) then
//    Entry := Items[0];
  if Assigned(Entry) and Entry.Visible then begin
    // calculate absolute position
    Result := ControlDetails.ClientRect;
    // correct for borders
    if ControlDetails.BorderStyle = bsSingle then begin
      InflateRect(Result, -GetBorder, 0);
      if (ControlDetails.Selected <> Nil) and (Entry.Index > ControlDetails.Selected.Index) then
        OffsetRect(Result, 0, -GetBorder)
      else
        OffsetRect(Result, 0, GetBorder);
    end;
    //
    // we count how many entries before current entry is there, and we need to check if those entries are inside rendering rectangle
    I := 0;
    Item := Entry;
    repeat
      Item := View.GetPrevEntry(Item); // go up
      // check if item is visible and inside ClientRect
      if (Item <> Nil) and Item.Visible and RectInRect(Item.RelativeDisplayRect, ControlDetails.ClientRect) then
        Inc(I);
      // this condition maybe needed later
      //else if Item.State = esCollapsed then
    until Item = Nil;
    //Dec(I);
    //
    // and here we have our absolute position set, this is probably ok, but something with I variable is now
    Result.Top := I * ControlDetails.EntryHeight;
    Result.Bottom := Result.Top + ControlDetails.EntryHeight;


// @TODO: delete this
    // now correct for relative position
//    Result.Top := Result.Top - ControlDetails.EntryHeight * ControlDetails.TopIndex;
//    Result.Bottom := Result.Bottom - ControlDetails.EntryHeight * ControlDetails.TopIndex;




//    if (ControlDetails.Selected <> Nil) and (Entry.Index > ControlDetails.Selected.Index) then
//      Result.Top := Result.Bottom - ControlDetails.EntryHeight * (Count - Entry.Index)
//    else
//      Result.Top := Entry.Index * ControlDetails.EntryHeight;
//    Result.Bottom := Result.Top + ControlDetails.EntryHeight;
//    for I := Entry.Index - 1 downto 0 do begin
//      Item := View.ItemFromIndex(I);
//      if Item.Visible then begin
//        Rect := Item.DisplayRect;
//        if Result.Top < Rect.Bottom then
//          OffsetRect(Result, 0, Rect.Bottom - Result.Top)
//        else if (ControlDetails.Selected = Nil) or (Entry.Index <= ControlDetails.Selected.Index) then
//          OffsetRect(Result, 0, Rect.Bottom - Result.Top);
//        Break;
//      end
//      else if I = 0 then
//        OffsetRect(Result, 0, -Result.Top);
//    end;
  end
  else
    SetRectEmpty(Result);
end;

// @TODO: this is wrong in this implementation anyway, fix this
function TEntryItems.GetNearestTop: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do begin
    if Items[I].Visible then begin
      if Result = TCustomEntryView(Control).FTopIndex then begin
        Result := I;
        Break;
      end
      else
        Inc(Result);
    end;
  end;
end;
*)
procedure TEntryItems.Put(Index: Integer; Value: TEntryItem);
begin
  SetItem(Index, Value);
end;

procedure TEntryItems.Assign(Source: TPersistent);
var
  LItems: TEntryItems;
  I: Integer;
begin
  if Source is TEntryItems then begin
    LItems := TEntryItems(Source);
    //
    BeginUpdate;
    Clear;
    for I := 0 to LItems.Count - 1 do
      Add.Assign(LItems[I]);
    EndUpdate;
  end
  else
    inherited Assign(Source);
end;

function TEntryItems.Add: TEntryItem;
begin
  Result := inherited Add as TEntryItem;
  Result.FParentEntries := Self;
  Result.FIndex := GetTreeIndex(Result);
  Result.FLevel := GetTreeLevel(Result);
end;

function TEntryItems.Insert(Index: Integer): TEntryItem;
begin
  Result := inherited Insert(Index) as TEntryItem;
end;

//{ TEntryItems }
//
//constructor TFolderBar.Create(Collection: TCollection);
//begin
//  inherited Create(Collection);
//  FVisible := True;
//  FItems := TEntryItems.Create((Collection as TFolderBars).Control, Self);
//  FImageIndex := -1;
//  FSelectedIndex := -1;
//end;
//
//procedure TFolderBar.Assign(Source: TPersistent);
//var
//  Bar: TFolderBar;
//begin
//  if Source is TFolderBar then begin
//    Bar := TFolderBar(Source);
//    //
//    Caption := Bar.Caption;
//    ImageIndex := Bar.ImageIndex;
//    SelectedIndex := Bar.SelectedIndex;
//    Visible := Bar.Visible;
//    Items.Assign(Bar.Items);
//  end
//  else
//    inherited Assign(Source);
//end;
//
//destructor TFolderBar.Destroy;
//begin
//  FItems.Free;
//  if FDataObject and (FData <> Nil) then
//    TObject(FData).Free
//  else if (FData <> Nil) and Assigned(FUserFreeProc) then
//    FUserFreeProc(FData);
//  inherited Destroy;
//end;
//
//procedure TFolderBar.SetCaption(const Value: string);
//begin
//  if Value <> FCaption then begin
//    FCaption := Value;
//    Changed(True);
//  end;
//end;
//
//function TFolderBar.GetDisplayRect: TRect;
//var
//  Folders: TFolderBars;
//  ParentControlDetails: TFolderItemDetails;
//  Rect: TRect;
//  I: Integer;
//begin
//  Folders := Collection as TFolderBars;
//  if Folders.Control.Perform(CM_ITEMDETAILS, Integer(@ParentControlDetails), 0) = 0 then begin // technically not possible
//    SetRectEmpty(Result);
//    Exit;
//  end;
//  if Visible then begin
//    // calculate absolute position
//    Result := ParentControlDetails.ClientRect;
//    if (ParentControlDetails.Selected <> Nil) and (Index > ParentControlDetails.Selected.Index) then
//      Result.Top := Result.Bottom - ParentControlDetails.FolderHeight * (Folders.Count - Index)
//    else
//      Result.Top := Index * ParentControlDetails.FolderHeight;
//    Result.Bottom := Result.Top + ParentControlDetails.FolderHeight;
//    // correct for borders
//    if ParentControlDetails.BorderStyle = bsSingle then begin
//      InflateRect(Result, -GetBorder, 0);
//      if (ParentControlDetails.Selected <> Nil) and (Index > ParentControlDetails.Selected.Index) then
//        OffsetRect(Result, 0, -GetBorder)
//      else
//        OffsetRect(Result, 0, GetBorder);
//    end;
//    for I := Index - 1 downto 0 do
//      if Folders[I].Visible then begin
//        Rect := Folders[I].DisplayRect;
//        if Result.Top < Rect.Bottom then
//          OffsetRect(Result, 0, Rect.Bottom - Result.Top)
//        else if (ParentControlDetails.Selected = Nil) or (Index <= ParentControlDetails.Selected.Index) then
//          OffsetRect(Result, 0, Rect.Bottom - Result.Top);
//        Break;
//      end
//      else if I = 0 then
//        OffsetRect(Result, 0, -Result.Top);
//    // now correct for relative position
//    Result.Top := Result.Top - ParentControlDetails.FolderHeight * ParentControlDetails.TopIndex;
//    Result.Bottom := Result.Bottom - ParentControlDetails.FolderHeight * ParentControlDetails.TopIndex;
//  end
//  else
//    SetRectEmpty(Result);
//end;
//
//procedure TFolderBar.SetImageIndex(Value: Integer);
//begin
//  if Value <> FImageIndex then begin
//    FImageIndex := Value;
//    Changed(True);
//  end;
//end;
//
//procedure TFolderBar.SetItems(Value: TEntryItems);
//begin
//  if Value <> FItems then
//    FItems.Assign(Value);
//end;
//
//function TFolderBar.GetNearestTop: Integer;
//var
//  I: Integer;
//begin
//  Result := 0;
//  for I := 0 to Items.Count - 1 do
//    if Items[I].Visible then
//      if Result = FTopIndex then begin
//        Result := I;
//        Break;
//      end
//      else
//        Inc(Result);
//end;
//
//procedure TFolderBar.SetSelectedIndex(Value: Integer);
//begin
//  if Value <> FSelectedIndex then begin
//    FSelectedIndex := Value;
//    Changed(True);
//  end;
//end;
//
//procedure TFolderBar.SetTopIndex(Value: Integer);
//begin
//  if Value > Items.Count - 1 then
//    Value := Items.Count - 1;
//  if Value < 0 then
//    Value := 0;
//  if Value <> FTopIndex then begin
//    FTopIndex := Value;
//    Changed(True);
//  end;
//end;
//
//procedure TFolderBar.SetVisible(Value: Boolean);
//begin
//  if Value <> FVisible then begin
//     FVisible := Value;
//     Changed(True);
//  end;
//end;
//
//{ TFolderBars }
//
//constructor TFolderBars.Create(Control: TControl);
//begin
//  inherited Create(TFolderBar);
//  FControl := Control;
//end;
//
//procedure TFolderBars.Assign(Source: TPersistent);
//var
//  Bars: TFolderBars absolute Source;
//  I: Integer;
//begin
//  if Source is TFolderBars then begin
//    BeginUpdate;
//    Clear;
//    for I := 0 to Bars.Count - 1 do
//      Add.Assign(Bars[I]);
//    EndUpdate;
//  end
//  else
//    inherited Assign(Source);
//end;
//
//function TFolderBars.Add: TFolderBar;
//begin
//  Result := inherited Add as TFolderBar;
//end;
//
//function TFolderBars.Insert(Index: Integer): TFolderBar;
//begin
//  Result := inherited Insert(Index) as TFolderBar;
//end;
//
//procedure TFolderBars.Update(Item: TCollectionItem);
//begin
//  inherited Update(Item);
//  if FControl <> Nil then
//    FControl.Update;
//end;
//
//function TFolderBars.Get(Index: Integer): TFolderBar;
//begin
//  if (Index > -1) and (Index < Count) then
//    Result := GetItem(Index) as TFolderBar
//  else
//    Result := Nil;
//end;
//
//procedure TFolderBars.Put(Index: Integer; Value: TFolderBar);
//begin
//  SetItem(Index, Value);
//end;

{ TCustomScrollBar }

constructor TCustomScrollBar.Create(AOwner: TComponent; AKind: TScrollBarKind);
begin
  inherited Create(AOwner);
  // inherited properties first
  ControlStyle := ControlStyle + [csOpaque];
{$IF CompilerVersion > 29}
  StyleElements := [];
{$IFEND}
  BevelEdges := [];
  BevelOuter := bvNone;
  BevelInner := bvNone;
//  FullRepaint := False;
  DoubleBuffered := True;
  ParentBackground := False;
  ParentColor := True;
  // OS scrollbar width and height
  FXYScrollSize.cx := GetSystemMetrics(SM_CXVSCROLL);
  FXYScrollSize.cy := GetSystemMetrics(SM_CYHSCROLL);
  //
  Width := FXYScrollSize.cx;
  //
  Visible := False;

  // our properties
  FRange := -1;
  FPageSize := -1;
  FPosition := -1;
  FScrollBarKind := AKind;
  FUpdating := False;
  //
  FThumbButtonColorInactive := clNone;
  FThumbButtonColorActive   := clNone;
  FThumbButtonColorPressed  := clNone;
  //
  FScrollColorInactive := $00202020; // xxBBGGRR
  FScrollColorActive   := $00404040;
  FScrollColorPressed  := $00808080;
  //
  FShowing := shHidden;
  FShowingChangeEvent := Nil;
  //
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 15;
  FTimer.OnTimer := AnimationTimer;
  FTimer.Enabled := False;
  //
  FVisibleState := False;
  FCurrentWidth := 0;
  FTargetWidth := Width;
  FCurrentAlpha := 0;
  FTargetAlpha := 0;
  FThinWidth := 1;
  FExpandedWidth := Width;
end;

destructor TCustomScrollBar.Destroy;
begin
  FTimer.OnTimer := Nil;
  FTimer.Enabled := False;
  FTimer.Free;
  inherited;
end;

procedure TCustomScrollBar.SetRange(const Value: Integer);
begin
  if Value <= 0 then
    Exit;
  //
  if FRange <> Value then begin
    FRange := Value;

    // Clamp current position
    FPosition := EnsureRange(FPosition, 0, GetMaxPosition);

    if not FUpdating then
      Invalidate;
  end;
end;

procedure TCustomScrollBar.SetPageSize(const Value: Integer);
begin
  if FPageSize <> Value then begin
    FPageSize := Value;

    // Clamp current position
    FPosition := EnsureRange(FPosition, 0, GetMaxPosition);

    if not FUpdating then
      Invalidate;
  end;
end;

procedure TCustomScrollBar.SetPosition(const Value: Integer);
var
  NewPos: Integer;
begin
  NewPos := EnsureRange(Value, 0, GetMaxPosition);

  if FPosition <> NewPos then begin
    FPosition := NewPos;

    if not FUpdating then
      Invalidate;

    // optional:
    // if Assigned(FOnScroll) then
    //   FOnScroll(Self, FPosition);
  end;
end;

function TCustomScrollBar.GetMaxPosition: Integer;
begin
  Result := FRange - FPageSize;

  if Result < 0 then
    Result := 0;
end;

procedure TCustomScrollBar.DoShowing(const AState: TCustomScrollBarShowingEnum);
begin
  if FShowing <> AState then begin
    FShowing := AState;
    if Assigned(FShowingChangeEvent) then
      FShowingChangeEvent(Self, AState);
  end;
end;

procedure TCustomScrollBar.AnimationTimer(Sender: TObject);
begin
  // width interpolation
  FCurrentWidth := FCurrentWidth + Round((FTargetWidth - FCurrentWidth) * 0.25);

  // alpha interpolation
  FCurrentAlpha := FCurrentAlpha + Round((FTargetAlpha - FCurrentAlpha) * 0.25);


  Invalidate;

  // stop when close enough
  if Abs(FCurrentWidth - FTargetWidth) < 2 then
    FCurrentWidth := FTargetWidth;

  if Abs(FCurrentAlpha - FTargetAlpha) < 2 then
    FCurrentAlpha := FTargetAlpha;

  if (FCurrentWidth = FTargetWidth) and (FCurrentAlpha = FTargetAlpha) then begin
    FTimer.Enabled := False;
    case FShowing of
      shShowing  : DoShowing(shVisibleThin);
      shExpanding: DoShowing(shVisibleFull);
      shCollapse : DoShowing(shVisibleThin);
      shHiding   : begin
        DoShowing(shHidden);
        Visible := False;
      end;
    end;
  end;
end;

procedure TCustomScrollBar.SetInfo(const ARange, APageSize, APosition: Integer);
begin
  if (ARange < 0) or (APosition < 0) then
    Exit;
  FUpdating := True;
  try
    Range := ARange;
    PageSize := APageSize;
    Position := APosition;
  finally
    FUpdating := False;
    Invalidate;
  end;
end;

procedure TCustomScrollBar.ShowAnimated;
begin
  if not Visible and (FShowing = shHidden) then begin
    Visible := True;

    FTargetAlpha := 255;
    FTargetWidth := FThinWidth;

    FTimer.Enabled := True;
  end;
end;

procedure TCustomScrollBar.HideAnimated;
begin
  FTargetAlpha := 0;
  FTargetWidth := 1;

  FTimer.Enabled := True;
end;

procedure TCustomScrollBar.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  FTargetWidth := FExpandedWidth;
  FTimer.Enabled := True;
end;

procedure TCustomScrollBar.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FTargetWidth := FThinWidth;
  FTimer.Enabled := True;
end;

//  shHidden,
//  shShowing, // shFadeInThin,
//  shVisibleThin,
//  shExpanding,
//  shVisibleFull, //shExpanded,
//  shCollapse,
//  //shVisibleThin
//  shHiding // shFadeOut

procedure TCustomScrollBar.CMVisibleChanged(var Msg: TMessage);
begin
  if (Msg.WParam = 0) and (FShowing = shVisibleThin) then
    DoShowing(shHiding)
  else if (Msg.WParam = 1) and (FShowing = shHidden) then
    DoShowing(shShowing);
  //
  inherited;
  //
//  if (FShowing = shShowing) or (FShowing = shHiding) then begin
//    if FShowing = shHiding then
//      DoShowing(shHidden)
//    else if FShowing = shShowing then
//      DoShowing(shVisibleThin);
//  end;
end;

procedure TCustomScrollBar.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TCustomScrollBar.WMNCHitTest(var Msg: TWMNCHitTest);
//var
//  P: TPoint;
//  R: TRect;
begin
  inherited;
  // this should make entire control transparent when mouse clicking
  Msg.Result := HTTRANSPARENT;
end;

function TCustomScrollBar.CalculateThumbRect(const DrawRect: TRect; const Button: TCustomScrollBarThumbButtonEnum): TRect;
var
  ButtonSize: Integer;
  TrackTop: Integer;
  TrackBottom: Integer;
  TrackHeight: Integer;
  ThumbHeight: Integer;
  ThumbTop: Integer;
  DrawWidth: Integer;
  DrawHeight: Integer;
  AvailableTrack: Integer;
begin
  DrawWidth := DrawRect.Width;
  DrawHeight := DrawRect.Height;

  ButtonSize := FXYScrollSize.cy;

  case Button of
    tbTop: begin
      Result := Rect(
        DrawRect.Left,
        DrawRect.Top,
        DrawRect.Right,
        DrawRect.Top + ButtonSize
      );
    end;

    tbBottom: begin
      Result := Rect(
        DrawRect.Left,
        DrawRect.Bottom - ButtonSize,
        DrawRect.Right,
        DrawRect.Bottom
      );
    end;

    tbScroll: begin
        TrackTop := DrawRect.Top + ButtonSize;
        TrackBottom := DrawRect.Bottom - ButtonSize;

        TrackHeight := TrackBottom - TrackTop;

        // No scrolling needed
        if (FRange <= 0) or (FPageSize >= FRange) then begin
          Result := Rect(
            DrawRect.Left,
            TrackTop,
            DrawRect.Right,
            TrackBottom
          );
          Exit;
        end;

        // Thumb size proportional to visible content
        ThumbHeight := MulDiv(TrackHeight, FPageSize, FRange);

        // Minimum thumb size
        if ThumbHeight < 32 then
          ThumbHeight := 32;

        // Never exceed track
        if ThumbHeight > TrackHeight then
          ThumbHeight := TrackHeight;

        // Available movement range
        AvailableTrack := TrackHeight - ThumbHeight;

        // Thumb position
        ThumbTop := TrackTop + MulDiv(AvailableTrack, FPosition, GetMaxPosition);

        Result := Rect(
          DrawRect.Left,
          ThumbTop,
          DrawRect.Right,
          ThumbTop + ThumbHeight
        );
      end;
  else
    Result := Rect(0, 0, 0, 0);
  end;
end;

procedure TCustomScrollBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

procedure TCustomScrollBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

procedure TCustomScrollBar.Paint;
var
  LRect: TRect;
begin
  LRect := ClientRect;
  DrawBackground(Canvas, LRect); // clear background
  LRect.Left := Width - FTargetWidth;
  DrawButtons(Canvas, LRect); // draw buttons
end;

procedure TCustomScrollBar.DrawBackground(const ACanvas: TCanvas; var ARect: TRect);
begin
  // do we really want to paint background ??? how about transparent control
  Canvas.Brush.Style := bsSolid;
  // transparent background simulation
  Canvas.Brush.Color := TWinControlAccess(Parent).Color;
  Canvas.FillRect(ClientRect);
end;

function LerpByte(A, B: Byte; T: Single): Byte;
begin
  Result := Round(A + (B - A) * T);
end;

procedure TCustomScrollBar.DrawButtons(const ACanvas: TCanvas; const ARect: TRect);
var
  ThumbRect: TRect;
  BG, FG: COLORREF;
  R, G, B: Byte;
  T: Single;
begin
  // paint top thumb
  if (FShowing = shExpanding) or (FShowing = shVisibleFull) or (FShowing = shCollapse) then begin
    ThumbRect := CalculateThumbRect(ARect, tbTop);
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ThumbRect);
    DrawArrow(ACanvas, ThumbRect, daUp, [], 2, 4);
  end;

  // paint scroll
  ThumbRect := CalculateThumbRect(ARect, tbScroll);
  // thumb color
  BG := ColorToRGB(TWinControlAccess(Parent).Color);
  FG := ColorToRGB(clGray);

  T := FCurrentAlpha / 255.0;

  R := LerpByte(GetRValue(BG), GetRValue(FG), T);
  G := LerpByte(GetGValue(BG), GetGValue(FG), T);
  B := LerpByte(GetBValue(BG), GetBValue(FG), T);

  Canvas.Brush.Color := RGB(R, G, B);
  Canvas.FillRect(ThumbRect);

  // paint bottom thumb
  if (FShowing = shExpanding) or (FShowing = shVisibleFull) or (FShowing = shCollapse) then begin
    ThumbRect := CalculateThumbRect(ARect, tbBottom);
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ThumbRect);
    DrawArrow(ACanvas, ThumbRect, daDown, [], 2, 4);
  end;
end;

{ TCustomEntryView }

constructor TCustomEntryView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clAppWorkspace;
  DoubleBuffered := True;
  Height := 250;
  Width := 150;
  //
  FActiveIndex := -1;
  FBorderStyle := bsSingle;
  FCaptureItem := Nil;
  FEntryHeight := 25;
  FEntryImages := Nil;
  FEntries := TEntryItems.Create(Self, Nil); // no parent entry
  FHotTrack := False; // allow hot track
  FHotIndex := -1;
//  FItemImages: TCustomImageList;
//  FItemHeight: Integer;
  FMouseItem := Nil;
  FMultiSelect := False; // allow multi-select
  FSelectedCount := 0;
  FSelected := Nil; // if not multi-select, than this is set, may be usefull
  FSelectedItems := TObjectList<TEntryItem>.Create(False); // we don't own items here, just storing references
  FItemIndex := -1;
  FTextHeight := -1;
  FTopIndex := -1;
  FLocked := False;
  FLockedIndex := -1;
  FShift := [];
  FShiftIndex := -1;
  FEntriesCount := 0;
  FVisibleEntriesCount := 0;
  //
  FOverlayPicture := TPicture.Create;
  FOverlayPicture.OnChange := OverlayChange;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := ImagesChange;
  FActiveColor := $0080F080; // xxBBGGRR
  FSelectedColor := clHighlight;
  FHotColor := $00E06464;
  //
  FVerticalScrollBar := TEntryViewScrollBar.Create(Self, sbVertical);
  FVerticalScrollBar.Parent := Self;
  FVerticalScrollBar.Width := 16;
  FVerticalScrollBar.Align := alRight;
  FVerticalScrollBar.OnShowingChange := VerticalScrollBarShowingChange;
  //
  FDrawVerticalScrollBar := True;
end;

destructor TCustomEntryView.Destroy;
begin
  UpdateImages(FEntryImages, Nil);
  FSelectedItems.Clear; // explicit
  FSelectedItems.Free;
  FEntries.Free;
  FChangeLink.OnChange := Nil;
  FChangeLink.Free;
  FOverlayPicture.OnChange := Nil;
  FOverlayPicture.Free;
  FVerticalScrollBar.Free;
  inherited Destroy;
end;

procedure TCustomEntryView.DblClick;
var
  MousePoint: TPoint;
  Entry: TEntryItem;
  EntryType: TEntryItemTypeEnum;
  IsSubDirectory: Boolean;
begin
  MousePoint := Mouse.CursorPos;
  MousePoint := Self.ScreenToClient(MousePoint);
  Entry := EntryFromPoint(MousePoint.X, MousePoint.Y);
  if Entry <> Nil then begin
    if Entry.Items.Count > 0 then begin // collapse or expand as default behavior when Entry has children; so for now it can't be selected by double click
      case Entry.State of
        esCollapsed: Entry.State := esExpanded;
        esExpanded : Entry.State := esCollapsed;
      end;
    end
    else begin // Entry has no children, if it is a sub-directory - open it, else - select Entry and fire OnSelection event
      // now i can't just assume that i know what is inside Entries list, so i need to ask user to tell me what is inside
      // the user responds with:
      // - sub-directory
      // - file
      if Assigned(FOnItemGetType) then begin
        EntryType := etUnknown;
        FOnItemGetType(Self, Entry, EntryType);
        //
        case EntryType of
          etUnknown: ;
          etSubDirectory,
          etFile: begin
            IsSubDirectory := EntryType = etSubDirectory;
            if Assigned(FOnItemSelection) then
              FOnItemSelection(Self, Entry, IsSubDirectory);
          end;
        end;
      end;
    end;
  end;
  inherited;
end;

procedure TCustomEntryView.ImagesChange(Sender: TObject);
var
  DC: HDC;
  F: HFont;
  ImageHeight: Integer;
begin
  DC := GetDC(0);
  F := SelectObject(DC, Font.Handle);
  FTextHeight := CalculateCaptionSize(DC, 'Why?').cY; // do not modiffy
  SelectObject(DC, F);
  ReleaseDC(0, DC);
  //
  // calculate folder height
  if FEntryImages <> Nil then
    ImageHeight := FEntryImages.Height
  else
    ImageHeight := 0;
  if ImageHeight > FTextHeight then
    FEntryHeight := ImageHeight
  else
    FEntryHeight := FTextHeight;
  Inc(FEntryHeight, Round(FTextHeight * 1.5));
  //
  Invalidate; // repaint visible entries
end;

procedure TCustomEntryView.InvalidateItem(Item: Integer);
var
  Rect: TRect;
begin
  if Item > -1 then begin
    if HandleAllocated then begin
      if DoubleBuffered then
        Invalidate
      else begin
        Rect := ItemRect(Item);
        InvalidateRect(Handle, @Rect, True);
      end;
    end;
  end;
end;

procedure TCustomEntryView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  // remove OS (windows) painted scrollbars
  // after this we need our own scrollbar component, that will paint them as we want, so with a little help form non-human this task will be faster
  Params.WindowClass.style := Params.WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  // don't repaint areas occupied by child controls
  Params.Style := Params.Style or WS_CLIPCHILDREN;
end;

procedure TCustomEntryView.CreateHandle;
begin
  inherited CreateHandle;
  if FTextHeight = 0 then
    FTextHeight := Canvas.TextHeight(' ');
  HideNativeScrollBars;
  UpdateScrollRange;
  ShowModernScrollBar;
end;

procedure TCustomEntryView.DoItemClick(Item: TEntryItem);
begin
  CaptureItem := Nil;
  if Assigned(FOnItemClick) then
    FOnItemClick(Self, Item);
end;

procedure TCustomEntryView.EnsureItemVisible;
begin
//  if (ItemIndex < 0) or (TopIndex >= ItemIndex) then
//    Exit;
  if ItemRect(ItemIndex).Top < 0 then begin
    TopIndex := ItemIndex - Abs(ItemRect(ItemIndex).Top) div FEntryHeight + 1;
  end;
  if ItemRect(ItemIndex).Bottom > ClientHeight then begin
    TopIndex := ItemIndex - ClientHeight div FEntryHeight + 1;
  end;
end;

//function TCustomEntryView.GetClientRect: TRect;
//begin
//  inherited;
////  Result.Left := 0;
////  Result.Top := 0;
////  Result.Right := Width - IfThen(CanShowScrollBar or FVerticalScrollBar.Visible, FVerticalScrollBar.Width, 0);
////  Result.Bottom := Height;
//end;

function TCustomEntryView.ClientArea: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := Width - IfThen(CanShowScrollBar{ or FVerticalScrollBar.Visible}, FVerticalScrollBar.Width, 0);
  Result.Bottom := Height;
end;

function TCustomEntryView.VerticalScrollBarArea: TRect;
begin
  Result.Left := Width - IfThen(CanShowScrollBar{ or FVerticalScrollBar.Visible}, FVerticalScrollBar.Width, 0);
  Result.Top := 0;
  Result.Right := Width;
  Result.Bottom := Height;
end;

function TCustomEntryView.ItemFromPoint(X, Y: Integer): TEntryItem;
var
  P: TPoint;
  I: Integer;
begin
  P := Point(X, Y);
  Result := EntryFromPoint(X, Y); // { + (FTopIndex * FEntryHeight)});
  if Result <> Nil then
    Exit;
  if Selected <> Nil then
    for I := 0 to Selected.Items.Count - 1 do
      if PtInRect(Selected.Items[I].RelativeDisplayRect, P) then begin
        Result := Selected.Items[I];
        Break;
      end;
end;

function TCustomEntryView.EntryFromPoint(X, Y: Integer): TEntryItem;
var
  P: TPoint;
  I: Integer;
  Item: TEntryItem;
  ItemRect: TRect;
begin
  Result := Nil;
  if Entries.Count = 0 then
    Exit;

  // here we have some problems locating item that we hover over
  // left one list is ok, but right list is not working for now, lets fix it
  //
  // ok, we have totaly wrong positions calculation algo, we need to fix it first and than locating hovered row should be easy
  // this task is simple to state, but it probably needs alot of work, so get to it
  // but first, i need to think about it, i know what i want to achive, i'm not sure right now what i need to destroy and/or recreate
  // to not put myself in a situation, that can be harder to bend to my needs
  // simple solution, and probably obvious, is to store this location in an entry itself
  // right now i calculate it every time when i need to use it, it is burning cpu cycles without need,
  // and may cause renderer to slow down, while iterating through the list
  // it is not easy to mentally picture it, but i need relative positions only, absolute ones are not usefull to me right now

  P := Point(X, Y);
  Item := Entries.Items[0];
  while Item <> Nil do begin
//    ItemRect := TRect.Empty;
//    if Item.Visible then
    ItemRect := Item.RelativeDisplayRect;
//    OutputDebugString(PChar('Mouse: (' + IntToStr(X) + ', ' + IntToStr(Y) + '), Item: "' + Item.Caption + '", ItemVisible: ' + BoolToStr(Item.Visible, True) + ', ItemEnabled: ' + BoolToStr(Item.Enabled, True) + ', ItemRect: (' + IntToStr(ItemRect.Left) + ', ' + IntToStr(ItemRect.Top) + ', ' + IntToStr(ItemRect.Right) + ', ' + IntToStr(ItemRect.Bottom) + ')'));
    if Item.Visible and PtInRect(ItemRect, P) then begin
      Result := Item;
      Exit;
    end;
    //
    Item := Entries.GetNextEntry(Item);
  end;
end;


// little explanation of what is going on

// i have this component TCustomEntryView, it was earlier named different and used in other project
// because i didn't wanted to make it from scratch, i decided to refactor it and reuse in my NitroEDitor for some things
// not only to present folders and files names, but to primarly make it a tree display
// this tree of entries of any kind, later would be used to display NitroPascal compiler project properties and other things,
// for now i do not know what it will be usefull for, but it must be flexible enough

// now, there is this tree entries list, each entry has its own Index and Level

  //  Root collection
  //   ├─ Item A - (Index: 0; Level: 0)
  //   │   ├─ Item A1 - (Index: 1; Level: 1)
  //   │   ├─ Item A2 - (Index: 2; Level: 1)
  //   │   │   └─ Item A2a - (Index: 3; Level: 2)
  //   │   └─ Item A3 - (Index: 4; Level: 1)
  //   └─ Item B - (Index: 5; Level: 0)
  //       └─ Item B1 - (Index: 6; Level: 1)

// so if we do some transformation, we get this:

  //  Root collection
  //   ├─ (0:0) - Item A
  //   │   ├─ (1:1) - Item A1
  //   │   ├─ (2:1) - Item A2
  //   │   │   └─ (3:2) - Item A2a
  //   │   └─ (4:1) - Item A3
  //   └─ (5:0) - Item B
  //       └─ (6:1) - Item B1

// and any item that has children can be Collapsed or Expanded showing its children
// also any item can be not Visible
// this states change how tree is rendered/displayed
// not visible items are not rendered, and there Indexes are set to -1, so we can skip them
// this should look like this:

  //  Root collection
  //   ├─ (0:0) - Item A
  //   │   ├─ (1:1) - Item A1
  //   │   ├─ (-1:1) - Item A2 - [hidden item]
  //   │   │   └─ (-1:2) - Item A2a
  //   │   └─ (2:1) - Item A3
  //   └─ (3:0) - Item B
  //       └─ (4:1) - Item B1

// now, when i render entries, all of them are visible, so there is no need to do enything with Indexes,
// there is one problem, number of entries to display maybe greater then number of rows that this component can display at once
// this is handled by showing only those that we need at any given moment, if this situation occurs, we show scrollbars,
// for user to be able to scroll this list up or down, if user scrolls down, the internal variable TopIndex is set, to the value
// that corresponds with scrollbar position, value greater than 0 (zero) indicates that we scrolled down,
// i calculate Entries position based on this TopIndex variable, and for now i have trouble with indicating for user that mouse
// hovered over an Entry should be repainted as hot, this situation is present if TopIndex is non zero

// i have to debug this situation, and find where i made a mistake
// ok, now i found it, this was wrong, adding offset to the mouse position, was causing some problems in matching mouse pos with Entry row pos
// Result := EntryFromPoint(X, Y); // { + (FTopIndex * FEntryHeight)});



function TCustomEntryView.ItemAtPos(const Pos: TPoint; Existing: Boolean): Integer;
var
  count: Integer;
begin
  count := VisibleEntriesCount;
  Result := FTopIndex + (Pos.Y div FEntryHeight);
  if Result > count - 1 then
    if Existing then
      Result := -1
    else
      Result := count - 1;
  if FLocked and (Result <> FLockedIndex) then
    Result := -1;
end;

function TCustomEntryView.ItemFromIndex(const Index: Integer): TEntryItem;
var
  Entry, Item: TEntryItem;
  i, idx: Integer;
begin
  //
  Result := Nil;
  idx := 0;

  // select item knowing only its index on the list, from items that have children, so we need to enter those lists if needed
  // i could do it by recursion, but linear solutin is an option here

  // last time i stated:
  // no, i do not have time to fight with this, i must go forward, with this project
  //
  // i must say that i was tired, id did not have enough mental capacity to thing hard at this algorithm that i atempted to create
  // i knew what i need to do, but had trouble with picturing how to put it in, into existing at the time code
  //
  // after couple of days of break, i returned to this with a clear mind and started by implementing functions that are responsilbe for
  // picking entries:
  // - first_item, last_item, prev_item, next_item, prev_sibling, next_sibling
  //
  // here are those functions:

//  function GetFirstEntry(const Entry: TEntryItem): TEntryItem;
//  function GetLastEntry(const Entry: TEntryItem): TEntryItem;
//  function GetPrevEntry(Entry: TEntryItem): TEntryItem;
//  function GetNextEntry(Entry: TEntryItem): TEntryItem;
//  function GetFirstEntrySibling(const Entry: TEntryItem): TEntryItem;
//  function GetLastEntrySibling(const Entry: TEntryItem): TEntryItem;
//  function GetPrevEntrySibling(const Entry: TEntryItem): TEntryItem;
//  function GetNextEntrySibling(const Entry: TEntryItem): TEntryItem;

  // those functions operate on this tree structure:
  //
  //  Root collection
  //   ├─ Item A
  //   │   ├─ Item A1
  //   │   ├─ Item A2
  //   │   │   └─ Item A2a
  //   │   └─ Item A3
  //   └─ Item B
  //       └─ Item B1

  // this tree entries have 2 properties that can change how three must be rendered
  // first property : Visible - determines that whole entry is shown or not
  // second property: State   - tells renderer to show only this entry or render any children that this entry has

  // DFS implementation without local stack

  Entry := Nil;
  if Entries.Count > 0 then
    Entry := Entries.Items[0];

  while Entry <> Nil do begin
    // process item
    if Entry.Visible then begin
      if idx = Index then
        Exit(Entry)
      else if Entry.State = esCollapsed then begin
        Inc(idx);
      end
      else begin // if Item.State = esExpanded then
        // ok now we must check all the children, if they have any children of their own
        Inc(idx);

        if Entry.Items.Count > 0 then begin
          Entry := Entry.Items[0];
          Continue;
        end;
      end;
    end;
    //
    Entry := Entries.GetNextEntry(Entry);
  end;
end;

function TCustomEntryView.ItemSelected(const Item: TEntryItem): Boolean;
begin
  Result := FSelectedItems.IndexOf(Item) > -1;
end;

function TCustomEntryView.ItemRect(Item: Integer): TRect;
var
  Top: Integer;
begin
  Top := (Item - FTopIndex) * FEntryHeight;
  Result := Rect(0, Top, ClientWidth, Top + FEntryHeight);
end;

procedure TCustomEntryView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_HOME: ItemIndex := 0;
    VK_END: ItemIndex := VisibleEntriesCount - 1;
    VK_NEXT: SetScrollIndex(ItemIndex + ClientHeight div FEntryHeight);
    VK_PRIOR: SetScrollIndex(ItemIndex - ClientHeight div FEntryHeight);
    VK_LEFT,
    VK_RIGHT: Key := 0;
    VK_UP: begin
      if (Selected <> Nil) and (Selected.TreeIndex > 0) then
        Selected := ItemFromIndex(Selected.TreeIndex - 1);
      Key := 0;
    end;
    VK_DOWN: begin
      if (Selected <> Nil) and (Selected.TreeIndex < VisibleEntriesCount - 1) then
        Selected := ItemFromIndex(Selected.TreeIndex + 1);
      Key := 0;
    end;
  end;
  EnsureItemVisible;
end;

procedure TCustomEntryView.Loaded;
begin
  inherited Loaded;
  ActiveIndex := FActiveIndex;
end;

procedure TCustomEntryView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  function CanFocus: Boolean;
  var
    W: TWinControl;
  begin
    W := Self;
    while W <> Nil do
      if not W.Visible or not W.Enabled then
        Exit(False)
      else
        W := W.Parent;
    Result := True;
  end;

var
//  Point: TPoint;
  Entry: TEntryItem;
//  ScrollButton: TFolderScrollButton;
  Rect: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then begin
    if CanFocus then
      SetFocus;
    Entry := EntryFromPoint(X, Y);
    if Entry <> Nil then begin
      if MultiSelect and (ssShift in Shift) then begin
        // ok, now we have to check if selected Entry is on the list, if it is not, than add, else remove
        if not ItemSelected(Entry) then // add
          FSelectedItems.Add(Entry)
        else // remove
          FSelectedItems.Remove(Entry);
        CaptureItem := Entry; // this does invalidate
      end
      else
        Selected := Entry; // this does invalidate
      Exit;
    end;
  end;
end;

      // capture item is used as indicator of what item was focussed
//    CaptureItem := ItemFromPoint(X, Y);
//    if CaptureItem <> Nil then begin
//      Rect := CaptureItem.DisplayRect;
//      InvalidateRect(Handle, @Rect, False);
//    end;

//    Point := GetPoint(X, Y);
//    FMousePoint := Point(X, Y);
//    FButtons := [];
//    for ScrollButton := Low(TFolderScrollButton) to High(TFolderScrollButton) do
//      if PtInRect(ButtonRect[ScrollButton], Point) then begin
//        Include(FButtons, ScrollButton);
//        Rect := ButtonRect[ScrollButton];
//        InvalidateRect(Handle, @Rect, False);
//        if CaptureItem <> Nil then begin
//          Rect := CaptureItem.DisplayRect;
//          InvalidateRect(Handle, @Rect, False);
//        end;
//        CaptureItem := Nil;
//        SetTimer(Handle, 1, 125, Nil);
//        Exit;
//      end;


procedure TCustomEntryView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  HideNativeScrollBars;
  ShowModernScrollBar;
  inherited MouseMove(Shift, X, Y);
  if not MouseCapture then begin
    CaptureItem := ItemFromPoint(X, Y);
    FHotIndex := ItemAtPos(Point(X, Y), True);
  end;
end;

procedure TCustomEntryView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Entry: TEntryItem;
//  Point: TPoint;
//  ScrollButton: TFolderScrollButton;
  Rect: TRect;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if csDesigning in ComponentState then
    Exit;
  if Button = mbLeft then begin
//    FMousePoint := Point(X, Y);
//    Point := GetPoint(X, Y);
//    if FButtons <> [] then
//      for ScrollButton := Low(TFolderScrollButton) to High(TFolderScrollButton) do
//        if PtInRect(ButtonRect[ScrollButton], Point) then
//          if ScrollButton in FButtons then begin
//            CaptureItem := Nil;
//            FButtons := [];
//            Exit;
//          end;
//    if FButtons <> [] then begin
//      FButtons := [];
//      Invalidate;
//    end;
    Entry := EntryFromPoint(X, Y);
    if CaptureItem <> Nil then begin
      Rect := CaptureItem.AbsoluteDisplayRect;
      InvalidateRect(Handle, @Rect, False);
    end;
    if CaptureItem = Entry then
      DoItemClick(CaptureItem);
//    CaptureItem := ItemFromPoint(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

procedure TCustomEntryView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent <> Nil) then begin
    if AComponent = FEntryImages then
      UpdateImages(FEntryImages, Nil);
//    else if AComponent = FItemImages then
//      UpdateImages(FItemImages, Nil);
  end;
end;

procedure TCustomEntryView.Clear;
begin
  FActiveIndex := -1;
  FCaptureItem := Nil;
  FHotIndex := -1;
  FMouseItem := Nil;
  FSelected := Nil;
  FSelectedItems.Clear;
  FItemIndex := -1;
  FTopIndex := 0;
  FShiftIndex := -1;
  //
  Entries.Clear;
end;

procedure TCustomEntryView.BeginUpdate;
begin
  Entries.BeginUpdate;
end;

procedure TCustomEntryView.EndUpdate;
begin
  Entries.EndUpdate;
end;

procedure TCustomEntryView.Update;
begin
  ActiveIndex := FActiveIndex;
end;

function TCustomEntryView.GetFirstEntry: TEntryItem;
begin
  Result := Nil;
  if Entries.Count > 0 then
    Result := Entries.Items[0];
end;

function TCustomEntryView.GetLastEntry: TEntryItem;
begin
  Result := Nil;
  if Entries.Count > 0 then
    Result := Entries.GetLastEntry(Entries.Items[Entries.Count - 1]);
end;

function TCustomEntryView.GetPrevEntry(Entry: TEntryItem): TEntryItem;
begin
  Result := Nil;
  if Entry = Nil then
    Exit;

  Result := Entries.GetPrevEntry(Entry);
end;

function TCustomEntryView.GetNextEntry(Entry: TEntryItem): TEntryItem;
begin
  Result := Nil;
  if Entry = Nil then
    Exit;

  Result := Entries.GetNextEntry(Entry);
end;

function TCustomEntryView.GetFirstEntrySibling(const Entry: TEntryItem): TEntryItem;
begin
  Result := Nil;
  if Entry = Nil then
    Exit;

  Result := Entries.GetFirstEntrySibling(Entry);
end;

function TCustomEntryView.GetLastEntrySibling(const Entry: TEntryItem): TEntryItem;
begin
  Result := Nil;
  if Entry = Nil then
    Exit;

  Result := Entries.GetLastEntrySibling(Entry);
end;

function TCustomEntryView.GetPrevEntrySibling(const Entry: TEntryItem): TEntryItem;
begin
  Result := Nil;
  if Entry = Nil then
    Exit;

  Result := Entries.GetPrevEntrySibling(Entry);
end;

function TCustomEntryView.GetNextEntrySibling(const Entry: TEntryItem): TEntryItem;
begin
  Result := Nil;
  if Entry = Nil then
    Exit;

  Result := Entries.GetNextEntrySibling(Entry);
end;

procedure TCustomEntryView.UpdateImages(var InternalImages: TCustomImageList;
  ExternalImages: TCustomImageList);
begin
  if InternalImages <> Nil then begin
    InternalImages.UnRegisterChanges(FChangeLink);
    InternalImages.RemoveFreeNotification(Self);
  end;
  InternalImages := ExternalImages;
  if InternalImages <> Nil then begin
    InternalImages.RegisterChanges(FChangeLink);
    InternalImages.FreeNotification(Self);
  end;
  Invalidate;
end;

procedure TCustomEntryView.HideNativeScrollBars;
begin
  FlatSB_ShowScrollBar(Handle, SB_BOTH, False);
end;

procedure TCustomEntryView.UpdateScrollRange;
var
  ScrollInfo: TScrollInfo;
  count: Integer;
begin
  if HandleAllocated then begin
    count := VisibleEntriesCount; // all visible and expanded items
    ScrollInfo.cbSize := SizeOf(TScrollInfo);
    ScrollInfo.fMask := SIF_PAGE or SIF_POS or SIF_RANGE;
    ScrollInfo.nMin := 0;
    ScrollInfo.nMax := count;
    ScrollInfo.nPage := ClientHeight div FEntryHeight;
    ScrollInfo.nPos := FTopIndex;
    //
    if count - FTopIndex < Integer(ScrollInfo.nPage) then
      SetTopIndex(count - Integer(ScrollInfo.nPage));
    // and here is the magic, we let know to OS that we have vertical scrollbar and sets its info, but OS does not render it, we do it ourselfs
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
    FVerticalScrollBar.SetInfo(ScrollInfo.nMax{ * FEntryHeight}, ScrollInfo.nPage, ScrollInfo.nPos);
  end;
//  with ScrollInfo do begin
//    ScrollInfo.cbSize := SizeOf(TScrollInfo);
//    ScrollInfo.fMask := SIF_PAGE or SIF_POS or SIF_RANGE;
//    ScrollInfo.nMin := 0;
//    ScrollInfo.nMax := FScrollWidth;
//    ScrollInfo.nPage := ClientWidth;
//    ScrollInfo.nPos := FScrollLeft;
//    SetScrollInfo(Handle, SB_Horz, ScrollInfo, True);
//  end;
end;

procedure TCustomEntryView.OverlayChange(Sender: TObject);
begin
  FOverlayPicture.Bitmap.Transparent := True;
  FOverlayPicture.Bitmap.TransparentMode := tmAuto;
  Invalidate;
end;

//  procedure DrawButtons(DC: HDC; Brush: HBRUSH);
//  const
//    Arrows: array[TFolderScrollButton] of TDirection = (drUp, drDown);
//  var
//    Button: TFolderScrollButton;
//    Rect: TRect;
//    Down: Boolean;
//    State: TDrawState;
//  begin
//    for Button := Low(TFolderScrollButton) to High(TFolderScrollButton) do begin
//      Rect := ButtonRect[Button];
//      if not IsRectEmpty(Rect) then begin
//        if not StyleServices.Available then
//          FillRect(DC, Rect, COLOR_3DFACE + 1);
//        Down := MouseCapture and (Button in FButtons);
//        if Down then
//          State := [dsPressed]
//        else
//          State := [dsHot];
//        DrawThemeScroll(DC, Arrows[Button], Rect, State);
//        //SelectClipRect(DC, Rect, RGN_DIFF);
//      end;
//    end;
//  end;

procedure TCustomEntryView.Paint;
var
  C, SB: TRect;
begin
  C := ClientArea;
  DrawBackground(Canvas, C); // clear background
  if FDrawVerticalScrollBar or (not MouseInClient and Focused and not FVerticalScrollBar.Visible) then begin
    FDrawVerticalScrollBar := False;
    SB := VerticalScrollBarArea;
    DrawBackground(Canvas, SB); // clear background
  end;
  DrawItems(Canvas, C); //draw items
end;

procedure TCustomEntryView.DrawBackground(const ACanvas: TCanvas; var ARect: TRect);
begin
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := ColorToRGB(Color);
  ACanvas.FillRect(ARect);
  //
  if FBorderStyle = bsSingle then begin
    DrawThemeBorder(ACanvas.Handle, Color, ARect, []);
    InflateRect(ARect, -GetBorder, -GetBorder);
    //SelectClipRect(DC, Rect, RGN_AND);
  end;
end;

//  if (Shadow = 0) and StyleServices.Enabled then
//    Shadow := LoadIcon(MainInstance, 'SHADOW');

//  Folder := Selected;
//  if Folder <> Nil then begin
//    Brush := CreateSolidBrush(ColorToRGB(Color));
//    //HotBrush := CreateSolidBrush(ColorToRGB(clHighlight));
//    for J := Folder.GetNearestTop to Folder.Items.Count - 1 do begin
//      Item := Folder.Items[J];
//      if not Item.Visible then
//        Continue;
//      DrawRect := Item.DisplayRect;
//      SetBkMode(DC, OPAQUE);
//      FillRect(DC, DrawRect, Brush);
//      Point := DrawRect.TopLeft;
//      if FItemImages <> Nil then
//        //Point.X := (DrawRect.Right - DrawRect.Left) div 2 - FItemImages.Width div 2;
//        Point.X := DrawRect.Left + FItemImages.Width;
//      Point.Y := DrawRect.Top + (DrawRect.Bottom - DrawRect.Top - FItemImages.Width) div 2;
//      if (FCaptureItem <> Nil) and (FCaptureItem = Item) then begin
//        //FillRect(DC, DrawRect, HotBrush);
////        if StyleServices.Enabled then begin
////          DrawIconEx(DC, DrawRect.Left, DrawRect.Top, Shadow, 1, 1, 0 , 0, DI_NORMAL);
////          DrawIconEx(DC, DrawRect.Left, DrawRect.Top, Shadow, WidthOf(DrawRect), HeightOf(DrawRect), 0 , 0, DI_NORMAL);
////          if MouseCapture then begin
////            DrawIconEx(DC, DrawRect.Right - 1, DrawRect.Top, Shadow, 1, HeightOf(DrawRect), 0 , 0, DI_NORMAL);
////            DrawIconEx(DC, DrawRect.Left + 1, DrawRect.Bottom - 1, Shadow, WidthOf(DrawRect) - 2, 1, 0 , 0, DI_NORMAL);
////            DrawIconEx(DC, DrawRect.Left, DrawRect.Top, Shadow, 2, HeightOf(DrawRect), 0 , 0, DI_NORMAL);
////            DrawIconEx(DC, DrawRect.Left + 2, DrawRect.Top, Shadow, WidthOf(DrawRect) - 4, 2, 0 , 0, DI_NORMAL);
////            DrawIconEx(DC, DrawRect.Left, DrawRect.Top, Shadow, WidthOf(DrawRect), HeightOf(DrawRect), 0 , 0, DI_NORMAL);
////          end;
////        end
////        else
//        if MouseCapture then
//          DrawThemeThinButton(DC, DrawRect, [dsPressed])
//        else
//          DrawThemeThinButton(DC, DrawRect, [dsHot]);
//        InflateRect(DrawRect, -1, -1);
//        if not StyleServices.Enabled and (Overlay = Nil) then
//          FillRect(DC, DrawRect, Brush);
//      end
//      else if Overlay = Nil then
//        FillRect(DC, DrawRect, Brush);
//      Dec(DrawRect.Right, 4);
//      if (FCaptureItem <> Nil) and (FCaptureItem = Item) then
//        if MouseCapture then begin
//          OffsetRect(DrawRect, 1, 1);
//          Inc(Point.X);
//          Inc(Point.Y);
//        end;
//      Inc(DrawRect.Left, 4);
//      if (FItemImages <> Nil) and (Item.ImageIndex > -1) then begin
//        ImageList_DrawEx(FItemImages.Handle, Item.ImageIndex, DC, Point.X, Point.Y, 0, 0, CLR_NONE, CLR_NONE, ILD_NORMAL);
//        Inc(DrawRect.Left, Point.X + FItemImages.Width);
//        if not Item.Enabled and (FOverlayPicture.Height > 0) then
//          Canvas.Draw(DrawRect.Left + (DrawRect.Right - DrawRect.Left - FOverlayPicture.Width) div 2 - FTextHeight div 4, Top + (DrawRect.Bottom - DrawRect.Top - FOverlayPicture.Height) div 2 - FTextHeight div 2, FOverlayPicture.Bitmap);
//      end;
//      SetTextColor(DC, GetTextColor(Color));
//      SetBkMode(DC, TRANSPARENT);
//      if (FCaptureItem <> Nil) and (FCaptureItem = Item) then
//        OffsetRect(DrawRect, 0, 1);
//      Dec(DrawRect.Right, 4);
//      //DrawRect.Top := DrawRect.Bottom - Trunc(FTextHeight * 1.5);
//      DrawCaption(DC, Item.Caption, DrawRect, drLeft);
////      if (FCaptureItem <> Nil) and (FCaptureItem = Item) then begin
////        InflateRect(DrawRect, 1, 1);
////        if MouseCapture then
////          OffsetRect(DrawRect, -1, -1);
////      end;
//    end;
//    DeleteObject(Brush);
//    //DeleteObject(HotBrush);
////    DrawButtons(DC, CheckeredBrush);
//  end;
//  DeleteObject(CheckeredBrush);
//  Item := Nil;
//  if Selected <> Nil then
//    for I := Selected.Items.Count - 1 downto 0 do
//      if Selected.Items[I].Visible then begin
//        Item := Selected.Items[I];
//        Break;
//      end;
//  if Item <> Nil then begin
//    Rect.Top := Item.DisplayRect.Bottom;
//    I := Selected.Index + 1;
//    for J := I to Folders.Count - 1 do
//      if Folders[J].Visible then begin
//        Rect.Bottom := Folders[J].DisplayRect.Top;
//        Break;
//      end;
//    if Rect.Bottom < Rect.Top then
//      Rect.Bottom := Rect.Top;
//  end
//  else if Selected <> Nil then begin
//    Rect.Top := Selected.DisplayRect.Top + FFolderHeight;
//    I := Selected.Index + 1;
//    for J := Folders.Count - 1 downto I do
//      if Folders[J].Visible then begin
//        Rect.Bottom := Folders[J].DisplayRect.Top;
//        Break;
//      end;
//  end
//  else begin
//    Rect.Top := 0;
//    for I := Folders.Count - 1 downto 0 do
//      if Folders[I].Visible then begin
//        Rect.Top := Folders[I].DisplayRect.Bottom;
//        Break;
//      end;
//  end;
//  if (Overlay = Nil) or (Selected = Nil) then begin
//  	Brush := CreateSolidBrush(ColorToRGB(Color));
//    FillRect(DC, Rect, Brush);
//  	DeleteObject(Brush);
//  end;


// dsDisabled,
// dsPressed,
// dsSelected,
// dsHot,
// dsFocused,
// dsChecked,
// dsExpanded,
// dsDefaulted,

procedure TCustomEntryView.DrawItems(const ACanvas: TCanvas; const ARect: TRect);
var
  Clip, Row, R: TRect;
  I: Integer;
  DrawState: TDrawState;
  Item: TEntryItem;
begin
  //
  Clip := Canvas.ClipRect;
  Row := ARect;
  Row.Height := FEntryHeight;
//  if FScrollWidth > 0 then
//    Row.Width := FScrollWidth;
  for I := 0 to ARect.Height div (FEntryHeight + 1) do begin // divide client-area into horizontal strips and render Entries
    // set strip dimmensions
    if I + FTopIndex > VisibleEntriesCount - 1 then
      Break;
    R := Row;
    R.Top := R.Top + I * FEntryHeight;
    R.Height := FEntryHeight;
    if R.Bottom <= Clip.Top then // if we are ouside of client-area, go to next entry
      Continue;
    if R.Top >= Clip.Bottom then // if we are ouside of client-area, go to next entry
      Continue;
    //
    // set render state
    DrawState := [];
    if Focused and (FTopIndex + I = ItemIndex) then // draw focused only Entry that is active
      Include(DrawState, dsFocused);
    if FTopIndex + I = ItemIndex then begin // ok, ItemIndex is not set at all, it returns -1, this is not right
      if FMultiSelect then
        Include(DrawState, dsDefaulted)
      else
        Include(DrawState, dsSelected);
    end;
    if FMultiSelect and ItemSelected(ItemFromIndex(FTopIndex + I)) then
      Include(DrawState, dsSelected);
    if FTopIndex + I = FHotIndex then
      Include(DrawState, dsHot);
    //
    DrawItem(ACanvas, FTopIndex + I, R, DrawState);
  end;
end;

procedure TCustomEntryView.DrawItem(const ACanvas: TCanvas; const ItemIdx: Integer; const ARect: TRect; DrawState: TDrawState);
var
  Item: TEntryItem;
begin
  Item := ItemFromIndex(ItemIdx);
  if (Item = Nil) or
     ((Item.ParentEntries <> Nil) and (Item.ParentEntries.OwnerEntry <> Nil) and (Item.ParentEntries.OwnerEntry.Items.Count > 0) and (Item.ParentEntries.OwnerEntry.State = esCollapsed)) then
    Exit;
  //
  DrawEntryItem(ACanvas, ARect, DrawState, Item);
end;

//procedure TCustomEntryView.DrawFolder(const ACanvas: TCanvas; const ARect: TRect; const DrawState: TDrawState; const Folder: TFolderBar);
//var
//  DC: HDC;
//  DrawRect: TRect;
//  CheckeredBrush: HBRUSH;
//  top: Integer;
//begin
//  if not Assigned(Folder) then
//    Exit;
//  //
//  DC := ACanvas.Handle;
//  CheckeredBrush := GetBrush(CheckeredBitmap);
//  //DrawRect := Folder.DisplayRect;
//  DrawRect := ARect;
//
//  // clear item background
//  SetBkMode(DC, OPAQUE);
//  ACanvas.Brush.Color := ColorToRGB(Color);
//  ACanvas.FillRect(ARect);
//
//  if StyleServices.Enabled then begin
//    Inc(DrawRect.Right, 10);
//    if dsSelected in DrawState then
//      StyleServices.DrawElement(DC, GetDetails(thHeaderItemHot), DrawRect)
//    else
//      StyleServices.DrawElement(DC, GetDetails(thHeaderItemNormal), DrawRect);
//    Dec(DrawRect.Right, 10);
//  end
//  else begin
//    DrawFrame(DC, DrawRect, dfRaised);
//    InflateRect(DrawRect, -1, -1);
//    if dsSelected in DrawState then
//      FillRect(DC, DrawRect, CheckeredBrush)
//    else
//      ACanvas.FillRect(DrawRect);
//  end;
//  DeleteObject(CheckeredBrush);
//  // draw item icon
//  InflateRect(DrawRect, -6, 0);
//  if FFolderImages <> Nil then begin
//    top := (FItemHeight - FFolderImages.Height) div 2;
//    if (dsSelected in DrawState) and (Folder.SelectedIndex > -1) then begin
//      ImageList_DrawEx(FFolderImages.Handle, Folder.SelectedIndex, DC, DrawRect.Left, DrawRect.Top + top, 0, 0, CLR_NONE, CLR_NONE, ILD_NORMAL);
//    end
//    else if Folder.ImageIndex > -1 then
//      ImageList_DrawEx(FFolderImages.Handle, Folder.ImageIndex, DC, DrawRect.Left, DrawRect.Top + top, 0, 0, CLR_NONE, CLR_NONE, ILD_NORMAL);
//    Inc(DrawRect.Left, FFolderImages.Width + 2);
//  end;
//  // draw item caption
//  InflateRect(DrawRect, -1, -1);
//
//  ACanvas.Font.Color := clWindowText;
//  SetBkMode(DC, TRANSPARENT);
//  DrawCaption(ACanvas, Folder.Caption, DrawRect, drLeft);
//  //
////  InflateRect(DrawRect, 2, 2);
////  if FFolderImages <> Nil then
////    Dec(DrawRect.Left, FFolderImages.Width + 2);
//////  DrawRect := Folder.DisplayRect;
//////    SelectClipRect(DC, DrawRect, RGN_DIFF);
////  if FOverlayPicture <> Nil then
////    BitBlt(DC, DrawRect.Left, DrawRect.Bottom, DrawRect.Width, DrawRect.Height, FOverlayPicture.Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
//end;

//  if StyleServices.Enabled then begin
//    //Inc(DrawRect.Right, 10);
//    if (DrawState * [dsSelected, dsHot] = [dsSelected, dsHot]) or (dsHot in DrawState) then begin
//      ACanvas.Brush.Bitmap := CheckeredBitmap;
//      //ACanvas.Brush.Style := bsClear;
//      ACanvas.FillRect(DrawRect);
//      ACanvas.Brush.Bitmap := Nil;
//    end
//    else if dsSelected in DrawState then
//      StyleServices.DrawElement(DC, GetDetails(thHeaderItemHot), DrawRect)
//    else if dsDefaulted in DrawState then
//      StyleServices.DrawElement(DC, GetDetails(thHeaderItemHot), DrawRect)
//    //Dec(DrawRect.Right, 10);
//  end
//  else begin
//    DrawFrame(DC, DrawRect, dfRaised);
//    InflateRect(DrawRect, -1, -1);
//    if dsSelected in DrawState then begin
//      //FillRect(DC, DrawRect, CheckeredBrush)
//      ACanvas.Brush.Bitmap := CheckeredBitmap;
//      ACanvas.FillRect(DrawRect);
//      ACanvas.Brush.Bitmap := Nil;
//    end
//    else
//      ACanvas.FillRect(DrawRect);
//  end;
  //DeleteObject(CheckeredBrush);

procedure TCustomEntryView.DrawEntryItem(const ACanvas: TCanvas; const ARect: TRect; const DrawState: TDrawState; const Item: TEntryItem);
var
  DC: HDC;
  DrawRect: TRect;
//  CheckeredBrush: HBRUSH;
  top: Integer;
  Indent: Integer;
begin
  if not Assigned(Item) then
    Exit;
  //
  //Indent := GetIndentLevel(Item);
  Indent := Item.TreeLevel;
  //
  DC := ACanvas.Handle;
//  CheckeredBrush := GetBrush(CheckeredBitmap);
  DrawRect := ARect;

  // clear item background
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := ColorToRGB(Color);
  if (DrawState * [dsSelected, dsHot] = [dsSelected, dsHot]) or (dsHot in DrawState) then
    ACanvas.Brush.Color := ColorToRGB(HotColor)
  else if dsSelected in DrawState then
    ACanvas.Brush.Color := ColorToRGB(SelectedColor)
  else if dsDefaulted in DrawState then
    ACanvas.Brush.Color := ColorToRGB(ActiveColor);
  ACanvas.FillRect(ARect);

  // if focused
  if not (csLoading in ComponentState) and (dsFocused in DrawState) then
    DrawBorder(ACanvas, ARect, clWhite, 2); // @TODO: fix overlay mode

  Inc(DrawRect.Left, 20 * Indent);

  // draw item icon
  InflateRect(DrawRect, -6, 0);
  if FEntryImages <> Nil then begin
    top := (FEntryHeight - FEntryImages.Height) div 2;
    if Item.ImageIndex > -1 then
      ImageList_DrawEx(FEntryImages.Handle, Item.ImageIndex, DC, DrawRect.Left, DrawRect.Top + top, 0, 0, CLR_NONE, CLR_NONE, ILD_NORMAL);
    Inc(DrawRect.Left, FEntryImages.Width + 2);
  end;
  // draw item caption
  InflateRect(DrawRect, -1, -1);
  ACanvas.Font.Color := clWindow;
  if DrawState * [dsSelected, dsHot] = [dsSelected, dsHot] then
    ACanvas.Font.Color := TWinControlAccess(Parent).Color
  else if dsSelected in DrawState then
    ACanvas.Font.Color := clWindowText
  else if dsHot in DrawState then
    ACanvas.Font.Color := clCaptionText;
  SetBkMode(DC, TRANSPARENT);
  DrawCaption(ACanvas, Item.Caption, DrawRect, drLeft);
  // draw expand indicator if item has children
  if Item.Items.Count > 0 then begin
    if Item.State = esCollapsed then
      DrawArrow(ACanvas, Rect(ARect.Right - 20, ARect.Top, ARect.Right, ARect.Bottom), daDown, DrawState, 2, 5)
    else // esExpanded
      DrawArrow(ACanvas, Rect(ARect.Right - 20, ARect.Top, ARect.Right, ARect.Bottom), daUp, DrawState, 2, 5);
  end;
end;

procedure TCustomEntryView.Scroll(Delta: Integer);
begin
  ScrollBy(0, Delta);
end;

procedure TCustomEntryView.ScrollBy(DeltaX, DeltaY: Integer);
var
  R: TRect;
begin
  if DoubleBuffered then
    Invalidate
  else if HandleAllocated then begin
    R := ClientRect;
//    R.Top := FHeaderSize;
    ScrollWindow(Handle, DeltaX, DeltaY, @R, @R);
  end
end;

procedure TCustomEntryView.ScrollToSelection;
begin
  if FItemIndex < FTopIndex then
    SetTopIndex(FItemIndex)
  else if FItemIndex >= FTopIndex + (ClientHeight + 1) div FEntryHeight then
    SetTopIndex(FItemIndex - (ClientHeight - 1) div FEntryHeight);
end;

procedure TCustomEntryView.SelectItem(PriorIndex, NewIndex: Integer; var CanSelect: Boolean);
begin
  if CanSelect then begin
    FItemIndex := NewIndex;
//    if Assigned(FOnSelectItem) then
//      FOnSelectItem(Self);
  end;
end;

procedure TCustomEntryView.SetActiveIndex(Value: Integer);
var
  count: Integer;
  Entry: TEntryItem;
begin
  FActiveIndex := Value;
  if csLoading in ComponentState then
    Exit;
  // ok, Value can't be less than zero
  if FActiveIndex < -1 then
    FActiveIndex := -1;
  // but FEntries is a tree, so FEntries.Count is not enough to determine the number of elements
  // so, we need to count the expanded and visible entries, each time we are going to select one
  count := VisibleEntriesCount;
  if FActiveIndex > count - 1 then
    FActiveIndex := count - 1;
  // here we need to use ItemFromIndex
  Entry := ItemFromIndex(FActiveIndex);
  if (FActiveIndex > -1) and (Entry <> Nil) and Entry.Visible then
    Selected := Entry // route through setter, so the component can know what is selected
  else
    Selected := Nil;
  Invalidate;
  // let Delphi form know, that we modified something within our component
  ComponentPropertiesChanged;
end;

procedure TCustomEntryView.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

//function TCustomEntryView.GetButtonRect(Button: TFolderScrollButton): TRect;
//var
//	ButtonHeight: Integer;
//  Rect: TRect;
//  Counter: Integer;
//  I: Integer;
//begin
//  ButtonHeight := GetSystemMetrics(SM_CXVSCROLL) + 4;
//  SetRectEmpty(Result);
//  if Selected <> Nil then begin
//    Rect := GetSelectedRect;
//    if HeightOf(Rect) > ButtonHeight * 2 + 8 then
//      case Button of
//        fbScrollUp: begin
//          if Selected.TopIndex > 0 then
//            Result := GetRect(Rect.Right - ButtonHeight - 4, Rect.Top + 4, Rect.Right - 4, Rect.Top + ButtonHeight + 4);
//        end;
//        fbScrollDown: begin
//          Counter := 0;
//          for I := Selected.Items.Count - 1 downto 1 do begin
//            if Selected.Items[I].Visible then begin
//              if Counter = 1 then
//                Break;
//              if Selected.Items[I].DisplayRect.Bottom > Rect.Bottom then
//                Result := GetRect(Rect.Right - ButtonHeight - 4, Rect.Bottom - ButtonHeight - 4, Rect.Right - 4, Rect.Bottom - 4);
//              Inc(Counter);
//            end;
//          end;
//          if Counter = 0 then
//            SetRectEmpty(Result);
//        end;
//      end;
//  end;
//end;

procedure TCustomEntryView.ComponentPropertiesChanged;
var
  Form: TCustomForm;
begin
  if csDesigning in ComponentState then begin
    Form := GetParentForm(Self);
    if Form <> Nil then
      Form.Designer.Modified;
  end;
end;

procedure TCustomEntryView.EntriesChanged;
begin
  FEntriesCount := GetEntriesCount;
  FVisibleEntriesCount := GetVisibleEntriesCount;
  HideNativeScrollBars;
  UpdateScrollRange;
  ShowModernScrollBar;
end;

  //  Root collection
  //   ├─ (0:0) - Item A - [collapsed]
  //   │   ├─ (1:1) - Item A1 -
  //   │   ├─ (-1:1) - Item A2 - [collapsed]
  //   │   │   └─ (-1:2) - Item A2a - parent visible and collapsed - hide me
  //   │   └─ (2:1) - Item A3
  //   └─ (3:0) - Item B
  //       └─ (4:1) - Item B1

function TCustomEntryView.GetEntriesCount: Integer;
var
  Item: TEntryItem;
begin
  Result := 0;
  //
  if Entries.Count = 0 then
    Exit;
  //
  Item := Entries.Items[0];
  while Item <> Nil do begin
    Inc(Result);
    Item := Entries.GetNextEntry(Item);
  end;
end;

function TCustomEntryView.GetVisibleEntriesCount: Integer;
var
  Item: TEntryItem;
begin
  Result := 0;
  //
  if Entries.Count = 0 then
    Exit;
  //
  Item := Entries.Items[0];
  while Item <> Nil do begin
    //if (Item.ParentIsExpanded and Item.Visible) or (Item.Visible and (not Item.HasChildren or (Item.HasChildren and not Item.IsExpanded))) then
    if Item.Visible then
      Inc(Result);
//    Inc(Result, IfThen(Item.Items.Count > 0, Item.Items.Count, 0));
    Item := Entries.GetNextEntry(Item);
  end;
end;

procedure TCustomEntryView.SetCaptureItem(Value: TEntryItem);
var
  Rect: tRect;
begin
//  if FButtons <> [] then
//    Value := Nil;
  if Value <> FCaptureItem then begin
    if FCaptureItem <> Nil then begin
      Rect := FCaptureItem.RelativeDisplayRect;
      InvalidateRect(Handle, @Rect, False);
    end;
    FCaptureItem := Value;
    if Value <> Nil then
      if FCaptureItem.Enabled then begin
        Rect := FCaptureItem.RelativeDisplayRect;
        InvalidateRect(Handle, @Rect, False);
      end
      else
        FCaptureItem := Nil;
  end;
end;

procedure TCustomEntryView.SetEntryHeight(Value: Integer);
begin
  if Value <> FEntryHeight then begin
    FEntryHeight := Value;
    Invalidate;
  end;
end;

procedure TCustomEntryView.SetEntryImages(Value: TCustomImageList);
begin
  if Value <> FEntryImages then begin
    UpdateImages(FEntryImages, Value);
    ImagesChange(FEntryImages);
  end;
end;

procedure TCustomEntryView.SetEntries(Value: TEntryItems);
begin
  if Value <> FEntries then
    FEntries.Assign(Value);
end;

procedure TCustomEntryView.SetItemIndex(Value: Integer);
var
  PriorIndex: Integer;
  CanSelect: Boolean;
  WasSelected: Boolean;
  count, I: Integer;
begin
  if FLocked then
    if Value > -1 then
      Value := FLockedIndex;
  count := VisibleEntriesCount;
  if Value > count - 1 then
    Value := count - 1;
  if Value > -1 then
    FLockedIndex := Value;
  if Value <> FItemIndex then begin
    PriorIndex := FItemIndex;
    if not HandleAllocated then begin
      FItemIndex := Value;
      Exit;
    end;
    CanSelect := True;
    SelectItem(FItemIndex, Value, CanSelect);
    if CanSelect then begin
      if PriorIndex > -1 then
        InvalidateItem(PriorIndex);
      FItemIndex := Value;
      ScrollToSelection;
      if PriorIndex <> FItemIndex then begin
        InvalidateItem(FItemIndex);
        if FMultiSelect and (FItemIndex > -1) then
          if ssShift in FShift then begin
//            if (FShiftIndex > -1) and (FShiftIndex < Length(FSelectItems)) then
//              WasSelected := FSelectItems[FShiftIndex]
//            else
//              WasSelected := False;
//            for I := Low(FSelectItems) to High(FSelectItems) do
//              FSelectItems[I] := False;
(*
            SetLength(FSelectItems, 0);
            FSelectCount := 0;
            if FShiftIndex > -1 then begin
              if FItemIndex < FShiftIndex then
                for I := FShiftIndex - 1 downto FItemIndex do begin
//                  FSelectItems[I] := True;
                  Inc(FSelectCount);
                end
                else for I := FShiftIndex + 1 to FItemIndex do begin
//                  FSelectItems[I] := True;
                  Inc(FSelectCount);
                end;
              if WasSelected then begin
//                FSelectItems[FShiftIndex] := True;
                Inc(FSelectCount);
              end;
              Invalidate;
            end
            else begin
*)
//              FSelectItems[FItemIndex] := True;
              FSelectedCount := 1;
              FShiftIndex := FItemIndex;
              Invalidate;
//            end;
          end
          else if ssCtrl in FShift then begin
//            if FSelectItems[FItemIndex] then
//              Dec(FSelectCount);
//            FSelectItems[FItemIndex] := not FSelectItems[FItemIndex];
//            if FSelectItems[FItemIndex] then
//              Inc(FSelectCount);
            if FSelectedCount > 1 then
              Invalidate;
          end
//          else if (PriorIndex > -1) and (FSelectCount = 1) and FSelectItems[FItemIndex] then begin
//            FSelectItems[PriorIndex] := False;
//            FSelectItems[FItemIndex] := True;
//          end
          else begin
//            for I := FSelectItems.Lo to FSelectItems.Hi do
//              FSelectItems[I] := False;
            FSelectedCount := 1;
//            FSelectItems[FItemIndex] := True;
            Invalidate;
          end;
      end;
      if not (ssShift in FShift) then
        FShiftIndex := FItemIndex;
    end;
  end
  else if FMultiSelect and (FItemIndex > -1) and (ssCtrl in FShift) then begin
//    if FSelectItems[FItemIndex] then
//      Dec(FSelectCount);
//    FSelectItems[FItemIndex] := not FSelectItems[FItemIndex];
//    if FSelectItems[FItemIndex] then
//      Inc(FSelectCount);
    if FSelectedCount > 1 then
      Invalidate
    else
      InvalidateItem(FItemIndex);
  end;
  FShift := FShift - [ssCtrl];
end;

procedure TCustomEntryView.SetMultiSelect(Value: Boolean);
//var
//  count: Integer;
begin
  if Value <> FMultiSelect then begin
    FMultiSelect := Value;
    FSelectedItems.Clear;
//    count := GetCount;
//    if FMultiSelect and (count > 0) then begin
////      FSelectItems.Length := FCount;
////      if FItemIndex > -1 then
////        FSelectItems[FItemIndex] := True;
//    end
//    else
      FSelectedCount := 0;
    Invalidate;
    FHotIndex := -1;
    KillTimer(Handle, 1);
  end;
end;

procedure TCustomEntryView.SetOverlayPicture(Value: TPicture);
begin
  FOverlayPicture.Assign(Value);
  Invalidate;
end;

procedure TCustomEntryView.SetScrollIndex(Value: Integer);
var
  count: Integer;
begin
  count := VisibleEntriesCount;
  if count = 0 then
    SetItemIndex(-1)
  else if Value > count - 1 then begin
    SetItemIndex(count - 1);
    SetTopIndex(FTopIndex + 1);
  end
  else if Value < 0 then
    SetItemIndex(0)
  else
    SetItemIndex(Value);
end;

procedure TCustomEntryView.SetSelected(Value: TEntryItem);
begin
  if Value <> FSelected then begin
    FSelected := Value;
    if FSelected = Nil then
      FItemIndex := -1
    else
      FItemIndex := FSelected.TreeIndex; // each visible Entry has its own index number in the tree
    if FActiveIndex <> FItemIndex then
      FActiveIndex := FItemIndex;
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TCustomEntryView.SetTopIndex(Value: Integer);
var
  ScrollInfo: TScrollInfo;
  Delta: Integer;
  P: TPoint;
  count: Integer;
begin
  count := VisibleEntriesCount;
  if Value > count - ClientHeight div FEntryHeight then
    Value := count - ClientHeight div FEntryHeight;
  if Value < 0 then
    Value := 0;
  if Value <> FTopIndex then begin
    Delta := (FTopIndex - Value) * FEntryHeight;
    FTopIndex := Value;
    if FHotTrack then
      if FHotIndex > - 1 then
        InvalidateItem(FHotIndex);
    FHotIndex := -1;
    ScrollInfo.cbSize := Sizeof(TScrollInfo);
    ScrollInfo.fMask := SIF_POS;
    ScrollInfo.nPos := FTopIndex;
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
    FVerticalScrollBar.Position := ScrollInfo.nPos;
    Scroll(Delta);
    InvalidateItem(FItemIndex);
    if FHotTrack then begin
      P := ScreenToClient(Mouse.CursorPos);
      FHotIndex := ItemAtPos(P, False);
      if FHotIndex > -1 then
        if PtInRect(ItemRect(FHotIndex), P) then
          InvalidateItem(FHotIndex)
        else
          FHotIndex := -1;
    end;
//    if FVerticalScrollBar.Visible then
//      FVerticalScrollBar.Invalidate;
  end;
end;

procedure TCustomEntryView.SetActiveColor(Value: TColor);
begin
  if FActiveColor <> Value then begin
    FActiveColor := Value;
    Repaint;
    ComponentPropertiesChanged;
  end;
end;

procedure TCustomEntryView.SetSelectedColor(Value: TColor);
begin
  if FSelectedColor <> Value then begin
    FSelectedColor := Value;
    Repaint;
    ComponentPropertiesChanged;
  end;
end;

procedure TCustomEntryView.SetHotColor(Value: TColor);
begin
  if FHotColor <> Value then begin
    FHotColor := Value;
    Repaint;
    ComponentPropertiesChanged;
  end;
end;

function TCustomEntryView.GetSelectedRect: TRect;
var
  I: Integer;
begin
  Result := ClientArea;
  if FBorderStyle = bsSingle then
    InflateRect(Result, -GetBorder, -GetBorder);
  if Selected <> Nil then begin
    Result.Top := Selected.AbsoluteDisplayRect.Bottom;
    for I := Selected.Index + 1 to Entries.Count - 1 do
      if Entries[I].Visible then begin
        Result.Bottom := Entries[I].AbsoluteDisplayRect.Top;
        Break;
      end;
  end
  else
    SetRectEmpty(Result);
end;

//function TCustomEntryView.GetIndentLevel(const Entry: TEntryItem): Integer;
//var
//  Entries: TEntryItems;
//begin
//  Result := 0;
//  //
//  if Entry = Nil then
//    Exit;
//  //
//  Entries := Entry.ParentEntries;
//  while Entries <> Nil do begin
//    if Entries.OwnerEntry <> Nil then begin
//      Inc(Result);
//      Entries := Entries.OwnerEntry.ParentEntries;
//    end
//    else
//      Break;
//  end;
//end;

function TCustomEntryView.CanShowScrollBar: Boolean;
var
  ScrollBar: TEntryViewScrollBar;
  ControlSize: Integer;
begin
  Result := True;
  // get orientation values
  ScrollBar := FVerticalScrollBar;
  ControlSize := Height;
  //
  if (ScrollBar.Range = 0) or (ScrollBar.Range * FEntryHeight <= ControlSize) then
    Result := False;
end;

procedure TCustomEntryView.UpdateModernScrollBar;
var
  ScrollBar: TEntryViewScrollBar;
  ControlSize: Integer;
begin
  if not CanShowScrollBar then
    Exit;

  ControlSize := Height;

  if FVerticalScrollBar.Range = 0 then
    Exit;

  FVerticalScrollBar.SetInfo(VisibleEntriesCount, ControlSize div FEntryHeight, FTopIndex);
end;

procedure TCustomEntryView.ShowModernScrollBar;
begin
  //FVerticalScrollBar.Visible := True;
  if CanShowScrollBar then
    FVerticalScrollBar.ShowAnimated;
  UpdateModernScrollBar;
  if FVerticalScrollBar.Visible then
    FVerticalScrollBar.BringToFront;
end;

procedure TCustomEntryView.HideModernScrollBar;
begin
  FVerticalScrollBar.HideAnimated;
end;

procedure TCustomEntryView.InvalidateScrollBarArea;
begin
  FDrawVerticalScrollBar := True;
  Invalidate;
end;

procedure TCustomEntryView.VerticalScrollBarShowingChange(Sender: TObject; State: TCustomScrollBarShowingEnum);
begin
  if State = shHidden then begin
    InvalidateScrollBarArea;
  end;
end;

procedure TCustomEntryView.CMDesignHitTest(var Msg: TCMDesignHitTest);
const
  HitTests: array[Boolean] of Integer = (0, 1);
begin
  inherited;
  with Msg do
    Result := HitTests[EntryFromPoint(XPos, YPos) <> Nil];
end;

procedure TCustomEntryView.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  Canvas.Font := Font;
  FTextHeight := Canvas.TextHeight(' ');
  if FEntryImages <> Nil then
    ImagesChange(FEntryImages);
//  if FItemImages <> Nil then
//    ImagesChange(FItemImages);
end;

procedure TCustomEntryView.CMMouseEnter(var Msg: TMessage);
begin
  HideNativeScrollBars;
  ShowModernScrollBar;
  inherited;
  CaptureItem := FMouseItem;
  FMouseItem := Nil;
  Repaint;
end;

procedure TCustomEntryView.CMMouseLeave(var Msg: TMessage);
begin
  HideModernScrollBar;
  inherited;
  if MouseCapture then
    FMouseItem := CaptureItem
  else
    FMouseItem := Nil;
  CaptureItem := Nil;
  FHotIndex := -1;
  Repaint;
end;

procedure TCustomEntryView.CMEntryImages(var Msg: TMessage);
begin
	Msg.Result := Integer(FEntryImages);
end;

//procedure TCustomEntryView.CMItemImages(var Msg: TMessage);
//begin
//	Message.Result := Integer(FItemImages);
//end;

procedure TCustomEntryView.CMItemDetails(var Msg: TMessage);
var
  Details: PEntryItemDetails;
begin
  Details := PEntryItemDetails(Msg.WParam);
  Details.BorderStyle := BorderStyle;
  Details.Selected := Selected;
  Details.ClientRect := ClientArea;
  Details.EntryHeight := FEntryHeight;
  //Details.FolderHeight := FFolderHeight;
  Details.TopIndex := FTopIndex;
  //
  Msg.Result := 1;
end;

procedure TCustomEntryView.CNItemClick(var Msg: TMessage);
begin
  DoItemClick(TEntryItem(Msg.WParam));
  Msg.Result := 1;
end;

procedure TCustomEntryView.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TCustomEntryView.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TCustomEntryView.WMSize(var Msg: TWMSize);
begin
  inherited;
  HideNativeScrollBars;
  UpdateScrollRange;
  ShowModernScrollBar;
end;

procedure TCustomEntryView.WMMouseWheel(var Msg: TWMMouseWheel);
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
  if delta > 0 then
    SetTopIndex(FTopIndex + 1)
  else
    SetTopIndex(FTopIndex - 1);
  inherited;
end;

procedure TCustomEntryView.WMNCHitTest(var Msg: TWMNCHitTest);
var
  P: TPoint;
  R: TRect;
begin
  inherited;

  P := ScreenToClient(Point(Msg.XPos, Msg.YPos));
  R := ClientRect;

  // Right-side fake scrollbar
  if P.X >= R.Right - FVerticalScrollBar.Width then begin
    Msg.Result := HTVSCROLL;
    Exit;
  end;
end;

procedure TCustomEntryView.WMVScroll(var Msg: TWMScroll);
begin
  HideNativeScrollBars;
  case Msg.ScrollCode of
    SB_TOP       : SetTopIndex(0);
    SB_BOTTOM    : SetTopIndex(VisibleEntriesCount - 1);
    SB_LINEDOWN  : SetTopIndex(FTopIndex + 1);
    SB_LINEUP    : SetTopIndex(FTopIndex - 1);
    SB_PAGEDOWN  : SetTopIndex(FTopIndex + ClientHeight div FEntryHeight);
    SB_PAGEUP    : SetTopIndex(FTopIndex - ClientHeight div FEntryHeight);
    SB_THUMBTRACK: SetTopIndex(Msg.Pos);
  end;
end;

procedure TCustomEntryView.WMHScroll(var Msg: TWMScroll);
begin
  // we do not scroll left/right
  HideNativeScrollBars;
end;

procedure TCustomEntryView.WMTimer(var Msg: TWMTimer);
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

procedure TCustomEntryView.WMSetFocus(var Msg: TWMSetFocus);
begin
  HideNativeScrollBars;
  ShowModernScrollBar;
  inherited;
  Invalidate;
end;

procedure TCustomEntryView.WMKillFocus(var Msg: TWMKillFocus);
begin
  HideModernScrollBar;
  inherited;
//  FMouseCapture := False;
//  FDownIndex := -1;
  Invalidate;
end;

initialization
  CheckeredBitmap := GetBitmap(clBtnFace, clBtnHighlight);

finalization
  CheckeredBitmap.Free;

end.
