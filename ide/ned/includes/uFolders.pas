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
  Forms,
  ImgList,
  CommCtrl;

const
  CM_FOLDERBASE       = $AA00;
  CM_FOLDERIMAGES     = CM_FOLDERBASE + 1;
  CM_ITEMIMAGES       = CM_FOLDERIMAGES + 1;
  CM_ITEMDETAILS      = CM_ITEMIMAGES + 1;
  CN_ITEMCLICK        = CM_ITEMDETAILS + 1;

{ TFolderItem }

type
  TFolderItems = class;
  TFolderBar = class;
  TFolderBars = class;

  TFolderItemDetails = record
    BorderStyle: TborderStyle;
    Selected: TFolderBar;
    ClientRect: TRect;
    ItemHeight: Integer;
    FolderHeight: Integer;
  end;
  PFolderItemDetails = ^TFolderItemDetails;

  TUserFreeProc = procedure (var AData: Pointer) of object;

  TFolderItem = class(TCollectionItem)
  private
    FCaption: string;
    FEnabled: Boolean;
    FVisible: Boolean;
    FImageIndex: Integer;
    FData: Pointer;
    FDataObject: Boolean;
    FUserFreeProc: TUserFreeProc;
    //
    procedure SetCaption(const Value: string);
    procedure SetEnabled(Value: Boolean);
    procedure SetVisible(Value: Boolean);
    procedure SetImageIndex(Value: Integer);
    //
    function GetDisplayRect: TRect;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    //
    procedure Assign(Source: TPersistent); override;
    procedure Click;
    property DisplayRect: TRect read GetDisplayRect;
    property Data: Pointer read FData write FData;
    property DataObject: Boolean read FDataObject write FDataObject;
  published
    property Caption: string read FCaption write SetCaption;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Visible: Boolean read FVisible write SetVisible;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property UserFreeProc: TUserFreeProc read FUserFreeProc write FUserFreeProc;
  end;

{ TFolderItems }

  TFolderItems = class(TCollection)
  private
    FFolder: TFolderBar;
    FControl: TControl;
  protected
    procedure Update(Item: TCollectionItem); override;
    function Get(Index: Integer): TFolderItem;
    procedure Put(Index: Integer; Value: TFolderItem);
  public
    constructor Create(Control: TControl; Folder: TFolderBar);
    //
    procedure Assign(Source: TPersistent); override;
    function Add: TFolderItem;
    function Insert(Index: Integer): TFolderItem;
    property Items[Index: Integer]: TFolderItem read Get write Put; default;
    property Folder: TFolderBar read FFolder;
    property Control: TControl read FControl;
  end;

{ TFolderBar }

  TFolderBar = class(TCollectionItem)
  private
    FCaption: string;
    FVisible: Boolean;
    FImageIndex: Integer;
    FData: Pointer;
    FDataObject: Boolean;
    FItems: TFolderItems;
    FSelectedIndex: Integer;
    FTopIndex: Integer;
    FUserFreeProc: TUserFreeProc;
    //
    procedure SetCaption(const Value: string);
    procedure SetVisible(Value: Boolean);
    procedure SetImageIndex(Value: Integer);
    procedure SetItems(Value: TFolderItems);
    procedure SetSelectedIndex(Value: Integer);
    procedure SetTopIndex(Value: Integer);
    //
    function GetDisplayRect: TRect;
  protected
    function GetNearestTop: Integer;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    //
    procedure Assign(Source: TPersistent); override;
    property Data: Pointer read FData write FData;
    property DataObject: Boolean read FDataObject write FDataObject;
    property DisplayRect: TRect read GetDisplayRect;
    property TopIndex: Integer read FTopIndex write SetTopIndex;
  published
    property Caption: string read FCaption write SetCaption;
    property Visible: Boolean read FVisible write SetVisible;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Items: TFolderItems read FItems write SetItems;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property UserFreeProc: TUserFreeProc read FUserFreeProc write FUserFreeProc;
  end;

{ TFolderBars }

  TFolderBars = class(TCollection)
  private
    FControl: TControl;
  protected
    procedure Update(Item: TCollectionItem); override;
    function Get(Index: Integer): TFolderBar;
    procedure Put(Index: Integer; Value: TFolderBar);
  public
    constructor Create(Control: TControl);
    procedure Assign(Source: TPersistent); override;
    function Add: TFolderBar;
    function Insert(Index: Integer): TFolderBar;
    property Items[Index: Integer]: TFolderBar read Get write Put; default;
    property Control: TControl read FControl;
  end;

{ TCustomFolderView }

  TFolderItemEvent = procedure(Sender: TObject; Item: TFolderItem) of object;

  TFolderScrollButton = (fbScrollUp, fbScrollDown);
  TFolderScrollButtons = set of TFolderScrollButton;

  TCustomFolderView = class(TCustomControl)
  private
    FActiveIndex: Integer;
    FBorderStyle: TBorderStyle;
    FButtons: TFolderScrollButtons;
    FCaptureItem: TFolderItem;
    FChangeLink: TChangeLink;
    FFolderHeight: Integer;
    FFolderImages: TCustomImageList;
    FFolders: TFolderBars;
    FItemImages: TCustomImageList;
    FItemHeight: Integer;
    FMouseItem: TFolderItem;
    FOverlay: TPicture;
    FSelected: TFolderBar;
    FTextHeight: Integer;
    FOnChange: TNotifyEvent;
    FOnItemClick: TFolderItemEvent;
    procedure ImagesChange(Sender: TObject);
    procedure OverlayChange(Sender: TObject);
    procedure SetActiveIndex(Value: Integer);
    procedure SetBorderStyle(Value: TBorderStyle);
    function GetButtonRect(Button: TFolderScrollButton): TRect;
    procedure SetCaptureItem(Value: TFolderItem);
    procedure SetFolderHeight(Value: Integer);
    procedure SetFolderImages(Value: TCustomImageList);
    procedure SetFolders(Value: TFolderBars);
    procedure SetItemImages(Value: TCustomImageList);
    procedure SetOverlay(Value: TPicture);
    procedure SetSelected(Value: TFolderBar);
    function GetSelectedRect: TRect;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMFolderImages(var Message: TMessage); message CM_FOLDERIMAGES;
    procedure CMItemImages(var Message: TMessage); message CM_ITEMIMAGES;
    procedure CMItemDetails(var Message: TMessage); message CM_ITEMDETAILS;
    procedure CNItemClick(var Message: TMessage); message CN_ITEMCLICK;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
  protected
    procedure CreateHandle; override;
    procedure DoItemClick(Item: TFolderItem); dynamic;
    function ItemFromPoint(X, Y: Integer): TFolderItem;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure UpdateImages(var InternalImages: TCustomImageList; ExternalImages: TCustomImageList);
    property ActiveIndex: Integer read FActiveIndex write SetActiveIndex;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property ButtonRect[Button: TFolderScrollButton]: TRect read GetButtonRect;
    property CaptureItem: TFolderItem read FCaptureItem write SetCaptureItem;
    property FolderHeight: Integer read FFolderHeight write SetFolderHeight;
    property FolderImages: TCustomImageList read FFolderImages write SetFolderImages;
    property Overlay: TPicture read FOverlay write SetOverlay;
    property Folders: TFolderBars read FFolders write SetFolders;
    property ItemImages: TCustomImageList read FItemImages write SetItemImages;
    property Selected: TFolderBar read FSelected write SetSelected;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnItemClick: TFolderItemEvent read FOnItemClick write FOnItemClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update; override;
    function FolderFromPoint(X, Y: Integer): TFolderBar;
  end;

{ TFolderView }

  TFolderView = class(TCustomFolderView)
  public
    property Selected;
    property FolderHeight;
  published
    property Align;
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
    property FolderImages;
    property Folders;
    property ItemImages;
    property Overlay;
    property OnItemClick;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property OnCanResize;
    property OnDblClick;
    property OnChange;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{$R SHADOW.RES}

uses
  Themes,
  UXTheme;

type
  TDirection = (drLeft, drUp, drRight, drDown, drCenter, drFill, drWrap);
  //TDirections = set of TDirection;
  TDrawState = set of (dsDisabled, dsPressed, dsHot, dsFocused, dsChecked, dsExpanded, dsDefaulted, dsThin, dsFlat);
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

//------------------------------------------------------------------------------------------------------------------------------

function GetBorder: Integer;
begin
	if StyleServices.Enabled then
  	Result := 1
  else
  	Result := GetSystemMetrics(SM_CXEDGE);
end;

function AverageColor(Color: TColor): Byte;
var
	RGB: TRGBQuad absolute Color;
begin
  Color := ColorToRGB(Color);
  Result := (RGB.rgbBlue + RGB.rgbGreen + RGB.rgbRed) div 3;
end;

function GetTextColor(Background: TColor): TColor;
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

function CalculateCaptionSize(DC: HDC; const Text: string): TSize;
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

function CreateFastBitmap(Width, Height: Integer): TFastBitmap;
var
  BitmapInfo: TBitmapinfo;
begin
  Result.DC := CreateCompatibleDC(0);
  FillChar(BitmapInfo, SizeOf(BitmapInfo), #0);
  with BitmapInfo.bmiHeader do
  begin
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

procedure DestroyFastBitmap(const Bitmap: TFastBitmap);
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

function GetDetails(Widget: TThemedScrollBar): TThemedElementDetails; overload;
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

function GetDetails(Widget: TThemedComboBox): TThemedElementDetails; overload;
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

function GetDetails(Widget: TThemedHeader): TThemedElementDetails; overload;
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

function GetDetails(Widget: TThemedToolBar): TThemedElementDetails; overload;
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

procedure DrawScroll(DC: HDC; Theme: TThemedScrollBar; Rect: TRect);
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

procedure DrawThemeScroll(DC: HDC; Direction: TDirection; const Rect: TRect; State: TDrawState);
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

procedure OverwriteObject(DC: HDC; Obj: HGDIOBJ);
begin
  DeleteObject(SelectObject(DC, Obj));
end;

procedure SelectClipRect(DC: HDC; const Rect: TRect; Mode: Integer);
var
  Region: HRGN;
begin
  with Rect do
    Region := CreateRectRgn(Left, Top, Right, Bottom);
  ExtSelectClipRgn(DC, Region, Mode);
  DeleteObject(Region);
end;

procedure DrawFrame(DC: HDC; Rect: TRect; State: TDrawFrameState);
var
  PriorPen: HPEN;
  PriorPoint: TPoint;
begin
  if State = dfFlat then
    Exit;
  with Rect do
  begin
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

procedure DrawThemeBorder(DC: HDC; Color: TColor; const Rect: TRect; State: TDrawState);
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

function GetBrush(Bitmap: TBitmap): HBRUSH;
var
  LogBrush: TLogBrush;
begin
  LogBrush.lbStyle := BS_PATTERN;
  LogBrush.lbColor := 0;
  LogBrush.lbHatch := Bitmap.Handle;
  Result := CreateBrushIndirect(LogBrush);
end;

procedure DrawCaption(DC: HDC;  const Caption: string; Rect: TRect; Direction: TDirection; Enabled: Boolean = True);
var
  DrawRect: TRect;
  PriorMode: Integer;
  PriorColor: COLORREF;
begin
  DrawRect := Rect;
  PriorMode := SetBkMode(DC, TRANSPARENT);
  if Enabled then
    DrawText(DC, PChar(Caption), -1, DrawRect, Directions[Direction])
  else begin
    OffsetRect(DrawRect, 1, 1);
    PriorColor := SetTextColor(DC, GetSysColor(COLOR_BTNHIGHLIGHT));
    DrawText(DC, PChar(Caption), -1, DrawRect, Directions[Direction]);
    OffsetRect(DrawRect, -1, -1);
    SetTextColor(DC, GetSysColor(COLOR_GRAYTEXT));
    DrawText(DC, PChar(Caption), -1, DrawRect, Directions[Direction]);
    SetTextColor(DC, PriorColor);
  end;
  SetBkMode(DC, PriorMode);
end;

procedure DrawThemeThinButton(DC: HDC; const Rect: TRect; State: TDrawState);
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

function GetBitmap(ForeColor: TColor; BackColor: TColor): TBitmap;
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

{ TFolderItem }

constructor TFolderItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FEnabled := True;
  FVisible := True;
  FImageIndex := -1;
end;

destructor TFolderItem.Destroy;
begin
  if FDataObject and (FData <> Nil) then
    TObject(FData).Free
  else if (FData <> Nil) and Assigned(FUserFreeProc) then
    FUserFreeProc(FData);
  inherited Destroy;
end;

procedure TFolderItem.Assign(Source: TPersistent);
var
  Item: TFolderItem;
begin
  if Source is TFolderItem then begin
    Item := TFolderItem(Source);
    //
    Caption := Item.Caption;
    ImageIndex := Item.ImageIndex;
    Enabled := Item.Enabled;
    Visible := Item.Visible;
  end
  else
    inherited Assign(Source);
end;

procedure TFolderItem.Click;
begin
  (Collection as TFolderItems).Control.Perform(CN_ITEMCLICK, Integer(Self), 0);
end;

procedure TFolderItem.SetCaption(const Value: string);
begin
  if Value <> FCaption then begin
    FCaption := Value;
    Changed(True);
  end;
end;

procedure TFolderItem.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then begin
    FEnabled := Value;
    Changed(True);
  end;
end;

function TFolderItem.GetDisplayRect: TRect;
var
  Folder: TFolderBar;
  TopIndex: Integer;
  Details: TFolderItemDetails;
  VisibleIndex: Integer;
  I: Integer;
begin
  Folder := (Collection as TFolderItems).Folder;
  TopIndex := (Collection as TFolderItems).Control.Perform(CM_ITEMDETAILS, Integer(@Details), 0);
  if TopIndex = 0 then begin
    SetRectEmpty(Result);
    Exit;
  end;
  TopIndex := Folder.GetNearestTop;
  if (Folder = Details.Selected) and (TopIndex < Index + 1) and FVisible then begin
    Result := Folder.DisplayRect;
    OffsetRect(Result, 0, Details.FolderHeight);
    VisibleIndex := 0;
    for I := Folder.GetNearestTop to Index - 1 do
      if Folder.Items[I].Visible then
        Inc(VisibleIndex);
    Result.Top := Result.Top + (Details.ItemHeight * VisibleIndex);
    Result.Bottom := Result.Top + Details.ItemHeight;
  end
  else
    SetRectEmpty(Result);
end;

procedure TFolderItem.SetImageIndex(Value: Integer);
begin
  if Value <> FImageIndex then begin
    FImageIndex := Value;
    Changed(True);
  end;
end;

procedure TFolderItem.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then begin
    FVisible := Value;
    Changed(True);
  end;
end;

{ TFolderItems }

constructor TFolderItems.Create(Control: TControl; Folder: TFolderBar);
begin
  inherited Create(TFolderItem);
  FFolder := Folder;
  FControl := Control;
end;

procedure TFolderItems.Assign(Source: TPersistent);
var
  FolderItems: TFolderItems;
  I: Integer;
begin
  if Source is TFolderItems then begin
    FolderItems := TFolderItems(Source);
    //
    BeginUpdate;
    Clear;
    for I := 0 to FolderItems.Count - 1 do
      Add.Assign(FolderItems[I]);
    EndUpdate;
  end
  else
    inherited Assign(Source);
end;

function TFolderItems.Add: TFolderItem;
begin
  Result := inherited Add as TFolderItem;
end;

function TFolderItems.Insert(Index: Integer): TFolderItem;
begin
  Result := inherited Insert(Index) as TFolderItem;
end;

procedure TFolderItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  TFolderBars(FFolder.Collection).Changed;
end;

function TFolderItems.Get(Index: Integer): TFolderItem;
begin
  Result := GetItem(Index) as TFolderItem;
end;

procedure TFolderItems.Put(Index: Integer; Value: TFolderItem);
begin
  SetItem(Index, Value);
end;

{ TFolderBar }

var
  CheckeredBitmap: TBitmap = Nil;

constructor TFolderBar.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FVisible := True;
  FItems := TFolderItems.Create((Collection as TFolderBars).Control, Self);
  FImageIndex := -1;
  FSelectedIndex := -1;
end;

procedure TFolderBar.Assign(Source: TPersistent);
var
  Bar: TFolderBar;
begin
  if Source is TFolderBar then begin
    Bar := TFolderBar(Source);
    //
    Caption := Bar.Caption;
    ImageIndex := Bar.ImageIndex;
    SelectedIndex := Bar.SelectedIndex;
    Visible := Bar.Visible;
    Items.Assign(Bar.Items);
  end
  else
    inherited Assign(Source);
end;

destructor TFolderBar.Destroy;
begin
  FItems.Free;
  if FDataObject and (FData <> Nil) then
    TObject(FData).Free
  else if (FData <> Nil) and Assigned(FUserFreeProc) then
    FUserFreeProc(FData);
  inherited Destroy;
end;

procedure TFolderBar.SetCaption(const Value: string);
begin
  if Value <> FCaption then begin
    FCaption := Value;
    Changed(True);
  end;
end;

function TFolderBar.GetDisplayRect: TRect;
var
  Folders: TFolderBars;
  Details: TFolderItemDetails;
  Rect: TRect;
  I: Integer;
begin
  Folders := Collection as TFolderBars;
  if Folders.Control.Perform(CM_ITEMDETAILS, Integer(@Details), 0) = 0 then begin
    SetRectEmpty(Result);
    Exit;
  end;
  if Visible then begin
    Result := Details.ClientRect;
    if (Details.Selected <> Nil) and (Index > Details.Selected.Index) then
      Result.Top := Result.Bottom - Details.FolderHeight * (Folders.Count - Index)
    else
      Result.Top := Index * Details.FolderHeight;
    Result.Bottom := Result.Top + Details.FolderHeight;
    if Details.BorderStyle = bsSingle then begin
      InflateRect(Result, -GetBorder, 0);
      if (Details.Selected <> Nil) and (Index > Details.Selected.Index) then
        OffsetRect(Result, 0, -GetBorder)
      else
        OffsetRect(Result, 0, GetBorder);
    end;
    for I := Index - 1 downto 0 do
      if Folders[I].Visible then begin
        Rect := Folders[I].DisplayRect;
        if Result.Top < Rect.Bottom then
          OffsetRect(Result, 0, Rect.Bottom - Result.Top)
        else if (Details.Selected = Nil) or (Index <= Details.Selected.Index) then
          OffsetRect(Result, 0, Rect.Bottom - Result.Top);
        Break;
      end
      else if I = 0 then
        OffsetRect(Result, 0, -Result.Top);
  end
  else
    SetRectEmpty(Result);
end;

procedure TFolderBar.SetImageIndex(Value: Integer);
begin
  if Value <> FImageIndex then begin
    FImageIndex := Value;
    Changed(True);
  end;
end;

procedure TFolderBar.SetItems(Value: TFolderItems);
begin
  if Value <> FItems then
    FItems.Assign(Value);
end;

function TFolderBar.GetNearestTop: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Items.Count - 1 do
    if Items[I].Visible then
      if Result = FTopIndex then begin
        Result := I;
        Break;
      end
      else
        Inc(Result);
end;

procedure TFolderBar.SetSelectedIndex(Value: Integer);
begin
  if Value <> FSelectedIndex then begin
    FSelectedIndex := Value;
    Changed(True);
  end;
end;

procedure TFolderBar.SetTopIndex(Value: Integer);
begin
  if Value > Items.Count - 1 then
    Value := Items.Count - 1;
  if Value < 0 then
    Value := 0;
  if Value <> FTopIndex then begin
    FTopIndex := Value;
    Changed(True);
  end;
end;

procedure TFolderBar.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then begin
     FVisible := Value;
     Changed(True);
  end;
end;

{ TFolderBars }

constructor TFolderBars.Create(Control: TControl);
begin
  inherited Create(TFolderBar);
  FControl := Control;
end;

procedure TFolderBars.Assign(Source: TPersistent);
var
  Bars: TFolderBars absolute Source;
  I: Integer;
begin
  if Source is TFolderBars then begin
    BeginUpdate;
    Clear;
    for I := 0 to Bars.Count - 1 do
      Add.Assign(Bars[I]);
    EndUpdate;
  end
  else
    inherited Assign(Source);
end;

function TFolderBars.Add: TFolderBar;
begin
  Result := inherited Add as TFolderBar;
end;

function TFolderBars.Insert(Index: Integer): TFolderBar;
begin
  Result := inherited Insert(Index) as TFolderBar;
end;

procedure TFolderBars.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if FControl <> Nil then
    FControl.Update;
end;

function TFolderBars.Get(Index: Integer): TFolderBar;
begin
  if (Index > -1) and (Index < Count) then
    Result := GetItem(Index) as TFolderBar
  else
    Result := Nil;
end;

procedure TFolderBars.Put(Index: Integer; Value: TFolderBar);
begin
  SetItem(Index, Value);
end;

{ TCustomFolderView }

constructor TCustomFolderView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clAppWorkspace;
  Height := 250;
  Width := 150;
  FActiveIndex := -1;
  FBorderStyle := bsSingle;
  FFolderHeight := 25;
  FItemHeight := 25;
  FFolders := TFolderBars.Create(Self);
  FOverlay := TPicture.Create;
  FOverlay.OnChange := OverlayChange;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := ImagesChange;
end;

destructor TCustomFolderView.Destroy;
begin
  UpdateImages(FFolderImages, Nil);
  UpdateImages(FItemImages, Nil);
  FFolders.Free;
  FChangeLink.Free;
  FOverlay.OnChange := Nil;
  FOverlay.Free;
  inherited Destroy;
end;

procedure TCustomFolderView.ImagesChange(Sender: TObject);
var
  DC: HDC;
  F: HFont;
  ImageHeight: Integer;
begin
  DC := GetDC(0);
  F := SelectObject(DC, Font.Handle);
  FTextHeight := CalculateCaptionSize(DC, ' ').cY;
  SelectObject(DC, F);
  ReleaseDC(0, DC);
  if FFolderImages <> Nil then
    ImageHeight := FFolderImages.Height
  else
    ImageHeight := 0;
  if ImageHeight > FTextHeight then
    FFolderHeight := ImageHeight
  else
    FFolderHeight := FTextHeight;
  Inc(FFolderHeight, 10);
  if FItemImages <> Nil then
    ImageHeight := FItemImages.Height
  else
    ImageHeight := 0;
  if ImageHeight > FTextHeight then
    FItemHeight := ImageHeight
  else
    FItemHeight := FTextHeight;
  Inc(FItemHeight, Round(FTextHeight * 1.5));
//  Inc(FItemHeight, 10);
  Invalidate;
end;

procedure TCustomFolderView.DoItemClick(Item: TFolderItem);
begin
  CaptureItem := Nil;
  if Assigned(FOnItemClick) then
    FOnItemClick(Self, Item);
end;

procedure TCustomFolderView.CreateHandle;
begin
  inherited CreateHandle;
  if FTextHeight = 0 then
    FTextHeight := Canvas.TextHeight(' ');
end;

function TCustomFolderView.FolderFromPoint(X, Y: Integer): TFolderBar;
var
  P: TPoint;
  I: Integer;
begin
  P := Point(X, Y);
  Result := Nil;
  for I := 0 to FFolders.Count - 1 do
    if PtInRect(Folders[I].DisplayRect, P) then begin
      Result := FFolders[I];
      Break;
    end;
end;

function TCustomFolderView.ItemFromPoint(X, Y: Integer): TFolderItem;
var
  P: TPoint;
  I: Integer;
begin
  P := Point(X, Y);
  Result := Nil;
  if FolderFromPoint(X, Y) <> Nil then
    Exit;
  if Selected <> Nil then
    for I := 0 to Selected.Items.Count - 1 do
      if PtInRect(Selected.Items[I].DisplayRect, P) then begin
        Result := Selected.Items[I];
        Break;
      end;
end;

procedure TCustomFolderView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_UP:
      if (Selected <> Nil) and (Selected.Index > 0) then
        Selected := FFolders[Selected.Index - 1];
    VK_DOWN:
      if (Selected <> Nil) and (Selected.Index < FFolders.Count - 1) then
        Selected := FFolders[Selected.Index + 1];
  end;
end;

procedure TCustomFolderView.Loaded;
begin
  inherited Loaded;
  ActiveIndex := FActiveIndex;
end;

procedure TCustomFolderView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Point: TPoint;
  Folder: TFolderBar;
  ScrollButton: TFolderScrollButton;
  Rect: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then begin
    SetFocus;
    Point := GetPoint(X, Y);
    FButtons := [];
    for ScrollButton := Low(TFolderScrollButton) to High(TFolderScrollButton) do
      if PtInRect(ButtonRect[ScrollButton], Point) then begin
        Include(FButtons, ScrollButton);
        Rect := ButtonRect[ScrollButton];
        InvalidateRect(Handle, @Rect, False);
        if CaptureItem <> Nil then begin
          Rect := CaptureItem.DisplayRect;
          InvalidateRect(Handle, @Rect, False);
        end;
        CaptureItem := Nil;
        SetTimer(Handle, 1, 125, Nil);
        Exit;
      end;
    Folder := FolderFromPoint(X, Y);
    if Folder <> Nil then begin
      Selected := Folder;
      Exit;
    end;
    CaptureItem := ItemFromPoint(X, Y);
    if CaptureItem <> Nil then begin
      Rect := CaptureItem.DisplayRect;
      InvalidateRect(Handle, @Rect, False);
    end;
  end;
end;

procedure TCustomFolderView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if not MouseCapture then
    CaptureItem := ItemFromPoint(X, Y);
end;

procedure TCustomFolderView.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Point: TPoint;
  ScrollButton: TFolderScrollButton;
  Rect: TRect;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if not (csDesigning in ComponentState) then
    if Button = mbLeft then begin
      Point := GetPoint(X, Y);
      if FButtons <> [] then
        for ScrollButton := Low(TFolderScrollButton) to High(TFolderScrollButton) do
          if PtInRect(ButtonRect[ScrollButton], Point) then
            if ScrollButton in FButtons then begin
              CaptureItem := Nil;
              FButtons := [];
              Exit;
            end;
      if FButtons <> [] then begin
        FButtons := [];
        Invalidate;
      end;
      if CaptureItem <> Nil then begin
        Rect := CaptureItem.DisplayRect;
        InvalidateRect(Handle, @Rect, False);
        if CaptureItem = ItemFromPoint(X, Y) then
          DoItemClick(CaptureItem);
      end;
      with Mouse.CursorPos do
        CaptureItem := ItemFromPoint(X, Y);
    end;
end;

procedure TCustomFolderView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent <> Nil) then
    if AComponent = FFolderImages then
      UpdateImages(FFolderImages, Nil)
    else if AComponent = FItemImages then
      UpdateImages(FItemImages, Nil);
end;

procedure TCustomFolderView.Update;
begin
  ActiveIndex := FActiveIndex;
end;

procedure TCustomFolderView.UpdateImages(var InternalImages: TCustomImageList;
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

procedure TCustomFolderView.OverlayChange(Sender: TObject);
begin
  FOverlay.Bitmap.Transparent := True;
  FOverlay.Bitmap.TransparentMode := tmAuto;
  Invalidate;
end;

procedure TCustomFolderView.Paint;
const
  Shadow: HICON = 0;

  procedure DrawButtons(DC: HDC; Brush: HBRUSH);
  const
    Arrows: array[TFolderScrollButton] of TDirection = (drUp, drDown);
  var
    Button: TFolderScrollButton;
    Rect: TRect;
    Down: Boolean;
    State: TDrawState;
  begin
    for Button := Low(TFolderScrollButton) to High(TFolderScrollButton) do begin
      Rect := ButtonRect[Button];
      if not IsRectEmpty(Rect) then begin
        if not StyleServices.Available then
          FillRect(DC, Rect, COLOR_3DFACE + 1);
        Down := MouseCapture and (Button in FButtons);
        if Down then
          State := [dsPressed]
        else
          State := [dsHot];
        DrawThemeScroll(DC, Arrows[Button], Rect, State);
        //SelectClipRect(DC, Rect, RGN_DIFF);
      end;
    end;
  end;

var
  DC: HDC;
  Rect: TRect;
  DrawRect: TRect;
  Brush: HBRUSH;
//  HotBrush: HBRUSH;
  CheckeredBrush: HBRUSH;
  Folder: TFolderBar;
  Item: TFolderItem;
  Point: TPoint;
  I: Integer;
  J: Integer;
begin
  inherited Paint;
  if (Shadow = 0) and StyleServices.Enabled then
    Shadow := LoadIcon(MainInstance, 'SHADOW');
  Rect := ClientRect;
  DC := Canvas.Handle;
  SetBkMode(DC, OPAQUE);
  Brush := CreateSolidBrush(ColorToRGB(Color));
  FillRect(DC, Rect, Brush);
  if FBorderStyle = bsSingle then begin
    DrawThemeBorder(DC, Color, Rect, []);
    InflateRect(Rect, -GetBorder, -GetBorder);
    //SelectClipRect(DC, Rect, RGN_AND);
  end;
//  Brush := GetSysColorBrush(Color); //  COLOR_BTNFACE);
  CheckeredBrush := GetBrush(CheckeredBitmap);
  for I := 0 to FFolders.Count - 1 do begin
    Folder := FFolders[I];
    if not Folder.Visible then
      Continue;
    DrawRect := Folder.DisplayRect;
    if StyleServices.Enabled then begin
      SetBkMode(DC, OPAQUE);
      FillRect(DC, DrawRect, ColorToRGB(Color));
			Inc(DrawRect.Right, 10);
      if Folder = Selected then
        StyleServices.DrawElement(DC, GetDetails(thHeaderItemHot), DrawRect)
      else
        StyleServices.DrawElement(DC, GetDetails(thHeaderItemNormal), DrawRect);
			Dec(DrawRect.Right, 10);
    end
    else begin
      DrawFrame(DC, DrawRect, dfRaised);
      InflateRect(DrawRect, -1, -1);
      if Folder = Selected then
        FillRect(DC, DrawRect, CheckeredBrush)
      else
        FillRect(DC, DrawRect, Brush);
    end;
    InflateRect(DrawRect, -6, 0);
    if FFolderImages <> Nil then begin
      if (Folder = Selected) and (Folder.SelectedIndex > -1) then begin
        ImageList_DrawEx(FFolderImages.Handle, Folder.SelectedIndex, DC, DrawRect.Left, DrawRect.Top + (FFolderHeight - FFolderImages.Height) div 2, 0, 0, CLR_NONE, CLR_NONE, ILD_NORMAL);
      end
      else if Folder.ImageIndex > -1 then
        ImageList_DrawEx(FFolderImages.Handle, Folder.ImageIndex, DC, DrawRect.Left, DrawRect.Top + (FFolderHeight - FFolderImages.Height) div 2, 0, 0, CLR_NONE, CLR_NONE, ILD_NORMAL);
      Inc(DrawRect.Left, FFolderImages.Width + 2);
    end;
    InflateRect(DrawRect, -1, -1);
    SetTextColor(DC, GetSysColor(COLOR_WINDOWTEXT));
    SetBkMode(DC, TRANSPARENT);
    DrawCaption(DC, Folder.Caption, DrawRect, drLeft);
    InflateRect(DrawRect, 2, 2);
    if FFolderImages <> Nil then
      Dec(DrawRect.Left, FFolderImages.Width + 2);
    DrawRect := Folder.DisplayRect;
//    SelectClipRect(DC, DrawRect, RGN_DIFF);
    if FOverlay <> Nil then
      BitBlt(DC, DrawRect.Left, DrawRect.Bottom, DrawRect.Width, DrawRect.Height, FOverlay.Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
  end;
  DeleteObject(Brush);
  Folder := Selected;
  if Folder <> Nil then begin
    Brush := CreateSolidBrush(ColorToRGB(Color));
    //HotBrush := CreateSolidBrush(ColorToRGB(clHighlight));
    for J := Folder.GetNearestTop to Folder.Items.Count - 1 do begin
      Item := Folder.Items[J];
      if not Item.Visible then
        Continue;
      DrawRect := Item.DisplayRect;
      SetBkMode(DC, OPAQUE);
      FillRect(DC, DrawRect, Brush);
      Point := DrawRect.TopLeft;
      if FItemImages <> Nil then
        //Point.X := (DrawRect.Right - DrawRect.Left) div 2 - FItemImages.Width div 2;
        Point.X := DrawRect.Left + FItemImages.Width;
      Point.Y := DrawRect.Top + (DrawRect.Bottom - DrawRect.Top - FItemImages.Width) div 2;
      if (FCaptureItem <> Nil) and (FCaptureItem = Item) then begin
        //FillRect(DC, DrawRect, HotBrush);
        if StyleServices.Enabled then begin
          DrawIconEx(DC, DrawRect.Left, DrawRect.Top, Shadow, 1, 1, 0 , 0, DI_NORMAL);
          DrawIconEx(DC, DrawRect.Left, DrawRect.Top, Shadow, WidthOf(DrawRect), HeightOf(DrawRect), 0 , 0, DI_NORMAL);
          if MouseCapture then begin
            DrawIconEx(DC, DrawRect.Right - 1, DrawRect.Top, Shadow, 1, HeightOf(DrawRect), 0 , 0, DI_NORMAL);
            DrawIconEx(DC, DrawRect.Left + 1, DrawRect.Bottom - 1, Shadow, WidthOf(DrawRect) - 2, 1, 0 , 0, DI_NORMAL);
            DrawIconEx(DC, DrawRect.Left, DrawRect.Top, Shadow, 2, HeightOf(DrawRect), 0 , 0, DI_NORMAL);
            DrawIconEx(DC, DrawRect.Left + 2, DrawRect.Top, Shadow, WidthOf(DrawRect) - 4, 2, 0 , 0, DI_NORMAL);
            DrawIconEx(DC, DrawRect.Left, DrawRect.Top, Shadow, WidthOf(DrawRect), HeightOf(DrawRect), 0 , 0, DI_NORMAL);
          end;
        end
        else if MouseCapture then
          DrawThemeThinButton(DC, DrawRect, [dsPressed])
        else
          DrawThemeThinButton(DC, DrawRect, [dsHot]);
        InflateRect(DrawRect, -1, -1);
        if not StyleServices.Enabled and (Overlay = Nil) then
          FillRect(DC, DrawRect, Brush);
      end
      else if Overlay = Nil then
        FillRect(DC, DrawRect, Brush);
      Dec(DrawRect.Right, 4);
      if (FCaptureItem <> Nil) and (FCaptureItem = Item) then
        if MouseCapture then begin
          OffsetRect(DrawRect, 1, 1);
          Inc(Point.X);
          Inc(Point.Y);
        end;
      Inc(DrawRect.Left, 4);
      if (FItemImages <> Nil) and (Item.ImageIndex > -1) then begin
        ImageList_DrawEx(FItemImages.Handle, Item.ImageIndex, DC, Point.X, Point.Y, 0, 0, CLR_NONE, CLR_NONE, ILD_NORMAL);
        Inc(DrawRect.Left, Point.X + FItemImages.Width);
        if not Item.Enabled and (FOverlay.Height > 0) then
          Canvas.Draw(DrawRect.Left + (DrawRect.Right - DrawRect.Left - FOverlay.Width) div 2 - FTextHeight div 4, Top + (DrawRect.Bottom - DrawRect.Top - FOverlay.Height) div 2 - FTextHeight div 2, FOverlay.Bitmap);
      end;
      SetTextColor(DC, GetTextColor(Color));
      SetBkMode(DC, TRANSPARENT);
      if (FCaptureItem <> Nil) and (FCaptureItem = Item) then
        OffsetRect(DrawRect, 0, 1);
      Dec(DrawRect.Right, 4);
      //DrawRect.Top := DrawRect.Bottom - Trunc(FTextHeight * 1.5);
      DrawCaption(DC, Item.Caption, DrawRect, drLeft);
//      if (FCaptureItem <> Nil) and (FCaptureItem = Item) then begin
//        InflateRect(DrawRect, 1, 1);
//        if MouseCapture then
//          OffsetRect(DrawRect, -1, -1);
//      end;
    end;
    DeleteObject(Brush);
    //DeleteObject(HotBrush);
    DrawButtons(DC, CheckeredBrush);
  end;
  DeleteObject(CheckeredBrush);
  Item := Nil;
  if Selected <> Nil then
    for I := Selected.Items.Count - 1 downto 0 do
      if Selected.Items[I].Visible then begin
        Item := Selected.Items[I];
        Break;
      end;
  if Item <> Nil then begin
    Rect.Top := Item.DisplayRect.Bottom;
    I := Selected.Index + 1;
    for J := I to Folders.Count - 1 do
      if Folders[J].Visible then begin
        Rect.Bottom := Folders[J].DisplayRect.Top;
        Break;
      end;
    if Rect.Bottom < Rect.Top then
      Rect.Bottom := Rect.Top;
  end
  else if Selected <> Nil then begin
    Rect.Top := Selected.DisplayRect.Top + FFolderHeight;
    I := Selected.Index + 1;
    for J := Folders.Count - 1 downto I do
      if Folders[J].Visible then begin
        Rect.Bottom := Folders[J].DisplayRect.Top;
        Break;
      end;
  end
  else begin
    Rect.Top := 0;
    for I := Folders.Count - 1 downto 0 do
      if Folders[I].Visible then begin
        Rect.Top := Folders[I].DisplayRect.Bottom;
        Break;
      end;
  end;
  if (Overlay = Nil) or (Selected = Nil) then begin
  	Brush := CreateSolidBrush(ColorToRGB(Color));
    FillRect(DC, Rect, Brush);
  	DeleteObject(Brush);
  end;
end;

procedure TCustomFolderView.SetActiveIndex(Value: Integer);
var
  Form: TCustomForm;
begin
  FActiveIndex := Value;
  if csLoading in ComponentState then
    Exit;
  if FActiveIndex < -1 then
    FActiveIndex := -1;
  if FActiveIndex > FFolders.Count - 1 then
    FActiveIndex := FFolders.Count - 1;
  if (FActiveIndex > -1) and (FFolders[FActiveIndex].Visible) then
    FSelected := FFolders[FActiveIndex]
  else
    FSelected := Nil;
  Invalidate;
  if csDesigning in ComponentState then begin
    Form := GetParentForm(Self);
    if Form <> Nil then
      Form.Designer.Modified;
  end;
end;

procedure TCustomFolderView.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

function TCustomFolderView.GetButtonRect(Button: TFolderScrollButton): TRect;
var
	ButtonHeight: Integer;
  Rect: TRect;
  Counter: Integer;
  I: Integer;
begin
  ButtonHeight := GetSystemMetrics(SM_CXVSCROLL) + 4;
  SetRectEmpty(Result);
  if Selected <> Nil then begin
    Rect := GetSelectedRect;
    if HeightOf(Rect) > ButtonHeight * 2 + 8 then
      case Button of
        fbScrollUp: begin
          if Selected.TopIndex > 0 then
            Result := GetRect(Rect.Right - ButtonHeight - 4, Rect.Top + 4, Rect.Right - 4, Rect.Top + ButtonHeight + 4);
        end;
        fbScrollDown: begin
          Counter := 0;
          for I := Selected.Items.Count - 1 downto 1 do begin
            if Selected.Items[I].Visible then begin
              if Counter = 1 then
                Break;
              if Selected.Items[I].DisplayRect.Bottom > Rect.Bottom then
                Result := GetRect(Rect.Right - ButtonHeight - 4, Rect.Bottom - ButtonHeight - 4, Rect.Right - 4, Rect.Bottom - 4);
              Inc(Counter);
            end;
          end;
          if Counter = 0 then
            SetRectEmpty(Result);
        end;
      end;
  end;
end;

procedure TCustomFolderView.SetCaptureItem(Value: TFolderItem);
var
  Rect: tRect;
begin
  if FButtons <> [] then
    Value := Nil;
  if Value <> FCaptureItem then begin
    if FCaptureItem <> Nil then begin
      Rect := FCaptureItem.DisplayRect;
      InvalidateRect(Handle, @Rect, False);
    end;
    FCaptureItem := Value;
    if Value <> Nil then
      if FCaptureItem.Enabled then begin
        Rect := FCaptureItem.DisplayRect;
        InvalidateRect(Handle, @Rect, False);
      end
      else
        FCaptureItem := Nil;
  end;
end;

procedure TCustomFolderView.SetFolderHeight(Value: Integer);
begin
  if Value <> FFolderHeight then begin
    FFolderHeight := Value;
    Invalidate;
  end;
end;

procedure TCustomFolderView.SetFolderImages(Value: TCustomImageList);
begin
  if Value <> FFolderImages then begin
    UpdateImages(FFolderImages, Value);
    ImagesChange(FFolderImages);
  end;
end;

procedure TCustomFolderView.SetFolders(Value: TFolderBars);
begin
  if Value <> FFolders then
    FFolders.Assign(Value);
end;

procedure TCustomFolderView.SetItemImages(Value: TCustomImageList);
begin
  if Value <> FItemImages then begin
    UpdateImages(FItemImages, Value);
    ImagesChange(FItemImages);
  end;
end;

procedure TCustomFolderView.SetOverlay(Value: TPicture);
begin
  FOverlay.Assign(Value);
  Invalidate;
end;

procedure TCustomFolderView.SetSelected(Value: TFolderBar);
begin
  if Value <> FSelected then begin
    FSelected := Value;
    if FSelected = Nil then
      FActiveIndex := -1
    else
      FActiveIndex := FSelected.Index;
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

function TCustomFolderView.GetSelectedRect: TRect;
var
  I: Integer;
begin
  Result := ClientRect;
  if FBorderStyle = bsSingle then
    InflateRect(Result, -GetBorder, -GetBorder);
  if Selected <> Nil then begin
    Result.Top := Selected.DisplayRect.Bottom;
    for I := Selected.Index + 1 to Folders.Count - 1 do
      if Folders[I].Visible then begin
        Result.Bottom := Folders[I].DisplayRect.Top;
        Break;
      end;
  end
  else
    SetRectEmpty(Result);
end;

procedure TCustomFolderView.CMDesignHitTest(var Message: TCMDesignHitTest);
const
  HitTests: array[Boolean] of Integer = (0, 1);
begin
  inherited;
  with Message do
    Result := HitTests[FolderFromPoint(XPos, YPos) <> Nil];
end;

procedure TCustomFolderView.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font := Font;
  FTextHeight := Canvas.TextHeight(' ');
  if FFolderImages <> Nil then
    ImagesChange(FFolderImages);
  if FItemImages <> Nil then
    ImagesChange(FItemImages);
end;

procedure TCustomFolderView.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  CaptureItem := FMouseItem;
  FMouseItem := Nil;
end;

procedure TCustomFolderView.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if MouseCapture then
    FMouseItem := CaptureItem
  else
    FMouseItem := Nil;
  CaptureItem := Nil;
end;

procedure TCustomFolderView.CMFolderImages(var Message: TMessage);
begin
	Message.Result := Integer(FFolderImages);
end;

procedure TCustomFolderView.CMItemImages(var Message: TMessage);
begin
	Message.Result := Integer(FItemImages);
end;

procedure TCustomFolderView.CMItemDetails(var Message: TMessage);
var
  Details: PFolderItemDetails;
begin
  Details := PFolderItemDetails(Message.WParam);
  Details.BorderStyle := BorderStyle;
  Details.Selected := Selected;
  Details.ClientRect := ClientRect;
  Details.ItemHeight := FItemHeight;
  Details.FolderHeight := FFolderHeight;
  Message.Result := 1;
end;

procedure TCustomFolderView.CNItemClick(var Message: TMessage);
begin
  DoItemClick(TFolderItem(Message.WParam));
  Message.Result := 1;
end;

procedure TCustomFolderView.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TCustomFolderView.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TCustomFolderView.WMMouseWheel(var Message: TWMMouseWheel);
var
  Delta: Integer;
begin
  if Selected <> Nil then begin
    Delta := -Message.WheelDelta div 120;
    if (not IsRectEmpty(ButtonRect[fbScrollUp])) and (Delta = -1) then
      Selected.TopIndex := Selected.TopIndex - 1
    else if (not IsRectEmpty(ButtonRect[fbScrollDown])) and (Delta = 1) then
      Selected.TopIndex := Selected.TopIndex + 1;
    if not MouseCapture then
      with Message, ScreenToClient(Point(XPos, YPos)) do
          CaptureItem := ItemFromPoint(X, Y);
  end;
  inherited;
end;

procedure TCustomFolderView.WMTimer(var Message: TWMTimer);
begin
  if FButtons = [] then begin
    KillTimer(Handle, 1);
    Exit;
  end;
  with Selected do
    if fbScrollUp in FButtons then begin
      TopIndex := TopIndex - 1;
      if IsRectEmpty(ButtonRect[fbScrollUp]) then
        FButtons := [];
    end
    else begin
      TopIndex := TopIndex + 1;
      if IsRectEmpty(ButtonRect[fbScrollDown]) then
        FButtons := [];
    end;
  if FButtons = [] then
    KillTimer(Handle, 1);
end;

initialization
  CheckeredBitmap := GetBitmap(clBtnFace, clBtnHighlight);
  
finalization
  CheckeredBitmap.Free;
  
end.
