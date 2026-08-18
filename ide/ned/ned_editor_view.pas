unit ned_editor_view;

interface

uses
  SysUtils,
  Messages,
  Classes,
  Controls,
  Forms,
  Graphics,
  Types,
  ExtCtrls,
  CommCtrl,
  Generics.Collections,
  ned_editor_buffer;

type
  TNEDCommandShortCut = Low(LongWord)..High(LongWord); // 2 words 0..32767 + 0..32767
  TNEDCommandIDType = 0..High(Word); // 0..32767
  TNEDUserCommandIDType = 1000..High(Word);

const
  // cursor specific commands
  cmdCursorLeft            = 1;  // Move cursor left one char
  cmdCursorRight           = 2;  // Move cursor right one char
  cmdCursorUp              = 3;  // Move cursor up one line
  cmdCursorDown            = 4;  // Move cursor down one line
  cmdCursorLineStart       = 5;  // Move cursor to beginning of line
  cmdCursorLineEnd         = 6;  // Move cursor to end of line
  //
  cmdCursorPageUp          = 7;  // Move cursor up one page
  cmdCursorPageDown        = 8;  // Move cursor down one page
  cmdCursorPageLeft        = 9;  // Move cursor left one page
  cmdCursorPageRight       = 10; // Move cursor right one page
  cmdCursorPageTop         = 11; // Move cursor to top of page
  cmdCursorPageBottom      = 12; // Move cursor to bottom of page
  cmdCursorEditorTop       = 13; // Move cursor to absolute beginning
  cmdCursorEditorBottom    = 14; // Move cursor to absolute end
  //
  cmdCursorWordLeft        = 15; // Move cursor left one word
  cmdCursorWordRight       = 16; // Move cursor right one word
  //
  cmdCursorGotoXY          = 17; // Move cursor to specific coordinates, Data = PPoint

  // edit commands
  cmdEditDelete            = 41; // Delete last char (i.e. backspace key)
  cmdEditDeleteInPlace     = 42; // Delete char at cursor (i.e. delete key)
  cmdEditDeleteWholeWord   = 43; // Delete Word from cursor
  cmdEditDeleteBOWord      = 44; // Delete from cursor to start of word
  cmdEditDeleteEOWord      = 45; // Delete from cursor to end of word
  cmdEditDeleteBOL         = 46; // Delete from cursor to beginning of line
  cmdEditDeleteEOL         = 47; // Delete from cursor to end of line
  cmdEditDeleteLine        = 48; // Delete current line
  cmdEditClearAll          = 49; // Delete everything
  cmdEditLineBreak         = 50; // Break line at current position, move caret to new line
  cmdEditInsertLine        = 51; // Break line at current position, leave caret
  cmdEditChar              = 52; // Insert a character at current position

  // window commands
  cmdWindowSwitchToNext        = 81; // Switch to next editor window if any available
  cmdWindowSwitchToPrev        = 82; // Switch to prev editor window if any available
  cmdWindowOpenCurrentOnRight  = 83; // Open current editor on right side (split to right)
  cmdWindowOpenCurrentOnBottom = 84; // Open current editor on bottom side (split to bottom)
  cmdWindowOpenNewOnRight      = 85; // Open new editor on bottom side (split to bottom)
  cmdWindowOpenNewOnBottom     = 86; // Open new editor on bottom side (split to bottom)

type
  TNEDCustomEditorView = class;

  TNEDEditorGutterNumberingTypeEnum = (
    gnContinous, // 1.2.3.4.5.6.7.8.9.10....
    gnOriginal,  // taken form original line numbers while loading
    gnDifference // 1...10..[-12][+12]..20....
  );

  TNEDEditorGutterLineNumberingTypeEnum = (
    lnHidden,
    lnEachLine,
    lnEachTenth, // just like in delphi, 10 . . . 20 . . . 30
    lnEachTenthWithDashedFifths // just like in delphi, 10 . - . 20 . - . 30
  );

  TNEDEditorGutter = class(TPersistent)
  private
    FEditorControl: TNEDCustomEditorView;
    //
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
    FNumbersType: TNEDEditorGutterNumberingTypeEnum;
    FLineNumberType: TNEDEditorGutterLineNumberingTypeEnum;
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
    procedure SetNumbersType(const Value: TNEDEditorGutterNumberingTypeEnum);
    procedure SetLineNumberType(const Value: TNEDEditorGutterLineNumberingTypeEnum);
    procedure SetLineIndicatorWidth(const Value: Integer);
    procedure SetLineModifiedIndicatorColor(const Value: TColor);
    procedure SetLineSavedIndicatorColor(const Value: TColor);
    procedure SetLineDeletedIndicatorColor(const Value: TColor);
    procedure SetLineHiddenIndicatorColor(const Value: TColor);
    //
    function GetMaxLineNumberChars(const LineNumbers: Integer): Integer;
    function GetLineNumbersLength(const LineNumbers: Integer): Integer;
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
    property NumbersType: TNEDEditorGutterNumberingTypeEnum read FNumbersType write SetNumbersType;
    property LineNumberType: TNEDEditorGutterLineNumberingTypeEnum read FLineNumberType write SetLineNumberType;
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

  TNEDEditorCaretTypeEnum = (
    ctVerticalLine,       // "|"
    ctThinVerticalLine,
    ctHorizontalLine,     // "_"
    ctThinHorizontalLine,
    ctHalfBlock,          // "="
    ctBlock               // "#"
  );

  TNEDEditorCaretModeEnum = (
    cmInsert,
    cmOverwrite
  );

  TNEDEditorCaretInfo = record
    Size: TPoint;
    Offset: TPoint;
  end;

  TNEDCaretPosition = type TPoint;
  PNEDCaretPosition = ^TNEDCaretPosition;

  TNEDEditorCaret = class(TPersistent)
  private
    FEditorControl: TNEDCustomEditorView;
    //
    FCaretCreated: Boolean;
    FCaretEnabled: Boolean;
    FCaretVisible: Boolean;
    FCaretSizeInfo: TNEDEditorCaretInfo;
    FCaretInsertType: TNEDEditorCaretTypeEnum;
    FCaretOverwriteType: TNEDEditorCaretTypeEnum;
    FCaretOverwriteTypeWithBorders: Boolean;
    FCaretMode: TNEDEditorCaretModeEnum;
    FCaretPosition: TNEDCaretPosition;
    FMultiCaretEnabled: Boolean;
    FMultiCarets: TList<PNEDCaretPosition>;
    FMultiCaretSavedCaretPosition: TNEDCaretPosition;
    //FMultiCaretTimer: TTimer;
  private
    procedure SetCaretEnabled(const Value: Boolean);
    procedure SetCaretVisible(const Value: Boolean);
    procedure SetCaretSize(const Value: TPoint; const Offset: TPoint);
    procedure SetCaretInsertType(const Value: TNEDEditorCaretTypeEnum);
    procedure SetCaretOverwriteType(const Value: TNEDEditorCaretTypeEnum);
    procedure SetCaretOverwriteTypeWithBorders(const Value: Boolean);
    procedure SetCaretMode(const Value: TNEDEditorCaretModeEnum);
    procedure SetCaretPosition(const Value: TNEDCaretPosition);
    procedure SetMultiCaretEnabled(const Value: Boolean);
  protected
    procedure CaretSetPosition(const CaretPos: TNEDCaretPosition);
    //
    procedure UpdateEditor;
    procedure Paint(const ACanvas: TCanvas; const ARect: TRect);
  public
    constructor Create(const AParentControl: TNEDCustomEditorView); reintroduce;
    destructor Destroy; override;
    //
    procedure CaretCreate;
    procedure CaretDestroy;
    procedure CaretMove(const DeltaLine, DeltaColumn: Integer);
    procedure CaretScroll(const DeltaLine, DeltaColumn: Integer);
    procedure CaretShow;
    procedure CaretHide;
    procedure CaretSetLocation(const CaretPos: TNEDCaretPosition);
    procedure CaretUpdate(const Show: Boolean = True);
    //
    procedure CaretAddMultiMark(const CaretPos: TNEDCaretPosition);
    procedure CaretDeleteMultiMark(const CaretPos: TNEDCaretPosition);
    procedure CaretClearMultiMarks;
    //
    function IsInsertMode: Boolean;
    function IsOverwriteMode: Boolean;
    function IsCaretAtEOL: Boolean;
  published
    property CaretEnabled: Boolean read FCaretEnabled write SetCaretEnabled;
    property CaretVisible: Boolean read FCaretVisible write SetCaretVisible;
    property CaretSize: TNEDEditorCaretInfo read FCaretSizeInfo; // write SetCaretSize;
    property CaretInsertType: TNEDEditorCaretTypeEnum read FCaretInsertType write SetCaretInsertType;
    property CaretOverwriteType: TNEDEditorCaretTypeEnum read FCaretOverwriteType write SetCaretOverwriteType;
    property CaretOverwriteTypeWithBorders: Boolean read FCaretOverwriteTypeWithBorders write SetCaretOverwriteTypeWithBorders;
    property CaretMode: TNEDEditorCaretModeEnum read FCaretMode write SetCaretMode;
    property CaretPosition: TNEDCaretPosition read FCaretPosition write SetCaretPosition;
    property MultiCaretEnabled: Boolean read FMultiCaretEnabled write SetMultiCaretEnabled;
  end;

  TNEDEditorSelection = class(TPersistent)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  published
  end;

  TNEDCustomScrollBarMeasureEnum = (smClient, smTop, smScroll, smTrack, smBottom);
  TNEDCustomScrollBarHitTestEnum = (htNone, htTopButton, htBottomButton, htThumb, htTrack);
  TNEDCustomScrollBarStateEnum = (
    shHidden,
    shShowing, // shFadeInThin,
    shVisibleThin,
    shExpand,
    shVisibleFull, //shExpanded,
    shCollapse,
    //shVisibleThin
    shHiding // shFadeOut
  );

  TNEDCustomScrollBarStateChangeEvent = procedure (Sender: TObject; State: TNEDCustomScrollBarStateEnum) of object;

  TNEDCustomScrollBarActionEnum = (
    saLineUp,
    saLineDown,
    saPageUp,
    saPageDown,
    saThumbTrack,
    saThumbPosition
  );

  TNEDCustomScrollBarScrollEvent = procedure (Sender: TObject; const Action: TNEDCustomScrollBarActionEnum; const Position: Integer) of object;

  TNEDEditorScrollbar = class(TPersistent)
  private
    FEditorControl: TNEDCustomEditorView;
    //
    FVisible: Boolean;
    FRange: Integer; // total content size
    FPageSize: Integer; // visible size
    FPosition: Integer; // current offset
    FScrollBarKind: TScrollBarKind;
    FSize: Integer;
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
    FAnimationTimer: TTimer;
    FHoverTimer: TTimer;
    FVisibleState: Boolean;
    FCurrentWidth: Integer;
    FTargetWidth: Integer;
    FCurrentAlpha: Byte;
    FTargetAlpha: Byte;
    FThinWidth: Integer;
    FExpandedWidth: Integer;
    //
    FPrevMouseInScrollBar: Boolean;
    FMouseInScrollBar: Boolean;
    FWantVisible: Boolean;
    FWantExpanded: Boolean;
    FState: TNEDCustomScrollBarStateEnum;
    FStateChangeEvent: TNEDCustomScrollBarStateChangeEvent;
    //
    FDragging: Boolean;
    FDragStartY: Integer;
    FDragStartPosition: Integer;
    FPressedPart: TNEDCustomScrollBarHitTestEnum;
    FRepeatTimer: TTimer;
    FScrollEvent: TNEDCustomScrollBarScrollEvent;
  private
    procedure SetVisible(const Value: Boolean);
    procedure SetRange(const Value: Integer);
    procedure SetPageSize(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetSize(const Value: Integer);
    procedure SetState(const AState: TNEDCustomScrollBarStateEnum);
    //procedure SetParentControlPosition;
    function ClientRect: TRect;
    function ClientHeight: Integer;
    function ClientWidth: Integer;
    function GetMaxPosition: Integer;
    function GetInteractiveRect: TRect;
    function HitTest(const P: TPoint): TNEDCustomScrollBarHitTestEnum;
    procedure UpdateState;
    procedure DoStateChange(const AState: TNEDCustomScrollBarStateEnum);
    procedure DoScroll(const AAction: TNEDCustomScrollBarActionEnum; const APosition: Integer);
    procedure BeginShowing;
    procedure BeginHiding;
    procedure BeginExpand;
    procedure BeginCollapse;
    procedure AnimationTimer(Sender: TObject);
    procedure HoverTimer(Sender: TObject);
    procedure RepeatTimer(Sender: TObject);
    // messages
    procedure CMMouseEnter(var Msg: TMessage); // message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); // message CM_MOUSELEAVE;
//    procedure CMVisibleChanged(var Msg: TMessage); // message CM_VISIBLECHANGED;
//    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); // message WM_ERASEBKGND;
//    procedure WMNCHitTest(var Msg: TWMNCHitTest); // message WM_NCHITTEST;
  protected
    procedure CalcAutoRange; virtual;
    function  ControlSize(ControlSB, AssumeSB: Boolean): Integer;
    function  CalculateRect(const DrawRect: TRect; const Button: TNEDCustomScrollBarMeasureEnum): TRect;
    function  IsMouseInScrollBar(X, Y: Integer): Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); dynamic;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure Paint(const ACanvas: TCanvas);
    procedure DrawBackground(const ACanvas: TCanvas; var ARect: TRect); virtual;
    procedure DrawButtons(const ACanvas: TCanvas; const ARect: TRect); virtual;
    procedure Update(ControlSB, AssumeSB: Boolean); virtual;
  public
    constructor Create(const AParentControl: TNEDCustomEditorView; AKind: TScrollBarKind); reintroduce;
    destructor Destroy; override;
    // methods
    function NeedsScrollBarVisible: Boolean;
    function IsScrollBarVisible: Boolean;
    procedure SetInfo(const ARange, APageSize, APosition: Integer);
    procedure ShowAnimated;
    procedure HideAnimated;
    // properties
    property Visible: Boolean read FVisible write SetVisible;
    property Range: Integer read FRange write SetRange;
    property PageSize: Integer read FPageSize write SetPageSize;
    property Position: Integer read FPosition write SetPosition;
    property Size: Integer read FSize write SetSize;
    property MaxPosition: Integer read GetMaxPosition;
    property MouseInScrollBar: Boolean read FMouseInScrollBar;
    // events
    property OnStateChange: TNEDCustomScrollBarStateChangeEvent read FStateChangeEvent write FStateChangeEvent;
    property OnScroll: TNEDCustomScrollBarScrollEvent read FScrollEvent write FScrollEvent;
  end;

  TNEDEditorPropertiesEnum = (
    epAltSetsColumnMode,      // Holding down the Alt Key will put the selection mode into columnar format
    epAutoIndent,             // Will indent the caret on new lines with the same amount of leading white space as the preceding line
    epAutoSave,               // save automatically
    epDisableScrollArrows,    // Disables the scroll bar arrow buttons when you can't scroll in that direction any more
    //epDragDropEditing,        // Allows you to select a block of text and drag it within the document to another location
    //epDropFiles,              // Allows the editor accept OLE file drops
    epEnhancedHomeKey,         // enhances home key positioning, similar to visual studio
    epEnhancedEndKey,          // enhances End key positioning, similar to JDeveloper
    epGroupUndo,              // When undoing/redoing actions, handle all continous changes of the same kind in one call instead undoing/redoing each command separately
    epUndoAfterSave,          // keep undo histroy
    epUndoLimit,              // max undo history records
    //epHalfPageScroll,         // When scrolling with page-up and page-down commands, only scroll a half page at a time
    //epHideShowScrollbars,     // if enabled, then the scrollbars will only show when necessary.  If you have ScrollPastEOL, then it the horizontal bar will always be there (it uses MaxLength instead)
    epKeepCaretX,             // When moving through lines w/o Cursor Past EOL, keeps the X position of the cursor
    epKeepOriginalEOL,        // do not change line endings
    //epNoCaret,                // Makes it so the caret is never visible
    epNoSelection,            // Disables selecting text
    epReadOnly,               // Make editor read only; disable any external input, except loading from file or Text property
    epRightMouseMovesCursor,  // When clicking with the right mouse for a popup menu, move the cursor to that location
    //epScrollByOneLess,        // Forces scrolling to be one less
    //epScrollHintFollows,      // The scroll hint follows the mouse when scrolling vertically
    //epScrollPastEof,          // Allows the cursor to go past the end of file marker
    epScrollPastEol,          // Allows the cursor to go past the last character into the white space at the end of a line
    //epShowScrollHint,         // Shows a hint of the visible line numbers when scrolling vertically
    epShowRightMargin,        // Show right margin
    epShowSpecialChars,       // Shows the special Characters
//    epShowDeletedLines,       // Show lines that are deleted
//    epShowHiddenLines,        // Show lines that are hidden
//    epShowSpaceLines,         // Show lines that are marked as space-line
    epShowNonVisibleLines,    // Show deleted and hidden lines
    epShowNonRenderableLines, // Show deleted and hidden and space-lines
    epShowSpaces,             // Show spaces when epShowSpecialChars is on
    epShowTabs,               // Show tabs when epShowSpecialChars is on and epTabsToSpaces is off
    epShowEndOfLines,         // Show EOLs when epShowSpecialChars is on
    epShowWordWraps,          // Show WordWraps when epShowSpecialChars is on
    epSmartTabs,              // When tabbing, the cursor will go to the next non-white space character of the previous line
    epSmartTabsDelete,         // similar to Smart Tabs, but when you delete characters
    //epSpecialLineDefaultFG,   // disables the foreground text color override when using the OnSpecialLineColor event
    epTabIndent,              // When active <Tab> and <Shift><Tab> act as block indent, unindent when text is selected
    epTabsToSpaces,           // Converts a tab character to a specified number of space characters
    epTrimTrailingSpaces,     // Spaces at the end of lines will be trimmed and not saved
    epShowLigatures,          // Shows font ligatures, by default it is disabled
    epCopyPlainText,          // Do not include additional clipboard formats when you copy to Clipboard or drag text
    epWordWrap,               // Do word wrap
    epWrapWithRightEdge       // WordWrap with RightEdge position instead of the whole text area
  );

  TNEDEditorProperties = set of TNEDEditorPropertiesEnum;

  TNEDEditorTextEntryModeEnum = (
    teInsert,
    teOverwrite
  );

  TNEDEditorOptions = class(TPersistent)
  private
    FEditorControl: TNEDCustomEditorView;
    //
    FEncoding: TEncoding;
    FEditorProperties: TNEDEditorProperties;
    FAutoSaveInterval: Integer; // seconds
    FLineBreak: String;
    FUndoLimit: Integer; // max records
    FRightMargin: Integer;
    FTabWidth: Integer;
    FTabWidthToSpaces: Integer;
    FTextEntryMode: TNEDEditorTextEntryModeEnum;
    FTextMargin: Integer;
  private
    procedure SetEncoding(const Value: TEncoding);
    procedure SetEditorProperties(const Value: TNEDEditorProperties);
    procedure SetAutoSaveInterval(const Value: Integer);
    procedure SetLineBreak(const Value: String);
    procedure SetUndoLimit(const Value: Integer);
    procedure SetRightMargin(const Value: Integer);
    procedure SetTabWidth(const Value: Integer);
    procedure SetTabWidthToSpaces(const Value: Integer);
    procedure SetTextEntryMode(const Value: TNEDEditorTextEntryModeEnum);
    procedure SetTextMargin(const Value: Integer);
  protected
    //
    procedure UpdateEditor;
  public
    constructor Create(const AParentControl: TNEDCustomEditorView); reintroduce;
    destructor Destroy; override;
    //
    function WordWrap: Boolean;
    function ReadOnly: Boolean;
  published
    property Encoding: TEncoding read FEncoding write SetEncoding;
    property EditorProperties: TNEDEditorProperties read FEditorProperties write SetEditorProperties;
    property AutoSaveInterval: Integer read FAutoSaveInterval write SetAutoSaveInterval;
    property LineBreak: String read FLineBreak write SetLineBreak;
    property UndoLimit: Integer read FUndoLimit write SetUndoLimit;
    property RightMargin: Integer read FRightMargin write SetRightMargin;
    property TabWidth: Integer read FTabWidth write SetTabWidth;
    property TabWidthToSpaces: Integer read FTabWidthToSpaces write SetTabWidthToSpaces;
    property TextEntryMode: TNEDEditorTextEntryModeEnum read FTextEntryMode write SetTextEntryMode;
    property TextMargin: Integer read FTextMargin write SetTextMargin;
  end;

  TNEDEditorColors = class(TPersistent)
  private
    FEditorControl: TNEDCustomEditorView;
    //
    FBlockSelectedColor: TColor;
    FLineSelectedColor: TColor;
    FLineFocusedColor: TColor;
    FRightMarginColor: TColor;
  protected
    procedure SetBlockSelectedColor(const Value: TColor);
    procedure SetLineSelectedColor(const Value: TColor);
    procedure SetLineFocusedColor(const Value: TColor);
    procedure SetRightMarginColor(const Value: TColor);
    //
    procedure UpdateEditor;
  public
    constructor Create(const AParentControl: TNEDCustomEditorView); reintroduce;
    destructor Destroy; override;
  published
    property BlockSelectedColor: TColor read FBlockSelectedColor write SetBlockSelectedColor;
    property LineSelectedColor: TColor read FLineSelectedColor write SetLineSelectedColor;
    property LineFocusedColor: TColor read FLineFocusedColor write SetLineFocusedColor;
    property RightMarginColor: TColor read FRightMarginColor write SetRightMarginColor;
  end;

  TNEDEditorCommand = class;
  TNEDCommandInvokeFunction = procedure (Sender: TNEDEditorCommand; const CommandID: TNEDCommandIDType; var Handled: Boolean) of object;

  TNEDEditorCommand = class
  private
    FEditorControl: TNEDCustomEditorView;
    //
    FCommandID: TNEDCommandIDType;
    FIsComplexCommand: Boolean;
    FIsFullKeyMap: Boolean;
    FDisplayName: String;
    FDisplayHint: String;
    FInvokeFunction: TNEDCommandInvokeFunction;
  protected
  public
    constructor Create(const CommandID: TNEDCommandIDType; DisplayName, DisplayHint: String; const InvokeFunction: TNEDCommandInvokeFunction);
    destructor Destroy; override;
    //
    function InvokeCommand: Boolean;
    //
    property IsComplexCommand: Boolean read FIsComplexCommand;
    property DisplayName: String read FDisplayName;
    property DisplayHint: String read FDisplayHint;
  end;

  // this should be O(1) search not O(n) as in SynEdit
  TNEDEditorKeyboardMap = class(TPersistent)
  private
    FEditorControl: TNEDCustomEditorView;
    //
    // TNEDCommandShortCut = Low(LongWord)..High(LongWord) translated from
    // VK_xxx + CTRL/ALT/SHIFT using Menus.ShortCut function
    // and than converted into EditorCommand
    FEditorKeyboardMap: TObjectDictionary<TNEDCommandShortCut, TNEDEditorCommand>;
  protected
  public
    constructor Create(const AParentControl: TNEDCustomEditorView); reintroduce;
    destructor Destroy; override;
    //
    procedure AddSimpleMapping(const Key: Word; const Shift: TShiftState; const Command: TNEDEditorCommand); // Key = VK_xxx virtual key code
    procedure AddComplexMapping(const Key1: Word; const Shift1: TShiftState; const Key2: Word; const Shift2: TShiftState; const Command: TNEDEditorCommand); // Key = VK_xxx virtual key code
    function IsComplexCommand(const Key: Word; const Shift: TShiftState): Boolean;
    function GetCommand(const Key: Word; const Shift: TShiftState): TNEDEditorCommand; overload;
    function GetCommand(const Key1: Word; const Shift1: TShiftState; const Key2: Word; const Shift2: TShiftState): TNEDEditorCommand; overload;
  end;

  TNEDEditorKeyMap = record
    Key: Word;
    Shift: TShiftState;
  end;

  TNEDEditorInfoDetails = record
    FilePath: String;
    FileCreation: TDateTime;
    FileModification: TDateTime;
    FileReadOnly: Boolean;
    FileEncodingType: String;
    FileLineBreakType: String;
    FileInsertType: String;
    FileType: String; // NED Project; NED Source File; Pascal / Delphi; FreePascal; Text; JavaScript; Java; Python; C / C++; PHP; HTML; CSS
    //
    Line: Integer;   // 1 based; 0 - means unknown
    Column: Integer; // 1 based; 0 - means unknown
    Lines: Integer; // document lines number + 1
    MaxColumn: Integer; // longest line number of columns
    LineMaxColumn: Integer; // current line number of columns
    Words: Integer;
    EmptyLines: Integer;
    CommentLines: Integer;
    Modified: Boolean;
    Saved: Boolean;
    AutoSave: Boolean;
    TopIndex: Integer;
    ViewLines: Integer;
    //
    Status: String;
    StatusProgress: Integer;
    StatusMaxProgress: Integer;
    Zoom: Integer;
    MinZoom: Integer;
    MaxZoom: Integer;
    VersionStr: String;
    Version: Integer;
  end;
  PNEDEditorInfoDetails = ^TNEDEditorInfoDetails;

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
    FVerticalScrollBar: TNEDEditorScrollbar;
    FHorizontalScrollBar: TNEDEditorScrollbar;
    //
    FOptions: TNEDEditorOptions;
    FColors: TNEDEditorColors;
    FKeyboardMap: TNEDEditorKeyboardMap;
  private
    FModified: Boolean;
    FCommentLineCount: Integer;
    FBlankLineCount: Integer;
    //
    FTopIndex: Integer; // vertical scroll
    FLeftIndex: Integer; // horizontal scroll
    FTextWidth: Integer; // cache
    FTextHeight: Integer; // cache
    FEditorLines: Integer; // cache
    FEditorChars: Integer; // cache
    FVisibleLinesCount: Integer; // cache
    FLongestLineCharsCount: Integer; // cache
    FActiveLineIndex: Integer;
    //
    FProcessNextKeyMap: Boolean;
    FIgnoreInput: Boolean;
    FPreviousKeyMap: TNEDEditorKeyMap;
    FWindowSwitchQueryNext: Boolean;
    FUpdatingScrollBars: Boolean;
    FAutoRangeCount: Integer;
    //
    // setters

    //
    // getters
    function GetCaretPosition: TNEDCaretPosition;
    //
    // internal functions
    procedure InitializeKeyboardMappings;
    procedure DoInternalCommand(Sender: TNEDEditorCommand; const CommandID: TNEDCommandIDType; var Handled: Boolean);
    function GetEditorEncodingStr: String;
    function GetEditorLineBreakStr: String;
    function GetEditorInsertStr: String;
    function GetEditorFileTypeStr: String;
  private
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMDialogKey(var Msg: TCMDialogKey); message CM_DIALOGKEY; // grab TAB key before delphi can still it and switch it off
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
//    procedure CMEntryImages(var Msg: TMessage); message CM_ENTRYIMAGES;
//    //procedure CMItemImages(var Msg: TMessage); message CM_ITEMIMAGES;
//    procedure CMItemDetails(var Msg: TMessage); message CM_ITEMDETAILS;
//    procedure CNItemClick(var Msg: TMessage); message CN_ITEMCLICK;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TWMScroll); message WM_HSCROLL;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
//    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    //function  DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DblClick; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure DrawBackground(const ACanvas: TCanvas; var ARect: TRect); virtual;
    procedure DrawLines(const ACanvas: TCanvas; const ARect: TRect); virtual;
    procedure DrawLine(const ACanvas: TCanvas; const ARect: TRect; const LineIdx: Integer); virtual;
    //
    procedure SetDocument(const DocumentBuffer: TNEDEditorBuffer);
    procedure SetObserver(const DocumentObserver: TNEDDocumentObserver);
    procedure SetTopIndex(Value: Integer);
    procedure SetLeftIndex(Value: Integer);
    procedure SetActiveLineIndex(Value: Integer);
    procedure CalcAutoRange; virtual;
    procedure HideNativeScrollBars;
    procedure UpdateScrollBars; virtual;
    procedure ShowModernScrollBars;
    procedure HideModernScrollBars;
    //
    function GetGutterArea: TRect;
    function GetLinesArea: TRect;
    function GetMinimapArea: TRect;
    function GetCharWidth: Integer;
    function GetLineHeight: Integer;
    function GetEditorLines: Integer;
    function GetEditorChars: Integer;
    //
    procedure InvalidateLine(const LineIndex: Integer);
    procedure ScrollBy(DeltaX, DeltaY: Integer);
    procedure ScrollLineIntoView(const ALine: Integer);
    procedure ScrollToSelection;
    function LineSelected(const LineIdx: Integer): Boolean;
    function LineColumnInView(const LineColumn: TNEDTextPosition): Boolean;
    function LineColumnToPixels(const LineColumn: TNEDTextPosition): TPoint;
    function ColumnToPixels(const S: String; Col: Integer): Integer;
    function TextWidth(const S: String): Integer;
    function CaretToLineColumn(const CaretPos: TNEDCaretPosition): TNEDTextPosition;
    function CaretToPixels(const CaretPos: TNEDCaretPosition): TPoint;
    function LineColumnToCaret(const LineColumn: TNEDTextPosition): TNEDCaretPosition; overload;
    function LineColumnToCaret(const Line, Column: Integer): TNEDCaretPosition; overload;
    //
    procedure DocumentChanged(const Change: TNEDDocumentChangeInfo);
    procedure LineInserted(const Change: TNEDDocumentChangeInfo);
    procedure LineDeleted(const Change: TNEDDocumentChangeInfo);
    procedure LineChanged(const Change: TNEDDocumentChangeInfo);
    procedure VerticalScroll(Sender: TObject; const Action: TNEDCustomScrollBarActionEnum; const Position: Integer);
    procedure HorizontalScroll(Sender: TObject; const Action: TNEDCustomScrollBarActionEnum; const Position: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //
    procedure DisableAutoRange;
    procedure EnableAutoRange;
    procedure ReportEditorInfo;
  published
    property Document: TNEDEditorBuffer read FDocument write SetDocument;
    property Gutter: TNEDEditorGutter read FGutter;
    property Minimap: TNEDEditorMinimap read FMinimap;
    property Caret: TNEDEditorCaret read FCaret;
    property Selection: TNEDEditorSelection read FSelection;
    property HorizontalScrollBar: TNEDEditorScrollbar read FHorizontalScrollBar;
    property VerticalScrollBar: TNEDEditorScrollbar read FVerticalScrollBar;
    property Options: TNEDEditorOptions read FOptions;
    property Colors: TNEDEditorColors read FColors;
    //
    property CharWidth: Integer read GetCharWidth;
    property LineHeight: Integer read GetLineHeight;
    property TopIndex: Integer read FTopIndex;
    property VisibleLinesCount: Integer read FVisibleLinesCount;
    property LongestLineCharsCount: Integer read FLongestLineCharsCount;
    property EditorLines: Integer read FEditorLines;
    property EditorChars: Integer read FEditorChars;
    property ActiveLineIndex: Integer read FActiveLineIndex write SetActiveLineIndex;
    property CaretPosition: TNEDCaretPosition read GetCaretPosition;
    property EditorFileType: String read GetEditorFileTypeStr;
  end;

  TNEDEditorView = class(TNEDCustomEditorView)
  published
    property PopupMenu;
  end;

const
  CM_NED_EDITOR_BASE     = $BE00; // max $BFFF
  //
  CM_NED_EDITORINFO_BASE = CM_NED_EDITOR_BASE + 0;
  CM_NED_EDITORINFO_DETAILS = CM_NED_EDITORINFO_BASE + 0;

  CM_NED_WINDOW_BASE = CM_NED_EDITOR_BASE + 10;
  CM_NED_WINDOW_SWITCH = CM_NED_WINDOW_BASE + 0;

implementation

uses
  Windows,
  Menus,
  Character,
  Imm,
  Math;

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
  aResult := '';
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
      P[I].Offset(0{-Offset}, HalfSize)
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

{ TNEDEditorGutter }

constructor TNEDEditorGutter.Create(const AParentControl: TNEDCustomEditorView);
begin
  FEditorControl := AParentControl;
  FVisible := True;
  FWidth := 35;
  FAutoSize := True;
  FColor := clNone;
  FParentColor := True;
  FFontColor := clGrayText;
  FParentFontColor := False;
  FActiveLineFontColor := clWhite;
  FSpacer := True;
  FSpacerWidth := 2;
  FSpacerColor := clBlack;
  FSpacerParentColor := False;
  FNumbersType := gnContinous;
  FLineNumberType := lnEachTenthWithDashedFifths;
  FLineIndicatorWidth := 2;
  FLineModifiedIndicatorColor := clYellow;
  FLineSavedIndicatorColor := clGreen;
  FLineDeletedIndicatorColor := clRed;
  FLineHiddenIndicatorColor := clGray;
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

procedure TNEDEditorGutter.SetNumbersType(const Value: TNEDEditorGutterNumberingTypeEnum);
begin
  if FNumbersType <> Value then begin
    FNumbersType := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorGutter.SetLineNumberType(const Value: TNEDEditorGutterLineNumberingTypeEnum);
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

function TNEDEditorGutter.GetMaxLineNumberChars(const LineNumbers: Integer): Integer;
begin
  Result := 1;
  while (LineNumbers div Trunc(Power(10, Result))) > 0 do
    Inc(Result);
  Inc(Result);
end;

function TNEDEditorGutter.GetLineNumbersLength(const LineNumbers: Integer): Integer;
begin
  Result := GetMaxLineNumberChars(LineNumbers) * FEditorControl.GetCharWidth;
end;

function TNEDEditorGutter.GetWidth: Integer;
begin
  if not AutoSize then begin
    if not FVisible then
      Exit(0);
    Result := Width;
  end
  else begin
    // calculate gutter width
    Result := 0;
    if not FVisible then
      Exit;

    if FSpacer then
      Result := Result + FSpacerWidth;
    if (FLineNumberType > lnHidden) and (FEditorControl.Document <> Nil) then
      Result := Result + GetLineNumbersLength(FEditorControl.Document.VisibleLinesCount);

    Result := Result + FLineIndicatorWidth;
  end;
end;

procedure TNEDEditorGutter.UpdateEditor;
begin
  if FEditorControl <> Nil then
    FEditorControl.Repaint;
end;

procedure TNEDEditorGutter.Paint(const ACanvas: TCanvas; const ARect: TRect);
var
  NumbersR: TRect;
  IndicatorsR: TRect;
  SpacerR: TRect;
begin
  // clear background
  if ParentColor then
    ACanvas.Brush.Color := FEditorControl.Color
  else
    ACanvas.Brush.Color := Color;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.FillRect(ARect);

  // paint line numbers
  NumbersR := TRect.Empty;
  if (FLineNumberType > lnHidden) and (FEditorControl.Document <> Nil) then begin
    NumbersR := ARect;
    NumbersR.Width := ARect.Width - FLineIndicatorWidth - IfThen(FSpacer, FSpacerWidth, 0);
    DrawLineNumbers(ACanvas, NumbersR);
  end;

  // paint line state indicators
  IndicatorsR := ARect;
  IndicatorsR.Left := ARect.Left + NumbersR.Right;
  IndicatorsR.Width := ARect.Width - NumbersR.Width - IfThen(FSpacer, FSpacerWidth, 0);
  DrawLineStateIndicators(ACanvas, IndicatorsR);

  // paint spacer
  if FSpacer then begin
    SpacerR := ARect;
    SpacerR.Left := ARect.Right - FSpacerWidth;
    SpacerR.Width := FSpacerWidth;
    DrawSpacer(ACanvas, SpacerR);
  end;
end;

procedure TNEDEditorGutter.DrawLineNumbers(const ACanvas: TCanvas; const ARect: TRect);
var
  Y, I, LineIdx, LineOffset: Integer;
  LineR: TRect;
  LineProperties: TNEDLineProperties;
  LineNumber: String;
  CurrentLineHeight: Integer;
begin
  // @TODO: comment or remove this - debug only
//  ACanvas.Brush.Color := clRed;
//  ACanvas.Brush.Style := bsSolid;
//  ACanvas.FillRect(ARect);

  ACanvas.Font.Assign(FEditorControl.Font);
  if not ParentFontColor then
    ACanvas.Font.Color := FontColor;
  //
  I := 0;
  LineIdx := 1;
  LineOffset := 0;
  Y := 0; // render height accumulator
  while Y < ARect.Height do begin // divide client-area into horizontal strips and render Lines
    if I + FEditorControl.TopIndex > FEditorControl.VisibleLinesCount - 1 then
      Break;

    LineProperties := FEditorControl.Document.Lines[I + FEditorControl.TopIndex];
    if not LineProperties.IsVisible and not (epShowNonVisibleLines in FEditorControl.Options.EditorProperties) then begin
      Inc(I);
      Inc(LineOffset);
      Continue;
    end;

    CurrentLineHeight := LineProperties.GetLineHeight(FEditorControl.LineHeight);

    case FNumbersType of
      gnContinous: begin
        if (FEditorControl.TopIndex + LineIdx - 1 + LineOffset) = FEditorControl.ActiveLineIndex then
          LineNumber := IntToStr(LineIdx + FEditorControl.TopIndex)
        else begin
          case FLineNumberType of
            lnHidden: LineNumber := '';
            lnEachLine: begin
              LineNumber := IntToStr(LineIdx + FEditorControl.TopIndex);
            end;
            lnEachTenth: begin
              if (LineIdx + FEditorControl.TopIndex = 1) or ((LineIdx + FEditorControl.TopIndex) mod 10 = 0) then
                LineNumber := IntToStr(LineIdx + FEditorControl.TopIndex)
              else
                LineNumber := '·';
            end;
            lnEachTenthWithDashedFifths: begin
              if (LineIdx + FEditorControl.TopIndex = 1) or ((LineIdx + FEditorControl.TopIndex) mod 10 = 0) then
                LineNumber := IntToStr(LineIdx + FEditorControl.TopIndex)
              else if (LineIdx + FEditorControl.TopIndex) mod 5 = 0 then
                LineNumber := '-'
              else
                LineNumber := '·';
            end;
          end;
        end;
      end;
      gnOriginal: begin
        if FEditorControl.TopIndex + I = FEditorControl.ActiveLineIndex then
          LineNumber := IntToStr(LineProperties.LineNo)
        else begin
          case FLineNumberType of
            lnHidden: LineNumber := '';
            lnEachLine: begin
              LineNumber := IntToStr(LineProperties.LineNo);
            end;
            lnEachTenth: begin
              if (LineProperties.LineNo = 1) or (LineProperties.LineNo mod 10 = 0) then
                LineNumber := IntToStr(LineProperties.LineNo)
              else
                LineNumber := '·';
            end;
            lnEachTenthWithDashedFifths: begin
              if (LineProperties.LineNo = 1) or (LineProperties.LineNo mod 10 = 0) then
                LineNumber := IntToStr(LineProperties.LineNo)
              else if LineProperties.LineNo mod 5 = 0 then
                LineNumber := '-'
              else
                LineNumber := '·';
            end;
          end;
        end;
      end;
      gnDifference: begin

      end;
    end;

    LineR := Rect(ARect.Left, ARect.Top + Y, 0, 0);
    LineR.Width := ARect.Right - FLineIndicatorWidth - IfThen(FSpacer, FSpacerWidth);
    LineR.Height := CurrentLineHeight;

    if FEditorControl.TopIndex + I = FEditorControl.ActiveLineIndex then begin // active line
      ACanvas.Font.Color := FActiveLineFontColor;
    end
    else begin // other line
      ACanvas.Font.Assign(FEditorControl.Font);
      if not ParentFontColor then
        ACanvas.Font.Color := FontColor;
    end;

    ACanvas.TextRect(LineR, LineNumber, [tfRight, tfTop, tfSingleLine]);
    LineNumber := '';
    //
    Inc(I);
    Inc(LineIdx);
    Inc(Y, CurrentLineHeight);
  end;
end;

procedure TNEDEditorGutter.DrawLineStateIndicators(const ACanvas: TCanvas; const ARect: TRect);
var
  Y, I: Integer;
  LineR: TRect;
  LineProperties: TNEDLineProperties;
  CurrentLineHeight: Integer;
begin
  // @TODO: comment or remove this - debug only
//  ACanvas.Brush.Color := clPurple;
//  ACanvas.Brush.Style := bsSolid;
//  ACanvas.FillRect(ARect);

  I := 0;
  Y := 0; // render height accumulator
  while Y < ARect.Height do begin // divide client-area into horizontal strips and render Lines
    if I + FEditorControl.TopIndex > FEditorControl.VisibleLinesCount - 1 then
      Break;

    LineProperties := FEditorControl.Document.Lines[I + FEditorControl.TopIndex];
    if not LineProperties.IsVisible and not (epShowNonVisibleLines in FEditorControl.Options.EditorProperties) then begin
      Inc(I);
      Continue;
    end;

    CurrentLineHeight := LineProperties.GetLineHeight(FEditorControl.LineHeight);

    LineR := Rect(ARect.Left, ARect.Top + Y, ARect.Right, 0);
    //LineR.Width := FLineIndicatorWidth;
    LineR.Height := CurrentLineHeight;

    if ParentColor then
      ACanvas.Brush.Color := FEditorControl.Color
    else
      ACanvas.Brush.Color := Color;
    //
    if LineProperties.Deleted then
      ACanvas.Brush.Color := FLineDeletedIndicatorColor
    else if LineProperties.Hidden then
      ACanvas.Brush.Color := FLineHiddenIndicatorColor
    else if LineProperties.Spacer then begin
      // no color;
    end
    else if LineProperties.Modified then
      ACanvas.Brush.Color := FLineModifiedIndicatorColor
    else if LineProperties.Saved then
      ACanvas.Brush.Color := FLineSavedIndicatorColor;

    ACanvas.Brush.Style := bsSolid;
    ACanvas.FillRect(LineR);

    //
    Inc(I);
    Inc(Y, CurrentLineHeight);
  end;
end;

procedure TNEDEditorGutter.DrawSpacer(const ACanvas: TCanvas; const ARect: TRect);
begin
  ACanvas.Brush.Color := FSpacerColor;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.FillRect(ARect);
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

constructor TNEDEditorCaret.Create(const AParentControl: TNEDCustomEditorView);
begin
  FEditorControl := AParentControl;
  //
  FCaretCreated := False;
  FCaretEnabled := True;
  FCaretVisible := False;
  FCaretSizeInfo.Size := Point(0, 0);
  FCaretSizeInfo.Offset := Point(0, 0);
  FCaretInsertType := ctBlock;
  FCaretOverwriteType := ctBlock;
  FCaretOverwriteTypeWithBorders := True;
  FCaretMode := cmInsert;
  FCaretPosition := TNEDCaretPosition.Create(1, 1);
  FMultiCaretEnabled := False;
  FMultiCarets := TList<PNEDCaretPosition>.Create;
  FMultiCaretSavedCaretPosition := TNEDCaretPosition.Create(0, 0);
end;

destructor TNEDEditorCaret.Destroy;
begin
  CaretClearMultiMarks;
  FreeAndNil(FMultiCarets);
  FEditorControl := Nil;
  inherited;
end;

procedure TNEDEditorCaret.SetCaretEnabled(const Value: Boolean);
begin
  if FCaretEnabled <> Value then begin
    FCaretEnabled := Value;
    CaretUpdate;
    UpdateEditor;
  end;
end;

procedure TNEDEditorCaret.SetCaretVisible(const Value: Boolean);
begin
  if FCaretVisible <> Value then begin
    FCaretVisible := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorCaret.SetCaretSize(const Value: TPoint; const Offset: TPoint);
begin
  FCaretSizeInfo.Size := Value;
  FCaretSizeInfo.Offset := Offset;
end;

procedure TNEDEditorCaret.SetCaretInsertType(const Value: TNEDEditorCaretTypeEnum);
begin
  if FCaretInsertType <> Value then begin
    FCaretInsertType := Value;
    CaretUpdate;
    UpdateEditor;
  end;
end;

procedure TNEDEditorCaret.SetCaretOverwriteType(const Value: TNEDEditorCaretTypeEnum);
begin
  if FCaretOverwriteType <> Value then begin
    FCaretOverwriteType := Value;
    CaretUpdate;
    UpdateEditor;
  end;
end;

procedure TNEDEditorCaret.SetCaretOverwriteTypeWithBorders(const Value: Boolean);
begin
  if FCaretOverwriteTypeWithBorders <> Value then begin
    FCaretOverwriteTypeWithBorders := Value;
    CaretUpdate;
    UpdateEditor;
  end;
end;

procedure TNEDEditorCaret.SetCaretMode(const Value: TNEDEditorCaretModeEnum);
begin
  if FCaretMode <> Value then begin
    FCaretMode := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorCaret.SetCaretPosition(const Value: TNEDCaretPosition);
begin
  if (FCaretPosition.Y <> Value.Y) or (FCaretPosition.X <> Value.X) then begin
    FCaretPosition := Value;
    CaretUpdate;
    UpdateEditor;
  end;
end;

procedure TNEDEditorCaret.SetMultiCaretEnabled(const Value: Boolean);
begin
  if FMultiCaretEnabled <> Value then begin
    FMultiCaretEnabled := Value;
    CaretUpdate;
    UpdateEditor;
  end;
end;

procedure TNEDEditorCaret.CaretSetPosition(const CaretPos: TNEDCaretPosition);
begin
  Windows.SetCaretPos(CaretPos.X, CaretPos.Y);
end;

procedure TNEDEditorCaret.UpdateEditor;
begin
  if FEditorControl <> Nil then
    FEditorControl.Repaint;
end;

procedure TNEDEditorCaret.Paint(const ACanvas: TCanvas; const ARect: TRect);
begin

end;

procedure TNEDEditorCaret.CaretCreate;
var
  ct: TNEDEditorCaretTypeEnum;
  cw, ch: Integer;
  ox, oy: Integer;
begin
  if (FEditorControl = Nil) or not FEditorControl.HandleAllocated then
    Exit;

  // CreateCaret automatically destroys the previous one.
  // We don't have to worry about cleaning up the old one here with DestroyCaret.
  if IsInsertMode then
    ct := FCaretInsertType
  else
    ct := FCaretOverwriteType;
  case ct of
    ctHorizontalLine: begin
      cw := FEditorControl.FTextWidth;
      ch := 2;
      ox := 0;
      oy := FEditorControl.FTextHeight - 1;
    end;
    ctThinHorizontalLine: begin
      cw := FEditorControl.FTextWidth;
      ch := 1;
      ox := 0;
      oy := FEditorControl.FTextHeight;
    end;
    ctVerticalLine: begin
      cw := 2;
      ch := FEditorControl.FTextHeight;
      ox := 0;
      oy := 0;
    end;
    ctThinVerticalLine: begin
      cw := 1;
      ch := FEditorControl.FTextHeight;
      ox := 0;
      oy := 0;
    end;
    ctHalfBlock: begin
      cw := FEditorControl.FTextWidth;
      ch := FEditorControl.FTextHeight div 2;
      ox := 0;
      oy := ch;
    end;
    ctBlock: begin
      cw := FEditorControl.FTextWidth;
      ch := FEditorControl.FTextHeight;
      ox := 0;
      oy := 0;
    end;
  end;
  FCaretSizeInfo.Size := Point(cw, ch);
  FCaretSizeInfo.Offset := Point(ox, oy);

  if FEditorControl.Focused {or FAlwaysShowCaret} then begin
    Windows.CreateCaret(FEditorControl.Handle, 0, cw, ch);
    FCaretCreated := True;
    CaretUpdate;
  end;
end;

procedure TNEDEditorCaret.CaretDestroy;
begin
  if FCaretCreated then
    Windows.DestroyCaret;
  FCaretCreated := False;
end;

procedure TNEDEditorCaret.CaretMove(const DeltaLine, DeltaColumn: Integer);
var
  LineLength: Integer;
begin
  // this function can't exit from Editor line-area rect, it's for placing caret inside this rect only
  if FEditorControl = Nil then
    Exit;

  if DeltaLine > 0 then begin
    if FCaretPosition.Y + DeltaLine < FEditorControl.VisibleLinesCount then
      Inc(FCaretPosition.Y, DeltaLine);
    Exit;
  end
  else if DeltaLine < 0 then begin
    if FCaretPosition.Y + DeltaLine > 1 then
      Inc(FCaretPosition.Y, DeltaLine);
    Exit;
  end;

  LineLength := FEditorControl.Document.Lines[FEditorControl.CaretToLineColumn(FCaretPosition).Line].Length;
  if DeltaColumn > 0 then begin
    if epScrollPastEol in FEditorControl.Options.EditorProperties then begin
      Inc(FCaretPosition.X, DeltaColumn);
    end
    else begin
      if FCaretPosition.X + DeltaColumn <= LineLength + 1 then
        Inc(FCaretPosition.X, DeltaColumn);
    end;
  end
  else if DeltaColumn < 0 then begin
    if FCaretPosition.X + DeltaColumn > 0 then
      Inc(FCaretPosition.X, DeltaColumn);
  end;
end;

procedure TNEDEditorCaret.CaretScroll(const DeltaLine, DeltaColumn: Integer);
var
  LineLength: Integer;
begin
  // this function doesn't care about Editor line-area rect
  if FEditorControl = Nil then
    Exit;

  if DeltaLine > 0 then begin
    Inc(FCaretPosition.Y, DeltaLine);
    Exit;
  end
  else if DeltaLine < 0 then begin
    Inc(FCaretPosition.Y, DeltaLine);
    Exit;
  end;

  LineLength := FEditorControl.Document.Lines[FEditorControl.CaretToLineColumn(FCaretPosition).Line].Length;
  if DeltaColumn > 0 then begin
    if epScrollPastEol in FEditorControl.Options.EditorProperties then begin
      Inc(FCaretPosition.X, DeltaColumn);
    end
    else begin
      Inc(FCaretPosition.X, DeltaColumn);
    end;
  end
  else if DeltaColumn < 0 then begin
    Inc(FCaretPosition.X, DeltaColumn);
  end;
end;

procedure TNEDEditorCaret.CaretShow;
begin
  if FCaretCreated and not FCaretVisible then begin
    if Windows.ShowCaret(FEditorControl.Handle) then
      FCaretVisible := True;
  end;
end;

procedure TNEDEditorCaret.CaretHide;
begin
  if FCaretCreated and FCaretVisible then begin
    if Windows.HideCaret(FEditorControl.Handle) then
      FCaretVisible := False;
  end;
end;

procedure TNEDEditorCaret.CaretSetLocation(const CaretPos: TNEDCaretPosition);
begin
  FCaretPosition := CaretPos;
end;

procedure TNEDEditorCaret.CaretUpdate(const Show: Boolean = True);
var
  CX, CY: Integer;
  ClientR: TRect;
  CaretPos: TNEDCaretPosition;
  CaretPixelPos: TPoint;
  cf: TCompositionForm;
begin
  if FEditorControl = Nil then
    Exit;
  //
  if not FCaretCreated or not FEditorControl.Focused {or FAlwaysShowCaret)} then
    Exit;
  //
  // The last space of a wrapped line may be out of view
  CaretPos := FCaretPosition;
  if FEditorControl.Options.WordWrap and not (epWrapWithRightEdge in FEditorControl.Options.EditorProperties) and
     IsCaretAtEOL and not FEditorControl.LineColumnInView(FEditorControl.CaretToLineColumn(CaretPos)) then
    Dec(CaretPos.X);

  CaretPixelPos := FEditorControl.CaretToPixels(CaretPos);
  CX := CaretPixelPos.X + FCaretSizeInfo.Offset.X;
  CY := CaretPixelPos.Y + FCaretSizeInfo.Offset.Y;
  ClientR := FEditorControl.GetLinesArea;

  CaretSetPosition(TNEDCaretPosition.Create(CX, CY));// Windows.SetCaretPos(CX, CY);

  if Show then begin
    if ClientR.Contains(Point(CX, CY)) then // (CX >= ClientR.Left) and (CX < ClientR.Right) and (CY >= ClientR.Top) and (CY < ClientR.Bottom) then
      CaretShow
    else
      CaretHide;
  end;

  cf.dwStyle := CFS_POINT;
  cf.ptCurrentPos := Point(CX, CY);
  ImmSetCompositionWindow(ImmGetContext(FEditorControl.Handle), @cf);
end;

procedure TNEDEditorCaret.CaretAddMultiMark(const CaretPos: TNEDCaretPosition);
begin

end;

procedure TNEDEditorCaret.CaretDeleteMultiMark(const CaretPos: TNEDCaretPosition);
begin

end;

procedure TNEDEditorCaret.CaretClearMultiMarks;
begin

end;

function TNEDEditorCaret.IsInsertMode: Boolean;
begin
  Result := FCaretMode = cmInsert;
end;

function TNEDEditorCaret.IsOverwriteMode: Boolean;
begin
  Result := FCaretMode = cmOverwrite;
end;

function TNEDEditorCaret.IsCaretAtEOL: Boolean;
begin
  if FEditorControl = Nil then
    Exit;

  Result := FCaretPosition.X >= FEditorControl.Document.Lines[FEditorControl.CaretToLineColumn(FCaretPosition).Line].Length;
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

constructor TNEDEditorScrollbar.Create(const AParentControl: TNEDCustomEditorView; AKind: TScrollBarKind);
begin
  FEditorControl := AParentControl;
  //
  // OS scrollbar width and height
  FXYScrollSize.cx := GetSystemMetrics(SM_CXVSCROLL);
  FXYScrollSize.cy := GetSystemMetrics(SM_CYHSCROLL);
  //
  // defaults
  FVisible := False;
  if AKind = sbHorizontal then
    FSize := FXYScrollSize.cy // height
  else
    FSize := FXYScrollSize.cx; // width
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
  FState := shHidden;
  FStateChangeEvent := Nil;
  //
  FAnimationTimer := TTimer.Create(FEditorControl);
  FAnimationTimer.Enabled := False;
  FAnimationTimer.Interval := 15;
  FAnimationTimer.OnTimer := AnimationTimer;
  //
  FHoverTimer := TTimer.Create(FEditorControl);
  FHoverTimer.Enabled := False;
  FHoverTimer.Interval := 1500;
  FHoverTimer.OnTimer := HoverTimer;
  //
  FVisibleState := False;
  FCurrentWidth := 0;
  FTargetWidth := FSize;
  FCurrentAlpha := 0;
  FTargetAlpha := 0;
  FThinWidth := 2;
  FExpandedWidth := FSize;
  //
  FPrevMouseInScrollBar := False;
  FMouseInScrollBar := False;
  FWantVisible := False;
  FWantExpanded := False;
  //
  FRepeatTimer := TTimer.Create(FEditorControl);
  FRepeatTimer.Enabled := False;
  FRepeatTimer.Interval := 50;
  FRepeatTimer.OnTimer := RepeatTimer;
  //
  FDragging := False;
  FDragStartY := -1;
  FDragStartPosition := -1;
  FPressedPart := htNone;
end;

destructor TNEDEditorScrollbar.Destroy;
begin
  FAnimationTimer.Enabled := False;
  FAnimationTimer.OnTimer := Nil;
  FAnimationTimer.Free;
  //
  FHoverTimer.Enabled := False;
  FHoverTimer.OnTimer := Nil;
  FHoverTimer.Free;
  //
  FRepeatTimer.Enabled := False;
  FRepeatTimer.OnTimer := Nil;
  FRepeatTimer.Free;
  inherited;
end;

procedure TNEDEditorScrollbar.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then begin
    FVisible := Value;

    if not FUpdating then
      FEditorControl.Invalidate;
  end;
end;

procedure TNEDEditorScrollbar.SetRange(const Value: Integer);
begin
  if Value <= 0 then
    Exit;
  //
  if FRange <> Value then begin
    FRange := Value;

    // Clamp current position
    FPosition := EnsureRange(FPosition, 0, GetMaxPosition);
    Update(False, False);

    if not FUpdating then
      FEditorControl.Invalidate;
  end;
end;

procedure TNEDEditorScrollbar.SetPageSize(const Value: Integer);
begin
  if FPageSize <> Value then begin
    FPageSize := Value;

    // Clamp current position
    FPosition := EnsureRange(FPosition, 0, GetMaxPosition);

    if not FUpdating then
      FEditorControl.Invalidate;
  end;
end;

procedure TNEDEditorScrollbar.SetPosition(const Value: Integer);
var
  NewPos: Integer;
begin
  NewPos := EnsureRange(Value, 0, GetMaxPosition);

  if FPosition <> NewPos then begin
    FPosition := NewPos;

    if not FUpdating then
      FEditorControl.Invalidate;

    // optional:
    // if Assigned(FOnScroll) then
    //   FOnScroll(Self, FPosition);
  end;
end;

procedure TNEDEditorScrollbar.SetSize(const Value: Integer);
begin
  if FSize <> Value then begin
    FSize := Value;

    if not FUpdating then
      FEditorControl.Invalidate;
  end;
end;

procedure TNEDEditorScrollbar.SetState(const AState: TNEDCustomScrollBarStateEnum);
begin
  if FState <> AState then begin
    FState := AState;
    if Assigned(FStateChangeEvent) then
      FStateChangeEvent(Self, AState);
  end;
end;

//procedure TNEDEditorScrollbar.SetParentControlPosition;
//var
//  Code: Word;
//begin
//  if FScrollBarKind = sbHorizontal then
//    Code := SB_HORZ
//  else
//    Code := SB_VERT;
//
////  if FlatSB_GetScrollPos(FEditorControl.Handle, Code) <> FPosition then
////    FlatSB_SetScrollPos(FEditorControl.Handle, Code, FPosition, True);
//end;

function TNEDEditorScrollbar.ClientRect: TRect;
begin
  case FScrollBarKind of
    sbHorizontal: Result := Rect(0, FEditorControl.ClientHeight - FSize, FEditorControl.ClientWidth, FEditorControl.ClientHeight);
    sbVertical  : Result := Rect(FEditorControl.ClientWidth - FSize, 0, FEditorControl.ClientWidth, FEditorControl.ClientHeight);
  end;
end;

function TNEDEditorScrollbar.ClientHeight: Integer;
begin
  Result := ClientRect.Height;
end;

function TNEDEditorScrollbar.ClientWidth: Integer;
begin
  Result := ClientRect.Width;
end;

function TNEDEditorScrollbar.GetMaxPosition: Integer;
begin
  Result := FRange - FPageSize;

  if Result < 0 then
    Result := 0;
end;

function TNEDEditorScrollbar.GetInteractiveRect: TRect;
begin
  Result := CalculateRect(ClientRect, smClient);
  Result.Left := Result.Right - Round(FCurrentWidth);
end;

function TNEDEditorScrollbar.HitTest(const P: TPoint): TNEDCustomScrollBarHitTestEnum;
var
  CR, AreaRect: TRect;
begin
  CR := ClientRect;
  // htNone, htTopButton, htBottomButton, htThumb, htTrack

  AreaRect := CalculateRect(CR, smTop);
  if PtInRect(AreaRect, P) then
    Exit(htTopButton);

  AreaRect := CalculateRect(CR, smScroll);
  if PtInRect(AreaRect, P) then
    Exit(htThumb);
  AreaRect := CalculateRect(CR, smTrack);
  if PtInRect(AreaRect, P) then
    Exit(htTrack);

  AreaRect := CalculateRect(CR, smBottom);
  if PtInRect(AreaRect, P) then
    Exit(htBottomButton);

  Result := htNone;
end;

procedure TNEDEditorScrollbar.UpdateState;
begin
  if not FWantVisible then begin
    if FState <> shHiding then
      BeginHiding;
    Exit;
  end;

  if FState = shHidden then begin
    BeginShowing;
    Exit;
  end;

  if FWantExpanded then begin
    if not (FState in [shExpand, shVisibleFull]) then
      BeginExpand;
    Exit;
  end;

  if not (FState in [shCollapse, shVisibleThin]) then
    BeginCollapse;
end;

procedure TNEDEditorScrollbar.DoScroll(const AAction: TNEDCustomScrollBarActionEnum; const APosition: Integer);
begin
  if Assigned(FScrollEvent) then
    FScrollEvent(Self, AAction, APosition);
end;

procedure TNEDEditorScrollbar.DoStateChange(const AState: TNEDCustomScrollBarStateEnum);
begin
  case AState of
    shHidden: ;
    shShowing: begin
      FTargetAlpha := 255;
      FTargetWidth := FThinWidth;
      FAnimationTimer.Enabled := True;
    end;
    shVisibleThin: begin
      if not FHoverTimer.Enabled and (FState <> shVisibleThin) then
        FHoverTimer.Enabled := True
      else if FState = shExpand then begin

      end;
    end;
    shExpand: ;
    shVisibleFull: begin
      if not FHoverTimer.Enabled and (FState <> shVisibleFull) then
        FHoverTimer.Enabled := True
      else if FState = shCollapse then begin

      end;
    end;
    shCollapse: ;
    shHiding: begin
      FTargetAlpha := 0;
      FTargetWidth := 0;
      FAnimationTimer.Enabled := True;
    end;
  end;
end;

procedure TNEDEditorScrollbar.BeginShowing;
begin
  if not FWantVisible then
    FWantVisible := True;

  FState := shShowing;

  FTargetAlpha := 255;
  FTargetWidth := FThinWidth;

  FAnimationTimer.Enabled := True;
end;

procedure TNEDEditorScrollbar.BeginHiding;
begin
  FState := shHiding;

  FTargetAlpha := 0;
  FTargetWidth := FThinWidth;

  FAnimationTimer.Enabled := True;
end;

procedure TNEDEditorScrollbar.BeginExpand;
begin
  FState := shExpand;

  FTargetWidth := FExpandedWidth;
  FTargetAlpha := 255;

  FAnimationTimer.Enabled := True;
end;

procedure TNEDEditorScrollbar.BeginCollapse;
begin
  FState := shCollapse;

  FTargetWidth := FThinWidth;
  FTargetAlpha := 255;

  FAnimationTimer.Enabled := True;
end;

procedure TNEDEditorScrollbar.AnimationTimer(Sender: TObject);
const
  mulby: Single = 0.2;
  lesseq: Integer = 2; // mulby * 10;
begin
  // width interpolation
  FCurrentWidth := FCurrentWidth + Round((FTargetWidth - FCurrentWidth) * mulby);

  // alpha interpolation
  FCurrentAlpha := FCurrentAlpha + Round((FTargetAlpha - FCurrentAlpha) * mulby);

  FEditorControl.Invalidate;

  // stop when close enough
  if Abs(FCurrentWidth - FTargetWidth) <= lesseq then
    FCurrentWidth := FTargetWidth;

  if Abs(FCurrentAlpha - FTargetAlpha) <= lesseq then
    FCurrentAlpha := FTargetAlpha;

  if (FCurrentWidth = FTargetWidth) and (FCurrentAlpha = FTargetAlpha) then begin
    FAnimationTimer.Enabled := False;
//    case FState of
//      shShowing    : SetState(shVisibleThin);
//      shVisibleThin: begin
//        if (FTargetWidth = 0) or (FTargetAlpha = 0) then begin // hiding scrollbar
//          SetState(shHiding);
//          Visible := False;
//        end;
//      end;
//      shExpand     : SetState(shVisibleFull);
//      shCollapse   : SetState(shVisibleThin);
//      shHiding     : begin
//        SetState(shHidden);
//        Visible := False;
//      end;
//    end;

    case FState of
      shShowing: begin
        if FWantExpanded then
          BeginExpand
        else
          FState := shVisibleThin;
      end;

      shExpand: FState := shVisibleFull;

      shCollapse: FState := shVisibleThin;

      shHiding: begin
        FWantVisible := False;
        FState := shHidden;
        FCurrentAlpha := 0;
        FEditorControl.Invalidate;
      end;
    end;
  end;
end;

procedure TNEDEditorScrollbar.HoverTimer(Sender: TObject);
begin
  FHoverTimer.Enabled := False;
  if FState <> shVisibleFull then begin
    FTargetWidth := FExpandedWidth;
    SetState(shExpand);
    FAnimationTimer.Enabled := True;
  end
  else if FState <> shVisibleThin then begin
    FTargetWidth := FThinWidth;
    SetState(shCollapse);
    FAnimationTimer.Enabled := True;
  end;
end;

procedure TNEDEditorScrollbar.RepeatTimer(Sender: TObject);
var
  Action: TNEDCustomScrollBarActionEnum;
  P: TPoint;
begin
  case FPressedPart of
    htTopButton   : begin
      Action := saLineUp;
      Position := Position - 1;
    end;
    htBottomButton: begin
      Action := saLineDown;
      Position := Position + 1;
    end;
    htTrack: begin
      GetCursorPos(P);
      P := FEditorControl.ScreenToClient(P);
      if P.Y < CalculateRect(ClientRect, smScroll).Top then begin
        Action := saPageUp;
        Position := Position - PageSize;
      end
      else if P.Y > CalculateRect(ClientRect, smScroll).Bottom then begin
        Action := saPageDown;
        Position := Position + PageSize;
      end
      else
        Action := saThumbPosition;
    end;
  end;
  if Action <> saThumbPosition then
    DoScroll(Action, Position);
end;

function TNEDEditorScrollbar.NeedsScrollBarVisible: Boolean;
begin
  Result := FRange > ControlSize(False, False) div FEditorControl.LineHeight;
end;

function TNEDEditorScrollbar.IsScrollBarVisible: Boolean;
begin
  Result := Visible and NeedsScrollBarVisible;
end;

procedure TNEDEditorScrollbar.SetInfo(const ARange, APageSize, APosition: Integer);
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
    FEditorControl.Invalidate;
  end;
end;

procedure TNEDEditorScrollbar.ShowAnimated;
begin
  if not FWantVisible and (FState = shHidden) then begin
    FWantVisible := True;
    UpdateState;
//    DoStateChange(shShowing);
  end;
end;

procedure TNEDEditorScrollbar.HideAnimated;
begin
  FWantVisible := False;
  UpdateState;
//  DoStateChange(shHiding);
end;

procedure TNEDEditorScrollbar.CMMouseEnter(var Msg: TMessage);
var
  P: TPoint;
begin
  inherited;
  P := Mouse.CursorPos;
  P := FEditorControl.ScreenToClient(P);
  //
  if FScrollBarKind = sbVertical then begin
    if (P.X >= FEditorControl.ClientWidth - Size) then begin
      FWantExpanded := True;
      UpdateState;
      //DoStateChange(shVisibleFull);
    end;
  end
  else begin
    if (P.Y >= FEditorControl.ClientHeight - Size) then begin
      FWantExpanded := True;
      UpdateState;
      //DoStateChange(shVisibleFull);
    end;
  end;
end;

procedure TNEDEditorScrollbar.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FWantExpanded := False;
  UpdateState;
//  DoStateChange(shVisibleThin);
end;

//  shHidden,
//  shShowing, // shFadeInThin,
//  shVisibleThin,
//  shExpanding,
//  shVisibleFull, //shExpanded,
//  shCollapse,
//  //shVisibleThin
//  shHiding // shFadeOut

//procedure TNEDEditorScrollbar.CMVisibleChanged(var Msg: TMessage);
//begin
//  if (Msg.WParam = 1) and (FState = shHidden) then
//    SetState(shShowing);
//  //
//  inherited;
//  //
//  if (Msg.WParam = 0) and (FState = shHiding) then
//    SetState(shHidden)
//
////  if (FShowing = shShowing) or (FShowing = shHiding) then begin
////    if FShowing = shHiding then
////      SetState(shHidden)
////    else if FShowing = shShowing then
////      SetState(shVisibleThin);
////  end;
//end;

//procedure TNEDEditorScrollbar.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
//begin
//  Msg.Result := 1;
//end;

//procedure TNEDEditorScrollbar.WMNCHitTest(var Msg: TWMNCHitTest);
//var
//  P: TPoint;
//begin
////  inherited;
//  // this should make entire control transparent when mouse clicking
//  P := FEditorControl.ScreenToClient(Point(Msg.XPos, Msg.YPos));
//
//  case HitTest(P) of
//    htTopButton,
//    htBottomButton,
//    htThumb: Msg.Result := HTCLIENT;
//    htTrack: begin
//      Msg.Result := HTCLIENT;
////      if FState = shVisibleFull then
////        Msg.Result := HTCLIENT
////      else
////        Msg.Result := HTTRANSPARENT;
//    end;
//  else
//    Msg.Result := HTTRANSPARENT;
//  end;
//end;

procedure TNEDEditorScrollbar.CalcAutoRange;
begin
  if FScrollBarKind = sbVertical then begin
    Range := FEditorControl.VisibleLinesCount;
  end
  else begin
    Range := 0; // @TODO: calculate longest line
  end;
end;

function TNEDEditorScrollbar.ControlSize(ControlSB, AssumeSB: Boolean): Integer;
begin
  //
  // do not change that:
  //   ... Visible and (FRange > FEditorControl.ClientHeight div FEditorControl.LineHeight) ...
  // into
  //   IsScrollBarVisible
  // because of cyclic function call
  //
  if FScrollBarKind = sbVertical then
    Result := ClientHeight - IfThen(not ControlSB and not AssumeSB and Visible and
                                    (FRange > FEditorControl.ClientHeight div FEditorControl.LineHeight),
                                    FEditorControl.HorizontalScrollBar.Size)
  else
    Result := ClientWidth - IfThen(not ControlSB and not AssumeSB and Visible and
                                   (FRange > FEditorControl.ClientWidth div FEditorControl.CharWidth),
                                   FEditorControl.VerticalScrollBar.Size);
end;

function TNEDEditorScrollbar.CalculateRect(const DrawRect: TRect; const Button: TNEDCustomScrollBarMeasureEnum): TRect;
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
    smClient: begin
      Result := DrawRect;
    end;

    smTop: begin
      Result := Rect(
        DrawRect.Left,
        DrawRect.Top,
        DrawRect.Right,
        DrawRect.Top + ButtonSize
      );
    end;

    smBottom: begin
      Result := Rect(
        DrawRect.Left,
        DrawRect.Bottom - ButtonSize,
        DrawRect.Right,
        DrawRect.Bottom
      );
    end;

    smScroll: begin
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

    smTrack: begin
      Result := Rect(
        DrawRect.Left,
        ButtonSize,
        DrawRect.Right,
        DrawRect.Bottom - ButtonSize
      );
    end;
  else
    Result := Rect(0, 0, 0, 0);
  end;
end;

function TNEDEditorScrollbar.IsMouseInScrollBar(X, Y: Integer): Boolean;
begin
  FPrevMouseInScrollBar := FMouseInScrollBar;
  FMouseInScrollBar := False;
  if FScrollBarKind = sbVertical then begin
    if (X >= ClientRect.Left) then
      FMouseInScrollBar := True;
  end
  else begin // sbHorizontal
    if (Y >= ClientRect.Top) then
      FMouseInScrollBar := True;
  end;
  Result := FMouseInScrollBar;
end;

procedure TNEDEditorScrollbar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Action: TNEDCustomScrollBarActionEnum;
begin
  if not IsMouseInScrollBar(X, Y) then
    Exit;

  if Button <> mbLeft then
    Exit;

  FPressedPart := HitTest(Point(X, Y));

  case FPressedPart of
    htThumb: begin
      FDragging := True;

      FDragStartY := Y;
      FDragStartPosition := Position;

      FEditorControl.MouseCapture := True;
    end;

    htTrack: begin
      if Y < CalculateRect(ClientRect, smScroll).Top then begin
        Action := saPageUp;
        Position := Position - PageSize;
      end
      else begin
        Action := saPageDown;
        Position := Position + PageSize;
      end;

      DoScroll(Action, Position);

      FEditorControl.MouseCapture := True;
      FRepeatTimer.Enabled := True;
    end;

    htTopButton: begin
      Position := Position - 1;

      DoScroll(saLineUp, Position);

      FRepeatTimer.Enabled := True;
    end;

    htBottomButton: begin
      Position := Position + 1;

      DoScroll(saLineDown, Position);

      FRepeatTimer.Enabled := True;
    end;
  end;
//  SetParentControlPosition;
end;

procedure TNEDEditorScrollbar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  DeltaPixels: Integer;
  TrackHeight: Integer;
  ThumbHeight: Integer;
  AvailableTrack: Integer;
  NewPos: Integer;
//  Action: TCustomScrollBarActionEnum;
begin
  IsMouseInScrollBar(X, Y);
  if not FEditorControl.MouseCapture and (FPrevMouseInScrollBar <> FMouseInScrollBar) then begin
    if FMouseInScrollBar then
      BeginExpand
    else
      BeginCollapse;
  end;

  if not FDragging and not FMouseInScrollBar then
    Exit;

  if not FDragging then
    Exit;

  DeltaPixels := Y - FDragStartY;

  ThumbHeight := CalculateRect(ClientRect, smScroll).Height;
  TrackHeight := ClientHeight;

  AvailableTrack := TrackHeight - ThumbHeight;

  if AvailableTrack <= 0 then
    Exit;

  NewPos := FDragStartPosition + MulDiv(DeltaPixels, GetMaxPosition, AvailableTrack);

  Position := EnsureRange(NewPos, 0, GetMaxPosition);
//  SetParentControlPosition;

  DoScroll(saThumbTrack, Position);
end;

procedure TNEDEditorScrollbar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not IsMouseInScrollBar(X, Y) and not FPrevMouseInScrollBar and not FDragging then
    Exit;

  if Button <> mbLeft then
    Exit;

  FDragging := False;
  FEditorControl.MouseCapture := False;
  FRepeatTimer.Enabled := False;
  FPressedPart := htNone;
  DoScroll(saThumbPosition, Position);
end;

procedure TNEDEditorScrollbar.Paint(const ACanvas: TCanvas);
var
  LRect: TRect;
begin
  LRect := ClientRect;
  DrawBackground(ACanvas, LRect); // clear background
  DrawButtons(ACanvas, GetInteractiveRect); // draw buttons
end;

procedure TNEDEditorScrollbar.DrawBackground(const ACanvas: TCanvas; var ARect: TRect);
begin
  // do we really want to paint background ??? how about transparent control
  ACanvas.Brush.Style := bsSolid;
  // transparent background simulation
  ACanvas.Brush.Color := FEditorControl.Color; // TWinControlAccess(Parent).Color;
  ACanvas.FillRect(ARect);
end;

function LerpByte(A, B: Byte; T: Single): Byte;
begin
  Result := Round(A + (B - A) * T);
end;

procedure TNEDEditorScrollbar.DrawButtons(const ACanvas: TCanvas; const ARect: TRect);
var
  ThumbRect: TRect;
  BG, FG: COLORREF;
  R, G, B: Byte;
  T: Single;
begin
  if FState = shHidden then
    Exit;

  // paint top thumb
  if (FState = shExpand) or (FState = shVisibleFull) or (FState = shCollapse) then begin
    ThumbRect := CalculateRect(ARect, smTop);
    ACanvas.Brush.Color := FEditorControl.Color;
    ACanvas.FillRect(ThumbRect);
    DrawArrow(ACanvas, ThumbRect, daUp, [], 2, 4);
  end;

  // paint scroll
  ThumbRect := CalculateRect(ARect, smScroll);
  // thumb color
  BG := ColorToRGB(FEditorControl.Color); // TWinControlAccess(Parent).Color);
  FG := ColorToRGB(clGray);

  T := FCurrentAlpha / 255.0;

  R := LerpByte(GetRValue(BG), GetRValue(FG), T);
  G := LerpByte(GetGValue(BG), GetGValue(FG), T);
  B := LerpByte(GetBValue(BG), GetBValue(FG), T);

  ACanvas.Brush.Color := RGB(R, G, B);
  ACanvas.FillRect(ThumbRect);

  // paint bottom thumb
  if (FState = shExpand) or (FState = shVisibleFull) or (FState = shCollapse) then begin
    ThumbRect := CalculateRect(ARect, smBottom);
    ACanvas.Brush.Color := FEditorControl.Color;
    ACanvas.FillRect(ThumbRect);
    DrawArrow(ACanvas, ThumbRect, daDown, [], 2, 4);
  end;
end;

procedure TNEDEditorScrollbar.Update(ControlSB, AssumeSB: Boolean);
var
  Code: Word;
//  ScrollInfo: TScrollInfo;
  CalcRange: Integer;
begin
  CalcRange := 0;
  Code := SB_HORZ;
  if FScrollBarKind = sbVertical then
    Code := SB_VERT;

  if Visible then begin
    CalcRange := Range - (ControlSize(ControlSB, AssumeSB) div FEditorControl.LineHeight);
    if CalcRange < 0 then
      CalcRange := 0;
  end;

//  ScrollInfo.cbSize := SizeOf(ScrollInfo);
//  ScrollInfo.fMask := SIF_ALL;
//  ScrollInfo.nMin := 0;
//  if CalcRange > 0 then
//    ScrollInfo.nMax := Range
//  else
//    ScrollInfo.nMax := 0;
//  ScrollInfo.nPage := (ControlSize(ControlSB, AssumeSB) + 1) div FEditorControl.LineHeight;
//  ScrollInfo.nPos := FPosition;
//  ScrollInfo.nTrackPos := FPosition;
//  SetScrollInfo(FEditorControl.Handle, Code, ScrollInfo, True);
//  SetPosition(FPosition);

  if CalcRange > 0 then
    SetInfo(Range, (ControlSize(ControlSB, AssumeSB) + 1) div FEditorControl.LineHeight, FPosition)
  else
    SetInfo(0, (ControlSize(ControlSB, AssumeSB) + 1) div FEditorControl.LineHeight, FPosition);
end;

{ TNEDEditorOptions }

constructor TNEDEditorOptions.Create(const AParentControl: TNEDCustomEditorView);
begin
  FEditorControl := AParentControl;
  //
  FEncoding := TEncoding.UTF8;
  //
  FEditorProperties := [
    epAutoIndent,
    epAutoSave,
    epDisableScrollArrows,
    epEnhancedHomeKey,
    epEnhancedEndKey,
    epGroupUndo,
    epUndoAfterSave,
    epKeepCaretX,
    epRightMouseMovesCursor,
    //epScrollPastEof,
    epScrollPastEol,
    epShowRightMargin,
    //epShowNonVisibleLines,
    epSmartTabs,
    epSmartTabsDelete,
    epTabIndent,
    epTabsToSpaces,
    epTrimTrailingSpaces,
    epCopyPlainText,
    epWordWrap,
    epWrapWithRightEdge
  ];
  //
  FAutoSaveInterval := 10000; // 10 seconds
  FLineBreak := #13#10; // CRLF
  FUndoLimit := 65536; // max records
  FRightMargin := 160;
  FTabWidth := 4;
  FTabWidthToSpaces := 2;
  FTextEntryMode := teInsert;
end;

destructor TNEDEditorOptions.Destroy;
begin
  if not TEncoding.IsStandardEncoding(FEncoding) then
    FEncoding.Free;
  FEditorControl := Nil;
  inherited;
end;

procedure TNEDEditorOptions.SetEncoding(const Value: TEncoding);
begin
  if FEncoding <> Value then begin
    if not TEncoding.IsStandardEncoding(FEncoding) then
      FEncoding.Free;

    FEncoding := Value;

    UpdateEditor;
  end;
end;

procedure TNEDEditorOptions.SetEditorProperties(const Value: TNEDEditorProperties);
begin
  if FEditorProperties <> Value then begin
    FEditorProperties := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorOptions.SetAutoSaveInterval(const Value: Integer);
begin
  if FAutoSaveInterval <> Value then begin
    FAutoSaveInterval := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorOptions.SetLineBreak(const Value: String);
begin
  if FLineBreak <> Value then begin
    FLineBreak := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorOptions.SetUndoLimit(const Value: Integer);
begin
  if FUndoLimit <> Value then begin
    FUndoLimit := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorOptions.SetRightMargin(const Value: Integer);
begin
  if FRightMargin <> Value then begin
    FRightMargin := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorOptions.SetTabWidth(const Value: Integer);
begin
  if FTabWidth <> Value then begin
    FTabWidth := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorOptions.SetTabWidthToSpaces(const Value: Integer);
begin
  if FTabWidthToSpaces <> Value then begin
    FTabWidthToSpaces := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorOptions.SetTextEntryMode(const Value: TNEDEditorTextEntryModeEnum);
begin
  if FTextEntryMode <> Value then begin
    FTextEntryMode := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorOptions.SetTextMargin(const Value: Integer);
begin
  if FTextMargin <> Value then begin
    FTextMargin := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorOptions.UpdateEditor;
begin
  if FEditorControl <> Nil then begin
    FEditorControl.ActiveLineIndex := FEditorControl.CaretToLineColumn(FEditorControl.CaretPosition).Line;
    FEditorControl.Repaint;
  end;
end;

function TNEDEditorOptions.WordWrap: Boolean;
begin
  Result := epWordWrap in FEditorProperties;
end;

function TNEDEditorOptions.ReadOnly: Boolean;
begin
  Result := epReadOnly in FEditorProperties;
end;

{ TNEDEditorColors }

//One Dark Theme Core Colors
//  UI & Backgrounds
//    Background: rgb(40, 44, 52) — Deep matte grey.
//    Gutter/Line Numbers: rgb(75, 82, 99) — Subdued gray-blue.
//    Selection Highlighting: rgb(62, 68, 81) — Medium gray.
//  Syntax & Tokens
//    Keywords: rgb(198, 120, 221) — Soft magenta.
//    Functions & Methods: rgb(97, 175, 239) — Clear sky blue.
//    Variables & Properties: rgb(224, 108, 117) — Soft coral red.
//    Strings & Values: rgb(152, 195, 121) — Mint sage green.
//    Classes & Types: rgb(229, 192, 123) — Warm amber yellow.
//    Comments: rgb(92, 99, 112) — Muted slate gray.

//Core Environment
//  Background: RGB(40, 44, 52) | #282C34
//  Default Text: RGB(171, 178, 191) | #ABB2BF
//Syntax Tokens
//  Keywords (e.g., if, return): RGB(198, 120, 221) | #C678DD
//  Functions & Methods: RGB(97, 175, 239) | #61AFEF
//  Strings: RGB(152, 195, 121) | #98C379
//  Variables & Properties: RGB(224, 108, 117) | #E06C75
//  Numbers & Booleans: RGB(209, 154, 102) | #D19A66
//  Classes & Types: RGB(229, 192, 123) | #E5C07B
//  Comments: RGB(92, 99, 112) | #5C6370

// 0 Black         #000000
// 1 Blue          #0000AA
// 2 Green         #00AA00
// 3 Cyan          #00AAAA
// 4 Red           #AA0000
// 5 Magenta       #AA00AA
// 6 Brown         #AA5500
// 7 Light Gray    #AAAAAA

// 8 Dark Gray     #555555
// 9 Light Blue    #5555FF
//10 Light Green   #55FF55
//11 Light Cyan    #55FFFF
//12 Light Red     #FF5555
//13 Light Magenta #FF55FF
//14 Yellow        #FFFF55
//15 White         #FFFFFF

constructor TNEDEditorColors.Create(const AParentControl: TNEDCustomEditorView);
begin
  FEditorControl := AParentControl;
  //
  FBlockSelectedColor := $3e4451; // rgb(62, 68, 81)
  FLineSelectedColor := $4b52c7; // rgb(75, 82, 199)
  FLineFocusedColor := $375264; // rgb(55, 82, 100)
  FRightMarginColor := $c8dc40; // rgb(200, 220, 64)
end;

destructor TNEDEditorColors.Destroy;
begin
  FEditorControl := Nil;
  inherited;
end;

procedure TNEDEditorColors.UpdateEditor;
begin
  if FEditorControl <> Nil then
    FEditorControl.Repaint;
end;

procedure TNEDEditorColors.SetBlockSelectedColor(const Value: TColor);
begin
  if FBlockSelectedColor <> Value then begin
    FBlockSelectedColor := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorColors.SetLineSelectedColor(const Value: TColor);
begin
  if FLineSelectedColor <> Value then begin
    FLineSelectedColor := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorColors.SetLineFocusedColor(const Value: TColor);
begin
  if FLineFocusedColor <> Value then begin
    FLineFocusedColor := Value;
    UpdateEditor;
  end;
end;

procedure TNEDEditorColors.SetRightMarginColor(const Value: TColor);
begin
  if FRightMarginColor <> Value then begin
    FRightMarginColor := Value;
    UpdateEditor;
  end;
end;

{ TNEDEditorCommand }

constructor TNEDEditorCommand.Create(const CommandID: TNEDCommandIDType; DisplayName, DisplayHint: String; const InvokeFunction: TNEDCommandInvokeFunction);
begin
  FEditorControl := Nil; // set by TNEDEditorKeyboardMap
  FCommandID := CommandID;
  FIsComplexCommand := False;
  FIsFullKeyMap := True;
  FDisplayName := DisplayName;
  FDisplayHint := DisplayHint;
  FInvokeFunction := InvokeFunction;
end;

destructor TNEDEditorCommand.Destroy;
begin
  FEditorControl := Nil;
  FCommandID := 0;
  FDisplayName := '';
  FDisplayHint := '';
  FInvokeFunction := Nil;
  inherited;
end;

function TNEDEditorCommand.InvokeCommand: Boolean;
begin
  Result := False;
  if FEditorControl = Nil then
    Exit;
  if not Assigned(FInvokeFunction) then
    Exit;
  //
  FInvokeFunction(Self, FCommandID, Result);
end;

{ TNEDEditorKeyboardMap }

constructor TNEDEditorKeyboardMap.Create(const AParentControl: TNEDCustomEditorView);
begin
  FEditorControl := AParentControl;
  //
  FEditorKeyboardMap := TObjectDictionary<TNEDCommandShortCut, TNEDEditorCommand>.Create([doOwnsValues]);
end;

destructor TNEDEditorKeyboardMap.Destroy;
begin
  FEditorKeyboardMap.Free;
  inherited;
end;

procedure TNEDEditorKeyboardMap.AddSimpleMapping(const Key: Word; const Shift: TShiftState; const Command: TNEDEditorCommand);
var
  ShortCutKeyMap: TNEDCommandShortCut;
begin
  if Command = Nil then
    Exit;

  Command.FEditorControl := FEditorControl;
  ShortCutKeyMap := Menus.ShortCut(Key, Shift);

  if not FEditorKeyboardMap.ContainsKey(ShortCutKeyMap) then
    FEditorKeyboardMap.Add(ShortCutKeyMap, Command);
end;

procedure TNEDEditorKeyboardMap.AddComplexMapping(const Key1: Word; const Shift1: TShiftState; const Key2: Word; const Shift2: TShiftState; const Command: TNEDEditorCommand);
var
  ShortCutKeyMap1: TShortCut;
  ShortCutKeyMap2: TShortCut;
  ShortCutKeyMap: TNEDCommandShortCut;
begin
  if Command = Nil then
    Exit;

  Command.FEditorControl := FEditorControl;
  Command.FIsComplexCommand := True;
  Command.FIsFullKeyMap := False;

  ShortCutKeyMap1 := Menus.ShortCut(Key1, Shift1);
  ShortCutKeyMap2 := Menus.ShortCut(Key2, Shift2);
  ShortCutKeyMap := (ShortCutKeyMap1 shl 16) or (ShortCutKeyMap2 and $FFFF); // combine shortcut1 with shortcut2

  if not FEditorKeyboardMap.ContainsKey(ShortCutKeyMap1 shl 16) then
    FEditorKeyboardMap.Add(ShortCutKeyMap1 shl 16, Command);
  Command.FIsFullKeyMap := True;
  if not FEditorKeyboardMap.ContainsKey(ShortCutKeyMap) then
    FEditorKeyboardMap.Add(ShortCutKeyMap, Command);
end;

function TNEDEditorKeyboardMap.IsComplexCommand(const Key: Word; const Shift: TShiftState): Boolean;
var
  ShortCutKeyMap: TNEDCommandShortCut;
begin
  ShortCutKeyMap := Menus.ShortCut(Key, Shift);
  ShortCutKeyMap := ShortCutKeyMap shl 16;

  Result := FEditorKeyboardMap.ContainsKey(ShortCutKeyMap);
end;

function TNEDEditorKeyboardMap.GetCommand(const Key: Word; const Shift: TShiftState): TNEDEditorCommand;
var
  ShortCutKeyMap: TNEDCommandShortCut;
begin
  ShortCutKeyMap := Menus.ShortCut(Key, Shift);

  if not FEditorKeyboardMap.TryGetValue(ShortCutKeyMap, Result) then
    Result := Nil;
end;

function TNEDEditorKeyboardMap.GetCommand(const Key1: Word; const Shift1: TShiftState; const Key2: Word; const Shift2: TShiftState): TNEDEditorCommand;
var
  ShortCutKeyMap1: TShortCut;
  ShortCutKeyMap2: TShortCut;
  ShortCutKeyMap: TNEDCommandShortCut;
begin
  ShortCutKeyMap1 := Menus.ShortCut(Key1, Shift1);
  ShortCutKeyMap2 := Menus.ShortCut(Key2, Shift2);
  ShortCutKeyMap := (ShortCutKeyMap1 shl 16) or (ShortCutKeyMap2 and $FFFF); // combine shortcut1 with shortcut2

  if not FEditorKeyboardMap.TryGetValue(ShortCutKeyMap, Result) then
    Result := Nil;
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
  //Color := $001F1F1F;
  Color := $00330000; //xxBBGGRR
  //Color := $00333333; //xxBBGGRR
  //
  Font.Name := 'Source Code Pro Semibold';
  Font.Size := 12;
  Font.Color := clWhite;
  //
  FDocument := Nil;
  FObserver := Nil;
  FGutter := TNEDEditorGutter.Create(Self);
  FMinimap := TNEDEditorMinimap.Create;
  FCaret := TNEDEditorCaret.Create(Self);
  FSelection := TNEDEditorSelection.Create;
  FVerticalScrollBar := TNEDEditorScrollbar.Create(Self, sbVertical);
  FHorizontalScrollBar := TNEDEditorScrollbar.Create(Self, sbHorizontal);
  FVerticalScrollBar.Visible := True;
  FHorizontalScrollBar.Visible := True;
  FVerticalScrollBar.OnScroll := VerticalScroll;
  FHorizontalScrollBar.OnScroll := HorizontalScroll;
  //
  FOptions := TNEDEditorOptions.Create(Self);
  FColors := TNEDEditorColors.Create(Self);
  FKeyboardMap := TNEDEditorKeyboardMap.Create(Self);
  FProcessNextKeyMap := True;
  FPreviousKeyMap.Key := 0;
  FPreviousKeyMap.Shift := [];
  FIgnoreInput := False;
  FWindowSwitchQueryNext := False;
  FUpdatingScrollBars := False;
  //
  //
  FModified := False;
  FCommentLineCount := 0;
  FBlankLineCount := 0;
  //
  FTopIndex := 0;
  FLeftIndex := 0;
  FTextWidth := -1;
  FTextHeight := -1;
  FEditorLines := -1;
  FEditorChars := -1;
  FVisibleLinesCount := 0;
  FLongestLineCharsCount := 0;
  FActiveLineIndex := 0;
//  FCaretPos.Line := 1;
//  FCaretPos.Column := 1;
  //
  InitializeKeyboardMappings;
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
  FKeyboardMap.Free;
  inherited;
end;

procedure TNEDCustomEditorView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  // remove OS (windows) painted scrollbars
  // after this we need our own scrollbars renderer, that will paint them as we want
  Params.WindowClass.style := Params.WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  // don't repaint areas occupied by child controls - not sure for now, if this is usefull
  Params.Style := Params.Style or WS_CLIPCHILDREN or WS_TABSTOP;
  // remove OS (windows) painted scrollbars
  Params.Style := Params.Style and not (WS_VSCROLL or WS_HSCROLL);
end;

procedure TNEDCustomEditorView.CreateHandle;
begin
  inherited CreateHandle;

  if FTextWidth = -1 then begin
    FTextWidth := Canvas.TextWidth(' ');
    FEditorChars := GetEditorChars;
  end;
  if FTextHeight = -1 then begin
    FTextHeight := Canvas.TextHeight(' ');
    FEditorLines := GetEditorLines;
  end;
//  HideNativeScrollBars;
  UpdateScrollBars;
  ShowModernScrollBars;
end;

procedure TNEDCustomEditorView.DestroyHandle;
begin
  FTextWidth := -1;
  FTextHeight := -1;
  inherited DestroyHandle;
end;

procedure TNEDCustomEditorView.Loaded;
begin
  inherited Loaded;

  if FTextWidth = -1 then begin
    FTextWidth := Canvas.TextWidth(' ');
    FEditorChars := GetEditorChars;
  end;
  if FTextHeight = -1 then begin
    FTextHeight := Canvas.TextHeight(' ');
    FEditorLines := GetEditorLines;
  end;
end;

procedure TNEDCustomEditorView.DisableAutoRange;
begin
  Inc(FAutoRangeCount);
end;

procedure TNEDCustomEditorView.EnableAutoRange;
begin
  if FAutoRangeCount > 0 then begin
    Dec(FAutoRangeCount);
    if (FAutoRangeCount = 0) and (FHorizontalScrollBar.Visible or FVerticalScrollBar.Visible) then
      CalcAutoRange;
  end;
end;

function TNEDCustomEditorView.GetCaretPosition: TNEDCaretPosition;
begin
  Result := FCaret.CaretPosition;
end;

procedure TNEDCustomEditorView.InitializeKeyboardMappings;
begin
  FKeyboardMap.AddSimpleMapping(VK_LEFT,  [], TNEDEditorCommand.Create(cmdCursorLeft,      'CursorLeft', 'Move cursor left one char', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(VK_RIGHT, [], TNEDEditorCommand.Create(cmdCursorRight,     'CursorRight', 'Move cursor right one char', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(VK_UP,    [], TNEDEditorCommand.Create(cmdCursorUp,        'CursorUp', 'Move cursor up one line', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(VK_DOWN,  [], TNEDEditorCommand.Create(cmdCursorDown,      'CursorDown', 'Move cursor down one line', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(VK_HOME,  [], TNEDEditorCommand.Create(cmdCursorLineStart, 'CursorLineStart', 'Move cursor to beginning of line', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(VK_END,   [], TNEDEditorCommand.Create(cmdCursorLineEnd,   'CursorLineEnd', 'Move cursor to end of line', DoInternalCommand));
  //
  FKeyboardMap.AddSimpleMapping(VK_PRIOR, [], TNEDEditorCommand.Create(cmdCursorPageUp,    'CursorPageUp', '', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(VK_NEXT,  [], TNEDEditorCommand.Create(cmdCursorPageDown,  'CursorPageDown', '', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(VK_LEFT,  [ssAlt],  TNEDEditorCommand.Create(cmdCursorPageLeft,     'CursorPageLeft', 'Move cursor left one page', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(VK_RIGHT, [ssAlt],  TNEDEditorCommand.Create(cmdCursorPageRight,    'CursorPageRight', 'Move cursor right one page', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(VK_PRIOR, [ssCtrl], TNEDEditorCommand.Create(cmdCursorPageTop,      'CursorPageTop', 'Move cursor to top of page', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(VK_NEXT,  [ssCtrl], TNEDEditorCommand.Create(cmdCursorPageBottom,   'CursorPageBottom', 'Move cursor to bottom of page', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(VK_HOME,  [ssCtrl], TNEDEditorCommand.Create(cmdCursorEditorTop,    'CursorEditorTop', 'Move cursor to absolute beginning', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(VK_END,   [ssCtrl], TNEDEditorCommand.Create(cmdCursorEditorBottom, 'CursorEditorBottom', 'Move cursor to absolute end', DoInternalCommand));
  //
  FKeyboardMap.AddSimpleMapping(VK_LEFT,  [ssCtrl], TNEDEditorCommand.Create(cmdCursorWordLeft,  'CursorWordLeft', 'Move cursor left one word', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(VK_RIGHT, [ssCtrl], TNEDEditorCommand.Create(cmdCursorWordRight, 'CursorWordRight', 'Move cursor right one word', DoInternalCommand));
//  FKeyboardMap.AddSimpleMapping(VK_, [], TNEDEditorCommand.Create(cmd, '', '', DoInternalCommand));

  FKeyboardMap.AddSimpleMapping(VK_BACK,   [], TNEDEditorCommand.Create(cmdEditDelete,                   'EditDelete', 'Delete last char (i.e. backspace key)', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(VK_DELETE, [], TNEDEditorCommand.Create(cmdEditDeleteInPlace,            'EditDeleteInPlace', 'Delete char at cursor (i.e. delete key)', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(VK_DELETE, [ssAlt], TNEDEditorCommand.Create(cmdEditDeleteWholeWord,     'EditDeleteWholeWord', 'Delete Word under cursor', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(VK_BACK,   [ssCtrl], TNEDEditorCommand.Create(cmdEditDeleteBOWord,       'EditDeleteBOWord', 'Delete from cursor to start of word', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(Ord('T'),  [ssCtrl], TNEDEditorCommand.Create(cmdEditDeleteEOWord,       'EditDeleteEOWord', 'Delete from cursor to end of word', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(VK_BACK,   [ssShift], TNEDEditorCommand.Create(cmdEditDeleteBOL,         'EditDeleteBOL', 'Delete from cursor to beginning of line', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(Ord('Y'),  [ssCtrl, ssShift], TNEDEditorCommand.Create(cmdEditDeleteEOL, 'EditDeleteEOL', 'Delete from cursor to end of line', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(Ord('Y'),  [ssCtrl], TNEDEditorCommand.Create(cmdEditDeleteLine,         'EditDeleteLine', 'Delete current line', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(VK_DELETE, [ssCtrl, ssShift], TNEDEditorCommand.Create(cmdEditClearAll,  'EditClearAll', 'Delete everything', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(VK_RETURN, [], TNEDEditorCommand.Create(cmdEditLineBreak,                'EditLineBreak', 'Break line at current position, move caret to new line', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(VK_RETURN, [ssShift], TNEDEditorCommand.Create(cmdEditInsertLine,        'EditInsertLine', 'Break line at current position, leave caret', DoInternalCommand));
//  FKeyboardMap.AddSimpleMapping(VK_, [], TNEDEditorCommand.Create(cmdEditChar, '', 'Insert a character at current position', DoInternalCommand));

  FKeyboardMap.AddSimpleMapping(VK_TAB, [ssCtrl], TNEDEditorCommand.Create(cmdWindowSwitchToNext, 'WindowSwitchToNext', 'Switch to next editor window if any available', DoInternalCommand));
  FKeyboardMap.AddSimpleMapping(VK_TAB, [ssCtrl, ssShift], TNEDEditorCommand.Create(cmdWindowSwitchToPrev, 'WindowSwitchToPrev', 'Switch to prev editor window if any available', DoInternalCommand));

end;

procedure TNEDCustomEditorView.DoInternalCommand(Sender: TNEDEditorCommand; const CommandID: TNEDCommandIDType; var Handled: Boolean);

  procedure SetHandled;
  begin
    Handled := True;
  end;

var
  LineLength: Integer;
  LineText: String;
  FirstChar: Integer;
  LCaretPos: TNEDCaretPosition;
  LTextPos: TNEDTextPosition;
  LTopOffset, LLastLine: Integer;
begin
  try
    LCaretPos := CaretPosition;

    case CommandID of
      cmdCursorLeft: begin
        LTextPos := CaretToLineColumn(LCaretPos);
        if (LTextPos.Line < FTopIndex) or (LTextPos.Line > (FTopIndex + FEditorLines)) then
          ScrollLineIntoView(ActiveLineIndex);
        //
        LCaretPos.X := Max(LCaretPos.X - 1, 1);
        SetHandled;
      end;

      cmdCursorRight: begin
        LTextPos := CaretToLineColumn(LCaretPos);
        if (LTextPos.Line < FTopIndex) or (LTextPos.Line > (FTopIndex + FEditorLines)) then
          ScrollLineIntoView(ActiveLineIndex);
        //
        if not (epScrollPastEol in Options.EditorProperties) then begin
          LineLength := Document.Lines[CaretToLineColumn(LCaretPos).Line].Length;
          if LineLength > 0 then
            LCaretPos.X := Min(LCaretPos.X + 1, LineLength + 1);
        end
        else
          LCaretPos.X := Max(LCaretPos.X + 1, 1); // ensure LCaretPos.X always greater then zero
        SetHandled;
      end;

      cmdCursorUp: begin
        LTextPos := CaretToLineColumn(LCaretPos);
        if (LTextPos.Line < FTopIndex) or (LTextPos.Line > (FTopIndex + FEditorLines)) then
          ScrollLineIntoView(ActiveLineIndex);
        //
        LCaretPos.Y := Max(LCaretPos.Y - 1, 1);
        LTextPos := CaretToLineColumn(LCaretPos);
        if LTextPos.Line = FTopIndex - 1 then
          ScrollBy(0, -1);
        ActiveLineIndex := LTextPos.Line;
        SetHandled;
      end;

      cmdCursorDown: begin
        LTextPos := CaretToLineColumn(LCaretPos);
        if (LTextPos.Line < FTopIndex) or (LTextPos.Line > (FTopIndex + FEditorLines)) then
          ScrollLineIntoView(ActiveLineIndex);
        //
        LCaretPos.Y := Min(LCaretPos.Y + 1, LineColumnToCaret(VisibleLinesCount - 1, 0).Y);
        LTextPos := CaretToLineColumn(LCaretPos);
        if LTextPos.Line = FTopIndex + FEditorLines + 1 then
          ScrollBy(0, 1);
        ActiveLineIndex := LTextPos.Line;
        SetHandled;
      end;

      cmdCursorLineStart: begin
        if epEnhancedHomeKey in Options.EditorProperties then begin
          LineText := Document.GetLineText(CaretToLineColumn(LCaretPos).Line);
          FirstChar := 1;
          while (FirstChar < LineText.Length) and IsWhiteSpace(LineText[FirstChar]) do
            Inc(FirstChar);

          if LCaretPos.X > FirstChar then
            LCaretPos.X := FirstChar
          else if FirstChar = LCaretPos.X then
            LCaretPos.X := 1
          else
            LCaretPos.X := FirstChar;
        end
        else
          LCaretPos.X := 1;
        ActiveLineIndex := CaretToLineColumn(LCaretPos).Line;
        SetHandled;
      end;

      cmdCursorLineEnd: begin
        LineLength := Document.Lines[CaretToLineColumn(LCaretPos).Line].Length;
        if epEnhancedEndKey in Options.EditorProperties then begin
          LineText := Document.GetLineText(CaretToLineColumn(LCaretPos).Line);
          FirstChar := LineLength;
          while (LineText.Length > 0) and (FirstChar > 1) and IsWhiteSpace(LineText[FirstChar]) do
            Dec(FirstChar);

          if FirstChar = 0 then
            LCaretPos.X := 1
          else if LCaretPos.X > FirstChar then
            LCaretPos.X := FirstChar
          else if FirstChar = LCaretPos.X then
            LCaretPos.X := LineLength
          else
            LCaretPos.X := FirstChar;
        end
        else
          LCaretPos.X := LineLength;

        if (LCaretPos.X > 1) and (epScrollPastEol in Options.EditorProperties) then
          Inc(LCaretPos.X);

        ActiveLineIndex := CaretToLineColumn(LCaretPos).Line;
        SetHandled;
      end;

      cmdCursorPageUp: begin
        //LTopOffset := Max(FTopIndex - FEditorLines - 1, 0);
        LCaretPos.Y := Max(LCaretPos.Y - FEditorLines - 1, 1);
        //SetTopIndex(LTopOffset);
        ActiveLineIndex := CaretToLineColumn(LCaretPos).Line;
        SetHandled;
      end;

      cmdCursorPageDown: begin
        //LTopOffset := Min(FTopIndex + FEditorLines + 1, VisibleLinesCount - 1);
        LCaretPos.Y := Min(LCaretPos.Y + FEditorLines + 1, LineColumnToCaret(VisibleLinesCount - 1, 0).Y);
        //SetTopIndex(LTopOffset);
        ActiveLineIndex := CaretToLineColumn(LCaretPos).Line;
        SetHandled;
      end;

      cmdCursorPageTop: begin
        LTextPos := CaretToLineColumn(LCaretPos);
        LTopOffset := LTextPos.Line - FTopIndex; // how far from top of the view in lines
        LCaretPos.Y := Max(LCaretPos.Y - (LineColumnToCaret(LTopOffset, 0).Y - 1), 1);
        ActiveLineIndex := CaretToLineColumn(LCaretPos).Line;
        SetHandled;
      end;

      cmdCursorPageBottom: begin
        LTextPos := CaretToLineColumn(LCaretPos);
        LTopOffset := LTextPos.Line - FTopIndex; // how far from top of the view in lines
        LLastLine := FEditorLines;
        if LLastLine * LineHeight > GetLinesArea.Height then
          Dec(LLastLine);
        LCaretPos.Y := Min(LCaretPos.Y + LineColumnToCaret(LLastLine - LTopOffset - 1, 0).Y, LineColumnToCaret(VisibleLinesCount - 1, 0).Y);
        ActiveLineIndex := CaretToLineColumn(LCaretPos).Line;
        SetHandled;
      end;

      cmdCursorPageLeft: begin
      end;

      cmdCursorPageRight: begin
      end;

      cmdCursorEditorTop: begin
        LCaretPos := TNEDCaretPosition.Create(1, 1);
        ActiveLineIndex := CaretToLineColumn(LCaretPos).Line;
        SetHandled;
      end;

      cmdCursorEditorBottom: begin
        if FVisibleLinesCount > 0 then begin
          LCaretPos := LineColumnToCaret(FVisibleLinesCount - 1, 0);
          ActiveLineIndex := CaretToLineColumn(LCaretPos).Line;
          SetHandled;
        end;
      end;

      cmdEditDelete: begin // backspace
        SetHandled;
        if epReadOnly in Options.EditorProperties then
          Exit;

        if Document.Lines[LTextPos.Line].Deleted and (epShowNonVisibleLines in Options.EditorProperties) then
          Exit;

        FCaret.CaretHide;
        FDocument.Delete(LTextPos, 1, True);
        FCaret.CaretShow;
      end;

      cmdEditDeleteInPlace: begin // delete
        SetHandled;
        if epReadOnly in Options.EditorProperties then
          Exit;

        LTextPos := CaretToLineColumn(LCaretPos);
        if Document.Lines[LTextPos.Line].Deleted and (epShowNonVisibleLines in Options.EditorProperties) then
          Exit;

        FCaret.CaretHide;
        FDocument.Delete(LTextPos, 1, False);
        FCaret.CaretShow;

        Exit;
      end;

      cmdEditDeleteWholeWord: begin
      end;

      cmdEditDeleteBOWord: begin
      end;

      cmdEditDeleteEOWord: begin
      end;

      cmdEditDeleteBOL: begin
      end;

      cmdEditDeleteEOL: begin
      end;

      cmdEditDeleteLine: begin
      end;

      cmdEditClearAll: begin
      end;

      cmdEditLineBreak: begin
      end;

      cmdEditInsertLine: begin
      end;

      cmdEditChar: begin
        SetHandled;
//        FCaret.CaretHide;
//        FDocument.Insert(LTextPos, Key);
//        FCaret.CaretShow;
      end;

      cmdWindowSwitchToNext,
      cmdWindowSwitchToPrev: begin
        SetHandled;
        PostMessage(Application.MainFormHandle,
                    CM_NED_WINDOW_SWITCH,
                    Integer(Self),
                    IfThen(CommandID = cmdWindowSwitchToPrev, -1, 1) //
                   ); // WParam = Editor; LParam = move forward: 1 / backward: -1
      end;

    end;

    FCaret.CaretPosition := LCaretPos;
  finally
    ReportEditorInfo;
  end;
end;

procedure TNEDCustomEditorView.ReportEditorInfo;
var
  Form: TCustomForm;
  EditorInfoDetails: TNEDEditorInfoDetails;
  LineColumn: TNEDTextPosition;
begin
  Form := GetParentForm(Self, False);
  if Form <> Nil then begin
    with EditorInfoDetails do begin
      FilePath := Document.FilePath;
      FileCreation := Document.FileCreateDateTime;
      FileModification := Document.FileModifyDateTime;
      FileReadOnly := Document.FileReadOnly;
      FileEncodingType := GetEditorEncodingStr;
      FileLineBreakType := GetEditorLineBreakStr;
      FileInsertType := GetEditorInsertStr;
      FileType := GetEditorFileTypeStr;
      //
      LineColumn := CaretToLineColumn(Caret.CaretPosition);
      Line := LineColumn.Line + 1;
      Column := LineColumn.Column + 1;
      Lines := Document.LinesCount;
      MaxColumn := -1;
      LineMaxColumn := -1;
      Words := 0;
      EmptyLines := 0;
      CommentLines := 0;
      Modified := Document.Modified;
      Saved := not Document.Modified;
      AutoSave := epAutoSave in Options.EditorProperties;
      TopIndex := TopIndex;
      ViewLines := FEditorLines;
      //
      Status := '';
      StatusProgress := -1;
      StatusMaxProgress := -1;
      Zoom := 100;
      MinZoom := 10;
      MaxZoom := 1000;
      VersionStr := IntToStr(Document.Version);
      Version := Document.Version;
    end;
    //
    if Form.Perform(CM_NED_EDITORINFO_DETAILS, Integer(Self), Integer(@EditorInfoDetails)) = 0 then begin
      // some error happend
    end;
  end;
end;

function TNEDCustomEditorView.GetEditorEncodingStr: String;
begin
  Result := '---';
  if Document.Encoding = TEncoding.ANSI then
    Result := 'ANSI'
  else if Document.Encoding = TEncoding.ASCII then
    Result := 'ASCII'
  else if Document.Encoding = TEncoding.Unicode then
    Result := 'Unicode'
  else if Document.Encoding = TEncoding.UTF7 then
    Result := 'UTF-7'
  else if Document.Encoding = TEncoding.UTF8 then
    Result := 'UTF-8';

  if (Result <> '---') and (Document.FileBOM or Document.EncodingWriteBOM) then
    Result := Result + ' BOM';
end;

function TNEDCustomEditorView.GetEditorLineBreakStr: String;
begin
  Result := '---';
  if Document.LineBreak = #10 then
    Result := 'LF'   // line feed
  else if Document.LineBreak = #13 then
    Result := 'CR'   // carriage return
  else if Document.LineBreak = #13#10 then
    Result := 'CRLF' // carriage return plus line feed
  else if Document.LineBreak = #$85 then
    Result := 'NEL'  // next line
  else if Document.LineBreak = #$2028 then
    Result := 'LS'   // unicode line separator
  else if Document.LineBreak = #$2029 then
    Result := 'PS'   // unicode paragraph separator
  else if Document.LineBreak = #11 then
    Result := 'VT'   // vertical tab
  else if Document.LineBreak = #12 then
    Result := 'FF';  // form feed
end;

function TNEDCustomEditorView.GetEditorInsertStr: String;
begin
  Result := '---';
  if Caret.CaretMode = cmInsert then
    Result := 'Insert'
  else if Caret.CaretMode = cmOverwrite then
    Result := 'Overwrite';
end;

function TNEDCustomEditorView.GetEditorFileTypeStr: String;
var
  ext: String;
begin
// NED Project; NED Source File; Pascal / Delphi; FreePascal; Text; JavaScript; Java; Python; C / C++; PHP; HTML; CSS
  Result := '---';
  ext := ExtractFileExt(Document.FilePath);
  // NitroPascal
  if SameText(ext, '.npe') then
    Result := 'NP project'
  else if SameText(ext, '.npc') then
    Result := 'NP source'

  // Pascal / Delphi
  else if SameText(ext, '.pas') then
    Result := 'Pascal / Delphi unit'
  else if SameText(ext, '.dpr') then
    Result := 'Delphi project'
  else if SameText(ext, '.dpk') then
    Result := 'Delphi package'
  else if SameText(ext, '.inc') then
    Result := 'Pascal / Delphi include'
  else if SameText(ext, '.dfm') then
    Result := 'Delphi form'
  else if SameText(ext, '.fmx') then
    Result := 'Delphi FMX form'
  else if SameText(ext, '.dproj') then
    Result := 'Delphi XML project'

  // Free Pascal
  else if SameText(ext, '.pp') then
    Result := 'Free Pascal unit'
  else if SameText(ext, '.lpr') then
    Result := 'Free Pascal project'
  else if SameText(ext, '.lpi') then
    Result := 'Free Pascal XML project'
  else if SameText(ext, '.lfm') then
    Result := 'Free Pascal form'

  // Text
  else if SameText(ext, '.txt') then
    Result := 'Text file'

  // JavaScript
  else if SameText(ext, '.js') then
    Result := 'JavaScript source'
  else if SameText(ext, '.mjs') then
    Result := 'JavaScript module'
  else if SameText(ext, '.cjs') then
    Result := 'JavaScript common'
  else if SameText(ext, '.jsx') then
    Result := 'JavaScript XML'
  else if SameText(ext, '.vue') then
    Result := 'JavaScript VUE component'
  else if SameText(ext, '.svelte') then
    Result := 'JavaScript SVELTE component'
  else if SameText(ext, '.ts') then
    Result := 'TypeScript'
  else if SameText(ext, '.tsx') then
    Result := 'TypeScript XML'
  else if SameText(ext, '.json') then
    Result := 'JSON source'

  // Java
  else if SameText(ext, '.java') then
    Result := 'Java source'
  else if SameText(ext, '.jsp') then
    Result := 'JavaServer pages'
  else if SameText(ext, '.jspx') then
    Result := 'JavaServer XML pages'
  else if SameText(ext, '.gradle') then
    Result := 'Gradle config'
  else if SameText(ext, '.properties') then
    Result := 'Java config'
  else if SameText(ext, '.policy') then
    Result := 'Java security policy'

  // Python
  else if SameText(ext, '.py') then
    Result := 'Python source'
  else if SameText(ext, '.pyw') then
    Result := 'Python for Windows'
  else if SameText(ext, '.ipynb') then
    Result := 'Interactive Python Notebook'

  // C
  else if SameText(ext, '.c') then
    Result := 'C source'
  else if SameText(ext, '.h') then
    Result := 'C header'

  // C++
  else if SameText(ext, '.cpp') then
    Result := 'C++ source'
  else if SameText(ext, '.cc') then
    Result := 'C++ source'
  else if SameText(ext, '.cxx') then
    Result := 'C++ source'
  else if SameText(ext, '.c++') then
    Result := 'C++ source'
  else if SameText(ext, '.hpp') then
    Result := 'C++ header'
  else if SameText(ext, '.hxx') then
    Result := 'C++ header'
  else if SameText(ext, '.h++') then
    Result := 'C++ header'
  else if SameText(ext, '.inl') then
    Result := 'C++ inline'

  // PHP
  else if SameText(ext, '.php') then
    Result := 'PHP source'
  else if SameText(ext, '.phtml') then
    Result := 'PHP HTML'
  else if SameText(ext, '.phps') then
    Result := 'PHP source view'
  else if SameText(ext, '.env') then
    Result := 'PHP environment'
  else if SameText(ext, '.htaccess') then
    Result := 'PHP Server config'

  // HTML
  else if SameText(ext, '.html') then
    Result := 'HTML source'
  else if SameText(ext, '.htm') then
    Result := 'HTML source'
  else if SameText(ext, '.xhtml') then
    Result := 'eXtendable HTML source'

  // CSS
  else if SameText(ext, '.css') then
    Result := 'CSS source'
  else if SameText(ext, '.pcss') then
    Result := 'PostCSS source'
  else if SameText(ext, '.postcss') then
    Result := 'PostCSS source';
end;

procedure TNEDCustomEditorView.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
  inherited;
  Msg.Result := 1;
end;

procedure TNEDCustomEditorView.CMDialogKey(var Msg: TCMDialogKey);
var
  Cmd: TNEDEditorCommand;
  Shift: TShiftState;
begin
  Shift := KeyDataToShiftState(Msg.KeyData);
  if Focused and (Msg.CharCode = VK_TAB) and (ssCtrl in Shift) then begin
    if ssShift in Shift then begin
      Cmd := FKeyboardMap.GetCommand(VK_TAB, [ssCtrl, ssShift]);
      if (Cmd <> Nil) and Cmd.InvokeCommand then
        Msg.Result := 1;
    end
    else begin
      Cmd := FKeyboardMap.GetCommand(VK_TAB, [ssCtrl]);
      if (Cmd <> Nil) and Cmd.InvokeCommand then
        Msg.Result := 1;
    end;
  end
  else
    inherited;
end;

procedure TNEDCustomEditorView.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  Canvas.Font := Font;
  if HandleAllocated then begin
    FTextWidth := Canvas.TextWidth(' ');
    FTextHeight := Canvas.TextHeight(' ');
    FEditorLines := GetEditorLines;
    FEditorChars := GetEditorChars;
  end;
end;

procedure TNEDCustomEditorView.CMMouseEnter(var Msg: TMessage);
begin
  ShowModernScrollBars;
  if FHorizontalScrollBar.IsScrollBarVisible then
    FHorizontalScrollBar.CMMouseEnter(Msg);
  if FVerticalScrollBar.IsScrollBarVisible then
    FVerticalScrollBar.CMMouseEnter(Msg);
  inherited;
//  CaptureItem := FMouseItem;
//  FMouseItem := Nil;
  Repaint;
end;

procedure TNEDCustomEditorView.CMMouseLeave(var Msg: TMessage);
//var
//  P: TPoint;
begin
//  GetCursorPos(P);
//  P := ScreenToClient(P);
//  if not PtInRect(ClientRect, P) then
  if FVerticalScrollBar.IsScrollBarVisible then
    FVerticalScrollBar.CMMouseLeave(Msg);
  if FHorizontalScrollBar.IsScrollBarVisible then
    FHorizontalScrollBar.CMMouseLeave(Msg);
  HideModernScrollBars;
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

procedure TNEDCustomEditorView.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  // sended by TWinControl.CNKeyDown procedure, after that message, if result is 0,
  // Delphi performs WM_GETDLGCODE, so we could decide here what we want the answear to be,
  // in Msg.CharCode we get VK_xxx of virtual key that triggered the event, for desired mask:
  //   VK_TAB: Mask := DLGC_WANTTAB;
  //   VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN: Mask := DLGC_WANTARROWS;
  //   VK_RETURN, VK_EXECUTE, VK_ESCAPE, VK_CANCEL: Mask := DLGC_WANTALLKEYS;

  if Focused and (Msg.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then begin
    FWindowSwitchQueryNext := True;
    Msg.Result := 0; // we want all special keys to be processed by parent form
  end;

//  if (Char(Msg.CharCode) = #13) then
//    Msg.Result := 1;
//  if (Msg.CharCode = VK_ESCAPE) and Modified then
//    Message.Result := 1;
end;

procedure TNEDCustomEditorView.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  if FWindowSwitchQueryNext then begin
    FWindowSwitchQueryNext := False;
    Msg.Result := 0; // we want all special keys to be processed by parent form
    Exit;
  end
  else
    Msg.Result := Msg.Result or DLGC_WANTARROWS or DLGC_WANTTAB or DLGC_WANTALLKEYS or DLGC_WANTCHARS;

//  Msg.Result := Msg.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
//  if fWantTabs then
//    Msg.Result := Msg.Result or DLGC_WANTTAB;
//  if fWantReturns then
//    Msg.Result := Msg.Result or DLGC_WANTALLKEYS;
end;

procedure TNEDCustomEditorView.WMSize(var Msg: TWMSize);
var
  SaveUpdatingScrollBars: Boolean;
begin
//  HideNativeScrollBars;
  inherited;
  FEditorLines := GetEditorLines;
  FEditorChars := GetEditorChars;
  //
  SaveUpdatingScrollBars := FUpdatingScrollBars;
  FUpdatingScrollBars := True;
  try
    CalcAutoRange;
  finally
    FUpdatingScrollBars := SaveUpdatingScrollBars;
  end;
  if FHorizontalScrollBar.Visible or FVerticalScrollBar.Visible then begin
    UpdateScrollBars;
    ShowModernScrollBars;
  end;
end;

procedure TNEDCustomEditorView.WMMouseWheel(var Msg: TWMMouseWheel);
var
  Delta, Lines: Integer;
  p: TPoint;
begin
  Delta := -Msg.WheelDelta div 120;
  Lines := Mouse.WheelScrollLines;

//  if Selected <> Nil then begin
//    if not IsRectEmpty(ButtonRect[fbScrollUp]) and (Delta = -1) then
//      Selected.TopIndex := Selected.TopIndex - 1
//    else if not IsRectEmpty(ButtonRect[fbScrollDown]) and (Delta = 1) then
//      Selected.TopIndex := Selected.TopIndex + 1;
//    if not MouseCapture then begin
//      p := ScreenToClient(Point(Message.XPos, Message.YPos));
//      CaptureItem := ItemFromPoint(p.X, p.Y);
//    end;
//  end;
  if Delta > 0 then
    SetTopIndex(FTopIndex + Lines)
  else
    SetTopIndex(FTopIndex - Lines);

  Msg.Result := 1;
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
  ShowModernScrollBars;
  inherited;
  FCaret.CaretCreate;
  Invalidate;
end;

procedure TNEDCustomEditorView.WMKillFocus(var Msg: TWMKillFocus);
begin
//  HideNativeScrollBars;
  HideModernScrollBars;
  inherited;
//  FMouseCapture := False;
//  FDownIndex := -1;
  Invalidate;
  // Added check for focused to prevent caret disappearing problem
  if Focused {or FAlwaysShowCaret} then
    Exit;
  FCaret.CaretHide;
  FCaret.CaretDestroy;
//  if FHideSelection and SelAvail then
//    InvalidateSelection;
end;

//procedure TNEDCustomEditorView.WMNCHitTest(var Msg: TWMNCHitTest);
//var
//  P: TPoint;
//  R: TRect;
//begin
//  inherited;
//
//  P := ScreenToClient(Point(Msg.XPos, Msg.YPos));
//  R := ClientRect;
//
//  // Right-side fake scrollbar
////  if (P.X >= R.Right - FVerticalScrollBar.Width) and not FVerticalScrollBar.Visible then begin
////    //Msg.Result := HTVSCROLL;
////    FVerticalScrollBar.ShowAnimated;
////  end;
//end;

procedure TNEDCustomEditorView.KeyDown(var Key: Word; Shift: TShiftState);
var
  Cmd: TNEDEditorCommand;
//  PressedKey: Char;
begin
  inherited;

  if (Key = 0) or (FDocument = Nil) then
    Exit;

  if Options.ReadOnly then begin
    Key := 0;
    Exit;
  end;

// VK_BACK // it can't be handled and disabled in this place, so we have to do it by the book, at KeyPress event

  try
    if FProcessNextKeyMap and FKeyboardMap.IsComplexCommand(Key, Shift) then begin
      FProcessNextKeyMap := False;
      FPreviousKeyMap.Key := Key;
      FPreviousKeyMap.Shift := Shift;
    end
    else begin
      Cmd := FKeyboardMap.GetCommand(FPreviousKeyMap.Key, FPreviousKeyMap.Shift, Key, Shift);
      if (Cmd <> Nil) and Cmd.InvokeCommand then
        Key := 0; // eat it
      FProcessNextKeyMap := True;
    end;
  finally
  end;

//  PressedKey := GetCharFromVirtualKey(Key);
//  if PressedKey <> '' then begin
////      CaretPos := FCaret.GetCaretPosition;
//    FDocument.Insert(CaretPos, PressedKey);
//  end;
//  PressedKey := #0;
end;

procedure TNEDCustomEditorView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;

  try
    //FIgnoreInput
  finally
    ReportEditorInfo;
  end;
end;

procedure TNEDCustomEditorView.KeyPress(var Key: Char);
var
  Cmd: TNEDEditorCommand;
  Shift: TShiftState;
  LCaretPos: TNEDCaretPosition;
  LTextPos: TNEDTextPosition;
begin
  inherited;
  if (Key = #0) or (FDocument = Nil) then
    Exit;

  if Options.ReadOnly then
    Exit;

  try
    LCaretPos := CaretPosition;
    LTextPos := CaretToLineColumn(LCaretPos);

    if Key = #8 then begin // backspace
      Shift := KeyboardStateToShiftState;
      Cmd := FKeyboardMap.GetCommand(VK_BACK, Shift);
      if (Cmd <> Nil) then
        Cmd.InvokeCommand;
      FProcessNextKeyMap := True;
      Key := #0;
      Exit;
    end;

    if FIgnoreInput then begin
      FIgnoreInput := False;
      Exit;
    end;

    if Key <> #0 then begin
      FProcessNextKeyMap := True;
      FCaret.CaretHide;
      FDocument.Insert(LTextPos, Key);
      FCaret.CaretShow;
      Key := #0;
    end;
  finally
    ReportEditorInfo;
  end;
end;

procedure TNEDCustomEditorView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  procedure SetCursorPositionFromMousePoint(const MousePoint: TPoint);
  var
    LCaretPos: TNEDCaretPosition;
    CaretX, CaretY, MaxPosX, MaxPosY: Integer;
  begin
    if PtInRect(GetLinesArea, MousePoint) then begin // lines area clicked
      CaretY := FTopIndex + Y div LineHeight;
      CaretX := FLeftIndex + (X - FGutter.GetWidth) div CharWidth;
      MaxPosX := FLeftIndex + FEditorChars;
      MaxPosY := FTopIndex + FEditorLines;
      if CaretX > MaxPosX then
        CaretX := MaxPosX;
      if CaretY > MaxPosY then
        CaretY := MaxPosY;
      LCaretPos := TNEDCaretPosition.Create(CaretX + 1, CaretY + 1);
      ActiveLineIndex := CaretToLineColumn(LCaretPos).Line;
      FCaret.CaretPosition := LCaretPos;
    end;
  end;

begin
  inherited;
  SetFocus;

  try
    if FVerticalScrollBar.IsScrollBarVisible then
      FVerticalScrollBar.MouseDown(Button, Shift, X, Y);
    if FHorizontalScrollBar.IsScrollBarVisible then
      FHorizontalScrollBar.MouseDown(Button, Shift, X, Y);
    //
    if (Button in [mbLeft, mbRight]) then begin
      if (Button = mbRight) and (epRightMouseMovesCursor in Options.EditorProperties) then begin // right mouse click - lines area clicked
        SetCursorPositionFromMousePoint(Point(X, Y));
      end
      else begin // left mouse click
        if X <= FGutter.GetWidth then begin // gutter clicked

        end
        else begin // lines area clicked
          SetCursorPositionFromMousePoint(Point(X, Y));
        end;
      end;
    end;
  finally
    ReportEditorInfo;
  end;
end;

procedure TNEDCustomEditorView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
//  HideNativeScrollBars;
  inherited;

  try
    if PtInRect(GetLinesArea, Point(X, Y)) and not MouseCapture then
      Cursor := crIBeam
    else
      Cursor := crDefault;
    //
    if FVerticalScrollBar.IsScrollBarVisible then
      FVerticalScrollBar.MouseMove(Shift, X, Y);
    if FHorizontalScrollBar.IsScrollBarVisible then
      FHorizontalScrollBar.MouseMove(Shift, X, Y);
    //

  finally
//    ReportEditorInfo;
  end;
end;

procedure TNEDCustomEditorView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  try
    if FVerticalScrollBar.IsScrollBarVisible then
      FVerticalScrollBar.MouseUp(Button, Shift, X, Y);
    if FHorizontalScrollBar.IsScrollBarVisible then
      FHorizontalScrollBar.MouseUp(Button, Shift, X, Y);
    //
    if PtInRect(GetLinesArea, Point(X, Y)) and not MouseCapture then
      Cursor := crIBeam
    else
      Cursor := crDefault;
    //

  finally
    ReportEditorInfo;
  end;
end;

//function TNEDCustomEditorView.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
//var
//  Delta: Integer;
//  p: TPoint;
//begin
//  Delta := -WheelDelta div 120;
////  if Selected <> Nil then begin
////    if not IsRectEmpty(ButtonRect[fbScrollUp]) and (Delta = -1) then
////      Selected.TopIndex := Selected.TopIndex - 1
////    else if not IsRectEmpty(ButtonRect[fbScrollDown]) and (Delta = 1) then
////      Selected.TopIndex := Selected.TopIndex + 1;
////    if not MouseCapture then begin
////      p := ScreenToClient(Point(Message.XPos, Message.YPos));
////      CaptureItem := ItemFromPoint(p.X, p.Y);
////    end;
////  end;
//  if delta > 0 then ;
////    SetTopIndex(FTopIndex + 1)
////  else
////    SetTopIndex(FTopIndex - 1);
//  inherited;
//end;

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
  LinesR: TRect;
begin
  LinesR := GetLinesArea;
  DrawBackground(Canvas, LinesR); // clear background
  if (MouseInClient or Focused) and FVerticalScrollBar.Visible and FVerticalScrollBar.NeedsScrollBarVisible then begin
    FVerticalScrollBar.Paint(Canvas);
  end;
  if (MouseInClient or Focused) and FHorizontalScrollBar.Visible and FHorizontalScrollBar.NeedsScrollBarVisible then begin
    FHorizontalScrollBar.Paint(Canvas);
  end;
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
  Row, R: TRect;
  Y, I: Integer;
  LineProperties: TNEDLineProperties;
  CurrentLineHeight: Integer;
begin
  ClipR := ACanvas.ClipRect;
  I := 0;
  Y := 0; // render height accumulator
  while Y < ARect.Height do begin // divide client-area into horizontal strips and render Lines
    // ARect.Height div (LineHeight)
    if I + FTopIndex > VisibleLinesCount - 1 then
      Break;

    // set strip dimmensions
    LineProperties := Document.Lines[I + FTopIndex];
    if not LineProperties.IsVisible and not (epShowNonVisibleLines in Options.EditorProperties) then begin
      Inc(I);
      Continue;
    end;

    CurrentLineHeight := LineProperties.GetLineHeight(LineHeight);
    Row := ARect;
    //Row.Height := LineHeight;
//  if FScrollWidth > 0 then
//    Row.Width := FScrollWidth;

    R := Row;
    R.Top := R.Top + Y;
    R.Height := CurrentLineHeight;
    if R.Bottom <= ClipR.Top then begin // if we are ouside of cliping-area, go to next entry
      Inc(I);
      Inc(Y, CurrentLineHeight);
      //
      Continue;
    end;
    if R.Top >= ClipR.Bottom then begin // if we are ouside of cliping-area, go to next entry
      Inc(I);
      Inc(Y, CurrentLineHeight);
      //
      Continue;
    end;
    //
    DrawLine(ACanvas, R, FTopIndex + I);
    //
    Inc(I);
    Inc(Y, CurrentLineHeight);
  end;
end;

procedure TNEDCustomEditorView.DrawLine(const ACanvas: TCanvas; const ARect: TRect; const LineIdx: Integer);
var
  is_focused, is_selected: Boolean;
  line_bg_color: TColor;
  LineProperties: TNEDLineProperties;
  LineText: String;
  LineRect: TRect;
begin
  // set render state
  is_focused := Focused and (LineIdx = ActiveLineIndex); // draw focused only Line that is active
  is_selected := LineSelected(LineIdx);
  line_bg_color := Color;
  if is_focused then
    line_bg_color := FColors.LineFocusedColor;
  if is_selected then
    line_bg_color := FColors.LineSelectedColor;

  LineProperties := Document.Lines[LineIdx];
  if LineProperties.Deleted and (epShowNonVisibleLines in Options.EditorProperties) then begin
    line_bg_color := clRed; // FColors.;
    ACanvas.Brush.Color := line_bg_color;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.FillRect(ARect);
  end;

  LineText := Document.GetLineText(LineIdx);
  LineRect := ARect;

  ACanvas.Brush.Style := bsClear;
  ACanvas.Brush.Color := line_bg_color;
  if is_focused and not is_selected then begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.FillRect(ARect);
  end;

  ACanvas.Font.Assign(Self.Font);
  ACanvas.TextRect(LineRect, LineText, [tfLeft, tfTop, tfSingleLine]);
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

  if DocumentObserver <> Nil then begin
    FObserver.OnDocumentChanged := DocumentChanged;
    FObserver.OnLineInserted    := LineInserted;
    FObserver.OnLineDeleted     := LineDeleted;
    FObserver.OnLineChanged     := LineChanged;
  end;
end;

procedure TNEDCustomEditorView.SetTopIndex(Value: Integer);
var
  Delta: Integer;
  P: TPoint;
  count: Integer;
begin
  count := VisibleLinesCount;
  if Value > count - FEditorLines then
    Value := count - FEditorLines;
  if Value < 0 then
    Value := 0;
  if Value <> FTopIndex then begin
    FCaret.CaretHide;
    //Delta := (FTopIndex - Value) * LineHeight;
    Delta := FTopIndex - Value;
    FTopIndex := Value;
//    if FHotTrack then
//      if FHotIndex > - 1 then
//        InvalidateItem(FHotIndex);
//    FHotIndex := -1;
    FVerticalScrollBar.Position := FTopIndex;
//    FCaret.CaretScroll(Delta, 0);
    FCaret.CaretUpdate;
//    InvalidateItem(FItemIndex);
//    if FHotTrack then begin
//      P := ScreenToClient(Mouse.CursorPos);
//      FHotIndex := ItemAtPos(P, False);
//      if FHotIndex > -1 then
//        if PtInRect(ItemRect(FHotIndex), P) then
//          InvalidateItem(FHotIndex)
//        else
//          FHotIndex := -1;
//    end;
//    if FVerticalScrollBar.Visible then
//      FVerticalScrollBar.Invalidate;
  end;
end;

procedure TNEDCustomEditorView.SetLeftIndex(Value: Integer);
var
  Delta: Integer;
  P: TPoint;
  count: Integer;
begin
  count := LongestLineCharsCount;
  if Value > count - FEditorChars then
    Value := count - FEditorChars;
  if Value < 0 then
    Value := 0;
  if Value <> FLeftIndex then begin
    Delta := (FLeftIndex - Value) * CharWidth;
    FLeftIndex := Value;
//    if FHotTrack then
//      if FHotIndex > - 1 then
//        InvalidateItem(FHotIndex);
//    FHotIndex := -1;
    FHorizontalScrollBar.Position := FLeftIndex;
//    Scroll(Delta);
//    InvalidateItem(FItemIndex);
//    if FHotTrack then begin
//      P := ScreenToClient(Mouse.CursorPos);
//      FHotIndex := ItemAtPos(P, False);
//      if FHotIndex > -1 then
//        if PtInRect(ItemRect(FHotIndex), P) then
//          InvalidateItem(FHotIndex)
//        else
//          FHotIndex := -1;
//    end;
//    if FVerticalScrollBar.Visible then
//      FVerticalScrollBar.Invalidate;
  end;
end;

procedure TNEDCustomEditorView.SetActiveLineIndex(Value: Integer);
var
  PriorIndex: Integer;
begin
  if FActiveLineIndex <> Value then begin
    if Value < 0 then
      Value := 0;
    if Value >= FDocument.LinesCount then
      Value := FDocument.LinesCount - 1;
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

procedure TNEDCustomEditorView.CalcAutoRange;
begin
  if FAutoRangeCount <= 0 then begin
    HorizontalScrollBar.CalcAutoRange;
    VerticalScrollBar.CalcAutoRange;
  end;
end;

procedure TNEDCustomEditorView.HideNativeScrollBars;
begin
//  if HandleAllocated then
//    FlatSB_ShowScrollBar(Handle, SB_BOTH, False);
end;

procedure TNEDCustomEditorView.UpdateScrollBars;
begin
  if not FUpdatingScrollBars and HandleAllocated then try
    FUpdatingScrollBars := True;
    if FVerticalScrollBar.NeedsScrollBarVisible then begin
      FHorizontalScrollBar.Update(False, True);
      FVerticalScrollBar.Update(True, False);
    end
    else if FHorizontalScrollBar.NeedsScrollBarVisible then begin
      FVerticalScrollBar.Update(False, True);
      FHorizontalScrollBar.Update(True, False);
    end
    else begin
      FVerticalScrollBar.Update(False, False);
      FHorizontalScrollBar.Update(True, False);
    end;
    FEditorLines := GetEditorLines;
    FEditorChars := GetEditorChars;
  finally
    FUpdatingScrollBars := False;
  end;
end;

procedure TNEDCustomEditorView.ShowModernScrollBars;
begin
  if FVerticalScrollBar.Visible and FVerticalScrollBar.NeedsScrollBarVisible then
    FVerticalScrollBar.ShowAnimated;
  if FHorizontalScrollBar.Visible and FHorizontalScrollBar.NeedsScrollBarVisible then
    FHorizontalScrollBar.ShowAnimated;
end;

procedure TNEDCustomEditorView.HideModernScrollBars;
begin
  FVerticalScrollBar.HideAnimated;
end;

function TNEDCustomEditorView.GetGutterArea: TRect;
begin
  Result := TRect.Empty;
  if not FGutter.Visible then
    Exit;

  Result := Rect(0, 0, FGutter.GetWidth, Self.Height);
  if FHorizontalScrollBar.Visible and FHorizontalScrollBar.NeedsScrollBarVisible then
    Result.Bottom := Result.Bottom - FHorizontalScrollBar.Size;
end;

function TNEDCustomEditorView.GetLinesArea: TRect;
var
  Gutter: TRect;
begin
  Gutter := GetGutterArea;
  Result := Rect(Gutter.Width, 0, Self.Width, Self.Height);
  if FVerticalScrollBar.Visible and FVerticalScrollBar.NeedsScrollBarVisible then
    Result.Right := Result.Right - FVerticalScrollBar.Size;
  if FHorizontalScrollBar.Visible and FHorizontalScrollBar.NeedsScrollBarVisible then
    Result.Bottom := Result.Bottom - FHorizontalScrollBar.Size;
end;

function TNEDCustomEditorView.GetMinimapArea: TRect;
begin
  // @TODO
end;

function TNEDCustomEditorView.GetCharWidth: Integer;
begin
  if FTextWidth = -1 then begin
    FTextWidth := Canvas.TextWidth(' ');
    FEditorChars := GetEditorChars;
  end;

  Result := FTextWidth;
end;

function TNEDCustomEditorView.GetLineHeight: Integer;
begin
  if FTextHeight = -1 then begin
    FTextHeight := Canvas.TextHeight(' ');
    FEditorLines := GetEditorLines;
  end;

  Result := FTextHeight;
end;

function TNEDCustomEditorView.GetEditorLines: Integer;
begin
  Result := Max((GetLinesArea.Height div LineHeight) - 1, 0); // in lines
end;

function TNEDCustomEditorView.GetEditorChars: Integer;
begin
  Result := Max((GetLinesArea.Width div CharWidth) - 1, 0); // in characters
end;

procedure TNEDCustomEditorView.InvalidateLine(const LineIndex: Integer);
begin
  // @TODO
end;

procedure TNEDCustomEditorView.ScrollBy(DeltaX, DeltaY: Integer);
begin
  if (FLeftIndex > 0) and (DeltaX < 0) then
    SetLeftIndex(FLeftIndex - 1)
  else if (FLeftIndex < LongestLineCharsCount) and (DeltaX > 0) then
    SetLeftIndex(FLeftIndex + 1);
  //
  if (FTopIndex > 0) and (DeltaY < 0) then
    SetTopIndex(FTopIndex - 1)
  else if (FTopIndex < VisibleLinesCount) and (DeltaY > 0) then
    SetTopIndex(FTopIndex + 1);
end;

procedure TNEDCustomEditorView.ScrollLineIntoView(const ALine: Integer);
begin
  if ALine < FTopIndex then
    SetTopIndex(ALine)
  else if ALine > FTopIndex + FEditorLines then
    SetTopIndex(FTopIndex + (ALine - FTopIndex - FEditorLines));
end;

procedure TNEDCustomEditorView.ScrollToSelection;
var
  LTopOffset: Integer;
begin
  LTopOffset := FActiveLineIndex - FTopIndex; // how far from top of the view in lines
  if (LTopOffset < 0) or (LTopOffset > FEditorLines) then
    SetTopIndex(FTopIndex + LTopOffset);
end;

function TNEDCustomEditorView.LineSelected(const LineIdx: Integer): Boolean;
begin
  // @TODO
end;

function TNEDCustomEditorView.LineColumnInView(const LineColumn: TNEDTextPosition): Boolean;
begin
  Result := InRange(LineColumn.Line, TopIndex, TopIndex + ClientHeight div FTextHeight) and
            InRange(LineColumnToPixels(LineColumn).X, FGutter.GetWidth + Options.TextMargin, ClientWidth - Options.TextMargin);
end;

function TNEDCustomEditorView.LineColumnToPixels(const LineColumn: TNEDTextPosition): TPoint;
var
  LineLen: Integer;
  S: String;
begin
  Result.Y := (LineColumn.Line - TopIndex) * LineHeight;

  LineLen := Document.Lines[LineColumn.Line].Length;
  S := Document.GetLineText(LineColumn.Line);

  if LineColumn.Column = 0 then
    Result.X := 0
  else if LineLen = 0 then
    Result.X := (LineColumn.Column) * CharWidth
  else if LineColumn.Column > LineLen then
     Result.X := TextWidth(S) + (LineColumn.Column - LineLen) * CharWidth
  else
    Result.X := ColumnToPixels(S, LineColumn.Column);
  Inc(Result.X, FGutter.GetWidth + Options.TextMargin);
end;

function TNEDCustomEditorView.ColumnToPixels(const S: String; Col: Integer): Integer;
var
  P: Char;
  TabSize, TabSizeAdd, Char_Width: Integer;
  I, SLen: Integer;
begin
  Result := 0;

  Char_Width := CharWidth;
  if epTabsToSpaces in Options.EditorProperties then
    TabSize := Options.TabWidthToSpaces * Char_Width
  else
    TabSize := Options.TabWidth * Char_Width;

  I := 1;
  SLen := S.Length;
  while (I < SLen) and (I < Col) do begin
    P := S[I];
    case P of
      #9: begin
        TabSizeAdd := Result mod TabSize;
        Inc(Result, TabSize - TabSizeAdd);
      end;
      #32..#126: Inc(Result, Char_Width);
    else
      begin
        Inc(Result, Canvas.TextWidth(P));
      end;
    end;
    Inc(I);
  end;
end;

function TNEDCustomEditorView.TextWidth(const S: String): Integer;
var
  P: Char;
  TabSize, TabSizeAdd, Char_Width: Integer;
  I, SLen: Integer;
begin
  Result := 0;

  Char_Width := CharWidth;
  if epTabsToSpaces in Options.EditorProperties then
    TabSize := Options.TabWidthToSpaces * Char_Width
  else
    TabSize := Options.TabWidth * Char_Width;

  I := 1;
  SLen := S.Length;
  while I < SLen do begin
    P := S[I];
    case P of
      #9: begin
        TabSizeAdd := Result mod TabSize;
        Inc(Result, TabSize - TabSizeAdd);
      end;
      #32..#126: Inc(Result, Char_Width);
    else
      begin
        Inc(Result, Canvas.TextWidth(P));
      end;
    end;
    Inc(I);
  end;
end;

function TNEDCustomEditorView.CaretToLineColumn(const CaretPos: TNEDCaretPosition): TNEDTextPosition;
var
  CaretX, CaretY: Integer;
  Line, Column, TopOffset: Integer;
  LineProp: TNEDLineProperties;
  Ascending: Boolean;
begin
  Result := TNEDTextPosition.LineColumn(0, 0);

  CaretX := CaretPos.X - 1;
  CaretY := CaretPos.Y - 1;

  Line := TopIndex;
  Column := 0;
  TopOffset := 0;

  Ascending := CaretY > Line;

  while (Ascending and (Line < FVisibleLinesCount)) or (not Ascending and (Line >= 0)) do begin
    LineProp := Document.Lines[Line];
    if not LineProp.IsVisible and not (epShowNonVisibleLines in Options.EditorProperties) then begin
      if Ascending then begin
        Inc(Line);
        Inc(TopOffset);
      end
      else begin
        Dec(Line);
        Dec(TopOffset);
      end;
      Continue;
    end;

    if Line - TopOffset = CaretY then begin
      Column := CaretX;
      Result := TNEDTextPosition.LineColumn(Line, Column);
      Break;
    end;
    //
    if Ascending then
      Inc(Line)
    else
      Dec(Line);
  end;
end;

function TNEDCustomEditorView.CaretToPixels(const CaretPos: TNEDCaretPosition): TPoint;
var
  LineIdx, LineLen: Integer;
  S: String;
begin
  Result.Y := (CaretPos.Y - 1 - TopIndex) * LineHeight;
  LineIdx := CaretToLineColumn(CaretPos).Line;
  LineLen := Document.Lines[LineIdx].Length;
  S := Document.GetLineText(LineIdx);

  if CaretPos.X = 1 then
    Result.X := 0
  else if LineLen = 0 then
    Result.X := (CaretPos.X - 1) * CharWidth
  else if CaretPos.X > LineLen then
     //Result.X := TextWidth(S) + (CaretPos.X - 1 - LineLen) * CharWidth
     Result.X := (CaretPos.X - 1) * CharWidth
  else
    Result.X := ColumnToPixels(S, CaretPos.X);
  Inc(Result.X, FGutter.GetWidth + Options.TextMargin);
end;

function TNEDCustomEditorView.LineColumnToCaret(const LineColumn: TNEDTextPosition): TNEDCaretPosition;
begin
  Result := LineColumnToCaret(LineColumn.Line, LineColumn.Column);
end;

//  Result.Y := Line - TopIndex + 1;
//  Result.X := Column + 1;
function TNEDCustomEditorView.LineColumnToCaret(const Line, Column: Integer): TNEDCaretPosition;
var
  DocLine: Integer;
  HiddenCount: Integer;
  LineProp: TNEDLineProperties;
begin
  // top-left position of a lines view begins at line:1, column:1
  Result := TNEDCaretPosition.Create(1, 1);

  // desired caret X/Y position must be calculated in that way to take into account the deleted lines in our document

  HiddenCount := 0;

  if not (epShowNonVisibleLines in Options.EditorProperties) then begin
    for DocLine := 0 to Line - 1 do begin
      LineProp := Document.Lines[DocLine];
      if not LineProp.IsVisible then
        Inc(HiddenCount);
    end;
  end;

  Result := TNEDCaretPosition.Create(Column + 1, Line - HiddenCount + 1);
end;

procedure TNEDCustomEditorView.DocumentChanged(const Change: TNEDDocumentChangeInfo);
var
  LCaretPos: TNEDCaretPosition;
begin
  // @TODO - ??? do something ???
  FVisibleLinesCount := Document.VisibleLinesCount;
  if Change.Kind = dcInsert then begin
    FCaret.CaretMove(0, Change.InsertedLength);
    FCaret.CaretUpdate(False);
//    LCaretPos := LineColumnToCaret(Change.Position);
//    if (CaretPos.X <> LCaretPos.X) or (CaretPos.Y <> LCaretPos.Y) then begin
//      FCaret.CaretSetLocation(LCaretPos);
//      FCaret.CaretUpdate(False);
//    end;
  end
  else if Change.Kind = dcDelete then begin
    if Change.Operation = opDeleteBKSP then begin
      LCaretPos := CaretPosition;
      if LCaretPos.X - Change.DeletedLength = 0 then
        FCaret.CaretMove(-1, 0)
      else
        FCaret.CaretMove(0, -Change.DeletedLength);
      FCaret.CaretUpdate(False);
    end;
  end
  else if Change.Kind = dcLineInsert then begin
    LCaretPos := LineColumnToCaret(Change.Position);
    if (CaretPosition.X <> LCaretPos.X) or (CaretPosition.Y <> LCaretPos.Y) then begin
      FCaret.CaretSetLocation(LCaretPos);
      FCaret.CaretUpdate(False);
    end;
  end
  else if Change.Kind = dcLineDelete then begin
    LCaretPos := LineColumnToCaret(Change.Position);
    if (CaretPosition.X <> LCaretPos.X) or (CaretPosition.Y <> LCaretPos.Y) then begin
      FCaret.CaretSetLocation(LCaretPos);
      FCaret.CaretUpdate(False);
    end;
  end
  else if Change.Kind = dcLineChanged then begin
    LCaretPos := LineColumnToCaret(Change.Position);
    if (CaretPosition.X <> LCaretPos.X) or (CaretPosition.Y <> LCaretPos.Y) then begin
      FCaret.CaretSetLocation(LCaretPos);
      FCaret.CaretUpdate(False);
    end;
  end;
  ActiveLineIndex := CaretToLineColumn(CaretPosition).Line;

  // and than
  CalcAutoRange;
//  HideNativeScrollBars;
  UpdateScrollBars;
  ShowModernScrollBars;
  Invalidate;

  ReportEditorInfo;
end;

procedure TNEDCustomEditorView.LineInserted(const Change: TNEDDocumentChangeInfo);
begin
  DocumentChanged(Change);
end;

procedure TNEDCustomEditorView.LineDeleted(const Change: TNEDDocumentChangeInfo);
begin
  DocumentChanged(Change);
end;

procedure TNEDCustomEditorView.LineChanged(const Change: TNEDDocumentChangeInfo);
begin
  DocumentChanged(Change);
end;

procedure TNEDCustomEditorView.VerticalScroll(Sender: TObject; const Action: TNEDCustomScrollBarActionEnum; const Position: Integer);
begin
//    saLineUp,
//    saLineDown,
//    saPageUp,
//    saPageDown,
//    saThumbTrack,
//    saThumbPosition
  FTopIndex := Position;
  Invalidate;
end;

procedure TNEDCustomEditorView.HorizontalScroll(Sender: TObject; const Action: TNEDCustomScrollBarActionEnum; const Position: Integer);
begin
//  FTopIndex := Position;
//  Invalidate;
end;

end.

