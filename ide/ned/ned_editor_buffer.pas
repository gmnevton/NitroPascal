//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-06
// All rights reserved.
//

unit ned_editor_buffer;

//  Core idea
//
//  1. Back-buffer / Document (model)
//    Owns the text
//    Applies edits
//    Maintains undo/redo
//    Emits change notifications
//
//  2. Controller (optional)
//    Translates user input -> document operations
//    Keeps UI logic out of the model
//
//  3. View (visual editor)
//    Can exist multiple times per document
//    Subscribes to document changes
//    Renders text
//    Handles caret, selection

//-----
//  1. Back-buffer (shared document)
//    This is the heart of system
//
//    Key requirements:
//      No UI code
//      Thread-safe (optional)
//      Change notifications
//      Efficient storage (not just a string)
//
//  2. View (visual editor)
//    Each editor instance:
//      references the SAME text document
//      has its own:
//        caret position
//        selection
//        scroll state

interface

uses
  SysUtils,
  Classes,
  Graphics,
  Generics.Collections;

type
  TNEDPieceBufferTypeEnum = (
    pbOriginal,
    pbInputBuffer
  );

  TNEDPieceOrigin = packed record
    Line: Integer;
    Column: Integer;
  end;
  PNEDPieceOrigin = ^TNEDPieceOrigin;

  TNEDPiece = packed record
  public
    // source buffer
    Buffer: TNEDPieceBufferTypeEnum;
    // position inside source buffer
    Offset: Integer;
    Length: Integer;
    // origin tracking
    Origin: PNEDPieceOrigin;
  public
    procedure CreateOrigin(const Line, Column: Integer);
  end;
  PNEDPiece = ^TNEDPiece;

  TNEDPieces = class
  private
    FList: TList<PNEDPiece>;
    FLineNo: Integer;
    //
    procedure SetLineNo(const Value: Integer);
    //
    function GetCount: Integer;
    function GetPiece(Index: Integer): PNEDPiece;
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Clear;
    //
    procedure Add(const Piece: PNEDPiece);
    procedure Insert(Index: Integer; const Piece: PNEDPiece);
    procedure Delete(Index: Integer);
    //
    function TotalLength: Integer;
    //
    function FindPiece(Position: Integer; out PieceIndex: Integer; out PieceOffset: Integer): Boolean;
    //
    procedure SplitPiece(PieceIndex: Integer; SplitOffset: Integer; const Trim: Boolean);
    procedure InsertPiece(Position: Integer; const Piece: PNEDPiece);
    procedure DeleteRange(Position: Integer; Count: Integer; const Trim: Boolean);
    //
    procedure Coalesce;
    //
    property Count: Integer read GetCount;
    property LineNo: Integer read FLineNo write SetLineNo;
    //
    property Pieces[Index: Integer]: PNEDPiece read GetPiece; default;
  end;

  // line state flags
  TNEDLineFlagEnum = (
    lfDeleted,       // line logically removed from view
    lfHidden,        // hidden by folding
    lfSpacer,        // artificial wrapped/spacer line
    //
    lfModified,      // modified since last save
    lfSaved,         // saved after modification
    //
    lfInserted,      // line inserted
    lfMerged,        // line merged
    lfSplit,         // line splited
    //
    lfBookmark,      // user bookmark
    lfBreakpoint,    // debugger breakpoint
    lfFolded,        // line starts collapsed block
    //
    lfDirty,         // requires layout recalculation
    lfSyntaxDirty    // requires syntax reparse
  );

  TNEDLineFlags = set of TNEDLineFlagEnum;

  TNEDLineProperties = class
  private
    FPieces: TNEDPieces;
    //FTokens: TList<TNEDTextToken>;
    FFlags: TNEDLineFlags;    // line state
    FLevel: Integer;          // indentation level determined by parser
    FLength: Integer;         // character count in line; cached value
    FLineNo: Integer;         // physical document line number
    FOriginalLineNo: Integer; // original line number from loaded file
    FLineId: Integer;         // stable identity, never changes even when line moves
    FParentLineId: Integer;   // set while splits are made
    FLineHeight: Integer;     // custom line height
    FStartPos: Integer;       // starting document position
    FWrapCount: Integer;      // visual line count after wrapping
    FFoldLevel: Integer;      // fold support
    FFoldParent: Integer;
    FFoldChildren: Integer;
    FTokenStart: Integer;     // syntax support
    FTokenCount: Integer;
    FTag: NativeInt;          // user data
  protected
    function GetDeleted: Boolean;
    function GetHidden: Boolean;
    function GetModified: Boolean;
    function GetSaved: Boolean;
    function GetSpacer: Boolean;
    procedure SetDeleted(const Value: Boolean);
    procedure SetHidden(const Value: Boolean);
    procedure SetModified(const Value: Boolean);
    procedure SetSaved(const Value: Boolean);
    procedure SetSpacer(const Value: Boolean);
    procedure SetLineNo(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Clear;
    procedure UpdateLength;         // recalculate cached length from pieces
    //
    function IsEmpty: Boolean;      // returns true when line contains no text
    function IsVisible: Boolean;    // returns true when line is visible
    function IsRenderable: Boolean; // returns true when line participates in layout
    function GetLineHeight(const DefaultLineHeight: Integer): Integer;
    //
    procedure AddPiece(const Piece: PNEDPiece); // piece manipulation helpers
    procedure InsertPiece(Position: Integer; const Piece: PNEDPiece);
    procedure DeleteRange(Position: Integer; Count: Integer; const Trim: Boolean);
    //
    // accessors
    property Pieces: TNEDPieces read FPieces;
    property Deleted: Boolean read GetDeleted write SetDeleted;
    property Hidden: Boolean read GetHidden write SetHidden;
    property Modified: Boolean read GetModified write SetModified;
    property Saved: Boolean read GetSaved write SetSaved;
    property Spacer: Boolean read GetSpacer write SetSpacer;
    property Flags: TNEDLineFlags read FFlags write FFlags;
    property Level: Integer read FLevel write FLevel;
    property Length: Integer read FLength;
    property LineNo: Integer read FLineNo write SetLineNo;
    property OriginalLineNo: Integer read FOriginalLineNo;
    property LineId: Integer read FLineId;
    property ParentLineId: Integer read FParentLineId;
    property StartPos: Integer read FStartPos write FStartPos;
    property WrapCount: Integer read FWrapCount write FWrapCount;
    property FoldLevel: Integer read FFoldLevel write FFoldLevel;
    property FoldParent: Integer read FFoldParent write FFoldParent;
    property FoldChildren: Integer read FFoldChildren write FFoldChildren;
    property TokenStart: Integer read FTokenStart write FTokenStart;
    property TokenCount: Integer read FTokenCount write FTokenCount;
    property Tag: NativeInt read FTag write FTag;
  end;

  TNEDTextPosition = record
  public
    Line: Integer;
    Column: Integer;
    //
    class function LineColumn(const ALine, AColumn: Integer): TNEDTextPosition; static;
  end;
  PNEDTextPosition = TNEDTextPosition;

  TNEDTextRange = record
    StartPos: TNEDTextPosition;
    EndPos: TNEDTextPosition;
  end;

  TNEDTextSelection = record
    StartPos: TNEDTextPosition;
    EndPos: TNEDTextPosition;
    CaretPos: TNEDTextPosition;
  end;

  TNEDEditOperationKindEnum = (
    opNone,
    opInsert,
    opDeleteDEL,
    opDeleteBKSP,
    opReplace
  );

  TNEDEditOperation = record
    Kind: TNEDEditOperationKindEnum;
    Position: TNEDTextPosition;
    Text: String;
    Length: Integer;
    // affected line range
    StartLine: Integer;
    EndLine: Integer;
    // document version
    Version: Integer;
  end;

  TNEDTextTokenKindEnum = (
    ttkKeyword,
    ttkIdentifier,
    ttkString,
    ttkNumber,
    ttkComment,
    ttkOperator,
    ttkType,
    ttkFunction,
    ttkClass
  );

  TNEDTokenFormat = record
    Font: TFont;
    TextAlignment: TVerticalAlignment; // vtop, vcenter, vbottom
    WordBreak: Boolean; // this character token is a line break
    WordWrap: Boolean; // internal; wrap line in this place while rendering
    LineBreak: String; // oryginal line break
    LineBreakType: TTextLineBreakStyle;
    LetterSpacing: Integer;
    WordSpacing: Integer;
    TextIndentation: Integer;
  end;

  TNEDTextToken = record
    Kind: TNEDTextTokenKindEnum;
    Offset: Integer;
    Length: Integer;
    Format: TNEDTokenFormat;
  end;

{ TNEDStringList }

  TNEDStringList = class(TStringList)
  protected
    function GetObject(Index: Integer): TNEDLineProperties; reintroduce;
    procedure PutObject(Index: Integer; AObject: TNEDLineProperties); reintroduce;
    procedure InsertItem(Index: Integer; const S: String; AObject: TNEDLineProperties); reintroduce;
  public
    function AddPair(const Name, Value: string; AObject: TNEDLineProperties): TStrings; reintroduce;
    function AddObject(const S: String; AObject: TNEDLineProperties): Integer; reintroduce;
    procedure AddStrings(const Strings: TArray<string>; const Objects: TArray<TNEDLineProperties>); reintroduce;
    function IndexOfObject(AObject: TNEDLineProperties): Integer; reintroduce;
    procedure InsertObject(Index: Integer; const S: String; AObject: TNEDLineProperties); reintroduce;
    //
    property Objects[Index: Integer]: TNEDLineProperties read GetObject write PutObject;
  end;

  // type of document modification
  TNEDDocumentChangeKindEnum = (
    dcUnknown,
    dcInsert,
    dcDelete,
    dcReplace,
    dcLineInsert,
    dcLineDelete,
    dcLineChanged,
    dcLoad,
    dcReload,
    dcReset,
    dcDocumentProperties
  );

  // detailed change notification
  TNEDDocumentChangeInfo = record
    Kind: TNEDDocumentChangeKindEnum;
    Operation: TNEDEditOperationKindEnum;

    // character positions
    Position: TNEDTextPosition;
    DeletedLength: Integer;
    InsertedLength: Integer;

    // affected line range
    StartLine: Integer;
    EndLine: Integer;

    // optional sender
    Sender: TObject;
  end;

  TNEDCustomDocument = class;

{ TNEDDocumentObserver }

  TNEDDocumentChangedEvent = procedure (const Change: TNEDDocumentChangeInfo) of object;

  // base observer class
  // editor views, syntax engines, minimaps, code explorers and parsers can derive from it
  TNEDDocumentObserver = class
  private
    FDocument: TNEDCustomDocument;
    FEnabled: Boolean;
    FUpdateLock: Integer;
    FOnDocumentChanged: TNEDDocumentChangedEvent;
    FOnLineInserted: TNEDDocumentChangedEvent;
    FOnLineDeleted: TNEDDocumentChangedEvent;
    FOnLineChanged: TNEDDocumentChangedEvent;
  protected
    // character-level change
    procedure DoDocumentChanged(const Change: TNEDDocumentChangeInfo); virtual;
    // line-level notifications
    procedure DoLineInserted(LineIndex: Integer; AffectedLineStart, AffectedLineEnd: Integer); virtual;
    procedure DoLineDeleted(const Operation: TNEDEditOperationKindEnum; LineIndex: Integer; AffectedLineStart, AffectedLineEnd: Integer); virtual;
    procedure DoLineChanged(const Operation: TNEDEditOperationKindEnum; LineIndex: Integer; AffectedLineStart, AffectedLineEnd: Integer); virtual;
    // full document operations
    procedure DoDocumentReloaded; virtual;
    procedure DoDocumentReset; virtual;
  public
    constructor Create(const ADocument: TNEDCustomDocument); reintroduce;
    destructor Destroy; override;
    // multiple changes can be grouped between BeginUpdate / EndUpdate
    procedure BeginUpdate;
    procedure EndUpdate;
    //
    function IsUpdating: Boolean;
    //
    // main notification entry point
    procedure DocumentChanged(const Position: Integer; const Kind: TNEDDocumentChangeKindEnum; const Operation: TNEDEditOperationKindEnum; DeletedLength: Integer; InsertedLength: Integer); overload; virtual;
    procedure DocumentChanged(const LineColumn: TNEDTextPosition; const Kind: TNEDDocumentChangeKindEnum; const Operation: TNEDEditOperationKindEnum; DeletedLength: Integer; InsertedLength: Integer); overload; virtual;
    procedure DocumentChanged(const Change: TNEDDocumentChangeInfo); overload; virtual;
    //
    // line notifications
    procedure LineInserted(LineIndex: Integer; const NewLine: Boolean); virtual;
    procedure LineDeleted(const Operation: TNEDEditOperationKindEnum; LineIndex: Integer); virtual;
    procedure LineChanged(const Operation: TNEDEditOperationKindEnum; LineIndex: Integer); virtual;
    //
    // full document notifications
    procedure DocumentReloaded; virtual;
    procedure DocumentReset; virtual;
    //
    property Enabled: Boolean read FEnabled write FEnabled;
    property OnDocumentChanged: TNEDDocumentChangedEvent read FOnDocumentChanged write FOnDocumentChanged;
    property OnLineInserted: TNEDDocumentChangedEvent read FOnLineInserted write FOnLineInserted;
    property OnLineDeleted: TNEDDocumentChangedEvent read FOnLineDeleted write FOnLineDeleted;
    property OnLineChanged: TNEDDocumentChangedEvent read FOnLineChanged write FOnLineChanged;
  end;

{ TNEDCustomDocument }

  TNEDCustomDocument = class
  private
    FFilePath: String;
    FFileCreateDateTime: TDateTime;
    FFileModifyDateTime: TDateTime;
    FFileSize: UInt64;
    FFileReadOnly: Boolean;
    FFileBOM: Boolean;
    //
    FEncoding: TEncoding;
    FEncodingWriteBOM: Boolean;
    FLines: TNEDStringList; // logical lines
    FInputBuffer: String; // piece table add buffer
    FObservers: TList<TNEDDocumentObserver>; // attached views/services.
    // Undo / Redo
    FVersion: Integer;
    FUndo: TStack<TNEDEditOperation>;
    FRedo: TStack<TNEDEditOperation>;
    FInUndoRedo: Boolean;
    //
    FEmptyLineCreate: Boolean;
    FLineBreak: String;
    FLineBreakLen: Integer;
    FNextLineId: Integer;
    // modification state
    FModified: Boolean;
    FUpdateCount: Integer; // update lock
    FLength: Integer; // cached document length
    FLineCount: Integer; // cached line count
    FVisibleLineCount: Integer; // cached line count
  private
    procedure SetFileReadOnly(const Value: Boolean);
    procedure SetEncoding(const Value: TEncoding);
    procedure SetEncodingWriteBOM(const Value: Boolean);
    procedure SetLineBreak(const Value: String);
  protected
    procedure NotifyObservers(Position: Integer; const Kind: TNEDDocumentChangeKindEnum; const Operation: TNEDEditOperationKindEnum; DeletedLen: Integer; InsertedLen: Integer); overload;
    procedure NotifyObservers(const LineColumn: TNEDTextPosition; const Kind: TNEDDocumentChangeKindEnum; const Operation: TNEDEditOperationKindEnum; DeletedLen: Integer; InsertedLen: Integer); overload;
    //
    procedure NotifyLineInserted(LineIndex: Integer; const NewLine: Boolean);
    procedure NotifyLineDeleted(const Operation: TNEDEditOperationKindEnum; LineIndex: Integer);
    procedure NotifyLineChanged(const Operation: TNEDEditOperationKindEnum; LineIndex: Integer);
    //
    //function FindPiece(Position: Integer; out PieceIndex: Integer; out PieceOffset: Integer): Boolean;
    // position conversion
    function FindLine(Position: Integer; out LineIndex: Integer; out Column: Integer): Boolean;
    function GetLine(Index: Integer): TNEDLineProperties;
    //
    // internal helpers
    procedure RebuildCaches;
    procedure RebuildLineNumbers;
    procedure MarkModified;
    function TryMergeUndo(const Op: TNEDEditOperation): Boolean;
    //
    function InternalLineLength(LineIndex: Integer): Integer;
    //
    // line management
    function GetNextLineId: Integer;
    function CreateLine: TNEDLineProperties;
    function CreatePiece: PNEDPiece;
    procedure InsertLine(LineIndex: Integer; const LineText: String; const Line: TNEDLineProperties);
    procedure DeleteLine(const Operation: TNEDEditOperationKindEnum; LineIndex: Integer);
    procedure MergeLines(const Operation: TNEDEditOperationKindEnum; FirstLine: Integer; var SecondLine: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    //
    // document lifecycle
    procedure Clear;
    //
    procedure LoadFromFile(const FileName: String);
    procedure SaveToFile;
    procedure SaveToFileAs(const FileName: String);
    //
    // update batching
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdating: Boolean;
    //
    // observers
    procedure AddObserver(const Observer: TNEDDocumentObserver);
    procedure RemoveObserver(const Observer: TNEDDocumentObserver);
    //
    // editing
    procedure Insert(const Position: Integer; const Text: String); overload;
    procedure Insert(const LineColumn: TNEDTextPosition; const Text: String); overload;
    procedure Insert(const Line, Column: Integer; const Text: String); overload;
    //
    procedure Delete(const Position: Integer; const Count: Integer; const Backspace: Boolean); overload;
    procedure Delete(const LineColumn: TNEDTextPosition; const Count: Integer; const Backspace: Boolean); overload;
    procedure Delete(const Line, Column: Integer; const Count: Integer; const Backspace: Boolean); overload;
    //
    procedure Replace(const Position: Integer; const Count: Integer; const Text: String); overload;
    procedure Replace(const LineColumn: TNEDTextPosition; const Count: Integer; const Text: String); overload;
    procedure Replace(const Line, Column: Integer; const Count: Integer; const Text: String); overload;
    //
    // undo / redo
    procedure Undo;
    procedure Redo;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    //
    // queries
    function GetLength: Integer;
    function GetText: String;
    function GetTextRange(Position: Integer; Count: Integer): String; overload;
    function GetTextRange(Line, Column: Integer; Count: Integer): String; overload;
    function GetOriginalLineText(LineIndex: Integer): String;
    function GetLineText(LineIndex: Integer): String;
    function GetChar(Index: Integer): Char;
    //
    // position conversion
    function PositionToLineColumn(Position: Integer; out Line: Integer; out Column: Integer): Boolean; overload;
    function PositionToLineColumn(Position: Integer; out LineColumn: TNEDTextPosition): Boolean; overload;
    function LineColumnToPosition(Line: Integer; Column: Integer): Integer; overload;
    function LineColumnToPosition(LineColumn: TNEDTextPosition): Integer; overload;
    //
    // line operations
    function LineCount: Integer;
    function VisibleLineCount: Integer;
    function LastLine: Integer;
    //
    // state
    function IsModified: Boolean;
    //
    // properties
    property FilePath: String read FFilePath; // write FFilePath;
    property FileCreateDateTime: TDateTime read FFileCreateDateTime;
    property FileModifyDateTime: TDateTime read FFileModifyDateTime;
    property FileSize: UInt64 read FFileSize;
    property FileReadOnly: Boolean read FFileReadOnly write SetFileReadOnly;
    property FileBOM: Boolean read FFileBOM;
    //
    property Encoding: TEncoding read FEncoding write SetEncoding;
    property EncodingWriteBOM: Boolean read FEncodingWriteBOM write SetEncodingWriteBOM;
    property FileLength: Integer read GetLength;
    property Modified: Boolean read FModified;
    property LineBreak: String read FLineBreak write SetLineBreak;
    property Lines[Index: Integer]: TNEDLineProperties read GetLine;
    property Version: Integer read FVersion;
  end;

  TNEDEditorBuffer = class(TNEDCustomDocument);

implementation

function IfThen(const Cond: Boolean; const TrueValue: Integer; const FalseValue: Integer = 0): Integer; inline;
begin
  if Cond then
    Result := TrueValue
  else
    Result := FalseValue;
end;

{ TNEDPiece }

procedure TNEDPiece.CreateOrigin(const Line, Column: Integer);
begin
  New(Origin);
  Origin.Line := Line;
  Origin.Column := Column;
end;

{ TNEDPieces }

constructor TNEDPieces.Create;
begin
  inherited Create;
  FList := TList<PNEDPiece>.Create;
  FLineNo := -1;
end;

destructor TNEDPieces.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TNEDPieces.Clear;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do begin
    if (FList[I] <> Nil) and (FList[I].Origin = Nil) then
      Dispose(FList[I])
    else if (FList[I] <> Nil) and (FList[I].Origin <> Nil) and (FLineNo > 0) and (FList[I].Origin.Line = FLineNo - 1) then begin
      Dispose(FList[I].Origin);
      FList[I].Origin := Nil;
      Dispose(FList[I])
    end
    else
      Dispose(FList[I]);
    FList[I] := Nil;
  end;
  FList.Clear;
end;

procedure TNEDPieces.SetLineNo(const Value: Integer);
begin
  FLineNo := Value;
end;

function TNEDPieces.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TNEDPieces.GetPiece(Index: Integer): PNEDPiece;
begin
  Result := Nil;
  if (Index < 0) or (Index >= Count) then
    Exit;
  //
  Result := FList[Index];
end;

procedure TNEDPieces.Add(const Piece: PNEDPiece);
begin
  FList.Add(Piece);
end;

procedure TNEDPieces.Insert(Index: Integer; const Piece: PNEDPiece);
begin
  FList.Insert(Index, Piece);
end;

procedure TNEDPieces.Delete(Index: Integer);
var
  Piece: PNEDPiece;
begin
  if (Index < 0) or (Index >= Count) then
    Exit;
  //
  Piece := FList[Index];
  Dispose(Piece);
  FList.Delete(Index);
end;

function TNEDPieces.TotalLength: Integer;
var
  I: Integer;
begin
  Result := 0;
  //
  for I := 0 to FList.Count - 1 do
    Inc(Result, FList[I].Length);
end;

function TNEDPieces.FindPiece(Position: Integer; out PieceIndex, PieceOffset: Integer): Boolean;
var
  I: Integer;
  CurrentPos: Integer;
begin
  Result := False;

  PieceIndex := -1;
  PieceOffset := 0;

  if (Position < 0) or (FList.Count = 0) then
    Exit;

  CurrentPos := 0;

  for I := 0 to FList.Count - 1 do begin
    if Position < CurrentPos + FList[I].Length then begin
      PieceIndex := I;
      PieceOffset := Position - CurrentPos;

      Exit(True);
    end;

    Inc(CurrentPos, FList[I].Length);
  end;

  if Position = CurrentPos then begin
    PieceIndex := FList.Count;
    PieceOffset := 0;
    Result := True;
  end;
end;

procedure TNEDPieces.SplitPiece(PieceIndex: Integer; SplitOffset: Integer; const Trim: Boolean);
var
  Piece: PNEDPiece;
  LeftPiece: PNEDPiece;
  RightPiece: PNEDPiece;
begin
  if (PieceIndex < 0) or (PieceIndex >= FList.Count) then
    Exit;

  Piece := FList[PieceIndex];

  if (SplitOffset <= 0) or (SplitOffset >= Piece.Length) then
    Exit;

  New(LeftPiece);
  LeftPiece.Buffer := Piece.Buffer;
  LeftPiece.Offset := Piece.Offset;
  LeftPiece.Length := SplitOffset;
  LeftPiece.Origin := Piece.Origin;

  if not Trim then begin
    New(RightPiece);
    RightPiece.Buffer := Piece.Buffer;
    RightPiece.Offset := Piece.Offset + SplitOffset;
    RightPiece.Length := Piece.Length - SplitOffset;
    RightPiece.Origin := Piece.Origin;
  end;

  if Piece.Origin <> Nil then
    Piece.Origin := Nil; // do not dispose of this object here
  Dispose(Piece);
  FList[PieceIndex] := LeftPiece;
  if not Trim then
    FList.Insert(PieceIndex + 1, RightPiece);
end;

procedure TNEDPieces.InsertPiece(Position: Integer; const Piece: PNEDPiece);
var
  PieceIndex: Integer;
  PieceOffset: Integer;
begin
  if FList.Count = 0 then begin
    FList.Add(Piece);
    Exit;
  end;

  if not FindPiece(Position, PieceIndex, PieceOffset) then
    Exit;

  if PieceIndex = FList.Count then begin
    FList.Add(Piece);

    Coalesce;
    Exit;
  end;

  if PieceOffset = 0 then begin
    FList.Insert(PieceIndex, Piece);

    Coalesce;
    Exit;
  end;

  if PieceOffset = FList[PieceIndex].Length then begin
    FList.Insert(PieceIndex + 1, Piece);

    Coalesce;
    Exit;
  end;

  SplitPiece(PieceIndex, PieceOffset, False);
  FList.Insert(PieceIndex + 1, Piece);

  Coalesce;
end;

procedure TNEDPieces.DeleteRange(Position: Integer; Count: Integer; const Trim: Boolean);
var
  StartPiece: Integer;
  StartOffset: Integer;

  EndPiece: Integer;
  EndOffset: Integer;

  EndPos: Integer;

  I: Integer;
begin
  if Count <= 0 then
    Exit;

  EndPos := Position + Count;

  if not FindPiece(Position, StartPiece, StartOffset) then
    Exit;

  if not FindPiece(EndPos, EndPiece, EndOffset) then
    Exit;

  // Split end piece first
  if (EndPiece < FList.Count) and (EndOffset > 0) then begin
    SplitPiece(EndPiece, EndOffset, False);

    Inc(EndPiece);
  end;

  // Split start piece second
  if (StartPiece < FList.Count) and (StartOffset > 0) then begin
    SplitPiece(StartPiece, StartOffset, Trim);

    Inc(StartPiece);

    if (Trim and (EndPiece > StartPiece)) or (not Trim and (EndPiece >= StartPiece)) then
      Inc(EndPiece);
    // ensure EndPiece is in range
    if EndPiece > FList.Count then
      EndPiece := FList.Count;
  end;

  // Remove all fully covered pieces
  for I := EndPiece - 1 downto StartPiece do begin
    Dispose(FList[I]);
    FList.Delete(I);
  end;

  Coalesce;
end;

procedure TNEDPieces.Coalesce;
var
  I: Integer;
  A: PNEDPiece;
  B: PNEDPiece;
begin
  I := 0;

  while I < FList.Count - 2 do begin
    A := FList[I];
    B := FList[I + 1];

    // Adjacent fragments from the same source buffer
    // can be merged into one larger piece
    if (A.Buffer = B.Buffer) and (A.Offset + A.Length = B.Offset) then begin
      A.Length := A.Length + B.Length;

      FList[I] := A;
      Dispose(B);
      FList.Delete(I + 1);

      Continue;
    end;

    Inc(I);
  end;
end;

{ TNEDLineProperties }

constructor TNEDLineProperties.Create;
begin
  inherited Create;
  FPieces := TNEDPieces.Create;

  FFlags := [lfDirty, lfSyntaxDirty];

  FLevel := 0;
  FLength := 0;

  FLineNo := -1;
  FLineHeight := -1;
  FStartPos := 0;

  FWrapCount := 1;

  FFoldLevel := 0;
  FFoldParent := -1;
  FFoldChildren := 0;

  FTokenStart := -1;
  FTokenCount := 0;

  FTag := 0;
end;

destructor TNEDLineProperties.Destroy;
begin
  FPieces.Free;
  inherited;
end;

function TNEDLineProperties.GetDeleted: Boolean;
begin
  Result := lfDeleted in FFlags;
end;

function TNEDLineProperties.GetHidden: Boolean;
begin
  Result := lfHidden in FFlags;
end;

function TNEDLineProperties.GetModified: Boolean;
begin
  Result := lfModified in FFlags;
end;

function TNEDLineProperties.GetSaved: Boolean;
begin
  Result := lfSaved in FFlags;
end;

function TNEDLineProperties.GetSpacer: Boolean;
begin
  Result := lfSpacer in FFlags;
end;

procedure TNEDLineProperties.SetDeleted(const Value: Boolean);
begin
  if Value then begin
    Include(FFlags, lfDeleted);
  end
  else
    Exclude(FFlags, lfDeleted);
  Include(FFlags, lfModified);
end;

procedure TNEDLineProperties.SetHidden(const Value: Boolean);
begin
  if Value then
    Include(FFlags, lfHidden)
  else
    Exclude(FFlags, lfHidden);
end;

procedure TNEDLineProperties.SetModified(const Value: Boolean);
begin
  if Value then begin
    Exclude(FFlags, lfSaved);
    Include(FFlags, lfModified);
  end;
//  else
//    Exclude(FFlags, lfModified);
end;

procedure TNEDLineProperties.SetSaved(const Value: Boolean);
begin
  if Modified and Value then begin
    Exclude(FFlags, lfModified);
    Include(FFlags, lfSaved);
  end;
end;

procedure TNEDLineProperties.SetSpacer(const Value: Boolean);
begin
  if Value then
    Include(FFlags, lfSpacer)
  else
    Exclude(FFlags, lfSpacer);
end;

procedure TNEDLineProperties.SetLineNo(const Value: Integer);
begin
  FLineNo := Value;
  FPieces.LineNo := Value;
end;

procedure TNEDLineProperties.Clear;
begin
  FPieces.Clear;

  FFlags := [lfDirty, lfSyntaxDirty];

  FLevel := 0;
  FLength := 0;

  FWrapCount := 1;

  FFoldLevel := 0;
  FFoldParent := -1;
  FFoldChildren := 0;

  FTokenStart := -1;
  FTokenCount := 0;
end;

procedure TNEDLineProperties.UpdateLength;
begin
  FLength := FPieces.TotalLength;
end;

function TNEDLineProperties.IsEmpty: Boolean;
begin
  Result := FLength = 0;
end;

function TNEDLineProperties.IsVisible: Boolean;
begin
  Result := not (lfDeleted in FFlags) and not (lfHidden in FFlags);
end;

function TNEDLineProperties.IsRenderable: Boolean;
begin
  Result := IsVisible and not (lfSpacer in FFlags);
end;

function TNEDLineProperties.GetLineHeight(const DefaultLineHeight: Integer): Integer;
begin
  Result := DefaultLineHeight;
  if FLineHeight > DefaultLineHeight then
    Result := FLineHeight;
end;

procedure TNEDLineProperties.AddPiece(const Piece: PNEDPiece);
begin
  FPieces.Add(Piece);

  Include(FFlags, lfDirty);
  Include(FFlags, lfModified);

  UpdateLength;
end;

procedure TNEDLineProperties.InsertPiece(Position: Integer; const Piece: PNEDPiece);
begin
  FPieces.InsertPiece(Position, Piece);

  Include(FFlags, lfDirty);
  Include(FFlags, lfModified);

  UpdateLength;
end;

procedure TNEDLineProperties.DeleteRange(Position: Integer; Count: Integer; const Trim: Boolean);
begin
  FPieces.DeleteRange(Position, Count, Trim);

  Include(FFlags, lfDirty);
  Include(FFlags, lfModified);

  UpdateLength;
end;

{ TNEDTextPosition }

class function TNEDTextPosition.LineColumn(const ALine, AColumn: Integer): TNEDTextPosition;
begin
  Result.Line := ALine;
  Result.Column := AColumn;
end;

{ TNEDStringList }

function TNEDStringList.GetObject(Index: Integer): TNEDLineProperties;
begin
  Result := TNEDLineProperties(inherited GetObject(Index));
end;

procedure TNEDStringList.PutObject(Index: Integer; AObject: TNEDLineProperties);
begin
  inherited PutObject(Index, AObject);
end;

procedure TNEDStringList.InsertItem(Index: Integer; const S: String; AObject: TNEDLineProperties);
begin
  inherited InsertItem(Index, S, AObject);
end;

function TNEDStringList.AddPair(const Name, Value: string; AObject: TNEDLineProperties): TStrings;
begin
  Result := inherited AddPair(Name, Value, AObject);
end;

function TNEDStringList.AddObject(const S: String; AObject: TNEDLineProperties): Integer;
begin
  Result := inherited AddObject(S, AObject);
end;

procedure TNEDStringList.AddStrings(const Strings: TArray<string>; const Objects: TArray<TNEDLineProperties>);
var
  LObjects: TArray<TObject>;
  L, I: Integer;
begin
  L := Length(Objects);
  SetLength(LObjects, L);
  try
    for I := 0 to L - 1 do
      LObjects[I] := Objects[I];
    inherited AddStrings(Strings, LObjects);
  finally
    SetLength(LObjects, 0);
  end;
end;

function TNEDStringList.IndexOfObject(AObject: TNEDLineProperties): Integer;
begin
  Result := inherited IndexOfObject(AObject);
end;

procedure TNEDStringList.InsertObject(Index: Integer; const S: String; AObject: TNEDLineProperties);
begin
  inherited InsertObject(Index, S, AObject);
end;

{ TNEDDocumentObserver }

constructor TNEDDocumentObserver.Create(const ADocument: TNEDCustomDocument);
begin
  inherited Create;
  FDocument := ADocument;
  FEnabled := True;
  FUpdateLock := 0;
end;

destructor TNEDDocumentObserver.Destroy;
begin
  FDocument := Nil;
  inherited;
end;

procedure TNEDDocumentObserver.DoDocumentChanged(const Change: TNEDDocumentChangeInfo);
begin
  if Assigned(FOnDocumentChanged) then
    FOnDocumentChanged(Change);
end;

procedure TNEDDocumentObserver.DoLineInserted(LineIndex: Integer; AffectedLineStart, AffectedLineEnd: Integer);
var
  Change: TNEDDocumentChangeInfo;
begin
  FillChar(Change, SizeOf(Change), 0);

  Change.Kind := dcLineInsert;
  Change.Operation := opInsert;
  Change.Position := TNEDTextPosition.LineColumn(LineIndex, 0);
  Change.DeletedLength := -1;
  Change.InsertedLength := -1;
  Change.StartLine := AffectedLineStart;
  Change.EndLine := AffectedLineEnd;

  if Assigned(FOnLineInserted) then
    FOnLineInserted(Change);
end;

procedure TNEDDocumentObserver.DoLineDeleted(const Operation: TNEDEditOperationKindEnum; LineIndex: Integer; AffectedLineStart, AffectedLineEnd: Integer);
var
  Change: TNEDDocumentChangeInfo;
begin
  FillChar(Change, SizeOf(Change), 0);

  Change.Kind := dcLineDelete;
  Change.Operation := Operation;
  Change.Position := TNEDTextPosition.LineColumn(LineIndex, 0);
  Change.DeletedLength := -1;
  Change.InsertedLength := -1;
  Change.StartLine := AffectedLineStart;
  Change.EndLine := AffectedLineEnd;

  if Assigned(FOnLineDeleted) then
    FOnLineDeleted(Change);
end;

procedure TNEDDocumentObserver.DoLineChanged(const Operation: TNEDEditOperationKindEnum; LineIndex: Integer; AffectedLineStart, AffectedLineEnd: Integer);
var
  Change: TNEDDocumentChangeInfo;
begin
  FillChar(Change, SizeOf(Change), 0);

  Change.Kind := dcLineChanged;
  Change.Operation := Operation;
  Change.Position := TNEDTextPosition.LineColumn(LineIndex, 0);
  Change.DeletedLength := -1;
  Change.InsertedLength := -1;
  Change.StartLine := AffectedLineStart;
  Change.EndLine := AffectedLineEnd;

  if Assigned(FOnLineChanged) then
    FOnLineChanged(Change);
end;

procedure TNEDDocumentObserver.DoDocumentReloaded;
begin
  // default implementation
end;

procedure TNEDDocumentObserver.DoDocumentReset;
begin
  // default implementation
end;

procedure TNEDDocumentObserver.BeginUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TNEDDocumentObserver.EndUpdate;
begin
  if FUpdateLock > 0 then
    Dec(FUpdateLock);
end;

function TNEDDocumentObserver.IsUpdating: Boolean;
begin
  Result := FUpdateLock > 0;
end;

procedure TNEDDocumentObserver.DocumentChanged(const Position: Integer; const Kind: TNEDDocumentChangeKindEnum; const Operation: TNEDEditOperationKindEnum; DeletedLength: Integer; InsertedLength: Integer);
var
  Change: TNEDDocumentChangeInfo;
  LineColumn: TNEDTextPosition;
begin
  FillChar(Change, SizeOf(Change), 0);

  if not Assigned(FDocument) then
    Exit;

  if not FDocument.PositionToLineColumn(Position, LineColumn) then
    Exit;

  Change.Kind := Kind;
  Change.Operation := Operation;
  Change.Position := LineColumn;
  Change.DeletedLength := DeletedLength;
  Change.InsertedLength := InsertedLength;

  DocumentChanged(Change);
end;

procedure TNEDDocumentObserver.DocumentChanged(const LineColumn: TNEDTextPosition; const Kind: TNEDDocumentChangeKindEnum; const Operation: TNEDEditOperationKindEnum; DeletedLength: Integer; InsertedLength: Integer);
var
  Change: TNEDDocumentChangeInfo;
begin
  FillChar(Change, SizeOf(Change), 0);

  Change.Kind := Kind;
  Change.Operation := Operation;
  Change.Position := LineColumn;
  Change.DeletedLength := DeletedLength;
  Change.InsertedLength := InsertedLength;

  DocumentChanged(Change);
end;

procedure TNEDDocumentObserver.DocumentChanged(const Change: TNEDDocumentChangeInfo);
begin
  if not FEnabled then
    Exit;

  if IsUpdating then
    Exit;

  DoDocumentChanged(Change);
end;

procedure TNEDDocumentObserver.LineInserted(LineIndex: Integer; const NewLine: Boolean);
begin
  if not FEnabled then
    Exit;

  if IsUpdating then
    Exit;

  DoLineInserted(LineIndex + IfThen(NewLine, 1), LineIndex, LineIndex + 1);
end;

procedure TNEDDocumentObserver.LineDeleted(const Operation: TNEDEditOperationKindEnum; LineIndex: Integer);
begin
  if not FEnabled then
    Exit;

  if IsUpdating then
    Exit;

  DoLineDeleted(Operation, LineIndex, LineIndex, LineIndex + 1);
end;

procedure TNEDDocumentObserver.LineChanged(const Operation: TNEDEditOperationKindEnum; LineIndex: Integer);
begin
  if not FEnabled then
    Exit;

  if IsUpdating then
    Exit;

  DoLineChanged(Operation, LineIndex, LineIndex, LineIndex + 1);
end;

procedure TNEDDocumentObserver.DocumentReloaded;
begin
  if not FEnabled then
    Exit;

  if IsUpdating then
    Exit;

  DoDocumentReloaded;
end;

procedure TNEDDocumentObserver.DocumentReset;
begin
  if not FEnabled then
    Exit;

  if IsUpdating then
    Exit;

  DoDocumentReset;
end;

{ TNEDCustomDocument }

constructor TNEDCustomDocument.Create;
var
  Line: TNEDLineProperties;
begin
  inherited Create;

  FFilePath := '';
//  FOriginalBuffer := '';
  FInputBuffer := '';

  FEncoding := TEncoding.UTF8;
  FLines := TNEDStringList.Create;
  FLines.SetEncoding(FEncoding);
  FObservers := TList<TNEDDocumentObserver>.Create;

  FUndo := TStack<TNEDEditOperation>.Create;
  FRedo := TStack<TNEDEditOperation>.Create;
  FInUndoRedo := False;

  FLineBreak := #13#10; // CRLF
  FLineBreakLen := 2;
  FNextLineId := 0;

  FModified := False;
  FUpdateCount := 0;

  FLength := 0;
  FLineCount := 0;
  FVisibleLineCount := 0;
  //
  // always keep at least one line
  Line := CreateLine;
  FLines.AddObject('', Line);
  FEmptyLineCreate := True;

  RebuildCaches;
end;

destructor TNEDCustomDocument.Destroy;
var
  I: Integer;
begin
  for I := 0 to FLines.Count - 1 do
    FLines.Objects[I].Free;
  FLines.Free;
  if not TEncoding.IsStandardEncoding(FEncoding) then
    FEncoding.Free
  else
    FEncoding := Nil;
  FObservers.Free;
  //FOriginalBuffer := '';
  FInputBuffer := '';
  //
  FUndo.Clear;
  FRedo.Clear;

  FUndo.Free;
  FRedo.Free;
  inherited;
end;

procedure TNEDCustomDocument.SetFileReadOnly(const Value: Boolean);
begin
  if FFileReadOnly <> Value then begin
    FFileReadOnly := Value;

    NotifyObservers(0, dcDocumentProperties, opNone, 0, 0);
  end;
end;

procedure TNEDCustomDocument.SetEncoding(const Value: TEncoding);
begin
  if FEncoding <> Value then begin
    if not TEncoding.IsStandardEncoding(FEncoding) then
      FEncoding.Free;

    if TEncoding.IsStandardEncoding(Value) then
      FEncoding := Value
    else if Value <> Nil then
      FEncoding := Value.Clone
    else
      FEncoding := TEncoding.UTF8;

    NotifyObservers(0, dcDocumentProperties, opNone, 0, 0);
  end;
end;

procedure TNEDCustomDocument.SetEncodingWriteBOM(const Value: Boolean);
begin
  if FEncodingWriteBOM <> Value then begin
    FEncodingWriteBOM := Value;

    NotifyObservers(0, dcDocumentProperties, opNone, 0, 0);
  end;
end;

procedure TNEDCustomDocument.SetLineBreak(const Value: String);
begin
  if FLineBreak <> Value then begin
    FLineBreak := Value;
    FLineBreakLen := Length(FLineBreak);

    RebuildCaches;
    NotifyObservers(0, dcDocumentProperties, opNone, 0, 0);
  end;
end;

procedure TNEDCustomDocument.NotifyObservers(Position: Integer; const Kind: TNEDDocumentChangeKindEnum; const Operation: TNEDEditOperationKindEnum; DeletedLen: Integer; InsertedLen: Integer);
var
  LineColumn: TNEDTextPosition;
begin
  if IsUpdating then
    Exit;

  if not PositionToLineColumn(Position, LineColumn) then
    Exit;

  NotifyObservers(LineColumn, Kind, Operation, DeletedLen, InsertedLen);
end;

procedure TNEDCustomDocument.NotifyObservers(const LineColumn: TNEDTextPosition; const Kind: TNEDDocumentChangeKindEnum; const Operation: TNEDEditOperationKindEnum; DeletedLen: Integer; InsertedLen: Integer);
var
  Observer: TNEDDocumentObserver;
begin
  if IsUpdating then
    Exit;

  for Observer in FObservers do
    Observer.DocumentChanged(LineColumn, Kind, Operation, DeletedLen, InsertedLen);
end;

procedure TNEDCustomDocument.NotifyLineInserted(LineIndex: Integer; const NewLine: Boolean);
var
  Observer: TNEDDocumentObserver;
begin
  if IsUpdating then
    Exit;

  for Observer in FObservers do
    Observer.LineInserted(LineIndex, NewLine);
end;

procedure TNEDCustomDocument.NotifyLineDeleted(const Operation: TNEDEditOperationKindEnum; LineIndex: Integer);
var
  Observer: TNEDDocumentObserver;
begin
  if IsUpdating then
    Exit;

  for Observer in FObservers do
    Observer.LineDeleted(Operation, LineIndex);
end;

procedure TNEDCustomDocument.NotifyLineChanged(const Operation: TNEDEditOperationKindEnum; LineIndex: Integer);
var
  Observer: TNEDDocumentObserver;
begin
  if IsUpdating then
    Exit;

  for Observer in FObservers do
    Observer.LineChanged(Operation, LineIndex);
end;

function TNEDCustomDocument.FindLine(Position: Integer; out LineIndex, Column: Integer): Boolean;
var
  I: Integer;
  CurrentPos: Integer;
  LineLen: Integer;
begin
  Result := False;

  LineIndex := -1;
  Column := -1;

  if Position < 0 then
    Exit;

  CurrentPos := 0;

  for I := 0 to FLines.Count - 1 do begin
    if Lines[I].Deleted then
      Continue;

    LineLen := Lines[I].Length;
//    if I < FLines.Count - 1 then
//      Inc(LineLen, FLineBreakLen);

    if Position <= CurrentPos + LineLen then begin
      LineIndex := I;
      Column := Position - CurrentPos;

      Exit(True);
    end;

    if I < FLines.Count - 1 then
      Inc(LineLen, FLineBreakLen);

    Inc(CurrentPos, LineLen);
  end;

  if Position = CurrentPos then begin
    LineIndex := LastLine;
    Column := Lines[LastLine].Length;
    Result := True;
  end;
end;

function TNEDCustomDocument.GetLine(Index: Integer): TNEDLineProperties;
begin
  Result := FLines.Objects[Index];
end;

procedure TNEDCustomDocument.RebuildCaches;
var
  I: Integer;
begin
  FLength := 0;
  FVisibleLineCount := 0;

  for I := 0 to FLines.Count - 1 do begin
    Lines[I].UpdateLength;

    if not Lines[I].Deleted then begin
      Inc(FLength, Lines[I].Length);
      // CRLF between lines
      if I < FLines.Count - 1 then
        Inc(FLength, FLineBreakLen);
    end;

    if Lines[I].IsVisible then
      Inc(FVisibleLineCount);
  end;

  FLineCount := FLines.Count;
end;

procedure TNEDCustomDocument.RebuildLineNumbers;
var
  I: Integer;
begin
  for I := 0 to FLines.Count - 1 do
    Lines[I].LineNo := I + 1;
end;

procedure TNEDCustomDocument.MarkModified;
begin
  FModified := True;
end;

function TNEDCustomDocument.InternalLineLength(LineIndex: Integer): Integer;
begin

end;

function TNEDCustomDocument.GetNextLineId: Integer;
begin
  Inc(FNextLineId);
  Result := FNextLineId;
end;

function TNEDCustomDocument.CreateLine: TNEDLineProperties;
begin
  Result := TNEDLineProperties.Create;
end;

function TNEDCustomDocument.CreatePiece: PNEDPiece;
begin
  New(Result);
  FillChar(Result^, SizeOf(TNEDPiece), 0);
  //
//  Result^.Id := FNextPieceId;
//  Inc(FNextPieceId);
end;

procedure TNEDCustomDocument.InsertLine(LineIndex: Integer; const LineText: String; const Line: TNEDLineProperties);
begin
  FLines.InsertObject(LineIndex, LineText, Line);
  Line.UpdateLength;

  RebuildLineNumbers;
  RebuildCaches;

  NotifyLineInserted(LineIndex, Length(LineText) = 0);
end;

procedure TNEDCustomDocument.DeleteLine(const Operation: TNEDEditOperationKindEnum; LineIndex: Integer);
var
  Prop: TNEDLineProperties;
begin
  if (LineIndex < 0) or (LineIndex >= FLines.Count) then
    Exit;

//  FLines.Objects[LineIndex].Free;
//  FLines.Delete(LineIndex);
  Prop := Lines[LineIndex];
  if Prop.Deleted then
    Exit;
  Prop.Deleted := True;

  RebuildLineNumbers;
  RebuildCaches;

  NotifyLineDeleted(Operation, LineIndex);
end;

procedure TNEDCustomDocument.MergeLines(const Operation: TNEDEditOperationKindEnum; FirstLine: Integer; var SecondLine: Integer);
var
  A: TNEDLineProperties;
  B: TNEDLineProperties;
  I: Integer;
begin
  if (FirstLine < 0) or (SecondLine < 0) then
    Exit;

  if FirstLine >= FLines.Count then
    Exit;

  if SecondLine >= FLines.Count then
    Exit;

  A := Lines[FirstLine];
  B := Lines[SecondLine];

  //
  // move all pieces from B to A
  if not A.Deleted then begin
    for I := 0 to B.Pieces.Count - 1 do begin
      if B.Pieces.Pieces[I].Origin = Nil then
        B.Pieces.Pieces[I].CreateOrigin(SecondLine, 0);
      A.Pieces.Add(B.Pieces.Pieces[I]);
    end;
    A.UpdateLength;
    DeleteLine(Operation, SecondLine);
  end
  else if A.Deleted and (A.Length > 0) then
    A.Deleted := False
  else
    Dec(SecondLine);
end;

procedure TNEDCustomDocument.Clear;
var
  I: Integer;
  Line: TNEDLineProperties;
begin
  BeginUpdate;
  try
    for I := 0 to FLines.Count - 1 do
      FLines.Objects[I].Free;
    FLines.Clear;
    //
    //FOriginalBuffer := '';
    FInputBuffer := '';
    //
    FUndo.Clear;
    FRedo.Clear;
    //
    if FEmptyLineCreate then begin
      Line := CreateLine;
      FLines.AddObject('', Line);
    end;
    //
    FModified := False;
    RebuildCaches;
  finally
    EndUpdate;
  end;
end;

procedure TNEDCustomDocument.LoadFromFile(const FileName: String);
var
  LineObj: TNEDLineProperties;
  Piece: PNEDPiece;
  I: Integer;
//  Offset: Integer;
begin
  BeginUpdate;
  try
    FEmptyLineCreate := False;
    try
      Clear;
    finally
      FEmptyLineCreate := True;
    end;
    //
    FLines.LoadFromFile(FileName);
    FFilePath := FileName;
    //
    // keep original file contents untouched
    //FOriginalBuffer := Source.Text;
    //
    // remove automatically created empty line
//    if FLines.Count = 1 then begin
//      FLines.Objects[0].Free;
//      FLines.Clear;
//    end;

//    Offset := 0;
    for I := 0 to FLines.Count - 1 do begin
      LineObj := CreateLine;
      // make default piece for non-empty line
      if FLines[I] <> '' then begin
        Piece := CreatePiece;
        Piece.Buffer := pbOriginal;
        Piece.Offset := 0; //Offset;
        Piece.Length := Length(FLines[I]);
        Piece.Origin := Nil;

        LineObj.Pieces.Add(Piece);
      end;

      LineObj.UpdateLength;

      //FLines.AddObject(Source[I], LineObj);
      FLines.Objects[I] := LineObj;

//      Inc(Offset, Length(Source[I]));

      //
      // original file buffer contains CRLF.
//      Inc(Offset, FLineBreakLen);
    end;

    if FLines.Count = 0 then
      FLines.AddObject('', CreateLine);

    FModified := False;

    RebuildLineNumbers;
    RebuildCaches;
  finally
    EndUpdate;
    NotifyObservers(0, dcLoad, opNone, 0, FLength);
  end;
end;

procedure TNEDCustomDocument.SaveToFile;
var
  Stream: TStringList;
begin
  Stream := TStringList.Create;
  try
    Stream.Text := GetText;
    Stream.SaveToFile(FFilePath);
    FModified := False;
  finally
    Stream.Free;
  end;
end;

procedure TNEDCustomDocument.SaveToFileAs(const FileName: String);
begin
  FFilePath := FileName;
  SaveToFile;
end;

function TNEDCustomDocument.TryMergeUndo(const Op: TNEDEditOperation): Boolean;
var
  TopOp: TNEDEditOperation;
begin
  Result := False;

  if FUndo.Count = 0 then
    Exit;

  TopOp := FUndo.Peek;

  // insert merge:
  //
  // user typed:
  //
  //   h
  //   e
  //   l
  //   l
  //   o
  //
  // into:
  //
  //   hello
  //
  if (TopOp.Kind = opInsert) and (Op.Kind = opInsert) then begin
    // new insertion must immediately follow previous insertion
    if LineColumnToPosition(Op.Position) = LineColumnToPosition(TopOp.Position) + Length(TopOp.Text) then begin
      FUndo.Pop;

      TopOp.Text := TopOp.Text + Op.Text;

      FUndo.Push(TopOp);

      Exit(True);
    end;
  end;

  // delete merge:
  //
  // backspace:
  //
  // abcde|
  // abcd|
  // abc|
  //
  // creates one undo record
  //
  if (TopOp.Kind = opDeleteBKSP) and (Op.Kind = opDeleteBKSP) then begin
    // consecutive backspace
    if LineColumnToPosition(Op.Position) = LineColumnToPosition(TopOp.Position) - Length(Op.Text) then begin
      FUndo.Pop;

      TopOp.Position := Op.Position;

      TopOp.Text := Op.Text + TopOp.Text;

      FUndo.Push(TopOp);

      Exit(True);
    end;

    // consecutive DEL key
    if LineColumnToPosition(Op.Position) = LineColumnToPosition(TopOp.Position) then begin
      FUndo.Pop;

      TopOp.Text := TopOp.Text + Op.Text;

      FUndo.Push(TopOp);

      Exit(True);
    end;
  end;
end;

procedure TNEDCustomDocument.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TNEDCustomDocument.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
end;

function TNEDCustomDocument.IsUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TNEDCustomDocument.AddObserver(const Observer: TNEDDocumentObserver);
begin
  if FObservers.IndexOf(Observer) < 0 then
    FObservers.Add(Observer);
end;

procedure TNEDCustomDocument.RemoveObserver(const Observer: TNEDDocumentObserver);
begin
  FObservers.Remove(Observer);
end;

procedure TNEDCustomDocument.Insert(const Position: Integer; const Text: String);
var
  LineIndex: Integer;
  Column: Integer;
begin
  if Text = '' then
    Exit;

  if not FindLine(Position, LineIndex, Column) then
    Exit;

  Insert(LineIndex, Column, Text);
end;

procedure TNEDCustomDocument.Insert(const LineColumn: TNEDTextPosition; const Text: String);
begin
  if Text = '' then
    Exit;

  Insert(LineColumn.Line, LineColumn.Column, Text);
end;

procedure TNEDCustomDocument.Insert(const Line, Column: Integer; const Text: String);
var
  LineProperties: TNEDLineProperties;
  Piece: PNEDPiece;
  AddOffset: Integer;
  Parts: TStringList;
  I: Integer;
  NewLine: TNEDLineProperties;
  UndoOp: TNEDEditOperation;
  RemainingLineText: String;
  RemainingLineTextLen: Integer;
begin
  RemainingLineText := '';
  if Text = '' then
    Exit;

  // record user edit
  if not FInUndoRedo then begin
    UndoOp.Kind := opInsert;
    UndoOp.Position := TNEDTextPosition.LineColumn(Line, Column);
    if Text = #13 then begin // line break
      UndoOp.Text := LineBreak;
      UndoOp.Length := Length(LineBreak);
    end
    else begin // other text
      UndoOp.Text := Text;
      UndoOp.Length := Length(Text);
    end;
    UndoOp.StartLine := -1;
    UndoOp.EndLine := -1;
    UndoOp.Version := -1;

    if not TryMergeUndo(UndoOp) then
      FUndo.Push(UndoOp);

    FRedo.Clear;
  end;

  AddOffset := Length(FInputBuffer);
  if Text = #13 then // line break
    //FInputBuffer := FInputBuffer + LineBreak
  else
    FInputBuffer := FInputBuffer + Text;

  LineProperties := Lines[Line];

  if Text = #13 then begin // line break was entered
    if Column = 0 then begin // insert new empty line before current
      // mark current line as modified
      LineProperties.Modified := True;
      // make new line properties
      NewLine := CreateLine;
      NewLine.Modified := True;
      // new empty line does not need any pieces created
      InsertLine(Line, '', NewLine);
    end
    else begin
      // copy all remaining text from current line
      RemainingLineText := TrimRight(GetTextRange(Line, Column, LineProperties.Length - Column));
      RemainingLineTextLen := Length(RemainingLineText);

//      // insert line break into current line
//      Piece := CreatePiece;
//      Piece.Buffer := pbInputBuffer;
//      Piece.Offset := AddOffset;
//      Piece.Length := Length(LineBreak);
//      Piece.Origin := Nil;

      LineProperties.DeleteRange(Column, RemainingLineTextLen, True); // remove remaining text
      //LineProperties.InsertPiece(Column, Piece); // add line break
//      LineProperties.AddPiece(Piece); // add line break

      // make new line properties
      NewLine := CreateLine;
      NewLine.Modified := True;

      // copy all remaining text from current line to the new line as Original buffer
      if RemainingLineTextLen > 0 then begin
        Piece := CreatePiece;
        Piece.Buffer := pbOriginal;
        Piece.Offset := 0;
        Piece.Length := RemainingLineTextLen;
        Piece.CreateOrigin(Line, Column);

        NewLine.Pieces.Add(Piece);
      end;
      InsertLine(Line + 1, RemainingLineText, NewLine);
      RemainingLineText := '';
    end;

    MarkModified;

    Exit;
  end;

  // fast path: no line breaks
  if Pos(#10, Text) = 0 then begin // @FIX: this #10 probably should be determined by our LineBreak type
    Piece := CreatePiece;
    Piece.Buffer := pbInputBuffer;
    Piece.Offset := AddOffset;
    Piece.Length := Length(Text);
    Piece.Origin := Nil;

    LineProperties.InsertPiece(Column, Piece);

    MarkModified;
    RebuildCaches;

    NotifyObservers(TNEDTextPosition.LineColumn(Line, Column), dcInsert, opInsert, 0, Length(Text));

    Exit;
  end;

  // multi-line insert
  Parts := TStringList.Create;
  try
    Parts.Text := StringReplace(Text, #13, '', [rfReplaceAll]); // @FIX: it assumes that Text contains #13#10 as line breaks

    Piece := CreatePiece;
    Piece.Buffer := pbInputBuffer;
    Piece.Offset := AddOffset;
    Piece.Length := Length(Parts[0]);
    Piece.Origin := Nil;

    LineProperties.InsertPiece(Column, Piece);

    Inc(AddOffset, Piece.Length + FLineBreakLen); // CRLF

    for I := 1 to Parts.Count - 1 do begin
      NewLine := CreateLine;

      if Parts[I] <> '' then begin
        Piece := CreatePiece;
        Piece.Buffer := pbInputBuffer;
        Piece.Offset := AddOffset;
        Piece.Length := Length(Parts[I]);
        Piece.Origin := Nil;

        NewLine.Pieces.Add(Piece);

        Inc(AddOffset, Piece.Length + FLineBreakLen); // CRLF
      end;

      NewLine.UpdateLength;

      InsertLine(Line + I, '', NewLine); // @FIX: there should be LineText provided
    end;
  finally
    Parts.Free;
  end;

  MarkModified;
  RebuildCaches;

  NotifyObservers(TNEDTextPosition.LineColumn(Line, Column), dcInsert, opInsert, 0, Length(Text));
end;

procedure TNEDCustomDocument.Delete(const Position, Count: Integer; const Backspace: Boolean);
var
  StartLine: Integer;
  StartColumn: Integer;
begin
  if Count <= 0 then
    Exit;

  if not FindLine(Position, StartLine, StartColumn) then
    Exit;

  Delete(StartLine, StartColumn, Count, Backspace);
end;

procedure TNEDCustomDocument.Delete(const LineColumn: TNEDTextPosition; const Count: Integer; const Backspace: Boolean);
begin
  Delete(LineColumn.Line, LineColumn.Column, Count, Backspace);
end;

procedure TNEDCustomDocument.Delete(const Line, Column, Count: Integer; const Backspace: Boolean);
var
  StartLine: Integer;
  StartColumn: Integer;
  CharCount: Integer;
  EndLine: Integer;
  EndColumn: Integer;
  UndoOp: TNEDEditOperation;
  DeletedText: String;
  ChangeKind: TNEDDocumentChangeKindEnum;
  Operation: TNEDEditOperationKindEnum;
begin
  StartLine := Line;
  StartColumn := Column;
  CharCount := Count;
  Operation := opDeleteDEL;
  if Backspace then begin
    Operation := opDeleteBKSP;
    CharCount := -Count;
    if not FindLine(LineColumnToPosition(Line, Column) + CharCount, StartLine, StartColumn) then
      Exit;
  end;

  if not FindLine(LineColumnToPosition(StartLine, StartColumn) + Count, EndLine, EndColumn) then
    Exit;

  // capture text before deletion
  DeletedText := GetTextRange(StartLine, StartColumn, Count); // @TODO: check if this needs to be trimed

  // record user edit
  if not FInUndoRedo then begin
    UndoOp.Kind := Operation;
    UndoOp.Position := TNEDTextPosition.LineColumn(StartLine, StartColumn); // @TODO: check if this is ok for backspace operation
    UndoOp.Text := DeletedText;
    UndoOp.Length := Length(DeletedText);
    UndoOp.StartLine := -1;
    UndoOp.EndLine := -1;
    UndoOp.Version := -1;

    if not TryMergeUndo(UndoOp) then
      FUndo.Push(UndoOp);

    FRedo.Clear;
  end;

  ChangeKind := dcDelete;
  //
  // single-line deletion
  if (StartLine = EndLine) and (StartColumn >= 0) and (Lines[StartLine].Length > 0) and (EndColumn <= Lines[StartLine].Length) then begin // delete operation
    Lines[StartLine].DeleteRange(StartColumn, Count, Backspace);
    //Lines[StartLine].UpdateLength;
  end
  else begin
    // remove tail from first line
    if Lines[StartLine].Length = 0 then
      Lines[StartLine].Deleted := True
    else// if StartColumn >= 0 then
      Lines[StartLine].DeleteRange(StartColumn, Lines[StartLine].Length - StartColumn, False); // Trim or not ???

    //
    // remove head from last line
    if (Lines[EndLine].Length = 0) and not Lines[EndLine].Deleted then begin
      Lines[EndLine].Deleted := True;
      Inc(EndLine);
    end
    else
      Lines[EndLine].DeleteRange(0, EndColumn, False); // Trim or not ???

    if Backspace and (Column + CharCount < 0) then
      StartColumn := Lines[StartLine].Length
    else begin
      if not Backspace and (EndColumn > StartColumn) then begin
        EndColumn := StartColumn;
        Inc(EndLine);
      end;

      if EndColumn >= 0 then
        StartColumn := EndColumn;
    end;

    //
    // merge both remaining fragments

    MergeLines(Operation, StartLine, EndLine);

    ChangeKind := dcLineChanged;

    //
    // delete lines in between
//    while FLines.Count > StartLine + 1 do begin
//      if StartLine + 1 >= EndLine then
//        Break;
//
//      DeleteLine(StartLine + 1);
//      Dec(EndLine);
//    end;
    if EndLine > StartLine then begin
      while EndLine > StartLine do begin
        DeleteLine(Operation, EndLine);
        Dec(EndLine);
      end;
    end;
  end;

  MarkModified;
  RebuildCaches;

  NotifyObservers(TNEDTextPosition.LineColumn(StartLine, StartColumn), ChangeKind, Operation, Count, 0);
end;

procedure TNEDCustomDocument.Replace(const Position, Count: Integer; const Text: String);
begin
  Delete(Position, Count, False);
  Insert(Position, Text);
end;

procedure TNEDCustomDocument.Replace(const LineColumn: TNEDTextPosition; const Count: Integer; const Text: String);
begin
  Delete(LineColumn, Count, False);
  Insert(LineColumn, Text);
end;

procedure TNEDCustomDocument.Replace(const Line, Column, Count: Integer; const Text: String);
begin
  Delete(Line, Column, Count, False);
  Insert(Line, Column, Text);
end;

procedure TNEDCustomDocument.Undo;
var
  Op: TNEDEditOperation;
begin
  if not CanUndo then
    Exit;

  Op := FUndo.Pop;

  FInUndoRedo := True;
  try
    case Op.Kind of
      opInsert: Delete(LineColumnToPosition(Op.Position), Length(Op.Text), False);
      opDeleteDEL: Insert(LineColumnToPosition(Op.Position), Op.Text);
      opDeleteBKSP: Insert(LineColumnToPosition(Op.Position), Op.Text);
    end;
  finally
    FInUndoRedo := False;
  end;

  FRedo.Push(Op);
end;

procedure TNEDCustomDocument.Redo;
var
  Op: TNEDEditOperation;
begin
  if not CanRedo then
    Exit;

  Op := FRedo.Pop;

  FInUndoRedo := True;
  try
    case Op.Kind of
      opInsert: Insert(LineColumnToPosition(Op.Position), Op.Text);
      opDeleteDEL: Delete(LineColumnToPosition(Op.Position), Length(Op.Text), False);
      opDeleteBKSP: Delete(LineColumnToPosition(Op.Position), Length(Op.Text), False);
    end;
  finally
    FInUndoRedo := False;
  end;

  FUndo.Push(Op);
end;

function TNEDCustomDocument.CanUndo: Boolean;
begin
  Result := FUndo.Count > 0;
end;

function TNEDCustomDocument.CanRedo: Boolean;
begin
  Result := FRedo.Count > 0;
end;

function TNEDCustomDocument.GetLength: Integer;
begin
  Result := FLength;
end;

function TNEDCustomDocument.GetText: String;
var
  SB: TStringBuilder;
  LineProp: TNEDLineProperties;
  I: Integer;
begin
  SB := TStringBuilder.Create;
  try
    for I := 0 to FLines.Count - 1 do begin
      SB.Append(GetLineText(I));
      LineProp := Lines[I];
      LineProp.Saved := True;

      if I < FLines.Count - 1 then
        SB.Append(FLineBreak);
    end;
    //
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TNEDCustomDocument.GetTextRange(Position, Count: Integer): String;
var
  S: String;
begin
  S := GetText;

  if Position < 0 then
    Position := 0;

  if Position > Length(S) then
    Position := Length(S);

  if Count < 0 then
    Count := 0;

  Result := Copy(S, Position + 1, Count);
end;

function TNEDCustomDocument.GetTextRange(Line, Column: Integer; Count: Integer): String;
var
  S: String;
begin
  Result := '';

  if Line < 0 then
    Line := 0;

  if Column < 0 then
    Column := 0;

  if Count < 0 then
    Count := 0;

  S := GetLineText(Line);
  if (Length(S) - Column) > Count then
    Result := Copy(S, Column + 1, Count)
  else begin
    Result := Copy(S, Column + 1, Length(S) - Column) + FLineBreak; // CRLF
    Dec(Count, Length(Result) + FLineBreakLen);
    Inc(Line);
    while (Count > 0) and (Line < LineCount) do begin
      S := GetLineText(Line);
      if Length(S) > Count then begin
        Result := Result + Copy(S, 1, Count);
        Break;
      end
      else begin
        Result := Result + S + FLineBreak; // CRLF
        Dec(Count, Length(S) + FLineBreakLen);
        Inc(Line);
      end;
    end;
  end;
end;

function TNEDCustomDocument.GetOriginalLineText(LineIndex: Integer): String;
begin
  Result := '';

  if (LineIndex < 0) or (LineIndex >= FLines.Count) then
    Exit;

  Result := FLines.Strings[LineIndex];
end;

function TNEDCustomDocument.GetLineText(LineIndex: Integer): String;
var
  Line: TNEDLineProperties;
  Piece: PNEDPiece;
  I: Integer;
  SB: TStringBuilder;
  OriginalLineText: String;
  TempLineIndex: Integer;
begin
  Result := '';

  if (LineIndex < 0) or (LineIndex >= FLines.Count) then
    Exit;

  Line := Lines[LineIndex];

  SB := TStringBuilder.Create;
  try
    if Line.Deleted {and (Line.Pieces.Count = 0)} then begin
      OriginalLineText := GetOriginalLineText(LineIndex);
      SB.Append(OriginalLineText);
      OriginalLineText := '';
    end
    else begin
      for I := 0 to Line.Pieces.Count - 1 do begin
        Piece := Line.Pieces[I];
        TempLineIndex := LineIndex;
        if Piece.Origin <> Nil then
          TempLineIndex := Piece.Origin.Line;
        case Piece.Buffer of
          pbOriginal: begin
            OriginalLineText := GetOriginalLineText(TempLineIndex);
            SB.Append(Copy(OriginalLineText, Piece.Offset + 1, Piece.Length));
            OriginalLineText := '';
          end;
          pbInputBuffer: SB.Append(Copy(FInputBuffer, Piece.Offset + 1, Piece.Length));
        end;
      end;
    end;
    //
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TNEDCustomDocument.GetChar(Index: Integer): Char;
var
  S: String;
begin
  S := GetText;

  if (Index < 0) or (Index >= Length(S)) then
    raise ERangeError.Create('Character index out of range');

  Result := S[Index + 1];
end;

function TNEDCustomDocument.PositionToLineColumn(Position: Integer; out Line, Column: Integer): Boolean;
begin
  Result := FindLine(Position, Line, Column);
end;

function TNEDCustomDocument.PositionToLineColumn(Position: Integer; out LineColumn: TNEDTextPosition): Boolean;
begin
  Result := FindLine(Position, LineColumn.Line, LineColumn.Column);
end;

function TNEDCustomDocument.LineColumnToPosition(Line, Column: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;

  for I := 0 to Line - 1 do begin
    if Lines[I].Deleted then
      Continue;

    Inc(Result, Lines[I].Length);
    if I < FLines.Count - 1 then
      Inc(Result, FLineBreakLen);
  end;

  Inc(Result, Column);
end;

function TNEDCustomDocument.LineColumnToPosition(LineColumn: TNEDTextPosition): Integer;
begin
  Result := LineColumnToPosition(LineColumn.Line, LineColumn.Column);
end;

function TNEDCustomDocument.LineCount: Integer;
begin
  Result := FLineCount;
end;

function TNEDCustomDocument.VisibleLineCount: Integer;
begin
  Result := FVisibleLineCount;
end;

function TNEDCustomDocument.LastLine: Integer;
begin
  Result := FLineCount - 1;
end;

function TNEDCustomDocument.IsModified: Boolean;
begin
  Result := FModified;
end;

end.

