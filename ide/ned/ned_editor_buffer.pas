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

  TNEDPiece = record
    Buffer: TNEDPieceBufferTypeEnum;
    Offset: Integer;
    Length: Integer;
  end;

  TNEDPieces = class
  private
    FList: TList<TNEDPiece>;
    //
    function GetCount: Integer;
    function GetPiece(Index: Integer): TNEDPiece;
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Clear;
    //
    procedure Add(const Piece: TNEDPiece);
    procedure Insert(Index: Integer; const Piece: TNEDPiece);
    procedure Delete(Index: Integer);
    //
    function TotalLength: Integer;
    //
    function FindPiece(Position: Integer; out PieceIndex: Integer; out PieceOffset: Integer): Boolean;
    //
    procedure SplitPiece(PieceIndex: Integer; SplitOffset: Integer);
    procedure InsertPiece(Position: Integer; const Piece: TNEDPiece);
    procedure DeleteRange(Position: Integer; Count: Integer);
    //
    procedure Coalesce;
    //
    property Count: Integer read GetCount;
    //
    property Pieces[Index: Integer]: TNEDPiece read GetPiece; default;
  end;

  // line state flags
  TNEDLineFlagEnum = (
    lfDeleted,       // line logically removed from view
    lfSpacer,        // artificial wrapped/spacer line
    lfModified,      // modified since last save
    lfBookmark,      // user bookmark
    lfBreakpoint,    // debugger breakpoint
    lfFolded,        // line starts collapsed block
    lfHidden,        // hidden by folding
    lfDirty,         // requires layout recalculation
    lfSyntaxDirty    // requires syntax reparse
  );

  TNEDLineFlags = set of TNEDLineFlagEnum;

  TNEDLineProperties = class
  private
    FPieces: TNEDPieces;
    //FTokens: TList<TNEDTextToken>;
    FFlags: TNEDLineFlags;  // line state
    FLevel: Integer;        // indentation level determined by parser
    FLength: Integer;       // character count in line; cached value
    FLineNo: Integer;       // physical document line number
    FStartPos: Integer;     // starting document position
    FWrapCount: Integer;    // visual line count after wrapping
    FFoldLevel: Integer;    // fold support
    FFoldParent: Integer;
    FFoldChildren: Integer;
    FTokenStart: Integer;   // syntax support
    FTokenCount: Integer;
    FTag: NativeInt;        // user data
  protected
    function GetDeleted: Boolean;
    function GetSpacer: Boolean;
    procedure SetDeleted(const Value: Boolean);
    procedure SetSpacer(const Value: Boolean);
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
    //
    procedure AddPiece(const Piece: TNEDPiece); // piece manipulation helpers
    procedure InsertPiece(Position: Integer; const Piece: TNEDPiece);
    procedure DeleteRange(Position: Integer; Count: Integer);
    //
    // accessors
    property Pieces: TNEDPieces read FPieces;
    property Deleted: Boolean read GetDeleted write SetDeleted;
    property Spacer: Boolean read GetSpacer write SetSpacer;
    property Flags: TNEDLineFlags read FFlags write FFlags;
    property Level: Integer read FLevel write FLevel;
    property Length: Integer read FLength;
    property LineNo: Integer read FLineNo write FLineNo;
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
    opInsert,
    opDelete,
    opReplace
  );

  TNEDEditOperation = record
    Kind: TNEDEditOperationKindEnum;
    Position: TNEDTextPosition;
    Text: String;
    Length: Integer;
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
    dcReload,
    dcReset
  );

  //
  // Detailed change notification.
  //
  TNEDDocumentChangeInfo = record
    Kind: TNEDDocumentChangeKindEnum;

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
  protected
    // character-level change
    procedure DoDocumentChanged(const Change: TNEDDocumentChangeInfo); virtual;
    // line-level notifications
    procedure DoLineInserted(LineIndex: Integer); virtual;
    procedure DoLineDeleted(LineIndex: Integer); virtual;
    procedure DoLineChanged(LineIndex: Integer); virtual;
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
    procedure DocumentChanged(Position: Integer; DeletedLength: Integer; InsertedLength: Integer); overload; virtual;
    procedure DocumentChanged(const LineColumn: TNEDTextPosition; DeletedLength: Integer; InsertedLength: Integer); overload; virtual;
    procedure DocumentChanged(const Change: TNEDDocumentChangeInfo); overload; virtual;
    //
    // line notifications
    procedure LineInserted(LineIndex: Integer); virtual;
    procedure LineDeleted(LineIndex: Integer); virtual;
    procedure LineChanged(LineIndex: Integer); virtual;
    //
    // full document notifications
    procedure DocumentReloaded; virtual;
    procedure DocumentReset; virtual;
    //
    property Enabled: Boolean read FEnabled write FEnabled;
    property OnDocumentChanged: TNEDDocumentChangedEvent read FOnDocumentChanged write FOnDocumentChanged;
  end;

{ TNEDCustomDocument }

  TNEDCustomDocument = class
  private
    FFilePath: String;
    FFileCreateDateTime: TDateTime;
    FFileModifyDateTime: TDateTime;
    FFileSize: UInt64;
    FLines: TNEDStringList; // logical lines
    FInputBuffer: String; // piece table add buffer
    FObservers: TList<TNEDDocumentObserver>; // attached views/services.
    // Undo / Redo
    FUndo: TStack<TNEDEditOperation>;
    FRedo: TStack<TNEDEditOperation>;
    FInUndoRedo: Boolean;
    //
    FEmptyLineCreate: Boolean;
    // modification state
    FModified: Boolean;
    FUpdateCount: Integer; // update lock
    FLength: Integer; // cached document length
    FLineCount: Integer; // cached line count
  protected
    procedure NotifyObservers(Position: Integer; DeletedLen: Integer; InsertedLen: Integer); overload;
    procedure NotifyObservers(const LineColumn: TNEDTextPosition; DeletedLen: Integer; InsertedLen: Integer); overload;
    //
    procedure NotifyLineInserted(LineIndex: Integer);
    procedure NotifyLineDeleted(LineIndex: Integer);
    procedure NotifyLineChanged(LineIndex: Integer);
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
    function CreateLine: TNEDLineProperties;
    procedure InsertLine(LineIndex: Integer; Line: TNEDLineProperties);
    procedure DeleteLine(LineIndex: Integer);
    procedure MergeLines(FirstLine: Integer; SecondLine: Integer);
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
    procedure Delete(const Position: Integer; const Count: Integer); overload;
    procedure Delete(const LineColumn: TNEDTextPosition; const Count: Integer); overload;
    procedure Delete(const Line, Column: Integer; const Count: Integer); overload;
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
    function LastLine: Integer;
    //
    // state
    function IsModified: Boolean;
    //
    // properties
    property FilePath: String read FFilePath; // write FFilePath;
    property FileLength: Integer read GetLength;
    property Modified: Boolean read FModified;
    property Lines[Index: Integer]: TNEDLineProperties read GetLine;
  end;

  TNEDEditorBuffer = class(TNEDCustomDocument);

implementation

{ TNEDPieces }

constructor TNEDPieces.Create;
begin
  inherited Create;
  FList := TList<TNEDPiece>.Create;
end;

destructor TNEDPieces.Destroy;
begin
  FList.Free;
  inherited;
end;

function TNEDPieces.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TNEDPieces.GetPiece(Index: Integer): TNEDPiece;
begin
  FillChar(Result, SizeOf(TNEDPiece), 0);
  if (Index < 0) or (Index >= Count) then
    Exit;
  //
  Result := FList[Index];
end;

procedure TNEDPieces.Clear;
begin
  FList.Clear;
end;

procedure TNEDPieces.Add(const Piece: TNEDPiece);
begin
  FList.Add(Piece);
end;

procedure TNEDPieces.Insert(Index: Integer; const Piece: TNEDPiece);
begin
  FList.Insert(Index, Piece);
end;

procedure TNEDPieces.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= Count) then
    Exit;
  //
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

  if Position < 0 then
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

procedure TNEDPieces.SplitPiece(PieceIndex, SplitOffset: Integer);
var
  Piece: TNEDPiece;
  LeftPiece: TNEDPiece;
  RightPiece: TNEDPiece;
begin
  if (PieceIndex < 0) or (PieceIndex >= FList.Count) then
    Exit;

  Piece := FList[PieceIndex];

  if (SplitOffset <= 0) or (SplitOffset >= Piece.Length) then
    Exit;

  LeftPiece := Piece;
  LeftPiece.Length := SplitOffset;

  RightPiece := Piece;
  RightPiece.Offset := Piece.Offset + SplitOffset;
  RightPiece.Length := Piece.Length - SplitOffset;

  FList[PieceIndex] := LeftPiece;
  FList.Insert(PieceIndex + 1, RightPiece);
end;

procedure TNEDPieces.InsertPiece(Position: Integer; const Piece: TNEDPiece);
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

  SplitPiece(PieceIndex, PieceOffset);
  FList.Insert(PieceIndex + 1, Piece);

  Coalesce;
end;

procedure TNEDPieces.DeleteRange(Position, Count: Integer);
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
    SplitPiece(EndPiece, EndOffset);

    Inc(EndPiece);
  end;

  // Split start piece second
  if (StartPiece < FList.Count) and (StartOffset > 0) then begin
    SplitPiece(StartPiece, StartOffset);

    Inc(StartPiece);

    if EndPiece > StartPiece then
      Inc(EndPiece);
  end;

  // Remove all fully covered pieces
  for I := EndPiece - 1 downto StartPiece do
    FList.Delete(I);

  Coalesce;
end;

procedure TNEDPieces.Coalesce;
var
  I: Integer;
  A: TNEDPiece;
  B: TNEDPiece;
begin
  I := 0;

  while I < FList.Count - 1 do begin
    A := FList[I];
    B := FList[I + 1];

    // Adjacent fragments from the same source buffer
    // can be merged into one larger piece
    if (A.Buffer = B.Buffer) and (A.Offset + A.Length = B.Offset) then begin
      A.Length := A.Length + B.Length;

      FList[I] := A;
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

function TNEDLineProperties.GetSpacer: Boolean;
begin
  Result := lfSpacer in FFlags;
end;

procedure TNEDLineProperties.SetDeleted(const Value: Boolean);
begin
  if Value then
    Include(FFlags, lfDeleted)
  else
    Exclude(FFlags, lfDeleted);
end;

procedure TNEDLineProperties.SetSpacer(const Value: Boolean);
begin
  if Value then
    Include(FFlags, lfSpacer)
  else
    Exclude(FFlags, lfSpacer);
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

procedure TNEDLineProperties.AddPiece(const Piece: TNEDPiece);
begin
  FPieces.Add(Piece);

  Include(FFlags, lfDirty);

  UpdateLength;
end;

procedure TNEDLineProperties.InsertPiece(Position: Integer; const Piece: TNEDPiece);
begin
  FPieces.InsertPiece(Position, Piece);

  Include(FFlags, lfDirty);

  UpdateLength;
end;

procedure TNEDLineProperties.DeleteRange(Position, Count: Integer);
begin
  FPieces.DeleteRange(Position, Count);

  Include(FFlags, lfDirty);

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

procedure TNEDDocumentObserver.DoLineInserted(LineIndex: Integer);
begin
  // default implementation
end;

procedure TNEDDocumentObserver.DoLineDeleted(LineIndex: Integer);
begin
  // default implementation
end;

procedure TNEDDocumentObserver.DoLineChanged(LineIndex: Integer);
begin
  // default implementation
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

procedure TNEDDocumentObserver.DocumentChanged(Position, DeletedLength, InsertedLength: Integer);
var
  Change: TNEDDocumentChangeInfo;
  LineColumn: TNEDTextPosition;
begin
  FillChar(Change, SizeOf(Change), 0);

  if not Assigned(FDocument) then
    Exit;

  if not FDocument.PositionToLineColumn(Position, LineColumn) then
    Exit;

  Change.Kind := dcUnknown;
  Change.Position := LineColumn;
  Change.DeletedLength := DeletedLength;
  Change.InsertedLength := InsertedLength;

  DocumentChanged(Change);
end;

procedure TNEDDocumentObserver.DocumentChanged(const LineColumn: TNEDTextPosition; DeletedLength, InsertedLength: Integer);
var
  Change: TNEDDocumentChangeInfo;
begin
  FillChar(Change, SizeOf(Change), 0);

  Change.Kind := dcUnknown;
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

procedure TNEDDocumentObserver.LineInserted(LineIndex: Integer);
begin
  if not FEnabled then
    Exit;

  if IsUpdating then
    Exit;

  DoLineInserted(LineIndex);
end;

procedure TNEDDocumentObserver.LineDeleted(LineIndex: Integer);
begin
  if not FEnabled then
    Exit;

  if IsUpdating then
    Exit;

  DoLineDeleted(LineIndex);
end;

procedure TNEDDocumentObserver.LineChanged(LineIndex: Integer);
begin
  if not FEnabled then
    Exit;

  if IsUpdating then
    Exit;

  DoLineChanged(LineIndex);
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

  FLines := TNEDStringList.Create;
  FObservers := TList<TNEDDocumentObserver>.Create;

  FUndo := TStack<TNEDEditOperation>.Create;
  FRedo := TStack<TNEDEditOperation>.Create;
  FInUndoRedo := False;

  FModified := False;
  FUpdateCount := 0;

  FLength := 0;
  FLineCount := 0;
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

procedure TNEDCustomDocument.NotifyObservers(Position, DeletedLen, InsertedLen: Integer);
var
  LineColumn: TNEDTextPosition;
begin
  if IsUpdating then
    Exit;

  if not PositionToLineColumn(Position, LineColumn) then
    Exit;

  NotifyObservers(LineColumn, DeletedLen, InsertedLen);
end;

procedure TNEDCustomDocument.NotifyObservers(const LineColumn: TNEDTextPosition; DeletedLen, InsertedLen: Integer);
var
  Observer: TNEDDocumentObserver;
begin
  if IsUpdating then
    Exit;

  for Observer in FObservers do
    Observer.DocumentChanged(LineColumn, DeletedLen, InsertedLen);
end;

procedure TNEDCustomDocument.NotifyLineInserted(LineIndex: Integer);
var
  Observer: TNEDDocumentObserver;
begin
  if IsUpdating then
    Exit;

  for Observer in FObservers do
    Observer.LineInserted(LineIndex);
end;

procedure TNEDCustomDocument.NotifyLineDeleted(LineIndex: Integer);
var
  Observer: TNEDDocumentObserver;
begin
  if IsUpdating then
    Exit;

  for Observer in FObservers do
    Observer.LineDeleted(LineIndex);
end;

procedure TNEDCustomDocument.NotifyLineChanged(LineIndex: Integer);
var
  Observer: TNEDDocumentObserver;
begin
  if IsUpdating then
    Exit;

  for Observer in FObservers do
    Observer.LineChanged(LineIndex);
end;

function TNEDCustomDocument.FindLine(Position: Integer; out LineIndex, Column: Integer): Boolean;
var
  I: Integer;
  CurrentPos: Integer;
  LineLen: Integer;
begin
  Result := False;

  LineIndex := -1;
  Column := 0;

  CurrentPos := 0;

  for I := 0 to FLines.Count - 1 do begin
    LineLen := Lines[I].Length;

    if Position <= CurrentPos + LineLen then begin
      LineIndex := I;
      Column := Position - CurrentPos;

      Exit(True);
    end;

    Inc(CurrentPos, LineLen);

    if I < FLines.Count - 1 then
      Inc(CurrentPos, 2);
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

  for I := 0 to FLines.Count - 1 do begin
    Lines[I].UpdateLength;

    Inc(FLength, Lines[I].Length);
    //
    // CRLF between lines.
    if I < FLines.Count - 1 then
      Inc(FLength, 2);
  end;

  FLineCount := FLines.Count;
end;

procedure TNEDCustomDocument.RebuildLineNumbers;
var
  I: Integer;
begin
  for I := 0 to FLines.Count - 1 do
    Lines[I].LineNo := I;
end;

procedure TNEDCustomDocument.MarkModified;
begin
  FModified := True;
end;

function TNEDCustomDocument.InternalLineLength(LineIndex: Integer): Integer;
begin

end;

function TNEDCustomDocument.CreateLine: TNEDLineProperties;
begin
  Result := TNEDLineProperties.Create;
end;

procedure TNEDCustomDocument.InsertLine(LineIndex: Integer; Line: TNEDLineProperties);
begin
  FLines.InsertObject(LineIndex, '', Line);

  RebuildLineNumbers;
  RebuildCaches;

  NotifyLineInserted(LineIndex);
end;

procedure TNEDCustomDocument.DeleteLine(LineIndex: Integer);
begin
  if (LineIndex < 0) or (LineIndex >= FLines.Count) then
    Exit;

  FLines.Objects[LineIndex].Free;
  FLines.Delete(LineIndex);

  RebuildLineNumbers;
  RebuildCaches;

  NotifyLineDeleted(LineIndex);
end;

procedure TNEDCustomDocument.MergeLines(FirstLine, SecondLine: Integer);
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
  for I := 0 to B.Pieces.Count - 1 do
    A.Pieces.Add(B.Pieces[I]);

  A.UpdateLength;

  DeleteLine(SecondLine);
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
  Piece: TNEDPiece;
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
        Piece.Buffer := pbOriginal;
        Piece.Offset := 0; //Offset;
        Piece.Length := Length(FLines[I]);

        LineObj.Pieces.Add(Piece);
      end;

      LineObj.UpdateLength;

      //FLines.AddObject(Source[I], LineObj);
      FLines.Objects[I] := LineObj;

//      Inc(Offset, Length(Source[I]));

      //
      // original file buffer contains CRLF.
//      Inc(Offset, 2);
    end;

    if FLines.Count = 0 then
      FLines.AddObject('', CreateLine);

    FModified := False;

    RebuildLineNumbers;
    RebuildCaches;
  finally
    EndUpdate;
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
  if (TopOp.Kind = opDelete) and (Op.Kind = opDelete) then begin
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
  Piece: TNEDPiece;
  AddOffset: Integer;
  Parts: TStringList;
  I: Integer;
  NewLine: TNEDLineProperties;
  UndoOp: TNEDEditOperation;
begin
  if Text = '' then
    Exit;

  // record user edit
  if not FInUndoRedo then begin
    UndoOp.Kind := opInsert;
    UndoOp.Position := TNEDTextPosition.LineColumn(Line, Column);
    UndoOp.Text := Text;
    UndoOp.Length := Length(Text);

    if not TryMergeUndo(UndoOp) then
      FUndo.Push(UndoOp);

    FRedo.Clear;
  end;

  AddOffset := Length(FInputBuffer);
  FInputBuffer := FInputBuffer + Text;

  LineProperties := Lines[Line];

  // fast path: no line breaks.
  if Pos(#10, Text) = 0 then begin
    Piece.Buffer := pbInputBuffer;
    Piece.Offset := AddOffset;
    Piece.Length := Length(Text);

    LineProperties.InsertPiece(Column, Piece);
    LineProperties.UpdateLength;

    MarkModified;
    RebuildCaches;

    NotifyObservers(TNEDTextPosition.LineColumn(Line, Column), 0, Length(Text));

    Exit;
  end;

  // multi-line insert
  Parts := TStringList.Create;
  try
    Parts.Text := StringReplace(Text, #13, '', [rfReplaceAll]);

    Piece.Buffer := pbInputBuffer;
    Piece.Offset := AddOffset;
    Piece.Length := Length(Parts[0]);

    LineProperties.InsertPiece(Column, Piece);

    Inc(AddOffset, Piece.Length + 2); // CRLF

    for I := 1 to Parts.Count - 1 do begin
      NewLine := CreateLine;

      if Parts[I] <> '' then begin
        Piece.Buffer := pbInputBuffer;
        Piece.Offset := AddOffset;
        Piece.Length := Length(Parts[I]);

        NewLine.Pieces.Add(Piece);

        Inc(AddOffset, Piece.Length + 2); // CRLF
      end;

      NewLine.UpdateLength;

      InsertLine(Line + I, NewLine);
    end;
  finally
    Parts.Free;
  end;

  MarkModified;
  RebuildCaches;

  NotifyObservers(TNEDTextPosition.LineColumn(Line, Column), 0, Length(Text));
end;

procedure TNEDCustomDocument.Delete(const Position, Count: Integer);
var
  StartLine: Integer;
  StartColumn: Integer;
begin
  if Count <= 0 then
    Exit;

  if not FindLine(Position, StartLine, StartColumn) then
    Exit;

  Delete(StartLine, StartColumn, Count);
end;

procedure TNEDCustomDocument.Delete(const LineColumn: TNEDTextPosition; const Count: Integer);
begin
  Delete(LineColumn.Line, LineColumn.Column, Count);
end;

procedure TNEDCustomDocument.Delete(const Line, Column, Count: Integer);
var
  StartLine: Integer;
  StartColumn: Integer;
  EndLine: Integer;
  EndColumn: Integer;
  UndoOp: TNEDEditOperation;
  DeletedText: String;
begin
  StartLine := Line;
  StartColumn := Column;
  if not FindLine(LineColumnToPosition(Line, Column) + Count, EndLine, EndColumn) then
    Exit;

  // capture text before deletion
  DeletedText := GetTextRange(Line, Column, Count);

  // record user edit
  if not FInUndoRedo then begin
    UndoOp.Kind := opDelete;
    UndoOp.Position := TNEDTextPosition.LineColumn(Line, Column);
    UndoOp.Text := DeletedText;
    UndoOp.Length := Length(DeletedText);

    if not TryMergeUndo(UndoOp) then
      FUndo.Push(UndoOp);

    FRedo.Clear;
  end;

  //
  // single-line deletion
  if StartLine = EndLine then begin
    Lines[StartLine].DeleteRange(StartColumn, Count);
    Lines[StartLine].UpdateLength;
  end
  else begin
    // remove tail from first line
    Lines[StartLine].DeleteRange(StartColumn, Lines[StartLine].Length - StartColumn);

    //
    // remove head from last line
    Lines[EndLine].DeleteRange(0, EndColumn);

    //
    // merge both remaining fragments
    MergeLines(StartLine, EndLine);

    //
    // delete lines in between
    while FLines.Count > StartLine + 1 do begin
      if StartLine + 1 >= EndLine then
        Break;

      DeleteLine(StartLine + 1);
      Dec(EndLine);
    end;
  end;

  MarkModified;
  RebuildCaches;

  NotifyObservers(TNEDTextPosition.LineColumn(Line, Column), Count, 0);
end;

procedure TNEDCustomDocument.Replace(const Position, Count: Integer; const Text: String);
begin
  Delete(Position, Count);
  Insert(Position, Text);
end;

procedure TNEDCustomDocument.Replace(const LineColumn: TNEDTextPosition; const Count: Integer; const Text: String);
begin
  Delete(LineColumn, Count);
  Insert(LineColumn, Text);
end;

procedure TNEDCustomDocument.Replace(const Line, Column, Count: Integer; const Text: String);
begin
  Delete(Line, Column, Count);
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
      opInsert: Delete(LineColumnToPosition(Op.Position), Length(Op.Text));
      opDelete: Insert(LineColumnToPosition(Op.Position), Op.Text);
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
      opDelete: Delete(LineColumnToPosition(Op.Position), Length(Op.Text));
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
  I: Integer;
begin
  SB := TStringBuilder.Create;
  try
    for I := 0 to FLines.Count - 1 do begin
      SB.Append(GetLineText(I));

      if I < FLines.Count - 1 then
        SB.Append(#13#10);
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
    Result := Copy(S, Column + 1, Length(S) - Column) + #13#10; // CRLF
    Dec(Count, Length(Result) + 2);
    Inc(Line);
    while (Count > 0) and (Line < LineCount) do begin
      S := GetLineText(Line);
      if Length(S) > Count then begin
        Result := Result + Copy(S, 1, Count);
        Break;
      end
      else begin
        Result := Result + S + #13#10; // CRLF
        Dec(Count, Length(S) + 2);
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
  Piece: TNEDPiece;
  I: Integer;
  SB: TStringBuilder;
begin
  Result := '';

  if (LineIndex < 0) or (LineIndex >= FLines.Count) then
    Exit;

  Line := Lines[LineIndex];

  SB := TStringBuilder.Create;
  try
    for I := 0 to Line.Pieces.Count - 1 do begin
      Piece := Line.Pieces[I];
      case Piece.Buffer of
        pbOriginal   : SB.Append(Copy(GetOriginalLineText(I), Piece.Offset + 1, Piece.Length));
        pbInputBuffer: SB.Append(Copy(FInputBuffer, Piece.Offset + 1, Piece.Length));
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
    Inc(Result, Lines[I].Length);

    if I < FLines.Count - 1 then
      Inc(Result, 2);
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

function TNEDCustomDocument.LastLine: Integer;
begin
  Result := FLineCount - 1;
end;

function TNEDCustomDocument.IsModified: Boolean;
begin
  Result := FModified;
end;

end.

