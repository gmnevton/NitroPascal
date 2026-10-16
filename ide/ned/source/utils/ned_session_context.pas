//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-10
// All rights reserved.
//

unit ned_session_context;

interface

uses
  SysUtils,
  Classes,
  Graphics,
  Generics.Collections,
  JSON.VerySimple,
  ned_projects,
  ned_editor_types;

type
  TNEDLayout = class
  private
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Reset;
  end;

//  TNEDEditorFileType = class
//
//  end;

//  TFontCharset = 0..255;
//  TFontPitch = (fpDefault, fpVariable, fpFixed);
//  TFontQuality = (fqDefault, fqDraft, fqProof, fqNonAntialiased, fqAntialiased, fqClearType, fqClearTypeNatural);
//  TFontStyle = (fsBold, fsItalic, fsUnderline, fsStrikeOut);

  TNEDEditorSettings = class
  private
    //FFileTypes: TDictionary<String, TNEDEditorFileType>;
    FBackgroundColor: TColor;
    FFontName: UTF8String;
    FFontSize: Integer;
    FFontStyle: TFontStyles;
    FFontColor: TColor;
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Reset;
    function Read(const ARoot: TJSONNode): Boolean;
    //
    property BackgroundColor: TColor read FBackgroundColor;
    property FontName: UTF8String read FFontName;
    property FontSize: Integer read FFontSize;
    property FontStyle: TFontStyles read FFontStyle;
    property FontColor: TColor read FFontColor;
  end;

  TNEDEditorHighlightSettings = class
  private
    FName: UTF8String;
    FDescription: UTF8String;
    FFileExtensions: TStringList;
    //
    FLineBreak: UTF8String; // CRLF or LF or CR
    FWhitespaceCharSet: UTF8String; // ' ', #9, #10, #13
    FIdentifierRuleSet: TNEDCharSetRule;
    FNumberRuleSet: TNEDCharSetRule;
    FStringDelimiter: UTF8Char; // " or '
    FAllowStringDelimiterNesting: Boolean;
    FNestedStringDelimiterEscapeing: Boolean;
    //
    FCommentsDelimiter: Array of TNEDEditorDelimiterPair; // see the compiler\concepts\comment\comment.npc file for detail explanation
    FDirectiveDelimiter: TNEDEditorDelimiterPair; // '{$' - directive begin; '}' directive end
    //FCompilerVariable: TNEDEditorDelimiterPair; // %project-name%
    //
    FAllowNestedComments: Boolean;
    FAllowMultilineStrings: Boolean;
    FAllowMultilineComments: Boolean;
    //
    //
    FKeywords: TStringList;
    FOperators: TStringList;
    FPunctuation: TStringList;
    //
    FBooleanLiterals: TStringList;
    FNilLiterals: TStringList;
    //
    //
    FHexPrefix: UTF8String; // 0xABCD, $ABCD
    FHexSuffix: UTF8String; // 0ABCDH or 0ABCDh - assembler
    FBinaryPrefix: UTF8String; // 0b01010101
    FBinarySuffix: UTF8String; // 01010101b
    FOctalPrefix: UTF8String; // 0o01234567
    FOctalSuffix: UTF8String; // 01234567o
    FExtendedSuffix: UTF8String; // 1.123e4 or 1.123E4
    //
    FDecimalSeparator: UTF8Char; // 12345.6789
    FAllowUnderscoreNumbers: Boolean; // eg.: 123_456_789
    //
    FCaseSensitive: Boolean;
  private
    function GetCommentsDelimitersCount: Integer;
    function GetCommentsDelimiters(const Index: Integer): TNEDEditorDelimiterPair;
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Reset;
    function Read(const ARoot: TJSONNode): Boolean;
    //
    property Name: UTF8String read FName;
    property Description: UTF8String read FDescription;
    property FileExtensions: TStringList read FFileExtensions;
    //
    property LineBreak: UTF8String read FLineBreak; // CRLF or LF or CR
    property WhitespaceCharSet: UTF8String read FWhitespaceCharSet; // ' ', #9, #10, #13
    property IdentifierRuleSet: TNEDCharSetRule read FIdentifierRuleSet;
    property NumberRuleSet: TNEDCharSetRule read FNumberRuleSet;
    property StringDelimiter: UTF8Char read FStringDelimiter; // " or '
    property AllowStringDelimiterNesting: Boolean read FAllowStringDelimiterNesting;
    property NestedStringDelimiterEscapeing: Boolean read FNestedStringDelimiterEscapeing;
    //
//    property FCommentsDelimiter: Array of TNEDEditorDelimiterPair; // see the compiler\concepts\comment\comment.npc file for detail explanation
    property CommentsDelimitersCount: Integer read GetCommentsDelimitersCount;
    property CommentsDelimiters[const Index: Integer]: TNEDEditorDelimiterPair read GetCommentsDelimiters;
    property DirectiveDelimiter: TNEDEditorDelimiterPair read FDirectiveDelimiter; // '{$' - directive begin; '}' directive end
    //
    property AllowNestedComments: Boolean read FAllowNestedComments;
    property AllowMultilineStrings: Boolean read FAllowMultilineStrings;
    property AllowMultilineComments: Boolean read FAllowMultilineComments;
    //
    //
    property Keywords: TStringList read FKeywords;
    property Operators: TStringList read FOperators;
    property Punctuation: TStringList read FPunctuation;
    //
    property BooleanLiterals: TStringList read FBooleanLiterals;
    property NilLiterals: TStringList read FNilLiterals;
    //
    //
    property HexPrefix: UTF8String read FHexPrefix; // 0xABCD, $ABCD
    property HexSuffix: UTF8String read FHexSuffix; // 0ABCDH or 0ABCDh - assembler
    property BinaryPrefix: UTF8String read FBinaryPrefix; // 0b01010101
    property BinarySuffix: UTF8String read FBinarySuffix; // 01010101b
    property OctalPrefix: UTF8String read FOctalPrefix; // 0o01234567
    property OctalSuffix: UTF8String read FOctalSuffix; // 01234567o
    property ExtendedSuffix: UTF8String read FExtendedSuffix; // 1.123e4 or 1.123E4
    //
    property DecimalSeparator: UTF8Char read FDecimalSeparator; // 12345.6789
    property AllowUnderscoreNumbers: Boolean read FAllowUnderscoreNumbers; // eg.: 123_456_789
    //
    property CaseSensitive: Boolean read FCaseSensitive;
  end;

  TNEDHighlighterProfiles = class
  private
    FLanguageSettings: TDictionary<String, TNEDEditorHighlightSettings>;
    //
    function GetLanguagesCount: Integer;
    function GetLanguages(const AIndex: Integer): TNEDEditorHighlightSettings; overload;
    function GetLanguages(const AName: String): TNEDEditorHighlightSettings; overload;
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Reset;
    function Read(const ARoot: TJSONNode; out AErrorMsg: String): Boolean;
    function Save: TJSONNode;
    //
    function HasFileType(const AName: String): Boolean;
    //
    property LanguagsCount: Integer read GetLanguagesCount;
    property LanguagesByIndex[const Index: Integer]: TNEDEditorHighlightSettings read GetLanguages;
    property LanguagesByName[const Name: String]: TNEDEditorHighlightSettings read GetLanguages;
  end;

  TNEDSettingsModeEnum = (
    smDark,
    smLight
  );

  TNEDSettingsStructure = record
    EditorSettings: TNEDEditorSettings;
    HighlighterProfiles: TNEDHighlighterProfiles;
  end;

  TNEDSettings = class
  private
    FDarkSettings: TNEDSettingsStructure;
    FLightSettings: TNEDSettingsStructure;
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Reset;
    function Read(const ARoot: TJSONNode; out AErrorMsg: String): Boolean;
    function Save: TJSONNode;
    //
    property DarkSettings: TNEDSettingsStructure read FDarkSettings;
    property LightSettings: TNEDSettingsStructure read FLightSettings;
  end;

  TNEDSessionContext = class
  private
    FContextID: String; // same as TNEDProfile.ID
    FName: String; // same as TNEDProfile.Name
    FCopiedFrom: String; // same as TNEDProfile.OriginateFrom
    FCreateDate: TDateTime;
    FModifyDate: TDateTime;
    FLastOpenedDate: TDateTime;
    FTimesOpened: Integer;
    FTimeUsedMinutes: Integer;
    //
    FStorage: TJSONVerySimple;
    FStorageFileName: String;
    FLoaded: Boolean;
    FError: Boolean;
    FErrorMsg: String;
  private
    FProjects: TObjectList<TNEDProject>;
    FFavorites: TObjectList<TNEDProject>;
    FRecents: TObjectList<TNEDProject>;
    FLayout: TNEDLayout;
    FSettings: TNEDSettings;
    //
    function GetFavorite(const AIndex: Integer): TNEDProject;
    function GetRecent(const AIndex: Integer): TNEDProject;
    function GetProject(const AIndex: Integer): TNEDProject;
  private
    procedure ReadProfileHeader;
    procedure ReadContext;
    procedure ReadProjects(const ARootNode: TJSONNode);
    procedure ReadFavorites(const ARootNode: TJSONNode);
    procedure ReadRecents(const ARootNode: TJSONNode);
    procedure ReadWorkspace;
    procedure ReadSettings(const ARootNode: TJSONNode);
    procedure ReadProject(const ANode: TJSONNode; const AList: TObjectList<TNEDProject>);
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure ContextClear;
    function ContextFavoritesCount: Integer;
    function ContextRecentsCount: Integer;
    function ContextAdd(const AProjectID, AName: String): TNEDProject;
    function ContextAddNew(const AName: String): TNEDProject;
    function ContextRemove(const AIndex: Integer): Boolean;
    function ContextMove(const AFromIndex, AToIndex: Integer): Boolean;
    function FindProject(const AType: TNEDProjectTypeEnum; const AID: String): TNEDProject;
    function FindFavorite(const AType: TNEDProjectTypeEnum; const AFilePath: String): TNEDProject;
    function FindRecent(const AType: TNEDProjectTypeEnum; const AFilePath: String): TNEDProject;
    function FindRecentFile(const AFilePath: String): TNEDProject;
    //
    procedure LoadContext(const AProfilePath: String); // read profile favorites and recents, IDE layout and settings
    procedure LoadSession(const ASessionPath: String); // read what was opened
    procedure LoadProject(const AProjectPath: String); // read opened projects
    procedure LoadFile(const AFilePath: String); // read opened file that was not part of a project
    //
    property ContextID: String read FContextID;
    property Name: String read FName;
    property OriginateFrom: String read FCopiedFrom;
    property CreateDate: TDateTime read FCreateDate;
    property ModifyDate: TDateTime read FModifyDate;
    property LastOpenedDate: TDateTime read FLastOpenedDate;
    property TimesOpened: Integer read FTimesOpened;
    property TimeUsedMinutes: Integer read FTimeUsedMinutes;
    //
    property Loaded: Boolean read FLoaded;
    property Error: Boolean read FError;
    property ErrorMsg: String read FErrorMsg;
    //
    property Favorite[const AIndex: Integer]: TNEDProject read GetFavorite;
    property Recent[const AIndex: Integer]: TNEDProject read GetRecent;
    property Project[const AIndex: Integer]: TNEDProject read GetProject;
    property Settings: TNEDSettings read FSettings;
  end;

implementation

uses
  ned_json_config_utils,
  ned_common_simple_types;

{ TNEDLayout }

constructor TNEDLayout.Create;
begin

end;

destructor TNEDLayout.Destroy;
begin

  inherited;
end;

procedure TNEDLayout.Reset;
begin

end;

{ TNEDEditorSettings }

constructor TNEDEditorSettings.Create;
begin
  inherited Create;
  //
  Reset;
end;

destructor TNEDEditorSettings.Destroy;
begin
  inherited;
end;

procedure TNEDEditorSettings.Reset;
begin
  FBackgroundColor := $00330000;
  FFontName := 'Source Code Pro Semibold';
  FFontSize := 12;
  FFontStyle := [];
  FFontColor := clWhite;
end;

function TNEDEditorSettings.Read(const ARoot: TJSONNode): Boolean;
var
  Root, Node: TJSONNode;
  idx: Integer;
begin
  Result := False;
  Reset;
  if ARoot = Nil then
    Exit;
  //
  Root := ARoot.FindNode('EditorSettings', [jtObject]);
  if Root <> Nil then begin
    FBackgroundColor := StringToColor(ARoot.Document.NodeAsString(Root, 'BackgroundColor', '$00000000'));
    Node := Root.FindNode('Font', [jtObject]);
    if Node <> Nil then begin
      FFontName := ARoot.Document.NodeAsString(Node, 'Name', 'Source Code Pro Semibold');
      FFontSize := ARoot.Document.NodeAsInteger(Node, 'Size', 12);
      FFontStyle := StringToFontStyles(ARoot.Document.NodeAsString(Node, 'Style', ''));
      FFontColor := StringToColor(ARoot.Document.NodeAsString(Node, 'Color', 'clWhite'));
    end;
  end;
end;

{ TNEDEditorHighlightSettings }

constructor TNEDEditorHighlightSettings.Create;
begin
  inherited Create;
  //
  FFileExtensions := TStringList.Create;
  //
  SetLength(FCommentsDelimiter, 0);
  //
  FKeywords := TStringList.Create;
  FOperators := TStringList.Create;
  FPunctuation := TStringList.Create;
  //
  FBooleanLiterals := TStringList.Create;
  FNilLiterals := TStringList.Create;
  //
  Reset;
end;

destructor TNEDEditorHighlightSettings.Destroy;
var
  i: Integer;
begin
  FName := '';
  FDescription := '';
  FFileExtensions.Free;
  //
  FLineBreak := '';
  FWhitespaceCharSet := '';
  FStringDelimiter := #0;
  //
  for i := 0 to High(FCommentsDelimiter) do begin
    FCommentsDelimiter[i].StartDelimiter := '';
    FCommentsDelimiter[i].EndDelimiter := '';
  end;
  SetLength(FCommentsDelimiter, 0);
  FDirectiveDelimiter.StartDelimiter := '';
  FDirectiveDelimiter.EndDelimiter := '';
  //
  //
  FKeywords.Free;
  FOperators.Free;
  FPunctuation.Free;
  //
  FBooleanLiterals.Free;
  FNilLiterals.Free;
  //
  FHexPrefix := '';
  FHexSuffix := '';
  FBinaryPrefix := '';
  FBinarySuffix := '';
  FOctalPrefix := '';
  FOctalSuffix := '';
  FExtendedSuffix := '';
  //
  FDecimalSeparator := #0;
  inherited;
end;

function TNEDEditorHighlightSettings.GetCommentsDelimitersCount: Integer;
begin
  Result := Length(FCommentsDelimiter);
end;

function TNEDEditorHighlightSettings.GetCommentsDelimiters(const Index: Integer): TNEDEditorDelimiterPair;
begin
  FillChar(Result, SizeOf(TNEDEditorDelimiterPair), 0);
  if (Index < 0) or (Index > High(FCommentsDelimiter)) then
    Exit;
  //
  Result := FCommentsDelimiter[Index];
end;

procedure TNEDEditorHighlightSettings.Reset;
var
  i: Integer;
begin
  FName := '';
  FDescription := '';
  FFileExtensions.Clear;
  //
  FLineBreak := ''; // CRLF or LF or CR
  FWhitespaceCharSet := ' '#9#10#13;
  FIdentifierRuleSet.CharSet := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_';
  FIdentifierRuleSet.UnderscoreStart := True;
  FIdentifierRuleSet.NumberStart := False;
  FIdentifierRuleSet.CanContainDash := False;
  FStringDelimiter := #0; // " or '
  FAllowStringDelimiterNesting := False;
  FNestedStringDelimiterEscapeing := False;
  //
  for i := 0 to High(FCommentsDelimiter) do begin
    FCommentsDelimiter[i].StartDelimiter := '';
    FCommentsDelimiter[i].EndDelimiter := '';
  end;
  SetLength(FCommentsDelimiter, 0);
  FDirectiveDelimiter.&Type := ectNotInComment; // '{$' - directive begin; '}' directive end
  FDirectiveDelimiter.StartDelimiter := '';
  FDirectiveDelimiter.EndDelimiter := '';
  FDirectiveDelimiter.CanNest := False;
  FDirectiveDelimiter.CanBeNested := False;
  //
  FAllowNestedComments := False;
  FAllowMultilineStrings := False;
  FAllowMultilineComments := False;
  //
  //
  FKeywords.Clear;
  FOperators.Clear;
  FPunctuation.Clear;
  //
  FBooleanLiterals.Clear;
  FNilLiterals.Clear;
  //
  FHexPrefix := '0x'; // 0xABCD, $ABCD
  FHexSuffix := 'h'; // 0ABCDH or 0ABCDh - assembler
  FBinaryPrefix := '0b'; // 0b01010101
  FBinarySuffix := 'b'; // 01010101b
  FOctalPrefix := '0o'; // 0o01234567
  FOctalSuffix := 'o'; // 01234567o
  FExtendedSuffix := 'e'; // 1.123e4 or 1.123E4
  //
  FDecimalSeparator := '.'; // 12345.6789
  FAllowUnderscoreNumbers := False; // eg.: 123_456_789
  //
  FCaseSensitive := False;
end;

function TNEDEditorHighlightSettings.Read(const ARoot: TJSONNode): Boolean;
var
  Root, Node: TJSONNode;
  idx: Integer;
begin
  Result := False;
  Reset;
  if ARoot = Nil then
    Exit;
  //
  FName := ARoot.Document.NodeAsString(ARoot, 'Name', '');
  FDescription := ARoot.Document.NodeAsString(ARoot, 'Description', '');
  Node := ARoot.FindNode('FileExtensions', [jtArray]);
  if Node <> Nil then begin
    Node := Node.FirstChild;
    while Node <> Nil do begin
      FFileExtensions.Add(Node.NodeValue);
      Node := Node.NextSibling;
    end;
  end;
  //
  FLineBreak := ARoot.Document.NodeAsString(ARoot, 'LineBreak', 'CRLF'); // CRLF or LF or CR
  FWhitespaceCharSet := ARoot.Document.NodeAsString(ARoot, 'WhitespaceCharSet', ' '#9#10#13);
  Root := ARoot.FindNode('IdentifierRuleSet', [jtObject]);
  if Root <> Nil then begin
    FIdentifierRuleSet.CharSet := ARoot.Document.NodeAsString(Root, 'CharSet', 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_');
    FIdentifierRuleSet.UnderscoreStart := ARoot.Document.NodeAsBoolean(Root, 'UnderscoreStart', True);
    FIdentifierRuleSet.NumberStart := ARoot.Document.NodeAsBoolean(Root, 'NumberStart', False);
    FIdentifierRuleSet.CanContainDash := ARoot.Document.NodeAsBoolean(Root, 'CanContainDash', False);
  end;
  Root := ARoot.FindNode('NumberRuleSet', [jtObject]);
  if Root <> Nil then begin
    FNumberRuleSet.CharSet := ARoot.Document.NodeAsString(Root, 'CharSet', '0123456789._xboe+-');
    FNumberRuleSet.UnderscoreStart := ARoot.Document.NodeAsBoolean(Root, 'UnderscoreStart', False);
    FNumberRuleSet.NumberStart := ARoot.Document.NodeAsBoolean(Root, 'NumberStart', True);
    FNumberRuleSet.CanContainDash := ARoot.Document.NodeAsBoolean(Root, 'CanContainDash', False);
  end;
  FStringDelimiter := UTF8Char(ARoot.Document.NodeAsChar(ARoot, 'StringDelimiter', #39)); // #34 - " or #39 - '
  FAllowStringDelimiterNesting := ARoot.Document.NodeAsBoolean(ARoot, 'AllowStringDelimiterNesting', False);
  FNestedStringDelimiterEscapeing := ARoot.Document.NodeAsBoolean(ARoot, 'NestedStringDelimiterEscapeing', False);
  //
  Root := ARoot.FindNode('CommentsDelimiter', [jtArray]);
  if Root <> Nil then begin
    Root := Root.FirstChild;
    while Root <> Nil do begin
      idx := Length(FCommentsDelimiter);
      SetLength(FCommentsDelimiter, idx + 1);
      //
      FCommentsDelimiter[idx].&Type := TNEDEditorDelimiterPair.StringToEditorCommentType(ARoot.Document.NodeAsString(Root, 'Type', ''));
      FCommentsDelimiter[idx].StartDelimiter := ARoot.Document.NodeAsString(Root, 'StartDelimiter', '');
      FCommentsDelimiter[idx].EndDelimiter := ARoot.Document.NodeAsString(Root, 'EndDelimiter', '');
      FCommentsDelimiter[idx].CanNest := ARoot.Document.NodeAsBoolean(Root, 'CanNest', False);
      FCommentsDelimiter[idx].CanBeNested := ARoot.Document.NodeAsBoolean(Root, 'CanBeNested', False);
      //
      Root := Root.NextSibling;
    end;
  end;
  Root := ARoot.FindNode('DirectiveDelimiter', [jtObject]);
  if Root <> Nil then begin
    FDirectiveDelimiter.&Type := TNEDEditorDelimiterPair.StringToEditorCommentType(ARoot.Document.NodeAsString(Root, 'Type', ''));
    FDirectiveDelimiter.StartDelimiter := ARoot.Document.NodeAsString(Root, 'StartDelimiter', '');
    FDirectiveDelimiter.EndDelimiter := ARoot.Document.NodeAsString(Root, 'EndDelimiter', '');
    FDirectiveDelimiter.CanNest := ARoot.Document.NodeAsBoolean(Root, 'CanNest', False);
    FDirectiveDelimiter.CanBeNested := ARoot.Document.NodeAsBoolean(Root, 'CanBeNested', False);
  end;
  //
  FAllowNestedComments := ARoot.Document.NodeAsBoolean(ARoot, 'AllowNestedComments', False);
  FAllowMultilineStrings := ARoot.Document.NodeAsBoolean(ARoot, 'AllowMultilineStrings', False);
  FAllowMultilineComments := ARoot.Document.NodeAsBoolean(ARoot, 'AllowMultilineComments', False);
  //
  //
  Root := ARoot.FindNode('Keywords', [jtArray]);
  if Root <> Nil then begin
    Root := Root.FirstChild;
    while Root <> Nil do begin
      FKeywords.Add(Root.NodeValue);
      //
      Root := Root.NextSibling;
    end;
  end;
  Root := ARoot.FindNode('Operators', [jtArray]);
  if Root <> Nil then begin
    Root := Root.FirstChild;
    while Root <> Nil do begin
      FOperators.Add(Root.NodeValue);
      //
      Root := Root.NextSibling;
    end;
  end;
  Root := ARoot.FindNode('Punctuation', [jtArray]);
  if Root <> Nil then begin
    Root := Root.FirstChild;
    while Root <> Nil do begin
      FPunctuation.Add(Root.NodeValue);
      //
      Root := Root.NextSibling;
    end;
  end;
  //
  Root := ARoot.FindNode('BooleanLiterals', [jtArray]);
  if Root <> Nil then begin
    Root := Root.FirstChild;
    while Root <> Nil do begin
      FBooleanLiterals.Add(Root.NodeValue);
      //
      Root := Root.NextSibling;
    end;
  end;
  Root := ARoot.FindNode('NilLiterals', [jtArray]);
  if Root <> Nil then begin
    Root := Root.FirstChild;
    while Root <> Nil do begin
      FNilLiterals.Add(Root.NodeValue);
      //
      Root := Root.NextSibling;
    end;
  end;
  //
  FHexPrefix := ARoot.Document.NodeAsString(ARoot, 'HexPrefix', '0x'); // 0xABCD, $ABCD
  FHexSuffix := ARoot.Document.NodeAsString(ARoot, 'HexSuffix', 'h'); // 0ABCDH or 0ABCDh - assembler
  FBinaryPrefix := ARoot.Document.NodeAsString(ARoot, 'BinaryPrefix', '0b'); // 0b01010101
  FBinarySuffix := ARoot.Document.NodeAsString(ARoot, 'BinarySuffix', 'b'); // 01010101b
  FOctalPrefix := ARoot.Document.NodeAsString(ARoot, 'OctalPrefix', '0o'); // 0o01234567
  FOctalSuffix := ARoot.Document.NodeAsString(ARoot, 'OctalSuffix', 'o'); // 01234567o
  FExtendedSuffix := ARoot.Document.NodeAsString(ARoot, 'ExtendedSuffix', 'e'); // 1.123e4 or 1.123E4
  //
  FDecimalSeparator := UTF8Char(ARoot.Document.NodeAsChar(ARoot, 'DecimalSeparator', #46)); // 12345.6789
  FAllowUnderscoreNumbers := ARoot.Document.NodeAsBoolean(ARoot, 'AllowUnderscoreNumbers', False); // eg.: 123_456_789
  //
  FCaseSensitive := ARoot.Document.NodeAsBoolean(ARoot, 'CaseSensitive', False);
end;

{ TNEDHighlighterProfiles }

constructor TNEDHighlighterProfiles.Create;
begin
  inherited Create;
  FLanguageSettings := TDictionary<String, TNEDEditorHighlightSettings>.Create;
end;

destructor TNEDHighlighterProfiles.Destroy;
begin
  FLanguageSettings.Free;
  inherited;
end;

function TNEDHighlighterProfiles.GetLanguagesCount: Integer;
begin
  Result := FLanguageSettings.Count;
end;

function TNEDHighlighterProfiles.GetLanguages(const AIndex: Integer): TNEDEditorHighlightSettings;
var
  Values: TArray<TNEDEditorHighlightSettings>;
begin
  Result := Nil;
  Values := FLanguageSettings.Values.ToArray;
  if (AIndex > -1) and (AIndex < Length(Values)) then
    Result := Values[AIndex];
end;

function TNEDHighlighterProfiles.GetLanguages(const AName: String): TNEDEditorHighlightSettings;
begin
  if not FLanguageSettings.TryGetValue(AName, Result) then
    Result := Nil;
end;

procedure TNEDHighlighterProfiles.Reset;
begin
  FLanguageSettings.Clear;
end;

function TNEDHighlighterProfiles.Read(const ARoot: TJSONNode; out AErrorMsg: String): Boolean;
var
  HighlighterProfiles, List, Node: TJSONNode;
  LCount: Integer;
  HighlightSettings: TNEDEditorHighlightSettings;
begin
  Result := False;
  AErrorMsg := '';
  Reset;
  if ARoot = Nil then begin
    AErrorMsg := 'EditorSettings can not be loaded, config "Settings" entry not found !';
    Exit;
  end;

  HighlighterProfiles := ARoot.FindNode('HighlighterProfiles', [jtObject]);
  if HighlighterProfiles <> Nil then begin
    //
    LCount := ARoot.Document.NodeAsInteger(HighlighterProfiles, 'Count', 0);
    List := HighlighterProfiles.FindNode('List', [jtObject]);
    if List <> Nil then begin
      if List.HasChildNodes and (List.ChildNodes.Count <> LCount) then begin // list not complete
        AErrorMsg := Trim(AErrorMsg + #13#10 + Format('HighlighterProfiles list elements count mismatch, expected: %d, but got: %d.', [LCount, List.ChildNodes.Count]));
        //
        Exit;
      end;
      //
      Node := List.FirstChild;
      while Node <> Nil do begin
        HighlightSettings := TNEDEditorHighlightSettings.Create;
        HighlightSettings.Read(Node);
        FLanguageSettings.Add(Node.NodeName, HighlightSettings);
        //
        Node := Node.NextSibling;
      end;
      //
      Result := True;
    end;
  end
  else
    AErrorMsg := 'HighlighterProfiles object entry can not be found !';
end;

function TNEDHighlighterProfiles.Save: TJSONNode;
begin

end;

function TNEDHighlighterProfiles.HasFileType(const AName: String): Boolean;
begin
  Result := FLanguageSettings.ContainsKey(AName);
end;

{ TNEDSettings }

constructor TNEDSettings.Create;
begin
  inherited Create;
  //
  FDarkSettings.EditorSettings := TNEDEditorSettings.Create;
  FDarkSettings.HighlighterProfiles := TNEDHighlighterProfiles.Create;
  //
  FLightSettings.EditorSettings := TNEDEditorSettings.Create;
  FLightSettings.HighlighterProfiles := TNEDHighlighterProfiles.Create;
end;

destructor TNEDSettings.Destroy;
begin
  FDarkSettings.EditorSettings.Free;
  FDarkSettings.HighlighterProfiles.Free;
  //
  FLightSettings.EditorSettings.Free;
  FLightSettings.HighlighterProfiles.Free;
  inherited;
end;

procedure TNEDSettings.Reset;
begin
  FDarkSettings.EditorSettings.Reset;
  FDarkSettings.HighlighterProfiles.Reset;
  //
  FLightSettings.EditorSettings.Reset;
  FLightSettings.HighlighterProfiles.Reset;
end;

function TNEDSettings.Read(const ARoot: TJSONNode; out AErrorMsg: String): Boolean;
var
  Dark, Light: TJSONNode;
begin
  Result := False;
  AErrorMsg := '';
  if ARoot = Nil then begin
    AErrorMsg := 'EditorSettings can not be loaded, config "Settings" entry not found !';
    Exit;
  end;

  Dark := ARoot.FindNode('DarkMode', [jtObject]);
  if not FDarkSettings.EditorSettings.Read(Dark) then begin
    AErrorMsg := AErrorMsg + #13#10 + 'EditorSettings for DarkMode can not be loaded, config "Settings.DarkMode" entry not found or incomplete !';
  end;
  if not FDarkSettings.HighlighterProfiles.Read(Dark, AErrorMsg) then begin
    AErrorMsg := AErrorMsg + #13#10 + 'HighlighterProfiles for DarkMode can not be loaded, config "Settings.DarkMode" entry not found or incomplete !';
  end;

  Light := ARoot.FindNode('LightMode', [jtObject]);
  if not FLightSettings.EditorSettings.Read(Light) then begin
    AErrorMsg := AErrorMsg + #13#10 + 'EditorSettings for LightMode can not be loaded, config "Settings.LightMode" entry not found or incomplete !';
  end;
  if not FLightSettings.HighlighterProfiles.Read(Light, AErrorMsg) then begin
    AErrorMsg := AErrorMsg + #13#10 + 'HighlighterProfiles for LightMode can not be loaded, config "Settings.LightMode" entry not found or incomplete !';
  end;
end;

function TNEDSettings.Save: TJSONNode;
begin

end;

{ TNEDSessionContext }

constructor TNEDSessionContext.Create;
begin
  FStorage := TJSONVerySimple.Create;
  FStorageFileName := '';
  FLoaded := False;
  FError := False;
  FErrorMsg := '';
  //
  FProjects := TObjectList<TNEDProject>.Create(True);
  FFavorites := TObjectList<TNEDProject>.Create(False);
  FRecents := TObjectList<TNEDProject>.Create(False);
  FLayout := TNEDLayout.Create;
  FSettings := TNEDSettings.Create;
end;

destructor TNEDSessionContext.Destroy;
begin
  FErrorMsg := '';
  //
  FSettings.Free;
  FLayout.Free;
  FRecents.Free;
  FFavorites.Free;
  FProjects.Free;
  FStorageFileName := '';
  FStorage.Free;
  inherited;
end;

function TNEDSessionContext.GetFavorite(const AIndex: Integer): TNEDProject;
begin
  Result := Nil;
  if (FFavorites.Count > 0) and (AIndex >= 0) and (AIndex < FFavorites.Count) then
    Result := FFavorites.Items[AIndex];
end;

function TNEDSessionContext.GetRecent(const AIndex: Integer): TNEDProject;
begin
  Result := Nil;
  if (FRecents.Count > 0) and (AIndex >= 0) and (AIndex < FRecents.Count) then
    Result := FRecents.Items[AIndex];
end;

function TNEDSessionContext.GetProject(const AIndex: Integer): TNEDProject;
begin
  Result := Nil;
  if (FProjects.Count > 0) and (AIndex >= 0) and (AIndex < FProjects.Count) then
    Result := FProjects.Items[AIndex];
end;

procedure TNEDSessionContext.ReadProfileHeader;
var
  DocRoot: TJSONNode;
begin
  DocRoot := FStorage.DocumentElement;
  if DocRoot <> Nil then begin
    FContextID := FStorage.NodeAsString(DocRoot, 'ID', '');
    FName := FStorage.NodeAsString(DocRoot, 'Name', '');
    FCopiedFrom := FStorage.NodeAsString(DocRoot, 'OriginateFrom', '');
    FCreateDate := FStorage.NodeAsInteger(DocRoot, 'CreateDate', 0);
    FModifyDate := FStorage.NodeAsInteger(DocRoot, 'ModifyDate', 0);
    FLastOpenedDate := FStorage.NodeAsInteger(DocRoot, 'LastOpenedDate', 0);
    FTimesOpened := FStorage.NodeAsInteger(DocRoot, 'TimesOpened', 0);
    FTimeUsedMinutes := FStorage.NodeAsInteger(DocRoot, 'TimeUsedMinutes', 0);
  end;
end;

procedure TNEDSessionContext.ReadContext;
var
  DocRoot, Root, SubRoot: TJSONNode;
begin
  DocRoot := FStorage.DocumentElement;
  if DocRoot <> Nil then begin
    Root := DocRoot.FindNode('Context', [jtObject]);
    if Root <> Nil then begin
      SubRoot := Root.FindNode('Projects', [jtObject]);
      ReadProjects(SubRoot);
      //
      SubRoot := Root.FindNode('Favorites', [jtObject]);
      ReadFavorites(SubRoot);
      //
      SubRoot := Root.FindNode('Recents', [jtObject]);
      ReadRecents(SubRoot);
      //
      SubRoot := Root.FindNode('Layout', [jtObject]);
      //
      SubRoot := Root.FindNode('Settings', [jtObject]);
      ReadSettings(SubRoot);
    end;
  end;
end;

procedure TNEDSessionContext.ReadProjects(const ARootNode: TJSONNode);
var
  List, Node: TJSONNode;
  LCount: Integer;
begin
  if ARootNode <> Nil then begin
    LCount := FStorage.NodeAsInteger(ARootNode, 'Count', 0);
    List := ARootNode.FindNode('List', [jtArray]);
    if List <> Nil then begin
      if List.HasChildNodes and (List.ChildNodes.Count <> LCount) then begin // list not complete
        FError := True;
        FErrorMsg := Trim(FErrorMsg + #13#10 + Format('Projects list elements count mismatch, expected: %d, but got: %d.', [LCount, List.ChildNodes.Count]));
        //
        Exit;
      end;
      //
      Node := List.FirstChild;
      while Node <> Nil do begin
        ReadProject(Node, FProjects);
        Node := Node.NextSibling;
      end;
    end;
  end;
end;

procedure TNEDSessionContext.ReadFavorites(const ARootNode: TJSONNode);
var
  List, Node: TJSONNode;
  LCount: Integer;
  Project: TNEDProject;
begin
  if ARootNode <> Nil then begin
    LCount := FStorage.NodeAsInteger(ARootNode, 'Count', 0);
    List := ARootNode.FindNode('List', [jtObject]);
    if List <> Nil then begin
      if List.HasChildNodes and (List.ChildNodes.Count <> LCount) then begin // list not complete
        FError := True;
        FErrorMsg := Trim(FErrorMsg + #13#10 + Format('Favorites list elements count mismatch, expected: %d, but got: %d.', [LCount, List.ChildNodes.Count]));
        //
        Exit;
      end;
      //
      Node := List.FirstChild;
      while Node <> Nil do begin
        if SameText(Node.NodeName, 'ID') then begin
          Project := FindProject(ptUnknown, Node.NodeValue);
          if Project <> Nil then
            FFavorites.Add(Project);
        end;
        //
        Node := Node.NextSibling;
      end;
    end;
  end;
end;

procedure TNEDSessionContext.ReadRecents(const ARootNode: TJSONNode);
var
  List, Node: TJSONNode;
  LCount: Integer;
  Project: TNEDProject;
begin
  if ARootNode <> Nil then begin
    LCount := FStorage.NodeAsInteger(ARootNode, 'Count', 0);
    List := ARootNode.FindNode('List', [jtObject]);
    if List <> Nil then begin
      if List.HasChildNodes and (List.ChildNodes.Count <> LCount) then begin // list not complete
        FError := True;
        FErrorMsg := Trim(FErrorMsg + #13#10 + Format('Recents list elements count mismatch, expected: %d, but got: %d.', [LCount, List.ChildNodes.Count]));
        //
        Exit;
      end;
      //
      Node := List.FirstChild;
      while Node <> Nil do begin
        if SameText(Node.NodeName, 'ID') then begin
          Project := FindProject(ptUnknown, Node.NodeValue);
          if Project <> Nil then
            FRecents.Add(Project);
        end;
        //
        Node := Node.NextSibling;
      end;
    end;
  end;
end;

procedure TNEDSessionContext.ReadWorkspace;
begin

end;

procedure TNEDSessionContext.ReadSettings(const ARootNode: TJSONNode);
var
  LErrorMsg: String;
begin
  FSettings.Reset;
  if ARootNode <> Nil then begin
    if not FSettings.Read(ARootNode, LErrorMsg) then begin
      FError := True;
      FErrorMsg := Trim(FErrorMsg + #13#10 + LErrorMsg);
    end;
  end;
end;

procedure TNEDSessionContext.ReadProject(const ANode: TJSONNode; const AList: TObjectList<TNEDProject>);
var
  //
  Project: TNEDProject;
begin
  if ANode = Nil then
    Exit;
  //
  Project := Nil; // satisfy compiler
  try
    Project := TNEDProject.Create(FStorage, ANode);
    AList.Add(Project);
  except
    if Project <> Nil then
      Project.Free;
    raise;
  end;
end;

procedure TNEDSessionContext.ContextClear;
begin
  FFavorites.Clear;
  FRecents.Clear;
  FLayout.Reset;
  FSettings.Reset;
  //FProjects.Clear;
end;

//Result := FProjects.Count;

function TNEDSessionContext.ContextFavoritesCount: Integer;
begin
  Result := FFavorites.Count;
end;

function TNEDSessionContext.ContextRecentsCount: Integer;
begin
  Result := FRecents.Count;
end;

function TNEDSessionContext.ContextAdd(const AProjectID, AName: String): TNEDProject;
begin
  try
    Result := Nil;
//    Result := TNEDProject.Create(FStorage, ANode);
//    Result.FID := AID;
//    Result.FIndex :=
//    FProjects.Add(Result);
  except
    if Result <> Nil then
      Result.Free;
    Result := Nil;
  end;
end;

function TNEDSessionContext.ContextAddNew(const AName: String): TNEDProject;
begin
  try
    Result := Nil;
//    Result := TNEDProject.Create(FStorage, ANode); // (AName, True);
//    Result.FIndex :=
//    FProjects.Add(Result);
//    if ASetAsDefault then
//      FDefaultIndex := Result.FIndex;
  except
    if Result <> Nil then
      Result.Free;
    Result := Nil;
  end;
end;

function TNEDSessionContext.ContextRemove(const AIndex: Integer): Boolean;
begin
  Result := False;
  if (FProjects.Count > 0) and (AIndex >= 0) and (AIndex < FProjects.Count) then begin
    FProjects.Delete(AIndex);
    Result := True;
  end;
end;

function TNEDSessionContext.ContextMove(const AFromIndex, AToIndex: Integer): Boolean;
begin
  Result := False;
  if (FProjects.Count > 0) and
     (AFromIndex <> AToIndex) and
     (AFromIndex >= 0) and (AFromIndex < FProjects.Count) and
     (AToIndex >= 0) and (AToIndex < FProjects.Count)
  then begin
    FProjects.Move(AFromIndex, AToIndex);
    Result := True;
  end;
end;

function TNEDSessionContext.FindProject(const AType: TNEDProjectTypeEnum; const AID: String): TNEDProject;
var
  i: Integer;
begin
  Result := Nil;
  for i := 0 to FProjects.Count - 1 do begin
    if (((AType > ptUnknown) and (FProjects.Items[i].&Type = AType)) or (AType = ptUnknown)) and SameText(FProjects.Items[i].ID, AID) then begin
      Result := FProjects.Items[i];
      Exit;
    end;
  end;
end;

function TNEDSessionContext.FindFavorite(const AType: TNEDProjectTypeEnum; const AFilePath: String): TNEDProject;
var
  i: Integer;
begin
  Result := Nil;
  for i := 0 to FFavorites.Count - 1 do begin
    if (FFavorites.Items[i].&Type = AType) and SameText(FFavorites.Items[i].FullFilePath, AFilePath) then begin
      Result := FFavorites.Items[i];
      Exit;
    end;
  end;
end;

function TNEDSessionContext.FindRecent(const AType: TNEDProjectTypeEnum; const AFilePath: String): TNEDProject;
var
  i: Integer;
begin
  Result := Nil;
  for i := 0 to FRecents.Count - 1 do begin
    if (FRecents.Items[i].&Type = AType) and SameText(FRecents.Items[i].FullFilePath, AFilePath) then begin
      Result := FRecents.Items[i];
      Exit;
    end;
  end;
end;

function TNEDSessionContext.FindRecentFile(const AFilePath: String): TNEDProject;
var
  i: Integer;
  Project: TNEDProject;
begin
  Result := Nil;
  for i := 0 to FRecents.Count - 1 do begin
    Project := FRecents.Items[i];
    if (Project.&Type = ptProject) and Project.ContainsFile(AFilePath) then begin
      Result := Project;
      Exit;
    end;
  end;
  //
  for i := 0 to FRecents.Count - 1 do begin
    Project := FRecents.Items[i];
    if (Project.&Type = ptFile) and SameText(Project.FullFilePath, AFilePath) then begin
      Result := Project;
      Exit;
    end;
  end;
end;

//    FProjects: TObjectList<TNEDProject>;
//    //
//    FFavorites: TObjectList<TNEDProject>;
//    FRecent: TObjectList<TNEDProject>;
//    FLayout: TNEDLayout;
//    FSettings: TNEDSettings;

procedure TNEDSessionContext.LoadContext(const AProfilePath: String);
begin
  try
    FError := False;
    FErrorMsg := '';
    ContextClear;
    FStorage.Clear;
    FStorageFileName := IncludeTrailingPathDelimiter(AProfilePath) + 'profile.jcfg';
    FStorage.LoadFromFile(FStorageFileName);
    FLoaded := FStorage.Empty;
    if FLoaded then begin
      ReadProfileHeader;
      ReadContext;
      ReadWorkspace;
    end;
  except
    // no error here
    on E: Exception do begin
      FError := True;
      FErrorMsg := E.ClassName + ': ' + E.Message;
    end;
  end;
end;

procedure TNEDSessionContext.LoadSession(const ASessionPath: String);
begin

end;

procedure TNEDSessionContext.LoadProject(const AProjectPath: String);
begin

end;

procedure TNEDSessionContext.LoadFile(const AFilePath: String);
begin

end;

end.

