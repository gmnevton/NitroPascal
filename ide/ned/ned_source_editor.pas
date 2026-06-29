//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-06
// All rights reserved.
//

unit ned_source_editor;

interface

uses
  SysUtils,
  Classes,
  Messages,
  Controls,
  ExtCtrls,
  Forms,
  Menus,
  Generics.Collections,
  UCL.Form,
  UCL.ThemeManager,
  UCL.SymbolButton,
  UCL.ScrollBox,
  UCL.Panel,
  UCL.PopupMenu,
  SynEditHighlighter,
  SynHighlighterGeneral,
  SynEdit,
  ned_editor_buffer,
  ned_editor_view;

type
  TNEDEditorInfo = class
  public
    Thumbstone: TUSymbolButton;
    Editor: TNEDEditorView;
  public
    constructor Create(const AThumbstone: TUSymbolButton; const AEditor: TNEDEditorView);
    destructor Destroy; override;
  end;

  TNEDEditorForm = class(TUForm)
    SynEdit1: TSynEdit; // this will be removed
    SynGeneralSyn1: TSynGeneralSyn;
    UPanel4: TUPanel;
    UScrollBox1: TUScrollBox;
    UPopupMenu1: TUPopupMenu;
    _mnuShowNonVisibleLines: TMenuItem;
    PopupMenu1: TPopupMenu;
    mnuShowNonVisibleLines: TMenuItem;
    USymbolButton1: TUSymbolButton;
    USymbolButton2: TUSymbolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuShowNonVisibleLinesClick(Sender: TObject);
  private
    Editors: TList<TNEDEditorInfo>;
    //
    procedure NEDEditorInfoDetails(var Msg: TMessage); message CM_NED_EDITORINFO_DETAILS;
  public
    function NewEditor(const Buffer: TNEDEditorBuffer): TNEDEditorView;
  end;

implementation

{$R *.dfm}

{ TNEDEditorInfo }

constructor TNEDEditorInfo.Create(const AThumbstone: TUSymbolButton; const AEditor: TNEDEditorView);
begin
  Thumbstone := AThumbstone;
  Editor := AEditor;
end;

destructor TNEDEditorInfo.Destroy;
begin
//  Thumbstone.Free;
//  Editor.Free;
  inherited;
end;

{ TNEDEditorForm }

procedure TNEDEditorForm.FormCreate(Sender: TObject);
begin
  Editors := TList<TNEDEditorInfo>.Create;
end;

procedure TNEDEditorForm.FormDestroy(Sender: TObject);
begin
  //
  Editors.Clear;
  Editors.Free;
end;

procedure TNEDEditorForm.FormShow(Sender: TObject);
begin
  //
end;

function TNEDEditorForm.NewEditor(const Buffer: TNEDEditorBuffer): TNEDEditorView;
var
  Button, SymbolButton: TUSymbolButton;
  i, cnt, ctrl_left: Integer;
begin
  Result := TNEDEditorView.Create(Self);
  Result.Parent := Self;
  Result.Align := alClient;
  Result.Document := Buffer;
  Result.PopupMenu := PopupMenu1;
  //
  for i := 0 to UScrollBox1.ControlCount - 1 do begin
    Button := TUSymbolButton(UScrollBox1.Controls[i]);
    Button.IsToggled := False;
  end;
  //
  cnt := UScrollBox1.ControlCount - 1;
  ctrl_left := 0;
  if cnt > 0 then begin
    ctrl_left := UScrollBox1.Controls[cnt - 1].Left;
    Inc(ctrl_left);
  end;
  SymbolButton := TUSymbolButton.Create(UScrollBox1);
  SymbolButton.Parent := UScrollBox1;
  SymbolButton.Top := 0;
  SymbolButton.Left := ctrl_left;
  SymbolButton.Height := UScrollBox1.Height;
  SymbolButton.Align := alLeft;
  SymbolButton.ParentColor := False;
  SymbolButton.IsToggleButton := True;
  SymbolButton.IsToggled := True;
  SymbolButton.Detail := '---';
  SymbolButton.Text := '---';
  //
  Editors.Add(TNEDEditorInfo.Create(SymbolButton, Result));
end;

procedure TNEDEditorForm.NEDEditorInfoDetails(var Msg: TMessage);
var
  EditorInfo: TNEDEditorInfo;
  EditorInfoDetails: PNEDEditorInfoDetails;
begin
  Msg.Result := Application.MainForm.Perform(Msg.Msg, Msg.WParam, Msg.LParam);
  //
  if Msg.WParam = 0 then begin

  end
  else begin
    for EditorInfo in Editors do begin
      if EditorInfo.Editor = TNEDEditorView(Msg.WParam) then begin
        if SameText(ExtractFileExt(EditorInfo.Editor.Document.FilePath), '.npe') then
          EditorInfo.Thumbstone.SymbolChar := ''
        else
          EditorInfo.Thumbstone.SymbolChar := '';
        EditorInfo.Thumbstone.Detail := EditorInfo.Editor.EditorFileType;
        EditorInfo.Thumbstone.Text := ExtractFileName(EditorInfo.Editor.Document.FilePath);
      end;
    end;
//    EditorInfoDetails := PNEDEditorInfoDetails(Msg.LParam);
  end;
end;

procedure TNEDEditorForm.mnuShowNonVisibleLinesClick(Sender: TObject);
var
  Editor: TNEDEditorView;
  EditorProp: TNEDEditorProperties;
begin
  if Sender = Nil then
    Exit;
  //
  Editor := TNEDEditorView(Sender);
  EditorProp := Editor.Options.EditorProperties;
  if mnuShowNonVisibleLines.Checked then begin
    Include(EditorProp, epShowNonVisibleLines);
  end
  else begin
    Exclude(EditorProp, epShowNonVisibleLines);
  end;
  Editor.Options.EditorProperties := EditorProp;
end;

end.

