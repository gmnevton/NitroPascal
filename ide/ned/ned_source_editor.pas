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
    //
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuShowNonVisibleLinesClick(Sender: TObject);
    procedure btnEditorThumbClick(Sender: TObject);
  private
    procedure CMDialogKey(var Msg: TCMDialogKey); message CM_DIALOGKEY; // grab TAB key before delphi can still it and switch it off
    procedure NEDEditorInfoDetails(var Msg: TMessage); message CM_NED_EDITORINFO_DETAILS;
  public
    function NewEditor(const Buffer: TNEDEditorBuffer): TNEDEditorView;
    class procedure SelectEditorByThumbstone(const Thumbstone: TUSymbolButton);
    class procedure SelectEditorByEditor(const Editor: TNEDEditorView);
    class procedure SelectEditorByIndex(const Index: Integer);
  end;

var
  NEDEditors: TList<TNEDEditorInfo>;

implementation

{$R *.dfm}

//uses
//  Windows,
//  Dialogs;

var
  NEDUniqueEditorNumber: Integer = 0;

procedure CreateEditorsList;
begin
  NEDEditors := TList<TNEDEditorInfo>.Create;
end;

procedure DestroyEditorsList;
begin
  NEDEditors.Clear;
  NEDEditors.Free;
end;

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
//
end;

procedure TNEDEditorForm.FormDestroy(Sender: TObject);
begin
//
end;

procedure TNEDEditorForm.FormShow(Sender: TObject);
begin
  //
end;

function TNEDEditorForm.NewEditor(const Buffer: TNEDEditorBuffer): TNEDEditorView;
var
  Button, SymbolButton: TUSymbolButton;
  i, ctrl_left: Integer;
begin
  Inc(NEDUniqueEditorNumber);
  //
  Result := TNEDEditorView.Create(Self);
  Result.Name := 'NEDEditorView' + IntToStr(NEDUniqueEditorNumber);
  Result.Parent := Self;
  Result.Align := alClient;
  Result.Document := Buffer;
  Result.PopupMenu := PopupMenu1;
  //
  ctrl_left := 0;
  for i := 0 to UScrollBox1.ControlCount - 1 do begin
    if UScrollBox1.Controls[i] is TUSymbolButton then begin
      Button := TUSymbolButton(UScrollBox1.Controls[i]);
      Button.IsToggled := False;
      if ctrl_left < UScrollBox1.Controls[i].Left + UScrollBox1.Controls[i].Width then
        ctrl_left := UScrollBox1.Controls[i].Left + UScrollBox1.Controls[i].Width;
    end;
  end;
  //
  SymbolButton := TUSymbolButton.Create(UScrollBox1);
  SymbolButton.Parent := UScrollBox1;
  SymbolButton.Top := 0;
  SymbolButton.Height := UScrollBox1.Height;
  SymbolButton.Left := ctrl_left + 1;
  SymbolButton.Align := alLeft;
  SymbolButton.ParentColor := True;
  SymbolButton.IsToggleButton := True;
  SymbolButton.IsToggled := True;
  SymbolButton.Detail := '---';
  SymbolButton.Text := '---';
  SymbolButton.OnClick := btnEditorThumbClick;
  //
  NEDEditors.Add(TNEDEditorInfo.Create(SymbolButton, Result));
end;

class procedure TNEDEditorForm.SelectEditorByThumbstone(const Thumbstone: TUSymbolButton);
var
  i: Integer;
  EditorInfo: TNEDEditorInfo;
  EditorView: TNEDEditorView;
begin
  EditorInfo := Nil;
  for i := 0 to NEDEditors.Count - 1 do begin
    if (EditorInfo = Nil) and (NEDEditors.Items[i].Thumbstone = Thumbstone) then begin
      EditorInfo := NEDEditors.Items[i];
      EditorInfo.Thumbstone.IsToggled := True;
    end
    else
      NEDEditors.Items[i].Thumbstone.IsToggled := False;
  end;
  //
  if EditorInfo <> Nil then begin
    EditorView := EditorInfo.Editor;
    EditorView.BringToFront;
    EditorView.ReportEditorInfo;
    EditorView.SetFocus;
  end;
end;

class procedure TNEDEditorForm.SelectEditorByEditor(const Editor: TNEDEditorView);
var
  i: Integer;
  EditorInfo: TNEDEditorInfo;
  EditorView: TNEDEditorView;
begin
  EditorInfo := Nil;
  for i := 0 to NEDEditors.Count - 1 do begin
    if (EditorInfo = Nil) and (NEDEditors.Items[i].Editor = Editor) then begin
      EditorInfo := NEDEditors.Items[i];
      EditorInfo.Thumbstone.IsToggled := True;
    end
    else
      NEDEditors.Items[i].Thumbstone.IsToggled := False;
  end;
  //
  if EditorInfo <> Nil then begin
    EditorView := EditorInfo.Editor;
    EditorView.BringToFront;
    EditorView.ReportEditorInfo;
    EditorView.SetFocus;
  end;
end;

class procedure TNEDEditorForm.SelectEditorByIndex(const Index: Integer);
var
  i: Integer;
  EditorInfo: TNEDEditorInfo;
  EditorView: TNEDEditorView;
begin
  EditorInfo := Nil;
  for i := 0 to NEDEditors.Count - 1 do begin
    if (EditorInfo = Nil) and (i = Index) then begin
      EditorInfo := NEDEditors.Items[i];
      EditorInfo.Thumbstone.IsToggled := True;
    end
    else
      NEDEditors.Items[i].Thumbstone.IsToggled := False;
  end;
  //
  if EditorInfo <> Nil then begin
    EditorView := EditorInfo.Editor;
    EditorView.BringToFront;
    EditorView.ReportEditorInfo;
    EditorView.SetFocus;
  end;
end;

procedure TNEDEditorForm.CMDialogKey(var Msg: TCMDialogKey);
begin
  inherited;
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
    for EditorInfo in NEDEditors do begin
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

procedure TNEDEditorForm.btnEditorThumbClick(Sender: TObject);
begin
  SelectEditorByThumbstone(TUSymbolButton(Sender));
end;

initialization
  CreateEditorsList;

finalization
  DestroyEditorsList;

end.

