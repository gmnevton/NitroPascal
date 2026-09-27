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
    Project: TObject;
    Thumbstone: TUSymbolButton;
    Editor: TNEDEditorView;
  public
    constructor Create(const AThumbstone: TUSymbolButton; const AEditor: TNEDEditorView);
    destructor Destroy; override;
  end;

  TNEDEditorForm = class(TUForm)
    SynEdit1: TSynEdit; // this will be removed
    SynGeneralSyn1: TSynGeneralSyn; // this will be removed
    UPanel4: TUPanel;
    UScrollBox1: TUScrollBox;
    UPopupMenu1: TUPopupMenu;
    _mnuShowNonVisibleLines: TMenuItem;
    PopupMenu1: TPopupMenu;
    mnuShowNonVisibleLines: TMenuItem;
    USymbolButton1: TUSymbolButton; // this will be removed
    USymbolButton2: TUSymbolButton; // this will be removed
    //
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuShowNonVisibleLinesClick(Sender: TObject);
    procedure btnEditorThumbClick(Sender: TObject);
  private
    procedure CMDialogKey(var Msg: TCMDialogKey); message CM_DIALOGKEY; // grab TAB key before delphi can still it and switch it off
    procedure NEDEditorInfoDetails(var Msg: TMessage); message CM_NED_EDITORINFO_DETAILS;
    procedure NEDEditorFocus(Sender: TNEDCustomEditorView; const FocusType: TNEDCustomEditorViewFocusEnum);
    //
    function FindEditorInfoByThumbstone(const Thumbstone: TUSymbolButton): TNEDEditorInfo;
    function FindEditorInfoByEditor(const Editor: TNEDEditorView): TNEDEditorInfo;
    procedure RemoveEditorInfo(const Info: TNEDEditorInfo);
  public
    function HasEditors: Boolean;
    function NewEditor(const Buffer: TNEDEditorBuffer; out Info: TNEDEditorInfo): TNEDEditorView;
    procedure DisposeEditor(var EditorView: TNEDEditorView);
    class procedure SelectEditorByThumbstone(const Thumbstone: TUSymbolButton);
    class procedure SelectEditorByEditor(const Editor: TNEDEditorView);
    class procedure SelectEditorByIndex(const Index: Integer);
    class procedure SelectThumbstoneByEditor(const Editor: TNEDEditorView);
  end;

var
  NEDEditorsInfo: TObjectList<TNEDEditorInfo>;

implementation

{$R *.dfm}

uses
//  Windows,
//  Dialogs;
  ned_main,
  ned_source_view;
//  ned_workspace_manager;

var
  NEDUniqueEditorNumber: Integer = 0;

procedure CreateEditorsList;
begin
  NEDEditorsInfo := TObjectList<TNEDEditorInfo>.Create(True);
end;

procedure DestroyEditorsList;
begin
  NEDEditorsInfo.Clear;
  NEDEditorsInfo.Free;
end;

{ TNEDEditorInfo }

constructor TNEDEditorInfo.Create(const AThumbstone: TUSymbolButton; const AEditor: TNEDEditorView);
begin
  Project := Nil;
  Thumbstone := AThumbstone;
  Editor := AEditor;
end;

destructor TNEDEditorInfo.Destroy;
begin
  Project := Nil;
  Thumbstone := Nil;
  Editor := Nil;
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

function TNEDEditorForm.HasEditors: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Self.ComponentCount - 1 do begin
    if Self.Components[i] is TNEDEditorView then
      Result := True;
  end;
end;

function TNEDEditorForm.NewEditor(const Buffer: TNEDEditorBuffer; out Info: TNEDEditorInfo): TNEDEditorView;
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
  Result.OnFocus := NEDEditorFocus;
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
  SymbolButton.RightCloseVisible := True;
  SymbolButton.OnClick := btnEditorThumbClick;
  //
  Info := TNEDEditorInfo.Create(SymbolButton, Result);
  NEDEditorsInfo.Add(Info);
end;

procedure TNEDEditorForm.DisposeEditor(var EditorView: TNEDEditorView);
begin
  EditorView.Free;
  EditorView := Nil;
  if not Self.HasEditors then begin
    TNEDViewForm(Self.Owner).DisposeEditorForm(Self);
  end;
end;

class procedure TNEDEditorForm.SelectEditorByThumbstone(const Thumbstone: TUSymbolButton);
var
  i: Integer;
  EditorInfo: TNEDEditorInfo;
  EditorView: TNEDEditorView;
begin
  EditorInfo := Nil;
  for i := 0 to NEDEditorsInfo.Count - 1 do begin
    if (EditorInfo = Nil) and (NEDEditorsInfo.Items[i].Thumbstone = Thumbstone) then begin
      EditorInfo := NEDEditorsInfo.Items[i];
      EditorInfo.Thumbstone.IsToggled := True;
    end
    else
      NEDEditorsInfo.Items[i].Thumbstone.IsToggled := False;
  end;
  //
  if EditorInfo <> Nil then begin
    NEDMainForm.SelectWorkspaceProjectEntry(EditorInfo);
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
  for i := 0 to NEDEditorsInfo.Count - 1 do begin
    if (EditorInfo = Nil) and (NEDEditorsInfo.Items[i].Editor = Editor) then begin
      EditorInfo := NEDEditorsInfo.Items[i];
      EditorInfo.Thumbstone.IsToggled := True;
    end
    else
      NEDEditorsInfo.Items[i].Thumbstone.IsToggled := False;
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
  for i := 0 to NEDEditorsInfo.Count - 1 do begin
    if (EditorInfo = Nil) and (i = Index) then begin
      EditorInfo := NEDEditorsInfo.Items[i];
      EditorInfo.Thumbstone.IsToggled := True;
    end
    else
      NEDEditorsInfo.Items[i].Thumbstone.IsToggled := False;
  end;
  //
  if EditorInfo <> Nil then begin
    EditorView := EditorInfo.Editor;
    EditorView.BringToFront;
    EditorView.ReportEditorInfo;
    EditorView.SetFocus;
  end;
end;

class procedure TNEDEditorForm.SelectThumbstoneByEditor(const Editor: TNEDEditorView);
var
  i: Integer;
  EditorInfo: TNEDEditorInfo;
  EditorView: TNEDEditorView;
begin
  EditorInfo := Nil;
  for i := 0 to NEDEditorsInfo.Count - 1 do begin
    if (EditorInfo = Nil) and (NEDEditorsInfo.Items[i].Editor = Editor) then begin
      EditorInfo := NEDEditorsInfo.Items[i];
      EditorInfo.Thumbstone.IsToggled := True;
    end
    else
      NEDEditorsInfo.Items[i].Thumbstone.IsToggled := False;
  end;
  //
  if EditorInfo <> Nil then begin
    NEDMainForm.SelectWorkspaceProjectEntry(EditorInfo);
//    EditorView := EditorInfo.Editor;
//    EditorView.BringToFront;
//    EditorView.ReportEditorInfo;
//    EditorView.SetFocus;
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
    for EditorInfo in NEDEditorsInfo do begin
      if EditorInfo.Editor = TNEDEditorView(Msg.WParam) then begin
        if SameText(ExtractFileExt(EditorInfo.Editor.Document.FilePath), '.npe') then
          EditorInfo.Thumbstone.SymbolChar := Char($E943) // Code
        else
          EditorInfo.Thumbstone.SymbolChar := Char($F000); // KnowledgeArticle
        EditorInfo.Thumbstone.Detail := EditorInfo.Editor.EditorFileType;
        EditorInfo.Thumbstone.Text := ExtractFileName(EditorInfo.Editor.Document.FilePath);
      end;
    end;
//    EditorInfoDetails := PNEDEditorInfoDetails(Msg.LParam);
  end;
end;

procedure TNEDEditorForm.NEDEditorFocus(Sender: TNEDCustomEditorView; const FocusType: TNEDCustomEditorViewFocusEnum);
var
  EditorInfo: TNEDEditorInfo;
begin
  if FocusType = vfSetFocus then begin
    EditorInfo := FindEditorInfoByEditor(TNEDEditorView(Sender));
    if (EditorInfo <> Nil) and not EditorInfo.Thumbstone.IsToggled then begin
      SelectThumbstoneByEditor(TNEDEditorView(Sender));
    end;
  end;
end;

function TNEDEditorForm.FindEditorInfoByThumbstone(const Thumbstone: TUSymbolButton): TNEDEditorInfo;
var
  i: Integer;
begin
  Result := Nil;
  for i := 0 to NEDEditorsInfo.Count - 1 do begin
    if NEDEditorsInfo.Items[i].Thumbstone = Thumbstone then begin
      Result := NEDEditorsInfo.Items[i];
      Exit;
    end;
  end;
end;

function TNEDEditorForm.FindEditorInfoByEditor(const Editor: TNEDEditorView): TNEDEditorInfo;
var
  i: Integer;
begin
  Result := Nil;
  for i := 0 to NEDEditorsInfo.Count - 1 do begin
    if NEDEditorsInfo.Items[i].Editor = Editor then begin
      Result := NEDEditorsInfo.Items[i];
      Exit;
    end;
  end;
end;

procedure TNEDEditorForm.RemoveEditorInfo(const Info: TNEDEditorInfo);
var
  i: Integer;
begin
  if Info = Nil then
    Exit;
  //
  i := NEDEditorsInfo.IndexOf(Info);
  if i > -1 then
    NEDEditorsInfo.Delete(i);
end;

procedure TNEDEditorForm.mnuShowNonVisibleLinesClick(Sender: TObject);
var
  LPopupMenu: TPopupMenu;
  Editor: TNEDEditorView;
  EditorProp: TNEDEditorProperties;
begin
  if Sender = Nil then
    Exit;
  //
  LPopupMenu := TPopupMenu(TMenuItem(Sender).GetParentMenu);
  Editor := TNEDEditorView(LPopupMenu.PopupComponent);
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
var
  EditorInfo: TNEDEditorInfo;
  EditorView: TNEDEditorView;
begin
  if TUSymbolButton(Sender).CloseClicked then begin
    EditorInfo := FindEditorInfoByThumbstone(TUSymbolButton(Sender));
    if EditorInfo <> Nil then begin
      TUSymbolButton(Sender).Free; // remove thumbstone
      EditorView := EditorInfo.Editor;
      //EditorView.Free;
      DisposeEditor(EditorView); // close editor view and if no other editors open, close editor form
      RemoveEditorInfo(EditorInfo); // remove editor info
    end;
  end
  else
    SelectEditorByThumbstone(TUSymbolButton(Sender));
end;

initialization
  CreateEditorsList;

finalization
  DestroyEditorsList;

end.

