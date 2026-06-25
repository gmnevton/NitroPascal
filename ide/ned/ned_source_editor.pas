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
  TNEDEditorForm = class(TUForm)
    SynEdit1: TSynEdit; // this will be removed
    SynGeneralSyn1: TSynGeneralSyn;
    UPanel4: TUPanel;
    UScrollBox1: TUScrollBox;
    USymbolButton1: TUSymbolButton;
    UPopupMenu1: TUPopupMenu;
    USymbolButton2: TUSymbolButton;
    _mnuShowNonVisibleLines: TMenuItem;
    PopupMenu1: TPopupMenu;
    mnuShowNonVisibleLines: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuShowNonVisibleLinesClick(Sender: TObject);
  private
    Buffer: TNEDEditorBuffer;
    Editor: TNEDEditorView;
    //
    procedure NEDEditorInfoDetails(var Msg: TMessage); message CM_NED_EDITORINFO_DETAILS;
  public
  end;

implementation

{$R *.dfm}

procedure TNEDEditorForm.FormCreate(Sender: TObject);
begin
  Buffer := TNEDEditorBuffer.Create;
  //
  Editor := TNEDEditorView.Create(Self);
  Editor.Parent := Self;
  Editor.Align := alClient;
  Editor.Document := Buffer;
  Editor.PopupMenu := PopupMenu1;
  //
  Buffer.LoadFromFile('d:\Borland Librarys\NitroPascal\compiler\tests\simple_project\first.npe');
end;

procedure TNEDEditorForm.FormDestroy(Sender: TObject);
begin
  Editor.Free;
  Buffer.Free;
end;

procedure TNEDEditorForm.FormShow(Sender: TObject);
begin
  Editor.SetFocus;
end;

procedure TNEDEditorForm.NEDEditorInfoDetails(var Msg: TMessage);
begin
  Msg.Result := Application.MainForm.Perform(Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TNEDEditorForm.mnuShowNonVisibleLinesClick(Sender: TObject);
var
  EditorProp: TNEDEditorProperties;
begin
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

