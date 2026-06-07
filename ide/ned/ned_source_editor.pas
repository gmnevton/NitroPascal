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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    Editor: TNEDEditorView;
  public
  end;

implementation

{$R *.dfm}

procedure TNEDEditorForm.FormCreate(Sender: TObject);
begin
  Editor := TNEDEditorView.Create(Self);
  Editor.Parent := Self;
  Editor.Align := alClient;
end;

procedure TNEDEditorForm.FormDestroy(Sender: TObject);
begin
  Editor.Free;
end;

procedure TNEDEditorForm.FormShow(Sender: TObject);
begin
//
end;

end.

