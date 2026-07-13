unit ned_source_view;

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
  ned_splitview_manager,
  ned_source_editor,
  ned_editor_buffer,
  ned_editor_view;

type
  TNEDViewForm = class(TUForm)
    pnlBaseView: TUPanel;
    //
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    SplitManager: TNEDSplittingManager;
    Buffers: TList<TNEDEditorBuffer>;
    Views: TList<TNEDEditorForm>;
    MainView: TNEDEditorForm;
    //
    function OpenNewBuffer(const FilePath: String; out Buffer: TNEDEditorBuffer; const HostControl: TWinControl; var View: TNEDEditorForm): Boolean;
    function OpenExistingBuffer(const Buffer: TNEDEditorBuffer; const View: TNEDEditorForm): TNEDEditorView;
  public
    function OpenFile(const FilePath: String; const SplitWindow: Boolean = False; const SplitType: TNEDSplitViewTypeEnum = stSplitNone): Boolean;
  end;

implementation

{$R *.dfm}

procedure TNEDViewForm.FormCreate(Sender: TObject);
begin
  SplitManager := TNEDSplittingManager.Create(pnlBaseView);
  Buffers := TList<TNEDEditorBuffer>.Create;
  Views := TList<TNEDEditorForm>.Create;
  MainView := Nil;
end;

procedure TNEDViewForm.FormDestroy(Sender: TObject);
begin
  Views.Free;
  Buffers.Free;
  SplitManager.Free;
end;

procedure TNEDViewForm.FormActivate(Sender: TObject);
begin
//
end;

procedure TNEDViewForm.FormDeactivate(Sender: TObject);
begin
//
end;

procedure TNEDViewForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
//
end;

procedure TNEDViewForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
//
end;

procedure TNEDViewForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
//
end;

procedure TNEDViewForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
//
end;

procedure TNEDViewForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
//
end;

procedure TNEDViewForm.FormResize(Sender: TObject);
begin
//
end;

procedure TNEDViewForm.FormShow(Sender: TObject);
begin
//
end;

function TNEDViewForm.OpenFile(const FilePath: String; const SplitWindow: Boolean; const SplitType: TNEDSplitViewTypeEnum): Boolean;
var
  Buffer: TNEDEditorBuffer;
  View: TNEDEditorForm;
  host_ctrl: TWinControl;
begin
  View := MainView;
  host_ctrl := pnlBaseView;
  if SplitWindow then begin
    host_ctrl:= SplitManager.Split(SplitType);
    View := Nil;
  end;
  //
  if OpenNewBuffer(FilePath, Buffer, host_ctrl, View) then begin
    Buffers.Add(Buffer);
    Views.Add(View);
    if MainView = Nil then
      MainView := View;
  end
  else begin

  end;
end;

function TNEDViewForm.OpenNewBuffer(const FilePath: String; out Buffer: TNEDEditorBuffer; const HostControl: TWinControl; var View: TNEDEditorForm): Boolean;
var
  Editor: TNEDEditorView;
begin
  try
    Buffer := TNEDEditorBuffer.Create;
    if View = Nil then begin
      View := TNEDEditorForm.Create(Self);
      View.Parent := HostControl;
      View.Align := alClient;
      View.Show;
      View.BringToFront;
    end;
    //
    Editor := View.NewEditor(Buffer);
    //
  //  Buffer.LoadFromFile('d:\Borland Librarys\NitroPascal\compiler\tests\simple_project\first.npe');
    Buffer.LoadFromFile(FilePath);
    //
    Editor.SetFocus;
    //
    Result := True;
  except
    Result := False;
  end;
end;

function TNEDViewForm.OpenExistingBuffer(const Buffer: TNEDEditorBuffer; const View: TNEDEditorForm): TNEDEditorView;
begin

end;

end.

