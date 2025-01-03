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
  SynEdit;

type
  TNEDEditorForm = class(TUForm)
    SynEdit1: TSynEdit;
    SynGeneralSyn1: TSynGeneralSyn;
    UPanel4: TUPanel;
    UScrollBox1: TUScrollBox;
    USymbolButton1: TUSymbolButton;
    UPopupMenu1: TUPopupMenu;
    USymbolButton2: TUSymbolButton;
  private
  public
  end;

implementation

{$R *.dfm}

end.

