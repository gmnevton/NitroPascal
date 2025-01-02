unit ned_source_editor;

interface

uses
  SysUtils,
  Classes,
  Controls,
  ExtCtrls,
  Forms,
  UCL.Form,
  UCL.ThemeManager,
  UCL.SymbolButton,
  UCL.ScrollBox,
  UCL.Panel,
  SynEditHighlighter,
  SynHighlighterGeneral,
  SynEdit;

type
  TForm1 = class(TUForm)
    SynEdit1: TSynEdit;
    SynGeneralSyn1: TSynGeneralSyn;
    UPanel4: TUPanel;
    UScrollBox1: TUScrollBox;
    USymbolButton1: TUSymbolButton;
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
