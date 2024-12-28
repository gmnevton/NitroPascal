unit ned_main;

interface

uses
  SysUtils,
  Classes,
  Controls,
  ExtCtrls,
  Forms,
  UCL.Form,
  UCL.ThemeManager,
  UCL.Types,
  UCL.Panel,
  UCL.CaptionBar,
  UCL.ScrollBox,
  UCL.ItemButton,
  UCL.SymbolButton;

type
  TNEDMainForm = class(TUForm)
    UThemeManager1: TUThemeManager;
    UCaptionBar1: TUCaptionBar;
    UPanel1: TUPanel;
    UPanel2: TUPanel;
    UPanel3: TUPanel;
    USymbolButton1: TUSymbolButton;
    UItemButton1: TUItemButton;
    UScrollBox1: TUScrollBox;
  private
  public
  end;

var
  NEDMainForm: TNEDMainForm;

implementation

{$R *.dfm}

end.
