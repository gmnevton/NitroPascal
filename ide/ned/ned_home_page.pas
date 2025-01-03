unit ned_home_page;

interface

uses
  SysUtils,
  Classes,
  Controls,
  StdCtrls,
  ExtCtrls,
  Forms,
  Menus,
  UCL.Form,
  UCL.ThemeManager,
  UCL.HyperLink,
  UCL.Text,
  UCL.ScrollBox,
  UCL.CheckBox,
  UCL.SymbolButton,
  UCL.ItemButton, UCL.RadioButton, UCL.Button, UCL.Panel;

type
  TNEDHomeForm = class(TUForm)
    UText1: TUText;
    UText2: TUText;
    UHyperLink1: TUHyperLink;
    UHyperLink2: TUHyperLink;
    UHyperLink3: TUHyperLink;
    UCheckBox1: TUCheckBox;
    USymbolButton1: TUSymbolButton;
    UText5: TUText;
    URadioButton1: TURadioButton;
    UText6: TUText;
    URadioButton2: TURadioButton;
    URadioButton3: TURadioButton;
    UButton1: TUButton;
    UButton2: TUButton;
    GridPanel1: TGridPanel;
    UPanel1: TUPanel;
    UText3: TUText;
    UScrollBox1: TUScrollBox;
    UItemButton1: TUItemButton;
    UPanel2: TUPanel;
    UText4: TUText;
    UScrollBox2: TUScrollBox;
    UItemButton2: TUItemButton;
  private
  public
  end;

implementation

{$R *.dfm}

end.
