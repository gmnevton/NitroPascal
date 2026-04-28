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
  UCL.ItemButton,
  UCL.RadioButton,
  UCL.Button,
  UCL.Panel;

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
    UHyperLink4: TUHyperLink;
    //
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure UHyperLink1Click(Sender: TObject);
    procedure UHyperLink2Click(Sender: TObject);
    procedure UHyperLink3Click(Sender: TObject);
    procedure URadioButton1DblClick(Sender: TObject);
    procedure URadioButton2DblClick(Sender: TObject);
    procedure URadioButton3DblClick(Sender: TObject);
    procedure UButton1Click(Sender: TObject);
    procedure UButton2Click(Sender: TObject);
    procedure UCheckBox1Click(Sender: TObject);
    procedure USymbolButton1Click(Sender: TObject);
  private
  public
  end;

implementation

{$R *.dfm}

uses
  UCL.Types;

procedure TNEDHomeForm.FormCreate(Sender: TObject);
begin
//
end;

procedure TNEDHomeForm.FormDestroy(Sender: TObject);
begin
//
end;

procedure TNEDHomeForm.FormShow(Sender: TObject);
begin
//
end;

procedure TNEDHomeForm.FormResize(Sender: TObject);
begin
//
end;

procedure TNEDHomeForm.UHyperLink1Click(Sender: TObject);
begin
//
end;

procedure TNEDHomeForm.UHyperLink2Click(Sender: TObject);
begin
//
end;

procedure TNEDHomeForm.UHyperLink3Click(Sender: TObject);
begin
//
end;

procedure TNEDHomeForm.URadioButton1DblClick(Sender: TObject);
begin
  GetCommonThemeManager.Theme := ttDark;
end;

procedure TNEDHomeForm.URadioButton2DblClick(Sender: TObject);
begin
  GetCommonThemeManager.Theme := ttLight;
end;

procedure TNEDHomeForm.URadioButton3DblClick(Sender: TObject);
begin
  GetCommonThemeManager.Theme := ttSystem;
end;

procedure TNEDHomeForm.UButton1Click(Sender: TObject);
begin
//
end;

procedure TNEDHomeForm.UButton2Click(Sender: TObject);
begin
//
end;

procedure TNEDHomeForm.UCheckBox1Click(Sender: TObject);
begin
//
end;

procedure TNEDHomeForm.USymbolButton1Click(Sender: TObject);
begin
//
end;

end.
