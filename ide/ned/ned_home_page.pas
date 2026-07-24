//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-07
// All rights reserved.
//

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
  UCL.Panel,
  SplitEx;

type
  TNEDHomeForm = class(TUForm)
    UText1: TUText;
    UText2: TUText;
    UHyperLink1: TUHyperLink;
    UHyperLink2: TUHyperLink;
    UHyperLink3: TUHyperLink;
    cbShowHomePage: TUCheckBox;
    USymbolButton1: TUSymbolButton;
    UText5: TUText;
    rbColorSchemeDark: TURadioButton;
    UText6: TUText;
    rbColorSchemeLight: TURadioButton;
    rbColorSchemeSystem: TURadioButton;
    UButton1: TUButton;
    UButton2: TUButton;
    GridPanel1: TGridPanel;
    UPanel1: TUPanel;
    UPanel2: TUPanel;
    UHyperLink4: TUHyperLink;
    UPanel3: TUPanel;
    SplitterEx1: TSplitterEx;
    UScrollBox2: TUScrollBox;
    UItemButton2: TUItemButton;
    UText4: TUText;
    UPanel4: TUPanel;
    UScrollBox1: TUScrollBox;
    UItemButton1: TUItemButton;
    UText3: TUText;
    //
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure UHyperLink1Click(Sender: TObject);
    procedure UHyperLink2Click(Sender: TObject);
    procedure UHyperLink3Click(Sender: TObject);
    procedure rbColorSchemeDarkClick(Sender: TObject);
    procedure rbColorSchemeLightClick(Sender: TObject);
    procedure rbColorSchemeSystemClick(Sender: TObject);
    procedure UButton1Click(Sender: TObject);
    procedure UButton2Click(Sender: TObject);
    procedure cbShowHomePageClick(Sender: TObject);
    procedure USymbolButton1Click(Sender: TObject);
  private
  public
  end;

implementation

{$R *.dfm}

uses
  UCL.Types,
  ned_config;

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
  NEDConfig.Lock;
  try
    if SameText(NEDConfig.ColorSchema, 'system') then begin
      rbColorSchemeSystem.Checked := True;
    end
    else if SameText(NEDConfig.ColorSchema, 'dark') then begin
      rbColorSchemeDark.Checked := True;
    end
    else if SameText(NEDConfig.ColorSchema, 'light') then begin
      rbColorSchemeLight.Checked := True;
    end;
    cbShowHomePage.Checked := NEDConfig.ShowHomePage;
//  FrameBrowser1.LoadURL('https://github.com/gmnevton/NitroPascal');
  finally
    NEDConfig.Unlock;
  end;
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

procedure TNEDHomeForm.rbColorSchemeDarkClick(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;
  //
  if rbColorSchemeDark.Checked then begin
    GetCommonThemeManager.Theme := ttDark;
    NEDConfig.ColorSchema := 'dark';
    NEDConfig.SaveConfig;
  end;
end;

procedure TNEDHomeForm.rbColorSchemeLightClick(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;
  //
  if rbColorSchemeLight.Checked then begin
    GetCommonThemeManager.Theme := ttLight;
    NEDConfig.ColorSchema := 'light';
    NEDConfig.SaveConfig;
  end;
end;

procedure TNEDHomeForm.rbColorSchemeSystemClick(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;
  //
  if rbColorSchemeSystem.Checked then begin
    GetCommonThemeManager.Theme := ttSystem;
    NEDConfig.ColorSchema := 'system';
    NEDConfig.SaveConfig;
  end;
end;

procedure TNEDHomeForm.UButton1Click(Sender: TObject);
begin
//
end;

procedure TNEDHomeForm.UButton2Click(Sender: TObject);
begin
//
end;

procedure TNEDHomeForm.cbShowHomePageClick(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;
  //
  NEDConfig.ShowHomePage := cbShowHomePage.Checked;
  NEDConfig.SaveConfig;
end;

procedure TNEDHomeForm.USymbolButton1Click(Sender: TObject);
begin
//
end;

end.
