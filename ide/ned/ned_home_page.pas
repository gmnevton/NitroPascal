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
  Types,
  Controls,
  Graphics,
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
  UCL.Graphics,
  SplitEx,
  ned_profiles,
  ned_projects,
  ned_common_simple_types;

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
    sbFavoritesList: TUScrollBox;
    UText4: TUText;
    UPanel4: TUPanel;
    sbRecentsList: TUScrollBox;
    UText3: TUText;
    UItemButton2: TUItemButton;
    UItemButton1: TUItemButton;
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
    FProfile: TNEDProfile;
    //
    function GetNewButtonTop(const AParentControl: TUScrollBox): Integer;
    procedure ClearFavoritesList;
    procedure FillFavoritesList;
    procedure ClearRecentsList;
    procedure FillRecentsList;
    procedure DetailDrawText(Sender: TUItemButton; const ACanvas: TCanvas; ARect: TRect; AText: String);
    procedure TitleDrawText(Sender: TUItemButton; const ACanvas: TCanvas; ARect: TRect; AText: String);
  public
    procedure SyncProperties(const AProfile: TNEDProfile);
    function  FindProjectGroup(const AProjectPath: String): TNEDProject; overload;
    function  FindProjectGroup(const AProjectID: TNEDUniqueID): TNEDProject; overload;
    function  FindProject(const AProjectPath: String): TNEDProject; overload;
    function  FindProject(const AProjectID: TNEDUniqueID): TNEDProject; overload;
    //
    property Profile: TNEDProfile read FProfile;
  end;

var
  NEDHomeForm: TNEDHomeForm;

implementation

{$R *.dfm}

uses
  UCL.Types,
  UCL.Utils,
  ned_config,
  ned_session_context,
  ned_main;

procedure TNEDHomeForm.FormCreate(Sender: TObject);
begin
  FProfile := Nil;
end;

procedure TNEDHomeForm.FormDestroy(Sender: TObject);
begin
  FProfile := Nil;
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

procedure TNEDHomeForm.SyncProperties(const AProfile: TNEDProfile);
begin
  FProfile := AProfile;
  FillFavoritesList;
  FillRecentsList;
end;

function TNEDHomeForm.FindProjectGroup(const AProjectPath: String): TNEDProject;
begin
  Result := FProfile.Session.FindFavorite(ptProjectGroup, AProjectPath);
  if Result = Nil then
    Result := FProfile.Session.FindRecent(ptProjectGroup, AProjectPath);
end;

function TNEDHomeForm.FindProjectGroup(const AProjectID: TNEDUniqueID): TNEDProject;
begin
  Result := Nil;
end;

function TNEDHomeForm.FindProject(const AProjectPath: String): TNEDProject;
begin
  Result := FProfile.Session.FindFavorite(ptProject, AProjectPath);
  if Result = Nil then
    Result := FProfile.Session.FindRecent(ptProject, AProjectPath);
end;

function TNEDHomeForm.FindProject(const AProjectID: TNEDUniqueID): TNEDProject;
begin
  Result := Nil;
end;

function TNEDHomeForm.GetNewButtonTop(const AParentControl: TUScrollBox): Integer;
var
  i: Integer;
  Button: TUItemButton;
begin
  Result := 0;
  for i := 0 to AParentControl.ControlCount - 1 do begin
    if AParentControl.Controls[i] is TUItemButton then begin
      Button := TUItemButton(AParentControl.Controls[i]);
      if Result < Button.Top + Button.Height then
        Result := Button.Top + Button.Height;
    end;
  end;
end;

procedure TNEDHomeForm.ClearFavoritesList;
begin
  while sbFavoritesList.ComponentCount > 0 do begin
    if (sbFavoritesList.ComponentCount = 1) and not (sbFavoritesList.Components[0] is TUItemButton) then
      Break;
    if sbFavoritesList.Components[0] is TUItemButton then
      sbFavoritesList.Components[0].Free;
  end;
end;

procedure TNEDHomeForm.FillFavoritesList;
var
  Project: TNEDProject;
  ItemButton: TUItemButton;
  i: Integer;
begin
  ClearFavoritesList;
  if FProfile = Nil then
    Exit;
  //
  for i := 0 to FProfile.Session.ContextFavoritesCount - 1 do begin
    Project := FProfile.Session.Favorite[i];
    //
    ItemButton := TUItemButton.Create(sbFavoritesList);
    ItemButton.Visible := False;
    ItemButton.Top := GetNewButtonTop(sbFavoritesList) + 1;
    ItemButton.Height := 56;
    ItemButton.Parent := sbFavoritesList;
    ItemButton.Align := alTop;
    ItemButton.ObjectsVisible := [iokCheckBox, iokLeftIcon, iokText, iokDetail];
    ItemButton.LeftIcon := '';
    if Project.&Type = ptProject then
      ItemButton.LeftIcon := Char($E9F9) // ReportDocument
    else if Project.&Type = ptProjectGroup then
      ItemButton.LeftIcon := Char($F000) // KnowledgeArticle
    else if Project.&Type = ptFile then
      ItemButton.LeftIcon := Char($E943); // Code
    ItemButton.Detail := Format('Created: %s'#13#10'Modified: %s'#13#10'LastOpened: %s'#13#10'Opened: %d  Used: %dm',
      [DateTimeToStr(Project.CreateDate), DateTimeToStr(Project.ModifyDate), DateTimeToStr(Project.LastOpenedDate), Project.TimesOpened, Project.TimeUsedMinutes]);
    ItemButton.Text := Project.Name + ' (' + Project.FileName + ')' + #13#10 + Project.FilePath;
    ItemButton.Transparent := False;
    ItemButton.OnDrawDetail := DetailDrawText;
    ItemButton.OnDrawText := TitleDrawText;
    ItemButton.Tag := Integer(Project);
    ItemButton.Visible := True;
  end;
end;

procedure TNEDHomeForm.ClearRecentsList;
begin
  while sbRecentsList.ComponentCount > 0 do begin
    if (sbRecentsList.ComponentCount = 1) and not (sbRecentsList.Components[0] is TUItemButton) then
      Break;
    if sbRecentsList.Components[0] is TUItemButton then
      sbRecentsList.Components[0].Free;
  end;
end;

procedure TNEDHomeForm.FillRecentsList;
var
  Project: TNEDProject;
  ItemButton: TUItemButton;
  i: Integer;
begin
  ClearRecentsList;
  if FProfile = Nil then
    Exit;
  //
  for i := 0 to FProfile.Session.ContextRecentsCount - 1 do begin
    Project := FProfile.Session.Recent[i];
    //
    ItemButton := TUItemButton.Create(sbRecentsList);
    ItemButton.Visible := False;
    ItemButton.Top := GetNewButtonTop(sbRecentsList) + 1;
    ItemButton.Height := 56;
    ItemButton.Parent := sbRecentsList;
    ItemButton.Align := alTop;
    ItemButton.ObjectsVisible := [iokCheckBox, iokLeftIcon, iokText, iokDetail, iokRightIcon];
    ItemButton.LeftIcon := '';
    if Project.&Type = ptProject then
      ItemButton.LeftIcon := Char($E9F9) // ReportDocument
    else if Project.&Type = ptProjectGroup then
      ItemButton.LeftIcon := Char($F000) // KnowledgeArticle
    else if Project.&Type = ptFile then
      ItemButton.LeftIcon := Char($E943); // Code
    ItemButton.Detail := Format('Created: %s'#13#10'Modified: %s'#13#10'LastOpened: %s'#13#10'Opened: %d  Used: %dm',
      [DateTimeToStr(Project.CreateDate), DateTimeToStr(Project.ModifyDate), DateTimeToStr(Project.LastOpenedDate), Project.TimesOpened, Project.TimeUsedMinutes]);
    ItemButton.Text := Project.Name + ' (' + Project.FileName + ')' + #13#10 + Project.FilePath;
    ItemButton.Transparent := False;
    ItemButton.OnDrawDetail := DetailDrawText;
    ItemButton.OnDrawText := TitleDrawText;
    ItemButton.Tag := Integer(Project);
    ItemButton.Visible := True;
  end;
end;

procedure TNEDHomeForm.DetailDrawText(Sender: TUItemButton; const ACanvas: TCanvas; ARect: TRect; AText: String);
begin
  InflateRect(ARect, -2, -2);
  DrawTextRect(ACanvas, taRightJustify, taVerticalCenter, ARect, AText, True, False);
end;

procedure TNEDHomeForm.TitleDrawText(Sender: TUItemButton; const ACanvas: TCanvas; ARect: TRect; AText: String);
var
  L1Height, L2Height, Delta, i: Integer;
  L1Text, L2Text, L3Text: String;
  L1Rect, L2Rect, L3Rect: TRect;
  Project: TNEDProject;
begin
  i := Pos(#13#10, AText);
  if i > 0 then begin
    L1Text := AText.Substring(0, i - 1);
    L2Text := AText.Substring(i + 1);
    ACanvas.Font.Style := [fsBold];
    ACanvas.Font.Size := 11;
    L1Height := ACanvas.TextHeight(L1Text);
    //
    ACanvas.Font.Assign(Sender.Font);
    ACanvas.Font.Size := 7;
    L2Height := ACanvas.TextHeight(L2Text);
    //
    L1Rect := ARect;
    L1Rect.Top := 2;
    L1Rect.Bottom := L1Height + 2;
    //
    L2Rect := ARect;
    L2Rect.Top := L1Rect.Bottom;
    L2Rect.Bottom := L2Rect.Top + L2Height;
    //
    L3Rect := ARect;
    L3Rect.Top := ARect.Bottom - L2Height * 2 - 2;
    L3Rect.Bottom := L3Rect.Top + L2Height * 2;
    //
    ACanvas.Font.Style := [fsBold];
    ACanvas.Font.Size := 11;
    DrawTextRect(ACanvas, taLeftJustify, taAlignTop, L1Rect, L1Text, False, False);
    //
    ACanvas.Font.Assign(Sender.Font);
    ACanvas.Font.Size := 7;
    Delta := +80;
    if (GetCommonThemeManager.Theme = ttSystem) then begin
      if GetCommonThemeManager.ThemeUsed = utDark then
        Delta := -80;
    end
    else if (GetCommonThemeManager.Theme = ttDark) then
      Delta := -80;
    //
    ACanvas.Font.Color := BrightenColor(ACanvas.Font.Color, Delta);
    DrawTextRect(ACanvas, taLeftJustify, taAlignTop, L2Rect, L2Text, False, False);
    //
    Project := TNEDProject(Sender.Tag);
    L3Text := Project.Description;
    DrawTextRect(ACanvas, taLeftJustify, taAlignTop, L3Rect, L3Text, True, False);
  end
  else begin
    DrawTextRect(ACanvas, taLeftJustify, taVerticalCenter, ARect, AText, False, False);
  end;
end;

procedure TNEDHomeForm.UHyperLink1Click(Sender: TObject);
begin
//
end;

procedure TNEDHomeForm.UHyperLink2Click(Sender: TObject);
begin
  TNEDMainForm(Application.MainForm).Open1.Click;
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
