//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-07
// All rights reserved.
//

unit ned_dialog_profiles;

interface

uses
  SysUtils,
  Classes,
  Controls,
  StdCtrls,
  ExtCtrls,
  Forms,
  ImageList,
  ImgList,
  ned_dialog_base,
  SplitEx,
  UCL.Button,
  UCL.Panel,
  UCL.QuickButton,
  UCL.TitleBar,
  UCL.Text,
  UCL.Edit,
  UCL.CheckBox,
  uFolders,
  ned_profiles;

type
  TNEDDialogProfiles = class(TNEDDialogBase)
    UPanel1: TUPanel;
    txtPath: TUText;
    UPanel2: TUPanel;
    SplitterEx1: TSplitterEx;
    UPanel4: TUPanel;
    UPanel5: TUPanel;
    UText1: TUText;
    UText2: TUText;
    edProfileInputName: TUEdit;
    UText3: TUText;
    UText4: TUText;
    edProfileInputImportPath: TUEdit;
    cbProfileInputSetDefault: TUCheckBox;
    btnProfileInputImportBrowse: TUButton;
    btnProfileAdd: TUQuickButton;
    btnProfileInputCancel: TUButton;
    btnProfileInputAdd: TUButton;
    btnProfileEdit: TUQuickButton;
    btnProfileDel: TUQuickButton;
    btnProfileImport: TUQuickButton;
    //
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    //
    procedure btnProfileAddClick(Sender: TObject);
    procedure btnProfileEditClick(Sender: TObject);
    procedure btnProfileDelClick(Sender: TObject);
    procedure btnProfileInputImportBrowseClick(Sender: TObject);
    procedure btnProfileInputCancelClick(Sender: TObject);
    procedure btnProfileInputAddClick(Sender: TObject);
    procedure edProfileInputNameChange(Sender: TObject);
    procedure edProfileInputImportPathChange(Sender: TObject);
    procedure cbProfileInputSetDefaultClick(Sender: TObject);
    procedure edProfileInputNameKeyPress(Sender: TObject; var Key: Char);
  private
    FProfilesPath: String;
    ProfilesView: TEntryView;
    FSelectedProfile: TNEDProfile;
    //
    procedure CheckInputForm;
    procedure ProfilesViewItemGetType(Sender: TObject; Item: TEntryItem; var ItemType: TEntryItemTypeEnum); // event
    procedure ProfilesViewItemSelection(Sender: TObject; Item: TEntryItem; IsSubDirectory: Boolean); // event
  public
    procedure FillProfilesList;
    //
    property SelectedProfile: TNEDProfile read FSelectedProfile;
  end;

implementation

{$R *.dfm}

uses
  Graphics,
  ned_config;

procedure TNEDDialogProfiles.FormCreate(Sender: TObject);
begin
  inherited;
  //
  ProfilesView := TEntryView.Create(Self);
  ProfilesView.Name := 'ProfilesView';
  ProfilesView.Parent := UPanel2;
  ProfilesView.Align := alClient;
  ProfilesView.ParentColor := True;
  ProfilesView.BorderStyle := bsNone;
  ProfilesView.EntryImages := ImageList1;
  ProfilesView.TabStop := True;
  ProfilesView.OnItemGetType := ProfilesViewItemGetType;
  ProfilesView.OnItemSelection := ProfilesViewItemSelection;
  //
  UPanel2.Width := ClientWidth - 2;
  UPanel2.Anchors := UPanel2.Anchors + [akRight];
  FProfilesPath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  FProfilesPath := FProfilesPath + 'profiles';
  CreateDir(FProfilesPath);
  //
  FSelectedProfile := Nil;
end;

procedure TNEDDialogProfiles.FormDestroy(Sender: TObject);
begin
  FSelectedProfile := Nil;
  FProfilesPath := '';
  ProfilesView.Free;
  inherited;
end;

procedure TNEDDialogProfiles.FormShow(Sender: TObject);
begin
  inherited;
  FillProfilesList;
  ProfilesView.Show;
end;

procedure TNEDDialogProfiles.CheckInputForm;
var
  ok: Boolean;
begin
  btnProfileInputAdd.Enabled := False;
  edProfileInputName.ResetErrorState;
  //
  ok := False;
  if Length(edProfileInputName.Text) > 0 then begin
    // forbidden names: CON, PRN, AUX, NUL, COM0 - COM9, LPT0 - LPT9
    // no '.' at the end
    ok := not SameText(edProfileInputName.Text, 'CON') and
          not SameText(edProfileInputName.Text, 'PRN') and
          not SameText(edProfileInputName.Text, 'AUX') and
          not SameText(edProfileInputName.Text, 'NUL') and
          not SameText(edProfileInputName.Text, 'COM') and
          not SameText(edProfileInputName.Text, 'COM0') and
          not SameText(edProfileInputName.Text, 'COM1') and
          not SameText(edProfileInputName.Text, 'COM2') and
          not SameText(edProfileInputName.Text, 'COM3') and
          not SameText(edProfileInputName.Text, 'COM4') and
          not SameText(edProfileInputName.Text, 'COM5') and
          not SameText(edProfileInputName.Text, 'COM6') and
          not SameText(edProfileInputName.Text, 'COM7') and
          not SameText(edProfileInputName.Text, 'COM8') and
          not SameText(edProfileInputName.Text, 'COM9') and
          not SameText(edProfileInputName.Text, 'LPT') and
          not SameText(edProfileInputName.Text, 'LPT0') and
          not SameText(edProfileInputName.Text, 'LPT1') and
          not SameText(edProfileInputName.Text, 'LPT2') and
          not SameText(edProfileInputName.Text, 'LPT3') and
          not SameText(edProfileInputName.Text, 'LPT4') and
          not SameText(edProfileInputName.Text, 'LPT5') and
          not SameText(edProfileInputName.Text, 'LPT6') and
          not SameText(edProfileInputName.Text, 'LPT7') and
          not SameText(edProfileInputName.Text, 'LPT8') and
          not SameText(edProfileInputName.Text, 'LPT9') and
          not String(edProfileInputName.Text).EndsWith('.') and
          (Length(FProfilesPath + '\' + edProfileInputName.Text) <= 260) and
          not DirectoryExists(FProfilesPath + '\' + edProfileInputName.Text);
    if not ok then begin
      edProfileInputName.SetErrorState;
    end;
  end;
  //
  btnProfileInputAdd.Enabled := ok;
end;

procedure TNEDDialogProfiles.ProfilesViewItemGetType(Sender: TObject; Item: TEntryItem; var ItemType: TEntryItemTypeEnum);
begin
  ItemType := etFile;
end;

procedure TNEDDialogProfiles.ProfilesViewItemSelection(Sender: TObject; Item: TEntryItem; IsSubDirectory: Boolean);
var
  Idx: Integer;
begin
  if IsSubDirectory then begin
    // for future use
  end
  else begin
    Idx := Integer(Item.Data); // we have Profile.Index here
    FSelectedProfile := NEDConfig.Profiles.Profile[Idx];
    if FSelectedProfile <> Nil then
      btnOKClick(Nil);
  end;
end;

procedure TNEDDialogProfiles.FillProfilesList;
var
  i: Integer;
  Profile: TNEDProfile;
  ProfileView: TEntryItem;
begin
  ProfilesView.BeginUpdate;
  try
    ProfilesView.Entries.Clear;
    //
    for i := 0 to NEDConfig.Profiles.Count - 1 do begin
      Profile := NEDConfig.Profiles.Profile[i];
      //
      ProfileView := ProfilesView.Entries.Add;
      ProfileView.Caption := Profile.Name;
      ProfileView.Data := Pointer(Profile.Index); // store profile Index as pointer; it doesn't matter
    end;
  finally
    ProfilesView.EndUpdate;
    ProfilesView.ActiveIndex := 0;
  end;
end;

procedure TNEDDialogProfiles.btnProfileAddClick(Sender: TObject);
begin
  btnProfileAdd.Enabled := False;
  btnProfileImport.Enabled := False;
  btnProfileEdit.Enabled := False;
  btnProfileDel.Enabled := False;
  //
  UPanel2.Anchors := UPanel2.Anchors - [akRight];
  UPanel2.Width := 200;
  SplitterEx1.Visible := True;
  UPanel5.Visible := True;
  edProfileInputImportPath.Enabled := False;
  btnProfileInputImportBrowse.Enabled := False;
  if Sender = btnProfileImport then begin
    edProfileInputImportPath.Enabled := True;
    btnProfileInputImportBrowse.Enabled := True;
  end;
  //
  btnProfileInputCancel.Enabled := True;
  btnProfileInputAdd.Caption := 'Add';
  btnProfileInputAdd.Enabled := False; // defaults to false, because we need to fill-in the form first
end;

procedure TNEDDialogProfiles.btnProfileEditClick(Sender: TObject);
begin
  btnProfileAdd.Enabled := False;
  btnProfileImport.Enabled := False;
  btnProfileEdit.Enabled := False;
  btnProfileDel.Enabled := False;
  //
  UPanel2.Anchors := UPanel2.Anchors - [akRight];
  UPanel2.Width := 200;
  SplitterEx1.Visible := True;
  UPanel5.Visible := True;
  edProfileInputImportPath.Enabled := False;
  btnProfileInputImportBrowse.Enabled := False;
  //
  btnProfileInputCancel.Enabled := True;
  btnProfileInputAdd.Caption := 'Save';
  btnProfileInputAdd.Enabled := False;
end;

procedure TNEDDialogProfiles.btnProfileDelClick(Sender: TObject);
begin
  btnProfileAdd.Enabled := False;
  btnProfileImport.Enabled := False;
  btnProfileEdit.Enabled := False;
  btnProfileDel.Enabled := False;
  //
end;

procedure TNEDDialogProfiles.btnProfileInputImportBrowseClick(Sender: TObject);
begin
//
end;

procedure TNEDDialogProfiles.edProfileInputNameChange(Sender: TObject);
begin
  CheckInputForm;
end;

procedure TNEDDialogProfiles.edProfileInputNameKeyPress(Sender: TObject; var Key: Char);
begin
  // forbidden chars: <, >, :, ", /, \, |, ?, and *
  if Key in [' ', '<', '>', ':', '"', '/', '\', '|', '?', '*'] then
    Key := #0;
end;

procedure TNEDDialogProfiles.edProfileInputImportPathChange(Sender: TObject);
begin
  CheckInputForm;
end;

procedure TNEDDialogProfiles.cbProfileInputSetDefaultClick(Sender: TObject);
begin
  CheckInputForm;
end;

procedure TNEDDialogProfiles.btnProfileInputCancelClick(Sender: TObject);
begin
  btnProfileInputCancel.Enabled := False;
  //
  UPanel5.Visible := False;
  SplitterEx1.Visible := False;
  UPanel2.Width := ClientWidth - 2;
  UPanel2.Anchors := UPanel2.Anchors + [akRight];
  btnProfileAdd.Enabled := True;
  btnProfileImport.Enabled := True;
  btnProfileEdit.Enabled := True;
  btnProfileDel.Enabled := True;
end;

procedure TNEDDialogProfiles.btnProfileInputAddClick(Sender: TObject);
var
  Profile: TNEDProfile;
begin
  btnProfileInputAdd.Enabled := False;
  //
  UPanel5.Visible := False;
  SplitterEx1.Visible := False;
  UPanel2.Width := ClientWidth - 2;
  UPanel2.Anchors := UPanel2.Anchors + [akRight];
  btnProfileAdd.Enabled := True;
  btnProfileImport.Enabled := True;
  btnProfileEdit.Enabled := True;
  btnProfileDel.Enabled := True;
  //
  if CreateDir(FProfilesPath + '\' + edProfileInputName.Text) then begin
    Profile := NEDConfig.Profiles.AddNew(edProfileInputName.Text, cbProfileInputSetDefault.Checked);
  //  if edProfileInputImportPath.Enabled and (Length(edProfileInputImportPath.Text) > 0) then
  //    Profile.CopyFrom('');
    NEDConfig.Update;
    NEDConfig.SaveConfig;
    FillProfilesList;
  end
  else begin
    // @TODO: can't create profile directory, show some error or something
  end;
end;

end.

