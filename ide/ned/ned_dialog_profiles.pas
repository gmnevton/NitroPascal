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
  ExtCtrls,
  Forms,
  ImageList,
  ImgList,
  ned_dialog_base,
  UCL.Button,
  UCL.Panel,
  UCL.QuickButton,
  UCL.TitleBar, SplitEx, Vcl.StdCtrls, UCL.Text, UCL.Edit, UCL.CheckBox;

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
    UCheckBox1: TUCheckBox;
    btnProfileInputImportBrowse: TUButton;
    btnProfileAdd: TUQuickButton;
    btnProfileInputCancel: TUButton;
    btnProfileInputAdd: TUButton;
    btnProfileEdit: TUQuickButton;
    btnProfileDel: TUQuickButton;
    btnProfileImport: TUQuickButton;
    procedure FormCreate(Sender: TObject);
    procedure btnProfileAddClick(Sender: TObject);
    procedure btnProfileEditClick(Sender: TObject);
    procedure btnProfileDelClick(Sender: TObject);
    procedure btnProfileInputImportBrowseClick(Sender: TObject);
    procedure btnProfileInputCancelClick(Sender: TObject);
    procedure btnProfileInputAddClick(Sender: TObject);
  private
  public
  end;

implementation

{$R *.dfm}

procedure TNEDDialogProfiles.FormCreate(Sender: TObject);
begin
  inherited;
  UPanel2.Width := ClientWidth - 2;
  UPanel2.Anchors := UPanel2.Anchors + [akRight];
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

procedure TNEDDialogProfiles.btnProfileInputCancelClick(Sender: TObject);
begin
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
begin
  UPanel5.Visible := False;
  SplitterEx1.Visible := False;
  UPanel2.Width := ClientWidth - 2;
  UPanel2.Anchors := UPanel2.Anchors + [akRight];
  btnProfileAdd.Enabled := True;
  btnProfileImport.Enabled := True;
  btnProfileEdit.Enabled := True;
  btnProfileDel.Enabled := True;
end;

end.

