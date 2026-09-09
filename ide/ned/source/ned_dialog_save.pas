//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-07
// All rights reserved.
//

unit ned_dialog_save;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  ImageList,
  ImgList,
  ned_dialog_base,
  UCL.Classes,
  UCL.Button,
  Vcl.ExtCtrls,
  UCL.Panel,
  UCL.QuickButton,
  UCL.TitleBar;

type
  TNEDDialogSave = class(TNEDDialogBase)
  private
  public
  end;

var
  NEDDialogSave: TNEDDialogSave;

implementation

{$R *.dfm}

end.

