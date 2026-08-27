//
// Nitro EDitor
// version 1.0
//
// Author: Grzegorz Molenda
// Created: 2024-12-27
// Modified: 2026-08
// All rights reserved.
//

unit ned_dialog_message;

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
  UITypes,
  UCL.Classes,
  UCL.Panel,
  UCL.TitleBar,
  UCL.Button,
  UCL.QuickButton,
  UCL.Text,
  ned_dialog_base;

type
  TNEDDialogTypeEnum = (
    dtWarning,
    dtError,
    dtInformation,
    dtConfirmation,
    dtCustom
  );

  TNEDDialogButton = record
    &Type: TMsgDlgBtn;
    Caption: String;
    Default: Boolean;
  end;
  TNEDDialogButtons = Array of TNEDDialogButton;

  TNEDDialogMessage = class(TNEDDialogBase)
    txtIcon: TUText;
    txtMessage: TUText;
    //
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FType: TNEDDialogTypeEnum;
    FTypeIcons: Array[TNEDDialogTypeEnum] of UTF8Char; // (#$E7BA, #$E783, #$E946, #$E9CE, #$E77B);
    FButtons: TNEDDialogButtons;
    FCustomIcon: UTF8Char;
    //
    procedure SetCustomIcon(const Value: UTF8Char);
  protected
    function CalculateMessageWindowHeight: Integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //
    function Execute(const ADialogType: TNEDDialogTypeEnum; const ADialogButtons: TNEDDialogButtons; const ACaption, AMessage: String): Boolean; reintroduce;
    //
    property CustomIcon: UTF8Char read FCustomIcon write SetCustomIcon;
  end;

implementation

{$R *.dfm}

uses
  Graphics;

{ TNEDDialogMessage }

constructor TNEDDialogMessage.Create(AOwner: TComponent);
begin
  inherited;
  FType := dtCustom;
  FTypeIcons[dtWarning]      := #$E7BA;
  FTypeIcons[dtError]        := #$E783;
  FTypeIcons[dtInformation]  := #$E946;
  FTypeIcons[dtConfirmation] := #$E9CE;
  FTypeIcons[dtCustom]       := #$E77B;
  SetLength(FButtons, 0);
  txtIcon.Caption := FTypeIcons[FType];
end;

destructor TNEDDialogMessage.Destroy;
begin
  UTitleBar1.Caption := '';
  txtIcon.Caption := '';
  txtMessage.Caption := '';
  inherited;
end;

procedure TNEDDialogMessage.SetCustomIcon(const Value: UTF8Char);
begin
  if Value <> #0 then
    FTypeIcons[dtCustom] := Value
  else
    FTypeIcons[dtCustom] := #$E77B;
end;

function TNEDDialogMessage.Execute(const ADialogType: TNEDDialogTypeEnum; const ADialogButtons: TNEDDialogButtons; const ACaption, AMessage: String): Boolean;
begin
  FType := ADialogType;
  FButtons := ADialogButtons;
  UTitleBar1.Caption := ACaption;
  txtIcon.Caption := FTypeIcons[ADialogType];
  txtMessage.Caption := AMessage;
  Height := CalculateMessageWindowHeight;
  Result := inherited Execute;
end;

procedure TNEDDialogMessage.FormCreate(Sender: TObject);
begin
  inherited;
  Height := 200;
end;

procedure TNEDDialogMessage.FormDestroy(Sender: TObject);
begin
  inherited;
//
end;

procedure TNEDDialogMessage.FormResize(Sender: TObject);
begin
  inherited;
//
end;

function TNEDDialogMessage.CalculateMessageWindowHeight: Integer;
begin
  Result := 72 + txtMessage.Canvas.TextExtent(txtMessage.Caption).cy + 78;
end;

end.

