inherited NEDDialogMessage: TNEDDialogMessage
  Caption = 'NEDDialogMessage'
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object txtIcon: TUText [2]
    Left = 40
    Top = 72
    Width = 43
    Height = 43
    Caption = #59322
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -43
    Font.Name = 'Segoe MDL2 Assets'
    Font.Style = []
    ParentFont = False
  end
  object txtMessage: TUText [3]
    Left = 96
    Top = 72
    Width = 457
    Height = 297
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 
      'Current workspace is not empty.'#13#10'While opening project group "%s' +
      '",'#13#10'would You like to:'#13#10'  [C]lear workspace and open selected pr' +
      'oject group'#13#10'or'#13#10'  [A]dd to this workspace ?'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    StyleElements = []
    TextKind = tkHeading
  end
  inherited UPanel3: TUPanel
    inherited btnCancel: TUButton
      Visible = False
    end
    inherited btnOK: TUButton
      Visible = False
    end
  end
end
