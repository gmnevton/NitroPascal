inherited NEDDialogProfiles: TNEDDialogProfiles
  Caption = 'NEDDialogProfiles'
  PixelsPerInch = 96
  TextHeight = 13
  inherited UTitleBar1: TUTitleBar
    Caption = 'Profiles...'
  end
  object SplitterEx1: TSplitterEx [2]
    Left = 201
    Top = 73
    Width = 7
    Height = 333
    AssignedControl = UPanel2
    AutoSnap = False
    DrawSpacer = True
    MinSize = 200
    ResizeStyle = rsUpdate
    Visible = False
    ExplicitTop = 74
    ExplicitHeight = 291
  end
  inherited UPanel3: TUPanel
    inherited btnOK: TUButton
      Caption = 'Select'
    end
  end
  object UPanel1: TUPanel [4]
    Left = 1
    Top = 32
    Width = 598
    Height = 41
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ShowCaption = False
    TabOrder = 1
    BackColor.Enabled = False
    BackColor.Color = clBlack
    BackColor.LightColor = 15132390
    BackColor.DarkColor = 2039583
    object txtPath: TUText
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 582
      Height = 25
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alClient
      AutoSize = False
      Caption = 'Select, open or import/create profile(s):'
      EllipsisPosition = epPathEllipsis
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      Transparent = True
      Layout = tlCenter
      ExplicitLeft = 53
      ExplicitWidth = 39
      ExplicitHeight = 17
    end
  end
  object UPanel2: TUPanel [5]
    Left = 1
    Top = 73
    Width = 200
    Height = 333
    Align = alLeft
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ShowCaption = False
    TabOrder = 2
    BackColor.Enabled = False
    BackColor.Color = clBtnFace
    BackColor.LightColor = 15132390
    BackColor.DarkColor = 2039583
    object UPanel4: TUPanel
      Left = 0
      Top = 0
      Width = 200
      Height = 40
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ShowCaption = False
      TabOrder = 0
      BackColor.Enabled = True
      BackColor.Color = clBlack
      BackColor.LightColor = 15132390
      BackColor.DarkColor = 4210752
      object btnProfileAdd: TUQuickButton
        Left = 20
        Top = 0
        Height = 40
        Hint = 'Add new profile'
        Align = alRight
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Segoe MDL2 Assets'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = btnProfileAddClick
        BackColors.Enabled = False
        BackColors.Color = clBlack
        BackColors.LightColor = 13619151
        BackColors.DarkColor = 3947580
        Caption = #60686
        ExplicitLeft = 16
        ExplicitTop = 6
        ExplicitHeight = 32
      end
      object btnProfileEdit: TUQuickButton
        Left = 110
        Top = 0
        Height = 40
        Hint = 'Edit profile'
        Align = alRight
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Segoe MDL2 Assets'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = btnProfileEditClick
        BackColors.Enabled = False
        BackColors.Color = clBlack
        BackColors.LightColor = 13619151
        BackColors.DarkColor = 3947580
        Caption = #59151
        ExplicitLeft = 162
        ExplicitTop = 3
      end
      object btnProfileDel: TUQuickButton
        Left = 155
        Top = 0
        Height = 40
        Hint = 'Delete profile'
        Align = alRight
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Segoe MDL2 Assets'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = btnProfileDelClick
        BackColors.Enabled = False
        BackColors.Color = clBlack
        BackColors.LightColor = 13619151
        BackColors.DarkColor = 3947580
        Caption = #59213
        ExplicitLeft = 174
        ExplicitTop = 3
      end
      object btnProfileImport: TUQuickButton
        Left = 65
        Top = 0
        Height = 40
        Hint = 'Import profile'
        Align = alRight
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Segoe MDL2 Assets'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = btnProfileAddClick
        BackColors.Enabled = False
        BackColors.Color = clBlack
        BackColors.LightColor = 13619151
        BackColors.DarkColor = 3947580
        Caption = #59446
        ExplicitLeft = 84
        ExplicitTop = 3
      end
    end
  end
  object UPanel5: TUPanel [6]
    Left = 208
    Top = 73
    Width = 391
    Height = 333
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = True
    ParentFont = False
    ShowCaption = False
    TabOrder = 3
    Visible = False
    BackColor.Enabled = False
    BackColor.Color = clBlack
    BackColor.LightColor = 15132390
    BackColor.DarkColor = 4210752
    DesignSize = (
      391
      333)
    object UText1: TUText
      Left = 6
      Top = 6
      Width = 122
      Height = 28
      Caption = 'Profile details:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -20
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TextKind = tkHeading
    end
    object UText2: TUText
      Left = 12
      Top = 55
      Width = 39
      Height = 17
      Caption = 'Name:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI Semibold'
      Font.Style = []
      ParentFont = False
      TextKind = tkEntry
    end
    object UText3: TUText
      Left = 12
      Top = 127
      Width = 161
      Height = 17
      Caption = 'Copy settings from profile:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI Semibold'
      Font.Style = []
      ParentFont = False
      TextKind = tkEntry
    end
    object UText4: TUText
      Left = 12
      Top = 191
      Width = 119
      Height = 17
      Caption = 'Import profile path:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI Semibold'
      Font.Style = []
      ParentFont = False
      TextKind = tkEntry
    end
    object edProfileInputName: TUEdit
      Left = 12
      Top = 78
      Width = 365
      ParentColor = False
      ParentFont = False
      Anchors = [akLeft, akTop, akRight]
      BevelOuter = bvNone
      Color = clBlack
      TabOrder = 0
      BorderThickness = 1
      BackColor.Enabled = False
      BackColor.Color = clWhite
      BackColor.LightColor = clWhite
      BackColor.DarkColor = clBlack
      BackColor.FocusedLightColor = clBlack
      BackColor.FocusedDarkColor = clBlack
      BorderColor.Enabled = False
      BorderColor.Color = clBlack
      BorderColor.LightColor = 10066329
      BorderColor.DarkColor = 6710886
      BorderColor.FocusedLightColor = 14120960
      BorderColor.FocusedDarkColor = 14120960
    end
    object edProfileInputImportPath: TUEdit
      Left = 12
      Top = 214
      Width = 333
      ParentColor = False
      ParentFont = False
      Anchors = [akLeft, akTop, akRight]
      BevelOuter = bvNone
      Color = 14211288
      Enabled = False
      ReadOnly = True
      TabOrder = 1
      BorderThickness = 1
      BackColor.Enabled = False
      BackColor.Color = clWhite
      BackColor.LightColor = clWhite
      BackColor.DarkColor = clBlack
      BackColor.FocusedLightColor = clBlack
      BackColor.FocusedDarkColor = clBlack
      BorderColor.Enabled = False
      BorderColor.Color = clBlack
      BorderColor.LightColor = 10066329
      BorderColor.DarkColor = 6710886
      BorderColor.FocusedLightColor = 14120960
      BorderColor.FocusedDarkColor = 14120960
      ControlState = csDisabled
    end
    object UCheckBox1: TUCheckBox
      Left = 12
      Top = 250
      Width = 98
      Anchors = [akLeft, akTop, akRight]
      AutoSize = True
      TabOrder = 3
      IconFont.Charset = DEFAULT_CHARSET
      IconFont.Color = clWindowText
      IconFont.Height = -20
      IconFont.Name = 'Segoe MDL2 Assets'
      IconFont.Style = []
      Caption = 'Set as default'
    end
    object btnProfileInputImportBrowse: TUButton
      Left = 353
      Top = 214
      Width = 24
      Anchors = [akTop, akRight]
      Enabled = False
      TabOrder = 2
      OnClick = btnProfileInputImportBrowseClick
      CustomColors.BackColors.Enabled = False
      CustomColors.BackColors.LightColor = 13421772
      CustomColors.BackColors.LightHover = 13421772
      CustomColors.BackColors.LightPress = 10066329
      CustomColors.BackColors.LightDisabled = 13421772
      CustomColors.BackColors.LightFocused = 13421772
      CustomColors.BackColors.DarkColor = 3355443
      CustomColors.BackColors.DarkHover = 3355443
      CustomColors.BackColors.DarkPress = 6710886
      CustomColors.BackColors.DarkDisabled = 3355443
      CustomColors.BackColors.DarkFocused = 3355443
      CustomColors.BorderColors.Enabled = False
      CustomColors.BorderColors.LightColor = 13421772
      CustomColors.BorderColors.LightHover = 8026746
      CustomColors.BorderColors.LightPress = 10066329
      CustomColors.BorderColors.LightDisabled = 8026746
      CustomColors.BorderColors.LightFocused = 8026746
      CustomColors.BorderColors.DarkColor = 3355443
      CustomColors.BorderColors.DarkHover = 8750469
      CustomColors.BorderColors.DarkPress = 6710886
      CustomColors.BorderColors.DarkDisabled = 8750469
      CustomColors.BorderColors.DarkFocused = 8750469
      CustomColors.TextColors.Enabled = False
      CustomColors.TextColors.LightColor = clBlack
      CustomColors.TextColors.LightHover = clBlack
      CustomColors.TextColors.LightPress = clBlack
      CustomColors.TextColors.LightDisabled = clGray
      CustomColors.TextColors.LightFocused = clBlack
      CustomColors.TextColors.DarkColor = clWhite
      CustomColors.TextColors.DarkHover = clWhite
      CustomColors.TextColors.DarkPress = clWhite
      CustomColors.TextColors.DarkDisabled = clGray
      CustomColors.TextColors.DarkFocused = clWhite
      BorderThickness = 2
      ButtonState = csDisabled
      Highlight = True
      Caption = '...'
    end
    object btnProfileInputCancel: TUButton
      Left = 171
      Top = 286
      Width = 100
      Height = 41
      Anchors = [akTop, akRight]
      TabOrder = 4
      OnClick = btnProfileInputCancelClick
      CustomColors.BackColors.Enabled = False
      CustomColors.BackColors.LightColor = 13421772
      CustomColors.BackColors.LightHover = 13421772
      CustomColors.BackColors.LightPress = 10066329
      CustomColors.BackColors.LightDisabled = 13421772
      CustomColors.BackColors.LightFocused = 13421772
      CustomColors.BackColors.DarkColor = 3355443
      CustomColors.BackColors.DarkHover = 3355443
      CustomColors.BackColors.DarkPress = 6710886
      CustomColors.BackColors.DarkDisabled = 3355443
      CustomColors.BackColors.DarkFocused = 3355443
      CustomColors.BorderColors.Enabled = False
      CustomColors.BorderColors.LightColor = 13421772
      CustomColors.BorderColors.LightHover = 8026746
      CustomColors.BorderColors.LightPress = 10066329
      CustomColors.BorderColors.LightDisabled = 8026746
      CustomColors.BorderColors.LightFocused = 8026746
      CustomColors.BorderColors.DarkColor = 3355443
      CustomColors.BorderColors.DarkHover = 8750469
      CustomColors.BorderColors.DarkPress = 6710886
      CustomColors.BorderColors.DarkDisabled = 8750469
      CustomColors.BorderColors.DarkFocused = 8750469
      CustomColors.TextColors.Enabled = False
      CustomColors.TextColors.LightColor = clBlack
      CustomColors.TextColors.LightHover = clBlack
      CustomColors.TextColors.LightPress = clBlack
      CustomColors.TextColors.LightDisabled = clGray
      CustomColors.TextColors.LightFocused = clBlack
      CustomColors.TextColors.DarkColor = clWhite
      CustomColors.TextColors.DarkHover = clWhite
      CustomColors.TextColors.DarkPress = clWhite
      CustomColors.TextColors.DarkDisabled = clGray
      CustomColors.TextColors.DarkFocused = clWhite
      BorderThickness = 2
      Caption = 'Cancel'
    end
    object btnProfileInputAdd: TUButton
      Left = 277
      Top = 286
      Width = 100
      Height = 41
      Anchors = [akTop, akRight]
      TabOrder = 5
      OnClick = btnProfileInputAddClick
      CustomColors.BackColors.Enabled = False
      CustomColors.BackColors.LightColor = 13421772
      CustomColors.BackColors.LightHover = 13421772
      CustomColors.BackColors.LightPress = 10066329
      CustomColors.BackColors.LightDisabled = 13421772
      CustomColors.BackColors.LightFocused = 13421772
      CustomColors.BackColors.DarkColor = 3355443
      CustomColors.BackColors.DarkHover = 3355443
      CustomColors.BackColors.DarkPress = 6710886
      CustomColors.BackColors.DarkDisabled = 3355443
      CustomColors.BackColors.DarkFocused = 3355443
      CustomColors.BorderColors.Enabled = False
      CustomColors.BorderColors.LightColor = 13421772
      CustomColors.BorderColors.LightHover = 8026746
      CustomColors.BorderColors.LightPress = 10066329
      CustomColors.BorderColors.LightDisabled = 8026746
      CustomColors.BorderColors.LightFocused = 8026746
      CustomColors.BorderColors.DarkColor = 3355443
      CustomColors.BorderColors.DarkHover = 8750469
      CustomColors.BorderColors.DarkPress = 6710886
      CustomColors.BorderColors.DarkDisabled = 8750469
      CustomColors.BorderColors.DarkFocused = 8750469
      CustomColors.TextColors.Enabled = False
      CustomColors.TextColors.LightColor = clBlack
      CustomColors.TextColors.LightHover = clBlack
      CustomColors.TextColors.LightPress = clBlack
      CustomColors.TextColors.LightDisabled = clGray
      CustomColors.TextColors.LightFocused = clBlack
      CustomColors.TextColors.DarkColor = clWhite
      CustomColors.TextColors.DarkHover = clWhite
      CustomColors.TextColors.DarkPress = clWhite
      CustomColors.TextColors.DarkDisabled = clGray
      CustomColors.TextColors.DarkFocused = clWhite
      BorderThickness = 2
      Highlight = True
      Caption = 'Add'
    end
  end
  inherited ImageList1: TImageList
    Left = 288
    Top = 0
  end
end
