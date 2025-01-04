object NEDMainForm: TNEDMainForm
  Left = 0
  Top = 0
  Caption = 'NED'
  ClientHeight = 561
  ClientWidth = 884
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 900
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCanResize = FormCanResize
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnKeyUp = FormKeyUp
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object splLeft: TSplitterEx
    Left = 245
    Top = 40
    Width = 7
    Height = 489
    AssignedControl = pnlLeft
    AutoSnap = False
    DrawSpacer = True
    MinSize = 200
    ResizeStyle = rsUpdate
    ExplicitLeft = 230
    ExplicitHeight = 728
  end
  object barCaption: TUCaptionBar
    Left = 0
    Top = 0
    Width = 884
    Height = 40
    Caption = '                  Nitro EDitor'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 0
    BackColors.Enabled = False
    BackColors.Color = clBlack
    BackColors.LightColor = 15921906
    BackColors.DarkColor = 2829099
    BackColors.FocusedLightColor = 14120960
    BackColors.FocusedDarkColor = 1525760
    Menu = mnuMain
    MenuController.ButtonWidth = 54
    MenuController.ButtonHeight = 40
    MenuController.PosX = 180
    MenuController.Transparent = False
    MenuController.Font.Charset = DEFAULT_CHARSET
    MenuController.Font.Color = clWindowText
    MenuController.Font.Height = -12
    MenuController.Font.Name = 'Segoe UI'
    MenuController.Font.Style = []
    MenuController.ButtonBackColors.Enabled = False
    MenuController.ButtonBackColors.LightColor = 15921906
    MenuController.ButtonBackColors.LightHover = 14120960
    MenuController.ButtonBackColors.LightDisabled = clGray
    MenuController.ButtonBackColors.DarkColor = 2829099
    MenuController.ButtonBackColors.DarkHover = 1525760
    MenuController.ButtonBackColors.DarkDisabled = clGray
    MenuController.ButtonTextColors.Enabled = False
    MenuController.ButtonTextColors.LightColor = clBlack
    MenuController.ButtonTextColors.LightHover = clWhite
    MenuController.ButtonTextColors.LightDisabled = clGray
    MenuController.ButtonTextColors.DarkColor = clWhite
    MenuController.ButtonTextColors.DarkHover = clWhite
    MenuController.ButtonTextColors.DarkDisabled = clGray
    MenuController.MenuBorderColors.Enabled = False
    MenuController.MenuBorderColors.Color = clDefault
    MenuController.MenuBorderColors.LightColor = clBlack
    MenuController.MenuBorderColors.DarkColor = clSilver
    MenuController.MenuBackColors.Enabled = False
    MenuController.MenuBackColors.LightColor = clWhite
    MenuController.MenuBackColors.LightHover = clHighlight
    MenuController.MenuBackColors.LightDisabled = clGray
    MenuController.MenuBackColors.DarkColor = clBlack
    MenuController.MenuBackColors.DarkHover = clHighlight
    MenuController.MenuBackColors.DarkDisabled = clGray
    MenuController.MenuTextColors.Enabled = False
    MenuController.MenuTextColors.LightColor = clBlack
    MenuController.MenuTextColors.LightHover = clBlack
    MenuController.MenuTextColors.LightDisabled = clGray
    MenuController.MenuTextColors.DarkColor = clWhite
    MenuController.MenuTextColors.DarkHover = clWhite
    MenuController.MenuTextColors.DarkDisabled = clGray
    MenuOffset = 180
    UseSystemCaptionColor = True
    CaptionHeight = 40
    object btnClose: TUQuickButton
      Left = 839
      Top = 0
      Height = 40
      Align = alRight
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe MDL2 Assets'
      Font.Style = []
      ParentFont = False
      BackColors.Enabled = False
      BackColors.Color = clBlack
      BackColors.LightColor = 13619151
      BackColors.DarkColor = 3947580
      ButtonStyle = qbsQuit
      Caption = #59153
      ExplicitLeft = 672
      ExplicitTop = 16
      ExplicitHeight = 32
    end
    object btnMax: TUQuickButton
      Left = 794
      Top = 0
      Height = 40
      Align = alCustom
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe MDL2 Assets'
      Font.Style = []
      ParentFont = False
      BackColors.Enabled = False
      BackColors.Color = clBlack
      BackColors.LightColor = 13619151
      BackColors.DarkColor = 3947580
      ButtonStyle = qbsMax
      StickAlign = alRight
      StickToControl = btnClose
      Caption = #59683
      ExplicitLeft = 640
      ExplicitTop = 16
      ExplicitHeight = 32
    end
    object btnMin: TUQuickButton
      Left = 749
      Top = 0
      Height = 40
      Align = alCustom
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe MDL2 Assets'
      Font.Style = []
      ParentFont = False
      BackColors.Enabled = False
      BackColors.Color = clBlack
      BackColors.LightColor = 13619151
      BackColors.DarkColor = 3947580
      ButtonStyle = qbsMin
      StickAlign = alRight
      StickToControl = btnMax
      Caption = #57608
      ExplicitLeft = 1030
      ExplicitTop = -6
    end
    object btnShowHideToolbox: TUQuickButton
      Left = 0
      Top = 0
      Height = 40
      Align = alLeft
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe MDL2 Assets'
      Font.Style = []
      ParentFont = False
      OnClick = btnShowHideToolboxClick
      BackColors.Enabled = False
      BackColors.Color = clBlack
      BackColors.LightColor = 13619151
      BackColors.DarkColor = 3947580
      ButtonStyle = qbsNone
      Caption = #59136
      ExplicitLeft = 104
      ExplicitTop = 16
      ExplicitHeight = 32
    end
  end
  object pnlShortCuts: TUPanel
    Left = 0
    Top = 40
    Width = 45
    Height = 489
    Align = alLeft
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
    object USeparator2: TUSeparator
      Left = 0
      Top = 120
      Width = 45
      Height = 20
      Align = alTop
      Orientation = oHorizontal
    end
    object UPanel5: TUPanel
      Left = 0
      Top = 140
      Width = 45
      Height = 220
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ShowCaption = False
      TabOrder = 0
      BackColor.Enabled = False
      BackColor.Color = clBlack
      BackColor.LightColor = 15132390
      BackColor.DarkColor = 2039583
      object btnDebugRun: TUQuickButton
        Left = 0
        Top = 0
        Height = 40
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Segoe MDL2 Assets'
        Font.Style = []
        ParentFont = False
        BackColors.Enabled = False
        BackColors.Color = clBlack
        BackColors.LightColor = 13619151
        BackColors.DarkColor = 3947580
        ButtonStyle = qbsNone
        Caption = #59240
      end
      object btnDebugPause: TUQuickButton
        Left = 0
        Top = 40
        Height = 40
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Segoe MDL2 Assets'
        Font.Style = []
        ParentFont = False
        BackColors.Enabled = False
        BackColors.Color = clBlack
        BackColors.LightColor = 13619151
        BackColors.DarkColor = 3947580
        ButtonStyle = qbsNone
        Caption = #59241
      end
      object btnDebugStop: TUQuickButton
        Left = 0
        Top = 80
        Height = 40
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Segoe MDL2 Assets'
        Font.Style = []
        ParentFont = False
        BackColors.Enabled = False
        BackColors.Color = clBlack
        BackColors.LightColor = 13619151
        BackColors.DarkColor = 3947580
        ButtonStyle = qbsNone
        Caption = #59162
      end
      object USeparator1: TUSeparator
        Left = 0
        Top = 120
        Width = 45
        Height = 20
        Align = alTop
        Orientation = oHorizontal
        ExplicitTop = 135
        ExplicitWidth = 305
      end
      object btnDebugStepOver: TUQuickButton
        Left = 0
        Top = 140
        Height = 40
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Segoe MDL2 Assets'
        Font.Style = []
        ParentFont = False
        BackColors.Enabled = False
        BackColors.Color = clBlack
        BackColors.LightColor = 13619151
        BackColors.DarkColor = 3947580
        ButtonStyle = qbsNone
        Caption = #59539
      end
      object btnDebugStepInto: TUQuickButton
        Left = 0
        Top = 180
        Height = 40
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Segoe MDL2 Assets'
        Font.Style = []
        ParentFont = False
        BackColors.Enabled = False
        BackColors.Color = clBlack
        BackColors.LightColor = 13619151
        BackColors.DarkColor = 3947580
        ButtonStyle = qbsNone
        Caption = #59542
      end
    end
    object UPanel7: TUPanel
      Left = 0
      Top = 0
      Width = 45
      Height = 120
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
      object btnHome: TUQuickButton
        Left = 0
        Top = 0
        Height = 40
        Hint = 'Start page'
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Segoe MDL2 Assets'
        Font.Style = []
        ParentFont = False
        OnClick = btnHomeClick
        BackColors.Enabled = False
        BackColors.Color = clBlack
        BackColors.LightColor = 13619151
        BackColors.DarkColor = 3947580
        ButtonStyle = qbsNone
        Caption = #57615
      end
      object btnProject: TUQuickButton
        Left = 0
        Top = 40
        Height = 40
        Hint = 'Project'
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Segoe MDL2 Assets'
        Font.Style = []
        ParentFont = False
        BackColors.Enabled = False
        BackColors.Color = clBlack
        BackColors.LightColor = 13619151
        BackColors.DarkColor = 3947580
        ButtonStyle = qbsNone
        Caption = #60358
      end
      object btnSearch: TUQuickButton
        Left = 0
        Top = 80
        Height = 40
        Hint = 'Search'
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Segoe MDL2 Assets'
        Font.Style = []
        ParentFont = False
        BackColors.Enabled = False
        BackColors.Color = clBlack
        BackColors.LightColor = 13619151
        BackColors.DarkColor = 3947580
        ButtonStyle = qbsNone
        Caption = #63371
      end
    end
  end
  object pnlStatus: TUPanel
    Left = 0
    Top = 529
    Width = 884
    Height = 32
    Align = alBottom
    FullRepaint = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ShowCaption = False
    TabOrder = 2
    BackColor.Enabled = True
    BackColor.Color = 10444863
    BackColor.LightColor = 15132390
    BackColor.DarkColor = 10444863
    object txtFilePath: TUText
      AlignWithMargins = True
      Left = 8
      Top = 4
      Width = 44
      Height = 24
      Margins.Left = 8
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 4
      Align = alLeft
      Caption = 'FilePath'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitHeight = 17
    end
    object USeparator3: TUSeparator
      Left = 52
      Top = 0
      Height = 32
      Align = alLeft
      ExplicitLeft = 192
      ExplicitTop = 24
      ExplicitHeight = 50
    end
    object txtFileEncoding: TUText
      AlignWithMargins = True
      Left = 72
      Top = 4
      Width = 34
      Height = 24
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 4
      Align = alLeft
      Caption = 'UTF-8'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitHeight = 17
    end
    object USeparator4: TUSeparator
      Left = 106
      Top = 0
      Height = 32
      Align = alLeft
      ExplicitLeft = 148
    end
    object txtFileLineBreaks: TUText
      AlignWithMargins = True
      Left = 126
      Top = 4
      Width = 28
      Height = 24
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 4
      Align = alLeft
      Caption = 'CRLF'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitHeight = 17
    end
    object USeparator5: TUSeparator
      Left = 154
      Top = 0
      Height = 32
      Align = alLeft
      ExplicitLeft = 282
      ExplicitTop = 6
    end
    object txtFileEditMode: TUText
      AlignWithMargins = True
      Left = 174
      Top = 4
      Width = 32
      Height = 24
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 4
      Align = alLeft
      Caption = 'Insert'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitHeight = 17
    end
    object USeparator6: TUSeparator
      Left = 206
      Top = 0
      Height = 32
      Align = alLeft
      ExplicitLeft = 315
    end
    object txtFileEditPosition: TUText
      AlignWithMargins = True
      Left = 226
      Top = 4
      Width = 118
      Height = 24
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 4
      Align = alLeft
      Caption = 'Line: YY, Column: XX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitHeight = 17
    end
    object sepStatus: TUSeparator
      Left = 344
      Top = 0
      Height = 32
      Align = alLeft
      Visible = False
      ExplicitLeft = 358
      ExplicitTop = 6
    end
    object txtStatus: TUText
      AlignWithMargins = True
      Left = 364
      Top = 4
      Width = 15
      Height = 24
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 4
      Align = alLeft
      Caption = '---'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      Visible = False
      ExplicitHeight = 17
    end
    object btnFileZoomOut: TUQuickButton
      Left = 578
      Top = 0
      Align = alRight
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe MDL2 Assets'
      Font.Style = []
      ParentFont = False
      BackColors.Enabled = False
      BackColors.Color = clBlack
      BackColors.LightColor = 13619151
      BackColors.DarkColor = 3947580
      ButtonStyle = qbsNone
      Caption = #59167
      ExplicitLeft = 864
      ExplicitTop = 6
    end
    object btnFileZoomIn: TUQuickButton
      AlignWithMargins = True
      Left = 831
      Top = 0
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 8
      Margins.Bottom = 0
      Align = alRight
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe MDL2 Assets'
      Font.Style = []
      ParentFont = False
      BackColors.Enabled = False
      BackColors.Color = clBlack
      BackColors.LightColor = 13619151
      BackColors.DarkColor = 3947580
      ButtonStyle = qbsNone
      Caption = #59555
      ExplicitLeft = 1151
    end
    object barStatus: TUProgressBar
      AlignWithMargins = True
      Left = 387
      Top = 12
      Width = 191
      Height = 8
      Margins.Left = 8
      Margins.Top = 12
      Margins.Right = 0
      Margins.Bottom = 12
      Align = alClient
      TabOrder = 1
      Visible = False
      AniSet.AniKind = akOut
      AniSet.AniFunctionKind = afkQuartic
      AniSet.DelayStartTime = 0
      AniSet.Duration = 250
      AniSet.Step = 25
      BackColor.Enabled = False
      BackColor.Color = 15132390
      BackColor.LightColor = 13421772
      BackColor.DarkColor = 3355443
      FillColor.Enabled = False
      FillColor.Color = 15132390
      FillColor.LightColor = 13421772
      FillColor.DarkColor = 3355443
      Value = 0
      Orientation = oHorizontal
      ExplicitWidth = 123
    end
    object sliFileZoom: TUSlider
      AlignWithMargins = True
      Left = 627
      Top = 0
      Width = 200
      Height = 32
      Margins.Left = 4
      Margins.Top = 0
      Margins.Right = 4
      Margins.Bottom = 0
      Align = alRight
      TabOrder = 0
      TabStop = True
      BackColor.Enabled = False
      BackColor.LightColor = 10066329
      BackColor.LightHover = 6710886
      BackColor.LightPress = 10066329
      BackColor.LightDisabled = 13421772
      BackColor.LightFocused = 6710886
      BackColor.DarkColor = 6710886
      BackColor.DarkHover = 10066329
      BackColor.DarkPress = 6710886
      BackColor.DarkDisabled = 3355443
      BackColor.DarkFocused = 10066329
      CurColor.Enabled = False
      CurColor.LightColor = 14120960
      CurColor.LightHover = 1513239
      CurColor.LightPress = 13421772
      CurColor.LightDisabled = 14120960
      CurColor.LightFocused = 1513239
      CurColor.DarkColor = 14120960
      CurColor.DarkHover = 7763574
      CurColor.DarkPress = 15921906
      CurColor.DarkDisabled = 14120960
      CurColor.DarkFocused = 15921906
      Min = 50
      Max = 250
      Value = 100
      ExplicitLeft = 559
    end
  end
  object pnlWorkSpace: TUPanel
    Left = 252
    Top = 40
    Width = 632
    Height = 489
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ShowCaption = False
    TabOrder = 3
    BackColor.Enabled = False
    BackColor.Color = clBlack
    BackColor.LightColor = 15132390
    BackColor.DarkColor = 2039583
  end
  object pnlLeft: TUPanel
    Left = 45
    Top = 40
    Width = 200
    Height = 489
    Align = alLeft
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ShowCaption = False
    TabOrder = 4
    BackColor.Enabled = False
    BackColor.Color = clBlack
    BackColor.LightColor = 15132390
    BackColor.DarkColor = 2039583
    object boxSearch: TUScrollBox
      Left = 0
      Top = 0
      Width = 200
      Height = 489
      HorzScrollBar.Tracking = True
      VertScrollBar.Tracking = True
      Align = alClient
      BevelEdges = []
      BevelInner = bvNone
      BevelOuter = bvNone
      Color = 2039583
      ParentColor = False
      TabOrder = 1
      StyleElements = []
      BackColor.Enabled = False
      BackColor.Color = clBlack
      BackColor.LightColor = 15132390
      BackColor.DarkColor = 2039583
      object barSearch: TUTitleBar
        Left = 0
        Top = 0
        Width = 200
        Height = 24
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        DragMovement = False
        EnableSystemMenu = False
        Caption = 'Search'
        ExplicitWidth = 185
      end
      object vstSearch: TVirtualStringTree
        Left = 0
        Top = 24
        Width = 200
        Height = 465
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Header.AutoSizeIndex = 0
        Header.MainColumn = -1
        ParentColor = True
        TabOrder = 1
        Columns = <>
      end
    end
    object boxProject: TUScrollBox
      Left = 0
      Top = 0
      Width = 200
      Height = 489
      HorzScrollBar.Tracking = True
      VertScrollBar.Tracking = True
      Align = alClient
      BevelEdges = []
      BevelInner = bvNone
      BevelOuter = bvNone
      Color = 2039583
      ParentColor = False
      TabOrder = 0
      StyleElements = []
      BackColor.Enabled = False
      BackColor.Color = clBlack
      BackColor.LightColor = 15132390
      BackColor.DarkColor = 2039583
      object barProject: TUTitleBar
        Left = 0
        Top = 0
        Width = 200
        Height = 24
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        DragMovement = False
        EnableSystemMenu = False
        Caption = 'Project'
        ExplicitWidth = 185
      end
      object vstProject: TVirtualStringTree
        Left = 0
        Top = 24
        Width = 200
        Height = 465
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Header.AutoSizeIndex = 0
        Header.MainColumn = -1
        ParentColor = True
        TabOrder = 1
        Columns = <>
      end
    end
  end
  object mnuMain: TMainMenu
    Left = 289
    Top = 56
    object File1: TMenuItem
      AutoHotkeys = maManual
      Caption = 'File'
      object New1: TMenuItem
        Caption = 'New...'
        ShortCut = 16462
      end
      object Open1: TMenuItem
        Caption = 'Open...'
        ShortCut = 16463
      end
      object History1: TMenuItem
        Caption = 'History'
        object empty1: TMenuItem
          Caption = '--- empty ---'
        end
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Save1: TMenuItem
        Caption = 'Save'
        ShortCut = 16467
      end
      object Saveas1: TMenuItem
        Caption = 'Save as...'
      end
      object Saveall1: TMenuItem
        Caption = 'Save all'
        ShortCut = 49235
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Caption = 'Close'
        ShortCut = 16472
      end
      object Closeall1: TMenuItem
        Caption = 'Close all'
        ShortCut = 49240
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        ShortCut = 16465
      end
    end
    object Edit1: TMenuItem
      AutoHotkeys = maManual
      Caption = 'Edit'
      object Undo1: TMenuItem
        Caption = 'Undo'
        ShortCut = 16474
      end
      object Redo1: TMenuItem
        Caption = 'Redo'
        ShortCut = 24666
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Cut1: TMenuItem
        Caption = 'Cut'
        ShortCut = 16472
      end
      object Copy1: TMenuItem
        Caption = 'Copy'
        ShortCut = 16451
      end
      object Paste1: TMenuItem
        Caption = 'Paste'
        ShortCut = 16470
      end
    end
    object Search1: TMenuItem
      AutoHotkeys = maManual
      Caption = 'Search'
      object Find1: TMenuItem
        Caption = 'Find...'
        ShortCut = 16454
      end
      object Findfile1: TMenuItem
        Caption = 'Find file...'
        ShortCut = 24646
      end
      object Replace1: TMenuItem
        Caption = 'Replace...'
        ShortCut = 16456
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Repeatsearch1: TMenuItem
        Caption = 'Repeat search'
        ShortCut = 114
      end
    end
    object Project1: TMenuItem
      AutoHotkeys = maManual
      Caption = 'Project'
      object Resources1: TMenuItem
        Caption = 'Resources...'
      end
      object Options1: TMenuItem
        Caption = 'Options...'
      end
    end
    object Run1: TMenuItem
      AutoHotkeys = maManual
      Caption = 'Run'
      object Run2: TMenuItem
        Caption = 'Run'
        ShortCut = 120
      end
      object Runwithoutdebuging1: TMenuItem
        Caption = 'Run without debugging'
        ShortCut = 16504
      end
      object Parameters1: TMenuItem
        Caption = 'Parameters...'
      end
    end
    object Debug1: TMenuItem
      AutoHotkeys = maManual
      Caption = 'Debug'
      object Stepover1: TMenuItem
        Caption = 'Step over'
        ShortCut = 119
      end
      object Stepinto1: TMenuItem
        Caption = 'Step into'
        ShortCut = 118
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object erminate1: TMenuItem
        Caption = 'Terminate'
        ShortCut = 16497
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object Inspect1: TMenuItem
        Caption = 'Inspect...'
      end
      object Evaluate1: TMenuItem
        Caption = 'Evaluate...'
      end
      object Addwatch1: TMenuItem
        Caption = 'Add watch...'
      end
      object Addbreakpoint1: TMenuItem
        Caption = 'Add breakpoint...'
      end
    end
    object ools1: TMenuItem
      AutoHotkeys = maManual
      Caption = 'Tools'
      object Options2: TMenuItem
        Caption = 'Options...'
      end
    end
    object Window1: TMenuItem
      AutoHotkeys = maManual
      Caption = 'Window'
      object Nextwindow1: TMenuItem
        Caption = 'Next window'
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object empty2: TMenuItem
        Caption = '--- empty ---'
      end
    end
    object Help1: TMenuItem
      AutoHotkeys = maManual
      Caption = 'Help'
      object NEDprojectwebsite1: TMenuItem
        Caption = 'NED project webpage...'
      end
      object NitroPascalwebsite1: TMenuItem
        Caption = 'NitroPascal webpage...'
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object AboutNED1: TMenuItem
        Caption = 'About NED...'
      end
    end
  end
end
