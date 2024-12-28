object NEDMainForm: TNEDMainForm
  Left = 0
  Top = 0
  Caption = 'NED'
  ClientHeight = 800
  ClientWidth = 1200
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object UCaptionBar1: TUCaptionBar
    Left = 0
    Top = 0
    Width = 1200
    Height = 40
    ThemeManager = UThemeManager1
    Caption = '   Caption bar'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    ShowCaption = False
    TabOrder = 0
    BackColors.Enabled = False
    BackColors.Color = clBlack
    BackColors.LightColor = 15921906
    BackColors.DarkColor = 2829099
    BackColors.FocusedLightColor = 14120960
    BackColors.FocusedDarkColor = 1525760
    Menu = MainMenu1
    MenuController.ButtonWidth = 54
    MenuController.ButtonHeight = 40
    MenuController.PosX = 45
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
    MenuOffset = 45
    UseSystemCaptionColor = True
    CaptionHeight = 40
    ExplicitWidth = 800
    object btnClose: TUQuickButton
      Left = 1155
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
      Left = 1065
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
      StickToControl = btnClose
      Caption = #59683
      ExplicitLeft = 640
      ExplicitTop = 16
      ExplicitHeight = 32
    end
    object btnMin: TUQuickButton
      Left = 1110
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
      StickToControl = btnMax
      Caption = #57608
      ExplicitLeft = 638
      ExplicitTop = 2
      ExplicitHeight = 32
    end
    object UQuickButton6: TUQuickButton
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
  object UPanel1: TUPanel
    Left = 0
    Top = 40
    Width = 45
    Height = 720
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
    ExplicitHeight = 440
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
      object UQuickButton1: TUQuickButton
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
      object UQuickButton2: TUQuickButton
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
      object UQuickButton3: TUQuickButton
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
      object UQuickButton4: TUQuickButton
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
      object UQuickButton5: TUQuickButton
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
      object UQuickButton7: TUQuickButton
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
        Caption = #57615
      end
      object UQuickButton8: TUQuickButton
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
        Caption = #57615
      end
      object UQuickButton9: TUQuickButton
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
        Caption = #57615
      end
    end
  end
  object UPanel2: TUPanel
    Left = 0
    Top = 760
    Width = 1200
    Height = 40
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ShowCaption = False
    TabOrder = 2
    BackColor.Enabled = True
    BackColor.Color = 10436448
    BackColor.LightColor = 15132390
    BackColor.DarkColor = 10436448
    ExplicitTop = 480
    ExplicitWidth = 800
  end
  object UPanel3: TUPanel
    Left = 230
    Top = 40
    Width = 970
    Height = 720
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
    ExplicitLeft = 465
    ExplicitTop = 56
    ExplicitWidth = 615
    ExplicitHeight = 440
    object UPanel4: TUPanel
      Left = 0
      Top = 0
      Width = 970
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
      BackColor.Enabled = False
      BackColor.Color = clBlack
      BackColor.LightColor = 15132390
      BackColor.DarkColor = 2039583
      ExplicitWidth = 615
      object UScrollBox1: TUScrollBox
        Left = 0
        Top = 0
        Width = 970
        Height = 40
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
        ExplicitWidth = 570
        object USymbolButton1: TUSymbolButton
          Left = 0
          Top = 0
          Align = alLeft
          TabOrder = 1
          SymbolFont.Charset = DEFAULT_CHARSET
          SymbolFont.Color = clWindowText
          SymbolFont.Height = -16
          SymbolFont.Name = 'Segoe MDL2 Assets'
          SymbolFont.Style = []
          DetailFont.Charset = DEFAULT_CHARSET
          DetailFont.Color = clWindowText
          DetailFont.Height = -11
          DetailFont.Name = 'Tahoma'
          DetailFont.Style = []
          SymbolChar = #61440
          Text = 'first.npe'
          Detail = 'Project file'
          KeepOrginalColor = False
          Caption = 'USymbolButton1'
        end
        object USymbolButton2: TUSymbolButton
          Left = 250
          Top = 0
          Align = alLeft
          TabOrder = 2
          SymbolFont.Charset = DEFAULT_CHARSET
          SymbolFont.Color = clWindowText
          SymbolFont.Height = -16
          SymbolFont.Name = 'Segoe MDL2 Assets'
          SymbolFont.Style = []
          DetailFont.Charset = DEFAULT_CHARSET
          DetailFont.Color = clWindowText
          DetailFont.Height = -11
          DetailFont.Name = 'Tahoma'
          DetailFont.Style = []
          SymbolChar = #61440
          Text = 'first_main.npc'
          Detail = 'Source file'
          KeepOrginalColor = False
          Caption = 'USymbolButton1'
        end
      end
    end
    object SynEdit1: TSynEdit
      Left = 0
      Top = 40
      Width = 970
      Height = 680
      Align = alClient
      Color = 2039583
      Ctl3D = True
      ParentCtl3D = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -16
      Font.Name = 'Consolas'
      Font.Style = []
      Font.Quality = fqClearTypeNatural
      TabOrder = 1
      UseCodeFolding = False
      BorderStyle = bsNone
      Gutter.Color = 2039583
      Gutter.BorderColor = clGray
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clGray
      Gutter.Font.Height = -16
      Gutter.Font.Name = 'Consolas'
      Gutter.Font.Style = []
      Gutter.Font.Quality = fqClearTypeNatural
      Gutter.ShowLineNumbers = True
      Gutter.TrackChanges.Visible = True
      Gutter.Bands = <
        item
          Kind = gbkMarks
          Width = 13
        end
        item
          Kind = gbkLineNumbers
        end
        item
          Kind = gbkFold
        end
        item
          Kind = gbkTrackChanges
        end
        item
          Kind = gbkMargin
          Width = 3
        end>
      Highlighter = SynGeneralSyn1
      InsertCaret = ctBlock
      Lines.Strings = (
        'project '#39'First_Test_Project'#39';'
        ''
        '/.'
        
          '  {$program-type output-type output-subtype[ output-extension][ ' +
          'output-path]}'
        '  defines what type of compilation can be done:'
        '  you can define multiple compilation types in single project'
        ''
        '  supported output-type:'
        '    - Windows(32/64)'
        '    - Linux(32/64)'
        '    - Android(32/64)'
        '    - WebAssembly(32/64)'
        ''
        '  supported output-subtype:'
        
          '    - GUI     - means program that uses operating system Graphic' +
          ' User Interface like in eg.: Windows,'
        
          '    - CONSOLE - means program that outputs to the operating syst' +
          'em console (if available),'
        '    - DLL     - dynamically loaded library,'
        '    - TEXT    - text format'
        ''
        '  extension will be given to output of compiler, eg.:'
        
          '    if Windows(32/64) is defined than extension will be '#39'.exe'#39' o' +
          'r '#39'.dll'#39
        
          '    if Linux(32/64) is defined than extension will be '#39'.elf'#39' or ' +
          #39'.so'#39
        '    if Android(32/64) is defined than extension will be '#39'.apk'#39
        
          '    if WebAssembly is defined than extension will be '#39'.wasm'#39' or ' +
          #39'.wat'#39
        ''
        
          '  custom extension can be set by declaring {$extension '#39'.ext'#39'} f' +
          'or every project'
        '  example: {$program-type Windows32 DLL {$extension '#39'.dll'#39'}}'
        '    or short form {$program-type Windows32 DLL '#39'.dll'#39'}'
        
          '    or for every project type using concatenated version {$exten' +
          'sion '#39'* .dll * *'#39'}'
        
          '  extension that will not change are not required to specify, so' +
          ' usage of * is not required also'
        
          '  but if you want to specify extension using {$extension } that ' +
          'is second or third or fourth, but not changing'
        
          '  previous ones, than you must use * to let complier know what y' +
          'ou are up to'
        './'
        ''
        '{$program-type Windows32 GUI}'
        '{$program-type Windows32 DLL '#39'.dll'#39'}'
        '{$program-type Android64 GUI}'
        '{$program-type WebAssembly DLL}'
        ''
        '/.'
        '  {$search-path[ recursive] '#39'directory'#39'}'
        
          '  defines path(s) that project can use to search imported code f' +
          'iles'
        
          '  you can define multiple search paths for project by repeating ' +
          'this directive as many times as is needed'
        ''
        
          '  defining search path adds it to the project internal list of p' +
          'aths'
        
          '  single directive adds single choosen path without looking into' +
          ' specified directory'
        
          '  to automatically add children directories inside specified pat' +
          'h you may use optional switch '#39'recursive'#39
        ''
        '  specified path can be of type:'
        
          '    - relative  - to project directory, using '#39'.\\'#39' or '#39'..\\'#39' sp' +
          'ecifier to go in or out of project directory,'
        '    - absolute - specifying full path to desired directory'
        ''
        
          '  compiler checks if specified path exists and produces error if' +
          ' path is not existant or not reachable'
        ''
        
          '  you can modify compiler behavior by supplaing additional direc' +
          'tive:'
        '  {$warn-on-path-error enabled}'
        
          '  which disables underlying path error message and produces only' +
          ' warning'
        ''
        
          '  to disable previously enabled directive, you should use direct' +
          'ive:'
        '  {$warn-on-path-error disabled}'
        './'
        ''
        '{$search-path '#39'..\\..\\concepts\\'#39'}'
        '{$search-path recursive '#39'..\\..\\concepts\\'#39'}'
        ''
        'imports'
        '  // imports section'
        '  //'#39'Source_code_name'#39','
        '  first_main,'
        '  '#39'case statement'#39','
        '  //case,'
        '  '#39'class properties concept'#39','
        '  //properties,'
        '  '#39'comments'#39','
        '  //comments,'
        '  '#39'loops statements'#39
        '  //loops'
        '  ;'
        '  '
        
          '{. OK, this is a multi-line comment. It can be one line eighter.' +
          ' .}'
        '{.'
        
          '  But for now we need to test this compiler at every possible wa' +
          'y we could imagine.'
        '  So to do this we need some test cases, like this comment.'
        '.}'
        ''
        '(*'
        '  After that there will be one more comment to test.'
        '*)'
        ''
        '//type'
        '// Enum = (eiOne, eiTwo, eiThree, eiFour, eiFive);'
        ''
        'var'
        '  SomeString: String;'
        '  FirstProgram: Boolean;'
        '  ErrorMessage: String;'
        '  SomeEnum: Enum = eiThree;'
        ''
        '//const'
        '//  SomeConst: Boolean = True;'
        '//  SomeOtherConst1: Boolean = True;'
        '//  SomeOtherConst2: Boolean = True;'
        ''
        
          '//  SomeConst: Int = 20; @ensure: SomeConst % 2 = 0; // add comp' +
          'ile-time assertion when condition is not met'
        ''
        'initialization'
        '  // initialization code'
        '  if ConsoleAvailable then'
        '    ConsoleTitle := %project-name%;'
        '  //'
        '  FirstProgram := True;'
        '  //'
        '  if not FirstProgram then {'
        '    // nothing here'
        '  };'
        '  //'
        '  if 1 + 2 = 3 + 4 then {'
        '    if ConsoleAvailable then {'
        
          '      Writeln('#39'We are testing our compiler.'#39'#13#10'#39'\rIf everythi' +
          'ng will be ok, than\rmaybe we make a change in programming in Pa' +
          'scal.'#39');'
        
          '      Writeln('#39'This line shows that we are not limited by line l' +
          'ength and string literal length like in Delphi where you can not' +
          ' use lines that are longer than 255 characters. In NitroPascal Y' +
          'ou can have lines that are 2GB characters long. We are not sure ' +
          'why You may need it, but when we can have it, than why not ?'#39');'
        '    };'
        '  } else {'
        '    ErrorMessage := '#39'This is expected !'#39';'
        '  };'
        '  //'
        '  if ConsoleAvailable and (Length(ErrorMessage) > 0) then {'
        
          '      Writeln('#39'We are testing our compiler.'#39'#13#10'#39'\rIf everythi' +
          'ng will be ok, than\rmaybe we make a change in programming in Pa' +
          'scal.'#39');'
        
          '      Writeln('#39'This line shows that we are not limited by line l' +
          'ength and string literal length like in Delphi where you can not' +
          ' use lines that are longer than 255 characters. In NitroPascal Y' +
          'ou can have lines that are 2GB characters long. We are not sure ' +
          'why You may need it, but when we can have it, than why not ?'#39');'
        '  };'
        '  //'
        '  if SomeConst then {'
        '    // maybe some statements'
        '    SomeConst := False;'
        '    SomeOtherConst1 := False;'
        '    SomeOtherConst2 := False;'
        
          '  } else // test comments in various places, @TODO: use other ty' +
          'pes of comments as well'
        '  if SomeOtherConst1 then {'
        '    // maybe some statements'
        '    SomeConst := False;'
        '    SomeOtherConst1 := False;'
        '    SomeOtherConst2 := False;'
        '  } // else in next line, because we commented here'
        '  else if SomeOtherConst2 then {'
        '    // maybe some statements'
        '    SomeConst := False;'
        '    SomeOtherConst1 := False;'
        '    SomeOtherConst2 := False;'
        '  } else {'
        '    // maybe some statements'
        '    SomeConst := False;'
        '    SomeOtherConst1 := False;'
        '    SomeOtherConst2 := False;'
        '  };'
        '  //'
        '  case 2 > 1 {'
        '    if True: {'
        '    };'
        '    if False: {'
        '    };'
        '  };'
        '  //'
        '  case SomeEnum {'
        '    if .eiOne: {'
        '    };'
        '    if .eiTwo: {'
        '    };'
        '    if .eiThree: {'
        '    };'
        '    if .eiFour: {'
        '    };'
        '    if .eiFive: {'
        '    };'
        '  };'
        ''
        'finalization'
        '  // finalization code'
        '  for i := 0; i < 10; i += 1 {'
        '    // loop'
        '  };'
        ''
        'begin'
        '  // program initialization code'
        '  SomeString := '#39'this is a text'#39';'
        '  if ConsoleAvailable then {'
        '    Writeln(SomeString);'
        '    ConsolePause;'
        '  };'
        'end.')
      Options = [eoAutoIndent, eoDisableScrollArrows, eoDragDropEditing, eoEnhanceHomeKey, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoShowScrollHint, eoSmartTabDelete, eoTabIndent, eoTabsToSpaces, eoTrimTrailingSpaces, eoShowLigatures, eoCopyPlainText]
      RightEdge = 128
      RightEdgeColor = clYellow
      SelectedColor.Alpha = 0.400000005960464500
      ExplicitWidth = 570
      ExplicitHeight = 400
    end
  end
  object UPanel6: TUPanel
    Left = 45
    Top = 40
    Width = 185
    Height = 720
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
    ExplicitLeft = 256
    ExplicitTop = 248
    ExplicitHeight = 41
    object UScrollBox2: TUScrollBox
      Left = 0
      Top = 0
      Width = 185
      Height = 720
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
      ExplicitLeft = -6
      ExplicitTop = 6
      ExplicitHeight = 440
      object UItemButton1: TUItemButton
        Left = 0
        Top = 0
        Width = 185
        Align = alTop
        DragCursor = crDefault
        TabOrder = 1
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
        CustomColors.BorderColors.LightHover = 11184810
        CustomColors.BorderColors.LightPress = 10066329
        CustomColors.BorderColors.LightDisabled = 8026746
        CustomColors.BorderColors.LightFocused = 11184810
        CustomColors.BorderColors.DarkColor = 3355443
        CustomColors.BorderColors.DarkHover = 11184810
        CustomColors.BorderColors.DarkPress = 6710886
        CustomColors.BorderColors.DarkDisabled = 8750469
        CustomColors.BorderColors.DarkFocused = 11184810
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
        CustomColors.DetailColors.Enabled = False
        CustomColors.DetailColors.LightColor = clGray
        CustomColors.DetailColors.LightHover = clSilver
        CustomColors.DetailColors.LightPress = clSilver
        CustomColors.DetailColors.LightDisabled = clSilver
        CustomColors.DetailColors.LightFocused = clSilver
        CustomColors.DetailColors.DarkColor = 3355443
        CustomColors.DetailColors.DarkHover = 3355443
        CustomColors.DetailColors.DarkPress = 6710886
        CustomColors.DetailColors.DarkDisabled = 3355443
        CustomColors.DetailColors.DarkFocused = 3355443
        CustomColors.ActiveColors.Enabled = False
        CustomColors.ActiveColors.LightColor = 13421772
        CustomColors.ActiveColors.LightHover = 13421772
        CustomColors.ActiveColors.LightPress = 10066329
        CustomColors.ActiveColors.LightDisabled = 13421772
        CustomColors.ActiveColors.LightFocused = 13421772
        CustomColors.ActiveColors.DarkColor = 3355443
        CustomColors.ActiveColors.DarkHover = 3355443
        CustomColors.ActiveColors.DarkPress = 6710886
        CustomColors.ActiveColors.DarkDisabled = 3355443
        CustomColors.ActiveColors.DarkFocused = 3355443
        IconFont.Charset = DEFAULT_CHARSET
        IconFont.Color = clWindowText
        IconFont.Height = -20
        IconFont.Name = 'Segoe MDL2 Assets'
        IconFont.Style = []
        DetailFont.Charset = DEFAULT_CHARSET
        DetailFont.Color = clWindowText
        DetailFont.Height = -11
        DetailFont.Name = 'Tahoma'
        DetailFont.Style = []
        ObjectsVisible = [iokNone, iokLeftIcon, iokText]
        LeftIcon = #57759
        Text = 'Text'
        Detail = 'Detail'
        RightIcon = #59198
      end
      object UItemButton2: TUItemButton
        Left = 0
        Top = 40
        Width = 185
        Align = alTop
        DragCursor = crDefault
        TabOrder = 2
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
        CustomColors.BorderColors.LightHover = 11184810
        CustomColors.BorderColors.LightPress = 10066329
        CustomColors.BorderColors.LightDisabled = 8026746
        CustomColors.BorderColors.LightFocused = 11184810
        CustomColors.BorderColors.DarkColor = 3355443
        CustomColors.BorderColors.DarkHover = 11184810
        CustomColors.BorderColors.DarkPress = 6710886
        CustomColors.BorderColors.DarkDisabled = 8750469
        CustomColors.BorderColors.DarkFocused = 11184810
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
        CustomColors.DetailColors.Enabled = False
        CustomColors.DetailColors.LightColor = clGray
        CustomColors.DetailColors.LightHover = clSilver
        CustomColors.DetailColors.LightPress = clSilver
        CustomColors.DetailColors.LightDisabled = clSilver
        CustomColors.DetailColors.LightFocused = clSilver
        CustomColors.DetailColors.DarkColor = 3355443
        CustomColors.DetailColors.DarkHover = 3355443
        CustomColors.DetailColors.DarkPress = 6710886
        CustomColors.DetailColors.DarkDisabled = 3355443
        CustomColors.DetailColors.DarkFocused = 3355443
        CustomColors.ActiveColors.Enabled = False
        CustomColors.ActiveColors.LightColor = 13421772
        CustomColors.ActiveColors.LightHover = 13421772
        CustomColors.ActiveColors.LightPress = 10066329
        CustomColors.ActiveColors.LightDisabled = 13421772
        CustomColors.ActiveColors.LightFocused = 13421772
        CustomColors.ActiveColors.DarkColor = 3355443
        CustomColors.ActiveColors.DarkHover = 3355443
        CustomColors.ActiveColors.DarkPress = 6710886
        CustomColors.ActiveColors.DarkDisabled = 3355443
        CustomColors.ActiveColors.DarkFocused = 3355443
        IconFont.Charset = DEFAULT_CHARSET
        IconFont.Color = clWindowText
        IconFont.Height = -20
        IconFont.Name = 'Segoe MDL2 Assets'
        IconFont.Style = []
        DetailFont.Charset = DEFAULT_CHARSET
        DetailFont.Color = clWindowText
        DetailFont.Height = -11
        DetailFont.Name = 'Tahoma'
        DetailFont.Style = []
        ObjectsVisible = [iokNone, iokLeftIcon, iokText]
        LeftIcon = #57759
        Text = 'Text'
        Detail = 'Detail'
        RightIcon = #59198
        Caption = 'UItemButton2'
      end
      object UItemButton3: TUItemButton
        Left = 0
        Top = 80
        Width = 185
        Align = alTop
        DragCursor = crDefault
        TabOrder = 3
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
        CustomColors.BorderColors.LightHover = 11184810
        CustomColors.BorderColors.LightPress = 10066329
        CustomColors.BorderColors.LightDisabled = 8026746
        CustomColors.BorderColors.LightFocused = 11184810
        CustomColors.BorderColors.DarkColor = 3355443
        CustomColors.BorderColors.DarkHover = 11184810
        CustomColors.BorderColors.DarkPress = 6710886
        CustomColors.BorderColors.DarkDisabled = 8750469
        CustomColors.BorderColors.DarkFocused = 11184810
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
        CustomColors.DetailColors.Enabled = False
        CustomColors.DetailColors.LightColor = clGray
        CustomColors.DetailColors.LightHover = clSilver
        CustomColors.DetailColors.LightPress = clSilver
        CustomColors.DetailColors.LightDisabled = clSilver
        CustomColors.DetailColors.LightFocused = clSilver
        CustomColors.DetailColors.DarkColor = 3355443
        CustomColors.DetailColors.DarkHover = 3355443
        CustomColors.DetailColors.DarkPress = 6710886
        CustomColors.DetailColors.DarkDisabled = 3355443
        CustomColors.DetailColors.DarkFocused = 3355443
        CustomColors.ActiveColors.Enabled = False
        CustomColors.ActiveColors.LightColor = 13421772
        CustomColors.ActiveColors.LightHover = 13421772
        CustomColors.ActiveColors.LightPress = 10066329
        CustomColors.ActiveColors.LightDisabled = 13421772
        CustomColors.ActiveColors.LightFocused = 13421772
        CustomColors.ActiveColors.DarkColor = 3355443
        CustomColors.ActiveColors.DarkHover = 3355443
        CustomColors.ActiveColors.DarkPress = 6710886
        CustomColors.ActiveColors.DarkDisabled = 3355443
        CustomColors.ActiveColors.DarkFocused = 3355443
        IconFont.Charset = DEFAULT_CHARSET
        IconFont.Color = clWindowText
        IconFont.Height = -20
        IconFont.Name = 'Segoe MDL2 Assets'
        IconFont.Style = []
        DetailFont.Charset = DEFAULT_CHARSET
        DetailFont.Color = clWindowText
        DetailFont.Height = -11
        DetailFont.Name = 'Tahoma'
        DetailFont.Style = []
        ObjectsVisible = [iokNone, iokLeftIcon, iokText]
        LeftIcon = #57759
        Text = 'Text'
        Detail = 'Detail'
        RightIcon = #59198
        Caption = 'UItemButton3'
      end
      object UItemButton4: TUItemButton
        Left = 0
        Top = 120
        Width = 185
        Align = alTop
        DragCursor = crDefault
        TabOrder = 4
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
        CustomColors.BorderColors.LightHover = 11184810
        CustomColors.BorderColors.LightPress = 10066329
        CustomColors.BorderColors.LightDisabled = 8026746
        CustomColors.BorderColors.LightFocused = 11184810
        CustomColors.BorderColors.DarkColor = 3355443
        CustomColors.BorderColors.DarkHover = 11184810
        CustomColors.BorderColors.DarkPress = 6710886
        CustomColors.BorderColors.DarkDisabled = 8750469
        CustomColors.BorderColors.DarkFocused = 11184810
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
        CustomColors.DetailColors.Enabled = False
        CustomColors.DetailColors.LightColor = clGray
        CustomColors.DetailColors.LightHover = clSilver
        CustomColors.DetailColors.LightPress = clSilver
        CustomColors.DetailColors.LightDisabled = clSilver
        CustomColors.DetailColors.LightFocused = clSilver
        CustomColors.DetailColors.DarkColor = 3355443
        CustomColors.DetailColors.DarkHover = 3355443
        CustomColors.DetailColors.DarkPress = 6710886
        CustomColors.DetailColors.DarkDisabled = 3355443
        CustomColors.DetailColors.DarkFocused = 3355443
        CustomColors.ActiveColors.Enabled = False
        CustomColors.ActiveColors.LightColor = 13421772
        CustomColors.ActiveColors.LightHover = 13421772
        CustomColors.ActiveColors.LightPress = 10066329
        CustomColors.ActiveColors.LightDisabled = 13421772
        CustomColors.ActiveColors.LightFocused = 13421772
        CustomColors.ActiveColors.DarkColor = 3355443
        CustomColors.ActiveColors.DarkHover = 3355443
        CustomColors.ActiveColors.DarkPress = 6710886
        CustomColors.ActiveColors.DarkDisabled = 3355443
        CustomColors.ActiveColors.DarkFocused = 3355443
        IconFont.Charset = DEFAULT_CHARSET
        IconFont.Color = clWindowText
        IconFont.Height = -20
        IconFont.Name = 'Segoe MDL2 Assets'
        IconFont.Style = []
        DetailFont.Charset = DEFAULT_CHARSET
        DetailFont.Color = clWindowText
        DetailFont.Height = -11
        DetailFont.Name = 'Tahoma'
        DetailFont.Style = []
        ObjectsVisible = [iokNone, iokLeftIcon, iokText]
        LeftIcon = #57759
        Text = 'Text'
        Detail = 'Detail'
        RightIcon = #59198
        Caption = 'UItemButton4'
      end
      object UItemButton5: TUItemButton
        Left = 0
        Top = 160
        Width = 185
        Align = alTop
        DragCursor = crDefault
        TabOrder = 5
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
        CustomColors.BorderColors.LightHover = 11184810
        CustomColors.BorderColors.LightPress = 10066329
        CustomColors.BorderColors.LightDisabled = 8026746
        CustomColors.BorderColors.LightFocused = 11184810
        CustomColors.BorderColors.DarkColor = 3355443
        CustomColors.BorderColors.DarkHover = 11184810
        CustomColors.BorderColors.DarkPress = 6710886
        CustomColors.BorderColors.DarkDisabled = 8750469
        CustomColors.BorderColors.DarkFocused = 11184810
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
        CustomColors.DetailColors.Enabled = False
        CustomColors.DetailColors.LightColor = clGray
        CustomColors.DetailColors.LightHover = clSilver
        CustomColors.DetailColors.LightPress = clSilver
        CustomColors.DetailColors.LightDisabled = clSilver
        CustomColors.DetailColors.LightFocused = clSilver
        CustomColors.DetailColors.DarkColor = 3355443
        CustomColors.DetailColors.DarkHover = 3355443
        CustomColors.DetailColors.DarkPress = 6710886
        CustomColors.DetailColors.DarkDisabled = 3355443
        CustomColors.DetailColors.DarkFocused = 3355443
        CustomColors.ActiveColors.Enabled = False
        CustomColors.ActiveColors.LightColor = 13421772
        CustomColors.ActiveColors.LightHover = 13421772
        CustomColors.ActiveColors.LightPress = 10066329
        CustomColors.ActiveColors.LightDisabled = 13421772
        CustomColors.ActiveColors.LightFocused = 13421772
        CustomColors.ActiveColors.DarkColor = 3355443
        CustomColors.ActiveColors.DarkHover = 3355443
        CustomColors.ActiveColors.DarkPress = 6710886
        CustomColors.ActiveColors.DarkDisabled = 3355443
        CustomColors.ActiveColors.DarkFocused = 3355443
        IconFont.Charset = DEFAULT_CHARSET
        IconFont.Color = clWindowText
        IconFont.Height = -20
        IconFont.Name = 'Segoe MDL2 Assets'
        IconFont.Style = []
        DetailFont.Charset = DEFAULT_CHARSET
        DetailFont.Color = clWindowText
        DetailFont.Height = -11
        DetailFont.Name = 'Tahoma'
        DetailFont.Style = []
        ObjectsVisible = [iokNone, iokLeftIcon, iokText]
        LeftIcon = #57759
        Text = 'Text'
        Detail = 'Detail'
        RightIcon = #59198
        Caption = 'UItemButton5'
      end
      object UItemButton6: TUItemButton
        Left = 0
        Top = 200
        Width = 185
        Align = alTop
        DragCursor = crDefault
        TabOrder = 6
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
        CustomColors.BorderColors.LightHover = 11184810
        CustomColors.BorderColors.LightPress = 10066329
        CustomColors.BorderColors.LightDisabled = 8026746
        CustomColors.BorderColors.LightFocused = 11184810
        CustomColors.BorderColors.DarkColor = 3355443
        CustomColors.BorderColors.DarkHover = 11184810
        CustomColors.BorderColors.DarkPress = 6710886
        CustomColors.BorderColors.DarkDisabled = 8750469
        CustomColors.BorderColors.DarkFocused = 11184810
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
        CustomColors.DetailColors.Enabled = False
        CustomColors.DetailColors.LightColor = clGray
        CustomColors.DetailColors.LightHover = clSilver
        CustomColors.DetailColors.LightPress = clSilver
        CustomColors.DetailColors.LightDisabled = clSilver
        CustomColors.DetailColors.LightFocused = clSilver
        CustomColors.DetailColors.DarkColor = 3355443
        CustomColors.DetailColors.DarkHover = 3355443
        CustomColors.DetailColors.DarkPress = 6710886
        CustomColors.DetailColors.DarkDisabled = 3355443
        CustomColors.DetailColors.DarkFocused = 3355443
        CustomColors.ActiveColors.Enabled = False
        CustomColors.ActiveColors.LightColor = 13421772
        CustomColors.ActiveColors.LightHover = 13421772
        CustomColors.ActiveColors.LightPress = 10066329
        CustomColors.ActiveColors.LightDisabled = 13421772
        CustomColors.ActiveColors.LightFocused = 13421772
        CustomColors.ActiveColors.DarkColor = 3355443
        CustomColors.ActiveColors.DarkHover = 3355443
        CustomColors.ActiveColors.DarkPress = 6710886
        CustomColors.ActiveColors.DarkDisabled = 3355443
        CustomColors.ActiveColors.DarkFocused = 3355443
        IconFont.Charset = DEFAULT_CHARSET
        IconFont.Color = clWindowText
        IconFont.Height = -20
        IconFont.Name = 'Segoe MDL2 Assets'
        IconFont.Style = []
        DetailFont.Charset = DEFAULT_CHARSET
        DetailFont.Color = clWindowText
        DetailFont.Height = -11
        DetailFont.Name = 'Tahoma'
        DetailFont.Style = []
        ObjectsVisible = [iokNone, iokLeftIcon, iokText]
        LeftIcon = #57759
        Text = 'Text'
        Detail = 'Detail'
        RightIcon = #59198
        Caption = 'UItemButton6'
      end
      object UItemButton7: TUItemButton
        Left = 0
        Top = 240
        Width = 185
        Align = alTop
        DragCursor = crDefault
        TabOrder = 7
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
        CustomColors.BorderColors.LightHover = 11184810
        CustomColors.BorderColors.LightPress = 10066329
        CustomColors.BorderColors.LightDisabled = 8026746
        CustomColors.BorderColors.LightFocused = 11184810
        CustomColors.BorderColors.DarkColor = 3355443
        CustomColors.BorderColors.DarkHover = 11184810
        CustomColors.BorderColors.DarkPress = 6710886
        CustomColors.BorderColors.DarkDisabled = 8750469
        CustomColors.BorderColors.DarkFocused = 11184810
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
        CustomColors.DetailColors.Enabled = False
        CustomColors.DetailColors.LightColor = clGray
        CustomColors.DetailColors.LightHover = clSilver
        CustomColors.DetailColors.LightPress = clSilver
        CustomColors.DetailColors.LightDisabled = clSilver
        CustomColors.DetailColors.LightFocused = clSilver
        CustomColors.DetailColors.DarkColor = 3355443
        CustomColors.DetailColors.DarkHover = 3355443
        CustomColors.DetailColors.DarkPress = 6710886
        CustomColors.DetailColors.DarkDisabled = 3355443
        CustomColors.DetailColors.DarkFocused = 3355443
        CustomColors.ActiveColors.Enabled = False
        CustomColors.ActiveColors.LightColor = 13421772
        CustomColors.ActiveColors.LightHover = 13421772
        CustomColors.ActiveColors.LightPress = 10066329
        CustomColors.ActiveColors.LightDisabled = 13421772
        CustomColors.ActiveColors.LightFocused = 13421772
        CustomColors.ActiveColors.DarkColor = 3355443
        CustomColors.ActiveColors.DarkHover = 3355443
        CustomColors.ActiveColors.DarkPress = 6710886
        CustomColors.ActiveColors.DarkDisabled = 3355443
        CustomColors.ActiveColors.DarkFocused = 3355443
        IconFont.Charset = DEFAULT_CHARSET
        IconFont.Color = clWindowText
        IconFont.Height = -20
        IconFont.Name = 'Segoe MDL2 Assets'
        IconFont.Style = []
        DetailFont.Charset = DEFAULT_CHARSET
        DetailFont.Color = clWindowText
        DetailFont.Height = -11
        DetailFont.Name = 'Tahoma'
        DetailFont.Style = []
        ObjectsVisible = [iokNone, iokLeftIcon, iokText]
        LeftIcon = #57759
        Text = 'Text'
        Detail = 'Detail'
        RightIcon = #59198
        Caption = 'UItemButton7'
      end
      object UItemButton8: TUItemButton
        Left = 0
        Top = 280
        Width = 185
        Align = alTop
        DragCursor = crDefault
        TabOrder = 8
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
        CustomColors.BorderColors.LightHover = 11184810
        CustomColors.BorderColors.LightPress = 10066329
        CustomColors.BorderColors.LightDisabled = 8026746
        CustomColors.BorderColors.LightFocused = 11184810
        CustomColors.BorderColors.DarkColor = 3355443
        CustomColors.BorderColors.DarkHover = 11184810
        CustomColors.BorderColors.DarkPress = 6710886
        CustomColors.BorderColors.DarkDisabled = 8750469
        CustomColors.BorderColors.DarkFocused = 11184810
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
        CustomColors.DetailColors.Enabled = False
        CustomColors.DetailColors.LightColor = clGray
        CustomColors.DetailColors.LightHover = clSilver
        CustomColors.DetailColors.LightPress = clSilver
        CustomColors.DetailColors.LightDisabled = clSilver
        CustomColors.DetailColors.LightFocused = clSilver
        CustomColors.DetailColors.DarkColor = 3355443
        CustomColors.DetailColors.DarkHover = 3355443
        CustomColors.DetailColors.DarkPress = 6710886
        CustomColors.DetailColors.DarkDisabled = 3355443
        CustomColors.DetailColors.DarkFocused = 3355443
        CustomColors.ActiveColors.Enabled = False
        CustomColors.ActiveColors.LightColor = 13421772
        CustomColors.ActiveColors.LightHover = 13421772
        CustomColors.ActiveColors.LightPress = 10066329
        CustomColors.ActiveColors.LightDisabled = 13421772
        CustomColors.ActiveColors.LightFocused = 13421772
        CustomColors.ActiveColors.DarkColor = 3355443
        CustomColors.ActiveColors.DarkHover = 3355443
        CustomColors.ActiveColors.DarkPress = 6710886
        CustomColors.ActiveColors.DarkDisabled = 3355443
        CustomColors.ActiveColors.DarkFocused = 3355443
        IconFont.Charset = DEFAULT_CHARSET
        IconFont.Color = clWindowText
        IconFont.Height = -20
        IconFont.Name = 'Segoe MDL2 Assets'
        IconFont.Style = []
        DetailFont.Charset = DEFAULT_CHARSET
        DetailFont.Color = clWindowText
        DetailFont.Height = -11
        DetailFont.Name = 'Tahoma'
        DetailFont.Style = []
        ObjectsVisible = [iokNone, iokLeftIcon, iokText]
        LeftIcon = #57759
        Text = 'Text'
        Detail = 'Detail'
        RightIcon = #59198
        Caption = 'UItemButton8'
      end
      object UItemButton9: TUItemButton
        Left = 0
        Top = 320
        Width = 185
        Align = alTop
        DragCursor = crDefault
        TabOrder = 9
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
        CustomColors.BorderColors.LightHover = 11184810
        CustomColors.BorderColors.LightPress = 10066329
        CustomColors.BorderColors.LightDisabled = 8026746
        CustomColors.BorderColors.LightFocused = 11184810
        CustomColors.BorderColors.DarkColor = 3355443
        CustomColors.BorderColors.DarkHover = 11184810
        CustomColors.BorderColors.DarkPress = 6710886
        CustomColors.BorderColors.DarkDisabled = 8750469
        CustomColors.BorderColors.DarkFocused = 11184810
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
        CustomColors.DetailColors.Enabled = False
        CustomColors.DetailColors.LightColor = clGray
        CustomColors.DetailColors.LightHover = clSilver
        CustomColors.DetailColors.LightPress = clSilver
        CustomColors.DetailColors.LightDisabled = clSilver
        CustomColors.DetailColors.LightFocused = clSilver
        CustomColors.DetailColors.DarkColor = 3355443
        CustomColors.DetailColors.DarkHover = 3355443
        CustomColors.DetailColors.DarkPress = 6710886
        CustomColors.DetailColors.DarkDisabled = 3355443
        CustomColors.DetailColors.DarkFocused = 3355443
        CustomColors.ActiveColors.Enabled = False
        CustomColors.ActiveColors.LightColor = 13421772
        CustomColors.ActiveColors.LightHover = 13421772
        CustomColors.ActiveColors.LightPress = 10066329
        CustomColors.ActiveColors.LightDisabled = 13421772
        CustomColors.ActiveColors.LightFocused = 13421772
        CustomColors.ActiveColors.DarkColor = 3355443
        CustomColors.ActiveColors.DarkHover = 3355443
        CustomColors.ActiveColors.DarkPress = 6710886
        CustomColors.ActiveColors.DarkDisabled = 3355443
        CustomColors.ActiveColors.DarkFocused = 3355443
        IconFont.Charset = DEFAULT_CHARSET
        IconFont.Color = clWindowText
        IconFont.Height = -20
        IconFont.Name = 'Segoe MDL2 Assets'
        IconFont.Style = []
        DetailFont.Charset = DEFAULT_CHARSET
        DetailFont.Color = clWindowText
        DetailFont.Height = -11
        DetailFont.Name = 'Tahoma'
        DetailFont.Style = []
        ObjectsVisible = [iokNone, iokLeftIcon, iokText]
        LeftIcon = #57759
        Text = 'Text'
        Detail = 'Detail'
        RightIcon = #59198
        Caption = 'UItemButton9'
      end
    end
  end
  object UThemeManager1: TUThemeManager
    AccentColor = 6318152
    Colors.ButtonColors.BackColors.Enabled = False
    Colors.ButtonColors.BackColors.LightColor = clBlack
    Colors.ButtonColors.BackColors.LightHover = clBlack
    Colors.ButtonColors.BackColors.LightPress = clBlack
    Colors.ButtonColors.BackColors.LightDisabled = clBlack
    Colors.ButtonColors.BackColors.LightFocused = clBlack
    Colors.ButtonColors.BackColors.DarkColor = clBlack
    Colors.ButtonColors.BackColors.DarkHover = clBlack
    Colors.ButtonColors.BackColors.DarkPress = clBlack
    Colors.ButtonColors.BackColors.DarkDisabled = clBlack
    Colors.ButtonColors.BackColors.DarkFocused = clBlack
    Colors.ButtonColors.BorderColors.Enabled = False
    Colors.ButtonColors.BorderColors.LightColor = clBlack
    Colors.ButtonColors.BorderColors.LightHover = clBlack
    Colors.ButtonColors.BorderColors.LightPress = clBlack
    Colors.ButtonColors.BorderColors.LightDisabled = clBlack
    Colors.ButtonColors.BorderColors.LightFocused = clBlack
    Colors.ButtonColors.BorderColors.DarkColor = clBlack
    Colors.ButtonColors.BorderColors.DarkHover = clBlack
    Colors.ButtonColors.BorderColors.DarkPress = clBlack
    Colors.ButtonColors.BorderColors.DarkDisabled = clBlack
    Colors.ButtonColors.BorderColors.DarkFocused = clBlack
    Colors.ButtonColors.TextColors.Enabled = False
    Colors.ButtonColors.TextColors.LightColor = clBlack
    Colors.ButtonColors.TextColors.LightHover = clBlack
    Colors.ButtonColors.TextColors.LightPress = clBlack
    Colors.ButtonColors.TextColors.LightDisabled = clGray
    Colors.ButtonColors.TextColors.LightFocused = clBlack
    Colors.ButtonColors.TextColors.DarkColor = clWhite
    Colors.ButtonColors.TextColors.DarkHover = clWhite
    Colors.ButtonColors.TextColors.DarkPress = clWhite
    Colors.ButtonColors.TextColors.DarkDisabled = clGray
    Colors.ButtonColors.TextColors.DarkFocused = clWhite
    Colors.ItemButtonColors.BackColors.Enabled = False
    Colors.ItemButtonColors.BackColors.LightColor = clBlack
    Colors.ItemButtonColors.BackColors.LightHover = clBlack
    Colors.ItemButtonColors.BackColors.LightPress = clBlack
    Colors.ItemButtonColors.BackColors.LightDisabled = clBlack
    Colors.ItemButtonColors.BackColors.LightFocused = clBlack
    Colors.ItemButtonColors.BackColors.DarkColor = clBlack
    Colors.ItemButtonColors.BackColors.DarkHover = clBlack
    Colors.ItemButtonColors.BackColors.DarkPress = clBlack
    Colors.ItemButtonColors.BackColors.DarkDisabled = clBlack
    Colors.ItemButtonColors.BackColors.DarkFocused = clBlack
    Colors.ItemButtonColors.BorderColors.Enabled = False
    Colors.ItemButtonColors.BorderColors.LightColor = clBlack
    Colors.ItemButtonColors.BorderColors.LightHover = clBlack
    Colors.ItemButtonColors.BorderColors.LightPress = clBlack
    Colors.ItemButtonColors.BorderColors.LightDisabled = clBlack
    Colors.ItemButtonColors.BorderColors.LightFocused = clBlack
    Colors.ItemButtonColors.BorderColors.DarkColor = clBlack
    Colors.ItemButtonColors.BorderColors.DarkHover = clBlack
    Colors.ItemButtonColors.BorderColors.DarkPress = clBlack
    Colors.ItemButtonColors.BorderColors.DarkDisabled = clBlack
    Colors.ItemButtonColors.BorderColors.DarkFocused = clBlack
    Colors.ItemButtonColors.TextColors.Enabled = False
    Colors.ItemButtonColors.TextColors.LightColor = clBlack
    Colors.ItemButtonColors.TextColors.LightHover = clBlack
    Colors.ItemButtonColors.TextColors.LightPress = clBlack
    Colors.ItemButtonColors.TextColors.LightDisabled = clGray
    Colors.ItemButtonColors.TextColors.LightFocused = clBlack
    Colors.ItemButtonColors.TextColors.DarkColor = clWhite
    Colors.ItemButtonColors.TextColors.DarkHover = clWhite
    Colors.ItemButtonColors.TextColors.DarkPress = clWhite
    Colors.ItemButtonColors.TextColors.DarkDisabled = clGray
    Colors.ItemButtonColors.TextColors.DarkFocused = clWhite
    Colors.ItemButtonColors.DetailColors.Enabled = False
    Colors.ItemButtonColors.DetailColors.LightColor = clBlack
    Colors.ItemButtonColors.DetailColors.LightHover = clBlack
    Colors.ItemButtonColors.DetailColors.LightPress = clBlack
    Colors.ItemButtonColors.DetailColors.LightDisabled = clBlack
    Colors.ItemButtonColors.DetailColors.LightFocused = clBlack
    Colors.ItemButtonColors.DetailColors.DarkColor = clBlack
    Colors.ItemButtonColors.DetailColors.DarkHover = clBlack
    Colors.ItemButtonColors.DetailColors.DarkPress = clBlack
    Colors.ItemButtonColors.DetailColors.DarkDisabled = clBlack
    Colors.ItemButtonColors.DetailColors.DarkFocused = clBlack
    Colors.ItemButtonColors.ActiveColors.Enabled = False
    Colors.ItemButtonColors.ActiveColors.LightColor = clBlack
    Colors.ItemButtonColors.ActiveColors.LightHover = clBlack
    Colors.ItemButtonColors.ActiveColors.LightPress = clBlack
    Colors.ItemButtonColors.ActiveColors.LightDisabled = clBlack
    Colors.ItemButtonColors.ActiveColors.LightFocused = clBlack
    Colors.ItemButtonColors.ActiveColors.DarkColor = clBlack
    Colors.ItemButtonColors.ActiveColors.DarkHover = clBlack
    Colors.ItemButtonColors.ActiveColors.DarkPress = clBlack
    Colors.ItemButtonColors.ActiveColors.DarkDisabled = clBlack
    Colors.ItemButtonColors.ActiveColors.DarkFocused = clBlack
    Left = 592
    Top = 80
  end
  object MainMenu1: TMainMenu
    Left = 689
    Top = 80
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
  object SynGeneralSyn1: TSynGeneralSyn
    CommentAttri.Foreground = clGray
    CommentAttri.Style = []
    Comments = [csCStyle, csCPPStyle]
    DetectPreprocessor = False
    IdentifierAttri.Foreground = clMenuHighlight
    KeyAttri.Foreground = clFuchsia
    KeyAttri.Style = []
    KeyWords.Strings = (
      'AND'
      'ARRAY'
      'AS'
      'ASM'
      'BEGIN'
      'BREAK'
      'CASE'
      'CLASS'
      'CODE'
      'CONST'
      'CONSTRUCTOR'
      'DESTRUCTOR'
      'DISPINTERFACE'
      'DIV'
      'DO'
      'ELSE'
      'END'
      'EXCEPT'
      'EXPORT'
      'EXPORTS'
      'FILE'
      'FINALIZATION'
      'FINALLY'
      'FOR'
      'FUNCTION'
      'GOTO'
      'IF'
      'IMPLEMENTATION'
      'IMPORT'
      'IMPORTS'
      'IN'
      'INHERITED'
      'INITIALIZATION'
      'INLINE'
      'INTERFACE'
      'IS'
      'LABEL'
      'LIBRARY'
      'MOD'
      'NIL'
      'NOT'
      'OBJECT'
      'OF'
      'OR'
      'PACKED'
      'PROCEDURE'
      'PROJECT'
      'PROPERTY'
      'RAISE'
      'RECORD'
      'REPEAT'
      'RESOURCESTRING'
      'SET'
      'SHL'
      'SHR'
      'STRING'
      'THEN'
      'THREADVAR'
      'TO'
      'TRY'
      'TYPE'
      'UNIT'
      'UNTIL'
      'USES'
      'VAR'
      'WHILE'
      'WITH'
      'XOR')
    NumberAttri.Foreground = clRed
    PreprocessorAttri.Foreground = clLime
    StringAttri.Foreground = clAqua
    SymbolAttri.Foreground = clWhite
    Left = 782
    Top = 80
  end
end
