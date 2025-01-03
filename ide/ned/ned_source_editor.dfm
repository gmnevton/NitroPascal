object NEDEditorForm: TNEDEditorForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 800
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object SynEdit1: TSynEdit
    Left = 0
    Top = 40
    Width = 600
    Height = 760
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
    TabOrder = 0
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
  end
  object UPanel4: TUPanel
    Left = 0
    Top = 0
    Width = 600
    Height = 40
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
    object UScrollBox1: TUScrollBox
      Left = 0
      Top = 0
      Width = 600
      Height = 40
      HorzScrollBar.Tracking = True
      VertScrollBar.Tracking = True
      Align = alClient
      BevelEdges = []
      BevelInner = bvNone
      BevelOuter = bvNone
      DoubleBuffered = True
      Color = 2039583
      ParentColor = False
      ParentDoubleBuffered = False
      TabOrder = 0
      StyleElements = []
      BackColor.Enabled = False
      BackColor.Color = clBlack
      BackColor.LightColor = 15132390
      BackColor.DarkColor = 2039583
      ScrollOrientation = oHorizontal
      object USymbolButton1: TUSymbolButton
        Left = 0
        Top = 0
        Align = alLeft
        ParentColor = False
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
        Text = 'FileName'
        Detail = 'FileType'
        IsToggleButton = True
        KeepOrginalColor = False
      end
      object USymbolButton2: TUSymbolButton
        Left = 250
        Top = 0
        Align = alLeft
        ParentColor = False
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
        SymbolChar = #59715
        Text = 'FileName'
        Detail = 'FileType'
        IsToggleButton = True
        KeepOrginalColor = False
        ExplicitLeft = 296
        ExplicitTop = 3
      end
    end
  end
  object SynGeneralSyn1: TSynGeneralSyn
    CommentAttri.Foreground = clGray
    CommentAttri.Style = []
    Comments = [csCStyle, csCPPStyle]
    DetectPreprocessor = False
    IdentifierAttri.Foreground = clMenuHighlight
    IdentifierChars = '_-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
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
    Left = 343
    Top = 16
  end
  object UPopupMenu1: TUPopupMenu
    AniSet.AniKind = akOut
    AniSet.AniFunctionKind = afkQuartic
    AniSet.DelayStartTime = 0
    AniSet.Duration = 120
    AniSet.Step = 20
    Left = 430
    Top = 16
  end
end
