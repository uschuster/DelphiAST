object Form9: TForm9
  Left = 0
  Top = 0
  Caption = 'Form9'
  ClientHeight = 659
  ClientWidth = 760
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox1: TPaintBox
    Left = 0
    Top = 484
    Width = 760
    Height = 175
    Align = alBottom
    OnPaint = PaintBox1Paint
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 760
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btnOpen: TButton
      Left = 4
      Top = 4
      Width = 133
      Height = 25
      Caption = 'Open Delphi Source File'
      TabOrder = 0
      OnClick = btnOpenClick
    end
  end
  object SynEdit1: TSynEdit
    Left = 0
    Top = 41
    Width = 760
    Height = 443
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Pitch = fpFixed
    Font.Style = []
    TabOrder = 1
    Gutter.Font.Charset = ANSI_CHARSET
    Gutter.Font.Color = clGray
    Gutter.Font.Height = -13
    Gutter.Font.Name = 'Consolas'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Highlighter = SynPasSyn1
    Lines.Strings = (
      'unit t2;'
      ''
      'interface'
      ''
      'implementation'
      ''
      'procedure Test;'
      'var'
      '  S: string;'
      'begin'
      '  S := '#39'FooBar'#39';'
      ''
      '  begin'
      '    begin'
      '      begin'
      '        begin'
      '        end;'
      '      end;'
      '    end;'
      '  end;'
      ''
      '  repeat'
      '  until False;'
      ''
      '  while True do'
      '  begin'
      '  end;'
      'end;'
      ''
      'end.'
      ''
      '')
    FontSmoothing = fsmNone
  end
  object SynPasSyn1: TSynPasSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 176
    Top = 144
  end
  object OpenDialog: TOpenDialog
    Filter = 'Delphi Unit|*.pas|Delphi Package|*.dpk|Delphi Project|*.dpr'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 272
    Top = 96
  end
end
