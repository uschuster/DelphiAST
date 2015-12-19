object Form9: TForm9
  Left = 0
  Top = 0
  Caption = 'Form9'
  ClientHeight = 484
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 760
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
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
end
