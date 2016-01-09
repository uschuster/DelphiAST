object frmSyntaxTree: TfrmSyntaxTree
  Left = 0
  Top = 0
  Width = 307
  Height = 466
  TabOrder = 0
  object VST: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 307
    Height = 366
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    TabOrder = 0
    OnBeforeItemErase = VSTBeforeItemErase
    OnFocusChanged = VSTFocusChanged
    OnGetText = VSTGetText
    OnInitChildren = VSTInitChildren
    OnInitNode = VSTInitNode
    Columns = <>
  end
  object VSTInformation: TVirtualStringTree
    Left = 0
    Top = 366
    Width = 307
    Height = 100
    Align = alBottom
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    TabOrder = 1
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
    OnGetText = VSTInformationGetText
    Columns = <
      item
        Position = 0
        Width = 100
        WideText = 'Type / Key'
      end
      item
        Position = 1
        Width = 150
        WideText = 'Value'
      end>
  end
end
