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
    Height = 466
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
    OnGetText = VSTGetText
    OnInitChildren = VSTInitChildren
    OnInitNode = VSTInitNode
    Columns = <>
  end
end
