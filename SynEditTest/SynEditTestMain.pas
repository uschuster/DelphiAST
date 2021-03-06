unit SynEditTestMain;

interface

uses
  {$IFDEF WITH_SYNTAX_TREE}
  DelphiASTSyntaxTreeFrame,
  {$ENDIF}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, SynEdit,
  SynEditHighlighter, SynHighlighterPas, DelphiASTSynProxy, DelphiAST.Classes,
  Vcl.StdCtrls;

type
  TForm9 = class(TForm)
    Panel1: TPanel;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    PaintBox1: TPaintBox;
    btnOpen: TButton;
    OpenDialog: TOpenDialog;
    pnlSyntaxTree: TPanel;
    splSyntaxTree: TSplitter;
    btnFocusSyntaxTreeNodeAtCursor: TButton;
    cbColorScheme: TComboBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnFocusSyntaxTreeNodeAtCursorClick(Sender: TObject);
    procedure cbColorSchemeChange(Sender: TObject);
  private
    { Private declarations }
    FLegendBottomY: Integer;
    FHighlighter: TJVCSCompressedDiffSynProxyHighlighter;
    FNode: TSyntaxNode;
    FNodeCount: Integer;
    FNodeFileName: string;
    FNodeMaxLevel: Integer;
    FSampleMode: Boolean;
    {$IFDEF WITH_SYNTAX_TREE}
    FSyntaxTreeFrame: TfrmSyntaxTree;
    {$ENDIF}
    procedure AddSampleRanges;
    procedure DrawLegend(ACanvas: TCanvas; ARect: TRect);
    function FindNodeAtCursor(ACol, ALine: Integer; ANearest: Boolean): TSyntaxNode;
    {$IFDEF WITH_SYNTAX_TREE}
    procedure HandleSyntaxTreeGetColor(Sender: TObject; ANode: TSyntaxNode; ALevel: Integer; var AColor: TColor);
    procedure HandleSyntaxTreeFocused(Sender: TObject; ANode: TSyntaxNode);
    {$ENDIF}
    procedure UpdateCaption;
    procedure UpdateNode;
    procedure UpdateRanges;
    procedure UpdateRangesSemantic;
  public
    { Public declarations }
  end;

var
  Form9: TForm9;

implementation

uses
  DelphiAST, DelphiASTTempUtils, SymbolResolver;

{$R *.dfm}

procedure TForm9.btnOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    SynEdit1.Lines.LoadFromFile(OpenDialog.FileName);
    FNodeFileName := OpenDialog.FileName;
    UpdateNode;
    if cbColorScheme.ItemIndex = 1 then
    begin
      ResolveSymbols(FNode);
      UpdateRangesSemantic;
    end
    else
      UpdateRanges;
    UpdateCaption;
  end;
end;

procedure TForm9.cbColorSchemeChange(Sender: TObject);
begin
  if cbColorScheme.ItemIndex = 1 then
  begin
    FHighlighter.ColorScheme := hcsSemantic;
    ResolveSymbols(FNode);
    UpdateRangesSemantic;
  end
  else
  begin
    FHighlighter.ColorScheme := hcsLevels;
    UpdateRanges;
  end;
  SynEdit1.Invalidate;
  PaintBox1.Invalidate;
  UpdateCaption;
end;

procedure TForm9.btnFocusSyntaxTreeNodeAtCursorClick(Sender: TObject);
var
  Node: TSyntaxNode;
begin
  Node := FindNodeAtCursor(SynEdit1.CaretX, SynEdit1.CaretY, True);
  if Assigned(Node) then
  begin
    {$IFDEF WITH_SYNTAX_TREE}
    FSyntaxTreeFrame.FocusNode(Node);
    {$ENDIF}
  end
  else
    ShowMessage('No node found');
end;

procedure TForm9.DrawLegend(ACanvas: TCanvas; ARect: TRect);
var
  I, Y, TextX: Integer;
  ColorRect: TRect;
  S: string;
  R: TSize;
begin
  ACanvas.Brush.Color := clWindow;
  ACanvas.FillRect(ARect);
  R := ACanvas.TextExtent('Level 0');
  TextX := ARect.Left + 2 + R.cy + 2;
  Y := ARect.Top + 2;
  for I := 0 to FHighlighter.LevelColorCount - 1 do
  begin
    ColorRect := Rect(ARect.Left + 2, Y, ARect.Top + 2 + R.cy, Y + R.cy);
    ACanvas.Brush.Color := FHighlighter.LevelColors[I];
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Rectangle(ColorRect);
    ACanvas.Brush.Style := bsClear;
    if FHighlighter.ColorScheme = hcsSemantic then
    begin
      if I = 0 then
        S := 'Other'
      else
        S := Format('Symbol #%d', [I]);
    end
    else
      S := Format('Level %d', [I]);
    ACanvas.TextOut(TextX, Y, S);
    Inc(Y, R.cy + 2);
    FLegendBottomY := Y;
  end;
end;

function TForm9.FindNodeAtCursor(ACol, ALine: Integer; ANearest: Boolean): TSyntaxNode;

  function FindNode(ANode: TSyntaxNode): TSyntaxNode;
  var
    I: Integer;
    NodeMatches: Boolean;
  begin
    Result := nil;
    NodeMatches := (ANode.Col = ACol) and (ANode.Line = ALine);
    for I := Low(ANode.ChildNodes) to High(ANode.ChildNodes) do
    begin
      Result := FindNode(ANode.ChildNodes[I]);
      if Assigned(Result) then
        Break;
    end;
    if not Assigned(Result) and NodeMatches then
      Result := ANode;
  end;

  function GetDistance(ANode: TSyntaxNode): Integer;
  begin
    Result := MaxInt;
    if (ANode.Col < ACol) and (ANode.Line = ALine) then
      Result := ACol - ANode.Col;
  end;

  function FindNearestNode(ANode: TSyntaxNode; var ADistance: Integer): TSyntaxNode;
  var
    I, NodeDistance, ChildDistance: Integer;
    ChildResult, BestChildResult: TSyntaxNode;
  begin
    Result := nil;
    NodeDistance := GetDistance(ANode);
    if NodeDistance <= ADistance then
    begin
      ADistance := NodeDistance;
      Result := ANode;
    end;
    ChildDistance := ADistance;
    BestChildResult := nil;
    for I := Low(ANode.ChildNodes) to High(ANode.ChildNodes) do
    begin
      ChildResult := FindNearestNode(ANode.ChildNodes[I], ChildDistance);
      if Assigned(ChildResult) then
        BestChildResult := ChildResult;
      if ADistance = 1 then
        Break;
    end;
    if (ChildDistance <= ADistance) and Assigned(BestChildResult) then
    begin
      Result := BestChildResult;
      ADistance := ChildDistance;
    end;
  end;

var
  Distance: Integer;
begin
  Result := FindNode(FNode);
  if not Assigned(Result) and ANearest then
  begin
    Distance := MaxInt;
    Result := FindNearestNode(FNode, Distance);
  end;
end;

procedure TForm9.FormCreate(Sender: TObject);
begin
  SynPasSyn1.UseUserSettings(1);
  FHighlighter := TJVCSCompressedDiffSynProxyHighlighter.Create(nil);
  FHighlighter.InternalHighlighter := SynPasSyn1;
  SynEdit1.Highlighter := FHighlighter;
  cbColorScheme.ItemIndex := 0;
  {$IFDEF WITH_SYNTAX_TREE}
  FSyntaxTreeFrame := TfrmSyntaxTree.Create(Self);
  FSyntaxTreeFrame.Parent := pnlSyntaxTree;
  FSyntaxTreeFrame.Align := alClient;
  FSyntaxTreeFrame.OnGetColor := HandleSyntaxTreeGetColor;
  FSyntaxTreeFrame.OnSyntaxNodeFocusedEvent := HandleSyntaxTreeFocused;
  pnlSyntaxTree.Visible := True;
  splSyntaxTree.Visible := True;
  btnFocusSyntaxTreeNodeAtCursor.Visible := True;
  SynEdit1.AlwaysShowCaret := True;
  {$ENDIF}

  FSampleMode := False;
  if FSampleMode then
    AddSampleRanges
  else
  begin
    UpdateNode;
    UpdateRanges;
    UpdateCaption;
  end;
end;

{$IFDEF WITH_SYNTAX_TREE}
procedure TForm9.HandleSyntaxTreeGetColor(Sender: TObject; ANode: TSyntaxNode; ALevel: Integer; var AColor: TColor);
begin
  if ALevel < FHighlighter.LevelColorCount then
    AColor := FHighlighter.LevelColors[ALevel];
end;

procedure TForm9.HandleSyntaxTreeFocused(Sender: TObject; ANode: TSyntaxNode);
var
  C: TCompoundSyntaxNode;
  NewCaret, FromCoord, ToCoord: TBufferCoord;
begin
  if Assigned(ANode) then
  begin
    FromCoord.Char := ANode.Col;
    FromCoord.Line := ANode.Line;
    if ANode is TCompoundSyntaxNode then
    begin
      C := TCompoundSyntaxNode(ANode);
      ToCoord.Char := C.EndCol + 1;
      ToCoord.Line := C.EndLine;
    end
    else
      ToCoord := FromCoord;
  end
  else
  begin
    FromCoord := SynEdit1.CaretXY;
    ToCoord := FromCoord;
  end;
  NewCaret := FromCoord;
  SynEdit1.SetCaretAndSelection(NewCaret, FromCoord, ToCoord);
end;
{$ENDIF}

procedure TForm9.PaintBox1Paint(Sender: TObject);
begin
  DrawLegend(PaintBox1.Canvas, Rect(0, 0, PaintBox1.Width, PaintBox1.Height));
  PaintBox1.Height := FLegendBottomY;
end;

procedure TForm9.UpdateCaption;
begin
  Caption := Format('DelphiAST SynEdit Test [FileName: %s Node Count: %d Max Level: %d]',
    [FNodeFileName, FNodeCount, FNodeMaxLevel]);
end;

procedure TForm9.UpdateNode;
var
  B: TPasSyntaxTreeBuilder;
  SS: TStringStream;
begin
  {$IFDEF WITH_SYNTAX_TREE}
  FSyntaxTreeFrame.Node := nil;
  {$ENDIF}
  FreeAndNil(FNode);
  B := TPasSyntaxTreeBuilder.Create;
  try
    B.InitDefinesDefinedByCompiler;
    SS := TStringStream.Create(SynEdit1.Text);
    try
      FNode := B.Run(SS);
    finally
      SS.Free;
    end;
  finally
    B.Free;
  end;
  {$IFDEF WITH_SYNTAX_TREE}
  FSyntaxTreeFrame.Node := FNode;
  {$ENDIF}
end;

procedure TForm9.UpdateRanges;

  procedure WalkNodes(ANode: TSyntaxNode; ALevel: Integer = 0);
  var
    I: Integer;
    BR: TBlockRange;
    //C: TCompoundSyntaxNode;
  begin
    Inc(FNodeCount);
    if ALevel > FNodeMaxLevel then
      FNodeMaxLevel := ALevel;
    {
    if ANode is TCompoundSyntaxNode then
    begin
      C := TCompoundSyntaxNode(ANode);
      BR := FHighlighter.BlockRanges.Add;
      BR.FromCol := C.Col;
      BR.FromLine := C.Line;
      BR.ToCol := C.EndCol;
      BR.ToLine := C.EndLine;
      BR.Level := ALevel;
    end;
    }
    if ANode.HasEnd then
    begin
      BR := FHighlighter.BlockRanges.Add;
      BR.FromCol := ANode.Col;
      BR.FromLine := ANode.Line;
      BR.ToCol := ANode.FixedEndCol;
      BR.ToLine := ANode.FixedEndLine;
      BR.Level := ALevel;
    end;

    for I := Low(ANode.ChildNodes) to High(ANode.ChildNodes) do
      WalkNodes(ANode.ChildNodes[I], ALevel + 1);
  end;

begin
  FHighlighter.BlockRanges.Clear;
  FNodeCount := 0;
  FNodeMaxLevel := 0;
  if Assigned(FNode) then
    WalkNodes(FNode);
end;

procedure TForm9.UpdateRangesSemantic;

  procedure WalkNodes(ANode: TSyntaxNode);
  var
    I: Integer;
    BR: TBlockRange;
  begin
    Inc(FNodeCount);
    if Assigned(ANode.SymbolNode) or (ANode.ColorIndex > 0) then
    begin
      BR := FHighlighter.BlockRanges.Add;
      BR.FromCol := ANode.Col;
      BR.FromLine := ANode.Line;
      BR.ToCol := ANode.FixedEndCol;
      BR.ToLine := ANode.FixedEndLine;
      if Assigned(ANode.SymbolNode) then
        BR.Level := ANode.SymbolNode.ColorIndex - 1
      else
        BR.Level := ANode.ColorIndex - 1;
      if BR.Level > FNodeMaxLevel then
        FNodeMaxLevel := BR.Level;
    end;
    for I := Low(ANode.ChildNodes) to High(ANode.ChildNodes) do
      WalkNodes(ANode.ChildNodes[I]);
  end;

begin
  FHighlighter.BlockRanges.Clear;
  FNodeCount := 0;
  FNodeMaxLevel := 0;
  if Assigned(FNode) then
    WalkNodes(FNode);
end;

procedure TForm9.AddSampleRanges;
var
  BR: TBlockRange;
begin
  BR := FHighlighter.BlockRanges.Add;
  BR.FromCol := 1;
  BR.FromLine := 7;
  BR.ToCol := 4;
  BR.ToLine := 24;
  BR.Level := 0;

  BR := FHighlighter.BlockRanges.Add;
  BR.FromCol := 1;
  BR.FromLine := 8;
  BR.ToCol := 12;
  BR.ToLine := 9;
  BR.Level := 1;

  BR := FHighlighter.BlockRanges.Add;
  BR.FromCol := 3;
  BR.FromLine := 9;
  BR.ToCol := 12;
  BR.ToLine := 9;
  BR.Level := 2;

  BR := FHighlighter.BlockRanges.Add;
  BR.FromCol := 1;
  BR.FromLine := 10;
  BR.ToCol := 4;
  BR.ToLine := 24;
  BR.Level := 1;

  BR := FHighlighter.BlockRanges.Add;
  BR.FromCol := 3;
  BR.FromLine := 11;
  BR.ToCol := 16;
  BR.ToLine := 11;
  BR.Level := 2;

  BR := FHighlighter.BlockRanges.Add;
  BR.FromCol := 3;
  BR.FromLine := 13;
  BR.ToCol := 6;
  BR.ToLine := 16;
  BR.Level := 2;

  BR := FHighlighter.BlockRanges.Add;
  BR.FromCol := 5;
  BR.FromLine := 14;
  BR.ToCol := 8;
  BR.ToLine := 15;
  BR.Level := 3;
end;

end.
