unit SynEditTestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, SynEdit,
  SynEditHighlighter, SynHighlighterPas, DelphiASTSynProxy, DelphiAST.Classes;

type
  TForm9 = class(TForm)
    Panel1: TPanel;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FHighlighter: TJVCSCompressedDiffSynProxyHighlighter;
    FNode: TSyntaxNode;
    FSampleMode: Boolean;
    procedure AddSampleRanges;
    procedure UpdateNode;
    procedure UpdateRanges;
  public
    { Public declarations }
  end;

var
  Form9: TForm9;

implementation

uses
  DelphiAST;

{$R *.dfm}

procedure TForm9.FormCreate(Sender: TObject);
begin
  SynPasSyn1.UseUserSettings(1);
  FHighlighter := TJVCSCompressedDiffSynProxyHighlighter.Create(nil);
  FHighlighter.InternalHighlighter := SynPasSyn1;
  SynEdit1.Highlighter := FHighlighter;

  FSampleMode := False;
  if FSampleMode then
    AddSampleRanges
  else
  begin
    UpdateNode;
    UpdateRanges;
  end;
end;

procedure TForm9.UpdateNode;
var
  B: TPasSyntaxTreeBuilder;
  SS: TStringStream;
begin
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
end;

procedure TForm9.UpdateRanges;

  procedure WalkNodes(ANode: TSyntaxNode; ALevel: Integer = 0);
  var
    I: Integer;
    BR: TBlockRange;
    C: TCompoundSyntaxNode;
  begin
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

    for I := Low(ANode.ChildNodes) to High(ANode.ChildNodes) do
      WalkNodes(ANode.ChildNodes[I], ALevel + 1);
  end;

begin
  FHighlighter.BlockRanges.Clear;
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
