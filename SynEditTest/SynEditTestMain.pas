unit SynEditTestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, SynEdit,
  SynEditHighlighter, SynHighlighterPas, DelphiASTSynProxy;

type
  TForm9 = class(TForm)
    Panel1: TPanel;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FHighlighter: TJVCSCompressedDiffSynProxyHighlighter;
  public
    { Public declarations }
  end;

var
  Form9: TForm9;

implementation

{$R *.dfm}

procedure TForm9.FormCreate(Sender: TObject);
var
  BR: TBlockRange;
begin
  SynPasSyn1.UseUserSettings(1);
  FHighlighter := TJVCSCompressedDiffSynProxyHighlighter.Create(nil);
  FHighlighter.InternalHighlighter := SynPasSyn1;
  SynEdit1.Highlighter := FHighlighter;

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
