program SynEditTest;

uses
  Vcl.Forms,
  SynEditTestMain in 'SynEditTestMain.pas' {Form9},
  DelphiASTSynProxy in 'DelphiASTSynProxy.pas',
  DelphiASTTempUtils in 'DelphiASTTempUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm9, Form9);
  Application.Run;
end.
