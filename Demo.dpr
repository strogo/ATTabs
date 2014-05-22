program Project1;

uses
  Forms,
  DemoForm in 'DemoForm.pas' {Form1},
  ATTabs in 'ATTabs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
