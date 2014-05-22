unit DemoForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ATTabs;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    t, t0: TATTabs;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  t:= TATTabs.Create(Self);
  t.Parent:= Self;
  t.Align:= alTop;
  t.Left:= 80;
  t.Top:= 100;
  t.Width:= 600;
  t.Height:= 34;

  t.TabAngle:= 5;
  t.TabIndentText:= 0;
  t.TabIndentInit:= 20;

  t.DoAddTab('Tab');
  t.DoAddTab('I');
  t.DoAddTab('Tab three', clGreen);
  t.DoAddTab('Tab wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww', clBlue);
  t.DoAddTab('i');

  t0:= TATTabs.Create(Self);
  t0.Parent:= Self;
  t0.Align:= alBottom;
  t0.DoAddTab('Tab');
  t0.DoAddTab('Tab wwwwwwwwwwwwwwwwww');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  t.DoAddTab('test '+Inttostr(t.GetTabCount+1), Random(65000));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  t.DoDeleteTab(1);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  t.DoSetTabColor(1, Random(60000));
end;

end.
