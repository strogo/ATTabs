unit DemoForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ATTabs;

type
  TForm1 = class(TForm)
    bAdd: TButton;
    bDel: TButton;
    bColor: TButton;
    bLeft: TButton;
    bRt: TButton;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure bAddClick(Sender: TObject);
    procedure bDelClick(Sender: TObject);
    procedure bColorClick(Sender: TObject);
    procedure bLeftClick(Sender: TObject);
    procedure bRtClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }
    LockEdit: boolean;
    procedure TabClick(A: TObject);
    procedure TabPlusClick(A: TObject);
    procedure TabClose(Sender: TObject; ATabIndex: Integer;
    var ACanClose: boolean);
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
  t.Height:= 33;
  t.OnTabClick:= TabClick;
  t.OnTabPlusClick:= TabPlusClick;
  t.OnTabClose:= TabClose;
  t.TabAngle:= 4;
  t.TabIndentText:= 0;
  t.TabIndentInit:= 20;
  //t.TabIndentXRight:= 30;

  t.DoAddTab('Tab');
  t.DoAddTab('I');
  t.DoAddTab('Tab three', nil, false, clGreen);
  t.DoAddTab('Tab ________________________________________________________', nil, false, clBlue);
  t.DoAddTab('i');

  //angle
  t0:= TATTabs.Create(Self);
  t0.Parent:= Self;
  t0.Align:= alBottom;
  t0.TabButtonclose:= false;
  t0.TabButtonplus:= false;
  t0.DoAddTab('Tab');
  t0.DoAddTab('Tab wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww');

  //rectangle
  t0:= TATTabs.Create(Self);
  t0.Parent:= Self;
  t0.Align:= alBottom;
  t0.TabAngle:= 0;
  t0.TabIndentInter:= 2;
  t0.DoAddTab('Tab');
  t0.DoAddTab('Tab _____________________________________________________', nil, false, clGreen);
  t0.DoAddTab('Tab middle len', nil, false, clBlue);
end;

procedure TForm1.bAddClick(Sender: TObject);
begin
  t.DoAddTab('test '+StringOfChar('n', Random(20)), nil, false, Random(65000));
end;

procedure TForm1.bDelClick(Sender: TObject);
begin
  t.DoDeleteTab(1);
end;

procedure TForm1.bColorClick(Sender: TObject);
var
  Data: TATTabData;
begin
  Data:= t.GetTabData(t.tabindex);
  Data.TabColor:= Random(60000);
  t.Invalidate;
end;

procedure TForm1.bLeftClick(Sender: TObject);
begin
  t.tabIndex:= t.TabIndex-1;
end;

procedure TForm1.bRtClick(Sender: TObject);
begin
  t.tabIndex:= t.TabIndex+1;
end;

procedure TForm1.TabClick(A: TObject);
var
  d: TATTabData;
begin
  d:= t.GetTabData(t.TabIndex);
  LockEdit:= true;
  if Assigned(d) then
    Edit1.Text:= d.TabCaption
  else
    Edit1.Text:= '';  
  LockEdit:= false;
end;

procedure TForm1.TabPlusClick(A: TObject);
begin
  bAdd.Click;
end;


procedure TForm1.TabClose(Sender: TObject; ATabIndex: Integer;
  var ACanClose: boolean);
var
  d: TATTabData;
  s: string;
begin
  d:= (Sender as TATTabs).GetTabData(ATabIndex);
  if d=nil then Exit;
  s:= d.TabCaption;

  ACanClose:= true; //Pos('Tab', s)>0;
  if not ACanClose then
    MessageBeep(mb_iconwarning);
end;

procedure TForm1.Edit1Change(Sender: TObject);
var
  d: TATTabData;
begin
  d:= t.GetTabData(t.tabIndex);
  if d=nil then Exit;
  if LockEdit then Exit;

  d.TabCaption:= Edit1.Text;
  t.DoUpdateTabWidth(t.tabIndex);
end;

end.
