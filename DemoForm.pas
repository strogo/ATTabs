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
    chkX: TCheckBox;
    chkPlus: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure bAddClick(Sender: TObject);
    procedure bDelClick(Sender: TObject);
    procedure bColorClick(Sender: TObject);
    procedure bLeftClick(Sender: TObject);
    procedure bRtClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure chkXClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkPlusClick(Sender: TObject);
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
  //default tabs
  t:= TATTabs.Create(Self);
  t.Parent:= Self;
  t.Align:= alTop;
  t.OnTabClick:=     {$ifdef FPC}@{$endif} TabClick;
  t.OnTabPlusClick:= {$ifdef FPC}@{$endif} TabPlusClick;
  t.OnTabClose:=     {$ifdef FPC}@{$endif} TabClose;

  t.DoAddTab('Tab');
  t.DoAddTab('Tab middle len', nil, false, clGreen);
  t.DoAddTab('Tab ________________________________________________________', nil, false, clBlue);
  t.DoAddTab('I');

  //-----------------------------------
  //angle tabs below
  t0:= TATTabs.Create(Self);
  t0.Parent:= Self;
  t0.Align:= alBottom;
  t0.Font.Size:= 12;
  t0.Height:= 56;
  t0.ColorBg:= $F9EADB;
  t0.TabIndentTop:= 20;
  t0.TabIndentBottom:= 6;
  t0.TabIndentXSize:= 15;
  t0.TabIndentXInner:= 3;
  t0.TabIndentInit:= 4;
  t0.TabShowclose:= true; //false;
  t0.TabShowplus:= false;
  t0.TabShowScroll:= false;
  t0.TabShowMenu:= true;

  t0.DoAddTab('Tab');
  t0.DoAddTab('Tab wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww');

  //-----------------------------------
  //Firefox rectangle tabs
  t0:= TATTabs.Create(Self);
  t0.Parent:= Self;
  t0.Align:= alBottom;

  t0.Height:= 42;
  t0.TabShowScroll:= false;
  t0.TabShowBorderActiveLow:= true;
  t0.TabIndentInit:= 2;
  t0.TabIndentBottom:= 14;
  t0.TabIndentTop:= 4;
  t0.TabIndentXSize:= 13;
  t0.Font.Color:= clBlack;
  t0.ColorBg:= $F9EADB;
  t0.ColorBorderActive:= $ACA196;
  t0.ColorBorderPassive:= $ACA196;
  t0.ColorTabActive:= $FCF5ED;
  t0.ColorTabPassive:= $E0D3C7;
  t0.ColorTabOver:= $F2E4D7;
  t0.ColorCloseBg:= clNone;
  t0.ColorCloseBgOver:= $D5C9BD;
  t0.ColorCloseBorderOver:= $B0B0B0;
  t0.ColorCloseX:= $7B6E60;
  t0.ColorArrow:= $5C5751;
  t0.ColorArrowOver:= t0.ColorArrow;

  t0.TabAngle:= 0;
  t0.TabIndentInter:= 2;
  t0.DoAddTab('Firefox');
  t0.DoAddTab('A tab _____________________________________________________', nil, false, clGreen);
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

procedure TForm1.chkXClick(Sender: TObject);
var
  i: Integer;
begin
  t.TabShowClose:= chkX.Checked;
  for i:= 0 to t.TabCount-1 do
    t.DoUpdateTabWidth(i);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  chkX.Checked:= t.TabShowClose;
  chkPlus.Checked:= t.TabShowPlus;
end;

procedure TForm1.chkPlusClick(Sender: TObject);
var
  i: Integer;
begin
  t.TabShowPlus:= chkPlus.Checked;
  for i:= 0 to t.TabCount-1 do
    t.DoUpdateTabWidth(i);
end;

end.
