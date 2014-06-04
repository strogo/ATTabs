unit DemoForm;

interface

uses
  {$ifndef fpc}
  Windows,
  {$endif}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
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
    procedure TabClose(Sender: TObject; ATabIndex: Integer; var ACanClose: boolean);
    procedure TabDrawAfter(Sender: TObject;
      AType: TATTabElemType; ATabIndex: Integer;
      C: TCanvas; const ARect: TRect; var ACanDraw: boolean);
    procedure TabDrawBefore(Sender: TObject;
      AType: TATTabElemType; ATabIndex: Integer;
      C: TCanvas; const ARect: TRect; var ACanDraw: boolean);
  public
    { Public declarations }
    t, t0: TATTabs;
  end;

var
  Form1: TForm1;

implementation

uses Math;

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

  t.AddTab(-1, 'Tab');
  t.AddTab(-1, 'Tab middle len', nil, false, clGreen);
  t.AddTab(-1, 'Tab ________________________________________________________', nil, false, clBlue);
  t.AddTab(-1, 'I');
  t.AddTab(-1, 'I');
  t.AddTab(-1, 'I');
  t.AddTab(-1, 'I');
  t.AddTab(-1, 'I');

  //-----------------------------------
  //angle tabs below
  t0:= TATTabs.Create(Self);
  t0.Parent:= Self;
  t0.Align:= alBottom;
  t0.Font.Size:= 12;
  t0.Height:= 56;
  t0.OnTabDrawBefore:= {$ifdef FPC}@{$endif} TabDrawBefore;
  t0.OnTabDrawAfter:= {$ifdef FPC}@{$endif} TabDrawAfter;
  t0.ColorBg:= $F9EADB;

  t0.TabHeight:= 30;
  t0.TabWidthMax:= 170;
  t0.TabIndentTop:= 20;
  t0.TabIndentXSize:= 15;
  t0.TabIndentXInner:= 3;
  t0.TabIndentInit:= 4;
  //t0.TabShowclose:= tbShowActive;
  t0.TabShowplus:= false;
  t0.TabShowMenu:= false;
  t0.TabIndentDropI:= 6;

  t0.AddTab(-1, 'Owner-draw');
  t0.AddTab(-1, 'Tab wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww');
  t0.AddTab(-1, 'Last');

  //-----------------------------------
  //Firefox rectangle tabs
  t0:= TATTabs.Create(Self);
  t0.Parent:= Self;
  t0.Align:= alBottom;

  t0.Height:= 42;
  t0.TabShowBorderActiveLow:= true;
  t0.TabIndentInit:= 2;
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
  t0.TabDragEnabled:= false;

  t0.AddTab(-1, 'Firefox');
  t0.AddTab(-1, 'A tab _____________________________________________________', nil, false, clGreen);
  t0.AddTab(-1, 'Tab middle len', nil, false, clBlue);
end;

procedure TForm1.bAddClick(Sender: TObject);
begin
  t.AddTab(t.TabIndex+1, 'test '+StringOfChar('n', Random(20)), nil, false, Random(65000));
end;

procedure TForm1.bDelClick(Sender: TObject);
begin
  t.DeleteTab(1);
end;

procedure TForm1.bColorClick(Sender: TObject);
var
  d: TATTabData;
begin
  d:= t.GetTabData(t.tabindex);
  d.TabColor:= Random(60000);
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
{
var
  d: TATTabData;
  s: string;
  }
begin
  {
  d:= (Sender as TATTabs).GetTabData(ATabIndex);
  if d=nil then Exit;
  s:= d.TabCaption;
  ACanClose:= Pos('Tab', s)>0;
  }
  ACanClose:= true;
end;

procedure TForm1.Edit1Change(Sender: TObject);
var
  d: TATTabData;
begin
  d:= t.GetTabData(t.tabIndex);
  if d=nil then Exit;
  if LockEdit then Exit;

  d.TabCaption:= Edit1.Text;
  t.Invalidate;
end;

procedure TForm1.chkXClick(Sender: TObject);
begin
  if chkX.Checked then
    t.TabShowClose:= tbShowAll
  else
    t.TabShowClose:= tbShowNone;
  t.Invalidate;
end;

procedure TForm1.chkPlusClick(Sender: TObject);
begin
  t.TabShowPlus:= chkPlus.Checked;
  t.Invalidate;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  chkX.Checked:= t.TabShowClose=tbShowAll;
  chkPlus.Checked:= t.TabShowPlus;
end;

procedure TForm1.TabDrawAfter(Sender: TObject;
  AType: TATTabElemType; ATabIndex: Integer;
  C: TCanvas; const ARect: TRect; var ACanDraw: boolean);
begin
  if ATabIndex<0 then Exit;
  C.Font.Name:= 'Tahoma';
  C.Font.Size:= 8;
  C.Font.Color:= clBlue;
  C.TextOut((ARect.Left+ARect.Right) div 2 - 8, ARect.Top, Inttostr(ATabIndex));
end;

procedure TForm1.TabDrawBefore(Sender: TObject;
  AType: TATTabElemType; ATabIndex: Integer;
  C: TCanvas; const ARect: TRect; var ACanDraw: boolean);
var
  NColor: TColor;
begin
  if AType in [aeXButton, aeXButtonOver] then
  begin
    NColor:= C.Pen.Color;
    C.Pen.Width:= 2;
    C.Pen.Color:= IfThen(AType=aeXButton, clBlue, clRed);
    C.Ellipse(ARect);
    ACanDraw:= false;
    C.Pen.Color:= NColor;
    C.Pen.Width:= 1;
  end;
end;

end.
