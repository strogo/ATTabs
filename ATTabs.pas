unit ATTabs;

interface

{$ifndef FPC}
{$define windows}
{$endif}

uses
  {$ifdef windows}
  Windows, Messages,
  {$endif}
  {$ifdef FPC}
  LCLIntf,
  {$endif}
  Classes, Types, Graphics,
  Controls, ExtCtrls, Menus;

type
  TATTabData = class
  public
    TabCaption: string;
    TabObject: TObject;
    TabColor: TColor;
    TabModified: boolean;
  end;

type
  TATTabCloseEvent = procedure (Sender: TObject; ATabIndex: Integer;
    var ACanClose: boolean) of object;
  TATTabMenuEvent = procedure (Sender: TObject;
    var ACanShow: boolean) of object;
  TATTabDrawEvent = procedure (Sender: TObject; ATabIndex: Integer;
    ACanvas: TCanvas; const ARect: TRect; var ACanDraw: boolean) of object;

type
  TATTriType = (triDown, triLeft, triRight);
  TATTabShowClose = (tbShowNone, tbShowAll, tbShowActive);

//int constants for GetTabAt
const
  cAtTabPlus = -2;
  cAtArrowLeft = -3;
  cAtArrowRight = -4;
  cAtArrowDown = -5;

type
  TATTabs = class(TPanel)
  private
    FColorBg: TColor; //color of background (visible at top and between tabs)
    FColorBorderActive: TColor; //color of 1px border of active tab
    FColorBorderPassive: TColor; //color of 1px border of inactive tabs
    FColorTabActive: TColor; //color of active tab
    FColorTabPassive: TColor; //color of inactive tabs
    FColorTabOver: TColor; //color of inactive tabs, mouse-over
    FColorCloseBg: TColor; //color of small square with "x" mark, inactive
    FColorCloseBgOver: TColor; //color of small square with "x" mark, mouse-over
    FColorCloseBorderOver: TColor; //color of 1px border of "x" mark, mouse-over
    FColorCloseX: TColor; //color of "x" mark
    FColorArrow: TColor; //color of "down" arrow (tab menu), inactive
    FColorArrowOver: TColor; //color of "down" arrow, mouse-over
    
    FTabAngle: Integer; //angle of tab border: from 0 (vertcal border) to any size
    FTabWidthMin: Integer; //tab minimal width (used when lot of tabs)  
    FTabWidthMax: Integer; //tab maximal width (used when only few tabs)
    FTabWidthHideX: Integer; //tab minimal width, after which "x" mark hides for inactive tabs
    FTabIndentInter: Integer; //space between nearest tabs (no need for angled tabs)
    FTabIndentInit: Integer; //space between first tab and left control edge
    FTabIndentLeft: Integer; //space between text and tab left edge
    FTabIndentText: Integer; //space between text and tab top edge
    FTabIndentTop: Integer; //height of top empty space (colored with bg)
    FTabIndentBottom: Integer; //height of bottom empty space (colored with active tab)
    FTabIndentXRight: Integer; //space from "x" btn to right tab edge
    FTabIndentXInner: Integer; //space from "x" square edge to "x" mark
    FTabIndentXSize: Integer; //size of "x" mark
    FTabIndentColor: Integer; //height of "misc color" line
    FTabIndentArrowSize: Integer; //half-size of "arrow" mark
    FTabIndentArrowLeft: Integer; //space from scroll-arrows to left control edge
    FTabIndentArrowRight: Integer; //width of down-arrow area at right
    FTabShowClose: TATTabShowClose; //show mode for "x" buttons
    FTabShowPlus: boolean; //show "plus" tab
    FTabShowPlusText: string; //text of "plus" tab
    FTabShowScroll: boolean; //show scroll arrows (not implemented)
    FTabShowMenu: boolean; //show down arrow (menu of tabs)
    FTabShowBorderActiveLow: boolean; //show border line below active tab (like Firefox)

    //private
    FTabWidth: Integer; //tab width current (auto-sized)
    FTabIndex: Integer;
    FTabIndexOver: Integer;
    FTabList: TList;
    FTabMenu: TPopupMenu;

    FBitmap: TBitmap;
    FBitmapText: TBitmap;
    FOnTabClick: TNotifyEvent;
    FOnTabPlusClick: TNotifyEvent;
    FOnTabClose: TATTabCloseEvent;
    FOnTabMenu: TATTabMenuEvent;
    FOnTabDrawBefore: TATTabDrawEvent;
    FOnTabDrawAfter: TATTabDrawEvent;

    procedure DoPaintTo(C: TCanvas);
    procedure DoPaintTabTo(C: TCanvas; ARect: TRect;
      const ACaption: string;
      ATabBg, ATabBorder, ATabBorderLow, ATabHilite, ATabCloseBg, ATabCloseBorder: TColor;
      ACloseBtn: boolean);
    procedure DoPaintArrowTo(C: TCanvas; ATyp: TATTriType; ARect: TRect;
      AColorArr, AColorBg: TColor);
    procedure SetTabIndex(AIndex: Integer);
    procedure GetTabCloseColor(AIndex: Integer; const ARect: TRect;
      var AColorBg, AColorBorder: TColor);
    function IsIndexOk(AIndex: Integer): boolean;
    function IsShowX(AIndex: Integer): boolean;
    function IsNeedPaintTab(AIndex: Integer; ACanvas: TCanvas; const ARect: TRect): boolean;
    function DoAfterPaintTab(AIndex: Integer; ACanvas: TCanvas; const ARect: TRect): boolean;
    procedure TabMenuClick(Sender: TObject);
    function GetTabWidth_Plus_Raw: Integer;
    procedure DoUpdateTabWidths;
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
    function GetTabRectWidth(APlusBtn: boolean): Integer;
    function GetTabRect(AIndex: Integer): TRect;
    function GetTabRect_Plus: TRect;
    function GetTabRect_X(const ARect: TRect): TRect;
    procedure GetArrowRect(var RLeft, RRight, RDown: TRect);
    function GetTabAt(X, Y: Integer): Integer;
    function GetTabData(AIndex: Integer): TATTabData;
    function TabCount: Integer;
    property TabIndex: Integer read FTabIndex write SetTabIndex;
    procedure DoAddTab(const ACaption: string;
      AObject: TObject = nil;
      AModified: boolean = false;
      AColor: TColor = clNone);
    procedure DoDeleteTab(AIndex: Integer);
    procedure DoTabMenu;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    {$ifdef windows}
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    {$endif}
  published
    //copied by multi-carets in Synwrite
    //colors
    property ColorBg: TColor read FColorBg write FColorBg;
    property ColorBorderActive: TColor read FColorBorderActive write FColorBorderActive;
    property ColorBorderPassive: TColor read FColorBorderPassive write FColorBorderPassive;
    property ColorTabActive: TColor read FColorTabActive write FColorTabActive;
    property ColorTabPassive: TColor read FColorTabPassive write FColorTabPassive;
    property ColorTabOver: TColor read FColorTabOver write FColorTabOver;
    property ColorCloseBg: TColor read FColorCloseBg write FColorCloseBg;
    property ColorCloseBgOver: TColor read FColorCloseBgOver write FColorCloseBgOver;
    property ColorCloseBorderOver: TColor read FColorCloseBorderOver write FColorCloseBorderOver;
    property ColorCloseX: TColor read FColorCloseX write FColorCloseX;
    property ColorArrow: TColor read FColorArrow write FColorArrow;
    property ColorArrowOver: TColor read FColorArrowOver write FColorArrowOver;
    //spaces
    property TabAngle: Integer read FTabAngle write FTabAngle;
    property TabWidthMin: Integer read FTabWidthMin write FTabWidthMin;
    property TabWidthMax: Integer read FTabWidthMax write FTabWidthMax;
    property TabIndentInter: Integer read FTabIndentInter write FTabIndentInter;
    property TabIndentInit: Integer read FTabIndentInit write FTabIndentInit;
    property TabIndentLeft: Integer read FTabIndentLeft write FTabIndentLeft;
    property TabIndentText: Integer read FTabIndentText write FTabIndentText;
    property TabIndentTop: Integer read FTabIndentTop write FTabIndentTop;
    property TabIndentBottom: Integer read FTabIndentBottom write FTabIndentBottom;
    property TabIndentXRight: Integer read FTabIndentXRight write FTabIndentXRight;
    property TabIndentXInner: Integer read FTabIndentXInner write FTabIndentXInner;
    property TabIndentXSize: Integer read FTabIndentXSize write FTabIndentXSize;
    property TabIndentColor: Integer read FTabIndentColor write FTabIndentColor;
    property TabIndentArrowSize: Integer read FTabIndentArrowSize write FTabIndentArrowSize;
    property TabIndentArrowLeft: Integer read FTabIndentArrowLeft write FTabIndentArrowLeft;
    property TabIndentArrowRight: Integer read FTabIndentArrowRight write FTabIndentArrowRight;

    property TabShowClose: TATTabShowClose read FTabShowClose write FTabShowClose;
    property TabShowPlus: boolean read FTabShowPlus write FTabShowPlus;
    property TabShowPlusText: string read FTabShowPlusText write FTabShowPlusText;
    //property TabShowScroll: boolean read FTabShowScroll write FTabShowScroll; //disabled
    property TabShowMenu: boolean read FTabShowMenu write FTabShowMenu;
    property TabShowBorderActiveLow: boolean read FTabShowBorderActiveLow write FTabShowBorderActiveLow;

    //events
    property OnTabClick: TNotifyEvent read FOnTabClick write FOnTabClick;
    property OnTabPlusClick: TNotifyEvent read FOnTabPlusClick write FOnTabPlusClick;
    property OnTabClose: TATTabCloseEvent read FOnTabClose write FOnTabClose;
    property OnTabMenu: TATTabMenuEvent read FOnTabMenu write FOnTabMenu;
    property OnTabDrawBefore: TATTabDrawEvent read FOnTabDrawBefore write FOnTabDrawBefore;
    property OnTabDrawAfter: TATTabDrawEvent read FOnTabDrawAfter write FOnTabDrawAfter;
  end;

implementation

uses
  SysUtils, Forms, Math;


procedure DrawAntialisedLine(Canvas: TCanvas; const AX1, AY1, AX2, AY2: {real}Integer; const LineColor: TColor);
// http://stackoverflow.com/a/3613953/1789574
var
  swapped: boolean;

  procedure plot(const x, y, c: real);
  var
    resclr: TColor;
  begin
    if swapped then
      resclr := Canvas.Pixels[round(y), round(x)]
    else
      resclr := Canvas.Pixels[round(x), round(y)];
    resclr := RGB(round(GetRValue(resclr) * (1-c) + GetRValue(LineColor) * c),
                  round(GetGValue(resclr) * (1-c) + GetGValue(LineColor) * c),
                  round(GetBValue(resclr) * (1-c) + GetBValue(LineColor) * c));
    if swapped then
      Canvas.Pixels[round(y), round(x)] := resclr
    else
      Canvas.Pixels[round(x), round(y)] := resclr;
  end;

  function rfrac(const x: real): real;
  begin
    rfrac := 1 - frac(x);
  end;

  procedure swap(var a, b: real);
  var
    tmp: real;
  begin
    tmp := a;
    a := b;
    b := tmp;
  end;

var
  x1, x2, y1, y2, dx, dy, gradient, xend, yend, xgap, xpxl1, ypxl1,
  xpxl2, ypxl2, intery: real;
  x: integer;

begin
  //speed up drawing (AT)
  if (AX1 = AX2) or (AY1 = AY2) then
  begin
    Canvas.Pen.Color := LineColor;
    Canvas.MoveTo(AX1, AY1);
    Canvas.LineTo(AX2, AY2);
    Exit
  end;

  x1 := AX1;
  x2 := AX2;
  y1 := AY1;
  y2 := AY2;

  dx := x2 - x1;
  dy := y2 - y1;

  swapped := abs(dx) < abs(dy);
  if swapped then
  begin
    swap(x1, y1);
    swap(x2, y2);
    swap(dx, dy);
  end;
  if x2 < x1 then
  begin
    swap(x1, x2);
    swap(y1, y2);
  end;

  gradient := dy / dx;

  xend := round(x1);
  yend := y1 + gradient * (xend - x1);
  xgap := rfrac(x1 + 0.5);
  xpxl1 := xend;
  ypxl1 := floor(yend);
  plot(xpxl1, ypxl1, rfrac(yend) * xgap);
  plot(xpxl1, ypxl1 + 1, frac(yend) * xgap);
  intery := yend + gradient;

  xend := round(x2);
  yend := y2 + gradient * (xend - x2);
  xgap := frac(x2 + 0.5);
  xpxl2 := xend;
  ypxl2 := floor(yend);
  plot(xpxl2, ypxl2, rfrac(yend) * xgap);
  plot(xpxl2, ypxl2 + 1, frac(yend) * xgap);

  for x := round(xpxl1) + 1 to round(xpxl2) - 1 do
  begin
    plot(x, floor(intery), rfrac(intery));
    plot(x, floor(intery) + 1, frac(intery));
    intery := intery + gradient;
  end;

end;

procedure DrawTriangleRaw(C: TCanvas; const P1, P2, P3: TPoint; Color: TColor);
//optimize later, make antialiased draw
begin
  C.Brush.Color:= Color;
  C.Pen.Color:= Color;
  C.Polygon([P1, P2, P3]);
end;

procedure DrawTriangleType(C: TCanvas; Typ: TATTriType; const R: TRect; Color: TColor);
var
  P1, P2, P3: TPoint;
begin
  //P1/P2: points of vert/horz line
  //P3: end point at arrow direction
  case Typ of
    triDown:
    begin
      P1:= Point(R.Left, R.Top);
      P2:= Point(R.Right, R.Top);
      P3:= Point((R.Left+R.Right) div 2, R.Bottom);
      //DrawTriangleRaw(C, P1, P2, P3, Color);
      //paint little shifted line - w/out antialiasing
      DrawTriangleRaw(C, Point(P1.X, P1.Y), Point(P2.X, P2.Y), Point(P3.X, P3.Y-1), Color);
    end;
    triRight:
    begin
      P1:= Point(R.Left, R.Top);
      P2:= Point(R.Left, R.Bottom);
      P3:= Point(R.Right, (R.Top+R.Bottom) div 2);
      //DrawTriangleRaw(C, P1, P2, P3, Color);
      DrawTriangleRaw(C, Point(P1.X, P1.Y), Point(P2.X, P2.Y), Point(P3.X-1, P3.Y), Color);
    end;
    triLeft:
    begin
      P1:= Point(R.Right, R.Top);
      P2:= Point(R.Right, R.Bottom);
      P3:= Point(R.Left, (R.Top+R.Bottom) div 2);
      //DrawTriangleRaw(C, P1, P2, P3, Color);
      DrawTriangleRaw(C, Point(P1.X, P1.Y), Point(P2.X, P2.Y), Point(P3.X+1, P3.Y), Color);
    end;
  end;

  //paint line with correct coords- antialiased
  DrawAntialisedLine(C, P1.X, P1.Y, P3.X, P3.Y, Color);
  DrawAntialisedLine(C, P2.X, P2.Y, P3.X, P3.Y, Color);
end;

{ TATTabs }

function TATTabs.IsIndexOk(AIndex: Integer): boolean;
begin
  Result:= (AIndex>=0) and (AIndex<FTabList.Count);
end;

function TATTabs.TabCount: Integer;
begin
  Result:= FTabList.Count;
end;

constructor TATTabs.Create(AOnwer: TComponent);
begin
  inherited;

  Caption:= '';
  BorderStyle:= bsNone;
  ControlStyle:= ControlStyle+[csOpaque];

  Width:= 400;
  Height:= 35;

  FColorBg:= clBlack;
  FColorTabActive:= $808080;
  FColorTabPassive:= $786868;
  FColorTabOver:= $A08080;
  FColorBorderActive:= $A0A0A0;
  FColorBorderPassive:= $A07070;
  FColorCloseBg:= clNone;
  FColorCloseBgOver:= $6060E0;
  FColorCloseBorderOver:= FColorCloseBgOver;
  FColorCloseX:= clLtGray;
  FColorArrow:= $999999;
  FColorArrowOver:= $E0E0E0;

  FTabAngle:= 5;
  FTabWidthMin:= 26;
  FTabWidthMax:= 130;
  FTabWidthHideX:= 55;
  FTabIndentLeft:= 8;
  FTabIndentInter:= 0;
  FTabIndentInit:= 4;
  FTabIndentTop:= 5;
  FTabIndentBottom:= 6;
  FTabIndentText:= 6;
  FTabIndentXRight:= 3;
  FTabIndentXInner:= 3;
  FTabIndentXSize:= 12;
  FTabIndentArrowSize:= 4;
  FTabIndentArrowLeft:= 4;
  FTabIndentArrowRight:= 20;
  FTabIndentColor:= 3;
  
  FTabShowClose:= tbShowAll;
  FTabShowPlus:= true;
  FTabShowPlusText:= '+';
  FTabShowScroll:= false; //not supported
  FTabShowMenu:= true;
  FTabShowBorderActiveLow:= false;

  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= 1600;
  FBitmap.Height:= 60;

  FBitmapText:= TBitmap.Create;
  FBitmapText.PixelFormat:= pf24bit;
  FBitmapText.Width:= 600;
  FBitmapText.Height:= 60;

  Font.Name:= 'Tahoma';
  Font.Color:= $E0E0E0;
  Font.Size:= 8;

  FTabIndex:= 0;
  FTabIndexOver:= -1;
  FTabList:= TList.Create;
  FTabMenu:= nil;

  FOnTabClick:= nil;
  FOnTabPlusClick:= nil;
  FOnTabClose:= nil;
  FOnTabMenu:= nil;
  FOnTabDrawBefore:= nil;
  FOnTabDrawAfter:= nil;
end;

destructor TATTabs.Destroy;
var
  i: Integer;
begin
  for i:= TabCount-1 downto 0 do
  begin
    TObject(FTabList[i]).Free;
    FTabList[i]:= nil;
  end;
  FreeAndNil(FTabList);

  FreeAndNil(FBitmapText);
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TATTabs.Paint;
begin
  if Assigned(FBitmap) then
  begin
    DoPaintTo(FBitmap.Canvas);
    Canvas.CopyRect(ClientRect, FBitmap.Canvas, ClientRect);
  end;
end;

procedure TATTabs.DoPaintTabTo(
  C: TCanvas; ARect: TRect; const ACaption: string;
  ATabBg, ATabBorder, ATabBorderLow, ATabHilite, ATabCloseBg, ATabCloseBorder: TColor;
  ACloseBtn: boolean);
var
  PL1, PL2, PR1, PR2: TPoint;
  RText: TRect;
  NIndentL, NIndentR: Integer;
begin
  C.Brush.Color:= FColorBg;
  C.FillRect(ARect);

  C.Pen.Color:= ATabBg;
  C.Brush.Color:= ATabBg;

  NIndentL:= Max(FTabIndentLeft, FTabAngle);
  NIndentR:= NIndentL+IfThen(ACloseBtn, FTabIndentXRight+FTabIndentXSize div 2);
  RText:= Rect(ARect.Left+FTabAngle, ARect.Top, ARect.Right-FTabAngle, ARect.Bottom);
  C.FillRect(RText);
  RText:= Rect(ARect.Left+NIndentL, ARect.Top, ARect.Right-NIndentR, ARect.Bottom);

  //left triangle
  PL1:= Point(ARect.Left+FTabAngle, ARect.Top);
  PL2:= Point(ARect.Left-FTabAngle, ARect.Bottom-1);
  if FTabAngle>0 then
  begin
    //DrawTriangleRaw(C, PL1, PL2, Point(PL1.X, PL2.Y), ATabBg);
    //draw little shifted line- bottom-left point x+=1
    DrawTriangleRaw(C, PL1, Point(PL2.X+1, PL2.Y), Point(PL1.X, PL2.Y), ATabBg);
  end;

  //right triangle
  PR1:= Point(ARect.Right-FTabAngle-1, ARect.Top);
  PR2:= Point(ARect.Right+FTabAngle-1, ARect.Bottom-1);
  if FTabAngle>0 then
  begin
    //DrawTriangleRaw(C, PR1, PR2, Point(PR1.X, PR2.Y), ATabBg);
    //draw little shifted line- bottom-right point x-=1
    DrawTriangleRaw(C, PR1, Point(PR2.X-1, PR2.Y), Point(PR1.X, PR2.Y), ATabBg);
  end;  

  //caption
  FBitmapText.Canvas.Brush.Color:= ATabBg;
  FBitmapText.Canvas.FillRect(Rect(0, 0, FBitmapText.Width, FBitmapText.Height));
  FBitmapText.Canvas.Font.Assign(Self.Font);
  FBitmapText.Canvas.TextOut(
    FTabAngle,
    FTabIndentText,
    ACaption);
  C.CopyRect(
    RText,
    FBitmapText.Canvas,
    Rect(0, 0, RText.Right-RText.Left, RText.Bottom-RText.Top));

  //borders
  DrawAntialisedLine(C, PL1.X, PL1.Y, PL2.X, PL2.Y+1, ATabBorder);
  DrawAntialisedLine(C, PR1.X, PR1.Y, PR2.X, PR2.Y+1, ATabBorder);
  DrawAntialisedLine(C, PL1.X, PL1.Y, PR1.X, PL1.Y, ATabBorder);
  DrawAntialisedLine(C, PL2.X, ARect.Bottom, PR2.X+1, ARect.Bottom, ATabBorderLow);

  //tweak for border corners
  C.Pixels[PL1.X, PL1.Y]:= FColorBg;
  C.Pixels[PR1.X, PR1.Y]:= FColorBg;

  //color mark
  if ATabHilite<>clNone then
  begin
    C.Brush.Color:= ATabHilite;
    C.FillRect(Rect(PL1.X+1, PL1.Y+1, PR1.X, PR1.Y+1+FTabIndentColor));
  end;

  //"close" button
  if ACloseBtn then
  begin
    RText:= GetTabRect_X(ARect);
    C.Brush.Color:= IfThen(ATabCloseBg<>clNone, ATabCloseBg, ATabBg);
    C.FillRect(RText);
    C.Pen.Color:= IfThen(ATabCloseBorder<>clNone, ATabCloseBorder, ATabBg);
    C.Rectangle(RText);
    C.Brush.Color:= ATabBg;

    C.Pen.Color:= FColorCloseX;
    C.Pen.Width:= 2;
    C.MoveTo(RText.Left+FTabIndentXInner, RText.Top+FTabIndentXInner);
    C.LineTo(RText.Right-FTabIndentXInner-1, RText.Bottom-FTabIndentXInner-1);
    C.MoveTo(RText.Left+FTabIndentXInner, RText.Bottom-FTabIndentXInner-1);
    C.LineTo(RText.Right-FTabIndentXInner-1, RText.Top+FTabIndentXInner);
    C.Pen.Width:= 1;
  end;
end;

function TATTabs.GetTabWidth_Plus_Raw: Integer;
begin
  Canvas.Font.Assign(Self.Font);
  Result:= Canvas.TextWidth(FTabShowPlusText);
end;

function TATTabs.GetTabRectWidth(APlusBtn: boolean): Integer;
var
  NWidth: Integer;
begin
  if APlusBtn then
    NWidth:= GetTabWidth_Plus_Raw
  else
  begin
    NWidth:= FTabWidthMax;
  end;

  Result:= NWidth +
    2 * (FTabAngle + FTabIndentLeft);
     //+ IfThen(ACloseBtn, FTabIndentLeft+FTabIndentXRight);
end;


function TATTabs.GetTabRect(AIndex: Integer): TRect;
var
  i: Integer;
begin
  Result.Left:= FTabIndentInit+FTabAngle;
  Result.Right:= Result.Left;
  Result.Top:= FTabIndentTop;
  Result.Bottom:= ClientHeight-FTabIndentBottom;

  if IsIndexOk(AIndex) then
    for i:= 0 to TabCount-1 do
    begin
      Result.Left:= Result.Right + FTabIndentInter;
      Result.Right:= Result.Left + FTabWidth;
      if AIndex=i then Exit;
    end;
end;

function TATTabs.GetTabRect_Plus: TRect;
begin
  Result:= GetTabRect(TabCount-1);
  Result.Left:= Result.Right + FTabIndentInter;
  Result.Right:= Result.Left + GetTabRectWidth(true);
end;

function TATTabs.GetTabRect_X(const ARect: TRect): TRect;
var
  P: TPoint;
begin
  P:= Point(
    ARect.Right-FTabAngle-FTabIndentLeft-FTabIndentXRight,
    (ARect.Top+ARect.Bottom) div 2 + 1);
  Dec(P.X, FTabIndentXSize div 2);
  Dec(P.Y, FTabIndentXSize div 2);
  Result:= Rect(
    P.X,
    P.Y,
    P.X+FTabIndentXSize,
    P.Y+FTabIndentXSize);
end;

procedure TATTabs.GetTabCloseColor(AIndex: Integer; const ARect: TRect;
  var AColorBg, AColorBorder: TColor);
var
  P: TPoint;
begin
  AColorBg:= FColorCloseBg;
  AColorBorder:= FColorCloseBg;
  if IsShowX(AIndex) then
    if AIndex=FTabIndexOver then
    begin
      P:= Mouse.CursorPos;
      P:= ScreenToClient(P);
      if PtInRect(GetTabRect_X(ARect), P) then
      begin
        AColorBg:= FColorCloseBgOver;
        AColorBorder:= FColorCloseBorderOver;
      end;
    end;
end;

function TATTabs.IsNeedPaintTab(AIndex: Integer; ACanvas: TCanvas; const ARect: TRect): boolean;
begin
  Result:= true;
  if Assigned(FOnTabDrawBefore) then
    FOnTabDrawBefore(Self, AIndex, ACanvas, ARect, Result);
end;

function TATTabs.DoAfterPaintTab(AIndex: Integer; ACanvas: TCanvas; const ARect: TRect): boolean;
begin
  Result:= true;
  if Assigned(FOnTabDrawAfter) then
    FOnTabDrawAfter(Self, AIndex, ACanvas, ARect, Result);
end;

procedure TATTabs.DoPaintTo(C: TCanvas);
var
  i: Integer;
  RBottom: TRect;
  AColorXBg, AColorXBorder: TColor;
  ARect: TRect;
  AArrowLeft, AArrowRight, AArrowDown: TRect;
begin
  C.Brush.Color:= FColorBg;
  C.FillRect(ClientRect);

  DoUpdateTabWidths;

  //paint bottom rect
  RBottom:= Rect(0, ClientHeight-FTabIndentBottom, ClientWidth, ClientHeight);
  C.Brush.Color:= FColorTabActive;
  C.FillRect(RBottom);
  DrawAntialisedLine(C, 0, RBottom.Top, ClientWidth, RBottom.Top, FColorBorderActive);

  //paint "plus" tab
  if FTabShowPlus then
  begin
    ARect:= GetTabRect_Plus;
    AColorXBg:= clNone;
    AColorXBorder:= clNone;
    if IsNeedPaintTab(-1, C, ARect) then
    begin
      DoPaintTabTo(C, ARect,
        FTabShowPlusText,
        IfThen(FTabIndexOver=cAtTabPlus, FColorTabOver, FColorTabPassive),
        FColorBorderPassive,
        FColorBorderActive,
        clNone,
        AColorXBg,
        AColorXBorder,
        false
        );
      DoAfterPaintTab(-1, C, ARect);
    end;    
  end;

  //paint passive tabs
  for i:= TabCount-1 downto 0 do
    if i<>FTabIndex then
    begin
      ARect:= GetTabRect(i);
      GetTabCloseColor(i, ARect, AColorXBg, AColorXBorder);
      if IsNeedPaintTab(i, C, ARect) then
      begin
        DoPaintTabTo(C, ARect,
          TATTabData(FTabList[i]).TabCaption,
          IfThen(i=FTabIndexOver, FColorTabOver, FColorTabPassive),
          FColorBorderPassive,
          FColorBorderActive,
          TATTabData(FTabList[i]).TabColor,
          AColorXBg,
          AColorXBorder,
          IsShowX(i)
          );
        DoAfterPaintTab(i, C, ARect);
      end;
    end;

  //paint active tab
  i:= FTabIndex;
  if IsIndexOk(i) then
  begin
    ARect:= GetTabRect(i);
    GetTabCloseColor(i, ARect, AColorXBg, AColorXBorder);
    if IsNeedPaintTab(i, C, ARect) then
    begin
      DoPaintTabTo(C, ARect,
        TATTabData(FTabList[i]).TabCaption,
        FColorTabActive,
        FColorBorderActive,
        IfThen(FTabShowBorderActiveLow, FColorBorderActive, FColorTabActive),
        TATTabData(FTabList[i]).TabColor,
        AColorXBg,
        AColorXBorder,
        IsShowX(i)
        );
      DoAfterPaintTab(i, C, ARect);
    end;  
  end;

  //paint arrows
  GetArrowRect(AArrowLeft, AArrowRight, AArrowDown);

  if FTabShowScroll then
  begin
    DoPaintArrowTo(C, triLeft, AArrowLeft,
      IfThen(FTabIndexOver=cAtArrowLeft, FColorArrowOver, FColorArrow), FColorBg);
    DoPaintArrowTo(C, triRight, AArrowRight,
      IfThen(FTabIndexOver=cAtArrowRight, FColorArrowOver, FColorArrow), FColorBg);
  end;

  if FTabShowMenu then
  begin
    DoPaintArrowTo(C, triDown, AArrowDown,
      IfThen(FTabIndexOver=cAtArrowDown, FColorArrowOver, FColorArrow), FColorBg);
  end;
end;


function TATTabs.GetTabAt(X, Y: Integer): Integer;
var
  i: Integer;
  Pnt: TPoint;
  RLeft, RRight, RDown: TRect;
begin
  Result:= -1;
  Pnt:= Point(X, Y);

  //arrows?
  GetArrowRect(RLeft, RRight, RDown);

  if FTabShowScroll then
    if PtInRect(RLeft, Pnt) then
    begin
      Result:= cAtArrowLeft;
      Exit
    end;

  if FTabShowScroll then
    if PtInRect(RRight, Pnt) then
    begin
      Result:= cAtArrowRight;
      Exit
    end;

  if FTabShowMenu then
    if PtInRect(RDown, Pnt) then
    begin
      Result:= cAtArrowDown;
      Exit
    end;

  //normal tab?
  for i:= 0 to TabCount-1 do
    if PtInRect(GetTabRect(i), Pnt) then
    begin
      Result:= i;
      Exit;
    end;

  //plus tab?
  if FTabShowPlus then
    if PtInRect(GetTabRect_Plus, Pnt) then
    begin
      Result:= cAtTabPlus;
      Exit
    end;  
end;

procedure TATTabs.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRect;
begin
  FTabIndexOver:= GetTabAt(X, Y);

  if Button=mbLeft then
  begin
    case FTabIndexOver of
      cAtArrowLeft,
      cAtArrowRight:
        begin
          Beep;
          Exit
        end;

      cAtArrowDown:
        begin
          DoTabMenu;
          Exit
        end;

      cAtTabPlus:
        begin
          if Assigned(FOnTabPlusClick) then
            FOnTabPlusClick(Self);
          Exit;
        end;

      else
        begin
          if IsShowX(FTabIndexOver) then
          begin
            R:= GetTabRect(FTabIndexOver);
            R:= GetTabRect_X(R);
            if PtInRect(R, Point(X, Y)) then
            begin
              DoDeleteTab(FTabIndexOver);
              Exit
            end;
          end;
          SetTabIndex(FTabIndexOver);
        end;
    end;
  end;
end;

procedure TATTabs.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FTabIndexOver:= GetTabAt(X, Y);
  Invalidate;
end;

procedure TATTabs.Resize;
begin
  inherited;
  if Assigned(FBitmap) then
  begin
    FBitmap.Width:= Max(FBitmap.Width, Width);
    FBitmap.Height:= Max(FBitmap.Height, Height);
  end;
end;


procedure TATTabs.DoAddTab(
  const ACaption: string;
  AObject: TObject = nil;
  AModified: boolean = false;
  AColor: TColor = clNone);
var
  Data: TATTabData;
begin
  Data:= TATTabData.Create;
  Data.TabCaption:= ACaption;
  Data.TabObject:= AObject;
  Data.TabModified:= AModified;
  Data.TabColor:= AColor;
  //Data.TabWidth:= GetTabRectWidth(FTabShowClose, false);

  FTabList.Add(Data);
  Invalidate;
end;

procedure TATTabs.DoDeleteTab(AIndex: Integer);
var
  CanClose: boolean;
begin
  CanClose:= true;
  if Assigned(FOnTabClose) then
    FOnTabClose(Self, AIndex, CanClose);
  if not CanClose then Exit;  

  if IsIndexOk(AIndex) then
  begin
    TObject(FTabList[AIndex]).Free;
    FTabList.Delete(AIndex);

    //need to call OnTabClick
    if FTabIndex>AIndex then
      SetTabIndex(FTabIndex-1)
    else
    if (FTabIndex=AIndex) and (FTabIndex>0) and (FTabIndex>=TabCount) then
      SetTabIndex(FTabIndex-1)
    else
    if FTabIndex=AIndex then
      SetTabIndex(FTabIndex);

    Invalidate;
  end;
end;

procedure TATTabs.SetTabIndex(AIndex: Integer);
begin
  if IsIndexOk(AIndex) then
  begin
    FTabIndex:= AIndex;
    Invalidate;
    if Assigned(FOnTabClick) then
      FOnTabClick(Self);
  end;
end;


function TATTabs.GetTabData(AIndex: Integer): TATTabData;
begin
  if IsIndexOk(AIndex) then
    Result:= TATTabData(FTabList[AIndex])
  else
    Result:= nil;
end;

{$ifdef windows}
//needed to remove flickering on resize and mouse-over
procedure TATTabs.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result:= 1;
end;
{$endif}

(*
procedure TATTabs.DoUpdateTabWidth(AIndex: Integer; ANewWidth: Integer = 0);
var
  Data: TATTabData;
begin
  if IsIndexOk(AIndex) then
  begin
    Data:= TATTabData(FTabList[AIndex]);
    if ANewWidth>0 then
      Data.TabWidth:= ANewWidth
    else
      Data.TabWidth:= GetTabRectWidth(FTabShowClose, false);
    Invalidate;
  end;
end;
*)

procedure TATTabs.DoPaintArrowTo(C: TCanvas; ATyp: TATTriType; ARect: TRect;
  AColorArr, AColorBg: TColor);
const
  cRatio = 0.866; //sqrt(3)/2
var
  P: TPoint;
  R: TRect;
  N, SizeX, SizeY: Integer;
begin
  C.Brush.Color:= AColorBg;
  C.FillRect(ARect);

  N:= FTabIndentArrowSize;
  case ATyp of
    triLeft,
    triRight:
      begin
        SizeY:= N;
        SizeX:= Trunc(N*cRatio);
      end;
    else
      begin
        SizeX:= N;
        SizeY:= Trunc(N*cRatio);
      end;
  end;

  P:= CenterPoint(ARect);
  R:= Rect(P.X-SizeX, P.Y-SizeY, P.X+SizeX, P.Y+SizeY);
  DrawTriangleType(C, ATyp, R, AColorArr);
end;


procedure TATTabs.GetArrowRect(var RLeft, RRight, RDown: TRect);
begin
  RLeft.Top:= FTabIndentTop;
  RLeft.Bottom:= ClientHeight-FTabIndentBottom;
  RRight.Top:= RLeft.Top;
  RRight.Bottom:= RLeft.Bottom;
  RDown.Top:= RLeft.Top;
  RDown.Bottom:= RLeft.Bottom;

  RLeft.Left:= FTabIndentArrowLeft;
  RLeft.Right:= RLeft.Left+FTabIndentArrowSize*4;

  RRight.Left:= RLeft.Right+1;
  RRight.Right:= RRight.Left+FTabIndentArrowSize*4;

  RDown.Right:= ClientWidth;
  RDown.Left:= RDown.Right-FTabIndentArrowRight;
end;

procedure TATTabs.DoTabMenu;
var
  i: Integer;
  mi: TMenuItem;
  R1, R2, RDown: TRect;
  P: TPoint;
  bShow: boolean;
begin
  if TabCount=0 then Exit;

  bShow:= true;
  if Assigned(FOnTabMenu) then
    FOnTabMenu(Self, bShow);
  if not bShow then Exit;

  if not Assigned(FTabMenu) then
    FTabMenu:= TPopupMenu.Create(Self);
  FTabMenu.Items.Clear;

  for i:= 0 to TabCount-1 do
  begin
    mi:= TMenuItem.Create(Self);
    mi.Tag:= i;
    mi.Caption:= TATTabData(FTabList[i]).TabCaption;
    mi.OnClick:= {$ifdef FPC}@{$endif}TabMenuClick;
    mi.RadioItem:= true;
    mi.Checked:= i=FTabIndex;
    FTabMenu.Items.Add(mi);
  end;

  GetArrowRect(R1, R2, RDown);
  P:= Point(RDown.Left, RDown.Bottom);
  P:= ClientToScreen(P);
  FTabMenu.Popup(P.X, P.Y);
end;

procedure TATTabs.TabMenuClick(Sender: TObject);
begin
  SetTabIndex((Sender as TComponent).Tag);
end;

procedure TATTabs.DoUpdateTabWidths;
var
  Value, Count: Integer;
begin
  Count:= TabCount;
  if Count=0 then Exit;

  //tricky formula: calculate auto-width
  Value:= (ClientWidth
    - IfThen(FTabShowPlus, GetTabWidth_Plus_Raw + 2*FTabIndentLeft + 1*FTabAngle)
    - FTabAngle*2
    - FTabIndentInter
    - FTabIndentInit
    - IfThen(FTabShowMenu, FTabIndentArrowRight)) div Count
      - FTabIndentInter;

  if Value<FTabWidthMin then
    Value:= FTabWidthMin
  else
  if Value>FTabWidthMax then
    Value:= FTabWidthMax;

  FTabWidth:= Value;
end;

function TATTabs.IsShowX(AIndex: Integer): boolean;
begin
  case FTabShowClose of
    tbShowNone: Result:= false;
    tbShowAll: Result:= true;
    tbShowActive: Result:= AIndex=FTabIndex;
    else Result:= false;
  end;

  if (AIndex<>FTabIndex) and (FTabWidth<FTabWidthHideX) then
    Result:= false;
end;

end.
