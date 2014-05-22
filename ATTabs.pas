unit ATTabs;

interface

uses
  Windows,
  Classes, Types, Graphics,
  Controls, ExtCtrls;

type
  TATTabData = class
  public
    TabCaption: string;
    TabColor: TColor;
    TabModified: boolean;
    TabObject: TObject;
  end;

type
  TATTabs = class(TPanel)
  private
    FColorBg: TColor;
    FColorBorderActive: TColor;
    FColorBorderPassive: TColor;
    FColorTabActive: TColor;
    FColorTabPassive: TColor;
    FColorTabOver: TColor;
    FColorCloseBg: TColor;
    FColorCloseBgOver: TColor;
    FColorCloseX: TColor;
    FTabAngle: Integer;
    FTabWidthMin: Integer;
    FTabWidthMax: Integer;
    FTabIndentInit: Integer;
    FTabIndentLeft: Integer;
    FTabIndentTop: Integer;
    FTabIndentBottom: Integer;
    FTabIndentText: Integer;
    FTabIndex: Integer;
    FTabIndexOver: Integer;
    FTabColorSize: Integer;
    FTabCloseSize: Integer;
    FTabCloseButtons: boolean;
    FTabItems: TList;
    FBitmap: TBitmap;
    FBitmapText: TBitmap;
    FOnTabClick: TNotifyEvent;
    procedure DoPaintTo(C: TCanvas);
    procedure DoPaintTabTo(C: TCanvas; ARect: TRect;
      const ACaption: string;
      ATabBg, ATabBorder, ATabBorderLow, ATabHilite, ATabCloseBg: TColor);
    procedure SetTabIndex(AIndex: Integer);
    function GetTabCloseColor(AIndex: Integer; const ARect: TRect): TColor;
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
    function GetTabWidth(AIndex: Integer; AMinSize, AMaxSize: Integer): Integer;
    function GetTabRect(AIndex: Integer): TRect;
    function GetTabCloseRect(const ARect: TRect): TRect;
    function GetTabAt(X, Y: Integer): Integer;
    function GetTabData(AIndex: Integer): TATTabData;
    function TabCount: Integer;
    procedure DoAddTab(const ACaption: string; AColor: TColor = clNone);
    procedure DoDeleteTab(AIndex: Integer);
    property TabIndex: Integer read FTabIndex write SetTabIndex;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  published
    property TabAngle: Integer read FTabAngle write FTabAngle;
    property TabIndentTop: Integer read FTabIndentTop write FTabIndentTop;
    property TabIndentBottom: Integer read FTabIndentBottom write FTabIndentBottom;
    property TabIndentLeft: Integer read FTabIndentLeft write FTabIndentLeft;
    property TabIndentText: Integer read FTabIndentText write FTabIndentText;
    property TabIndentInit: Integer read FTabIndentInit write FTabIndentInit;
    property TabCloseButtons: boolean read FTabCloseButtons write FTabCloseButtons;
    property OnTabClick: TNotifyEvent read FOnTabClick write FOnTabClick;
  end;

implementation

uses
  SysUtils, Forms, Math;

{ TATTabs }

constructor TATTabs.Create(AOnwer: TComponent);
begin
  inherited;

  Caption:= '';
  BorderStyle:= bsNone;
  ControlStyle:= ControlStyle+[csOpaque];

  Width:= 400;
  Height:= 36;

  FColorBg:= clBlack;
  FColorTabActive:= $808080;
  FColorTabPassive:= $786868;
  FColorTabOver:= $A08080;
  FColorBorderActive:= $A0A0A0;
  FColorBorderPassive:= $A07070;
  FColorCloseBg:= clNone;
  FColorCloseBgOver:= $6060E0;
  FColorCloseX:= clLtGray;

  FTabAngle:= 5;
  FTabWidthMin:= 70;
  FTabWidthMax:= 200;
  FTabIndentLeft:= 8;
  FTabIndentInit:= 4;
  FTabIndentTop:= 4;
  FTabIndentBottom:= 6;
  FTabIndentText:= 3;
  FTabColorSize:= 3;
  FTabCloseButtons:= true;
  FTabCloseSize:= 5;

  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= Width;
  FBitmap.Height:= Height;

  FBitmapText:= TBitmap.Create;
  FBitmapText.PixelFormat:= pf24bit;
  FBitmapText.Width:= 600;
  FBitmapText.Height:= 100;

  Font.Name:= 'Tahoma';
  Font.Color:= $E0E0E0;
  Font.Size:= 8;

  FTabIndex:= 0;
  FTabIndexOver:= -1;
  FTabItems:= TList.Create;

  FOnTabClick:= nil;
end;

destructor TATTabs.Destroy;
var
  i: Integer;
begin
  for i:= FTabItems.Count-1 downto 0 do
  begin
    TObject(FTabItems[i]).Free;
    FTabItems[i]:= nil;
  end;
  FreeAndNil(FTabItems);

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

procedure DrawAntialisedLine(Canvas: TCanvas; const AX1, AY1, AX2, AY2: real; const LineColor: TColor);
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

procedure TATTabs.DoPaintTabTo(C: TCanvas; ARect: TRect;
  const ACaption: string;
  ATabBg, ATabBorder, ATabBorderLow, ATabHilite, ATabCloseBg: TColor);
var
  PL1, PL2, PR1, PR2: TPoint;
  RText: TRect;
  NIndent: Integer;
const
  cX = 2; //offset from [x] rectangle edge to "x" mark
begin
  C.Brush.Color:= FColorBg;
  C.FillRect(ARect);

  C.Pen.Color:= ATabBg;
  C.Brush.Color:= ATabBg;

  NIndent:= Max(FTabIndentLeft, FTabAngle);
  RText:= Rect(ARect.Left+FTabAngle, ARect.Top, ARect.Right-FTabAngle, ARect.Bottom);
  C.FillRect(RText);
  RText:= Rect(ARect.Left+NIndent, ARect.Top, ARect.Right-NIndent, ARect.Bottom);

  //left triangle
  PL1:= Point(ARect.Left+FTabAngle, ARect.Top);
  PL2:= Point(ARect.Left-FTabAngle, ARect.Bottom-1);
  C.Polygon([PL1, PL2, Point(PL1.X, PL2.Y)]);

  //right triangle
  PR1:= Point(ARect.Right-FTabAngle-1, ARect.Top);
  PR2:= Point(ARect.Right+FTabAngle-1, ARect.Bottom-1);
  C.Polygon([PR1, PR2, Point(PR1.X, PR2.Y)]);

  //caption
  FBitmapText.Canvas.Brush.Color:= ATabBg;
  FBitmapText.Canvas.FillRect(Rect(0, 0, FBitmapText.Width, FBitmapText.Height));
  FBitmapText.Canvas.Font.Assign(Self.Font);
  FBitmapText.Canvas.TextOut(
    FTabAngle,
    ARect.Top + FTabIndentText,
    ACaption);
  C.CopyRect(
    RText,
    FBitmapText.Canvas,
    Rect(0, 0, RText.Right-RText.Left, RText.Bottom-RText.Top));

  //borders
  DrawAntialisedLine(C, PL1.X, PL1.Y, PL2.X, PL2.Y, ATabBorder);
  DrawAntialisedLine(C, PR1.X, PR1.Y, PR2.X, PR2.Y, ATabBorder);
  DrawAntialisedLine(C, PL1.X, PL1.Y, PR1.X, PL1.Y, ATabBorder);
  DrawAntialisedLine(C, PL2.X, ARect.Bottom, PR2.X, ARect.Bottom, ATabBorderLow);

  //tweak for border corners
  C.Pixels[PL1.X, PL1.Y]:= FColorBg;
  C.Pixels[PR1.X, PR1.Y]:= FColorBg;

  //color mark
  if ATabHilite<>clNone then
  begin
    C.Brush.Color:= ATabHilite;
    C.FillRect(Rect(PL1.X+1, PL1.Y+1, PR1.X, PR1.Y+1+FTabColorSize));
  end;

  //"close" button
  if FTabCloseButtons then
  begin
    RText:= GetTabCloseRect(ARect);
    C.Brush.Color:= IfThen(ATabCloseBg<>clNone, ATabCloseBg, ATabBg);
    C.FillRect(RText);

    C.Pen.Color:= FColorCloseX;
    C.Pen.Width:= 2;
    C.MoveTo(RText.Left+cX, RText.Top+cX);
    C.LineTo(RText.Right-cX-1, RText.Bottom-cX-1);
    C.MoveTo(RText.Left+cX, RText.Bottom-cX-1);
    C.LineTo(RText.Right-cX-1, RText.Top+cX);
    C.Pen.Width:= 1;
  end;
end;

function TATTabs.GetTabCloseRect(const ARect: TRect): TRect;
var
  P: TPoint;
begin
  P:= Point(
    ARect.Right-FTabAngle-FTabCloseSize*2,
    (ARect.Top+ARect.Bottom) div 2);
  Result:= Rect(
    P.X-FTabCloseSize,
    P.Y-FTabCloseSize,
    P.X+FTabCloseSize,
    P.Y+FTabCloseSize);
end;

function TATTabs.GetTabWidth(AIndex: Integer; AMinSize, AMaxSize: Integer): Integer;
begin
  Canvas.Font.Assign(Self.Font);
  Result:=
    Canvas.TextWidth(TATTabData(FTabItems[AIndex]).TabCaption) +
    2*(FTabAngle + FTabIndentLeft) +
    IfThen(FTabCloseButtons, FTabCloseSize*3);

  if AMaxSize>0 then
    if Result>AMaxSize then
      Result:= AMaxSize;

  if AMinSize>0 then
    if Result<AMinSize then
      Result:= AMinSize;
end;

function TATTabs.GetTabRect(AIndex: Integer): TRect;
var
  NLeft, i: Integer;
begin
  NLeft:= FTabIndentInit+FTabAngle;
  for i:= 0 to FTabItems.Count-1 do
  begin
    Result.Left:= NLeft;
    Result.Right:= Result.Left + GetTabWidth(i, FTabWidthMin, FTabWidthMax);
    Result.Top:= FTabIndentTop;
    Result.Bottom:= ClientHeight-FTabIndentBottom;
    NLeft:= Result.Right;
    if AIndex=i then Exit;
  end;
end;

function TATTabs.GetTabCloseColor(AIndex: Integer; const ARect: TRect): TColor;
var
  P: TPoint;
begin
  Result:= FColorCloseBg;
  if FTabCloseButtons then
    if AIndex=FTabIndexOver then
    begin
      P:= Mouse.CursorPos;
      P:= ScreenToClient(P);
      if PtInRect(GetTabCloseRect(ARect), P) then
        Result:= FColorCloseBgOver;
    end;
end;

procedure TATTabs.DoPaintTo(C: TCanvas);
var
  i: Integer;
  RBottom: TRect;
  AColorClose: TColor;
  ARect: TRect;
begin
  C.Brush.Color:= FColorBg;
  C.FillRect(ClientRect);

  //paint bottom rect
  RBottom:= Rect(0, ClientHeight-FTabIndentBottom, ClientWidth, ClientHeight);
  C.Brush.Color:= FColorTabActive;
  C.FillRect(RBottom);
  DrawAntialisedLine(C, 0, RBottom.Top, ClientWidth, RBottom.Top, FColorBorderActive);

  //paint passive tabs
  for i:= 0 to FTabItems.Count-1 do
    if i<>FTabIndex then
    begin
      ARect:= GetTabRect(i);
      AColorClose:= GetTabCloseColor(i, ARect);
      DoPaintTabTo(C, ARect, TATTabData(FTabItems[i]).TabCaption,
        IfThen(i=FTabIndexOver,
          FColorTabOver,
          FColorTabPassive),
        FColorBorderPassive,
        FColorBorderActive,
        TATTabData(FTabItems[i]).TabColor,
        AColorClose
        );
    end;

  //paint active tab
  i:= FTabIndex;
  if (i>=0) and (i<FTabItems.Count) then
  begin
    ARect:= GetTabRect(i);
    AColorClose:= GetTabCloseColor(i, ARect);
    DoPaintTabTo(C, ARect, TATTabData(FTabItems[i]).TabCaption,
      FColorTabActive,
      FColorBorderActive,
      FColorTabActive,
      TATTabData(FTabItems[i]).TabColor,
      AColorClose);
  end;
end;

function TATTabs.GetTabAt(X, Y: Integer): Integer;
var
  i: Integer;
  Pnt: TPoint;
begin
  Result:= -1;
  Pnt:= Point(X, Y);
  for i:= 0 to FTabItems.Count-1 do
    if PtInRect(GetTabRect(i), Pnt) then
    begin
      Result:= i;
      Break;
    end;
end;

procedure TATTabs.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRect;
begin
  FTabIndexOver:= GetTabAt(X, Y);
  if FTabIndexOver>=0 then
  begin
    if FTabCloseButtons then
    begin
      R:= GetTabRect(FTabIndexOver);
      R:= GetTabCloseRect(R);
      if PtInRect(R, Point(X, Y)) then
      begin
        DoDeleteTab(FTabIndexOver);
        Exit
      end;
    end;

    SetTabIndex(FTabIndexOver);
    Invalidate;
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
  FBitmap.Width:= Width;
  FBitmap.Height:= Height;
end;


procedure TATTabs.DoAddTab(const ACaption: string; AColor: TColor = clNone);
var
  Data: TATTabData;
begin
  Data:= TATTabData.Create;
  Data.TabCaption:= ACaption;
  Data.TabColor:= AColor;
  FTabItems.Add(Data);

  Invalidate;
end;

procedure TATTabs.DoDeleteTab(AIndex: Integer);
begin
  if (AIndex>=0) and (AIndex<FTabItems.Count) then
  begin
    TObject(FTabItems[AIndex]).Free;
    FTabItems.Delete(AIndex);

    if FTabIndex>AIndex then
      Dec(FTabIndex)
    else
    if (FTabIndex=AIndex) and (FTabIndex>0) and (FTabIndex>=FTabItems.Count) then
      Dec(FTabIndex); 

    Invalidate;
  end;
end;

function TATTabs.TabCount: Integer;
begin
  Result:= FTabItems.Count;
end;

procedure TATTabs.SetTabIndex(AIndex: Integer);
begin
  if (AIndex>=0) and (AIndex<FTabItems.Count) then
  begin
    FTabIndex:= AIndex;
    if Assigned(FOnTabClick) then
      FOnTabClick(Self);
    Invalidate;
  end;
end;


function TATTabs.GetTabData(AIndex: Integer): TATTabData;
begin
  if (AIndex>=0) and (AIndex<FTabItems.Count) then
    Result:= TATTabData(FTabItems[AIndex])
  else
    Result:= nil;
end;

end.
