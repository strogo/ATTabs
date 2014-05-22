unit ATTabs;

interface

uses
  Windows,
  Classes, Types, Graphics,
  Controls, ExtCtrls;

type
  TATTabs = class(TPanel)
  private
    FColorBg: TColor;
    FColorBorderActive: TColor;
    FColorBorderPassive: TColor;
    FColorTabActive: TColor;
    FColorTabPassive: TColor;
    FColorTabOver: TColor;
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
    FTabColorRadius: Integer;
    FTabList: TStringList;
    FTabColors: TList;
    FBitmap: TBitmap;
    FBitmapText: TBitmap;
    procedure DoPaintTo(C: TCanvas);
    procedure DoPaintTabTo(C: TCanvas; ARect: TRect;
      ATabBg, ATabBorder, ATabBorderLow, ATabHilite: TColor;
      const ACaption: string);
    procedure DoSyncColors;
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
    function GetTabWidth(NIndex: Integer; NMinSize, NMaxSize: Integer): Integer;
    function GetTabRect(NIndex: Integer): TRect;
    function GetTabAt(X, Y: Integer): Integer;
    function GetTabCount: Integer;
    procedure DoAddTab(const ACaption: string; AColor: TColor = clNone);
    procedure DoDeleteTab(AIndex: Integer);
    procedure DoDeleteAllTabs;
    procedure DoSetTabColor(AIndex: Integer; AColor: TColor);
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
  FColorTabPassive:= $605050;
  FColorTabOver:= $A08080;
  FColorBorderActive:= $A0A0A0;
  FColorBorderPassive:= $A07070;

  FTabAngle:= 5;
  FTabWidthMin:= 70;
  FTabWidthMax:= 200;
  FTabIndentLeft:= 8;
  FTabIndentInit:= 4;
  FTabIndentTop:= 4;
  FTabIndentBottom:= 6;
  FTabIndentText:= 2;
  FTabColorRadius:= 6;

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
  Font.Size:= 10;

  FTabIndex:= 0;
  FTabIndexOver:= -1;

  FTabList:= TStringList.Create;
  FTabColors:= TList.Create;
end;

destructor TATTabs.Destroy;
begin
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
  ATabBg, ATabBorder, ATabBorderLow, ATabHilite: TColor;
  const ACaption: string);
var
  PL1, PL2, PR1, PR2: TPoint;
  RText: TRect;
  NIndent: Integer;
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

  //color mark
  if ATabHilite<>clNone then
  begin
    PL1:= Point(ARect.Right-FTabAngle-FTabColorRadius, (ARect.Top+ARect.Bottom) div 2);
    C.Brush.Color:= ATabHilite;
    C.Ellipse(
      PL1.X-FTabColorRadius,
      PL1.Y-FTabColorRadius,
      PL1.X+FTabColorRadius,
      PL1.Y+FTabColorRadius);
  end;
end;



function TATTabs.GetTabWidth(NIndex: Integer; NMinSize, NMaxSize: Integer): Integer;
begin
  Canvas.Font.Assign(Self.Font);
  Result:=
    Canvas.TextWidth(FTabList[NIndex]) +
    2*(FTabAngle + FTabIndentLeft);

  if NMaxSize>0 then
    if Result>NMaxSize then
      Result:= NMaxSize;

  if NMinSize>0 then
    if Result<NMinSize then
      Result:= NMinSize;
end;

function TATTabs.GetTabRect(NIndex: Integer): TRect;
var
  NLeft, i: Integer;
begin
  NLeft:= FTabIndentInit+FTabAngle;
  for i:= 0 to FTabList.Count-1 do
  begin
    Result.Left:= NLeft;
    Result.Right:= Result.Left + GetTabWidth(i, FTabWidthMin, FTabWidthMax);
    Result.Top:= FTabIndentTop;
    Result.Bottom:= ClientHeight-FTabIndentBottom;
    NLeft:= Result.Right;
    if NIndex=i then Exit;
  end;
end;

procedure TATTabs.DoPaintTo(C: TCanvas);
var
  i: Integer;
  RBottom: TRect;
begin
  C.Brush.Color:= FColorBg;
  C.FillRect(ClientRect);

  //paint bottom rect
  RBottom:= Rect(0, ClientHeight-FTabIndentBottom, ClientWidth, ClientHeight);
  C.Brush.Color:= FColorTabActive;
  C.FillRect(RBottom);
  DrawAntialisedLine(C, 0, RBottom.Top, ClientWidth, RBottom.Top, FColorBorderActive);

  //paint passive tabs
  for i:= 0 to FTabList.Count-1 do
    if i<>FTabIndex then
      DoPaintTabTo(C, GetTabRect(i),
        IfThen(i=FTabIndexOver,
          FColorTabOver,
          FColorTabPassive),
        FColorBorderPassive,
        FColorBorderActive,
        Integer(FTabColors[i]),
        FTabList[i]);

  //paint active tab
  i:= FTabIndex;
  if (i>=0) and (i<FTabList.Count) then
  DoPaintTabTo(C, GetTabRect(i),
    FColorTabActive,
    FColorBorderActive,
    FColorTabActive,
    Integer(FTabColors[i]),
    FTabList[i]);
end;

function TATTabs.GetTabAt(X, Y: Integer): Integer;
var
  i: Integer;
  Pnt: TPoint;
begin
  Result:= -1;
  Pnt:= Point(X, Y);
  for i:= 0 to FTabList.Count-1 do
    if PtInRect(GetTabRect(i), Pnt) then
    begin
      Result:= i;
      Break;
    end;
end;

procedure TATTabs.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  NOver: Integer;
begin
  NOver:= GetTabAt(X, Y);
  if NOver>=0 then
  begin
    FTabIndex:= NOver;
    Invalidate;
  end;
end;

procedure TATTabs.Resize;
begin
  inherited;
  FBitmap.Width:= Width;
  FBitmap.Height:= Height;
end;

procedure TATTabs.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FTabIndexOver:= GetTabAt(X, Y);
  Invalidate;
end;


procedure TATTabs.DoSyncColors;
begin
  while FTabColors.Count > FTabList.Count do
    FTabColors.Delete(FTabColors.Count-1);
  while FTabColors.Count < FTabList.Count do
    FTabColors.Add(Pointer(clNone));
end;

procedure TATTabs.DoAddTab(const ACaption: string; AColor: TColor = clNone);
begin
  DoSyncColors;
  FTabList.Add(ACaption);
  FTabColors.Add(Pointer(AColor));
  Invalidate;
end;

procedure TATTabs.DoDeleteTab(AIndex: Integer);
begin
  DoSyncColors;
  if (AIndex>=0) and (AIndex<FTabList.Count) then
  begin
    FTabList.Delete(AIndex);
    FTabColors.Delete(AIndex);

    if FTabIndex>AIndex then
      Dec(FTabIndex)
    else
    if (FTabIndex=AIndex) and (FTabIndex>0) and (FTabIndex>=FTabColors.Count) then
      Dec(FTabIndex); 

    Invalidate;
  end;
end;

procedure TATTabs.DoDeleteAllTabs;
begin
  FTabList.Clear;
  FTabColors.Clear;
end;

procedure TATTabs.DoSetTabColor(AIndex: Integer; AColor: TColor);
begin
  DoSyncColors;
  if (AIndex>=0) and (AIndex<FTabList.Count) then
  begin
    FTabColors[AIndex]:= Pointer(AColor);
    Invalidate;
  end;
end;

function TATTabs.GetTabCount: Integer;
begin
  Result:= FTabList.Count;
end;  


end.
