// govnocode detected
unit UnitMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, FMX.Colors, FMX.Controls.Presentation, FMX.Effects,
  FMX.Filter.Effects;

type
  TNetPoint = record
    X, Y: Double;
    Dx, Dy: Double;
  end;

  TFormMain = class(TForm)
    TimerAnimate: TTimer;
    PaintBox: TPaintBox;
    Panel: TPanel;
    HueTrackBarLinesColor: THueTrackBar;
    HueTrackBarPointsColor: THueTrackBar;
    TrackBarStartFadeDistance: TTrackBar;
    Label2: TLabel;
    Label3: TLabel;
    StyleBook: TStyleBook;
    LabelLineCount: TLabel;
    Label4: TLabel;
    procedure TimerAnimateTimer(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  private const
    // размер точки
    PointSize = 5;
    // расстояние начала фейда
    DefaultFadeDistance = 200;
    // количество точек
    DefaultPointCount = 45;
  private
    Points: array of TNetPoint;
    function PointColor: TAlphaColor;
    function LineColor: TAlphaColor;
    function FadeDistance: Double;
    function MakeNetPoint(const X, Y: Double): TNetPoint;
  protected
    procedure ApplyStyleLookup; override;
  public
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.UIConsts;

{$R *.fmx}

function TFormMain.MakeNetPoint(const X, Y: Double): TNetPoint;
var
  Angle: Double;
begin
  // координаты
  Result.X := X;
  Result.Y := Y;

  // скорость по горизонтали/вертикали
  Angle := Random * 2 * Pi;
  Result.Dx := Cos(Angle) * 4;
  Result.Dy := Sin(Angle) * 4;
end;

procedure TFormMain.ApplyStyleLookup;
begin
  inherited;
  Panel.Height := HueTrackBarPointsColor.BoundsRect.Bottom +
    HueTrackBarPointsColor.Margins.Bottom;
end;

function TFormMain.FadeDistance: Double;
begin
  Result := TrackBarStartFadeDistance.Value
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  ApplyStyleLookup;
  TrackBarStartFadeDistance.Value := DefaultFadeDistance;
  HueTrackBarLinesColor.Value := 0.79;
  HueTrackBarPointsColor.Value := 0.6;

  // генерируем точки
  SetLength(Points, DefaultPointCount);
  for I := 0 to High(Points) do
  begin
    Points[I] := MakeNetPoint(
      Random * PaintBox.LocalRect.Width,
      Random * PaintBox.LocalRect.Height
    );
  end;
end;

function TFormMain.LineColor: TAlphaColor;
begin
  Result := HSLtoRGB(HueTrackBarLinesColor.Value, 0.9, 0.7);
end;

procedure TFormMain.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if (Length(Points) < 100) and (Button = TMouseButton.mbLeft) then
  begin
    SetLength(Points, Length(Points) + 1);
    Points[High(Points)] := MakeNetPoint(X, Y);
  end
  else if (Length(Points) > 10) and (Button = TMouseButton.mbRight) then
    SetLength(Points, Length(Points) - 1);
end;

procedure TFormMain.PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
var
  Point: TNetPoint;
  I, J: Integer;
  Point1, Point2: TPointF;
  Fade: Double;
  LineCount: Integer;
begin
  // рисуем линии
  Canvas.Stroke.Color := LineColor;
  Canvas.Stroke.Kind := TBrushKind.Solid;
  LineCount := 0;
  for I := 0 to High(Points) do
  begin
    for J := I + 1 to High(Points) do
    begin
      Point1 := TPointF.Create(Points[I].X, Points[I].Y);
      Point2 := TPointF.Create(Points[J].X, Points[J].Y);

      // вычисляем расстояние между линиями в -00 -> 0..1
      Fade := -(Point1.Distance(Point2) - FadeDistance) / FadeDistance;

      // если фейд положителен рисуем
      if Fade > 0 then
      begin
        // толщина
        Canvas.Stroke.Thickness := Fade * 5 + 1;
        // draw ...
        Canvas.DrawLine(
          Point1,
          Point2,
          Fade
        );
        Inc(LineCount);
      end;
    end;
  end;

  // рисуем точки поверх линий
  Canvas.Fill.Color := PointColor;
  Canvas.Fill.Kind := TBrushKind.Solid;
  for Point in Points do
  begin
    Canvas.FillEllipse(
      TRectF.Create(
        Point.X - PointSize,
        Point.Y - PointSize,
        Point.X + PointSize,
        Point.Y + PointSize
      ),
      1.0
    );
  end;

  LabelLineCount.Text := 'Line count: ' + LineCount.ToString;
end;

function TFormMain.PointColor: TAlphaColor;
begin
  Result := HSLtoRGB(HueTrackBarPointsColor.Value, 0.9, 0.7);
end;

procedure TFormMain.TimerAnimateTimer(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to High(Points) do
  begin
    // перемещаем точку в соответсвии со скоростью
    Points[I].X := Points[I].X + Points[I].Dx;
    Points[I].Y := Points[I].Y + Points[I].Dy;

    // обрабатываем вылет за экран
    if Points[I].X < 0 then
      Points[I].Dx := Abs(Points[I].Dx);
    if Points[I].X > PaintBox.LocalRect.Width then
      Points[I].Dx := -Abs(Points[I].Dx);
    if Points[I].Y < 0 then
      Points[I].Dy := Abs(Points[I].Dy);
    if Points[I].Y > PaintBox.LocalRect.Height then
      Points[I].Dy := -Abs(Points[I].Dy);
  end;

  PaintBox.Repaint;
end;

end.
