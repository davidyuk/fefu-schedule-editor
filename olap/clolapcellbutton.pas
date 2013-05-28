unit CLOLAPCellButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, math, CLOLAPTypes;

type

  { TOLAPCellButton }

  TOLAPCellButton = class(TComponent)
  const
    Size = 16;
  protected
    FIcon: TPortableNetworkGraphic;
    {FTiedItemId, }FLeft, FTop, FItemId: Integer;
    FButtonKind: TOLAPButtonKind;
    FHover, FVisible: Boolean;
    FCallback: TOLAPButtonCallback;
  public
    property ItemId: Integer read FItemId;
    property ButtonKind: TOLAPButtonKind read FButtonKind;
    property Visible: Boolean read FVisible write FVisible;
    property Top: integer read FTop write FTop;
    property Left: integer read FLeft write FLeft;
    function MouseClick(x, y: integer):boolean;
    function MouseMove(x, y: integer):boolean;
    procedure Draw(ACanvas: TCanvas);
    constructor Create(TheOwner: TComponent; AItemId: integer; ACallback: TOLAPButtonCallback); virtual;
  end;

implementation

{ TOLAPCellButton }

function TOLAPCellButton.MouseClick(x, y: integer):boolean;
begin
  Result := false;
  if not Visible Then exit;
  Result := math.InRange(x, FLeft, FLeft+size) and Math.InRange(y, FTop, FTop+size);
  if Result Then FCallback(Self);
end;

function TOLAPCellButton.MouseMove(x, y: integer): boolean;
begin
  Result := False;
  if not Visible Then exit;
  FHover := math.InRange(x, FLeft, FLeft+size) and Math.InRange(y, FTop, FTop+size);
  Result := FHover;
end;

procedure TOLAPCellButton.Draw(ACanvas: TCanvas);
begin
  if not Visible Then exit;
  with ACanvas do begin
    if FHover Then begin
      Pen.Color := RGBToColor(128, 128, 255);
      Frame(FLeft-1, FTop-1, FLeft+Size, FTop+Size);
    end;
    CopyRect(Rect(FLeft, FTop, FLeft+Size-1, FTop+Size-1), FIcon.Canvas, Rect(0, 0, Size, Size));
  end;
end;

constructor TOLAPCellButton.Create(TheOwner: TComponent; AItemId: integer;
  ACallback: TOLAPButtonCallback);
begin
  inherited Create(TheOwner);
  FItemId := AItemId;
  FCallback := ACallback;
end;

end.

