unit CLFormScheduleCell;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, math;

type

  TCellItem = record
    id: integer;
    content: array of string;
  end;

  { TDrawGridCellButton }

  TDrawGridCellButton = class(TComponent)
  private const
    Size = 15;
  protected
    FTag: integer;
    FX, FY: Integer;
    FIsHover, FVisible: Boolean;
    procedure Click; virtual; abstract;
  public
    property Visible: Boolean read FVisible write FVisible;
    property Tag: integer read FTag write FTag;
    property Top: integer read FY write FY;
    property Left: integer read FX write FX;
    procedure MouseClick(x, y: integer);
    procedure MouseMove(x, y: integer);
    procedure Draw(ACanvas: TCanvas); virtual; abstract;
  end;

  { TDrawGridCellButtonFull }

  TDrawGridCellButtonFull = class(TDrawGridCellButton)
  private
    procedure Click; override;
  public
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TDrawGridCell = class;

  TFormSheduleProcedure = procedure (Cell: TDrawGridCell) of object;

  { TDrawGridCell }

  TDrawGridCell = class(TComponent)
  private
    FRect: TRect;
    FTextWidth, FTextHeight: integer;
    FItems: array of TCellItem;
    FButtons: array of TDrawGridCellButton;
    FFixed: Boolean;
    FShowFullCell: procedure(content: TDrawGridCell) of object;
    function GetItem(AIndex: integer): TCellItem;
  public
    property Item[AIndex: integer]: TCellItem read GetItem;
    property Fixed:boolean read FFixed write FFixed;
    property TextWidth: integer read FTextWidth;
    property TextHeight: integer read FTextHeight;
    property Rect: TRect read FRect;
    procedure AddItem(AID: integer; AStrArr: array of string);
    procedure Draw(ACanvas: TCanvas; ARect: TRect; ShowButtons: Boolean);
    procedure MouseClick(x, y: integer);
    procedure MouseMove(x, y: integer);
    procedure CreateButtons;
    procedure UpdateButtons;
    constructor Create(TheOwner: TComponent; AShowFullCell: TFormSheduleProcedure);
  end;

implementation

{ TDrawGridCellButtonFull }

procedure TDrawGridCellButtonFull.Click;
begin
  TDrawGridCell(Owner).FShowFullCell(TDrawGridCell(Owner));
end;

procedure TDrawGridCellButtonFull.Draw(ACanvas: TCanvas);
begin
  //inherited Draw(ACanvas);
  if not Visible Then exit;
  with ACanvas do begin
    if FIsHover Then begin
      Pen.Color := $bbbbbb;
      Frame(FX+1, FY+1, FX+Size, FY+Size);
    end;
    Pen.Color := clBlack;
    MoveTo(FX+2, FY+2);
    LineTo(FX+Size-2, FY+2);
    LineTo(FX+Size-2, FY+Size-2);
    LineTo(FX+2, FY+Size-2);
    LineTo(FX+2, FY+2);
    Line(FX+4, FY+4, FX+12, FY+4);
    Line(FX+4, FY+7, FX+12, FY+7);
    Line(FX+4, FY+10, FX+12, FY+10);
  end;
end;

{ TDrawGridCellButton }

procedure TDrawGridCellButton.MouseClick(x, y: integer);
begin
  if not Visible Then exit;
  if (x>FX) and (x<FX+size) and (y>FY) and (y<FY+size) Then Click;
end;

procedure TDrawGridCellButton.MouseMove(x, y: integer);
begin
  if not Visible Then exit;
  FIsHover := (x>FX) and (x<FX+size) and (y>FY) and (y<FY+size);
end;

{ TDrawGridCell }

procedure TDrawGridCell.AddItem(AID: integer; AStrArr: array of string);
var i: integer;
begin
  setLength(FItems, length(FItems)+1);
  with FItems[High(FItems)] do begin
    id := AID;
    SetLength(content, Length(AStrArr));
    for i:= 0 to High(AStrArr) do
      content[i] := AStrArr[i];
  end;
end;

function TDrawGridCell.GetItem(AIndex: integer): TCellItem;
begin
  result:= FItems[AIndex];
end;

procedure TDrawGridCell.Draw(ACanvas: TCanvas; ARect: TRect;
  ShowButtons: Boolean);
var i, j, top, width: integer;
begin
  FRect := ARect;
  if FFixed Then ACanvas.Brush.Color := $eeeeee
  else ACanvas.Brush.Color := clWhite;
  ACanvas.FillRect(ARect);
  width := 0;
  top:= ARect.Top;
  For i:= 0 to High(FItems) do with FItems[i] do begin
    for j:= 0 to High(content) do begin
      ACanvas.TextRect(ARect, ARect.Left, top, content[j]);
      top += ACanvas.TextHeight(content[j]);
      width := Max(width, ACanvas.TextWidth(content[j]));
    end;
    if i <> High(FItems) Then begin
      top += 6;
      ACanvas.Pen.Color := clGray;
      ACanvas.Line(ARect.Left+5, top-3, ARect.Right-5, top-3);
    end;
  end;
  FTextWidth := width;
  FTextHeight := top-ARect.Top;
  if not ShowButtons Then exit;
  if Length(FButtons) <> 0 Then
    FButtons[0].Visible := (FTextHeight > (ARect.Bottom - ARect.Top)) or (FTextWidth > (ARect.Right - ARect.Left));
  UpdateButtons;
  for i:= 0 to High(FButtons) do
    FButtons[i].Draw(ACanvas);
end;

procedure TDrawGridCell.MouseClick(x, y: integer);
var i: integer;
begin
  for i:= 0 to High(FButtons) do
    FButtons[i].MouseClick(x, y);
end;

procedure TDrawGridCell.MouseMove(x, y: integer);
var i: integer;
begin
  for i:= 0 to High(FButtons) do
    FButtons[i].MouseMove(x, y);
end;

procedure TDrawGridCell.CreateButtons;
begin
  If Length(FButtons) = 1 Then exit;
  setLength(FButtons, 1);
  FButtons[0] := TDrawGridCellButtonFull.Create(Self);
end;

procedure TDrawGridCell.UpdateButtons;
begin
  If Length(FButtons) = 0 Then exit;
  FButtons[0].Top := FRect.Bottom-18;
  FButtons[0].Left := FRect.Right-18;
end;

constructor TDrawGridCell.Create(TheOwner: TComponent;
  AShowFullCell: TFormSheduleProcedure);
begin
  inherited Create(TheOwner);
  FShowFullCell := AShowFullCell;
end;

end.

