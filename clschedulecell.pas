unit CLScheduleCell;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, math;

type

  TCellItem = record
    id: integer;
    content: array of string;
  end;

  ArrOfCellItem = Array of TCellItem;

  { TDrawGridCellButton }

  TDrawGridCellButton = class(TComponent)
  const
    Size = 16;
  protected
    FIcon: TPortableNetworkGraphic;
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
    procedure Draw(ACanvas: TCanvas);
    constructor Create(TheOwner: TComponent); override;
  end;

  { TDrawGridCellButtonFull }

  TDrawGridCellButtonFull = class(TDrawGridCellButton)
  private
    procedure Click; override;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TDrawGridCellButtonAdd }

  TDrawGridCellButtonAdd = class(TDrawGridCellButton)
  private
    procedure Click; override;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TDrawGridCellButtonRemove }

  TDrawGridCellButtonRemove = class(TDrawGridCellButton)
  private
    procedure Click; override;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TDrawGridCellButtonEdit }

  TDrawGridCellButtonEdit = class(TDrawGridCellButton)
  private
    procedure Click; override;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  TDrawGridCell = class;

  TShowFullCell = procedure (Cell: TDrawGridCell) of object;
  TEditCell = procedure (Cell: TDrawGridCell; Param: Integer) of object;

  { TDrawGridCell }

  TDrawGridCell = class(TComponent)
  private const
    Padding = 3;
  private
    FRect: TRect;
    FTextWidth, FTextHeight: integer;
    FItems: array of TCellItem;
    FButtons: array of TDrawGridCellButton;
    FFixed: Boolean;
    FShowFullCell: TShowFullCell;
    FEditCell: TEditCell;
    FPositionValue: TPoint;
    function GetText: string;
    procedure SetFixed(AFixed: boolean);
  public
    property PositionValue: TPoint read FPositionValue write FPositionValue;
    property Items: ArrOfCellItem read FItems write FItems;
    property Fixed: boolean read FFixed write SetFixed;
    property Text: string read GetText; //for HTML export
    property TextWidth: integer read FTextWidth;
    property TextHeight: integer read FTextHeight;
    property Rect: TRect read FRect;
    procedure AddItem(AID: integer; AStrArr: array of string);
    procedure Draw(ACanvas: TCanvas; ARect: TRect; ShowButtonFull: Boolean);
    procedure CalculateTextSize(ACanvas: TCanvas);
    procedure MouseClick(x, y: integer);
    procedure MouseMove(x, y: integer);
    constructor Create(TheOwner: TComponent; AShowFullCell: TShowFullCell; AEditCell: TEditCell);
  end;

  ArrOfArrOfDrawGridCell = array of array of TDrawGridCell;

implementation

var IconEdit, IconAdd, IconRemove, IconFull: TPortableNetworkGraphic;

{ TDrawGridCellButtonAdd }

procedure TDrawGridCellButtonAdd.Click;
begin
  TDrawGridCell(Owner).FEditCell(TDrawGridCell(Owner), 0);
end;

constructor TDrawGridCellButtonAdd.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIcon := IconAdd;
end;

{ TDrawGridCellButtonRemove }

procedure TDrawGridCellButtonRemove.Click;
begin
  TDrawGridCell(Owner).FEditCell(TDrawGridCell(Owner), -Tag);
end;

constructor TDrawGridCellButtonRemove.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIcon := IconRemove;
end;

{ TDrawGridCellButtonEdit }

procedure TDrawGridCellButtonEdit.Click;
begin
  TDrawGridCell(Owner).FEditCell(TDrawGridCell(Owner), Tag);
end;

constructor TDrawGridCellButtonEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIcon := IconEdit;
end;

{ TDrawGridCellButtonFull }

procedure TDrawGridCellButtonFull.Click;
begin
  TDrawGridCell(Owner).FShowFullCell(TDrawGridCell(Owner));
end;

constructor TDrawGridCellButtonFull.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIcon := IconFull;
end;

{ TDrawGridCellButton }

procedure TDrawGridCellButton.MouseClick(x, y: integer);
begin
  if not Visible Then exit;
  if math.InRange(x, FX, FX+size) and Math.InRange(y, FY, FY+size) Then Click;
end;

procedure TDrawGridCellButton.MouseMove(x, y: integer);
begin
  if not Visible Then exit;
  FIsHover := math.InRange(x, FX, FX+size) and Math.InRange(y, FY, FY+size);
end;

procedure TDrawGridCellButton.Draw(ACanvas: TCanvas);
begin
  if not Visible Then exit;
  with ACanvas do begin
    if FIsHover Then begin
      Pen.Color := RGBToColor(128, 128, 255);
      Frame(FX-1, FY-1, FX+Size, FY+Size);
    end;
    CopyRect(Rect(FX, FY, FX+Size-1, FY+Size-1), FIcon.Canvas, Rect(0, 0, 16, 16));
  end;
end;

constructor TDrawGridCellButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FVisible := True;
end;

{ TDrawGridCell }

procedure TDrawGridCell.AddItem(AID: integer; AStrArr: array of string);
var i: integer;
begin
  setLength(FItems, length(FItems)+1);
  with FItems[High(FItems)] do begin
    id := AID;
    { TODO : Лучше надо копировать массив }
    SetLength(content, Length(AStrArr));
    for i:= 0 to High(AStrArr) do
      content[i] := AStrArr[i];
  end;
end;

function TDrawGridCell.GetText: string;
const item_separator = '<newitem>';
var i, j: integer;
begin
  Result := '';
  For i:= 0 to High(FItems) do with FItems[i] do begin
    if i <> 0 Then Result += item_separator;
    for j:= 0 to High(content) do begin
      if j <> 0 Then Result += #13#10;
      Result += content[j];
    end;
  end;
end;

procedure TDrawGridCell.SetFixed(AFixed: boolean);
var i: integer;
begin
  FFixed:=AFixed;
  if FFixed Then exit;
  for i:= 0 to High(FButtons) do
    FButtons[i].Free;
  setLength(FButtons, Length(FItems)*2+2);
  { TODO: i := High(FItems)*2+2; равно нулю О_О }
  FButtons[0] := TDrawGridCellButtonFull.Create(Self);
  FButtons[1] := TDrawGridCellButtonAdd.Create(Self);
  for i:= 0 to High(FItems) do begin
    FButtons[i*2+2] := TDrawGridCellButtonEdit.Create(Self);
    FButtons[i*2+2].Tag := i+1;
    FButtons[i*2+3] := TDrawGridCellButtonRemove.Create(Self);
    FButtons[i*2+3].Tag := i+1;
  end;
end;

procedure TDrawGridCell.Draw(ACanvas: TCanvas; ARect: TRect;
  ShowButtonFull: Boolean);
var
  i, j, top: integer;
  topOfItem: array of integer;
begin
  FRect := ARect;
  if FFixed Then ACanvas.Brush.Color := $eeeeee
  else ACanvas.Brush.Color := clWhite;
  ACanvas.FillRect(FRect);
  FTextWidth := 0;
  SetLength(topOfItem, Length(FItems));
  with ARect do begin
    Top += Padding;
    Left += Padding;
    Bottom -= Padding;
    Right -= Padding;
  end;
  top:= ARect.Top;
  For i:= 0 to High(FItems) do with FItems[i] do begin
    if i <> 0 Then begin
      top += 6;
      ACanvas.Pen.Color := clGray;
      ACanvas.Line(ARect.Left+5, top-3, ARect.Right-5, top-3);
    end;
    for j:= 0 to High(content) do begin
      if j = 0 Then topOfItem[i] := top;
      ACanvas.TextRect(ARect, ARect.Left, top, content[j]);
      top += ACanvas.TextHeight(content[j]);
      FTextWidth := Max(FTextWidth, ACanvas.TextWidth(content[j])+2*Padding);
    end;
  end;
  FTextHeight := top-ARect.Top+Padding*2;

  If FFixed or (Length(FButtons)=0) Then exit;
  FButtons[0].Visible := ShowButtonFull and ((FTextHeight > (FRect.Bottom - FRect.Top)) or (FTextWidth > (FRect.Right - FRect.Left)));
  FButtons[0].Top := ARect.Bottom-18;
  FButtons[0].Left := FRect.Right-18;
  FButtons[1].Top := ARect.Top+2;
  if Length(FItems) = 0 Then FButtons[1].Left := FRect.Right-18
  else FButtons[1].Left := FRect.Right-18*3;
  FButtons[1].Visible := not FButtons[0].Visible or ((FRect.Bottom-FButtons[1].Top)>(FButtons[1].Size*2));
  for i:= 0 to High(FItems) do begin
    FButtons[i*2+2].Left := FRect.Right-18*2;
    FButtons[i*2+2].Top := topOfItem[i]+2;
    FButtons[i*2+2].Visible := not FButtons[0].Visible or ((FRect.Bottom-FButtons[i*2+2].Top)>(FButtons[i*2+2].Size*2));
    FButtons[i*2+3].Left := FRect.Right-18;
    FButtons[i*2+3].Top := topOfItem[i]+2;
    FButtons[i*2+3].Visible := not FButtons[0].Visible or ((FRect.Bottom-FButtons[i*2+3].Top)>(FButtons[i*2+3].Size*2));
  end;
  for i:= 0 to High(FButtons) do
    FButtons[i].Draw(ACanvas);
end;

procedure TDrawGridCell.CalculateTextSize(ACanvas: TCanvas);
var
  i, j: integer;
begin
  FTextWidth := 0;
  FTextHeight := Padding*2;
  For i:= 0 to High(FItems) do with FItems[i] do begin
    if i <> 0 Then FTextHeight += 6;
    for j:= 0 to High(content) do begin
      FTextHeight += ACanvas.TextHeight(content[j]);
      FTextWidth := Max(FTextWidth, ACanvas.TextWidth(content[j])+Padding*2);
    end;
  end;
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

constructor TDrawGridCell.Create(TheOwner: TComponent;
  AShowFullCell: TShowFullCell; AEditCell: TEditCell);
begin
  inherited Create(TheOwner);
  FShowFullCell := AShowFullCell;
  FEditCell := AEditCell;
end;

initialization

  IconAdd := TPortableNetworkGraphic.Create;
  IconEdit := TPortableNetworkGraphic.Create;
  IconFull := TPortableNetworkGraphic.Create;
  IconRemove := TPortableNetworkGraphic.Create;
  IconAdd.LoadFromFile('images\add.png');
  IconEdit.LoadFromFile('images\edit.png');
  IconFull.LoadFromFile('images\full.png');
  IconRemove.LoadFromFile('images\remove.png');

finalization

  IconAdd.Free;
  IconEdit.Free;
  IconFull.Free;
  IconRemove.Free;

end.

