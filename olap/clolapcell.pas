unit CLOLAPCell;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, math, CLOLAPCellButtons, CLOLAPCellButton, CLOLAPTypes;

type

  TCellItem = record
    id: integer;
    content: string;
  end;

  ArrOfCellItem = Array of TCellItem;

  { TOLAPCell }

  TOLAPCell = class(TComponent)
  private const
    Padding = 3;
    HRMargin = 5;
    StaticButtonsCount = 3; //ShowFull, Add, OpenTable
  private
    FWidth, FHeight, FItemHover: integer;
    FItems: array of TCellItem;
    FItemsTop: array of integer;
    FButtons: array of TOLAPCellButton;
    FFixed, FHover: Boolean;
    FRect: TRect;
    FCallback: TOLAPButtonCallback;
    FPosition: TPoint;
  public
    property Position: TPoint read FPosition;
    property Rect: TRect read FRect;
    property Items: ArrOfCellItem read FItems;
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property Hover: Boolean write FHover;
    function GetText: string;
    procedure AddItem(AID: integer; AContent: string);
    procedure Draw(ACanvas: TCanvas; ARect: TRect; ShowButtonFull: Boolean);
    procedure UpdateSize(ACanvas: TCanvas);
    procedure MouseClick(x, y: integer);
    procedure MouseMove(x, y: integer);
    constructor Create(AOwner: TComponent; AFixed: Boolean; ACallback: TOLAPButtonCallback; APosition: TPoint);
  end;

  TOLAPCells = array of array of TOLAPCell;

implementation


{ TOLAPCell }

procedure TOLAPCell.AddItem(AID: integer; AContent: string);
begin
  setLength(FItems, length(FItems)+1);
  with FItems[High(FItems)] do begin
    id := AID;
    content := AContent;
  end;
  if FFixed Then Exit;
  setLength(FItemsTop, Length(FItems)+1);
  setLength(FButtons, Length(FButtons)+2);
  FButtons[High(FButtons)-1] := TOLAPCellButtonEdit.Create(Self, AID, FCallback);
  FButtons[High(FButtons)] := TOLAPCellButtonRemove.Create(Self, AID, FCallback);
end;

function TOLAPCell.GetText: string;
const item_separator = '<newitem>';
var i: integer;
begin
  Result := '';
  For i:= 0 to High(FItems) do with FItems[i] do begin
    if i <> 0 Then Result += item_separator;
    Result += content;
  end;
end;

procedure TOLAPCell.Draw(ACanvas: TCanvas; ARect: TRect;
  ShowButtonFull: Boolean);
var
  i, j, currentTop, itemTop: integer;
  StringList: TStringList;
  f: boolean;
begin
  UpdateSize(ACanvas);

  FRect := ARect;

  if FFixed Then ACanvas.Brush.Color := clBtnFace
  else ACanvas.Brush.Color := clWhite;
  ACanvas.FillRect(ARect);

  with ARect do begin
    Top += Padding;
    Left += Padding;
    Bottom -= Padding;
    Right -= Padding;
  end;
  currentTop:= ARect.Top;
  StringList:= TStringList.Create;
  For i:= 0 to High(FItems) do with FItems[i] do begin
    if i <> 0 Then begin
      currentTop += Padding*2;
      ACanvas.Pen.Color := $bbbbbb;
      ACanvas.Line(ARect.Left+HRMargin, currentTop-Padding-1, ARect.Right-HRMargin, currentTop-Padding-1);
    end;
    itemTop := currentTop;
    StringList.Text := content;
    if not FFixed Then FItemsTop[i] := currentTop-Padding;
    for j:= 0 to StringList.Count-1 do begin
      ACanvas.TextRect(ARect, ARect.Left, currentTop, StringList.Strings[j]);
      currentTop += ACanvas.TextHeight(StringList.Strings[j]);
    end;
    if not FFixed and FHover and (i = FItemHover) Then begin
      ACanvas.Pen.Color := RGBToColor(201, 223, 242);
      ACanvas.Frame(ARect.Left-Padding+1, itemTop-Padding+1, ARect.Right+Padding-2, currentTop+Padding-2);
    end;
  end;
  StringList.Free;
  if (Length(FItems) <> 0) and Not FFixed Then FItemsTop[High(FItemsTop)] := currentTop;

  //0-ShowFull, 1-Add, 2-OpenTable
  If FFixed Then Exit;
  FButtons[0].Top := FRect.Bottom-18;
  FButtons[0].Left := FRect.Right-18;
  FButtons[0].Visible := ShowButtonFull and ((FHeight > (FRect.Bottom - FRect.Top)) or (FWidth > (FRect.Right - FRect.Left)));
  FButtons[0].Draw(ACanvas);
  If not FHover Then exit;
  FButtons[1].Top := FRect.Top+2;
  FButtons[2].Top := FRect.Top+2;
  f:= not FButtons[0].Visible;
  FButtons[1].Visible := f or ((FRect.Bottom-FButtons[1].Top)>(FButtons[1].Size*2));
  FButtons[2].Visible := (Length(FItems) <> 0) and ((FRect.Bottom-FButtons[2].Top)>(FButtons[2].Size*2));
  if Length(FItems) = 0 Then
    FButtons[1].Left := FRect.Right-18
  else begin
    FButtons[1].Left := FRect.Right-18*3;
    FButtons[2].Left := FRect.Right-18*4;
  end;
  for i:= 0 to High(FItems) do begin
    with FButtons[i*2+StaticButtonsCount] do begin
      Left := FRect.Right-18*(2-0);
      Top := FItemsTop[i]+2;
      Visible := f or ((FRect.Bottom-Top)>(Size*2));
    end;
    with FButtons[i*2+StaticButtonsCount+1] do begin
      Left := FRect.Right-18*(2-1);
      Top := FItemsTop[i]+2;
      Visible := f or ((FRect.Bottom-Top)>(Size*2));
    end;
  end;
  for i:= 1 to High(FButtons) do
    FButtons[i].Draw(ACanvas);
end;

procedure TOLAPCell.UpdateSize(ACanvas: TCanvas);
var
  i, j: integer;
  t: TStringList;
begin
  FWidth := 0;
  FHeight := Padding*2;
  For i:= 0 to High(FItems) do with FItems[i] do begin
    if i <> 0 Then FHeight += 6;
    t:= TStringList.Create;
    t.Text := content;
    for j:= 0 to t.Count-1 do begin
      FHeight += ACanvas.TextHeight(t.Strings[j]);
      FWidth := Max(FWidth, ACanvas.TextWidth(t.Strings[j]));
    end;
    t.Free;
  end;
  FWidth += Padding*2;
end;

procedure TOLAPCell.MouseClick(x, y: integer);
var i: integer;
begin
  for i:= 0 to High(FButtons) do
    if FButtons[i].MouseClick(x, y) Then Break;
end;

procedure TOLAPCell.MouseMove(x, y: integer);
var i: integer;
begin
  if (FRect.Right = 0) or (FRect.Bottom = 0) Then exit;
  if InRange(x, FRect.Left, FRect.Right)
    and InRange(y, FRect.Top, FRect.Bottom) Then begin
    FHover := True;
    for i:= 0 to High(FItemsTop)-1 do
      if InRange(y, FItemsTop[i], FItemsTop[i+1]) Then FItemHover := i;
  end else
    FHover := false;
  for i:= 0 to High(FButtons) do
    FButtons[i].MouseMove(x, y);
end;

constructor TOLAPCell.Create(AOwner: TComponent; AFixed: Boolean;
  ACallback: TOLAPButtonCallback; APosition: TPoint);
begin
  inherited Create(AOwner);
  FPosition := APosition;
  FCallback := ACallback;
  FFixed:=AFixed;
  setLength(FButtons, StaticButtonsCount);
  FButtons[0] := TOLAPCellButtonShowFull.Create(Self, -1, FCallback);
  FButtons[1] := TOLAPCellButtonAdd.Create(Self, -1, FCallback);
  FButtons[2] := TOLAPCellButtonOpenTable.Create(Self, -1, FCallback);
end;

end.
