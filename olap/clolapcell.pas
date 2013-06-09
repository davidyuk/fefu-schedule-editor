unit CLOLAPCell;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, math, CLOLAPCellButtons, CLOLAPCellButton, CLOLAPTypes;

type

  TCellItem = record
    id: integer;
    content: string;
    conflictids: array of integer;
  end;

  ArrOfCellItem = Array of TCellItem;

  { TOLAPCell }

  TOLAPCell = class(TComponent)
  private const
    Padding = 3;
    HRMargin = 5;
    StaticButtonsCount = 3; //ShowFull, Add, OpenTable
  private
    FWidth, FHeight, FItemHover, FItemHoverFixed: integer;
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
    property ItemHover: Integer read FItemHover;
    property Items: ArrOfCellItem read FItems;
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property Hover: Boolean write FHover;
    function GetText: string;
    function FixItemHovered:integer;
    procedure UnFixItemHovered;
    procedure AddItem(AID: integer; AContent: string; AConflictIds: array of integer);
    procedure Draw(ACanvas: TCanvas; ARect: TRect; ShowButtonFull: Boolean);
    procedure UpdateSize(ACanvas: TCanvas);
    procedure MouseClick(x, y: integer);
    function MouseMove(x, y: integer):boolean;
    constructor Create(AOwner: TComponent; AFixed: Boolean; ACallback: TOLAPButtonCallback; APosition: TPoint); virtual;
  end;

  TOLAPCells = array of array of TOLAPCell;

implementation


{ TOLAPCell }

procedure TOLAPCell.AddItem(AID: integer; AContent: string;
  AConflictIds: array of integer);
var i: integer;
begin
  setLength(FItems, length(FItems)+1);
  with FItems[High(FItems)] do begin
    id := AID;
    content := AContent;
    SetLength(conflictids, Length(AConflictIds));
    for i:= 0 to High(AConflictIds) do
      conflictids[i] := AConflictIds[i];
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

function TOLAPCell.FixItemHovered: integer;
begin
  FItemHoverFixed := FItemHover;
  Result := FItems[FItemHover].id;
end;

procedure TOLAPCell.UnFixItemHovered;
begin
  FItemHoverFixed := -1;
end;

procedure TOLAPCell.Draw(ACanvas: TCanvas; ARect: TRect;
  ShowButtonFull: Boolean);
var
  i, j, currentTop: integer;
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
    StringList.Text := content;
    if length(conflictids) <> 0 Then begin
      ACanvas.Brush.Color := RGBToColor(255, 200, 200);
      ACanvas.Brush.Style := bsBDiagonal;
      ACanvas.FillRect(FRect.Left, FRect.Top + FItemsTop[i], FRect.Right, FRect.Top + FItemsTop[i+1]);
      ACanvas.Brush.Style := bsSolid;
    end;
    for j:= 0 to StringList.Count-1 do begin
      ACanvas.TextRect(ARect, ARect.Left, currentTop, StringList.Strings[j]);
      currentTop += ACanvas.TextHeight(StringList.Strings[j]);
    end;
    if not FFixed and ((FHover and (i = FItemHover)) or (i = FItemHoverFixed)) Then begin
      if i = FItemHoverFixed Then ACanvas.Pen.Color := RGBToColor(242, 223, 201)
      else ACanvas.Pen.Color := RGBToColor(201, 223, 242);
      ACanvas.Line(FRect.Left+1, FRect.Top + FItemsTop[i]+1, FRect.Left+1, FRect.Top + FItemsTop[i+1]-3); //left
      ACanvas.Line(FRect.Right-3, FRect.Top + FItemsTop[i]+1, FRect.Right-3, FRect.Top + FItemsTop[i+1]-3); //right
      ACanvas.Line(FRect.Left+1, FRect.Top + FItemsTop[i]+1, FRect.Right-3, FRect.Top + FItemsTop[i]+1); //top
      ACanvas.Line(FRect.Left+1, FRect.Top + FItemsTop[i+1]-3, FRect.Right-3, FRect.Top + FItemsTop[i+1]-3); //bottom
      //ACanvas.Frame(ARect.Left-Padding+1, itemTop-Padding+1, ARect.Right+Padding-2, currentTop+Padding-2);
    end;
  end;
  StringList.Free;

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
      Top := FRect.Top + FItemsTop[i] + 2;
      Visible := f or ((FRect.Bottom-Top)>(Size*2));
    end;
    with FButtons[i*2+StaticButtonsCount+1] do begin
      Left := FRect.Right-18*(2-1);
      Top := FRect.Top + FItemsTop[i] + 2;
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
  FHeight := 0;
  For i:= 0 to High(FItems) do with FItems[i] do begin
    if i <> 0 Then FHeight += 6;
    t:= TStringList.Create;
    t.Text := content;
    if not FFixed Then FItemsTop[i] := FHeight;
    for j:= 0 to t.Count-1 do begin
      FHeight += ACanvas.TextHeight(t.Strings[j]);
      FWidth := Max(FWidth, ACanvas.TextWidth(t.Strings[j]));
    end;
    t.Free;
  end;
  FWidth += Padding*2;
  FHeight += Padding*2;
  if (Length(FItems) <> 0) and Not FFixed Then FItemsTop[High(FItemsTop)] := FHeight;
end;

procedure TOLAPCell.MouseClick(x, y: integer);
var i: integer;
begin
  for i:= 0 to High(FButtons) do
    if FButtons[i].MouseClick(x, y) Then Break;
end;

function TOLAPCell.MouseMove(x, y: integer): boolean;
var i: integer;
begin
  FItemHover := -1;
  if (FRect.Right = 0) or (FRect.Bottom = 0) Then exit;
  if InRange(x, FRect.Left, FRect.Right)
    and InRange(y, FRect.Top, FRect.Bottom) Then begin
    FHover := True;
    for i:= 0 to High(FItemsTop)-1 do
      if InRange(y, FRect.Top + FItemsTop[i], FRect.Top + FItemsTop[i+1]) Then FItemHover := i;
  end else
    FHover := false;
  Result := False;
  for i:= 0 to High(FButtons) do
    if FButtons[i].MouseMove(x, y) Then Result := true;
end;

constructor TOLAPCell.Create(AOwner: TComponent; AFixed: Boolean;
  ACallback: TOLAPButtonCallback; APosition: TPoint);
begin
  inherited Create(AOwner);
  FPosition := APosition;
  FCallback := ACallback;
  FFixed:=AFixed;
  FItemHoverFixed := -1;
  FItemHover := -1;
  setLength(FButtons, StaticButtonsCount);
  FButtons[0] := TOLAPCellButtonShowFull.Create(Self, -1, FCallback);
  FButtons[1] := TOLAPCellButtonAdd.Create(Self, -1, FCallback);
  FButtons[2] := TOLAPCellButtonOpenTable.Create(Self, -1, FCallback);
end;

end.

