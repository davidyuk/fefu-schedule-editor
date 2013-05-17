unit CLFormSchedule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ExtCtrls, CLFormChild, CLDatabase, CLMetadata, sqldb, db,
  CLFormScheduleCell, Math;

type

  { TFormSchedule }

  TFormSchedule = class(TFormChild)
    ButtonAline: TButton;
    ComboBoxH: TComboBox;
    ComboBoxV: TComboBox;
    ComboBoxSort: TComboBox;
    Datasource: TDatasource;
    DrawGrid: TDrawGrid;
    LabelError: TLabel;
    LabelH: TLabel;
    LabelV: TLabel;
    LabelSort: TLabel;
    PaintBox: TPaintBox;
    PanelControls: TPanel;
    PanelHint: TPanel;
    PanelTop: TPanel;
    PanelRight: TPanel;
    SQLQuery: TSQLQuery;
    procedure ButtonAlineClick(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure DrawGridClick(Sender: TObject);
    procedure DrawGridDblClick(Sender: TObject);
    procedure DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure PaintBoxMouseLeave(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
  private
    Cells: array of array of TDrawGridCell;
    MouseOnGrid: TPoint;
    LastMouseCursor: TCursor;
    FullTableCell: TDrawGridCell;
    procedure ReBuildDrawGridContent;
    function GetSelectSQL:string;
    function GetItemCount(ATableId: integer):integer;
    function FieldListStr(AFormat: String): String;
    function GetJoinedSQL: string;
    procedure ShowFullTableCell(content: TDrawGridCell);
    procedure AlineRow(ARow: integer);
    procedure AlineCol(ACol: integer);
  public
    procedure RefreshSQLContent; override;
    //constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ TFormSchedule }

procedure TFormSchedule.FormShow(Sender: TObject);
var i: integer;
begin
  FTableId := Metadata.GetTableId('schedule_items');
  FRecordId := -1;
  If FTableId = -1 Then begin
    MessageDlg('Таблица "schedule_items" не найдена', mtError,  [mbOK], 0);
    Close;
    Exit;
  end;
  for i:= 0 to high(Metadata[TableId].Columns) do begin
    ComboBoxH.AddItem(Metadata[TableId].Columns[i].display, nil);
    ComboBoxV.AddItem(Metadata[TableId].Columns[i].display, nil);
    ComboBoxSort.AddItem(Metadata[TableId].Columns[i].display, nil);
  end;
  ComboBoxH.ItemIndex := 1;
  ComboBoxV.ItemIndex := 2;
  ComboBoxSort.ItemIndex := 3;
  ReBuildDrawGridContent;
end;

procedure TFormSchedule.PaintBoxMouseLeave(Sender: TObject);
begin
  with Sender as TPaintBox do
    Parent.Visible := false;
end;

procedure TFormSchedule.PaintBoxPaint(Sender: TObject);
var t: TRect;
begin
  with Sender as TPaintBox do begin
    with t do begin
      Left := 0;
      Top := 0;
      Right := Width;
      Bottom := Height;
    end;
    FullTableCell.Draw(Canvas, t, false); { ReadBounds плохо подходит }
  end;
end;

procedure TFormSchedule.ComboBoxChange(Sender: TObject);
begin
  if ComboBoxH.ItemIndex = ComboBoxV.ItemIndex Then begin
    ComboBoxH.Font.Color := clYellow;
    ComboBoxV.Font.Color := clYellow;
    DrawGrid.Visible := false;
    Exit;
  end;
  ComboBoxH.Font.Color := clDefault;
  ComboBoxV.Font.Color := clDefault;
  DrawGrid.Visible := true;
  ReBuildDrawGridContent;
end;

procedure TFormSchedule.ButtonAlineClick(Sender: TObject);
var i: integer;
begin
  for i:= 0 to High(Cells) do
    AlineCol(i);
  for i:= 0 to High(Cells[0]) do
    AlineRow(i);
end;

procedure TFormSchedule.ReBuildDrawGridContent;
var i, j, x, y: integer; arrstr: array of string;

  procedure FillFixedPart(horizontal: boolean);
  var FSQLQuery: TSQLQuery; FDatasource: TDataSource; i, j: integer;
  begin
    FSQLQuery := TSQLQuery.Create(nil);
    FDatasource := TDataSource.Create(nil);
    if horizontal Then i:= ComboBoxH.ItemIndex
    else i:= ComboBoxV.ItemIndex;
    FDatasource.DataSet := FSQLQuery;
    FSQLQuery.Transaction := Transaction;
    FSQLQuery.SQL.Text := 'SELECT name FROM '+Metadata[TableId].Columns[i].referenceTable
      +' ORDER BY id';
    FSQLQuery.Open;
    if horizontal Then begin i:= 1; j:= 0; end
    else begin i:= 0; j:= 1; end;
    while not FDatasource.DataSet.EOF do begin
      Cells[i][j].AddItem(-1, [FDatasource.DataSet.Fields.Fields[0].AsString]);
      Cells[i][j].Fixed := True;
      if horizontal Then inc(i)
      else inc(j);
      FDatasource.DataSet.Next;
    end;
    FreeAndNil(FDatasource);
    FreeAndNil(FSQLQuery);
  end;

begin
  for i:= 0 to High(Cells) do
    for j:= 0 to High(Cells[i]) do
      Cells[i][j].Free;
  x:= GetItemCount(Metadata.GetTableId(Metadata[TableId].Columns[ComboBoxH.ItemIndex].referenceTable));
  y:= GetItemCount(Metadata.GetTableId(Metadata[TableId].Columns[ComboBoxV.ItemIndex].referenceTable));
  SetLength(Cells, x+1, y+1);
  for i:= 0 to x do
    for j:= 0 to y do
      Cells[i][j] := TDrawGridCell.Create(self, @ShowFullTableCell);
  DrawGrid.ColCount := x+1;
  DrawGrid.RowCount := y+1;

  Cells[0][0].Fixed := true;
  FillFixedPart(false);
  FillFixedPart(true);

  SQLQuery.Close;
  SQLQuery.Transaction := Transaction;
  SQLQuery.SQL.Text := GetJoinedSQL;
  SQLQuery.Open;

  while not Datasource.DataSet.EOF do begin
    With Datasource.DataSet do begin
      x:= Fields.Fields[ComboBoxH.ItemIndex].AsInteger;
      y:= Fields.Fields[ComboBoxV.ItemIndex].AsInteger;
      SetLength(arrstr, 0);
      for i:= 1 to Fields.Count-1 do begin
        if (i = ComboBoxH.ItemIndex) or (i = ComboBoxV.ItemIndex) then continue;
        SetLength(arrstr, Length(arrstr)+1);
        arrstr[High(arrstr)] := Fields.Fields[i].AsString;
      end;
      Cells[x][y].AddItem(Fields.Fields[0].AsInteger, arrstr);
      Cells[x][y].CreateButtons;
      Next;
    end;
  end;
end;

function TFormSchedule.GetJoinedSQL: string;
{ TODO : Эта функция повторяется в CLFormTable }
var
  firstPart: string;
  i: integer;
begin
with Metadata[TableId] do begin
  firstPart := '';
  result := '';
  for i:= 0 to High(Columns) do begin
    if (Columns[i].referenceTable = '') or (i = ComboBoxH.ItemIndex) or (i = ComboBoxV.ItemIndex) Then begin
      firstPart += ', '+name+'.'+Columns[i].name;
    end else begin
      firstPart += ', '+Columns[i].referenceTable+'.name';
      result+='INNER JOIN '+Columns[i].referenceTable+' ON '+name+'.'+Columns[i].name+' = '+Columns[i].referenceTable+'.id'+#13#10;
    end;
  end;
  result := 'SELECT '+Copy(firstPart, 3, length(firstPart))+' FROM '+name+#13#10+result;
  if (ComboBoxSort.ItemIndex <> ComboBoxH.ItemIndex) and
     (ComboBoxSort.ItemIndex <> ComboBoxV.ItemIndex) Then
    result += 'ORDER BY '+Columns[ComboBoxSort.ItemIndex].referenceTable+'.id';
end;
end;

procedure TFormSchedule.ShowFullTableCell(content: TDrawGridCell);
begin
  PanelHint.Width := content.TextWidth+2; //тк border - 1px
  PanelHint.Height := content.TextHeight+2;
  PanelHint.Left := content.Rect.Left+DrawGrid.Left;
  PanelHint.Top := content.Rect.Top+DrawGrid.Top;
  FullTableCell := content;
  PanelHint.Visible := true;
end;

procedure TFormSchedule.AlineRow(ARow: integer);
var i, m: integer;
begin
  m := 0;
  for i:= 0 to High(Cells) do
    m :=  Max(m, Cells[i][ARow].TextHeight+1);
  DrawGrid.RowHeights[ARow]:= m;
end;

procedure TFormSchedule.AlineCol(ACol: integer);
var i, m: integer;
begin
  m := 0;
  for i:= 0 to High(Cells[ACol]) do
    m :=  Max(m, Cells[ACol][i].TextWidth+1);
  DrawGrid.ColWidths[ACol]:= m;
end;

procedure TFormSchedule.DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var i, j, topPos, maxWidth, t: integer;
begin
  Cells[aCol][aRow].Draw(DrawGrid.Canvas, aRect, true);
end;

procedure TFormSchedule.DrawGridMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var CellX, CellY, i, j: integer;
begin
  MouseOnGrid := Point(x, y);
  with Sender as TDrawGrid do begin
    if Cursor = 0 Then LastMouseCursor := 0;
    MouseToCell(x, y, CellX, CellY);
    for i:= 0 to High(Cells) do
      for j:= 0 to High(Cells[i]) do
        Cells[i][j].MouseMove(x, y);
    //Cells[CellX][CellY].MouseMove(x, y);
    //InvalidateCell(CellX, CellY);
    Invalidate;
  end;
end;

procedure TFormSchedule.DrawGridClick(Sender: TObject);
begin
  with Sender as TDrawGrid do
    Cells[Col][Row].MouseClick(MouseOnGrid.x, MouseOnGrid.y);
end;

procedure TFormSchedule.DrawGridDblClick(Sender: TObject);
var Cell: TPoint;
begin
  with Sender as TDrawGrid do begin
    MouseToCell(MouseOnGrid.x, MouseOnGrid.y, Cell.x, Cell.y);
    if (Cell.x <> 0) and (Cell.y <> 0) Then exit;
    if (Cell.x = 0) and (Cell.y = 0) Then begin
      ButtonAlineClick(Nil);
      exit;
    end;
    if Cell.x = 0 Then AlineRow(Cell.y)
    else AlineCol(Cell.x);
  end;
end;

procedure TFormSchedule.RefreshSQLContent;
begin
  ReBuildDrawGridContent;
end;

function TFormSchedule.GetSelectSQL: string;
begin
  with Metadata[TableId] do
    result:='SELECT '+FieldListStr('%s') + ' FROM '+name+' WHERE id = '+intToStr(RecordId);
end;

function TFormSchedule.GetItemCount(ATableId: integer): integer;
var FSQLQuery: TSQLQuery; FDatasource: TDataSource;
begin
  FSQLQuery := TSQLQuery.Create(nil);
  FDatasource := TDataSource.Create(nil);
  FDatasource.DataSet := FSQLQuery;
  FSQLQuery.Transaction := Transaction;
  FSQLQuery.SQL.Text := 'SELECT COUNT(*) FROM '+Metadata[ATableId].name;
  FSQLQuery.Open;
  Result := FDatasource.DataSet.Fields.Fields[0].Value;
  FreeAndNil(FDatasource);
  FreeAndNil(FSQLQuery);
end;

function TFormSchedule.FieldListStr(AFormat: String): String;
var
  i: Integer;
begin
  Result := '';
  with Metadata[TableId] do
    for i := 0 to High(Columns) do begin
      if i > 0 then Result += ', ';
      Result += Format(AFormat, [Columns[i].name]);
    end;
end;

end.
