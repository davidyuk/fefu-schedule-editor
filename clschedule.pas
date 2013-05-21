unit CLSchedule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Grids, CLMetadata, CLScheduleCell, sqldb, db, CLDatabase;

procedure ReBuildGridContent(DrawGrid: TDrawGrid; var Cells: ArrOfArrOfDrawGridCell;
  TableId, Horizontal, Vertical, SortBy: Integer;
  ShowFullTableCell: TShowFullCell; EditCellItem: TEditCell;
  ShowEmptyLines, ShowFieldsNames: boolean; DisplayFields: array of boolean);

implementation

procedure ReBuildGridContent(DrawGrid: TDrawGrid; var Cells: ArrOfArrOfDrawGridCell;
  TableId, Horizontal, Vertical, SortBy: Integer;
  ShowFullTableCell: TShowFullCell; EditCellItem: TEditCell;
  ShowEmptyLines, ShowFieldsNames: boolean; DisplayFields: array of boolean);
  procedure FillStringList(StringList: TStringList; Field: integer);
  var
    FSQLQuery: TSQLQuery;
    FDatasource: TDataSource;
  begin
    FSQLQuery := TSQLQuery.Create(nil);
    FDatasource := TDataSource.Create(nil);
    FDatasource.DataSet := FSQLQuery;
    FSQLQuery.Transaction := Transaction;
    FSQLQuery.SQL.Text := 'SELECT id, name FROM '+Metadata[TableId].Columns[Field].referenceTable
      +' ORDER BY id';
    FSQLQuery.Open;
    while not FDatasource.DataSet.EOF do begin
      StringList.AddObject(FDatasource.DataSet.Fields.Fields[1].AsString,
        TObject(IntPtr(FDatasource.DataSet.Fields.Fields[0].AsInteger)));
      FDatasource.DataSet.Next;
    end;
    FreeAndNil(FDatasource);
    FreeAndNil(FSQLQuery);
  end;
  procedure RemoveEmpty(col: boolean);
  var
    i, j, k, c, r: integer;
    f: boolean;
  begin
    i:= 1;
    if col Then begin
      c := High(Cells);
      r := High(Cells[0]);
    end else begin
      r := High(Cells);
      c := High(Cells[0]);
    end;
    while i <= c do begin
      f:= true;
      for j:= 1 to r do
        if (col and (Cells[i][j].Text <> '')) or
           (not col and (Cells[j][i].Text <> '')) Then begin
          f:= false;
          break;
        end;
      if f Then begin
        for j:= 0 to r do
          if col Then FreeAndNil(Cells[i][j]) else FreeAndNil(Cells[j][i]);
        for j:= i to c-1 do
          for k:= 0 to r do
            if col Then Cells[j][k] := Cells[j+1][k]
            else Cells[k][j] := Cells[k][j+1];
        if col Then begin
          DrawGrid.ColCount := DrawGrid.ColCount - 1;
          SetLength(Cells, c, r+1);
          c := High(Cells);
          r := High(Cells[0]);
        end else begin
          DrawGrid.RowCount := DrawGrid.RowCount - 1;
          SetLength(Cells, r+1, c);
          r := High(Cells);
          c := High(Cells[0]);
        end;
      end else i += 1;
    end;
  end;
var
  i, j, x, y: integer;
  arrstr: array of string;
  Datasource: TDataSource;
  SQLQuery: TSQLQuery;
  StringListH, StringListV: TStringList;
begin
  for i:= 0 to High(Cells) do
    for j:= 0 to High(Cells[i]) do
      Cells[i][j].Free;

  StringListH := TStringList.Create;
  FillStringList(StringListH, Horizontal);
  StringListV := TStringList.Create;
  FillStringList(StringListV, Vertical);
  x:= StringListH.Count;
  y:= StringListV.Count;
  SetLength(Cells, x+1, y+1);
  for i:= 0 to x do
    for j:= 0 to y do begin
      Cells[i][j] := TDrawGridCell.Create(DrawGrid.Owner, ShowFullTableCell, EditCellItem);
      if (i <> 0) and (j <> 0) Then
        Cells[i][j].PositionValue := Point(IntPtr(StringListH.Objects[i-1]),
          IntPtr(StringListV.Objects[j-1]));
    end;
  DrawGrid.ColCount := x+1;
  DrawGrid.RowCount := y+1;
  for i:= 1 to x do
    Cells[i][0].AddItem(-1, [StringListH[i-1]]);
  for i:= 1 to y do
    Cells[0][i].AddItem(-1, [StringListV[i-1]]);

  SQLQuery := TSQLQuery.Create(nil);
  Datasource := TDataSource.Create(nil);
  Datasource.DataSet := SQLQuery;
  SQLQuery.Transaction := Transaction;
  if (SortBy = Vertical) or (SortBy = Horizontal) Then SortBy := -1;
  SQLQuery.SQL.Text := GetJoinedSQL(TableId, Vertical, Horizontal, SortBy);
  SQLQuery.Open;

  while not Datasource.DataSet.EOF do begin
    With Datasource.DataSet do begin
      x:= StringListH.IndexOfObject(TObject(IntPtr(Fields.Fields[Horizontal].AsInteger)));
      y:= StringListV.IndexOfObject(TObject(IntPtr(Fields.Fields[Vertical].AsInteger)));
      SetLength(arrstr, 0);
      for i:= 0 to Fields.Count-1 do begin
        if not DisplayFields[i] then continue;
        SetLength(arrstr, Length(arrstr)+1);
        if ShowFieldsNames then arrstr[High(arrstr)] := Metadata[TableId].Columns[i].display+': ';
        arrstr[High(arrstr)] += Fields.Fields[i].AsString;
      end;
      Cells[x+1][y+1].AddItem(Fields.Fields[0].AsInteger, arrstr);
      Next;
    end;
  end;

  SQLQuery.Free;
  Datasource.Free;
  StringListH.Free;
  StringListV.Free;

  if not ShowEmptyLines Then begin
    RemoveEmpty(True);
    RemoveEmpty(False);
  end;

  for i:= 0 to High(Cells) do
    for j:= 0 to High(Cells[i]) do
      Cells[i][j].Fixed := (i = 0) or (j = 0);
end;

end.

