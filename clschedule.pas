unit CLSchedule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Grids, CLMetadata, CLScheduleCell, sqldb, db,
  CLDatabase, CLFilter;

procedure ReBuildGridContent(TheOwner: TComponent; var Cells: ArrOfArrOfDrawGridCell;
  TableId, Horizontal, Vertical, SortBy: Integer;
  Filter: TFilter;
  ShowFullTableCell: TShowFullCell; EditCellItem: TEditCell;
  ShowEmptyLines, ShowFieldsNames: boolean; DisplayFields: array of boolean);

implementation

procedure ReBuildGridContent(TheOwner: TComponent; var Cells: ArrOfArrOfDrawGridCell;
  TableId, Horizontal, Vertical, SortBy: Integer;
  Filter: TFilter;
  ShowFullTableCell: TShowFullCell; EditCellItem: TEditCell;
  ShowEmptyLines, ShowFieldsNames: boolean; DisplayFields: array of boolean);
  procedure FillStringList(StringList: TStringList; Field: integer; FilterState: TFilterState);
  var
    FSQLQuery: TSQLQuery;
    FDatasource: TDataSource;
    arr: array of String;
    s: String;
    i: integer;
  begin
    s := Filter.GetWhereSQL(arr, FilterState);
    FSQLQuery := TSQLQuery.Create(nil);
    FDatasource := TDataSource.Create(nil);
    FDatasource.DataSet := FSQLQuery;
    FSQLQuery.Transaction := Transaction;
    { TODO: Нужно узнать есть или нет это в фильтре }
    FSQLQuery.SQL.Text := 'SELECT id, name FROM '+Metadata[TableId].Columns[Field].referenceTable;
    if Length(arr) <> 0 Then begin
      FSQLQuery.SQL.Text := FSQLQuery.SQL.Text+' '+s;
      for i:= 0 to High(arr) do
        FSQLQuery.ParamByName('P'+intToStr(i)).AsString:= arr[i];
    end;
    FSQLQuery.SQL.Text := FSQLQuery.SQL.Text+' ORDER BY id';
    FSQLQuery.Open;
    while not FDatasource.DataSet.EOF do begin
      StringList.AddObject(FDatasource.DataSet.Fields.Fields[1].AsString,
        TObject(IntPtr(FDatasource.DataSet.Fields.Fields[0].AsInteger)));
      FDatasource.DataSet.Next;
    end;
    FreeAndNil(FDatasource);
    FreeAndNil(FSQLQuery);
  end;
  procedure RemoveEmpty(col: boolean); { TODO: В этой функции ошибка: неправильно изменяется размер массива }
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
          SetLength(Cells, c, r+1);
          c := High(Cells);
          r := High(Cells[0]);
        end else begin
          SetLength(Cells, r+1, c);
          r := High(Cells);
          c := High(Cells[0]);
        end;
      end else i += 1;
    end;
  end;
var
  i, j, x, y: integer;
  s: string;
  arrstr: array of string;
  Datasource: TDataSource;
  SQLQuery: TSQLQuery;
  StringListH, StringListV: TStringList;
  FilterState, FilterStateH, FilterStateV: TFilterState;
  PFilterState: ^TFilterState;
begin
  for i:= 0 to High(Cells) do
    for j:= 0 to High(Cells[i]) do
      Cells[i][j].Free;

  FilterState := Filter.FilterState;
  FilterStateH.count := 0;
  FilterStateV.count := 0;
  i := 0;
  while i < FilterState.count do begin
    if (FilterState.field[i] = Horizontal) or (FilterState.field[i] = Vertical) Then begin
      if FilterState.field[i] = Horizontal Then PFilterState:=@FilterStateH
      else PFilterState := @FilterStateV;
      PFilterState^.field[PFilterState^.count]:= FilterState.field[i];
      PFilterState^.oper[PFilterState^.count]:= FilterState.oper[i];
      PFilterState^.content[PFilterState^.count]:= FilterState.content[i];
      PFilterState^.count := PFilterState^.count + 1;
      For j:= i to FilterState.count - 2 do begin
        FilterState.field[j]:= FilterState.field[j+1];
        FilterState.oper[j]:= FilterState.oper[j+1];
        FilterState.content[j]:= FilterState.content[j+1];
      end;
      Dec(FilterState.count);
    end else inc(i);
  end;

  StringListH := TStringList.Create;
  FillStringList(StringListH, Horizontal, FilterStateH);
  StringListV := TStringList.Create;
  FillStringList(StringListV, Vertical, FilterStateV);
  x:= StringListH.Count;
  y:= StringListV.Count;
  SetLength(Cells, x+1, y+1);
  for i:= 0 to x do
    for j:= 0 to y do begin
      Cells[i][j] := TDrawGridCell.Create(TheOwner, ShowFullTableCell, EditCellItem);
      if (i <> 0) and (j <> 0) Then
        Cells[i][j].PositionValue := Point(IntPtr(StringListH.Objects[i-1]),
          IntPtr(StringListV.Objects[j-1]));
    end;
  for i:= 1 to x do
    Cells[i][0].AddItem(-1, StringListH[i-1]);
  for i:= 1 to y do
    Cells[0][i].AddItem(-1, StringListV[i-1]);

  SQLQuery := TSQLQuery.Create(nil);
  Datasource := TDataSource.Create(nil);
  Datasource.DataSet := SQLQuery;
  SQLQuery.Transaction := Transaction;
  if (SortBy = Vertical) or (SortBy = Horizontal) Then SortBy := -1;
  SQLQuery.SQL.Text := GetJoinedSQL(TableId, Vertical, Horizontal);
  s:= Filter.GetWhereSQL(arrstr, FilterState);
  if length(arrstr) <> 0 Then begin
    SQLQuery.SQL.Text := SQLQuery.SQL.Text+' '+s;
    for i:= 0 to High(arrstr) do
      SQLQuery.ParamByName('P'+intToStr(i)).AsString:= arrstr[i];
  end;
  SQLQuery.SQL.Text := SQLQuery.SQL.Text+#13#10+GetOrderBySQL(TableId, SortBy);
  SQLQuery.Open;

  while not Datasource.DataSet.EOF do begin
    With Datasource.DataSet do begin
      x:= StringListH.IndexOfObject(TObject(IntPtr(Fields.Fields[Horizontal].AsInteger)));
      y:= StringListV.IndexOfObject(TObject(IntPtr(Fields.Fields[Vertical].AsInteger)));
      if (x <> -1) and (y <> -1) Then begin
        s:= '';
        for i:= 0 to Fields.Count-1 do begin
          if not DisplayFields[i] then continue;
          if s <> '' Then s += #13#10;
          if ShowFieldsNames then s += Metadata[TableId].Columns[i].display+': ';
          s += Fields.Fields[i].AsString;
        end;
        Cells[x+1][y+1].AddItem(Fields.Fields[0].AsInteger, s);
      end;
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

