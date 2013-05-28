unit CLOLAPGrid;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, Dialogs, db, sqldb, CLOLAPCell, CLMetadata, CLDatabase, CLOLAPTypes,
  CLFilter;

type

  ArrOfBoolean = array of Boolean;

  { TOLAPGrid }

  TOLAPGrid = class(TComponent)
  private
    Filter: TFilter;
    FTableId, FAxisX, FAxisY, FSort: integer;
    FStringListX, FStringListY: TStringList;
    FDisplayFields: array of Boolean;
    FShowNames, FShowEmpty: Boolean;
    FCallback: TOLAPButtonCallback;
    procedure FillStringList(StringList: TStringList; FieldId: integer);
    procedure HideEmpty(var ACells: TOLAPCells; Used: ArrOfBoolean; Col: boolean);
  public
    property AsixX: integer read FAxisX write FAxisX;
    property AsixY: integer read FAxisY write FAxisY;
    property SortBy: integer read FSort write FSort;
    property ShowNames: Boolean read FShowNames write FShowNames;
    property ShowEmpty: Boolean read FShowEmpty write FShowEmpty;
    procedure RebuildGrid(var FCells: TOLAPCells);
    constructor Create(AOwner: TComponent; ATableId: integer; AFilter: TFilter;ACallback: TOLAPButtonCallback); virtual;
    destructor Destroy; override;
  published
    property DisplayFields: ArrOfBoolean read FDisplayFields write FDisplayFields;
  end;

implementation

{ TOLAPGrid }

procedure TOLAPGrid.FillStringList(StringList: TStringList; FieldId: integer);
var
  FSQLQuery: TSQLQuery;
  FDatasource: TDataSource;
begin
  FSQLQuery := TSQLQuery.Create(nil);
  FDatasource := TDataSource.Create(nil);
  FDatasource.DataSet := FSQLQuery;
  FSQLQuery.Transaction := Transaction;
  FSQLQuery.SQL.Text := 'SELECT name FROM '
    +Metadata[FTableId].Columns[FieldId].referenceTable+#13#10'ORDER BY id';
  FSQLQuery.Open;
  StringList.Clear;
  with FDatasource.DataSet do
    while not EOF do begin
      StringList.Add(Fields.Fields[0].AsString);
      Next;
    end;
  FreeAndNil(FDatasource);
  FreeAndNil(FSQLQuery);
end;

procedure TOLAPGrid.HideEmpty(var ACells: TOLAPCells; Used: ArrOfBoolean;
  Col: boolean);
var x, t1, t2, i, j, k: integer;
begin
  if Col Then begin t1:= High(ACells); t2:= High(Acells[0]); end
  else begin t2:= High(ACells); t1:= High(Acells[0]); end;
  x:= 0;
  for i:= t1 downto 1 do
    if not Used[i-1] Then begin
      x+= 1;
      for j:= 0 to t2 do if Col Then ACells[i][j].Free else ACells[j][i].Free;
      for k:= i to t1-1 do
        for j:= 0 to t2 do
          if Col Then ACells[k][j] := ACells[k+1][j]
          else ACells[j][k] := ACells[j][k+1];
      Continue;
    end;
  t1:= 0; t2:= 0;
  if Col Then t1:= x else t2:= x;
  SetLength(ACells, Length(ACells)-t1, Length(ACells[0])-t2);
end;

procedure TOLAPGrid.RebuildGrid(var FCells: TOLAPCells);
var
  i, j, x, y: integer;
  s: string;
  sArr: array of string;
  SQLQuery: TSQLQuery;
  Datasource: TDataSource;
  UsedX, UsedY: array of Boolean;
begin
  for i:= 0 to high(FCells) do
    for j:= 0 to high(FCells[i]) do
      FCells[i][j].Free;

  FillStringList(FStringListX, FAxisX);
  FillStringList(FStringListY, FAxisY);
  SetLength(FCells, FStringListX.Count+1, FStringListY.Count+1);

  for i:= 0 to high(FCells) do
    for j:= 0 to high(FCells[i]) do
      FCells[i][j] := TOLAPCell.Create(Self, (i=0)or(j=0), FCallback, Point(i, j));

  for i:= 1 to High(FCells) do
    FCells[i][0].AddItem(-1, FStringListX.Strings[i-1]);
  for i:= 1 to High(FCells[0]) do
    FCells[0][i].AddItem(-1, FStringListY.Strings[i-1]);

  SQLQuery := TSQLQuery.Create(nil);
  Datasource := TDataSource.Create(nil);
  Datasource.DataSet := SQLQuery;
  SQLQuery.Transaction := Transaction;
  SQLQuery.SQL.Text := GetJoinedSQL(FTableId)+#13#10;
  if Filter.Applyed Then begin
    SQLQuery.SQL.Text:=SQLQuery.SQL.Text+Filter.GetWhereSQL(sArr)+#13#10;
    for i:= 0 to High(sArr) do
      SQLQuery.ParamByName('P'+intToStr(i)).AsString:= sArr[i];
  end;
  SQLQuery.SQL.Text := SQLQuery.SQL.Text+GetOrderBySQL(FTableId, [FAxisX, FAxisY, FSort], ['id', 'id', 'name']);
  SQLQuery.Open;

  x:= 0; y:= 0;
  SetLength(UsedX, FStringListX.Count);
  SetLength(UsedY, FStringListY.Count);
  while x < FStringListX.Count do With Datasource.DataSet do begin
    while (FStringListX.Strings[x]=Fields.Fields[FAxisX].AsString) and
          (FStringListY.Strings[y]=Fields.Fields[FAxisY].AsString) and
          not EOF do begin
      s:= '';
      for i:= 0 to Fields.Count-1 do begin
        if not FDisplayFields[i] then continue;
        if s <> '' Then s += #13#10;
        if FShowNames Then s += Metadata[FTableId].Columns[i].display+': ';
        s += Fields.Fields[i].AsString;
      end;
      UsedX[x]:= true;
      UsedY[y]:= true;
      FCells[x+1][y+1].AddItem(Fields.Fields[0].AsInteger, s);
      Next;
    end;
    if y = FStringListY.Count-1 Then begin
      y:= 0;
      x+= 1;
    end else y+= 1;
  end;

  SQLQuery.Free;
  Datasource.Free;

  if not ShowEmpty Then begin
    HideEmpty(FCells, UsedX, True);
    HideEmpty(FCells, UsedY, False);
  end;
end;

constructor TOLAPGrid.Create(AOwner: TComponent; ATableId: integer;
  AFilter: TFilter; ACallback: TOLAPButtonCallback);
begin
  inherited Create(AOwner);
  FTableId := ATableId;
  Filter := AFilter;
  FCallback := ACallback;
  SetLength(FDisplayFields, Length(Metadata[FTableId].Columns));
  FStringListX := TStringList.Create;
  FStringListY := TStringList.Create;
end;

destructor TOLAPGrid.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FStringListX);
  FreeAndNil(FStringListY);
end;

end.

