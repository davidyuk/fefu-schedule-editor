unit CLConflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, CLMetadata, CLDatabase, Dialogs;

type

  TConflict = record
    name: String;
    cells: array of integer;
  end;

  { TConflictsFinder }

  TConflictsFinder = class(TComponent)
  private const
    TableName = 'schedule_items';
    DayFieldName = 'day_id';
    TimeFieldName = 'time_id';
  private
    TableId, DayFieldId, TimeFieldId, FCount: integer;
    FConflicts: array of TConflict;
    function GetConflict(AIndex: integer): TConflict;
  public
    property Conflicts[AIndex: integer]: TConflict read GetConflict; default;
    property Count: integer read FCount;
    procedure UpdateConflicts;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  ConflictsFinder: TConflictsFinder;

implementation

{ TConflictsFinder }

function TConflictsFinder.GetConflict(AIndex: integer): TConflict;
begin
  Result := FConflicts[AIndex];
end;

procedure TConflictsFinder.UpdateConflicts;
var
  x, y, i, t: integer;
  key: String;
  SQLQuery: TSQLQuery;
  Datasource: TDataSource;
  StringList: TStringList;
begin
  SetLength(FConflicts, 0);

  SQLQuery := TSQLQuery.Create(nil);
  Datasource := TDataSource.Create(nil);
  Datasource.DataSet := SQLQuery;
  SQLQuery.Transaction := Transaction;
  SQLQuery.SQL.Text := GetSelectSQL(TableId, -1)+' '+GetOrderBySQL(TableId, [DayFieldId, TimeFieldId], []);
  ShowMessage(SQLQuery.SQL.Text);
  SQLQuery.Open;

  StringList := TStringList.Create;
  x:= -1; y:= -1;
  while not Datasource.DataSet.EOF do With Datasource.DataSet.Fields do begin
    if (x <> Fields[DayFieldId].AsInteger) or (y <> Fields[TimeFieldId].AsInteger) Then begin
      StringList.Clear;
      x := Fields[DayFieldId].AsInteger;
      y := Fields[TimeFieldId].AsInteger
    end;
    while not Datasource.DataSet.EOF
      and (x=Fields[DayFieldId].AsInteger)
      and (y=Fields[TimeFieldId].AsInteger) do begin
      //Все строки здесь в одно и тоже время

      for i:= 3 To Count-1 do begin
        key:= format('%d_%s', [i, Fields[i].AsString]);
        if StringList.Values[key] <> '' Then begin
          if Copy(StringList.Values[key], 1, 2) <> 'c_' Then begin
            SetLength(FConflicts, Length(FConflicts)+1);
            with FConflicts[High(FConflicts)] do begin
              name := 'Одинаковые значения поля: '+Metadata[TableId].Columns[i].display;
              SetLength(cells, 2);
              cells[0] := StrToInt(StringList.Values[key]);
              cells[1] := Fields[0].AsInteger;
            end;
            StringList.Values[key] := 'c_'+IntToStr(High(FConflicts));
          end else begin
            t := StrToInt(Copy(StringList.Values[key], 3, Length(StringList.Values[key])));
            with FConflicts[t] do begin
              SetLength(cells, Length(cells)+1);
              cells[High(cells)] := Fields[0].AsInteger;
            end;
          end;
        end else
          StringList.Values[key] := Fields[0].AsString; //запоминаем id
      end;

      Datasource.DataSet.Next;
    end;
  end;
  FreeAndNil(StringList);
  SQLQuery.Free;
  Datasource.Free;
  FCount := Length(FConflicts);
end;

constructor TConflictsFinder.Create(AOwner: TComponent);
var i: integer;
begin
  inherited Create(AOwner);
  TableId := Metadata.GetTableId(TableName);
  for i:= 0 to High(Metadata[TableId].Columns) do begin
    if Metadata[TableId].Columns[i].name = DayFieldName Then DayFieldId := i;
    if Metadata[TableId].Columns[i].name = TimeFieldName Then TimeFieldId := i;
  end;
end;

destructor TConflictsFinder.Destroy;
begin
  inherited Destroy;
end;

initialization

  ConflictsFinder := TConflictsFinder.Create(nil);

finalization

  ConflictsFinder.Free;

end.

