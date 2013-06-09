unit CLConflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, CLMetadata, CLDatabase, Dialogs, Types;

type

  arrOfInteger = array of integer;
//  TIntegerDynArray

  TConflictType = (ctSameFields);

  TConflict = record
    name: String;
    cells: array of integer;
    ctype: TConflictType;
  end;

  { TConflictsFinder }

  TConflictsFinder = class(TComponent)
  private const
    TableName = 'schedule_items';
    DayFieldName = 'day_id';
    TimeFieldName = 'time_id';
  private
    FTableId, DayFieldId, TimeFieldId, FCount: integer;
    FConflicts: array of TConflict;
    FConflictCellsId: TStringList;
    function GetConflict(AIndex: integer): TConflict;
  public
    property TableId: Integer read FTableId;
    property Conflicts[AIndex: integer]: TConflict read GetConflict; default;
    property Count: integer read FCount;
    procedure UpdateConflicts;
    procedure CheckCellId(id: Integer; var arr: arrOfInteger);
    function GetConflictTypeName(ConflictType: TConflictType): String;
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
  x, y, i, t1: integer;
  key, t2: String;
  SQLQuery: TSQLQuery;
  Datasource: TDataSource;
  StringList: TStringList;
begin
  SetLength(FConflicts, 0);
  FConflictCellsId.Clear;

  SQLQuery := TSQLQuery.Create(nil);
  Datasource := TDataSource.Create(nil);
  Datasource.DataSet := SQLQuery;
  SQLQuery.Transaction := Transaction;
  SQLQuery.SQL.Text := GetSelectSQL(FTableId, -1)+' '+GetOrderBySQL(FTableId, [DayFieldId, TimeFieldId], []);
  //ShowMessage(SQLQuery.SQL.Text);
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
              name := Metadata[FTableId].Columns[i].display;
              SetLength(cells, 2);
              cells[0] := StrToInt(StringList.Values[key]);
              cells[1] := Fields[0].AsInteger;
              with FConflictCellsId do begin
                t2:= IntToStr(cells[0]);
                if Values[t2] <> '' Then Values[t2] := Values[t2]+';';
                Values[t2] := Values[t2]+intToStr(High(FConflicts));
                t2:= IntToStr(cells[1]);
                if Values[t2] <> '' Then Values[t2] := Values[t2]+';';
                Values[t2] := Values[t2]+intToStr(High(FConflicts));
              end;
              ctype := ctSameFields;
            end;
            StringList.Values[key] := 'c_'+IntToStr(High(FConflicts));
          end else begin
            t1 := StrToInt(Copy(StringList.Values[key], 3, Length(StringList.Values[key])));
            with FConflicts[t1] do begin
              SetLength(cells, Length(cells)+1);
              cells[High(cells)] := Fields[0].AsInteger;
              with FConflictCellsId do begin
                t2 := IntToStr(cells[High(cells)]);
                if Values[t2] <> '' Then Values[t2] := Values[t2]+';';
                Values[t2] := Values[t2]+intToStr(t1);
              end;
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

procedure TConflictsFinder.CheckCellId(id: Integer; var arr: arrOfInteger);
var
  StringList: TStringList;
  i: integer;
begin
  StringList := TStringList.Create;
  StringList.Delimiter := ';';
  StringList.DelimitedText := FConflictCellsId.Values[intToStr(id)];
  SetLength(arr, StringList.Count);
  with StringList do
    for i:= 0 to Count-1 do
      arr[i] := StrToInt(Strings[i]);
  FreeAndNil(StringList);
end;

function TConflictsFinder.GetConflictTypeName(ConflictType: TConflictType
  ): String;
begin
  Case ConflictType of
    ctSameFields: Result := 'Одинаковые значения поля';
  else
    Exception.Create('Conflict type name not found');
  end;
end;

constructor TConflictsFinder.Create(AOwner: TComponent);
var i: integer;
begin
  inherited Create(AOwner);
  FConflictCellsId := TStringList.Create;
  FTableId := Metadata.GetTableId(TableName);
  for i := 0 to High(Metadata[FTableId].Columns) do begin
    if Metadata[FTableId].Columns[i].name = DayFieldName Then DayFieldId := i;
    if Metadata[FTableId].Columns[i].name = TimeFieldName Then TimeFieldId := i;
  end;
end;

destructor TConflictsFinder.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FConflictCellsId);
end;

initialization

  ConflictsFinder := TConflictsFinder.Create(nil);

finalization

  ConflictsFinder.Free;

end.

