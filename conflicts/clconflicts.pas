unit CLConflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, CLDatabase, Types, contnrs;

type

  TConflictType = (ctManyGroups, ctTeacherDifPlace, ctTeacherDifCourses,
    ctPlaceDifTeacher, ctPlaceDifCourses, ctPlaceOverflow,
    ctTeacherWrongSubject, ctGroupWrongSubject, ctLessonOverflow);
  // порядок задаётся на стороне БД в процедуре CONFLICTS

  TConflict = record
    cells: TIntegerDynArray;
    ctype: TConflictType;
  end;

  { TConflictsFinder }

  TConflictsFinder = class(TComponent)
  private
    FCount: integer;
    FConflicts: array of TConflict;
    FConflictCellsId: TBucketList;
    FCellsConflicts: array of TIntegerDynArray;
    function GetConflict(AIndex: integer): TConflict;
  public
    property Conflicts[AIndex: integer]: TConflict read GetConflict; default;
    property Count: integer read FCount;
    procedure UpdateConflicts;
    procedure CheckCellId(id: Integer; var arr: TIntegerDynArray);
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
  j: integer;
  SQLQuery: TSQLQuery;
  Datasource: TDataSource;
  StringList: TStringList;
  procedure AddValToEnd(val, index: integer);
  var t: integer;
  begin
    t:= Length(FCellsConflicts[index]);
    SetLength(FCellsConflicts[index], t+1);
    FCellsConflicts[index][t] := val;
  end;
  function CreateNewArray(val: integer): integer;
  begin
    Result:= Length(FCellsConflicts);
    SetLength(FCellsConflicts, Result+1);
    SetLength(FCellsConflicts[Result], 1);
    FCellsConflicts[Result][0] := val;
  end;

begin
  SetLength(FConflicts, 0);
  SetLength(FCellsConflicts, 0);
  FConflictCellsId.Free;
  FConflictCellsId := TBucketList.Create(bl16);

  SQLQuery := TSQLQuery.Create(nil);
  Datasource := TDataSource.Create(nil);
  Datasource.DataSet := SQLQuery;
  SQLQuery.Transaction := Transaction;
  SQLQuery.SQL.Text := 'SELECT CONFLICT_TYPE, SCHEDULE_ITEMS FROM CONFLICTS';
  SQLQuery.Open;

  //Datasource.DataSet.RecordCount; возвращает 10 вместо 102
  StringList := TStringList.Create;
  StringList.Delimiter := ',';
  with Datasource.DataSet do
    while not EOF do begin
      SetLength(FConflicts, Length(FConflicts)+1);
      with FConflicts[High(FConflicts)] do begin
        ctype := TConflictType(Fields[0].AsInteger);
        StringList.DelimitedText := Fields[1].AsString;
        SetLength(cells, StringList.Count);
        for j:= 0 to StringList.Count-1 do begin
          cells[j] := StrToInt(StringList[j]);
          with FConflictCellsId do
            if Exists(Pointer(cells[j])) Then
              AddValToEnd(High(FConflicts), Integer(Data[Pointer(cells[j])]))
            else
              FConflictCellsId.Add(Pointer(cells[j]), Pointer(CreateNewArray(High(FConflicts))));
        end;
        Next;
      end;
    end;

  FreeAndNil(StringList);
  SQLQuery.Free;
  Datasource.Free;
  FCount := Length(FConflicts);
end;

procedure TConflictsFinder.CheckCellId(id: Integer; var arr: TIntegerDynArray);
begin
  if FConflictCellsId.Exists(Pointer(id)) Then
    arr := FCellsConflicts[Integer(FConflictCellsId.Data[Pointer(id)])]
  else
    SetLength(arr, 0);
end;

function TConflictsFinder.GetConflictTypeName(ConflictType: TConflictType
  ): String;
begin
  Case ConflictType of
    ctGroupWrongSubject: Result := 'Группа не изучает этот предмет';
    ctLessonOverflow: Result := 'У группы много пар';
    ctManyGroups: Result := 'У группы несколько пар в одно время';
    ctPlaceDifCourses: Result := 'В аудитории несколько разных предметов';
    ctPlaceDifTeacher: Result := 'В аудитории несколько разных преподавателей';
    ctPlaceOverflow: Result := 'Переполнение аудитории';
    ctTeacherDifCourses: Result := 'Преподаватель ведёт разные предметы';
    ctTeacherDifPlace: Result := 'Преподаватель в разных аудиториях';
    ctTeacherWrongSubject: Result := 'Преподаватель не ведёт этот предмет';
  else
    Exception.Create('ConflictTypeName not found');
  end;
end;

constructor TConflictsFinder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConflictCellsId := TBucketList.Create(bl16);
end;

destructor TConflictsFinder.Destroy;
begin
  inherited Destroy;
  FConflictCellsId.Free;
end;

initialization

  ConflictsFinder := TConflictsFinder.Create(nil);

finalization

  ConflictsFinder.Free;

end.

