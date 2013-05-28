unit CLFormTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Forms, DBGrids, sysutils,
  ExtCtrls, StdCtrls, Buttons, sqldb, db, Dialogs, CLFilter, CLFilterTypes,
  CLDatabase, CLMetadata, CLFormChild, CLFormEdit, CLFormContainer;

type

  { TFormTable }

  TFormTable = class(TFormChild)
    ButtonAdd: TButton;
    ButtonEdit: TButton;
    ButtonRemove: TButton;
    Datasource: TDatasource;
    DBGrid: TDBGrid;
    PanelEdit: TPanel;
    SQLQuery: TSQLQuery;
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonEditClick(Sender: TObject);
    procedure ButtonRemoveClick(Sender: TObject);
    procedure DBGridColumnSized(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    Filter: TFilter;
    procedure RefreshColumns;
    procedure FilterApply;
  public
    procedure RefreshSQLContent; override;
    procedure BeforeRefreshSQLContent; override;
    procedure SetFilterState(FilterState: TFilterState);
    constructor Create(TheOwner: TComponent; ABookId: integer); virtual;
  end;

implementation

{$R *.lfm}

{ TFormTable }

procedure TFormTable.ButtonAddClick(Sender: TObject);
var Form: TFormEdit;
begin
  Form := TFormEdit.Create(Application, TableId, -1);
  FormContainer.AddForm(Form);
end;

procedure TFormTable.RefreshColumns;
var i: integer;
begin
  for i:= 0 to High(Metadata[TableId].Columns) do begin
    DBGrid.Columns.Items[i].Width:=Metadata[TableId].Columns[i].width;
    DBGrid.DataSource.DataSet.Fields[i].DisplayLabel:=Metadata[TableId].Columns[i].display;
  end;
end;

procedure TFormTable.RefreshSQLContent;
begin
  SQLQuery.Open;
  RefreshColumns;
end;

procedure TFormTable.BeforeRefreshSQLContent;
begin

end;

procedure TFormTable.SetFilterState(FilterState: TFilterState);
begin
  Filter.SetPanelsState(FilterState, True);
end;

constructor TFormTable.Create(TheOwner: TComponent; ABookId: integer);
begin
  inherited Create(TheOwner);
  FTableId:= ABookId;
  FRecordId:= -1;
  Datasource.DataSet := SQLQuery;
  DBGrid.DataSource := Datasource;
  SQLQuery.Transaction := Transaction;
  Datasource.Enabled := True;
  Caption:= Metadata[TableId].display;
  Filter := TFilter.Create(Self, TableId, Self, @FilterApply);
end;

procedure TFormTable.ButtonEditClick(Sender: TObject);
var Form: TFormEdit;
begin
  Form := TFormEdit.Create(Application, TableId, DBGrid.DataSource.DataSet.FieldByName('id').Value);
  FormContainer.AddForm(Form);
end;

procedure TFormTable.FilterApply;
var
  i: integer;
  s: string;
  sArr: array of string;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := GetJoinedSQL(TableId);
  if Filter.Applyed Then begin
    SQLQuery.SQL.Text:=SQLQuery.SQL.Text+' '+Filter.GetWhereSQL(sArr);
    for i:= 0 to High(sArr) do
      SQLQuery.ParamByName('P'+intToStr(i)).AsString:= sArr[i];
  end;
  SQLQuery.Open;
  RefreshColumns;
end;

procedure TFormTable.ButtonRemoveClick(Sender: TObject);
var
  i: integer;
  s, id: string;
  SQLQueryL: TSQLQuery;
begin
{ TODO : Надо спросить у пользователя, если он удаляет запись из справочника: присвоить этому полю значение Null; удалить все записи ссылающиеся на эту; установить всем записям, ссылающимся на эту значение Null }
  id := Datasource.DataSet.Fields.Fields[0].DisplayText;
  if id = '' Then exit;
  s:= '';
  for i:= 0 to Datasource.DataSet.Fields.Count-1 do
    s+= #13#10+DBGrid.DataSource.DataSet.Fields[i].DisplayLabel+': '+Datasource.DataSet.Fields.Fields[i].DisplayText;
  if MessageDlg('Подтверждение удаления записи', 'Вы действительно хотите удалить запись?'+
  s, mtWarning, mbOKCancel, 0) = mrOK Then begin
    SQLQueryL := TSQLQuery.Create(Nil);
    SQLQueryL.Transaction := Transaction;
    SQLQueryL.SQL.Text := 'DELETE from '+Metadata[TableId].name+' WHERE id = '+id;
    SQLQueryL.ExecSQL;
    FormContainer.BeforeRefreshSQLContent;
    Transaction.Commit;
    FormContainer.RefreshSQLContent;
    SQLQueryL.Free;
    SQLQuery.Open;
    RefreshColumns;
  end;
end;

procedure TFormTable.DBGridColumnSized(Sender: TObject);
var
  i: integer;
  t: TTable;
begin
  t:= Metadata[TableId];
  for i:= 0 to High(Metadata[TableId].Columns) do
    t.Columns[i].width := DBGrid.Columns.Items[i].Width;
  Metadata[TableId] := t;
end;

procedure TFormTable.FormShow(Sender: TObject);
begin
  FilterApply;
end;

end.

