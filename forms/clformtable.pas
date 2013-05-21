unit CLFormTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Forms, DBGrids, sysutils,
  ExtCtrls, StdCtrls, Buttons, sqldb, db, Dialogs, CLFilter,
  CLDatabase, CLMetadata, CLFormChild, CLFormEdit, CLFormContainer;

type

  { TFormTable }

  TFormTable = class(TFormChild)
      ButtonFilterAdd: TButton;
    ButtonFilterCancel: TButton;
    ButtonFilterFind: TButton;
    ButtonAdd: TButton;
    ButtonEdit: TButton;
    ButtonRemove: TButton;
    Datasource: TDatasource;
    DBGrid: TDBGrid;
    PanelEdit: TPanel;
    SQLQuery: TSQLQuery;
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonEditClick(Sender: TObject);
    procedure ButtonFilterCancelClick(Sender: TObject);
    procedure ButtonFilterFindClick(Sender: TObject);
    procedure ButtonRemoveClick(Sender: TObject);
    procedure DBGridColumnSized(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonFilterAddClick(Sender: TObject);
    private
      Filter: TFilter;
      procedure RefreshColumns;
    public
      procedure RefreshSQLContent; override;
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

procedure TFormTable.ButtonFilterAddClick(Sender: TObject);
begin
  Filter.AddPanel;
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
  Filter := TFilter.Create(Self, Metadata[TableId].Columns, Self);
end;

procedure TFormTable.ButtonEditClick(Sender: TObject);
var Form: TFormEdit;
begin
  Form := TFormEdit.Create(Application, TableId, DBGrid.DataSource.DataSet.FieldByName('id').Value);
  FormContainer.AddForm(Form);
end;

procedure TFormTable.ButtonFilterCancelClick(Sender: TObject);
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text:=GetJoinedSQL(TableId, -1, -1, -1);
  SQLQuery.Open;
  if Sender <> Nil Then TButton(Sender).Visible:=false;
  RefreshColumns;
end;

procedure TFormTable.ButtonFilterFindClick(Sender: TObject);
var
  i: integer;
  s: string;
  state: TFilterState;
begin
  state := Filter.GetFilterState;
  if state.count = 0 Then begin
    ButtonFilterCancelClick(Nil);
    ButtonFilterCancel.Visible := false;
    exit;
  end;
  SQLQuery.Close;
  s:=GetJoinedSQL(TableId, -1, -1, -1)+' WHERE '; { TODO : Как-то тут всё странно выглядит }
  for i:= 0 to state.count-1 do begin
    if i > 0 Then s += ' AND ' else s+= '';
    if Metadata[TableId].Columns[state.field[i]].referenceTable <> '' Then
      s += Metadata[TableId].Columns[state.field[i]].referenceTable+'.name'
    else
      s += Metadata[TableId].name+'.'+Metadata[TableId].Columns[state.field[i]].name;
    s += ' '+Format(filter_operators[state.oper[i]], [':P'+intToStr(i)]);
  end;
  SQLQuery.SQL.Text := s;
  for i:= 0 to state.count-1 do begin
    s := filter_contentleft[state.oper[i]]+state.content[i]+filter_contentright[state.oper[i]];
    SQLQuery.Params.ParamByName('P'+intToStr(i)).AsString:= s;
  end;
  SQLQuery.Open;
  ButtonFilterCancel.Visible:=true;
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
    Transaction.Commit;
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
  ButtonFilterCancelClick(Nil);
end;

end.

