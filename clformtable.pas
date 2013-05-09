unit CLFormTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Forms, DBGrids, sysutils,
  ExtCtrls, StdCtrls, Buttons, sqldb, db, Dialogs, CLFilter,
  CLDatabase, CLBooks, CLFormChild, CLFormEdit, CLFormContainer;

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
      procedure UpdateColumns;
      function GetJoinedSQL:string;
    public
      constructor Create(TheOwner: TComponent; ABookId: integer); virtual;
  end;

implementation

{$R *.lfm}

{ TFormTable }

procedure TFormTable.ButtonAddClick(Sender: TObject);
var Form: TFormEdit;
begin
  Form := TFormEdit.Create(Application, BookId, -1);
  FormContainer.AddForm(Form);
end;

procedure TFormTable.ButtonFilterAddClick(Sender: TObject);
begin
  Filter.AddPanel;
end;

procedure TFormTable.UpdateColumns;
var i: integer;
begin
  for i:= 0 to High(Books.Book[BookId].Columns) do begin
    DBGrid.Columns.Items[i].Width:=Books.Book[BookId].Columns[i].width;
    DBGrid.DataSource.DataSet.Fields[i].DisplayLabel:=Books.Book[BookId].Columns[i].disp;
  end;
end;

function TFormTable.GetJoinedSQL: string;
var
  firstPart: string;
  i: integer;
begin
with Books.Book[BookId] do begin
  firstPart := '';
  result := '';
  for i:= 0 to High(Columns) do begin
    if Columns[i].table = '' Then begin
      firstPart += ', '+table+'.'+Columns[i].name;
    end else begin
      firstPart += ', '+Columns[i].table+'.name';
      result+='INNER JOIN '+Columns[i].table+' ON '+table+'.'+Columns[i].name+' = '+Columns[i].table+'.id'+#13#10;
    end;
  end;
  result := Copy(firstPart, 3, length(firstPart))+' FROM '+table+#13#10+result;
end;
end;

constructor TFormTable.Create(TheOwner: TComponent; ABookId: integer);
begin
  inherited Create(TheOwner);
  FBookId:= ABookId;
  FRecordId:= -1;
  Datasource.DataSet := SQLQuery;
  DBGrid.DataSource := Datasource;
  SQLQuery.Transaction := Transaction;
  Datasource.Enabled := True;
  Caption:= Books.Book[BookId].name;
  Filter := TFilter.Create(Books.Book[BookId].Columns, Self);
end;

procedure TFormTable.ButtonEditClick(Sender: TObject);
var Form: TFormEdit;
begin
  Form := TFormEdit.Create(Application, BookId, DBGrid.SelectedIndex);
  FormContainer.AddForm(Form);
end;

procedure TFormTable.ButtonFilterCancelClick(Sender: TObject);
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text:='SELECT '+GetJoinedSQL;
  SQLQuery.Open;
  if Sender <> Nil Then TButton(Sender).Visible:=false;
  UpdateColumns;
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
  s:='SELECT '+GetJoinedSQL+' WHERE ';
  for i:= 0 to state.count-1 do begin
    if i > 0 Then s += ' AND ' else s+= '';
    if Books.Book[BookId].Columns[state.field[i]].table <> '' Then
      s += Books.Book[BookId].Columns[state.field[i]].table+'.name'
    else
      s += Books.Book[BookId].table+'.'+Books.Book[BookId].Columns[state.field[i]].name;
    s += ' '+Format(filter_operators[state.oper[i]], [':P'+intToStr(i)]);
  end;
  SQLQuery.SQL.Text := s;
  for i:= 0 to state.count-1 do begin
    s := filter_contentleft[state.oper[i]]+state.content[i]+filter_contentright[state.oper[i]];
    SQLQuery.Params.ParamByName('P'+intToStr(i)).AsString:= s;
  end;
  SQLQuery.Open;
  ButtonFilterCancel.Visible:=true;
  UpdateColumns;
end;


procedure TFormTable.ButtonRemoveClick(Sender: TObject);
var
  i: integer;
  s, id: string;
  SQLQueryL: TSQLQuery;
begin
  id := Datasource.DataSet.Fields.Fields[0].DisplayText;
  if id = '' Then exit;
  s:= '';
  for i:= 0 to Datasource.DataSet.Fields.Count-1 do
    s+= #13#10+DBGrid.DataSource.DataSet.Fields[i].DisplayLabel+': '+Datasource.DataSet.Fields.Fields[i].DisplayText;
  if MessageDlg('Подтверждение удаления записи', 'Вы действительно хотите удалить запись?'+
  s, mtWarning, mbOKCancel, 0) = mrOK Then begin
    SQLQueryL := TSQLQuery.Create(Nil);
    SQLQueryL.Transaction := Transaction;
    SQLQueryL.SQL.Text := 'DELETE from '+Books.Book[BookId].table+' WHERE id = '+id;
    SQLQueryL.ExecSQL;
    Transaction.Commit;
    SQLQueryL.Free;
    SQLQuery.Open;
    UpdateColumns;
  end;
end;

procedure TFormTable.DBGridColumnSized(Sender: TObject);
var i: integer;
begin
  for i:= 0 to High(Books.Book[BookId].Columns) do
    Books.Book[BookId].Columns[i].width := DBGrid.Columns.Items[i].Width;
end;

procedure TFormTable.FormShow(Sender: TObject);
begin
  ButtonFilterCancelClick(Nil);
end;

end.

