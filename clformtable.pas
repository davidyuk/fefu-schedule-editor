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
    ButtonFilterCancel: TButton;
    ButtonFilterFind: TButton;
    ButtonAdd: TButton;
    ButtonEdit: TButton;
    ButtonRemove: TButton;
    Datasource: TDatasource;
    DBGrid: TDBGrid;
    PanelEdit: TPanel;
    ButtonFilterAdd: TSpeedButton;
    SQLQuery: TSQLQuery;
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonEditClick(Sender: TObject);
    procedure ButtonFilterCancelClick(Sender: TObject);
    procedure ButtonFilterFindClick(Sender: TObject);
    procedure ButtonRemoveClick(Sender: TObject);
    procedure DBGridColumnSized(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ButtonFilterAddClick(Sender: TObject);
    private
      BookId: integer;
      Filter: TFilter;
      procedure UpdateColumns;
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
  Filter.AddPanel(TSpeedButton(Sender).Parent.Parent);
end;

procedure TFormTable.UpdateColumns;
var i: integer;
begin
  for i:= 0 to High(Books.Book[BookId].Columns) do begin
    DBGrid.Columns.Items[i].Width:=Books.Book[BookId].Columns[i].width;
    DBGrid.DataSource.DataSet.Fields[i].DisplayLabel:=Books.Book[BookId].Columns[i].disp;
    //DBGrid.Columns[i].DisplayFormat:=Books.Book[BookId].Columns[i].disp;
    //DBGrid.Columns[i].DisplayName:=Books.Book[BookId].Columns[i].disp;
  end;
end;

constructor TFormTable.Create(TheOwner: TComponent; ABookId: integer);
begin
  inherited Create(TheOwner);
  BookId:= ABookId;
  FKey:= 'book_id: '+IntToStr(BookId);

  Datasource.DataSet := SQLQuery;
  DBGrid.DataSource := Datasource;
  SQLQuery.Transaction := Database.Transaction;
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
  SQLQuery.SQL.Text:='SELECT '+Books.Book[BookId].sql;
  SQLQuery.Open;
  if Sender <> Nil Then TButton(Sender).Visible:=false;
  UpdateColumns;
end;

procedure TFormTable.ButtonFilterFindClick(Sender: TObject);
var
  i: integer;
begin
  if Filter.GetSQL[0] = '' Then exit;
  SQLQuery.Close;
  SQLQuery.SQL.Text:='SELECT '+Books.Book[BookId].sql+' WHERE '+Filter.GetSQL[0];
  //ShowMessage(SQLQuery.SQL.Text);
  for i:= 1 to high(Filter.GetSQL) do begin
    SQLQuery.Params.ParamByName('P'+intToStr(i-1)).AsString:=Filter.GetSQL[i];
    //ShowMessage(Filter.GetSQL[i]+':len:'+intToStr(length(Filter.GetSQL[i])));
    //ShowMessage(SQLQuery.Params.ParamByName('P'+intToStr(i-1)).AsString);
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
    SQLQueryL.Transaction := Database.Transaction;
    SQLQueryL.SQL.Text := 'DELETE from '+Books.Book[BookId].table+' WHERE id = '+id;
    SQLQueryL.ExecSQL;
    Database.Transaction.Commit;
    FreeAndNil(SQLQueryL);
    //ShowMessage(SQLQuery.SQL.Text);
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

procedure TFormTable.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SQLQuery.Close;
end;

procedure TFormTable.FormShow(Sender: TObject);
begin
  SQLQuery.SQL.Text:='SELECT '+Books.Book[BookId].sql;
  SQLQuery.Open;
  Datasource.Enabled := True;
  Caption:= Books.Book[BookId].name;

  Filter := TFilter.Create(Books.Book[BookId].Columns);
  Filter.AddPanel(ButtonFilterAdd.Parent.Parent);

  ButtonFilterCancelClick(Nil);
end;

end.

