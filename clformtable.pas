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
    ButtonFind: TButton;
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
    procedure ButtonFindClick(Sender: TObject);
    procedure ButtonRemoveClick(Sender: TObject);
    procedure DBGridColumnSized(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ButtonFilterAddClick(Sender: TObject);
    private
      BookId: integer;
      Filter: TFilter;
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
  //ShowMessage(IntToStr(Datasource.DataSet.FieldCount));
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

procedure TFormTable.ButtonFindClick(Sender: TObject);
begin
  ShowMessage(Filter.GetSQL[0]);
end;


procedure TFormTable.ButtonRemoveClick(Sender: TObject);
begin
  //DBGrid.SelectedColumn.DesignIndex;
  //DBGrid.SelectedField.FieldNo ;
  //DBGrid.SelectedRows.Count;
  //if MessageDlg('Подтверждение удаления записи', 'Вы действительно хотите удалить "'+
  //intToStr(DBGrid.SelectedField.FieldNo)+'"?', mtWarning, mbOKCancel, 0) = mrOK Then
  //  SQLQuery.DeleteSQL.Text := 'DELETE * from '+Books.Table[BookId]+
  //  ' WHERE id = '+intToStr(DBGrid.SelectedIndex);
end;

procedure TFormTable.DBGridColumnSized(Sender: TObject);
var i: integer;
begin
  for i:= 0 to High(Books.Columns[BookId]) do
    Books.Columns[BookId, i]:= DBGrid.Columns.Items[i].Width;
end;

procedure TFormTable.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Datasource.Enabled := False;
  SQLQuery.Close;
  SQLQuery.SQL.Clear;
end;

procedure TFormTable.FormShow(Sender: TObject);
var
  i: integer;
  //sArr: array of string;
begin
  SQLQuery.SQL.Text:=Books.Query[BookId];
  Caption:= Books.Name[BookId];
  SQLQuery.Open;
  Datasource.Enabled := True;

  {setLength(sArr, Datasource.DataSet.Fields.Count);
  for i:= 0 to Datasource.DataSet.Fields.Count-1 do
    sArr[i] := Datasource.DataSet.Fields.Fields[i].FieldName;}
  Filter := TFilter.Create(Datasource.DataSet.Fields);
  Filter.AddPanel(ButtonFilterAdd.Parent.Parent);

  for i:= 0 to High(Books.Columns[BookId]) do
    DBGrid.Columns.Items[i].Width:=Books.Columns[BookId, i];
end;

end.

