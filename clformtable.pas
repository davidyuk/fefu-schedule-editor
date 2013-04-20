unit CLFormTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Forms, DBGrids, sysutils,
  ExtCtrls, StdCtrls, Buttons, sqldb, db, Dialogs,
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
    procedure ButtonFilterRemoveClick(Sender: TObject);
    private
      filter: array of TPanel;
      bookId: integer;
  end;

implementation

{$R *.lfm}

{ TFormTable }

procedure TFormTable.ButtonAddClick(Sender: TObject);
begin
  FormContainer.AddForm(sGetParamFromKey(0)+';n-1', TFormEdit);
end;

procedure TFormTable.ButtonFilterAddClick(Sender: TObject);
var
  Panel: TPanel;
  SBRemove: TSpeedButton;
  CBKind, CBField: TComboBox;
  Edit: TEdit;
  i: Integer;
begin
  if length(filter)>20 Then exit;
  setLength(filter, length(filter)+1);
  Panel := TPanel.Create(TSpeedButton(Sender).Parent.Parent);
  Panel.Parent := TSpeedButton(Sender).Parent.Parent;
  Panel.BevelOuter:=bvNone;
  Panel.Align:=alBottom;
  //Panel.BorderSpacing.InnerBorder:=3;
  CBKind := TComboBox.Create(Panel); CBKind.Parent := Panel;
  CBKind.Align:=alLeft;
  CBKind.ReadOnly:=True;
  CBKind.BorderSpacing.Around := 3;
  CBKind.Items.Add('Больше');
  CBKind.Items.Add('Меньше');
  CBKind.Items.Add('Равно');
  CBKind.Items.Add('Начинается с');
  CBKind.Items.Add('Содержит');
  CBField := TComboBox.Create(Panel); CBField.Parent := Panel;
  CBField.Align:=alLeft;
  CBField.ReadOnly:=True;
  CBField.BorderSpacing.Around := 3;
  for i:= 0 to high(Books.TableFields[bookId]) do
    CBField.Items.Add(Books.TableFields[bookId, i]);
  Edit := TEdit.Create(Panel);
  Edit.Parent := Panel;
  Edit.Align:=alClient;
  Edit.BorderSpacing.Around := 3;
  Panel.Height := 29;
  SBRemove := TSpeedButton.Create(Panel); SBRemove.Parent := Panel;
  SBRemove.Caption:= 'X';
  SBRemove.Flat:=True;
  SBRemove.Align:= alRight;
  SBRemove.OnClick:= @ButtonFilterRemoveClick;
  SBRemove.BorderSpacing.Around := 3;

  filter[high(filter)] := Panel;

end;

procedure TFormTable.ButtonFilterRemoveClick(Sender: TObject);
var i: integer;
begin
  if length(filter) = 1 Then exit;
  for i:= 0 to high(filter) do
    if filter[i] = TSpeedButton(Sender).Parent Then begin
      freeAndNil(filter[i]);
      filter[i]:= filter[high(filter)];
    end;
  setLength(filter, length(filter)-1);
end;

procedure TFormTable.ButtonEditClick(Sender: TObject);
begin
  FormContainer.AddForm(sGetParamFromKey(0)+';n'+intToStr(DBGrid.SelectedIndex), TFormEdit);
end;

procedure TFormTable.ButtonFindClick(Sender: TObject);
begin

end;


procedure TFormTable.ButtonRemoveClick(Sender: TObject);
begin
  //DBGrid.SelectedColumn.DesignIndex;
  //DBGrid.SelectedField.FieldNo ;
  //DBGrid.SelectedRows.Count;
  //if MessageDlg('Подтверждение удаления записи', 'Вы действительно хотите удалить "'+
  //intToStr(DBGrid.SelectedField.FieldNo)+'"?', mtWarning, mbOKCancel, 0) = mrOK Then
  //  SQLQuery.DeleteSQL.Text := 'DELETE * from '+Books.Table[bookId]+
  //  ' WHERE id = '+intToStr(DBGrid.SelectedIndex);
end;

procedure TFormTable.DBGridColumnSized(Sender: TObject);
var i: integer;
begin
  for i:= 0 to High(Books.Columns[bookId]) do
    Books.Columns[bookId, i]:= DBGrid.Columns.Items[i].Width;
end;

procedure TFormTable.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Datasource.Enabled := False;
  SQLQuery.Close;
  SQLQuery.SQL.Clear;
end;

procedure TFormTable.FormShow(Sender: TObject);
var i: integer;
begin
  bookId := iGetParamFromKey(0);
  setLength(filter, 0);
  ButtonFilterAddClick(ButtonFilterAdd);
  Datasource.DataSet := SQLQuery;
  DBGrid.DataSource := Datasource;
  SQLQuery.Transaction := Database.Transaction;
  SQLQuery.SQL.Text:=Books.Query[bookId];
  Caption:= Books.Name[bookId];
  SQLQuery.Open;
  Datasource.Enabled := True;
  for i:= 0 to High(Books.Columns[bookId]) do
    DBGrid.Columns.Items[i].Width:=Books.Columns[bookId, i];
end;

end.

