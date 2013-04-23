unit CLFormTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Forms, DBGrids, sysutils,
  ExtCtrls, StdCtrls, Buttons, sqldb, db, Dialogs,
  CLDatabase, CLBooks, CLFormChild, CLFormEdit, CLFormContainer;

type

  FilterPanel = Record
    Panel: TPanel;
    CBKind, CBField: TComboBox;
    Edit: TEdit;
  end;

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
    const
      FilterTypesName: array[0..4] of string = ('Больше', 'Меньше', 'Равно', 'Начинается с', 'Содержит');
      FilterTypesValues: array[0..4] of string = ('> ":filter"', '< ":filter"', '= ":filter"', 'LIKE ":filter%"', 'LIKE "%:filter%"');
    private
      FilterPanels: array of FilterPanel;
      FilterFieldsName: array of string;
      BookId: integer;
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
var
  Panel: TPanel;
  SBRemove: TSpeedButton;
  CBKind, CBField: TComboBox;
  Edit: TEdit;
  i, n: Integer;
begin
  if length(FilterPanels)>20 Then exit;
  setLength(FilterPanels, length(FilterPanels)+1);
  n:= High(FilterPanels);

  Panel := TPanel.Create(TSpeedButton(Sender).Parent.Parent);
  Panel.Parent := TSpeedButton(Sender).Parent.Parent;
  Panel.BevelOuter:=bvNone;
  Panel.Align:=alBottom;
  Panel.Height := 29;
  FilterPanels[n].Panel := Panel;
  //Panel.BorderSpacing.InnerBorder:=3; не работает:(

  CBKind := TComboBox.Create(Panel);
  CBKind.Parent := Panel;
  CBKind.Align:=alLeft;
  CBKind.ReadOnly:=True;
  CBKind.BorderSpacing.Around := 3;
  for i:= 0 to high(FilterTypesValues) do
     CBKind.Items.Add(FilterTypesName[i]);
  FilterPanels[n].CBKind := CBKind;


  CBField := TComboBox.Create(Panel);
  CBField.Parent := Panel;
  CBField.Align:=alLeft;
  CBField.ReadOnly:=True;
  CBField.BorderSpacing.Around := 3;
  CBField.Width := 120;
  for i:= 0 to high(FilterFieldsName) do
    CBField.Items.Add(FilterFieldsName[i]);
  FilterPanels[n].CBField := CBField;

  Edit := TEdit.Create(Panel);
  Edit.Parent := Panel;
  Edit.Align:=alClient;
  Edit.BorderSpacing.Around := 3;
  FilterPanels[n].Edit := Edit;

  SBRemove := TSpeedButton.Create(Panel); SBRemove.Parent := Panel;
  SBRemove.Caption:= 'X';
  SBRemove.Flat:=True;
  SBRemove.Align:= alRight;
  SBRemove.OnClick:= @ButtonFilterRemoveClick;
  SBRemove.BorderSpacing.Around := 3;
end;

procedure TFormTable.ButtonFilterRemoveClick(Sender: TObject);
var i: integer;
begin
  if length(FilterPanels) = 1 Then exit;
  for i:= 0 to high(FilterPanels) do
    if FilterPanels[i].Panel = TSpeedButton(Sender).Parent Then begin
      freeAndNil(FilterPanels[i].Panel);
      FilterPanels[i]:= FilterPanels[high(FilterPanels)];
    end;
  setLength(FilterPanels, length(FilterPanels)-1);
end;

constructor TFormTable.Create(TheOwner: TComponent; ABookId: integer);
begin
  inherited Create(TheOwner);
  BookId:= ABookId;
  FKey:= 'book_id: '+IntToStr(BookId);
  setLength(FilterPanels, 0);
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
var
  i: integer;
  s: string;
begin
  s := '';
  for i:= 0 to High(FilterPanels) do with FilterPanels[i] do begin
    if (CBField.ItemIndex = -1) or (CBKind.ItemIndex = -1) or (Edit.Text = '') Then continue;
    s += FilterFieldsName[CBField.ItemIndex]+' ';
    s += FilterTypesValues[CBKind.ItemIndex]+' ';
  end;
  ShowMessage(s);
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
var i: integer;
begin
  SQLQuery.SQL.Text:=Books.Query[BookId];
  Caption:= Books.Name[BookId];
  SQLQuery.Open;
  Datasource.Enabled := True;
  setLength(FilterFieldsName, Datasource.DataSet.Fields.Count);
  for i:= 0 to Datasource.DataSet.Fields.Count-1 do
    FilterFieldsName[i] := Datasource.DataSet.Fields.Fields[i].FieldName;
  if length(FilterPanels) = 0 Then ButtonFilterAddClick(ButtonFilterAdd);
  for i:= 0 to High(Books.Columns[BookId]) do
    DBGrid.Columns.Items[i].Width:=Books.Columns[BookId, i];
end;

end.

