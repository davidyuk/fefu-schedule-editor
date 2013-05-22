unit CLFormEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, DbCtrls,
  ExtCtrls, StdCtrls, CLFormChild, sqldb, db, CLDatabase, CLMetadata,
  CLFormContainer, Graphics, DBGrids;

type

  { TFormEdit }

  TFormEdit = class(TFormChild)
    ButtonSaveClose: TButton;
    ButtonCancel: TButton;
    Datasource: TDatasource;
    DBGrid1: TDBGrid;
    LabelError: TLabel;
    PanelButtons: TPanel;
    ScrollBox: TScrollBox;
    SQLQuery: TSQLQuery;
    FDefaultValues: TStringList;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonSaveCloseClick(Sender: TObject);
    procedure DatasourceDataChange(Sender: TObject; Field: TField);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
  private
    DBEdits: array of TDBEdit;
    FEdited: Boolean;
    DBComboBoxes: array of TDBLookupComboBox;
    procedure BuildContent;
  public
    procedure RefreshSQLContent; override;
    procedure SetDefaultValue(ColumnId: Integer; Value: String);
    constructor Create(TheOwner: TComponent; ABookId, ARecordId: integer); virtual;
  end;

implementation

{$R *.lfm}

{ TFormEdit }


procedure TFormEdit.ButtonSaveCloseClick(Sender: TObject);
var
  Query: TSQLQuery;
  DataS: TDataSource;
  f: boolean;
  i: integer;
begin
  f:= true;
  for i:= 0  to High(DBEdits) do
    if DBEdits[i].Text = '' Then begin
      f:= false;
      TLabel(DBEdits[i].Tag).Font.Color := clRed;
    end else
      TLabel(DBEdits[i].Tag).Font.Color := clBlack;
  for i:= 0  to High(DBComboBoxes) do
    if DBComboBoxes[i].ItemIndex = -1 Then begin
      f:= false;
      TLabel(DBComboBoxes[i].Tag).Font.Color := $0000ff;
    end else
      TLabel(DBComboBoxes[i].Tag).Font.Color := $000000;
  if not f Then begin
    LabelError.Visible := True;
    exit;
  end else
    LabelError.Visible := False;
  if RecordId = -1 Then begin
    DataS := TDataSource.Create(Self);
    Query := TSQLQuery.Create(Self);
    DataS.DataSet := Query;
    Query.Transaction := Transaction;
    Query.SQL.Text:='SELECT GEN_ID('+Metadata[TableId].name+', 1) FROM RDB$DATABASE';
    Query.Open;
    Datasource.DataSet.FieldByName('id').Value:=DataS.DataSet.Fields.Fields[0].Value;
  end; { TODO: Нужно научиться задавать ID триггером на стороне СУБД }
  SQLQuery.Post;
  SQLQuery.ApplyUpdates;
  Transaction.Commit;
  FormContainer.RefreshSQLContent;
  Close;
end;

procedure TFormEdit.DatasourceDataChange(Sender: TObject; Field: TField);
begin
  FEdited := true;
end;

procedure TFormEdit.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := (SQLQuery.State <> dsEdit) and (SQLQuery.State <> dsInsert);
  CanClose := CanClose or not FEdited;
  CanClose := CanClose or (MessageDlg(
      'Подтверждение', 'Данные не сохранены. Вы действительно хотите отбросить изменения?',
      mtWarning, mbOKCancel, 0) = mrOK);
  if CanClose Then SQLQuery.Cancel;
end;

procedure TFormEdit.FormShow(Sender: TObject);
begin
  BuildContent;
end;

procedure TFormEdit.BuildContent;
var
  i: integer;
  Panel: TPanel;
  DBEdit: TDBEdit;
  Labell: TLabel;
  DBComboBox: TDBLookupComboBox;
  Query: TSQLQuery;
  DataS: TDataSource;
begin
  for i:= 0 to High(DBEdits) do
    DBEdits[i].Parent.Free;
  SetLength(DBEdits, 0);
  for i:= 0 to High(DBComboBoxes) do
    DBComboBoxes[i].Parent.Free;
  SetLength(DBComboBoxes, 0);
  for i:= 0 to high(Metadata[TableId].Columns) do
    if (RecordId = -1) and (FDefaultValues.Values[IntToStr(i)] <> '') Then
        Datasource.DataSet.Fields.Fields[i].AsString := FDefaultValues.Values[IntToStr(i)];
  for i:= high(Metadata[TableId].Columns) downto 0 do begin
    if (RecordId = -1) and (Metadata[TableId].Columns[i].name = 'id') Then continue;
    Panel := TPanel.Create(Self);
    Panel.Parent := ScrollBox;
    Panel.Align := alTop;
    Panel.BorderStyle:= bsNone;
    Panel.BorderWidth:= 0;
    Panel.Height:= 29;
    Labell := TLabel.Create(Panel);
    Labell.Caption:= Metadata[TableId].Columns[i].display;
    Labell.Align:= alLeft;
    Labell.Parent := Panel;
    Labell.BorderSpacing.Around:=6;
    Labell.AutoSize:=false;
    Labell.Width:= 150;
    if Metadata[TableId].Columns[i].referenceTable = '' Then begin
      DBEdit := TDBEdit.Create(Panel);
      DBEdit.Align:= alClient;
      DBEdit.Parent := Panel;
      DBEdit.BorderSpacing.Around:=1;
      DBEdit.DataSource := Datasource;
      DBEdit.DataField := Metadata[TableId].Columns[i].name;
      If DBEdit.DataField = 'id' Then DBEdit.Enabled := false;
      DBEdit.Tag:= IntPtr(Labell);
      SetLength(DBEdits, length(DBEdits)+1);
      DBEdits[High(DBEdits)] := DBEdit;
    end else begin
      DataS := TDataSource.Create(Self);
      Query := TSQLQuery.Create(Self);
      DataS.DataSet := Query;
      Query.Transaction := Transaction;
      Query.SQL.Text:='SELECT ID, NAME FROM '+Metadata[TableId].Columns[i].referenceTable;
      Query.Open;
      DBComboBox := TDBLookupComboBox.Create(Panel);
      DBComboBox.Align:= alClient;
      DBComboBox.Parent := Panel;
      DBComboBox.BorderSpacing.Around:=1;
      DBComboBox.DataSource := Datasource;
      DBComboBox.DataField := Metadata[TableId].Columns[i].name;
      DBComboBox.ListSource := DataS;
      DBComboBox.ListField := 'NAME';
      DBComboBox.KeyField := 'ID';
      DBComboBox.Tag:= IntPtr(Labell);
      SetLength(DBComboBoxes, Length(DBComboBoxes)+1);
      DBComboBoxes[High(DBComboBoxes)]:= DBComboBox;
    end;
  end;
  FEdited := False;
  FreeAndNil(FDefaultValues);
end;

procedure TFormEdit.RefreshSQLContent;
begin
  //BuildContent;
  { TODO : Нужно только обновить TDBLookupComboBox'ы (беда) }
end;

procedure TFormEdit.SetDefaultValue(ColumnId: Integer; Value: String);
begin
  FDefaultValues.Values[IntToStr(ColumnId)]:=Value;
end;

constructor TFormEdit.Create(TheOwner: TComponent; ABookId, ARecordId: integer);
begin
  inherited Create(TheOwner);
  FDefaultValues := TStringList.Create;
  FTableId := ABookId;
  FRecordId := ARecordId;
  SQLQuery.Transaction := Transaction;
  SQLQuery.SQL.Text := GetSelectSQL(TableId, RecordId);
  if RecordId <> -1 Then SQLQuery.UpdateSQL.Text := GetUpdateSQL(TableId, RecordId)
  else SQLQuery.InsertSQL.Text := GetInsertSQL(TableId);
  SQLQuery.Open;
  if RecordId = -1 Then SQLQuery.Append
  else SQLQuery.Edit;
end;

procedure TFormEdit.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

end.

