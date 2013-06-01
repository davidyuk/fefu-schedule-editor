unit CLFormEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, DbCtrls,
  ExtCtrls, StdCtrls, CLFormChild, sqldb, db, CLDatabase, CLMetadata,
  CLFormContainer, Graphics, Buttons;

type

  { TFormEdit }

  TFormEdit = class(TFormChild)
    ButtonSaveClose: TButton;
    ButtonCancel: TButton;
    Datasource: TDatasource;
    LabelError: TLabel;
    PanelButtons: TPanel;
    ScrollBox: TScrollBox;
    SQLQuery: TSQLQuery;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonSaveCloseClick(Sender: TObject);
    procedure DatasourceDataChange(Sender: TObject; Field: TField);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
  private
    FDefaultValues, FValues: TStringList;
    DBEdits: array of TDBEdit;
    DBComboBoxes: array of TDBLookupComboBox;
    FLockDefaultValues, FEdited: Boolean;
    procedure BuildContent;
    procedure UnLockField(Sender: TObject);
  public
    procedure LockDefaultValues;
    procedure RefreshSQLContent; override;
    procedure BeforeRefreshSQLContent; override;
    procedure SetDefaultValue(ColumnId: Integer; Value: String);
    constructor Create(TheOwner: TComponent; ATableId, ARecordId: integer); virtual;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TFormEdit }


procedure TFormEdit.ButtonSaveCloseClick(Sender: TObject);
var
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

  SQLQuery.Post;
  SQLQuery.ApplyUpdates;
  Close;
  FormContainer.BeforeRefreshSQLContent;
  Transaction.Commit;
  FormContainer.RefreshSQLContent;
end;

procedure TFormEdit.DatasourceDataChange(Sender: TObject; Field: TField);
begin
  FEdited:= true;
end;

procedure TFormEdit.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := not(SQLQuery.State in dsEditModes) or not FEdited or (MessageDlg(
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
  Button: TSpeedButton;
  Labell: TLabel;
  DBComboBox: TDBLookupComboBox;
  Query: TSQLQuery;
  DataS: TDataSource;
  Lock: Boolean;
begin
  for i:= 0 to High(DBEdits) do
    DBEdits[i].Parent.Free;
  SetLength(DBEdits, 0);
  for i:= 0 to High(DBComboBoxes) do
    DBComboBoxes[i].Parent.Free;
  SetLength(DBComboBoxes, 0);

  SQLQuery.Transaction := Transaction;
  SQLQuery.SQL.Text := GetSelectSQL(TableId, RecordId);
  if RecordId <> -1 Then SQLQuery.UpdateSQL.Text := GetUpdateSQL(TableId, RecordId)
  else SQLQuery.InsertSQL.Text := GetInsertSQL(TableId);
  SQLQuery.Open;
  if RecordId = -1 Then SQLQuery.Append
  else SQLQuery.Edit;
  Datasource.DataSet.FieldByName('id').Required := false;

  for i:= 0 to high(Metadata[TableId].Columns) do
    if FDefaultValues.Values[IntToStr(i)] <> '' Then
      Datasource.DataSet.Fields.Fields[i].AsString := FValues.Values[IntToStr(i)];
  for i:= high(Metadata[TableId].Columns) downto 0 do begin
    if (RecordId = -1) and (Metadata[TableId].Columns[i].name = 'id') Then continue;
    Lock := false;
    if FDefaultValues.Values[IntToStr(i)] <> '' Then begin
      Lock := (FValues.Values[IntToStr(i)]='')or(FDefaultValues.Values[IntToStr(i)]=FValues.Values[IntToStr(i)]);
      if Lock Then Datasource.DataSet.Fields.Fields[i].AsString := FDefaultValues.Values[IntToStr(i)];
    end;
    Panel := TPanel.Create(Self);
    Panel.Parent := ScrollBox;
    Panel.Align := alTop;
    Panel.BorderStyle:= bsNone;
    Panel.BorderWidth:= 0;
    Panel.AutoSize := true;
    Labell := TLabel.Create(Panel);
    Labell.Caption:= Metadata[TableId].Columns[i].display;
    Labell.Align:= alLeft;
    Labell.Parent := Panel;
    Labell.BorderSpacing.Around:=5;
    Labell.AutoSize:=false;
    Labell.Width:= 150;
    if Metadata[TableId].Columns[i].referenceTable = '' Then begin
      DBEdit := TDBEdit.Create(Panel);
      DBEdit.Align:= alClient;
      DBEdit.Parent := Panel;
      DBEdit.BorderSpacing.Around:=1;
      DBEdit.DataSource := Datasource;
      DBEdit.DataField := Metadata[TableId].Columns[i].name;
      If (DBEdit.DataField = 'id') or Lock Then DBEdit.Enabled := false;
      DBEdit.Tag:= PtrInt(Labell);
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
      DBComboBox.Tag:= PtrInt(Labell);
      SetLength(DBComboBoxes, Length(DBComboBoxes)+1);
      DBComboBoxes[High(DBComboBoxes)]:= DBComboBox;
    end;
    If Lock Then begin
      DBComboBox.Enabled := false;
      Button := TSpeedButton.Create(Self);
      Button.Align := alRight;
      Button.BorderSpacing.Around := 1;
      Button.Caption := ' Р ';
      Button.Hint:= 'Разблокировать';
      Button.ShowHint:=True;
      Button.Tag := PtrInt(DBComboBox);
      Button.OnClick := @UnLockField;
      Button.Parent := Panel;
      Button.AutoSize := True;
    end;
  end;
  FEdited:= false;
end;

procedure TFormEdit.UnLockField(Sender: TObject);
begin
  TControl(TComponent(Sender).Tag).Enabled := true;
  TControl(Sender).Visible := false;
end;

procedure TFormEdit.LockDefaultValues;
begin
  FLockDefaultValues:= true;
end;

procedure TFormEdit.RefreshSQLContent;
begin
  BuildContent;
end;

procedure TFormEdit.BeforeRefreshSQLContent;
var i: integer;
begin
  with Datasource.DataSet do
    for i:= 0 to FieldCount-1 do
      FValues.Values[IntToStr(i)]:= Fields.Fields[i].AsString;
end;

procedure TFormEdit.SetDefaultValue(ColumnId: Integer; Value: String);
begin
  FDefaultValues.Values[IntToStr(ColumnId)]:=Value;
end;

constructor TFormEdit.Create(TheOwner: TComponent; ATableId, ARecordId: integer);
begin
  inherited Create(TheOwner);
  FDefaultValues := TStringList.Create;
  FValues := TStringList.Create;
  FTableId := ATableId;
  FRecordId := ARecordId;
end;

destructor TFormEdit.Destroy;
begin
  inherited Destroy;
  FDefaultValues.Free;
  FValues.Free;
end;

procedure TFormEdit.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

end.

