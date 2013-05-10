unit CLFormEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DbCtrls,
  ExtCtrls, StdCtrls, DBGrids, CLFormChild, sqldb, db, CLDatabase, CLBooks;

type

  { TFormEdit }

  TFormEdit = class(TFormChild)
    ButtonSaveClose: TButton;
    ButtonSave: TButton;
    ButtonCancel: TButton;
    Datasource: TDatasource;
    DBGrid1: TDBGrid;
    PanelButtons: TPanel;
    ScrollBox: TScrollBox;
    SQLQuery: TSQLQuery;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonSaveCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    function GetSQL: string;
    function GetUpdateSQL: string;
    function GetInsertSQL: string;
  public
    constructor Create(TheOwner: TComponent; ABookId, ARecordId: integer); virtual;
  end;

implementation

{$R *.lfm}

{ TFormEdit }


procedure TFormEdit.ButtonSaveClick(Sender: TObject);
begin
  SQLQuery.Post;
  SQLQuery.ApplyUpdates;
  Transaction.Commit;
end;

procedure TFormEdit.ButtonSaveCloseClick(Sender: TObject);
begin
  if SQLQuery.State = dsEdit Then
    ButtonSaveClick(Sender);
  Close;
end;

procedure TFormEdit.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //CloseAction:= caFree;
end;

procedure TFormEdit.FormShow(Sender: TObject);
var
  i: integer;
  Panel: TPanel;
  DBEdit: TDBEdit;
  Labell: TLabel;
  ComboBox: TDBLookupComboBox;
  Query: TSQLQuery;
  DataS: TDataSource;
begin
  if RecordId = -1 Then begin
    DataS := TDataSource.Create(Self);
    Query := TSQLQuery.Create(Self);
    DataS.DataSet := Query;
    Query.Transaction := Transaction;
    Query.SQL.Text:='SELECT GEN_ID('+Books.Book[BookId].table+', 1) FROM RDB$DATABASE';
    Query.Open;
    Datasource.DataSet.Fields.FieldByName('id').Value:=DataS.DataSet.Fields.Fields[0].Value;
  end;
  for i:= high(Books.Book[BookId].Columns) downto 0 do begin
    Panel := TPanel.Create(Self);
    Panel.Parent := ScrollBox;
    Panel.Align := alTop;
    Panel.BorderStyle:= bsNone;
    Panel.BorderWidth:= 0;
    Panel.Height:= 29;
    Labell := TLabel.Create(Self);
    Labell.Caption:= Books.Book[BookId].Columns[i].disp;
    Labell.Align:= alLeft;
    Labell.Parent := Panel;
    Labell.BorderSpacing.Around:=6;
    Labell.AutoSize:=false;
    Labell.Width:= 150;
    if Books.Book[BookId].Columns[i].table = '' Then begin
      DBEdit := TDBEdit.Create(Self);
      DBEdit.Align:= alClient;
      DBEdit.Parent := Panel;
      DBEdit.BorderSpacing.Around:=1;
      DBEdit.DataSource := Datasource;
      DBEdit.DataField := Books.Book[BookId].Columns[i].name;
    end else begin
      DataS := TDataSource.Create(Self);
      Query := TSQLQuery.Create(Self);
      DataS.DataSet := Query;
      Query.Transaction := Transaction;
      Query.SQL.Text:='SELECT ID, NAME FROM '+Books.Book[BookId].Columns[i].table;
      Query.Open;
      ComboBox := TDBLookupComboBox.Create(Self);
      ComboBox.Align:= alClient;
      ComboBox.Parent := Panel;
      ComboBox.BorderSpacing.Around:=1;
      ComboBox.DataSource := Datasource;
      ComboBox.DataField := Books.Book[BookId].Columns[i].name;
      ComboBox.ListSource := DataS;
      ComboBox.ListField := 'NAME';
      ComboBox.KeyField := 'ID';
    end;
  end;
end;

function TFormEdit.GetSQL: string;
var
  firstPart: string;
  i: integer;
begin
with Books.Book[BookId] do begin
  firstPart := '';
  result := '';
  for i:= 0 to High(Columns) do
    firstPart += ', '+table+'.'+Columns[i].name;
  result := Copy(firstPart, 3, length(firstPart))+' FROM '+table+' WHERE id = '+intToStr(RecordId);
end;
end;

function TFormEdit.GetUpdateSQL: string;
var i: integer;
begin
with Books.Book[BookId] do begin
  result := '';
  for i:= 0 to High(Columns) do
    result += ', '+Columns[i].name+'=:'+Columns[i].name;
  result := 'UPDATE '+table+' SET '+Copy(result, 3, length(result))+' WHERE id = '+intToStr(RecordId);
end;
end;

function TFormEdit.GetInsertSQL: string;
var i: integer; s1, s2: string;
begin
with Books.Book[BookId] do begin
  result := ''; s1:= ''; s2:= '';
  for i:= 0 to High(Columns) do begin
    s1 += ', '+Columns[i].name;
    s2 += ', :'+Columns[i].name;
  end;
  result := 'INSERT INTO '+table+' ('+Copy(s1, 3, length(s1))+') VALUES ('
    +Copy(s2, 3, length(s2))+')';
end;
end;

constructor TFormEdit.Create(TheOwner: TComponent; ABookId, ARecordId: integer);
begin
  inherited Create(TheOwner);
  FBookId := ABookId;
  FRecordId := ARecordId;
  Datasource.DataSet := SQLQuery;
  Datasource.Enabled := True;
  SQLQuery.Transaction := Transaction;
    SQLQuery.SQL.Text := 'SELECT '+GetSQL;
  if RecordId <> -1 Then
    SQLQuery.UpdateSQL.Text := GetUpdateSQL
  else
    SQLQuery.InsertSQL.Text := GetInsertSQL;
  SQLQuery.Open;
  SQLQuery.Edit;
end;

procedure TFormEdit.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

end.

