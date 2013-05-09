unit CLFormEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DbCtrls,
  ExtCtrls, StdCtrls, DBGrids, CLFormChild, sqldb, db, CLDatabase, CLBooks;

type

  { TFormEdit }

  TFormEdit = class(TFormChild)
    ButtonSave: TButton;
    ButtonCancel: TButton;
    Datasource: TDatasource;
    PanelButtons: TPanel;
    ScrollBox: TScrollBox;
    SQLQuery: TSQLQuery;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    function GetSQL: string;
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
      DBEdit.DataSource := Datasource;
      DBEdit.DataField := Books.Book[BookId].Columns[i].name;
      DBEdit.BorderSpacing.Around:=1;
    end else begin
      ComboBox := TDBLookupComboBox.Create(Self);
      DataS := TDataSource.Create(Self);
      Query := TSQLQuery.Create(Self);
      DataS.DataSet := Query;
      Query.Transaction := Transaction;
      Query.SQL.Text:='SELECT ID, NAME FROM '+Books.Book[BookId].Columns[i].table;
      Query.Open;
      ComboBox.ListSource := DataS;
      ComboBox.ListField := 'NAME';
      ComboBox.KeyField := 'ID';
      ComboBox.DataSource := Datasource;
      ComboBox.DataField := Books.Book[BookId].Columns[i].name;
      //ComboBox.ItemIndex:= 2;
      ComboBox.Align:= alClient;
      ComboBox.Parent := Panel;
      ComboBox.ReadOnly := true;
      ComboBox.BorderSpacing.Around:=1;
      ComboBox.BorderSpacing.Right:= 3;
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

constructor TFormEdit.Create(TheOwner: TComponent; ABookId, ARecordId: integer);
begin
  inherited Create(TheOwner);
  FBookId := ABookId;
  FRecordId := ARecordId;
  Datasource.DataSet := SQLQuery;
  Datasource.Enabled := True;
  SQLQuery.Transaction := Transaction;
  //SQLQuery.UpdateSQL.Text := GetSQL;
  SQLQuery.SQL.Text := 'SELECT '+GetSQL;
  SQLQuery.Open;
  SQLQuery.Edit;
end;

procedure TFormEdit.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

end.

