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
    DBGrid: TDBGrid;
    Panel: TPanel;
    ScrollBox: TScrollBox;
    SQLQuery: TSQLQuery;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent; ABookId, ARecordId: integer); virtual;
  end;

implementation

{$R *.lfm}

{ TFormEdit }


procedure TFormEdit.ButtonSaveClick(Sender: TObject);
begin

end;

procedure TFormEdit.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //CloseAction:= caFree;
end;

procedure TFormEdit.FormShow(Sender: TObject);
var
  i: integer;
  p: TPanel;
  e: TDBEdit;
  l: TLabel;
  c: TDBLookupComboBox;
begin
  for i:= 0 to high(Books.Book[BookId].Columns) do begin
    p := TPanel.Create(Self);
    p.Parent := ScrollBox;
    p.Align := alTop;
    p.BorderStyle:= bsNone;
    p.BorderWidth:= 0;
    p.Height:= 29;
    l := TLabel.Create(Self);
    l.Caption:= Books.Book[BookId].Columns[i].disp;
    l.Align:= alLeft;
    l.Parent := p;
    l.BorderSpacing.Around:=6;
    l.AutoSize:=false;
    l.Width:= 150;
    if Books.Book[BookId].Columns[i].table = '' Then begin
      e := TDBEdit.Create(Self);
      e.Align:= alClient;
      e.Parent := p;
      e.BorderSpacing.Around:=1;
      e.BorderSpacing.Right:= 3;
    end else begin
      c := TDBLookupComboBox.Create(Self);
      SQLQuery.Close;
      SQLQuery.SQL.Text:='SELECT * FROM '+Books.Book[BookId].Columns[i].table;
      ShowMessage(SQLQuery.SQL.Text);
      c.ListSource := SQLQuery.DataSource;
      c.DataSource := SQLQuery.DataSource;
      SQLQuery.Open;
      c.KeyValue:='Б8103а';
      //c.ListField:= 'name';
      //c.ListFieldIndex:= 1;
      c.Align:= alClient;
      c.Parent := p;
      c.ReadOnly := true;
      c.BorderSpacing.Around:=1;
      c.BorderSpacing.Right:= 3;
    end;
  end;
end;

constructor TFormEdit.Create(TheOwner: TComponent; ABookId, ARecordId: integer);
begin
  inherited Create(TheOwner);
  FBookId := ABookId;
  FRecordId := ARecordId;
  Datasource.DataSet := SQLQuery;
  DBGrid.DataSource := Datasource;
  SQLQuery.Transaction := Database.Transaction;
  Datasource.Enabled := True;
end;

procedure TFormEdit.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

end.

