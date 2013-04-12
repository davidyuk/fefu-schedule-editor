unit CLFormTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  ExtCtrls, StdCtrls, sqldb, db;

type

  { TFormTable }

  TFormTable = class(TForm)
    ButtonCreate: TButton;
    ButtonEdit: TButton;
    ButtonRemove: TButton;
    Datasource: TDatasource;
    DBGrid: TDBGrid;
    PanelTools: TPanel;
    SQLQuery: TSQLQuery;
    procedure ButtonCreateClick(Sender: TObject);
    procedure ButtonEditClick(Sender: TObject);
    procedure ButtonRemoveClick(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    SQL: string;
    SQLTransaction: TSQLTransaction;
  public
    procedure LoadValues(NTableName: string; NSQLTransaction: TSQLTransaction);
    procedure LoadValues(NTitle, NSQL: string; NSQLTransaction: TSQLTransaction); overload;
  end;

implementation

{$R *.lfm}

{ TFormTable }

procedure TFormTable.ButtonCreateClick(Sender: TObject);
begin

end;

procedure TFormTable.ButtonEditClick(Sender: TObject);
begin

end;

procedure TFormTable.ButtonRemoveClick(Sender: TObject);
begin

end;


procedure TFormTable.FormChangeBounds(Sender: TObject);
begin
  BringToFront;
end;

procedure TFormTable.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Datasource.Enabled := False;
  SQLQuery.Close;
  SQLQuery.SQL.Clear;
end;

procedure TFormTable.FormShow(Sender: TObject);
begin
  Datasource.DataSet := SQLQuery;
  DBGrid.DataSource := Datasource;
  SQLQuery.Transaction := SQLTransaction;
  //assignFile(output, 'output.txt');
  //reWrite(output);
  //write(SQL);
  //closeFile(output);
  SQLQuery.SQL.Add(SQL);
  //SQLQuery.ExecSQL;
  //SQLTransaction.Commit;
  SQLQuery.Open;
  Datasource.Enabled := True;
end;

procedure TFormTable.LoadValues(NTableName: string; NSQLTransaction: TSQLTransaction);
begin
  Caption := 'Таблица: '+NTableName;
  SQLTransaction := NSQLTransaction;
  Left := Random(300);
  Top := Left;
  SQL := 'SELECT * FROM '+NTableName;
end;

procedure TFormTable.LoadValues(NTitle, NSQL: string;
  NSQLTransaction: TSQLTransaction);
begin
  Caption := NTitle;
  SQLTransaction := NSQLTransaction;
  Left := Random(300);
  Top := Left;
  SQL := NSQL;
end;

end.

