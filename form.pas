unit Form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, ComCtrls, StdCtrls, DBGrids, Menus;

type

  { TMainForm }

  TMainForm = class(TForm)
    Datasource: TDatasource;
    DBGrid: TDBGrid;
    IBConnection: TIBConnection;
    ListTable: TListView;
    MainMenu: TMainMenu;
    MenuItemFile: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemAbout: TMenuItem;
    PanelTools: TPanel;
    PanelGrid: TPanel;
    SQLQuery: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    ToggleBoxConnect: TToggleBox;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure ListTableSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure ToggleBoxConnectChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.ListTableSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  SQLQuery.Close;
  SQLQuery.SQL.Clear;
  SQLQuery.SQL.Add('SELECT * FROM '+Item.Caption+';');
  SQLQuery.ExecSQL;
  SQLTransaction.Commit;
  SQLQuery.Open;
  DBGrid.Visible := True;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  ToggleBoxConnect.Checked := False;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  SQLTransaction.DataBase := IBConnection;
  Datasource.DataSet := SQLQuery;
  SQLQuery.Transaction := SQLTransaction;
  DBGrid.DataSource := Datasource;
  IBConnection.CharSet := 'UNICODE_FSS';
  IBConnection.UserName := 'sysdba';
  IBConnection.Password := 'masterkey';
  IBConnection.DatabaseName := 'DB.FDB';
end;

procedure TMainForm.MenuItemAboutClick(Sender: TObject);
begin
  ShowMessage('Клиент для работы с БД' + #13#10 +
    'Денис Давидюк Б8103А' + #13#10 + 'апрель 2013');
end;

procedure TMainForm.MenuItemExitClick(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TMainForm.ToggleBoxConnectChange(Sender: TObject);
var
  tables: TStringList;
  item: TListItem;
  i: integer;
begin
  If not(ToggleBoxConnect.Checked) Then begin
    SQLTransaction.Active := False;
    IBConnection.Connected := False;
    PanelGrid.Caption := 'Вы не подключены к БД';
    ToggleBoxConnect.Caption := 'Подключиться к БД';
    ListTable.Items.Clear;
    DBGrid.Visible := False;
    exit;
  end else begin
    IBConnection.Connected := True;
    SQLTransaction.Active := True;

    tables:= TStringList.Create;
    IBConnection.GetTableNames(tables);
    for i := 0 to tables.Count-1 do begin
      item := ListTable.Items.Add;
      item.Caption := tables.Strings[i];
    end;
    If tables.Count = 0 Then PanelGrid.Caption := 'БД не содержит таблиц'
    else PanelGrid.Caption := '';
    ToggleBoxConnect.Caption := 'Отключиться от БД';
  end;
end;

end.

