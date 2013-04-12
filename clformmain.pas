unit CLFormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, FileUtil, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, ComCtrls, StdCtrls, Menus, CLAddParts;

type

  { TFormMain }

  TFormMain = class(TForm)
    IBConnection: TIBConnection;
    MainMenu: TMainMenu;
    MenuItemFile: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemAbout: TMenuItem;
    PanelForms: TPanel;
    PanelTools: TPanel;
    Splitter: TSplitter;
    SQLTransaction: TSQLTransaction;
    StatusBar: TStatusBar;
    ToggleBoxConnect: TToggleBox;
    TreeView: TTreeView;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure PanelFormsResize(Sender: TObject);
    procedure ToggleBoxConnectChange(Sender: TObject);
    procedure TreeViewSelectionChanged(Sender: TObject);
  private
    TreeRootTable, TreeRootQuery: TTreeNode;
    SQLQueryName, SQLQueryContent: array of string;
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;
  FormContainer: TFormContainer;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  ToggleBoxConnect.Checked := False;
end;

procedure TFormMain.FormShow(Sender: TObject);
var
  s: string;
begin
  assignFile(input, 'query.sql');
  reset(input);
  while not EOF(input) do begin
    setLength(SQLQueryName, length(SQLQueryName)+1);
    setLength(SQLQueryContent, length(SQLQueryContent)+1);
    readLn(s);
    SQLQueryName[high(SQLQueryName)] :=AnsiToUtf8(Copy(s, 3, length(s)-4));
    while s <> ';' do begin
      readLn(s);
      SQLQueryContent[high(SQLQueryContent)]+=AnsiToUtf8(s);
    end;
  end;
  closeFile(input);
  FormContainer := TFormContainer.Create();
  SQLTransaction.DataBase := IBConnection;
  IBConnection.CharSet := 'UNICODE_FSS';
  //без этой строки русские буквы не отображаются в Grid
  IBConnection.UserName := 'sysdba';
  IBConnection.Password := 'masterkey';
  IBConnection.DatabaseName := 'DB.FDB';
end;

procedure TFormMain.MenuItemAboutClick(Sender: TObject);
begin
  ShowMessage('Клиент для работы с БД' + #13#10 +
    'Денис Давидюк Б8103А' + #13#10 + 'апрель 2013');
end;

procedure TFormMain.MenuItemExitClick(Sender: TObject);
begin
  FormMain.Close;
end;

procedure TFormMain.PanelFormsResize(Sender: TObject);
begin
  If FormContainer <> Nil Then FormContainer.UpdateSize;
end;

procedure TFormMain.ToggleBoxConnectChange(Sender: TObject);
var
  tables: TStringList;
  i: integer;
  t: TTreeNode;
begin
  If not(ToggleBoxConnect.Checked) Then begin
    TreeView.Enabled := False;
    TreeView.Items.Clear;
    SQLTransaction.Active := False;
    IBConnection.Connected := False;
    StatusBar.Panels.Items[0].Text := 'Нет подключения';
    ToggleBoxConnect.Caption := 'Подключиться к БД';
    FormContainer.Clear;
  end else begin
    IBConnection.Connected := True;
    SQLTransaction.Active := True;

    tables:= TStringList.Create;
    IBConnection.GetTableNames(tables);
    TreeView.Enabled := True;
    TreeRootTable := TreeView.Items.Add(nil, 'Таблицы');
    TreeRootQuery := TreeView.Items.Add(nil, 'Запросы');
    for i := 0 to tables.Count-1 do
      TreeView.Items.AddChild(TreeRootTable, tables.Strings[i]);
    for i:= 0 to high(SQLQueryContent) do begin
      t := TreeView.Items.AddChild(TreeRootQuery, SQLQueryName[i]);
      t.Data := Pointer(i);
    end;

    If tables.Count = 0 Then StatusBar.Panels.Items[0].Text := 'Таблицы отсутствуют'
    else StatusBar.Panels.Items[0].Text := 'Подключено';
    ToggleBoxConnect.Caption := 'Отключиться от БД';
  end;
end;

procedure TFormMain.TreeViewSelectionChanged(Sender: TObject);
begin
  if TreeView.Enabled and (TreeView.Selected.Parent = TreeRootTable) Then
    FormContainer.AddFormTable(TreeView.Selected.Text, SQLTransaction, PanelForms);
  if TreeView.Enabled and (TreeView.Selected.Parent = TreeRootQuery) Then
    FormContainer.AddFormQuery(TreeView.Selected.Text, SQLQueryContent[Integer(TreeView.Selected.Data)], SQLTransaction, PanelForms);
end;

end.

