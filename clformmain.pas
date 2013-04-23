unit CLFormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, ExtCtrls, ComCtrls, StdCtrls,
  Menus, CLDatabase, CLBooks, CLFormContainer, CLFormTable;

type

  { TFormMain }

  TFormMain = class(TForm)
    MainMenu: TMainMenu;
    MenuItemFile: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemAbout: TMenuItem;
    PanelForms: TPanel;
    PanelTools: TPanel;
    Splitter: TSplitter;
    StatusBar: TStatusBar;
    ToggleBoxConnect: TToggleBox;
    TreeView: TTreeView;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure ToggleBoxConnectChange(Sender: TObject);
    procedure TreeViewClick(Sender: TObject);
  private
    TreeRootQuery: TTreeNode;
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormShow(Sender: TObject);
begin
  FormContainer := TFormContainer.Create(FormMain);
end;

procedure TFormMain.ToggleBoxConnectChange(Sender: TObject);
var
  i: integer;
  t: TTreeNode;
begin
  If ToggleBoxConnect.Checked Then begin
    Database.Connected := true;
    TreeView.Enabled := True;
    TreeRootQuery := TreeView.Items.Add(nil, 'Справочники');
    for i:= 0 to high(Books.Name) do begin
      t := TreeView.Items.AddChild(TreeRootQuery, Books.Name[i]);
      t.Data := Pointer(i); //запоминаем номер справочника
    end;
    StatusBar.Panels.Items[0].Text := 'Подключено';
    ToggleBoxConnect.Caption := 'Отключиться от БД';
  end else begin
    Database.Connected := false;
    TreeView.Enabled := False;
    TreeView.Items.Clear;
    StatusBar.Panels.Items[0].Text := 'Нет подключения';
    ToggleBoxConnect.Caption := 'Подключиться к БД';
    FormContainer.Clear;
  end;
end;

procedure TFormMain.TreeViewClick(Sender: TObject);
var
  Point: TPoint;
  Form: TFormTable;
begin
  Point := ScreenToClient(Mouse.CursorPos);
  Point.X -= TreeView.Left;
  Point.Y -= TreeView.Top;
  if TreeView.GetNodeAt(Point.X, Point.Y) = Nil Then exit;
  if TreeView.Selected.Parent = TreeRootQuery Then begin
    Form:= TFormTable.Create(Application, Integer(TreeView.Selected.Data));
    FormContainer.AddForm(Form);
  end;
end;

//
procedure TFormMain.MenuItemAboutClick(Sender: TObject);
begin
  ShowMessage('Клиент для работы с БД' + #13#10 +
    'Денис Давидюк Б8103А' + #13#10 + 'апрель 2013');
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  ToggleBoxConnect.Checked := False;
  FreeAndNil(Books);
  FreeAndNil(FormContainer);
  FreeAndNil(Database);
end;

procedure TFormMain.MenuItemExitClick(Sender: TObject);
begin
  FormMain.Close;
end;

end.

