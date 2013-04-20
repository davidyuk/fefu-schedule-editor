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
    procedure TreeViewSelectionChanged(Sender: TObject);
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
  FormContainer := TFormContainer.Create();
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

procedure TFormMain.TreeViewSelectionChanged(Sender: TObject);
begin
  if TreeView.Enabled and (TreeView.Selected.Parent = TreeRootQuery) Then
    FormContainer.AddForm('b'+intToStr(Integer(TreeView.Selected.Data)), TFormTable);
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

