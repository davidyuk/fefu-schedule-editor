unit CLFormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, ExtCtrls, ComCtrls, StdCtrls,
  Menus, CLDatabase, CLBooks, CLFormContainer, CLFormTable, Graphics;

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
    procedure FormTableClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure PanelFormsResize(Sender: TObject);
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
var
  b: TBitmap;
  il: TImageList;
begin
  il := TImageList.Create(TreeView);
  b := TBitmap.Create;
  b.LoadFromFile('img/b.bmp');
  il.AddMasked(b, clWhite);
  TreeView.Images := il;
  FormContainer := TFormContainer.Create(FormMain.PanelForms);
  PanelForms.Visible:= True;
end;

procedure TFormMain.ToggleBoxConnectChange(Sender: TObject);
var
  i: integer;
  t: TTreeNode;
begin
  If ToggleBoxConnect.Checked Then begin
    Database.Connected := true;
    if not Database.Connected Then begin
      ToggleBoxConnect.Checked := false;
      exit;
    end;
    TreeView.Enabled := True;
    TreeRootQuery := TreeView.Items.Add(nil, 'Справочники');
    for i:= 0 to high(Books.Book) do begin
      t := TreeView.Items.AddChild(TreeRootQuery, Books.Book[i].name);
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
  //Point: TPoint;
  Form: TFormTable;
begin
  {Point := ScreenToClient(Mouse.CursorPos);
  Point.X -= TreeView.Left;
  Point.Y -= TreeView.Top;
  if TreeView.GetNodeAt(Point.X, Point.Y) = Nil Then exit;}
  if not Assigned(TreeView.Selected.Parent) Then exit;
  if TreeView.Selected.Parent = TreeRootQuery Then begin
    Form:= TFormTable.Create(Application, Integer(TreeView.Selected.Data));
    Form.OnClose:= @FormTableClose;
    FormContainer.AddForm(Form);
    TreeView.Selected.ImageIndex:=0;
  end;
  TreeView.Selected.Selected:=false;
end;

procedure TFormMain.FormTableClose(Sender: TObject; var CloseAction: TCloseAction);
var i: integer;
begin
  for i:= 0 to TreeRootQuery.Count-1 do
    if integer(TreeRootQuery.Items[i].Data)=TFormTable(Sender).BookId Then
      TreeRootQuery.Items[i].ImageIndex:=-1;
  CloseAction:=caFree;
end;

//
procedure TFormMain.MenuItemAboutClick(Sender: TObject);
begin
  ShowMessage('Клиент для работы с БД' + #13#10 +
    'Денис Давидюк Б8103А' + #13#10 + 'май 2013');
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  ToggleBoxConnect.Checked := False;
end;

procedure TFormMain.MenuItemExitClick(Sender: TObject);
begin
  FormMain.Close;
end;

procedure TFormMain.PanelFormsResize(Sender: TObject);
begin
  FormContainer.UpdateSize;
end;

end.

