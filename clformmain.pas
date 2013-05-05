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
    PanelTools: TPanel;
    StatusBar: TStatusBar;
    ToggleBoxConnect: TToggleBox;
    TreeView: TTreeView;
    procedure FormTableClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure ToggleBoxConnectChange(Sender: TObject);
    procedure TreeViewClick(Sender: TObject);
  private
    TreeRootQuery: TTreeNode;
    FormTableArr: array of TFormTable;
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
  FormContainer := TFormContainer.Create();
end;

procedure TFormMain.ToggleBoxConnectChange(Sender: TObject);
var
  i: integer;
  t: TTreeNode;
begin
  If ToggleBoxConnect.Checked Then begin
    try
      Connection.Connected := True;
    except
      on E: Exception do begin
        MessageDlg('Ошибка','Невозможно подключиться к базе данных'+#13#10+E.Message, mtError, [mbOK], 0);
      end;
    end;
    if not Connection.Connected Then begin
      ToggleBoxConnect.Checked := false;
      exit;
    end;
    SetLength(FormTableArr, high(Books.Book)+1);
    TreeView.Enabled:=True;
    TreeRootQuery := TreeView.Items.Add(nil, 'Справочники');
    for i:= 0 to high(Books.Book) do begin
      t := TreeView.Items.AddChild(TreeRootQuery, Books.Book[i].name);
      t.Data := Pointer(i); //запоминаем номер справочника
    end;
    StatusBar.Panels.Items[0].Text := 'Подключено';
    ToggleBoxConnect.Caption := 'Отключиться от БД';
  end else begin
    TreeView.Enabled := False;
    TreeView.Items.Clear;
    StatusBar.Panels.Items[0].Text := 'Нет подключения';
    ToggleBoxConnect.Caption := 'Подключиться к БД';
    FormContainer.Clear;
    Connection.Connected := false;
  end;
end;

procedure TFormMain.TreeViewClick(Sender: TObject);
var
  Form: TFormTable;
  bid: integer;
begin
  if not Assigned(TreeView.Selected) Then exit;
  if not Assigned(TreeView.Selected.Parent) Then exit;
  if TreeView.Selected.Parent = TreeRootQuery Then begin
    bid := Integer(TreeView.Selected.Data);
    if (TreeView.Selected.ImageIndex=-1) and (not Assigned(FormTableArr[bid])) Then begin
      Form:= TFormTable.Create(Application, bid);
      FormTableArr[bid]:= Form;
      Form.OnClose:= @FormTableClose;
      FormContainer.AddForm(Form);
      TreeView.Selected.ImageIndex:=0;
    end else begin
      FormTableArr[bid].Show;
      FormTableArr[bid].BringToFront;
    end;
  end;
  TreeView.Selected.Selected:=false;
end;

procedure TFormMain.FormTableClose(Sender: TObject; var CloseAction: TCloseAction);
var i: integer;
begin
  for i:= 0 to TreeRootQuery.Count-1 do
    if integer(TreeRootQuery.Items[i].Data)=TFormTable(Sender).BookId Then
      TreeRootQuery.Items[i].ImageIndex:=-1;
  //CloseAction:=caFree;
end;

//
procedure TFormMain.MenuItemAboutClick(Sender: TObject);
begin
  ShowMessage('Клиент для работы с БД' + #13#10 +
    'Денис Давидюк Б8103А' + #13#10 + 'май 2013');
end;

procedure TFormMain.MenuItemExitClick(Sender: TObject);
begin
  FormMain.Close;
end;

end.

