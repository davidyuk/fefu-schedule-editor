unit CLFormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, ExtCtrls, ComCtrls, StdCtrls,
  Menus, CLDatabase, CLMetadata, CLFormContainer, CLFormTable, Graphics;

type

  { TFormMain }

  TFormMain = class(TForm)
    MainMenu: TMainMenu;
    MenuItemFile: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemAbout: TMenuItem;
    PanelTools: TPanel;
    ToggleBoxConnect: TToggleBox;
    TreeView: TTreeView;
    procedure FormTableClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormTableActive(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure ToggleBoxConnectChange(Sender: TObject);
    procedure TreeViewClick(Sender: TObject);
    procedure SetFormStateInTreeView(id: integer; setActive: boolean);
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
  Bitmap: TBitmap;
  ImageList: TImageList;
begin
  ImageList := TImageList.Create(TreeView);
  Bitmap := TBitmap.Create;
  Bitmap.LoadFromFile('images/b.bmp');
  ImageList.AddMasked(Bitmap, clWhite);
  FreeAndNil(Bitmap);
  Bitmap := TBitmap.Create;
  Bitmap.LoadFromFile('images/g.bmp');
  ImageList.AddMasked(Bitmap, clWhite);
  FreeAndNil(Bitmap);
  TreeView.Images := ImageList;
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
        ToggleBoxConnect.Checked := false;
        exit;
      end;
    end;
    SetLength(FormTableArr, Metadata.TableCount);
    TreeView.Enabled:=True;
    TreeRootQuery := TreeView.Items.Add(nil, 'Таблицы');
    for i:= 0 to Metadata.TableCount - 1 do begin
      t := TreeView.Items.AddChild(TreeRootQuery, Metadata[i].display);
      t.Data := Pointer(i); //запоминаем номер справочника
    end;
    ToggleBoxConnect.Caption := 'Отключиться от БД';
  end else begin
    TreeView.Enabled := False;
    TreeView.Items.Clear;
    ToggleBoxConnect.Caption := 'Подключиться к БД';
    FormContainer.Clear;
    Connection.Connected := false;
  end;
end;

procedure TFormMain.TreeViewClick(Sender: TObject);
var
  Form: TFormTable;
  tableid: integer;
begin
  if not Assigned(TreeView.Selected) Then exit;
  if not Assigned(TreeView.Selected.Parent) Then exit;
  if TreeView.Selected.Parent = TreeRootQuery Then begin
    tableid := Integer(TreeView.Selected.Data);
    if TreeView.Selected.ImageIndex=-1 Then begin
      Form:= TFormTable.Create(Application, tableid);
      FormTableArr[tableid]:= Form;
      Form.AddHandlerClose(@FormTableClose, True);
      //Form.AddHandlerOnChangeBounds(@FormTableActive);
      Form.OnActivate := @FormTableActive;
      FormContainer.AddForm(Form);
      SetFormStateInTreeView(tableid, true);
    end else begin
      SetFormStateInTreeView(tableid, true);
      FormTableArr[tableid].Show;
      FormTableArr[tableid].BringToFront;
    end;
  end;
  TreeView.Selected.Selected:=false;
end;

procedure TFormMain.SetFormStateInTreeView(id: integer; setActive: boolean);
var i, formId: integer;
begin
  for i:= 0 to TreeRootQuery.Count-1 do begin
    formId := Integer(TreeRootQuery.Items[i].Data);
    if TreeRootQuery.Items[i].ImageIndex = 1 Then
      TreeRootQuery.Items[i].ImageIndex := 0;
    if formId = id Then
      if setActive Then TreeRootQuery.Items[i].ImageIndex := 1
      else TreeRootQuery.Items[i].ImageIndex := -1
  end;
end;

procedure TFormMain.FormTableClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SetFormStateInTreeView(TFormTable(Sender).TableId, false);
end;

procedure TFormMain.FormTableActive(Sender: TObject);
begin
  SetFormStateInTreeView(TFormTable(Sender).TableId, true);
end;

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

