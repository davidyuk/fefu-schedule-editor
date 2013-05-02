unit CLFormEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DbCtrls,
  ExtCtrls, StdCtrls, CLFormChild, sqldb, db, CLBooks;

type

  { TFormEdit }

  TFormEdit = class(TFormChild)
    ButtonSave: TButton;
    ButtonCancel: TButton;
    Datasource: TDatasource;
    DBLookupComboBox1: TDBLookupComboBox;
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
  CloseAction:= caFree;
end;

procedure TFormEdit.FormShow(Sender: TObject);
var
  i: integer;
  p: TPanel;
  e: TEdit;
  c: TComboBox;
begin
  for i:= 0 to high(Books.Book[BookId].Columns) do begin
    //Books.Book[BookId].Columns[i].
  end;
end;

constructor TFormEdit.Create(TheOwner: TComponent; ABookId, ARecordId: integer);
begin
  inherited Create(TheOwner);
  FBookId := ABookId;
  FRecordId := ARecordId;
end;

procedure TFormEdit.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

end.

