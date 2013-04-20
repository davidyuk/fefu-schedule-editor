unit CLFormEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DbCtrls,
  ExtCtrls, StdCtrls, CLFormChild;

type

  { TFormEdit }

  TFormEdit = class(TFormChild)
    ButtonSave: TButton;
    ButtonCancel: TButton;
    Panel: TPanel;
    ScrollBox1: TScrollBox;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
  end;

implementation

{$R *.lfm}

{ TFormEdit }


procedure TFormEdit.ButtonSaveClick(Sender: TObject);
begin

end;

procedure TFormEdit.FormShow(Sender: TObject);
begin

end;

procedure TFormEdit.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

end.

