unit CLFormConflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, CLFormChild, CLConflicts;

type

  { TFormConflicts }

  TFormConflicts = class(TFormChild)
    ButtonUpdate: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    TreeView1: TTreeView;
    procedure ButtonUpdateClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;


implementation

{$R *.lfm}

{ TFormConflicts }

procedure TFormConflicts.ButtonUpdateClick(Sender: TObject);
var i, j: integer;
begin
//  TreeView1.Items.Clear;
//  TreeView1.Items.Add;
  Memo1.Text := '';
  ConflictsFinder.UpdateConflicts;
  for i:= 0 to ConflictsFinder.Count-1 do begin
    Memo1.Text := Memo1.Text + ConflictsFinder[i].name + ' ячейки: ';
    with ConflictsFinder[i] do
      for j:= 0 to High(cells) do
        Memo1.Text := Memo1.Text + ' ' + IntToStr(cells[j]);;
    Memo1.Text := Memo1.Text + #13#10;
  end;
end;

end.

