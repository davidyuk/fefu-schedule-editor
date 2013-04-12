unit CLAddParts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, CLFormTable, sqldb;

type

  { TFormContainer }

  TFormContainer = class
    private
      a: array of TForm;
      s: array of string;
      function search(text: string): integer;
    public
      procedure AddFormTable(NTableName: string; NSQLTransaction: TSQLTransaction; Parent: TWinControl);
      procedure AddFormQuery(NTitle, NSQL: string; NSQLTransaction: TSQLTransaction; Parent: TWinControl);
      procedure UpdateSize;
      procedure Clear;
  end;

implementation

{ TFormContainer }

function TFormContainer.search(text: string): integer;
var i: integer;
begin
  result := -1;
  for i := 0 to high(s) do
    if s[i] = text Then result := i;
end;

procedure TFormContainer.AddFormTable(NTableName: string;
  NSQLTransaction: TSQLTransaction; Parent: TWinControl);
var
  Form: TFormTable;
begin
  if search(NTableName) = -1 Then begin
    Form := TFormTable.Create(Parent);
    Form.Parent := Parent;
    setLength(a, length(a)+1);
    setLength(s, length(s)+1);
    a[high(a)] := Form;
    s[high(s)] := NTableName;
    Form.LoadValues(NTableName, NSQLTransaction);
    Form.Show();
  end else
    if a[search(NTableName)].Visible Then a[search(NTableName)].BringToFront
    else a[search(NTableName)].Show;
end;

procedure TFormContainer.AddFormQuery(NTitle, NSQL: string;
  NSQLTransaction: TSQLTransaction; Parent: TWinControl);
var
  Form: TFormTable;
begin
  if search(NTitle) = -1 Then begin
    Form := TFormTable.Create(Parent);
    Form.Parent := Parent;
    setLength(a, length(a)+1);
    setLength(s, length(s)+1);
    a[high(a)] := Form;
    s[high(s)] := NTitle;
    Form.LoadValues(NTitle, NSQL, NSQLTransaction);
    Form.Show();
  end else
    if a[search(NTitle)].Visible Then a[search(NTitle)].BringToFront
    else a[search(NTitle)].Show;
end;

procedure TFormContainer.UpdateSize;
var i: integer;
begin
  for i:= 0 to high(a) do
    if a[i].WindowState = wsMaximized Then begin
      a[i].Top := -8;
      a[i].Left := -8;
      a[i].Width := a[i].Parent.Width-4;
      a[i].Height := a[i].Parent.Height-25;
    end;
end;

procedure TFormContainer.Clear;
var i: integer;
begin
  for i:= 0 to high(a) do
    a[i].Close;
    //a[i].Free;
  setlength(a, 0);
  setLength(s, 0);
end;

end.

