unit CLFormContainer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls,  sqldb,
  CLFormChild;

type

  { TFormContainer }

  TFormContainer = class
    private
      Forms: array of TFormChild;
      MDIParent: TForm;
      function Search(Key: string): integer;
    public
      procedure AddForm(FormChild: TFormChild);
      procedure Clear;
      constructor Create(AMDIParent: TForm);
  end;

var
  FormContainer: TFormContainer;

implementation

{ TFormContainer }

procedure TFormContainer.AddForm(FormChild: TFormChild);
begin
  if search(FormChild.Key) = -1 Then begin
    //FormChild.Parent := MDIParent;
    setLength(Forms, length(Forms)+1);
    Forms[high(Forms)] := FormChild;
    FormChild.Show();
  end else begin
    Forms[search(FormChild.Key)].Show;
    Forms[search(FormChild.Key)].BringToFront;
    FreeAndNil(FormChild);
  end;
{
 есть ещё вариант с использованием массива (как в ф-и format) в аргументе
 конструктора, единственный минус в том, что невозможно будет определить,
 не открывая описание конструктора, порядок и значение элементов массива
}
end;

procedure TFormContainer.Clear;
var i: integer;
begin
  for i:= 0 to high(Forms) do begin
    Forms[i].Close;
    FreeAndNil(Forms[i]);
  end;
  setlength(Forms, 0);
end;

constructor TFormContainer.Create(AMDIParent: TForm);
begin
  inherited Create;
  MDIParent := AMDIParent;
end;

function TFormContainer.Search(Key: string): integer;
var i: integer;
begin
  result := -1;
  for i := 0 to high(Forms) do
    if Forms[i].Key = Key Then result := i;
end;

end.

