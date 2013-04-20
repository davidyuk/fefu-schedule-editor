unit CLFormContainer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls,  sqldb,
  CLFormChild;

type

  CTFormChild = class of TFormChild;

  { TFormContainer }

  TFormContainer = class
    private
      forms: array of TFormChild;
      function Search(text: string): integer;
    public
      procedure AddForm(key: string; tform: CTFormChild);
      procedure Clear;
  end;

var
  FormContainer: TFormContainer;

implementation

{ TFormContainer }

procedure TFormContainer.AddForm(key: string; tform: CTFormChild);
var
  Form: TFormChild;
begin
  if search(key) = -1 Then begin
    Form := tform.Create(Application, key);//tform.Create(key);
    //Form.Parent := Application;
    setLength(forms, length(forms)+1);
    forms[high(forms)] := Form;
    Form.Show();
  end else
    forms[search(key)].BringToFront;
end;

procedure TFormContainer.Clear;
var i: integer;
begin
  for i:= 0 to high(forms) do begin
    forms[i].Close;
    FreeAndNil(forms[i]);
  end;
  setlength(forms, 0);
end;

function TFormContainer.Search(text: string): integer;
var i: integer;
begin
  result := -1;
  for i := 0 to high(forms) do
    if forms[i].Key = text Then result := i;
end;

end.

