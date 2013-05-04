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
    public
      procedure AddForm(FormChild: TFormChild);
      procedure Clear;
  end;

var
  FormContainer: TFormContainer;

implementation

{ TFormContainer }

procedure TFormContainer.AddForm(FormChild: TFormChild);
begin
  setLength(Forms, length(Forms)+1);
  Forms[high(Forms)] := FormChild;
  FormChild.Show();
end;

procedure TFormContainer.Clear;
var i: integer;
begin
  for i:= 0 to high(Forms) do begin
    Forms[i].Free;
  end;
  setlength(Forms, 0);
end;

finalization

  FreeAndNil(FormContainer);

end.

