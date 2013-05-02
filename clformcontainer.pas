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
      FMDIParent: TWinControl;
    public
      procedure AddForm(FormChild: TFormChild);
      procedure UpdateSize;
      procedure Clear;
      constructor Create(AMDIParent: TWinControl);
  end;

var
  FormContainer: TFormContainer;

implementation

{ TFormContainer }

procedure TFormContainer.AddForm(FormChild: TFormChild);
begin
  //FormChild.Parent := FMDIParent;
  setLength(Forms, length(Forms)+1);
  Forms[high(Forms)] := FormChild;
  FormChild.Show();
end;

procedure TFormContainer.UpdateSize;
var i: integer;
begin
  for i:= 0 to High(Forms) do
    Forms[i].UpdateSize;
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

constructor TFormContainer.Create(AMDIParent: TWinControl);
begin
  inherited Create;
  FMDIParent := AMDIParent;
end;

finalization

  FreeAndNil(FormContainer);

end.

