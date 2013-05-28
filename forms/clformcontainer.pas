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
      procedure FormChildClose(Sender: TObject; var CloseAction: TCloseAction);
    public
      procedure AddForm(FormChild: TFormChild);
      procedure RefreshSQLContent;
      procedure BeforeRefreshSQLContent;
      procedure Clear;
  end;

var
  FormContainer: TFormContainer;

implementation

{ TFormContainer }

procedure TFormContainer.FormChildClose(Sender: TObject; var CloseAction: TCloseAction);
var i: integer; f: Boolean;
begin
  f:= false;
  for i:= 0 to High(Forms)-1 do begin
    if Forms[i] = Sender Then f:= true;
    if f Then Forms[i] := Forms[i+1];
  end;
  SetLength(Forms, Length(Forms)-1);
  CloseAction:= caFree;
end;

procedure TFormContainer.AddForm(FormChild: TFormChild);
begin
  setLength(Forms, length(Forms)+1);
  Forms[high(Forms)] := FormChild;
  FormChild.AddHandlerClose(@FormChildClose, False);
  FormChild.Show();
end;

procedure TFormContainer.RefreshSQLContent;
var i: integer;
begin
  for i:= 0 to High(Forms) do
    Forms[i].RefreshSQLContent;
end;

procedure TFormContainer.BeforeRefreshSQLContent;
var i: integer;
begin
  for i:= 0 to High(Forms) do
    Forms[i].BeforeRefreshSQLContent;
end;

procedure TFormContainer.Clear;
var i: integer;
begin
  for i:= 0 to high(Forms) do begin
    Forms[i].Close;
  end;
  setlength(Forms, 0);
end;

finalization

  FreeAndNil(FormContainer);

end.

