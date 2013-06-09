unit CLFormChild;

{$mode objfpc}{$H+}

interface

uses
  Forms;

type

  { TFormChild }

  TFormChild = class(TForm)
  protected
    FTableId, FRecordId: integer;
  public
    property TableId: integer read FTableId;
    property RecordId: integer read FRecordId;
    procedure RefreshSQLContent; virtual; abstract;
    procedure BeforeRefreshSQLContent; virtual;
  end;

implementation

{ TFormChild }

procedure TFormChild.BeforeRefreshSQLContent;
begin

end;

end.

