unit CLFormChild;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  ExtCtrls, StdCtrls, sqldb, db, strutils;

type

  { TFormChild }

  TFormChild = class(TForm)
  private
    const MDIMargin = 25;
  protected
    FBookId, FRecordId: integer;
  public
    property BookId: integer read FBookId;
    property RecordId: integer read FRecordId;
    constructor Create(TheOwner: TComponent); virtual;
  end;

implementation

{$R *.lfm}

{ TFormChild }

constructor TFormChild.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

end.

