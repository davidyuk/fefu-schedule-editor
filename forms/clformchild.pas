unit CLFormChild;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  ExtCtrls, StdCtrls, sqldb, db, strutils;

type

  { TFormChild }

  TFormChild = class(TForm)
  protected
    FTableId, FRecordId: integer;
  public
    property TableId: integer read FTableId;
    property RecordId: integer read FRecordId;
    procedure RefreshSQLContent; virtual; abstract;
  end;

implementation

{ TFormChild }

end.

