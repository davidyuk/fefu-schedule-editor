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
    FKey: string;
  public
    property Key: string read FKey;
    constructor Create(TheOwner: TComponent); virtual;
  end;

implementation

{$R *.lfm}

{ TFormChild }

constructor TFormChild.Create(TheOwner: TComponent{; AKey: string});
begin
  inherited Create(TheOwner);
end;

end.

