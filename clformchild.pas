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
    FKey: string;
  protected
    function sGetParamFromKey(i: Integer): string; overload;
    function iGetParamFromKey(i: Integer): integer; overload;
  public
    constructor Create(TheOwner: TComponent; AKey: string); virtual;
    property Key: string read FKey;
  end;

implementation

{$R *.lfm}

{ TFormChild }

function TFormChild.sGetParamFromKey(i: Integer): string;
begin
  //result := '';
  //if i > WordCount(FKey, ';') Then exit;
  if Pos(';', FKey) = 0 Then begin
    result := FKey;
    exit;
  end;
  result := ExtractWord(i, FKey, [';']);
end;

function TFormChild.iGetParamFromKey(i: Integer): integer;
begin
  result:= StrToInt(Copy(sGetParamFromKey(i), 2, length(FKey)-1));
end;

constructor TFormChild.Create(TheOwner: TComponent; AKey: string);
begin
  inherited Create(TheOwner);
  FKey := AKey;
end;

end.

