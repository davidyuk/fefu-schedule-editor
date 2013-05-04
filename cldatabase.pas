unit CLDatabase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, IBConnection, Dialogs;

type

  { TDatabase }

  TDatabase = class
  private
    FConnection: TIBConnection;
    FTransaction: TSQLTransaction;
    FConnected: Boolean;
    procedure SetConnected(AValue: boolean);
  public
    property Transaction: TSQLTransaction read FTransaction;
    property Connected: boolean read FConnected write SetConnected;
    constructor Create();
    destructor Destroy(); override;
  end;

var
  Database: TDatabase;

implementation

{ TDatabase }

procedure TDatabase.SetConnected(AValue: boolean);
begin
  if FConnected=AValue then Exit;
  FConnected:=AValue;
  If FConnected Then begin
    try
      FConnection.Connected := True;
      FTransaction.Active := True;
    except
      on E: Exception do begin
        MessageDlg('Ошибка','Невозможно подключиться к базе данных'+#13#10+E.Message, mtError, [mbOK], 0);
        FConnected := False;
      end;
    end;
  end else begin
    FTransaction.Active := False;
    FConnection.Connected := False;
  end;
end;

constructor TDatabase.Create;
begin
  FConnection := TIBConnection.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);
  FTransaction.DataBase := FConnection;
  FConnection.CharSet := 'UNICODE_FSS';
  //без этой строки русские буквы не отображаются в Grid
  FConnection.UserName := 'sysdba';
  FConnection.Password := 'masterkey';
  FConnection.DatabaseName := 'DB.FDB';
end;

destructor TDatabase.Destroy;
begin
  SetConnected(False);
  FreeAndNil(FTransaction);
  FreeAndNil(FConnection);
  inherited Destroy;
end;

initialization

  Database := TDatabase.Create();

finalization

  FreeAndNil(Database);

end.

