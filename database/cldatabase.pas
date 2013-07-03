unit CLDatabase;

{$mode objfpc}{$H+}

interface

uses
  sqldb, IBConnection;

var
  Connection: TIBConnection;
  Transaction: TSQLTransaction;

implementation

initialization

  Connection := TIBConnection.Create(Nil);
  Transaction := TSQLTransaction.Create(Nil);
  Transaction.DataBase := Connection;
  Connection.CharSet := 'UNICODE_FSS';
  //без этой строки русские буквы не отображаются в Grid
  Connection.UserName := 'sysdba';
  Connection.Password := 'masterkey';
  Connection.DatabaseName := './database/database.fdb';

finalization

  Connection.Connected := false;
  Transaction.Free;
  Connection.Free;

end.

