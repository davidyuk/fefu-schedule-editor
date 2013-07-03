unit CLDatabaseFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, Dialogs, CLDatabase;

procedure DeleteRecord(TableName: string; RecordId: integer);

implementation

procedure DeleteRecord(TableName: string; RecordId: integer);
var
  SQLQuery: TSQLQuery;
begin
  SQLQuery := TSQLQuery.Create(Nil);
  SQLQuery.Transaction := Transaction;
  SQLQuery.SQL.Text := 'DELETE from '+TableName+' WHERE id = '+intToStr(RecordId);
  try
    SQLQuery.ExecSQL;
    Transaction.Commit;
  except
    on E: Exception do begin
      if E.ClassName = 'EIBDatabaseError' Then
        MessageDlg('Произошла ошибка при удлении записи.'+#13#10+'Удалите записи, ссылающиеся на удаляемую запись.', mtError,  [mbOK], 0)
      else
        MessageDlg(E.Message, mtError,  [mbOK], 0);
      Transaction.Rollback;
    end;
  end;
  SQLQuery.Free;
end;

end.

