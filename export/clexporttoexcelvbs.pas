unit CLExportToExcelVBS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, strutils, CLScheduleCell, math;

type
  ArrOfArrOfString = array of array of string;

procedure ExportToExcelVBS(ATitle: string; ACells: ArrOfArrOfDrawGridCell; AConflicts: array of string);

implementation

procedure ExportToExcelVBS(ATitle: string; ACells: ArrOfArrOfDrawGridCell;
  AConflicts: array of string);
const item_separator = '<newitem>';
var
  SaveDialog: TSaveDialog;
  fileName, table, information, filters, conflicts: string;
  i, j, k, maxCount, curRow: integer; //!
  content: TStringList;
  s: string; //!
begin
  SaveDialog := TSaveDialog.Create(nil);
  SaveDialog.Filter := 'VB скрипт (*.vbs)|*.vbs|'; //!
  if SaveDialog.Execute Then fileName := SaveDialog.FileName
  else fileName := '';
  FreeAndNil(SaveDialog);
  if fileName = '' Then exit;

  content := TStringList.Create;
  content.LoadFromFile('export\excelpattern.vbs');

  information := 'Информация';

  { TODO : надо добавить вывод фильтров }
  filters := 'фильтры какие-нибудь';

  curRow := 0;
  table := '';
  for i:= 0 to High(ACells[0]) do begin
    maxCount := 0;
    for j:= 0 to High(ACells) do
      maxCount := max(maxCount, Length(ACells[j][i].Items));
    for j:= 0 to High(ACells) do
      for k:= 0 to maxCount-1 do begin
        if k < Length(ACells[j][i].Items) Then
          s := StringsReplace(ACells[j][i].Items[k].content, [#13#10], ['"&vbCrLf&"'], [rfReplaceAll])
        else
          s := '';
        table += Format('objExcel.Cells(%d, %d).Value = "%s"'#13#10, [curRow+k+1, j+1, s]);
      end;
    table += Format('objExcel.Range(C(1, %d, 1, %d)).Merge'#13#10, [curRow+1, curRow+maxCount]);
    curRow += maxCount;
  end;
  table += 'GridW = '+IntToStr(Length(ACells))+#13#10;
  table += 'GridH = '+IntToStr(curRow);

  {conflicts := '';
  if Length(AConflicts) <> 0 Then begin
    conflicts += 'Конфликты:'#13#10;
    conflicts += '<ol>'#13#10;
    for i:= 0 to High(AConflicts) do
      conflicts += '<li>'+AConflicts[i]+'</li>'#13#10;
    conflicts += '</ol>';
  end else
    conflicts := 'Конфликты не обнаружены';}

  content.Text := StringsReplace(content.Text, ['<%title%>', '<%table%>', '<%information%>', '<%filters%>', '<%conflicts%>'],
    [ATitle, table, information, filters, conflicts],[rfReplaceAll]);

  try
    s := UTF8Decode(content.Text);
    assign(output, Utf8ToAnsi(fileName));
    Rewrite(output);
    WriteLn(Copy(s, 2, Length(s))); { TODO: Разобраться с кодировками }
    Close(output);
  except
    on E: Exception do begin
      MessageDlg('Произошла ошибка при сохранении файла.'+#13#10+E.Message, mtError,  [mbOK], 0);
    end;
  end;
  FreeAndNil(content);
end;

end.

