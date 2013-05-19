unit CLExportToHTML;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, strutils;

type
  ArrOfArrOfString = array of array of string;

procedure ExportToHTML(ATitle: string; ACells: ArrOfArrOfString; AConflicts: array of string);

implementation

procedure ExportToHTML(ATitle: string; ACells: ArrOfArrOfString; AConflicts: array of string);
const item_separator = '<newitem>';
var
  SaveDialog: TSaveDialog;
  fileName, table, information, filters, conflicts: string;
  i, j: integer;
  content: TStringList;
begin
  SaveDialog := TSaveDialog.Create(nil);
  SaveDialog.Filter := 'Веб-страница (*.htm;*.html)|*.htm;*.html|';
  if SaveDialog.Execute Then fileName := SaveDialog.FileName
  else fileName := '';
  FreeAndNil(SaveDialog);
  if fileName = '' Then exit;

  content := TStringList.Create;
  content.LoadFromFile('export\htmlpattern.html');

  information := 'Информация';

  { TODO : надо добавить вывод фильтров }
  filters := 'фильтры какие-нибудь';

  table := '<table>'#13#10;
  for i:= 0 to High(ACells) do begin
    table += '<tr>'#13#10;
    for j:= 0 to High(ACells[i]) do
      table += '  <td>'+StringsReplace(ACells[i][j], [item_separator, #13#10], ['<hr>', '<br>'], [rfReplaceAll])+'</td>'#13#10;
    table += '</tr>'#13#10;
  end;
  table += '</table>';

  conflicts := '';
  if Length(AConflicts) <> 0 Then begin
    conflicts += 'Конфликты:'#13#10;
    conflicts += '<ol>'#13#10;
    for i:= 0 to High(AConflicts) do
      conflicts += '<li>'+AConflicts[i]+'</li>'#13#10;
    conflicts += '</ol>';
  end else
    conflicts := 'Конфликты не обнаружены';

  content.Text := StringsReplace(content.Text, ['<%title%>', '<%table%>', '<%information%>', '<%filters%>', '<%conflicts%>'],
    [ATitle, table, information, filters, conflicts],[rfReplaceAll]);

  try
    content.SaveToFile(fileName);
  except
    on E: Exception do begin
      MessageDlg('Произошла ошибка при сохранении файла.'+#13#10+E.Message, mtError,  [mbOK], 0);
    end;
  end;
  FreeAndNil(content);
end;

end.

