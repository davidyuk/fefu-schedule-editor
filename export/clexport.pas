unit CLExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, strutils, CLOLAPCell, math, CLFilter, CLFilterTypes,
  CLMetadata;

procedure ExportToExcelVBS(ATitle: string; ACaptionH, ACaptionV, ACaptionS: string;
  TableId: integer; AFilterState: TFilterState; ACells: TOLAPCells;
  AConflicts: array of string);
procedure ExportToHTML(ATitle: string; ACaptionH, ACaptionV, ACaptionS: string;
  TableId: integer; AFilterState: TFilterState; ACells: TOLAPCells;
  AConflicts: array of string);


implementation
const
  dpTitle = '<%title%>';
  dpTable = '<%table%>';
  dpInformation = '<%information%>';
  dpFilters = '<%filters%>';
  dpConflicts = '<%conflicts%>';
  dpLabelFilter = 'Выбранные фильтры:';
  dpLabelConflictsFound = 'Обнаружены конфликты:';
  dpLabelConflictsNotFound = 'Конфликты не обнаружены';
  dpLabelH = 'По горизонтали';
  dpLabelV = 'По вертикали';
  dpLabelS = 'Отсортированно по';

procedure SaveStringListToFile(StringList: TStringList; FileName: string; Ansi: boolean);
var s: string;
begin
  try
    if Not Ansi Then
      StringList.SaveToFile(Utf8ToAnsi(FileName))
    else begin
      s := UTF8Decode(StringList.Text);
      assign(output, Utf8ToAnsi(FileName));
      Rewrite(output);
      WriteLn(Copy(s, 2, Length(s))); { TODO: Разобраться с кодировками }
      Close(output);
    end;
  except
    on E: Exception do begin
      MessageDlg('Произошла ошибка при сохранении файла.'+#13#10+E.Message, mtError,  [mbOK], 0);
    end;
  end;
end;

function GetFileName(SaveDialogFilter: string; FileName: string): string;
var SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(nil);
  SaveDialog.Options:=SaveDialog.Options+[ofOverwritePrompt];
  SaveDialog.Filter := SaveDialogFilter;
  SaveDialog.FileName := FileName;
  if SaveDialog.Execute Then Result := SaveDialog.FileName
  else Result := '';
  FreeAndNil(SaveDialog);
end;

procedure ExportToExcelVBS(ATitle: string; ACaptionH, ACaptionV, ACaptionS: string;
  TableId: integer; AFilterState: TFilterState; ACells: TOLAPCells;
  AConflicts: array of string);
const
  dpCells = #9'.Cells(%d, %d).Value = "%s"'#13#10;
var
  fileName, title, table, information, filters, conflicts: string;
  i, j, k, maxCount, curRow: integer;
  content: TStringList;
  s: string;
begin
  fileName := GetFileName('VB скрипт (*.vbs)|*.vbs|', 'Безымянный.vbs');
  if fileName = '' Then exit;
  if Copy(fileName, Length(fileName)-3, Length(fileName)) <> '.vbs' Then fileName += '.vbs';

  content := TStringList.Create;
  content.LoadFromFile('./export/excelpattern.vbs');

  curRow := 1;
  title := Format(dpCells, [curRow, 1, ATitle]);
  title += Format(#9'.Cells(%d, %d).Font.Size = 18'#13#10, [curRow, 1]);
  title += Format(#9'.Range(C(1, %d, %d, %0:d)).Merge', [curRow, 3]);
  curRow += 2;
  information := Format(#9'.Range(C(1, %d, %d, %d)).Borders.Color = RGB(105, 105, 105)'#13#10, [curRow, 3, curRow+1]);
  information += Format(dpCells, [curRow, 1, dpLabelH]);
  information += Format(dpCells, [curRow, 2, dpLabelV]);
  information += Format(dpCells, [curRow, 3, dpLabelS]);
  curRow += 1;
  information += Format(dpCells, [curRow, 1, ACaptionH]);
  information += Format(dpCells, [curRow, 2, ACaptionV]);
  information += Format(dpCells, [curRow, 3, ACaptionS]);
  curRow += 2;

  if AFilterState.count <> 0 Then begin
    filters := Format(dpCells, [curRow, 1, dpLabelFilter]);
    filters += Format(#9'.Cells(%d, %d).Font.Size = 16'#13#10, [curRow, 1]);
    filters += Format(#9'.Range(C(1, %d, %d, %0:d)).Merge'#13#10, [curRow, 3]);
    curRow += 1;
    j:= curRow;
    for i:= 0 to AFilterState.count-1 do begin
      filters += Format(dpCells,
        [curRow, 1, Metadata[TableId].Columns[AFilterState.field[i]].display]);
      filters += Format(dpCells,
        [curRow, 2, filter_captions[AFilterState.oper[i]]]);
      filters += Format(dpCells,
        [curRow, 3, AFilterState.content[i]]);
      curRow += 1;
    end;
    filters += Format(#9'.Range(C(1, %d, %d, %d)).Borders.Color = RGB(105, 105, 105)', [j, 3, curRow-1]);
    curRow += 1;
  end else
    filters := '';

  if Length(AConflicts) <> 0 Then begin
    conflicts := Format(dpCells, [curRow, 1, dpLabelConflictsFound]);
    conflicts += Format(#9'.Cells(%d, %d).Font.Size = 16'#13#10, [curRow, 1]);
    conflicts += Format(#9'.Range(C(1, %d, %d, %0:d)).Merge'#13#10, [curRow, 3]);
    curRow += 1;
    j:= curRow;
    for i:= 0 to High(AConflicts) do begin
      conflicts += Format(dpCells,
        [curRow, 1, AConflicts[i]]);
      conflicts += Format(#9'.Range(C(1, %d, %d, %0:d)).Merge'#13#10, [curRow, 3]);
      curRow += 1;
    end;
    conflicts += Format(#9'.Range(C(1, %d, %d, %d)).Borders.Color = RGB(105, 105, 105)', [j, 3, curRow-1]);
    curRow += 2;
  end else
    conflicts := Format(dpCells, [curRow, 1, dpLabelConflictsNotFound]);

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
        if s <> '' Then table += Format(dpCells, [curRow+k+1, j+1, s]);
      end;
    table += Format(#9'.Range(C(1, %d, 1, %d)).Merge'#13#10, [curRow+1, curRow+maxCount]);
    curRow += maxCount;
  end;
  table += #9'GridW = '+IntToStr(Length(ACells))+#13#10;
  table += #9'GridH = '+IntToStr(curRow);

  content.Text := StringsReplace(content.Text,
    [dpTitle, dpTable, dpInformation, dpFilters, dpConflicts],
    [title, table, information, filters, conflicts],[rfReplaceAll]);
  SaveStringListToFile(content, fileName, True);
  FreeAndNil(content);
end;

procedure ExportToHTML(ATitle: string; ACaptionH, ACaptionV, ACaptionS: string;
  TableId: integer; AFilterState: TFilterState; ACells: TOLAPCells;
  AConflicts: array of string);
var
  fileName, table, information, filters, conflicts: string;
  i, j, k: integer;
  content: TStringList;
begin
  fileName := GetFileName('Веб-страница (*.htm;*.html)|*.htm;*.html|', 'Безымянный.html');
  if fileName = '' Then exit;
  if (Copy(fileName, Length(fileName)-3, Length(fileName)) <> '.htm')
    and (Copy(fileName, Length(fileName)-4, Length(fileName)) <> '.html') Then fileName += '.html';

  content := TStringList.Create;
  content.LoadFromFile('./export/htmlpattern.html');

  information := '<table>'#13#10'<tr><td>'+dpLabelH+'</td><td>'+dpLabelV+'</td><td>'+dpLabelS+'</td></tr>'#13#10;
  information += '<tr><td>'+ACaptionH+'</td><td>'+ACaptionV+'</td><td>'+ACaptionS+'</td></tr>'#13#10'</table>';

  if AFilterState.count <> 0 Then begin
    filters := '<h2>'+dpLabelFilter+'</h2>'#13#10'<table>'#13#10;
    for i:= 0 to AFilterState.count-1 do begin
      filters += '  <tr><td>'+Metadata[TableId].Columns[AFilterState.field[i]].display
        +'</td><td>'+filter_captions[AFilterState.oper[i]]
        +'</td><td>'+AFilterState.content[i];
      filters += '</td></tr>'#13#10;
    end;
    filters += '</table>'#13#10;
  end else
    filters := '';

  table := '<table id="schedule">'#13#10;
  for j:= 0 to High(ACells[0]) do begin
    table += '<tr>'#13#10;
    for i:= 0 to High(ACells) do begin
      table += '  <td>';
      for k:= 0 to High(ACells[i][j].Items) do begin
        if k <> 0 Then table += '<hr>';
        table += StringsReplace(ACells[i][j].Items[k].content, [#13#10], ['<br>'], [rfReplaceAll]);
      end;
      table += '</td>'#13#10;
    end;
    table += '</tr>'#13#10;
  end;
  table += '</table>';

  if Length(AConflicts) <> 0 Then begin
    conflicts := '<h2>'+dpLabelConflictsFound+'</h2>'#13#10'<table>'#13#10;
    for i:= 0 to High(AConflicts) do
      conflicts += '  <tr><td>'+AConflicts[i]+'</td></tr>'#13#10;
    conflicts += '</table>';
  end else
    conflicts := dpLabelConflictsNotFound;

  content.Text := StringsReplace(content.Text,
    [dpTitle, dpTable, dpInformation, dpFilters, dpConflicts],
    [ATitle, table, information, filters, conflicts],[rfReplaceAll]);
  SaveStringListToFile(content, fileName, false);
  FreeAndNil(content);
end;


end.

