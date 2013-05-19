unit CLExportTableToHTML;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, strutils;

type
  ArrOfArrOfString = array of array of string;

procedure ExportTableToHTML(Title: string; Cells: ArrOfArrOfString; Conflicts: array of string);

implementation

uses
  htmlwriter;

procedure ExportTableToHTML(Title: string; Cells: ArrOfArrOfString;
  Conflicts: array of string);
var SaveDialog: TSaveDialog; fileName: string; i, j: integer;
begin
  SaveDialog := TSaveDialog.Create(nil);
  //SaveDialog.Filter := '*.html';
  if SaveDialog.Execute Then fileName := SaveDialog.FileName
  else fileName := '';
  FreeAndNil(SaveDialog);
  if fileName = '' Then exit;
  AssignFile(output, fileName);
  ReWrite(output);
  writeLn('<!DOCTYPE html>');
  writeLn('<html>');
  writeLn('<head>');
  writeLn('<title>'+Title+'</title>');
  writeLn('<meta content="text/html; charset=UTF-8" http-equiv="content-type" />');
  writeLn('<style> table { border-collapse: collapse; } tr, td { border: 1px solid black; margin: 0; padding: 0; } td { padding: 3px; vertical-align: top; font-family: sans-serif; } #conflicts { border: 1px solid red; margin: 3px; padding: 3px; } </style>');
  writeLn('</head>');
  writeLn('<body>');
  writeLn('<table>');
  for i:= 0 to High(Cells) do begin
    writeLn('<tr>');
    for j:= 0 to High(Cells[i]) do
      writeLn('  <td>'+StringsReplace(Cells[i][j], [#13#10'newitem'#13#10, #13#10], ['<hr>', '<br>'], [rfReplaceAll])+'</td>');
    writeLn('</tr>');
  end;
  writeLn('</table>');
  if Length(Conflicts) <> 0 Then begin
    writeLn('<div id="conflicts">');
    writeLn('Конфликты:');
    writeLn('<ol>');
    for i:= 0 to High(Conflicts) do
      writeLn('<li>'+Conflicts[i]+'</li>');
    writeLn('</ol>');
    writeLn('</div>');
  end;
  writeLn('</body>');
  writeLn('</html>');
  close(output);
end;

end.

