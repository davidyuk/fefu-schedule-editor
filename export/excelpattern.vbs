const xlTop =-4160
'from: http://nerds-central.blogspot.ru/2007/02/excel-constant-definitions-for-vbscript.html
Dim WshShell, objExcel, objWorkbook, strFileName
Set WshShell = WScript.CreateObject("WScript.Shell")
strFileName = WshShell.CurrentDirectory&"\"&_
  Replace(WScript.ScriptName, ".vbs", ".xlsx")
Set objExcel = CreateObject("Excel.Application")
objExcel.Visible = False
Set objWorkbook = objExcel.Workbooks.Add()
Function C(i, j, k, l)
  C = chr(Asc("A")+i-1)&j&":"&chr(Asc("A")+k-1)&l
end function

'<%title%>

<%table%>

Set objRange = objExcel.Range(C(1, 1, 1, GridH))
objRange.Interior.Color = RGB(238, 238, 238)
objRange.Borders.Color = RGB(255, 255, 255)
Set objRange = objExcel.Range(C(1, 1, GridW, 1))
objRange.Interior.Color = RGB(238, 238, 238)
objRange.Borders.Color = RGB(255, 255, 255)
objExcel.Columns.VerticalAlignment = xlTop
objExcel.Columns.ColumnWidth = 50
objExcel.Rows.AutoFit()
objExcel.Columns.AutoFit()
objWorkbook.SaveAs(strFileName)
objExcel.Quit
MsgBox "Создание файла завершено успешно."&vbCrLf&strFileName