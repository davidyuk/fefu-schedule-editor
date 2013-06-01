const xlTop =-4160
const xlContinuous =&H1
'from: http://nerds-central.blogspot.ru/2007/02/excel-constant-definitions-for-vbscript.html
Function C(i, j, k, l)
  C = chr(Asc("A")+i-1)&j&":"&chr(Asc("A")+k-1)&l
end function

Set WshShell = WScript.CreateObject("WScript.Shell")
strFileName = WshShell.CurrentDirectory&"\"&_
  Replace(WScript.ScriptName, ".vbs", ".xlsx")

Set objExcel = CreateObject("Excel.Application")
objExcel.Visible = False
Set objWorkbook = objExcel.Workbooks.Add()
objExcel.Worksheets(3).Delete()

With objExcel.Worksheets(1)
	.Name = "Информация"
	.Activate
<%title%>
<%information%>
<%filters%>
<%conflicts%>
	.Rows.AutoFit
	.Columns.AutoFit
End With

With objExcel.Worksheets(2)
	.Name = "Расписание занятий"
	.Activate
<%table%>
End With

With objExcel.Range(C(1, 1, GridW, GridH))
	.Columns.VerticalAlignment = xlTop
	.Columns.ColumnWidth = 50
	.Rows.AutoFit
	.Columns.AutoFit
End With
With objExcel.Range(C(2, 2, GridW, GridH))
	.Borders.Color = RGB(192, 192, 192)
End With
With objExcel.Range(C(1, 1, 1, GridH))
	.Interior.Color = RGB(240, 240, 240)
	.Borders.Color = RGB(105, 105, 105)
    .Borders.LineStyle = xlContinuous
End With 
With objExcel.Range(C(1, 1, GridW, 1))
	.Interior.Color = RGB(240, 240, 240)
	.Borders.Color = RGB(105, 105, 105)
    .Borders.LineStyle = xlContinuous
End With

objExcel.Worksheets(1).Activate
objWorkbook.SaveAs(strFileName)
objExcel.Quit
MsgBox "Создание файла завершено успешно."&vbCrLf&strFileName