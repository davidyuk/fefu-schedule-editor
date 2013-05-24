unit CLFormSchedule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, Forms,
  Grids, ExtCtrls, CLFormChild, CLDatabase, CLMetadata, sqldb,
  CLScheduleCell, Math, CLFormContainer, CLFormEdit, CLFormTable,
  CLSchedule, CLFilter, CLExport;

type

  { TFormSchedule }

  TFormSchedule = class(TFormChild)
    ButtonFilterCancel: TButton;
    ButtonExportExcel: TButton;
    ButtonFilterAdd: TButton;
    ButtonFilter: TButton;
    ButtonExportHTML: TButton;
    ButtonAline: TButton;
    CheckBoxAutoSize: TCheckBox;
    CheckBoxEmpty: TCheckBox;
    CheckBoxNames: TCheckBox;
    ComboBoxH: TComboBox;
    ComboBoxV: TComboBox;
    ComboBoxSort: TComboBox;
    DrawGrid: TDrawGrid;
    LabelH: TLabel;
    LabelV: TLabel;
    LabelSort: TLabel;
    PaintBox: TPaintBox;
    PanelDisplayControls: TPanel;
    PanelDisplay: TPanel;
    PanelParams: TPanel;
    PanelControls: TPanel;
    PanelHint: TPanel;
    PanelTop: TPanel;
    PanelRight: TPanel;
    procedure ButtonAlineClick(Sender: TObject);
    procedure ButtonExportExcelClick(Sender: TObject);
    procedure ButtonExportHTMLClick(Sender: TObject);
    procedure ButtonFilterAddClick(Sender: TObject);
    procedure ButtonFilterCancelClick(Sender: TObject);
    procedure ButtonFilterClick(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure DrawGridDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DrawGridDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ParamsChange(Sender: TObject);
    procedure DrawGridClick(Sender: TObject);
    procedure DrawGridDblClick(Sender: TObject);
    procedure DrawGridDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure FormShow(Sender: TObject);
    procedure PaintBoxClick(Sender: TObject);
    procedure PaintBoxMouseLeave(Sender: TObject);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PaintBoxPaint(Sender: TObject);
  private
  const
    MaxCellAlineSideSizeCommon = 200;
    MaxCellAlineSideSize = 400;
  private
    Cells: ArrOfArrOfDrawGridCell;
    MouseOnGrid: TPoint;
    FullTableCell: TDrawGridCell;
    ShowEmptyLines, ShowFieldsNames, DoAline: boolean;
    DisplayFields: array of boolean;
    CheckBoxes: array of TCheckBox;
    Filter: TFilter;
    procedure ShowFullTableCell(content: TDrawGridCell);
    procedure EditCellItem(Sender: TDrawGridCell; Param: integer);
    procedure AlineRow(ARow: integer; common: boolean = False);
    procedure AlineCol(ACol: integer; common: boolean = False);
    procedure ReBuildDrawGridContent;
  public
    procedure RefreshSQLContent; override;
    //constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ TFormSchedule }

procedure TFormSchedule.FormShow(Sender: TObject);
var
  i: integer;
begin
  FTableId := Metadata.GetTableId('schedule_items');
  FRecordId := -1;
  for i := 0 to high(Metadata[TableId].Columns) do
  begin
    ComboBoxSort.AddItem(Metadata[TableId].Columns[i].display, nil);
    if Metadata[TableId].Columns[i].referenceTable = '' Then Continue;
    ComboBoxH.AddItem(Metadata[TableId].Columns[i].display, TObject(IntPtr(i)));
    ComboBoxV.AddItem(Metadata[TableId].Columns[i].display, TObject(IntPtr(i)));
  end;
  ComboBoxH.ItemIndex := 0;
  ComboBoxV.ItemIndex := 1;
  ComboBoxSort.ItemIndex := 3;
  FullTableCell := TDrawGridCell.Create(Self, @ShowFullTableCell, @EditCellItem);
  setLength(CheckBoxes, Length(Metadata[TableId].Columns));
  setLength(DisplayFields, Length(Metadata[TableId].Columns));
  for i := High(Metadata[TableId].Columns) downto 0 do
  begin
    CheckBoxes[i] := TCheckBox.Create(Self);
    CheckBoxes[i].Caption := Metadata[TableId].Columns[i].display;
    CheckBoxes[i].Visible := (i <> IntPtr(ComboBoxH.Items.Objects[ComboBoxH.ItemIndex]))
      and (i <> IntPtr(ComboBoxV.Items.Objects[ComboBoxV.ItemIndex]));
    CheckBoxes[i].Checked := (i <> 0) and CheckBoxes[i].Visible;
    CheckBoxes[i].OnChange := @ParamsChange;
    CheckBoxes[i].Parent := PanelParams;
    CheckBoxes[i].Align := alTop;
    DisplayFields[i] := CheckBoxes[i].Checked;
  end;
  ShowEmptyLines := true;
  ShowFieldsNames:= false;
  Filter := TFilter.Create(Self, Metadata[TableId], PanelDisplay);
  ReBuildDrawGridContent;
end;

procedure TFormSchedule.PaintBoxClick(Sender: TObject);
begin
  FullTableCell.MouseClick(MouseOnGrid.x, MouseOnGrid.y);
end;

procedure TFormSchedule.PaintBoxMouseLeave(Sender: TObject);
begin
  PanelHint.Visible := False;
end;

procedure TFormSchedule.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  MouseOnGrid := Point(x, y);
  FullTableCell.MouseMove(x, y);
  Invalidate;
end;

procedure TFormSchedule.PaintBoxPaint(Sender: TObject);
var
  t: TRect;
begin
  with t do
  begin
    Left := 0;
    Top := 0;
    Right := PaintBox.Width;
    Bottom := PaintBox.Height;
  end;
  FullTableCell.Draw(PaintBox.Canvas, t, False);
  { непонятно как обновить ReadBounds }
end;

procedure TFormSchedule.ComboBoxChange(Sender: TObject);
var i: integer;
begin
  if ComboBoxH.ItemIndex = ComboBoxV.ItemIndex then
  begin
    DrawGrid.Visible := False;
    Exit;
  end;
  DrawGrid.Visible := True;
  for i := 0 to High(CheckBoxes) do begin
    if not CheckBoxes[i].Visible Then CheckBoxes[i].Checked := True;
    CheckBoxes[i].Visible := (i <> IntPtr(ComboBoxH.Items.Objects[ComboBoxH.ItemIndex]))
      and (i <> IntPtr(ComboBoxV.Items.Objects[ComboBoxV.ItemIndex]));
    CheckBoxes[i].Checked := CheckBoxes[i].Checked and CheckBoxes[i].Visible;
  end;
  ReBuildDrawGridContent;
  DrawGrid.Invalidate;
end;

procedure TFormSchedule.DrawGridDragDrop(Sender, Source: TObject; X, Y: Integer
  );
begin

end;

procedure TFormSchedule.DrawGridDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  LabelH.Caption := Format('%d, %d', [x, y]);
  {if Source is TDrawGrid Then begin
    //TDrawGrid(Source).
  end;}
  Accept := false;
end;

procedure TFormSchedule.ParamsChange(Sender: TObject);
var i: integer;
begin
  ShowEmptyLines := CheckBoxEmpty.Checked;
  ShowFieldsNames := CheckBoxNames.Checked;
  for i := 0 to High(CheckBoxes) do
    DisplayFields[i] := CheckBoxes[i].Checked;
  ReBuildDrawGridContent;
  DrawGrid.Invalidate;
end;

procedure TFormSchedule.ButtonAlineClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to High(Cells) do
    AlineCol(i, True);
  for i := 0 to High(Cells[0]) do
    AlineRow(i, True);
end;

procedure TFormSchedule.ButtonExportExcelClick(Sender: TObject);
var t: TFilterState;
begin
  if ButtonFilterCancel.Visible Then t := Filter.FilterState
  else t.count := 0;
  ExportToExcelVBS('Расписание занятий',
    ComboBoxH.Items.Strings[ComboBoxH.ItemIndex],
    ComboBoxV.Items.Strings[ComboBoxV.ItemIndex],
    ComboBoxSort.Items.Strings[ComboBoxSort.ItemIndex],
    TableId,t, Cells, ['Conflict1', 'Conflict2']);
end;

procedure TFormSchedule.ButtonExportHTMLClick(Sender: TObject);
var t: TFilterState;
begin
  if ButtonFilterCancel.Visible Then t := Filter.FilterState
  else t.count := 0;
  ExportToHTML('Расписание занятий',
    ComboBoxH.Items.Strings[ComboBoxH.ItemIndex],
    ComboBoxV.Items.Strings[ComboBoxV.ItemIndex],
    ComboBoxSort.Items.Strings[ComboBoxSort.ItemIndex],
    TableId,t, Cells, ['Conflict1', 'Conflict2']);
end;

procedure TFormSchedule.ButtonFilterAddClick(Sender: TObject);
begin
  Filter.AddPanel;
end;

procedure TFormSchedule.ButtonFilterCancelClick(Sender: TObject);
begin
  ButtonFilterCancel.Visible := false;
  ReBuildDrawGridContent;
  DrawGrid.Invalidate;
end;

procedure TFormSchedule.ButtonFilterClick(Sender: TObject);
begin
  ButtonFilterCancel.Visible := true;
  ReBuildDrawGridContent;
  DrawGrid.Invalidate;
end;

procedure TFormSchedule.ShowFullTableCell(content: TDrawGridCell);
begin
  PanelHint.Width := content.TextWidth + 2; //тк border - 1px
  PanelHint.Height := content.TextHeight + 2;
  PanelHint.Left := content.Rect.Left + DrawGrid.Left + PanelDisplay.Left{ + 1}; { TODO: Немного кривой код }
  PanelHint.Top := content.Rect.Top + DrawGrid.Top + PanelDisplay.Top{ + 1};
  FullTableCell.Items := content.Items;
  FullTableCell.Fixed := False;
  PanelHint.Visible := True;
end;

procedure TFormSchedule.EditCellItem(Sender: TDrawGridCell; Param: integer);
var
  FormEdit: TFormEdit;
  FormTable: TFormTable;
  SQLQuery: TSQLQuery;
  FilterState: TFilterState;
begin
  if Param = MaxInt Then begin
    FormTable := TFormTable.Create(Application, TableId);
    FilterState.count := 0; // 2
    { FilterState.field[0] := ComboBoxH.ItemIndex;
    FilterState.oper[0] := 2;
    FilterState.content[0] := ComboBoxH.Caption;
    FilterState.field[1] := ComboBoxV.ItemIndex;
    FilterState.oper[1] := 2;
    FilterState.content[1] := ComboBoxV.Caption; }
    FormTable.SetFilterState(FilterState);
    FormContainer.AddForm(FormTable);
    exit;
  end;
  if Param = 0 then begin
    FormEdit := TFormEdit.Create(Application, TableId, -1);
    FormEdit.SetDefaultValue(IntPtr(ComboBoxH.Items.Objects[ComboBoxH.ItemIndex]), IntToStr(Sender.PositionValue.x));
    FormEdit.SetDefaultValue(IntPtr(ComboBoxV.Items.Objects[ComboBoxV.ItemIndex]), IntToStr(Sender.PositionValue.y));
    FormContainer.AddForm(FormEdit);
  end;
  if Param > 0 then begin
    FormEdit := TFormEdit.Create(Application, TableId, Sender.Items[Param - 1].id);
    FormContainer.AddForm(FormEdit);
  end;
  if Param < 0 then begin
    SQLQuery := TSQLQuery.Create(nil);
    SQLQuery.Transaction := Transaction;
    SQLQuery.SQL.Text := 'DELETE from '+Metadata[TableId].name+' WHERE id = '+IntToStr(Sender.Items[-Param-1].id);
    SQLQuery.ExecSQL;
    Transaction.Commit;
    SQLQuery.Free;
    FormContainer.RefreshSQLContent;
  end;
end;

procedure TFormSchedule.AlineRow(ARow: integer; common: boolean);
var
  i, m: integer;
begin
  m := 0;
  for i := 0 to High(Cells) do
    m := Max(m, Cells[i][ARow].TextHeight + 1);
  if common then
    m := Min(MaxCellAlineSideSizeCommon, m)
  else
    m := Min(MaxCellAlineSideSize, m);
  DrawGrid.RowHeights[ARow] := m;
end;

procedure TFormSchedule.AlineCol(ACol: integer; common: boolean);
var
  i, m: integer;
begin
  m := 0;
  for i := 0 to High(Cells[ACol]) do
    m := Max(m, Cells[ACol][i].TextWidth + 1);
  if common then
    m := Min(MaxCellAlineSideSizeCommon, m)
  else
    m := Min(MaxCellAlineSideSize, m);
  DrawGrid.ColWidths[ACol] := m;
end;

procedure TFormSchedule.ReBuildDrawGridContent;
begin
  ReBuildGridContent(Self, Cells, TableId,
    IntPtr(ComboBoxH.Items.Objects[ComboBoxH.ItemIndex]),
    IntPtr(ComboBoxV.Items.Objects[ComboBoxV.ItemIndex]),
    ComboBoxSort.ItemIndex,
    ButtonFilterCancel.Visible ,Filter,
    @ShowFullTableCell, @EditCellItem,
    ShowEmptyLines, ShowFieldsNames, DisplayFields);
  DrawGrid.ColCount := Length(Cells);
  DrawGrid.RowCount := Length(Cells[0]);
  DoAline := CheckBoxAutoSize.Checked;
end;

procedure TFormSchedule.DrawGridDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
var
  i, j: integer;
begin
  Cells[aCol][aRow].Draw(DrawGrid.Canvas, aRect, True);
  if DoAline then begin
    for i:= 0 to High(Cells) do
      For j:= 0 to High(Cells[i]) do
        Cells[i][j].CalculateTextSize(DrawGrid.Canvas);
    ButtonAlineClick(nil);
    DoAline := False;
  end;
end;

procedure TFormSchedule.DrawGridMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  CellX, CellY, i, j: integer;
begin
  MouseOnGrid := Point(x, y);
  with Sender as TDrawGrid do begin
    MouseToCell(x, y, CellX, CellY);
    for i := 0 to High(Cells) do
      for j := 0 to High(Cells[i]) do
        Cells[i][j].MouseMove(x, y);
    //Cells[CellX][CellY].MouseMove(x, y);
    //InvalidateCell(CellX, CellY);
    Invalidate;
  end;
end;

procedure TFormSchedule.DrawGridClick(Sender: TObject);
begin
  with Sender as TDrawGrid do begin
    Cells[Col][Row].MouseClick(MouseOnGrid.x, MouseOnGrid.y);
    BeginDrag(False);
  end;
end;

procedure TFormSchedule.DrawGridDblClick(Sender: TObject);
var
  Cell: TPoint;
begin
  with Sender as TDrawGrid do begin
    MouseToCell(MouseOnGrid.x, MouseOnGrid.y, Cell.x, Cell.y);
    if (Cell.x <> 0) and (Cell.y <> 0) then
      exit;
    if (Cell.x = 0) and (Cell.y = 0) then
    begin
      ButtonAlineClick(nil);
      exit;
    end;
    if Cell.x = 0 then
      AlineRow(Cell.y)
    else
      AlineCol(Cell.x);
  end;
end;

procedure TFormSchedule.RefreshSQLContent;
begin
  ReBuildDrawGridContent;
  DrawGrid.Invalidate;
end;

end.
