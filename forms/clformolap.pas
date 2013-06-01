unit CLFormOLAP;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Grids, CheckLst, CLFormChild, CLMetadata, CLOLAPGrid,
  CLExport, CLOLAPCell, CLOLAPCellButton, CLOLAPTypes, CLFormEdit, CLFormTable,
  CLFormContainer, math, sqldb, db, CLDatabase, CLFilter;

type

  { TFormOLAP }

  TFormOLAP = class(TFormChild)
    ButtonExportHTML: TButton;
    ButtonAline: TButton;
    ButtonExportExcel: TButton;
    CheckBoxAutoSize: TCheckBox;
    CheckBoxNames: TCheckBox;
    CheckBoxEmpty: TCheckBox;
    CheckGroupFields: TCheckGroup;
    ComboBoxX: TComboBox;
    ComboBoxY: TComboBox;
    ComboBoxS: TComboBox;
    DrawGrid: TDrawGrid;
    LabelX: TLabel;
    LabelY: TLabel;
    LabelS: TLabel;
    PaintBox: TPaintBox;
    PanelHint: TPanel;
    PanelClient: TPanel;
    PanelRight: TPanel;
    PanelTop: TPanel;
    procedure ButtonAlineClick(Sender: TObject);
    procedure ButtonExportExcelClick(Sender: TObject);
    procedure ButtonExportHTMLClick(Sender: TObject);
    procedure CheckGroupFieldsItemClick(Sender: TObject; Index: integer);
    procedure DrawGridClick(Sender: TObject);
    procedure DrawGridDblClick(Sender: TObject);
    procedure DrawGridDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DrawGridDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DrawGridEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DrawGridStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure FormShow(Sender: TObject);
    procedure AxisChange(Sender: TObject);
    procedure CheckBoxChange(Sender: TObject);
    procedure PaintBoxClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseLeave(Sender: TObject);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PaintBoxStartDrag(Sender: TObject; var DragObject: TDragObject);
  private
    DragItemId: integer;
    DragCell: TPoint;
    Filter: TFilter;
    MousePosition: TPoint;
    OLAPGrid: TOLAPGrid;
    FCells: TOLAPCells;
    FCellHint: TOLAPCell;
    procedure AlineCol(ACol: integer; common: boolean);
    procedure AlineRow(ARow: integer; common: boolean);
    procedure RebuildGrid;
    procedure OLAPCallback(Sender: TObject);
  private const
    MaxCellAlineSideSizeCommon = 200;
    MaxCellAlineSideSize = 400;
    TableName = 'schedule_items';
    TableDispName = 'Расписание занятий';
  public
    procedure RefreshSQLContent; override;
    procedure BeforeRefreshSQLContent; override;
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ TFormOLAP }

procedure TFormOLAP.FormShow(Sender: TObject);
var i: integer;
begin
  Caption := TableDispName;
  FTableId := Metadata.GetTableId(TableName);
  DrawGrid.DoubleBuffered := true;
  PanelHint.DoubleBuffered := true;
  Filter := TFilter.Create(Self, FTableId, PanelClient, @RebuildGrid);
  OLAPGrid:= TOLAPGrid.Create(Self, FTableId, Filter, @OLAPCallback);
  for i:= 0 to High(Metadata[FTableId].Columns) do begin
    ComboBoxS.AddItem(Metadata[FTableId].Columns[i].display, TObject(PtrInt(i)));
    CheckGroupFields.Items.Add(Metadata[FTableId].Columns[i].display);
    CheckGroupFields.Checked[i]:= i <> 0;
    OLAPGrid.DisplayFields[i]:= i <> 0;
    if Metadata[FTableId].Columns[i].referenceTable = '' Then Continue;
    ComboBoxX.AddItem(Metadata[FTableId].Columns[i].display, TObject(PtrInt(i)));
    ComboBoxY.AddItem(Metadata[FTableId].Columns[i].display, TObject(PtrInt(i)));
  end;
  ComboBoxX.ItemIndex := 0;
  ComboBoxY.ItemIndex := 1;
  ComboBoxS.ItemIndex := 3;
  OLAPGrid.ShowEmpty:= CheckBoxEmpty.Checked;
  OLAPGrid.ShowNames:= CheckBoxNames.Checked;
  AxisChange(Nil);
end;

procedure TFormOLAP.AxisChange(Sender: TObject);
  procedure SetCombBox(n: integer; v: Boolean);
  begin
    CheckGroupFields.Checked[n] := v;
    OLAPGrid.DisplayFields[n] := v;
  end;
begin
  if OLAPGrid.AsixX <> 0 Then begin
    SetCombBox(OLAPGrid.AsixX, true);
    SetCombBox(OLAPGrid.AsixY, true);
  end;
  OLAPGrid.AsixX := PtrInt(ComboBoxX.Items.Objects[ComboBoxX.ItemIndex]);
  OLAPGrid.AsixY := PtrInt(ComboBoxY.Items.Objects[ComboBoxY.ItemIndex]);
  OLAPGrid.SortBy := PtrInt(ComboBoxS.Items.Objects[ComboBoxS.ItemIndex]);
  SetCombBox(OLAPGrid.AsixX, false);
  SetCombBox(OLAPGrid.AsixY, false);
  RebuildGrid;
end;

procedure TFormOLAP.OLAPCallback(Sender: TObject);
var
  i: Integer;
  s: String;
  OLAPButton: TOLAPCellButton;
  OLAPCell: TOLAPCell;
  FormEdit: TFormEdit;
  FormTable: TFormTable;
  SQLQuery: TSQLQuery;
  FilterState: TFilterState;
begin
  OLAPButton := TOLAPCellButton(Sender);
  OLAPCell := TOLAPCell(OLAPButton.Owner);
  case OLAPButton.ButtonKind of
    obShowFull: begin
      OLAPCell := TOLAPCell(OLAPButton.Owner);
      FCellHint.Free;
      FCellHint := TOLAPCell.Create(Self, False, @OLAPCallback, OLAPCell.Position);
      for i:= 0 to High(OLAPCell.Items) do
        FCellHint.AddItem(OLAPCell.Items[i].id, OLAPCell.Items[i].content);
      FCellHint.UpdateSize(PaintBox.Canvas);
      PanelHint.Width := max(OLAPCell.Width+1, OLAPCell.Rect.Right-OLAPCell.Rect.Left);
      PanelHint.Height := max(OLAPCell.Height+1, OLAPCell.Rect.Bottom-OLAPCell.Rect.Top);
      PanelHint.Top := OLAPCell.Rect.Top+DrawGrid.Top;
      PanelHint.Left := OLAPCell.Rect.Left+DrawGrid.Left;
      FCellHint.Hover := true;
      PanelHint.Visible := true;
    end;
    obAdd, obEdit: begin
      FormEdit := TFormEdit.Create(Application, TableId, OLAPButton.ItemId);
      FormEdit.SetDefaultValue(OLAPGrid.AsixX, IntToStr(OLAPCell.Position.x));
      FormEdit.SetDefaultValue(OLAPGrid.AsixY, IntToStr(OLAPCell.Position.y));
      FormEdit.LockDefaultValues;
      FormContainer.AddForm(FormEdit);
    end;
    obRemove: begin
      if MessageDlg('Подтверждение удаления записи', 'Вы действительно хотите удалить запись?'+#13#10
        +OLAPCell.Items[OLAPCell.ItemHover].content, mtWarning, mbOKCancel, 0) = mrCancel Then Exit;
      SQLQuery := TSQLQuery.Create(nil); { TODO: нужно перенести в отдельную функцию }
      SQLQuery.Transaction := Transaction;
      SQLQuery.SQL.Text := 'DELETE from '+Metadata[TableId].name+' WHERE id = '+IntToStr(OLAPButton.ItemId);
      SQLQuery.ExecSQL;
      Transaction.Commit;
      SQLQuery.Free;
      FormContainer.RefreshSQLContent;
    end;
    obOpenTable: begin
      FormTable := TFormTable.Create(Application, TableId);
      FilterState := Filter.FilterState;
      FilterState.content[FilterState.count] := FCells[OLAPCell.Position.x][0].GetText;
      FilterState.field[FilterState.count] := OLAPGrid.AsixX;
      FilterState.oper[FilterState.count] := 2;
      FilterState.count += 1;
      FilterState.content[FilterState.count] := FCells[0][OLAPCell.Position.y].GetText;
      FilterState.field[FilterState.count] := OLAPGrid.AsixY;
      FilterState.oper[FilterState.count] := 2;
      FilterState.count += 1;
      FormTable.SetFilterState(FilterState);
      FormContainer.AddForm(FormTable);
    end;
  end;
end;

procedure TFormOLAP.RebuildGrid;
begin
  OLAPGrid.RebuildGrid(FCells);
  DrawGrid.ColCount := Length(FCells);
  DrawGrid.RowCount := Length(FCells[0]);
  if CheckBoxAutoSize.Checked Then ButtonAlineClick(Nil);
  Invalidate;
end;

//////
procedure TFormOLAP.DrawGridClick(Sender: TObject);
begin
  FCells[DrawGrid.Col][DrawGrid.Row].MouseClick(MousePosition.X, MousePosition.Y);
end;

procedure TFormOLAP.RefreshSQLContent;
begin
  RebuildGrid;
end;

procedure TFormOLAP.BeforeRefreshSQLContent;
begin

end;

procedure TFormOLAP.DrawGridDblClick(Sender: TObject);
var Cell: TPoint;
begin
  DrawGrid.MouseToCell(MousePosition.x, MousePosition.y, Cell.x, Cell.y);
  if (Cell.x <> 0) and (Cell.y <> 0) then exit;
  if (Cell.x = 0) and (Cell.y = 0) then begin
    ButtonAlineClick(nil);
    exit;
  end;
  if Cell.x = 0 then AlineRow(Cell.y, false)
  else AlineCol(Cell.x, false);
end;

procedure TFormOLAP.DrawGridDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  SQLQuery: TSQLQuery;
  Datasource: TDataSource;
  Col, Row: integer;
begin
  DrawGrid.MouseToCell(X, Y, Col, Row);
  SQLQuery := TSQLQuery.Create(nil);
  SQLQuery.Transaction := Transaction;
  SQLQuery.SQL.Text := 'UPDATE '+TableName+' SET '+
    Metadata[TableId].Columns[OLAPGrid.AsixX].name
    +'='''+IntToStr(FCells[Col][0].Position.x)+''', '+
    Metadata[TableId].Columns[OLAPGrid.AsixY].name
    +'='''+IntToStr(FCells[0][Row].Position.y)+''' WHERE id = '+intToStr(DragItemId);
  //ShowMessage(SQLQuery.SQL.Text);
  FormContainer.BeforeRefreshSQLContent;
  SQLQuery.ExecSQL;
  Transaction.Commit;
  FormContainer.RefreshSQLContent;
  SQLQuery.Free;
  RebuildGrid;
end;

procedure TFormOLAP.DrawGridDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var Col, Row: integer;
begin
  DrawGrid.MouseToCell(MousePosition.x, MousePosition.y, Col, Row);
  Accept := (Sender = Source) and ((DragCell.x <> Col) or (DragCell.y <> Row)) or (Sender = FCellHint);
end;

procedure TFormOLAP.DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var i, j: integer;
begin
  MousePosition := Point(X, Y);
  for i:= 0 to High(FCells) do
    for j:= 0 to High(FCells[0]) do
      FCells[i][j].MouseMove(x, y);
  DrawGrid.Invalidate;
end;

procedure TFormOLAP.DrawGridStartDrag(Sender: TObject;
  var DragObject: TDragObject);
var Col, Row: integer;
begin
  DrawGrid.MouseToCell(MousePosition.x, MousePosition.y, Col, Row);
  if (Row = 0) or (Col = 0) Then Exit;
  DragItemId := FCells[Col][Row].FixItemHovered;
  DragCell := Point(Col, Row);
end;

procedure TFormOLAP.DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  FCells[aCol][aRow].Draw(DrawGrid.Canvas, aRect, true);
end;

procedure TFormOLAP.DrawGridEndDrag(Sender, Target: TObject; X, Y: Integer);
var Col, Row: integer;
begin
  DrawGrid.MouseToCell(MousePosition.x, MousePosition.y, Col, Row);
  FCells[Col][Row].UnFixItemHovered;
  DrawGrid.InvalidateCell(Col, Row);
end;

procedure TFormOLAP.DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var Col, Row: integer;
begin
  DrawGrid.MouseToCell(X, Y, Col, Row);
  if (Col = 0) or (Row = 0) or
    (length(FCells[Col][Row].Items)=0) Then Exit;
  DrawGrid.BeginDrag(false, 3);
end;

procedure TFormOLAP.ButtonAlineClick(Sender: TObject);
var
  i, j: integer;
begin
  for i:= 0 to High(FCells) do
    for j:= 0 to High(FCells[0]) do
      FCells[i][j].UpdateSize(DrawGrid.Canvas);
  for i := 0 to High(FCells) do AlineCol(i, True);
  for i := 0 to High(FCells[0]) do AlineRow(i, True);
end;

procedure TFormOLAP.ButtonExportExcelClick(Sender: TObject);
begin
  ExportToExcelVBS('Расписание занятий',
    ComboBoxX.Items.Strings[ComboBoxX.ItemIndex],
    ComboBoxY.Items.Strings[ComboBoxY.ItemIndex],
    ComboBoxS.Items.Strings[ComboBoxS.ItemIndex],
    TableId, Filter.FilterState, FCells, ['Conflict1', 'Conflict2']);
end;

procedure TFormOLAP.ButtonExportHTMLClick(Sender: TObject);
begin
  ExportToHTML('Расписание занятий',
    ComboBoxX.Items.Strings[ComboBoxX.ItemIndex],
    ComboBoxY.Items.Strings[ComboBoxY.ItemIndex],
    ComboBoxS.Items.Strings[ComboBoxS.ItemIndex],
    TableId, Filter.FilterState, FCells, ['Conflict1', 'Conflict2']);
end;

procedure TFormOLAP.CheckGroupFieldsItemClick(Sender: TObject; Index: integer);
begin
  OLAPGrid.DisplayFields[Index] := CheckGroupFields.Checked[Index];
  RebuildGrid;
end;

procedure TFormOLAP.AlineCol(ACol: integer; common: boolean);
var i, m: integer;
begin
  m := 0;
  for i := 0 to High(FCells[ACol]) do
    m := Max(m, FCells[ACol][i].Width);
  if common then m := Min(MaxCellAlineSideSizeCommon, m)
  else m := Min(MaxCellAlineSideSize, m);
  DrawGrid.ColWidths[ACol] := m;
end;

procedure TFormOLAP.AlineRow(ARow: integer; common: boolean);
var i, m: integer;
begin
  m := 0;
  for i := 0 to High(FCells) do
    m := Max(m, FCells[i][ARow].Height);
  if common then m := Min(MaxCellAlineSideSizeCommon, m)
  else m := Min(MaxCellAlineSideSize, m);
  DrawGrid.RowHeights[ARow] := m;
end;

procedure TFormOLAP.CheckBoxChange(Sender: TObject);
begin
  OLAPGrid.ShowNames:= CheckBoxNames.Checked;
  OLAPGrid.ShowEmpty:= CheckBoxEmpty.Checked;
  RebuildGrid;
end;

procedure TFormOLAP.PaintBoxClick(Sender: TObject);
begin
  FCellHint.MouseClick(MousePosition.x, MousePosition.y);
end;

procedure TFormOLAP.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if DragItemId = -1 Then Exit;
  if not FCellHint.MouseMove(MousePosition.x, MousePosition.y) Then begin
    DrawGrid.BeginDrag(false, 3); { TODO: не работает OnClick и вызывается MouseLeave }
    DragItemId := FCellHint.FixItemHovered;
    DragCell := Point(-1, -1);
  end;
end;

procedure TFormOLAP.PaintBoxMouseLeave(Sender: TObject);
begin
  PanelHint.Visible := false;
end;

procedure TFormOLAP.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  MousePosition := Point(X, Y);
  FCellHint.MouseMove(X, Y);
  PaintBox.Invalidate;
end;

procedure TFormOLAP.PaintBoxPaint(Sender: TObject);
var t: TRect;
begin
  t:= Bounds(0, 0, PaintBox.Width, PaintBox.Height);
  FCellHint.Draw(PaintBox.Canvas, t, false);
end;

procedure TFormOLAP.PaintBoxStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  DragCell := Point(-1, -1);
end;

constructor TFormOLAP.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

end.

