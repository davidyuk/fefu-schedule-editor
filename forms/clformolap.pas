unit CLFormOLAP;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Grids, CheckLst, CLFormChild, CLMetadata, CLOLAPGrid,
  CLExport, CLOLAPCell, CLOLAPCellButton, CLOLAPTypes, CLFormEdit, CLFormTable,
  CLFormContainer, math, sqldb, CLDatabase, CLFilter, CLFilterTypes;

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
    procedure DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure AxisChange(Sender: TObject);
    procedure CheckBoxChange(Sender: TObject);
    procedure PaintBoxClick(Sender: TObject);
    procedure PaintBoxMouseLeave(Sender: TObject);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
  private
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
  Caption := 'Расписание занятий';
  FTableId := Metadata.GetTableId('schedule_items');
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
var X, Y, S: integer;
begin
  X := PtrInt(ComboBoxX.Items.Objects[ComboBoxX.ItemIndex]);
  Y := PtrInt(ComboBoxY.Items.Objects[ComboBoxY.ItemIndex]);
  S := PtrInt(ComboBoxS.Items.Objects[ComboBoxS.ItemIndex]);
  if OLAPGrid.AsixX <> 0 Then begin
    CheckGroupFields.Checked[OLAPGrid.AsixX] := true;
    CheckGroupFields.Checked[OLAPGrid.AsixY] := true;
    OLAPGrid.DisplayFields[OLAPGrid.AsixX] := true;
    OLAPGrid.DisplayFields[OLAPGrid.AsixY] := true;
  end;
  CheckGroupFields.Checked[X] := false;
  CheckGroupFields.Checked[Y] := false;
  OLAPGrid.DisplayFields[X] := false;
  OLAPGrid.DisplayFields[Y] := false;
  if PtrInt(ComboBoxX.Items.Objects[ComboBoxX.ItemIndex])
    = PtrInt(ComboBoxY.Items.Objects[ComboBoxY.ItemIndex]) Then
    DrawGrid.Visible := False
  else begin
    OLAPGrid.SortBy := S;
    OLAPGrid.AsixX := X;
    OLAPGrid.AsixY := Y;
    RebuildGrid;
    DrawGrid.Visible := True;
  end;
end;

procedure TFormOLAP.OLAPCallback(Sender: TObject);
var
  i: Integer;
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
      FormEdit.SetDefaultValue(PtrInt(ComboBoxX.Items.Objects[ComboBoxX.ItemIndex]), IntToStr(OLAPCell.Position.x));
      FormEdit.SetDefaultValue(PtrInt(ComboBoxY.Items.Objects[ComboBoxY.ItemIndex]), IntToStr(OLAPCell.Position.y));
      FormEdit.LockDefaultValues;
      FormContainer.AddForm(FormEdit);
    end;
    obRemove: begin
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
      FilterState.field[FilterState.count] := PtrInt(ComboBoxX.Items.Objects[ComboBoxX.ItemIndex]);
      FilterState.oper[FilterState.count] := 2;
      FilterState.count += 1;
      FilterState.content[FilterState.count] := FCells[0][OLAPCell.Position.y].GetText;
      FilterState.field[FilterState.count] := PtrInt(ComboBoxY.Items.Objects[ComboBoxY.ItemIndex]);
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
var Col, Row: integer;
begin
  DrawGrid.MouseToCell(MousePosition.X, MousePosition.Y, Col, Row);
  FCells[Col][Row].MouseClick(MousePosition.X, MousePosition.Y);
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

procedure TFormOLAP.DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  FCells[aCol][aRow].Draw(DrawGrid.Canvas, aRect, true);
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
  with t do begin
    Left := 0;
    Top := 0;
    Right := PaintBox.Width;
    Bottom := PaintBox.Height;
  end;
  FCellHint.Draw(PaintBox.Canvas, t, false); //PaintBox.BoundsRect - почти, но не то
end;

constructor TFormOLAP.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

end.

