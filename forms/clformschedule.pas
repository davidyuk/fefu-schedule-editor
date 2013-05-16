unit CLFormSchedule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ExtCtrls, DbCtrls, CLFormChild, CLDatabase, CLMetadata, sqldb, db;

type

  TItem = record
    id: integer;
    content: array of string;
  end;

  {TGridButton = class(TComponent)
  protected
    X, Y: integer;
  public
    procedure Draw(Canvas: TCanvas); virtual; abstract;
    procedure OnClick(x, y: integer); virtual; abstract;
    procedure Create(TheOwner: TComponent; AX, AY: integer); virtual;
  end;}

  { TFormSchedule }

  TFormSchedule = class(TFormChild)
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Datasource: TDatasource;
    DrawGrid: TDrawGrid;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    PanelTop: TPanel;
    PanelRight: TPanel;
    SQLQuery: TSQLQuery;
    procedure DrawGridClick(Sender: TObject);
    procedure DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormShow(Sender: TObject);
  private
    content: array of array of array of TItem;
    function GetSelectSQL:string;
    function GetItemCount(ATableId: integer):integer;
    function FieldListStr(AFormat: String): String;
    function GetJoinedSQL: string;
  public
    procedure RefreshSQLContent; override;
    //constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ TFormSchedule }

procedure TFormSchedule.FormShow(Sender: TObject);
var i, j, x, y: integer;

  procedure FillFixedPart(horizontal: boolean);
  var FSQLQuery: TSQLQuery; FDatasource: TDataSource; i, j: integer;
  begin
    FSQLQuery := TSQLQuery.Create(nil);
    FDatasource := TDataSource.Create(nil);
    if horizontal Then i:= ComboBox1.ItemIndex
    else i:= ComboBox2.ItemIndex;
    FDatasource.DataSet := FSQLQuery;
    FSQLQuery.Transaction := Transaction;
    FSQLQuery.SQL.Text := 'SELECT name FROM '+Metadata[TableId].Columns[i].referenceTable
      +' ORDER BY id';
    FSQLQuery.Open;
    if horizontal Then begin i:= 1; j:= 0; end
    else begin i:= 0; j:= 1; end;
    while not FDatasource.DataSet.EOF do begin
      SetLength(content[i][j], 1);
      SetLength(content[i][j][0].content, 1);
      content[i][j][0].id:= -1;
      content[i][j][0].content[0]:= FDatasource.DataSet.Fields.Fields[0].AsString;
      if horizontal Then inc(i)
      else inc(j);
      FDatasource.DataSet.Next;
    end;
    FreeAndNil(FDatasource);
    FreeAndNil(FSQLQuery);
  end;

begin
  FTableId := Metadata.GetTableId('schedule_items');
  FRecordId := -1;
  If FTableId = -1 Then begin
    MessageDlg('Таблица "schedule_items" не найдена', mtError,  [mbOK], 0);
    Close;
    Exit;
  end;
  for i:= 0 to high(Metadata[TableId].Columns) do begin
    ComboBox1.AddItem(Metadata[TableId].Columns[i].display, nil);
    ComboBox2.AddItem(Metadata[TableId].Columns[i].display, nil);
    ComboBox3.AddItem(Metadata[TableId].Columns[i].display, nil);
  end;
  ComboBox1.ItemIndex := 1;
  ComboBox2.ItemIndex := 2;
  ComboBox3.ItemIndex := 3;
  i:= GetItemCount(Metadata.GetTableId(Metadata[TableId].Columns[ComboBox1.ItemIndex].referenceTable));
  j:= GetItemCount(Metadata.GetTableId(Metadata[TableId].Columns[ComboBox2.ItemIndex].referenceTable));
  SetLength(content, i+1, j+1);
  DrawGrid.ColCount := i+1;
  DrawGrid.RowCount := j+1;

  FillFixedPart(false);
  FillFixedPart(true);

  SQLQuery.SQL.Text := GetJoinedSQL;
  SQLQuery.Transaction := Transaction;
  SQLQuery.Open;

  while not Datasource.DataSet.EOF do begin
    With Datasource.DataSet do begin
      x:= Fields.Fields[ComboBox1.ItemIndex].AsInteger;
      y:= Fields.Fields[ComboBox2.ItemIndex].AsInteger;
      SetLength(content[x,y], Length(content[x,y])+1);
      content[x][y][High(content[x,y])].id := Fields.Fields[0].AsInteger;
      for i:= 1 to Fields.Count-1 do begin
        if (i = ComboBox1.ItemIndex) or (i = ComboBox2.ItemIndex) then continue;
        j:= Length(content[x][y][High(content[x,y])].content);
        SetLength(content[x][y][High(content[x,y])].content, j+1);
        content[x][y][High(content[x,y])].content[j] += Fields.Fields[i].AsString;
      end;
      Next;
    end;
  end;

end;

function TFormSchedule.GetJoinedSQL: string;
{ TODO : Эта функция повторяется в CLFormTable }
var
  firstPart: string;
  i: integer;
begin
with Metadata[TableId] do begin
  firstPart := '';
  result := '';
  for i:= 0 to High(Columns) do begin
    if (Columns[i].referenceTable = '') or (i = ComboBox1.ItemIndex) or (i = ComboBox2.ItemIndex) Then begin
      firstPart += ', '+name+'.'+Columns[i].name;
    end else begin
      firstPart += ', '+Columns[i].referenceTable+'.name';
      result+='INNER JOIN '+Columns[i].referenceTable+' ON '+name+'.'+Columns[i].name+' = '+Columns[i].referenceTable+'.id'+#13#10;
    end;
  end;
  result := 'SELECT '+Copy(firstPart, 3, length(firstPart))+' FROM '+name+#13#10+result;
  result += 'ORDER BY '+Columns[ComboBox3.ItemIndex].referenceTable+'.id';
end;
end;

procedure TFormSchedule.DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var i, j, topPos, maxWidth, t: integer;
begin
  //aState = gd*
  topPos:= aRect.Top;
  maxWidth:= 0;
  for i:= 0 to High(content[aCol][aRow]) do begin
    for j:= 0 to High(content[aCol][aRow][i].content) do begin
      DrawGrid.Canvas.TextRect(aRect, aRect.Left+1, topPos+1, content[aCol][aRow][i].content[j]);
      topPos += DrawGrid.Canvas.TextHeight(content[aCol][aRow][i].content[j]);
      t:= DrawGrid.Canvas.TextWidth(content[aCol][aRow][i].content[j]);
      if t > maxWidth Then maxWidth := t;
    end;
    if i <> High(content[aCol][aRow]) Then begin
      topPos += 3;
      DrawGrid.Canvas.Pen.Color := clGray;
      DrawGrid.Canvas.Line(aRect.Left+5, topPos, aRect.Right-5, topPos);
      topPos += 3;
    end;
  end;
  if (topPos > aRect.Bottom) or (maxWidth > (aRect.Right-aRect.Left)) Then begin
    //ShowMessage(Format('%d %d %d %d', [topPos, aRect.Bottom, maxWidth, aRect.Right-aRect.Left]));
    { TODO : Флаг какой-нибудь нужен }
    DrawGrid.Canvas.Pen.Color := clRed;
    DrawGrid.Canvas.Frame(aRect.Left, aRect.Top, aRect.Right-1, aRect.Bottom-1);
  end;
end;

procedure TFormSchedule.DrawGridClick(Sender: TObject);
var i: integer; s: string;
begin
  with Sender as TDrawGrid do begin
    {s := '';
    for i:= 0 to High(content[Col][Row]) do
      s += content[Col][Row][i].content[0];
    ShowMessage('TextHeight: '+intToStr(Canvas.TextHeight(s)));
    ShowMessage('TextWidth: '+intToStr(Canvas.TextWidth(s)));
    ShowMessage(s);}
  end;
end;

procedure TFormSchedule.RefreshSQLContent;
begin
  SQLQuery.Open;{ TODO : Не известно что тут надо написать }
end;

function TFormSchedule.GetSelectSQL: string;
begin
  with Metadata[TableId] do
    result:='SELECT '+FieldListStr('%s') + ' FROM '+name+' WHERE id = '+intToStr(RecordId);
end;

function TFormSchedule.GetItemCount(ATableId: integer): integer;
var FSQLQuery: TSQLQuery; FDatasource: TDataSource;
begin
  FSQLQuery := TSQLQuery.Create(nil);
  FDatasource := TDataSource.Create(nil);
  FDatasource.DataSet := FSQLQuery;
  FSQLQuery.Transaction := Transaction;
  FSQLQuery.SQL.Text := 'SELECT COUNT(*) FROM '+Metadata[ATableId].name;
  FSQLQuery.Open;
  Result := FDatasource.DataSet.Fields.Fields[0].Value;
  FreeAndNil(FDatasource);
  FreeAndNil(FSQLQuery);
end;

function TFormSchedule.FieldListStr(AFormat: String): String;
var
  i: Integer;
begin
  Result := '';
  with Metadata[TableId] do
    for i := 0 to High(Columns) do begin
      if i > 0 then Result += ', ';
      Result += Format(AFormat, [Columns[i].name]);
    end;
end;

end.

