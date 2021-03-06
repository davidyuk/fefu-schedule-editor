unit CLMetadata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, DOM, XMLRead, XMLWrite;

type
  TDBValueType = (tInt, tStr);

  TColumn = record
    kind: TDBValueType;
    {имя в таблице, связанная таблица, отображаемое имя}
    name, referenceTable, display: string;
    width: integer;
  end;
  ArrOfColumns = array of TColumn;

  TTable = record
    display, name: string;
    Columns: array of TColumn;
  end;

  { TMetadata }

  TMetadata = class
  private const
    DBValueInXML: array [0..1] of string = ('int', 'str');
    FileName = './database/metadata.xml';
  private
    FTables: array of TTable;
    FTitle: String;
    FStringList: TStringList;
    function GetTable(AIndex: Integer): TTable;
    function GetTableCount: Integer;
    procedure SetTable(AIndex: Integer; AValue: TTable);
  public
    function GetTableId(ATableName: string): integer;
    property Table[AIndex: Integer]: TTable read GetTable write SetTable; default;
    property TableCount: Integer read GetTableCount;
    property Title: string read FTitle;
    constructor Create();
    destructor Destroy(); override;
  end;

  function FieldListStr(AFormat: String; TableId: integer): String;
  function GetSelectSQL(TableId, RecordId: integer): string;
  function GetUpdateSQL(TableId, RecordId: integer): string;
  function GetInsertSQL(TableId: integer): string;
  function GetJoinedSQL(TableId: integer): string;
  function GetOrderBySQL(TableId: integer; FieldsIds: array of integer;
    ReferenceTableFields: array of string): string;

var
  Metadata: TMetadata;

implementation

function FieldListStr(AFormat: String; TableId: integer): String;
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

function GetSelectSQL(TableId, RecordId: integer): string;
begin
  result:='SELECT '+FieldListStr('%s', TableId) + ' FROM '+Metadata[TableId].name;
  if RecordId <> -1 Then result += ' WHERE id = '+intToStr(RecordId);
end;

function GetUpdateSQL(TableId, RecordId: integer): string;
begin
  result:='UPDATE '+Metadata[TableId].name+' SET '
    +FieldListStr('%s=:%0:s', TableId) +' WHERE id = '+intToStr(RecordId);
end;

function GetInsertSQL(TableId: integer): string;
begin
  result:='INSERT INTO '+Metadata[TableId].name+' ('+FieldListStr('%s', TableId)
    +') VALUES ('+FieldListStr(':%s', TableId)+')';
end;

function GetJoinedSQL(TableId: integer): string;
var InnerJoin: string; i: integer;
begin
  Result := '';
  InnerJoin := '';
  with Metadata[TableId] do begin
    for i:= 0 to High(Columns) do begin
      if i <> 0 Then Result += ', ';
      if Columns[i].referenceTable = '' Then
        Result += name+'.'+Columns[i].name
      else begin
        Result += Columns[i].referenceTable+'.name';
        InnerJoin += Format(#13#10'INNER JOIN %s ON %s.%s = %s.id',
          [Columns[i].referenceTable, name, Columns[i].name, Columns[i].referenceTable]);
      end;
    end;
    Result := 'SELECT '+Result+' FROM '+name+InnerJoin;
  end;
end;

function GetOrderBySQL(TableId: integer; FieldsIds: array of integer;
  ReferenceTableFields: array of string): string;
var i: integer;
begin
  Result := '';
  with Metadata[TableId] do begin
    for i:= 0 to High(FieldsIds) do begin
      if i <> 0 Then Result += ', ';
      if Length(ReferenceTableFields) = Length(FieldsIds) Then
        If Columns[FieldsIds[i]].referenceTable = '' Then Result+= Columns[FieldsIds[i]].name
        else Result+= Columns[FieldsIds[i]].referenceTable+'.'+ReferenceTableFields[i]
      else
        Result+= Columns[FieldsIds[i]].name;
    end;
    if Result <> '' Then Result := 'ORDER BY '+Result;
  end;
end;

{ TMetadata }

function TMetadata.GetTable(AIndex: Integer): TTable;
begin
  result:= FTables[AIndex];
end;

function TMetadata.GetTableCount: Integer;
begin
  Result := Length(FTables);
end;

procedure TMetadata.SetTable(AIndex: Integer; AValue: TTable);
begin
  FTables[AIndex] := AValue;
end;

function TMetadata.GetTableId(ATableName: string): integer;
begin
  Result := FStringList.IndexOf(ATableName);
  if Result = -1 Then Raise Exception.Create('Table "'+ATableName+'" not found.');
end;

constructor TMetadata.Create;
var
  inp: TXMLDocument;
  Node1, Node2: TDOMNode;
  j: integer;

procedure readColumns();
var
  i, k, l: integer;
  Node: TDOMNode;
begin
  Node := Node2.FirstChild;
  k := 0;
  while Node <> Nil do begin
    setLength(FTables[j].Columns, k+1);
    for i:= 0 to Node.Attributes.Length-1 do
      case Node.Attributes[i].NodeName of
        'name': FTables[j].Columns[k].name:=UTF8Encode(Node.Attributes[i].TextContent);
        'kind':
          for l:= 0 to high(DBValueInXML) do
            if DBValueInXML[l] = UTF8Encode(Node.Attributes[i].TextContent) then
              FTables[j].Columns[k].kind := TDBValueType(l);
        'reference_table': FTables[j].Columns[k].referenceTable:=UTF8Encode(Node.Attributes[i].TextContent);
        'display': FTables[j].Columns[k].display:=UTF8Encode(Node.Attributes[i].TextContent);
        'width': FTables[j].Columns[k].width:=StrToInt(UTF8Encode(Node.Attributes[i].TextContent));
      end;
    Node := Node.NextSibling;
    inc(k);
  end;
end;

begin
  try
    ReadXMLFile(inp, FileName);
    FTitle := UTF8Encode(inp.DocumentElement.Attributes.Item[0].TextContent);
    Node1 := inp.DocumentElement.FirstChild;
    j:= 0;
    FStringList := TStringList.Create;
    while Node1 <> nil do begin
      setLength(FTables, j+1);
      FTables[j].display := UTF8Encode(Node1.Attributes.GetNamedItem('display').TextContent);
      FTables[j].name := UTF8Encode(Node1.Attributes.GetNamedItem('name').TextContent);
      FStringList.Insert(j, FTables[j].name);
      Node2 := Node1.FirstChild;
      while Node2 <> Nil do begin
        case Node2.NodeName of
          'columns': readColumns;
        end;
        Node2:= Node2.NextSibling;
      end;
      Node1:= Node1.NextSibling;
      inc(j);
    end;
  except
    on E: Exception do begin
      MessageDlg('Произошла ошибка при открытии файла параметров.'+#13#10+E.Message, mtError,  [mbOK], 0);
      Application.Terminate;
    end;
  end;
  FreeAndNil(inp);
end;

destructor TMetadata.Destroy;
var
  outp: TXMLDocument;
  Node1, Node2, Node3, Node4: TDOMElement;
  i, j: integer;
begin
  outp:= TXMLDocument.Create;
  Node1 := outp.CreateElement('document');
  Node1.SetAttribute('name',UTF8Decode(FTitle));
  for i:= 0 to high(FTables) do begin
    Node2:= outp.CreateElement('table');
    Node2.SetAttribute('display', UTF8Decode(FTables[i].display));
    Node2.SetAttribute('name', UTF8Decode(FTables[i].name));

    Node3:= outp.CreateElement('columns');
    for j:= 0 to high(FTables[i].Columns) do begin
      Node4 := outp.CreateElement('column');
      Node4.SetAttribute('name', UTF8Decode(FTables[i].Columns[j].name));
      Node4.SetAttribute('kind', UTF8Decode(DBValueInXML[Integer(FTables[i].Columns[j].kind)]));
      Node4.SetAttribute('reference_table', UTF8Decode(FTables[i].Columns[j].referenceTable));
      Node4.SetAttribute('display', UTF8Decode(FTables[i].Columns[j].display));
      Node4.SetAttribute('width', UTF8Decode(intToStr(FTables[i].Columns[j].width)));
      Node3.AppendChild(Node4);
    end;
    Node2.AppendChild(Node3);

    Node1.AppendChild(Node2);
  end;
  outp.AppendChild(Node1);
  WriteXMLFile(outp, FileName);
  FreeAndNil(outp);
  FreeAndNil(FStringList);
end;

initialization

  Metadata := TMetadata.Create();

finalization

  Metadata.Free;

end.

