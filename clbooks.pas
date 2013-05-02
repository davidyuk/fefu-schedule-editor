unit CLBooks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, DOM, XMLRead, XMLWrite;

type
  TDBValueType = (tInt, tStr);

  TColumn = record
    kind: TDBValueType;
    {имя в таблице, имя в объединённой таблице, связанная таблица, отображаемое имя}
    name, jname, table, disp: string;
    width: integer;
  end;

  TBook = record
    name, table, sql: string;
    Columns: array of TColumn;
  end;

  ArrOfTBook = array of TBook;
  ArrOfTColumn = array of TColumn;

  { TBooks }

  TBooks = class
  private const
    DBValueInXML: array [0..1] of string = ('int', 'str');
  private
    FBooks: array of TBook;
    FTitle: String;
  public
    property Book: ArrOfTBook read FBooks write FBooks;
    property Title: string read FTitle;
    constructor Create();
    destructor Destroy(); override;
  end;

var
  Books: TBooks;

implementation

{ TBooks }

constructor TBooks.Create;
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
    setLength(FBooks[j].Columns, k+1);
    for i:= 0 to Node.Attributes.Length-1 do
      case Node.Attributes[i].NodeName of
        'name': FBooks[j].Columns[k].name:=UTF8Encode(Node.Attributes[i].TextContent);
        'jname': FBooks[j].Columns[k].jname:=UTF8Encode(Node.Attributes[i].TextContent);
        'kind':
          for l:= 0 to high(DBValueInXML) do
            if DBValueInXML[l] = UTF8Encode(Node.Attributes[i].TextContent) then
              FBooks[j].Columns[k].kind := TDBValueType(l);
        'table': FBooks[j].Columns[k].table:=UTF8Encode(Node.Attributes[i].TextContent);
        'disp': FBooks[j].Columns[k].disp:=UTF8Encode(Node.Attributes[i].TextContent);
        'width': FBooks[j].Columns[k].width:=StrToInt(UTF8Encode(Node.Attributes[i].TextContent));
      end;
    Node := Node.NextSibling;
    inc(k);
  end;
end;

begin
  try
    ReadXMLFile(inp, 'query.xml');
    FTitle := UTF8Encode(inp.DocumentElement.Attributes.Item[0].TextContent);
    Node1 := inp.DocumentElement.FirstChild;
    j:= 0;
    while Node1 <> nil do begin
      setLength(FBooks, j+1);
      FBooks[j].name := UTF8Encode(Node1.Attributes[0].TextContent);
      FBooks[j].table := UTF8Encode(Node1.Attributes[1].TextContent);
      Node2 := Node1.FirstChild;
      while Node2 <> Nil do begin
        case Node2.NodeName of
          'sql': FBooks[j].sql:= UTF8Encode(Node2.TextContent);
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

destructor TBooks.Destroy;
var
  outp: TXMLDocument;
  Node1, Node2, Node3, Node4: TDOMElement;
  i, j: integer;
begin
  outp:= TXMLDocument.Create;
  Node1 := outp.CreateElement('document');
  Node1.SetAttribute('name',UTF8Decode(FTitle));
  for i:= 0 to high(FBooks) do begin
    Node2:= outp.CreateElement('book');
    Node2.SetAttribute('name', UTF8Decode(FBooks[i].name));
    Node2.SetAttribute('table', UTF8Decode(FBooks[i].table));

    Node3:= outp.CreateElement('sql');
    Node3.TextContent:= UTF8Decode(FBooks[i].sql);
    Node2.AppendChild(Node3);

    Node3:= outp.CreateElement('columns');
    for j:= 0 to high(FBooks[i].Columns) do begin
      Node4 := outp.CreateElement('column');
      Node4.SetAttribute('name', UTF8Decode(FBooks[i].Columns[j].name));
      Node4.SetAttribute('jname', UTF8Decode(FBooks[i].Columns[j].jname));
      Node4.SetAttribute('kind', UTF8Decode(DBValueInXML[Integer(FBooks[i].Columns[j].kind)]));
      Node4.SetAttribute('table', UTF8Decode(FBooks[i].Columns[j].table));
      Node4.SetAttribute('disp', UTF8Decode(FBooks[i].Columns[j].disp));
      Node4.SetAttribute('width', UTF8Decode(intToStr(FBooks[i].Columns[j].width)));
      Node3.AppendChild(Node4);
    end;
    Node2.AppendChild(Node3);

    Node1.AppendChild(Node2);
  end;
  outp.AppendChild(Node1);
  WriteXMLFile(outp, 'query.xml');
  FreeAndNil(outp);
end;

initialization

  Books := TBooks.Create();

finalization

  FreeAndNil(Books);

end.

