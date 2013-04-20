unit CLBooks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, sqldb, DOM, XMLRead, XMLWrite,
  CLFormChild, CLFormEdit;

type
  ArrOfString = array of string;
  ArrOfArrOfString = array of array of string;
  ArrOfArrOfInteger = array of array of integer;

  { TBooks }

  TBooks = class
  private
    FName, FQuery, FTable: ArrOfString;
    FTableFields: ArrOfArrOfString;
    FColumns: ArrOfArrOfInteger;
    FTitle: String;
  public
    property Name: ArrOfString read FName;
    property Query: ArrOfString read FQuery;
    property Table: ArrOfString read FTable;
    property Title: string read FTitle;
    property Columns: ArrOfArrOfInteger read FColumns write FColumns;
    property TableFields: ArrOfArrOfString read FTableFields;// write FColumns;
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
  s: string;
  j, k: integer;
begin
  ReadXMLFile(inp, 'query.xml');
  try
    FTitle := UTF8Encode(inp.DocumentElement.Attributes.Item[0].TextContent);
    Node1 := inp.DocumentElement.FirstChild;
    j:= 0;
    while Node1 <> nil do begin
      setLength(FName, j+1);
      setLength(FQuery, j+1);
      setLength(FTable, j+1);
      setLength(FColumns, j+1);
      setLength(FTableFields, j+1);
      FName[j] := UTF8Encode(Node1.Attributes[0].TextContent);
      Node2 := Node1.FirstChild;
      while Node2 <> Nil do begin
        case Node2.NodeName of
          'table': FTable[j]:= UTF8Encode(Node2.TextContent);
          'get_query': FQuery[j]:= UTF8Encode(Node2.TextContent);
          'columns': begin
              s := Node2.TextContent;
              k:= 0;
              while s <> '' do begin
                setLength(FColumns[j], k+1);
                if Pos(';', s) = 0 Then begin
                  FColumns[j][k] := strToInt(s);
                  break;
                end else begin
                  FColumns[j][k] := strToInt(copy(s, 1, Pos(';', s)-1));
                  delete(s, 1, Pos(';', s));
                end;
                inc(k);
              end;
            end;
          'columns_name': begin
              s := Node2.TextContent;
              k:= 0;
              while s <> '' do begin
                setLength(FTableFields[j], k+1);
                if Pos(';', s) = 0 Then begin
                  FTableFields[j][k] := s;
                  break;
                end else begin
                  FTableFields[j][k] := copy(s, 1, Pos(';', s)-1);
                  delete(s, 1, Pos(';', s));
                end;
                inc(k);
              end;
            end;
        end;
        Node2:= Node2.NextSibling;
      end;
      Node1:= Node1.NextSibling;
      inc(j);
    end;
  except
    on E: Exception do begin
      MessageDlg('Произошла ошибка при открытии файла:'+#13#10+E.Message, mtError,  [mbOK], 0);
      Application.Terminate;
    end;
  end;
  FreeAndNil(inp);
  FreeAndNil(Node1);
  FreeAndNil(Node2);
end;

destructor TBooks.Destroy;
var
  outp: TXMLDocument;
  Node1, Node2, Node3: TDOMElement;
  s: string;
  i, j: integer;
begin
  outp:= TXMLDocument.Create;
  Node1 := outp.CreateElement('document');
  Node1.SetAttribute('name', UTF8Decode(FTitle));
  for i:= 0 to high(FName) do begin
    Node2:= outp.CreateElement('book');
    Node2.SetAttribute('name', UTF8Decode(FName[i]));

    Node3:= outp.CreateElement('table');
    Node3.TextContent:= UTF8Decode(FTable[i]);
    Node2.AppendChild(Node3);
    Node3:= outp.CreateElement('get_query');
    Node3.TextContent:= UTF8Decode(FQuery[i]);
    Node2.AppendChild(Node3);
    s:= '';
    for j:= 0 to high(FColumns[i]) do
      s += intToStr(Columns[i, j])+';';
    Node3:= outp.CreateElement('columns');
    Node3.TextContent:= UTF8Decode(copy(s, 1, length(s)-1));
    Node2.AppendChild(Node3);

    Node1.AppendChild(Node2);
  end;
  outp.AppendChild(Node1);
  WriteXMLFile(outp, 'query.xml');
end;

end.

