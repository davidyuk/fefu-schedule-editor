unit CLFormConflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, CLFormChild, CLConflicts, CLFormContainer, CLFormTable;

type

  { TFormConflicts }

  TFormConflicts = class(TFormChild)
    PanelControls: TPanel;
    TreeViewConflicts: TTreeView;
    procedure FormShow(Sender: TObject);
    procedure TreeViewConflictsDblClick(Sender: TObject);
  private
    RootNodes: array of TTreeNode;
  public
    procedure RefreshSQLContent; override;
  end;


implementation

{$R *.lfm}

{ TFormConflicts }

procedure TFormConflicts.FormShow(Sender: TObject);
begin
  RefreshSQLContent;
end;

procedure TFormConflicts.TreeViewConflictsDblClick(Sender: TObject);
var FormTable: TFormTable;
begin
  if not Assigned(TreeViewConflicts.Selected) Then exit;
  if TreeViewConflicts.Selected.Data = Nil Then exit;
  FormTable := TFormTable.Create(Application, ConflictsFinder.TableId);
  FormTable.SetDisplayRecordIds(ConflictsFinder[Integer(TreeViewConflicts.Selected.Data)].cells);
  FormContainer.AddForm(FormTable);
end;

procedure TFormConflicts.RefreshSQLContent;
var
  t: TConflictType;
  i, j: integer;
  s: String;
begin
  ConflictsFinder.UpdateConflicts;
  TreeViewConflicts.Items.Clear;
  for t in TConflictType do begin
    SetLength(RootNodes, Length(RootNodes)+1);
    RootNodes[Ord(t)] := TreeViewConflicts.Items.Add(Nil, ConflictsFinder.GetConflictTypeName(t));
    RootNodes[Ord(t)].Data := Nil;
  end;
  for i:= 0 to ConflictsFinder.Count-1 do begin
    s := ConflictsFinder[i].name + ', ячейки:';
    with ConflictsFinder[i] do
      for j:= 0 to High(cells) do
        s += ' ' + IntToStr(cells[j]);
    TreeViewConflicts.Items.AddChildObject(RootNodes[Ord(ConflictsFinder[i].ctype)], s, Pointer(PtrInt(i)));
  end;
  for t in TConflictType do begin
    RootNodes[Ord(t)].Expand(True);
    RootNodes[Ord(t)].AlphaSort;
  end;
end;

end.

