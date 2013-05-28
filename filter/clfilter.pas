unit CLFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls, Controls, CLMetadata, CLFilterPanel,
  CLFilterTypes;

type

  TFilterCallBack = procedure of object;

  ArrOfStr = array of string;

  { TFilter }

  TFilter = class(TComponent)
  private
    FTableId: Integer;
    FParent: TWinControl;
    FButtonApply, FButtonCancel: TButton;
    FFilterState: TFilterState;
    Panels: array of TFilterPanel;
    FApplyed: Boolean;
    FOnApply: TFilterCallBack;
    procedure AddPanel(Sender: TObject);
    procedure ApplyFilter(Sender: TObject);
    procedure CancelFilter(Sender: TObject);
    procedure ButtonRemoveClick(Sender: TObject);
    function GetPanelsState: TFilterState;
  public
    procedure RemovePanel(panel: TFilterPanel);
    property Applyed: Boolean read FApplyed;
    property FilterState: TFilterState read FFilterState;
    procedure SetPanelsState(AFilterState: TFilterState; Apply: boolean);
    function GetWhereSQL(var Params: ArrOfStr): String; overload;
    constructor Create(TheOwner: TComponent; TableId: Integer; AParent: TWinControl; OnApply: TFilterCallBack);
  end;

implementation

{ TFilter }

procedure TFilter.AddPanel(Sender: TObject);
var n: Integer;
begin
  if length(Panels)=filter_maxcount Then exit;
  setLength(Panels, length(Panels)+1);
  n := High(Panels);
  Panels[n] := TFilterPanel.Create(Self, Metadata[FTableId].Columns, @ButtonRemoveClick);
  Panels[n].Parent := FParent;
end;

procedure TFilter.ApplyFilter(Sender: TObject);
begin
  FFilterState := GetPanelsState;
  if FilterState.count <> 0 Then begin
    FButtonCancel.Visible := true;
    FApplyed := true;
  end else
    FApplyed := false;
  FOnApply;
end;

procedure TFilter.CancelFilter(Sender: TObject);
begin
  FButtonCancel.Visible := false;
  FApplyed := false;
  FOnApply;
end;

procedure TFilter.ButtonRemoveClick(Sender: TObject);
begin
  RemovePanel(TFilterPanel(TButton(Sender).Owner));
end;

procedure TFilter.RemovePanel(panel: TFilterPanel);
var i, j: integer;
begin
  for i:= 0 to high(Panels) do
    if Panels[i] = panel Then begin
      freeAndNil(Panels[i]);
      for j:= i to high(Panels)-1 do
        Panels[j] := Panels[j+1];
    end;
  setLength(Panels, length(Panels)-1);
end;

function TFilter.GetWhereSQL(var Params: ArrOfStr): String;
var i: integer;
begin
  Result := 'WHERE ';
  for i:= 0 to FFilterState.count-1 do begin
    if i > 0 Then Result += ' AND ';
    if Metadata[FTableId].Columns[FFilterState.field[i]].referenceTable <> '' Then
      Result += Metadata[FTableId].Columns[FFilterState.field[i]].referenceTable+'.name'
    else
      Result += Metadata[FTableId].name+'.'+Metadata[FTableId].Columns[FFilterState.field[i]].name;
    Result += ' '+Format(filter_operators[FFilterState.oper[i]], [':P'+intToStr(i)]);
  end;
  SetLength(Params, FFilterState.count);
  for i:= 0 to FFilterState.count-1 do
    Params[i]:= filter_contentleft[FFilterState.oper[i]]+FFilterState.content[i]+filter_contentright[FFilterState.oper[i]]
end;

function TFilter.GetPanelsState: TFilterState;
var i, k: integer;
begin
  k := 0;
  for i:= 0 to High(Panels) do with Panels[i] do begin
    if (Field = -1) or (Operation = -1) or (Content = '') Then continue;
    Result.field[k] := Field;
    Result.oper[k] := Operation;
    Result.content[k] := Content;
    inc(k);
  end;
  Result.count:= k;
end;

procedure TFilter.SetPanelsState(AFilterState: TFilterState; Apply: boolean);
var i: integer;
begin
  for i:= 0 to High(Panels) do
    FreeAndNil(Panels[i]);
  setLength(Panels, 0);
  for i:= 0 to AFilterState.count-1 do begin
    AddPanel(nil);
    Panels[i].Field := AFilterState.field[i];
    Panels[i].Operation := AFilterState.oper[i];
    Panels[i].Content := AFilterState.content[i];
  end;
  if Apply Then ApplyFilter(nil);
end;

constructor TFilter.Create(TheOwner: TComponent; TableId: Integer;
  AParent: TWinControl; OnApply: TFilterCallBack);
var
  Panel: TPanel;
  Button: TButton;
begin
  inherited Create(TheOwner);
  FApplyed := false;
  setLength(Panels, 0);
  FTableId := TableId;
  FParent := AParent;
  FOnApply := OnApply;
  Panel := TPanel.Create(Self);
  Panel.Parent := FParent;
  Panel.BevelOuter:=bvNone;
  Panel.Align:=alTop;
  Panel.AutoSize := True;
  Button := TButton.Create(Self);
  Button.Caption := 'Добавить панель';
  Button.AutoSize := true;
  Button.BorderSpacing.Around := 3;
  Button.Parent := Panel;
  Button.Align := alRight;
  Button.OnClick := @AddPanel;
  FButtonApply := TButton.Create(Self);
  FButtonApply.Caption := 'Применить фильтр';
  FButtonApply.AutoSize := true;
  FButtonApply.BorderSpacing.Around := 3;
  FButtonApply.Parent := Panel;
  FButtonApply.Align := alRight;
  FButtonApply.OnClick := @ApplyFilter;
  FButtonCancel := TButton.Create(Self);
  FButtonCancel.Visible := false;
  FButtonCancel.Caption := 'Отменить фильтр';
  FButtonCancel.AutoSize := true;
  FButtonCancel.BorderSpacing.Around := 3;
  FButtonCancel.Parent := Panel;
  FButtonCancel.Align := alRight;
  FButtonCancel.OnClick := @CancelFilter;
  FButtonCancel.Visible := false;
end;

end.

