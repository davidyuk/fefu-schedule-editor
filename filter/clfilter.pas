unit CLFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls, Buttons, Controls, CLMetadata;

const
  filter_maxcount = 20;
  filter_operators: array[0..6] of string =
    ('< %s', '<= %s', '= %s', '>= %s', '> %s', 'LIKE %s', 'LIKE %s');
  filter_contentleft: array[0..6] of string =
    ('',     '',      '',     '',      '',     '',        '%');
  filter_contentright: array[0..6] of string =
    ('',     '',      '',     '',      '',     '%',       '%');
  filter_captions: array[0..6] of string =
    ('Меньше', 'Меньше или равно', 'Равно', 'Больше или равно', 'Больше', 'Начинается с', 'Содержит');

type

  TFilterState = Record
    count: integer;
    field: array [0..filter_maxcount-1] of integer;
    oper: array [0..filter_maxcount-1] of integer;
    content: array [0..filter_maxcount-1] of string;
  end;

  TFilterCallBack = procedure of object;

  ArrOfStr = array of string;

  { TFilterPanel }

  TFilterPanel = Class(TCustomPanel)
  private
    ComboBoxF, ComboBoxO: TComboBox;
    Edit: TEdit;
    Remove: TSpeedButton;
    function GetContent: String;
    function GetField: Integer;
    function GetOperation: Integer;
    procedure SetContent(AValue: String);
    procedure SetField(AValue: Integer);
    procedure SetOperation(AValue: Integer);
  public
    property Field: Integer read GetField write SetField;
    property Operation: Integer read GetOperation write SetOperation;
    property Content: String read GetContent write SetContent;
    constructor Create(TheOwner: TComponent; Columns: array of TColumn; ButtonRemoveClick: TNotifyEvent); virtual;
  end;

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
    FButtonCancel.Enabled := true;
    FApplyed := true;
  end else
    FApplyed := false;
  FOnApply;
end;

procedure TFilter.CancelFilter(Sender: TObject);
begin
  FButtonCancel.Enabled := false;
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
  Panel.BevelOuter:=bvNone;
  Panel.Align:=alTop;
  Panel.AutoSize := True;

  Button := TButton.Create(Self);
  Button.Caption := 'Добавить условие';
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
  FButtonCancel.Caption := 'Отменить фильтр';
  FButtonCancel.AutoSize := true;
  FButtonCancel.BorderSpacing.Around := 3;
  FButtonCancel.Parent := Panel;
  FButtonCancel.Align := alRight;
  FButtonCancel.OnClick := @CancelFilter;
  FButtonCancel.Enabled := false;

  Panel.Parent := FParent;
end;

{ TFilterPanel }

function TFilterPanel.GetContent: String;
begin
  result:= Edit.Text;
end;

function TFilterPanel.GetField: Integer;
begin
  result:= ComboBoxF.ItemIndex;
end;

function TFilterPanel.GetOperation: Integer;
begin
  Result := ComboBoxO.ItemIndex;
end;

procedure TFilterPanel.SetContent(AValue: String);
begin
  Edit.Text := AValue;
end;

procedure TFilterPanel.SetField(AValue: Integer);
begin
  ComboBoxF.ItemIndex := AValue;
end;

procedure TFilterPanel.SetOperation(AValue: Integer);
begin
  ComboBoxO.ItemIndex := AValue;
end;

constructor TFilterPanel.Create(TheOwner: TComponent;
  Columns: array of TColumn; ButtonRemoveClick: TNotifyEvent);
var i: integer;
begin
  inherited Create(TheOwner);
  BevelOuter:=bvNone;
  Align:=alTop;
  AutoSize := True;

  ComboBoxO := TComboBox.Create(Self);
  ComboBoxO.Parent := Self;
  ComboBoxO.Align:=alLeft;
  ComboBoxO.Width := 120;
  ComboBoxO.ReadOnly:=True;
  ComboBoxO.BorderSpacing.Around := 3;
  for i:= 0 to high(filter_captions) do
    ComboBoxO.Items.Add(filter_captions[i]);

  ComboBoxF := TComboBox.Create(Self);
  ComboBoxF.Parent := Self;
  ComboBoxF.Align:=alLeft;
  ComboBoxF.ReadOnly:=True;
  ComboBoxF.BorderSpacing.Around := 3;
  ComboBoxF.Width := 120;
  for i:= 0 to high(Columns) do
    ComboBoxF.Items.Add(Columns[i].display);

  Edit := TEdit.Create(Self);
  Edit.Parent := Self;
  Edit.Align:=alClient;
  Edit.Caption:= '';
  Edit.BorderSpacing.Around := 3;

  Remove := TSpeedButton.Create(Self);
  Remove.Parent := Self;
  Remove.Caption:= 'X';
  Remove.Flat:=True;
  Remove.Align:= alRight;
  Remove.OnClick:= ButtonRemoveClick;
  Remove.BorderSpacing.Around := 3;
end;

end.

