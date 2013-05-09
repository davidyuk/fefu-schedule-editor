unit CLFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls, Controls, Buttons, CLBooks;

const
  filter_maxcount = 20;
  filter_operators: array[0..6] of string =    ('< %s', '<= %s', '= %s', '>= %s', '> %s', 'LIKE %s', 'LIKE %s');
  filter_contentleft: array[0..6] of string =  ('',     '',      '',     '',      '',     '',        '%');
  filter_contentright: array[0..6] of string = ('',     '',      '',     '',      '',     '%',       '%');
  filter_captions: array[0..6] of string = ('Меньше', 'Меньше или равно', 'Равно', 'Больше или равно', 'Больше', 'Начинается с', 'Содержит');

type

  TFilterState = Record
    count: integer;
    field: array [0..filter_maxcount-1] of integer;
    oper: array [0..filter_maxcount-1] of integer;
    content: array [0..filter_maxcount-1] of string;
  end;

  { TFilterPanel }

  TFilterPanel = Class(TPanel)
  private
    ComboBoxF, ComboBoxO: TComboBox;
    Edit: TEdit;
    Remove: TSpeedButton;
    procedure ButtonRemoveClick(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent; Columns: array of TColumn); virtual;
    destructor Destroy; override;
  end;

  { TFilter }

  TFilter = class(TComponent)
  private
    FColumns: array of TColumn;
    FParent: TWinControl;
    Panels: array of TFilterPanel;
  public
    procedure AddPanel;
    procedure RemovePanel(panel: TFilterPanel);
    function GetFilterState: TFilterState;
    procedure SetFilterState(AFilterState: TFilterState);
    constructor Create(AColumns: ArrOfTColumn; AParent: TWinControl);
    destructor Destroy; override;
  end;

implementation

{ TFilterPanel }

procedure TFilterPanel.ButtonRemoveClick(Sender: TObject);
begin
  TFilter(Owner).RemovePanel(TFilterPanel(TSpeedButton(Sender).Parent));
end;

constructor TFilterPanel.Create(TheOwner: TComponent; Columns: array of TColumn);
var i: integer;
begin
  inherited Create(TheOwner);
  BevelOuter:=bvNone;
  Align:=alBottom;
  Height := 29;

  ComboBoxO := TComboBox.Create(TheOwner);
  ComboBoxO.Parent := Self;
  ComboBoxO.Align:=alLeft;
  ComboBoxO.Width := 120;
  ComboBoxO.ReadOnly:=True;
  ComboBoxO.BorderSpacing.Around := 3;
  for i:= 0 to high(filter_captions) do
    ComboBoxO.Items.Add(filter_captions[i]);

  ComboBoxF := TComboBox.Create(TheOwner);
  ComboBoxF.Parent := Self;
  ComboBoxF.Align:=alLeft;
  ComboBoxF.ReadOnly:=True;
  ComboBoxF.BorderSpacing.Around := 3;
  ComboBoxF.Width := 120;
  for i:= 0 to high(Columns) do
    ComboBoxF.Items.Add(Columns[i].disp);

  Edit := TEdit.Create(TheOwner);
  Edit.Parent := Self;
  Edit.Align:=alClient;
  Edit.Caption:= '';
  Edit.BorderSpacing.Around := 3;

  Remove := TSpeedButton.Create(TheOwner);
  Remove.Parent := Self;
  Remove.Caption:= 'X';
  Remove.Flat:=True;
  Remove.Align:= alRight;
  Remove.OnClick:= @ButtonRemoveClick;
  Remove.BorderSpacing.Around := 3;
end;

destructor TFilterPanel.Destroy;
begin
  FreeAndNil(ComboBoxF);
  FreeAndNil(ComboBoxO);
  FreeAndNil(Edit);
  FreeAndNil(Remove);
  inherited Destroy;
end;

{ TFilter }

procedure TFilter.AddPanel;
var n: Integer;
begin
  if length(Panels)=filter_maxcount Then exit;
  setLength(Panels, length(Panels)+1);
  n := High(Panels);
  Panels[n] := TFilterPanel.Create(TComponent(Self), FColumns);
  Panels[n].Parent := FParent;
end;

procedure TFilter.RemovePanel(panel: TFilterPanel);
var i, j: integer;
begin
  if length(Panels) = 1 Then begin
    Panels[0].ComboBoxF.ItemIndex := -1;
    Panels[0].ComboBoxO.ItemIndex := -1;
    Panels[0].Edit.Text := '';
    exit;
  end;
  for i:= 0 to high(Panels) do
    if Panels[i] = panel Then begin
      freeAndNil(Panels[i]);
      for j:= i to high(Panels)-1 do
        Panels[j] := Panels[j+1];
    end;
  setLength(Panels, length(Panels)-1);
end;

function TFilter.GetFilterState: TFilterState;
var
  i, k: integer;
begin
  k := 0;
  for i:= 0 to High(Panels) do with Panels[i] do begin
    if (ComboBoxF.ItemIndex = -1) or (ComboBoxO.ItemIndex = -1) or (Edit.Text = '') Then continue;
    Result.field[k] := ComboBoxF.ItemIndex;
    Result.oper[k] := ComboBoxO.ItemIndex;
    Result.content[k] := Edit.Text;
    inc(k);
  end;
  Result.count:= k;
end;

procedure TFilter.SetFilterState(AFilterState: TFilterState);
var i: integer;
begin
  for i:= 0 to High(Panels) do
    FreeAndNil(Panels[i]);
  setLength(Panels, 0);
  for i:= 0 to AFilterState.count do begin
    AddPanel;
    Panels[i].ComboBoxF.ItemIndex := AFilterState.field[i];
    Panels[i].ComboBoxO.ItemIndex := AFilterState.oper[i];
    Panels[i].Edit.Text := AFilterState.content[i];
  end;
end;

constructor TFilter.Create(AColumns: ArrOfTColumn; AParent: TWinControl);
begin
  setLength(Panels, 0);
  FColumns := AColumns;
  FParent := AParent;
  AddPanel;
end;

destructor TFilter.Destroy;
var i: integer;
begin
  For i:= 0 to high(Panels) do
    FreeAndNil(Panels[i]);
  inherited Destroy;
end;

end.

