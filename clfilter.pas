unit CLFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls, Controls, Buttons, CLBooks;

type

  FilterPanel = Record
    Panel: TPanel;
    CBKind, CBField: TComboBox;
    Edit: TEdit;
  end;

  arrOfString = array of string;

  { TFilterTypesContainer }

  TFilterTypesContainer = class
  const
    StringCaption: array[0..2] of string = ('Равно', 'Начинается с', 'Содержит');
    StringValue: array[0..2] of string = ('= ? ', 'LIKE ?%', 'LIKE %?%');
    IntegerCaption: array[0..2] of string = ('Больше', 'Меньше', 'Равно');
    IntegerValue: array[0..2] of string = ('> ? ', '< ? ', '= ? ');
    //TypesName: array[0..4] of string = ('Больше', 'Меньше', 'Равно', 'Начинается с', 'Содержит');
    //TypesValues: array[0..4] of string = ('> ?', '< ?', '= ?', 'LIKE ?%', 'LIKE %?%');
  public
    function GetCaption(DBValueType: TDBValueType): arrOfString;
    function GetValue(DBValueType: TDBValueType): arrOfString;
  end;

  { TFilter }

  TFilter = class
  private
    FColumns: array of TColumn;
    FParent: TWinControl;
    Panels: array of FilterPanel;
    procedure ButtonRemoveClick(Sender: TObject);
    procedure CBFieldChange(Sender: TObject);
  public
    procedure AddPanel;
    function GetSQL: arrOfString;
    constructor Create(AColumns: ArrOfTColumn; AParent: TWinControl);
  end;

var
  FTypes: TFilterTypesContainer;

implementation

{ TFilterTypesContainer }

function TFilterTypesContainer.GetCaption(DBValueType: TDBValueType
  ): arrOfString;
begin
  case DBValueType of
    tStr: result := StringCaption;
    tInt: result := IntegerCaption;
  end;
end;

function TFilterTypesContainer.GetValue(DBValueType: TDBValueType): arrOfString;
begin
  case DBValueType of
    tStr: result := StringValue;
    tInt: result := IntegerValue;
  end;
end;

{ TFilter }

procedure TFilter.AddPanel;
var
  Panel: TPanel;
  SBRemove: TSpeedButton;
  CBKind, CBField: TComboBox;
  Edit: TEdit;
  i, n: Integer;
begin
  if length(Panels)>20 Then exit;
  setLength(Panels, length(Panels)+1);
  n:= High(Panels);

  Panel := TPanel.Create(FParent);
  Panel.Parent := FParent;
  Panel.BevelOuter:=bvNone;
  Panel.Align:=alBottom;
  Panel.Height := 29;
  Panel.Tag := n;
  Panels[n].Panel := Panel;
  //Panel.BorderSpacing.InnerBorder:=3; не работает:(

  CBKind := TComboBox.Create(Panel);
  CBKind.Parent := Panel;
  CBKind.Align:=alLeft;
  CBKind.ReadOnly:=True;
  CBKind.BorderSpacing.Around := 3;
  //for i:= 0 to high(TypesValues) do
  //   CBKind.Items.Add(TypesName[i]);
  CBKind.Tag := n;
  CBKind.Enabled := false;
  Panels[n].CBKind := CBKind;


  CBField := TComboBox.Create(Panel);
  CBField.Parent := Panel;
  CBField.Align:=alLeft;
  CBField.ReadOnly:=True;
  CBField.BorderSpacing.Around := 3;
  CBField.Width := 120;
  for i:= 0 to high(FColumns) do
    CBField.Items.Add(FColumns[i].disp);
  CBField.Tag := n;
  CBField.OnChange:=@CBFieldChange;
  Panels[n].CBField := CBField;

  Edit := TEdit.Create(Panel);
  Edit.Parent := Panel;
  Edit.Align:=alClient;
  Edit.Caption:= '';
  Edit.BorderSpacing.Around := 3;
  Edit.Tag := n;
  Panels[n].Edit := Edit;

  SBRemove := TSpeedButton.Create(Panel); SBRemove.Parent := Panel;
  SBRemove.Caption:= 'X';
  SBRemove.Flat:=True;
  SBRemove.Align:= alRight;
  SBRemove.OnClick:= @ButtonRemoveClick;
  SBRemove.BorderSpacing.Around := 3;
end;

procedure TFilter.CBFieldChange(Sender: TObject);
var
  i: integer;
  cb: TComboBox;
  tf: TDBValueType;
begin
  cb := TComboBox(Sender);
  for i:= 0 to high(FColumns) do
    if FColumns[i].disp = cb.Text Then break;
  Panels[cb.Tag].CBKind.Enabled:= True;
  Panels[cb.Tag].CBKind.Clear;
  tf := FColumns[i].kind;
  for i:= 0 to High(FTypes.GetCaption(tf)) do
    Panels[cb.Tag].CBKind.AddItem(FTypes.GetCaption(tf)[i], TObject(IntPtr(tf)));
end;

procedure TFilter.ButtonRemoveClick(Sender: TObject);
var i: integer;
begin
  if length(Panels) = 1 Then begin
    Panels[0].CBField.ItemIndex := -1;
    Panels[0].CBKind.ItemIndex := -1;
    Panels[0].Edit.Text := '';
    exit;
  end;
  for i:= 0 to high(Panels) do
    if Panels[i].Panel = TSpeedButton(Sender).Parent Then begin
      freeAndNil(Panels[i].Panel);
      Panels[i]:= Panels[high(Panels)];
    end;
  setLength(Panels, length(Panels)-1);
end;

function TFilter.GetSQL: arrOfString;
var
  i, k, t: integer;
  s, st, pref, suff: string;
begin
  s := '';
  k := 0;
  setLength(result, length(Panels)+1);
  for i:= 0 to High(Panels) do with Panels[i] do begin
    if (CBField.ItemIndex = -1) or (CBKind.ItemIndex = -1) or (Edit.Text = '') Then continue;
    if k <> 0 Then st := ' AND ';
    s += st+FColumns[CBField.ItemIndex].jname+' ';
    t:= CBKind.ItemIndex;
    st := FTypes.GetValue(TDBValueType(IntPtr(CBKind.Items.Objects[t])))[t];
    t:= Pos('?', st);
    if st[t+1] <> ' ' then begin
      suff:=st[t+1];
      delete(st, t+1, 1);
    end else suff := '';
    if st[t-1] <> ' ' then begin
      dec(t);
      pref:=st[t];
      delete(st, t, 1);
    end else pref := '';
    Delete(st, t, 1);
    Insert(':P'+intToStr(k), st, t);
    s += st;
    inc(k);
    result[k] := pref+Edit.Text+suff;
  end;
  setLength(result, k+1);
  result[0] := s;
end;

constructor TFilter.Create(AColumns: ArrOfTColumn; AParent: TWinControl);
begin
  setLength(Panels, 0);
  FColumns := AColumns;
  FParent := AParent;
  AddPanel;
end;

initialization

  FTypes:= TFilterTypesContainer.Create;

end.

