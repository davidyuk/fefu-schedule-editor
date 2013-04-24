unit CLFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls, Controls, Buttons, db;

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
    StringValue: array[0..2] of string = ('= "?"', 'LIKE "?%"', 'LIKE "%?%"');
    IntegerCaption: array[0..2] of string = ('Больше', 'Меньше', 'Равно');
    IntegerValue: array[0..2] of string = ('> "?"', '< "?"', '= "?"');
    //TypesName: array[0..4] of string = ('Больше', 'Меньше', 'Равно', 'Начинается с', 'Содержит');
    //TypesValues: array[0..4] of string = ('> "?"', '< "?"', '= "?"', 'LIKE "?%"', 'LIKE "%?%"');
  public
    function GetCaption(FieldType: TFieldType): arrOfString;
    function GetValue(FieldType: TFieldType): arrOfString;
  end;

  { TFilter }

  TFilter = class
  private
    FFields: Tfields;
    //FieldsName: array of string;
    Panels: array of FilterPanel;
    procedure ButtonRemoveClick(Sender: TObject);
    procedure CBFieldChange(Sender: TObject);
  public
    procedure AddPanel(Parent: TWinControl);
    function GetSQL: arrOfString;
    constructor Create(AFields: TFields);
  end;

var
  FTypes: TFilterTypesContainer;

implementation

{ TFilterTypesContainer }

function TFilterTypesContainer.GetCaption(FieldType: TFieldType): arrOfString;
begin
  case FieldType of
    ftString: result := StringCaption;
    ftInteger: result := IntegerCaption;
  end;
end;

function TFilterTypesContainer.GetValue(FieldType: TFieldType): arrOfString;
begin
  case FieldType of
    ftString: result := StringValue;
    ftInteger: result := IntegerValue;
  end;
end;

{ TFilter }

procedure TFilter.AddPanel(Parent: TWinControl);
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

  Panel := TPanel.Create(Parent);
  Panel.Parent := Parent;
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
  for i:= 0 to FFields.Count-1 do
    CBField.Items.Add(FFields.Fields[i].FieldName);
  CBField.Tag := n;
  CBField.OnChange:=@CBFieldChange;
  Panels[n].CBField := CBField;

  Edit := TEdit.Create(Panel);
  Edit.Parent := Panel;
  Edit.Align:=alClient;
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
  tf: TFieldType;
begin
  cb := TComboBox(Sender);
  for i:= 0 to FFields.Count-1 do
    if FFields.Fields[i].FieldName = cb.Text Then break;
  Panels[cb.Tag].CBKind.Enabled:= True;
  Panels[cb.Tag].CBKind.Clear;
  tf := FFields.Fields[i].DataType;
  for i:= 0 to High(FTypes.GetCaption(tf)) do
    Panels[cb.Tag].CBKind.AddItem(FTypes.GetCaption(tf)[i], TObject(IntPtr(tf)));
end;

procedure TFilter.ButtonRemoveClick(Sender: TObject);
var i: integer;
begin
  if length(Panels) = 1 Then exit;
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
  s, st: string;
begin
  s := '';
  k := 0;
  setLength(result, length(Panels)+1);
  for i:= 0 to High(Panels) do with Panels[i] do begin
    if (CBField.ItemIndex = -1) or (CBKind.ItemIndex = -1) or (Edit.Text = '') Then continue;
    if k <> 0 Then st := ' AND ';
    s += st+'"'+FFields.Fields[CBField.ItemIndex].FieldName+'" ';
    t:= CBKind.ItemIndex;
    s += FTypes.GetValue(TFieldType(IntPtr(CBKind.Items.Objects[t])))[t];
    inc(k);
    result[k] := Edit.Text;
  end;
  setLength(result, k+1);
  result[0] := s;
end;

constructor TFilter.Create(AFields: TFields);
begin
  setLength(Panels, 0);
  FFields := AFields;
end;

initialization

  FTypes:= TFilterTypesContainer.Create;

end.

