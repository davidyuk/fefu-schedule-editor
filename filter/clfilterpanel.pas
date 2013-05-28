unit CLFilterPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, ExtCtrls, Controls, Buttons, CLMetadata, CLFilterTypes;

type
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

implementation

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

