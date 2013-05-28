unit CLOLAPCellButtons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, CLOLAPCellButton, CLOLAPTypes;

type

  { TOLAPCellButtonShowFull }

  TOLAPCellButtonShowFull = class(TOLAPCellButton)
  public
    constructor Create(TheOwner: TComponent; AItemId: integer; ACallback: TOLAPButtonCallback); override;
  end;

  { TOLAPCellButtonAdd }

  TOLAPCellButtonAdd = class(TOLAPCellButton)
  public
    constructor Create(TheOwner: TComponent; AItemId: integer; ACallback: TOLAPButtonCallback); override;
  end;

  { TOLAPCellButtonRemove }

  TOLAPCellButtonRemove = class(TOLAPCellButton)
  public
    constructor Create(TheOwner: TComponent; AItemId: integer; ACallback: TOLAPButtonCallback); override;
  end;

  { TOLAPCellButtonEdit }

  TOLAPCellButtonEdit = class(TOLAPCellButton)
  public
    constructor Create(TheOwner: TComponent; AItemId: integer; ACallback: TOLAPButtonCallback); override;
  end;

  { TOLAPCellButtonOpenTable }

  TOLAPCellButtonOpenTable = class(TOLAPCellButton)
  public
    constructor Create(TheOwner: TComponent; AItemId: integer; ACallback: TOLAPButtonCallback); override;
  end;

implementation

var IconEdit, IconAdd, IconRemove, IconTable, IconFull: TPortableNetworkGraphic;

{ TOLAPCellButtonOpenTable }

constructor TOLAPCellButtonOpenTable.Create(TheOwner: TComponent;
  AItemId: integer; ACallback: TOLAPButtonCallback);
begin
  inherited Create(TheOwner, AItemId, ACallback);
  FIcon := IconTable;
  FButtonKind := obOpenTable;
end;

{ TOLAPCellButtonAdd }

constructor TOLAPCellButtonAdd.Create(TheOwner: TComponent; AItemId: integer;
  ACallback: TOLAPButtonCallback);
begin
  inherited Create(TheOwner, AItemId, ACallback);
  FIcon := IconAdd;
  FButtonKind := obAdd;
end;

{ TOLAPCellButtonRemove }

constructor TOLAPCellButtonRemove.Create(TheOwner: TComponent;
  AItemId: integer; ACallback: TOLAPButtonCallback);
begin
  inherited Create(TheOwner, AItemId, ACallback);
  FIcon := IconRemove;
  FButtonKind := obRemove;
end;

{ TOLAPCellButtonEdit }

constructor TOLAPCellButtonEdit.Create(TheOwner: TComponent; AItemId: integer;
  ACallback: TOLAPButtonCallback);
begin
  inherited Create(TheOwner, AItemId, ACallback);
  FIcon := IconEdit;
  FButtonKind := obEdit;
end;

{ TOLAPCellButtonShowFull }

constructor TOLAPCellButtonShowFull.Create(TheOwner: TComponent;
  AItemId: integer; ACallback: TOLAPButtonCallback);
begin
  inherited Create(TheOwner, AItemId, ACallback);
  FIcon := IconFull;
  FButtonKind := obShowFull;
end;

initialization

  IconAdd := TPortableNetworkGraphic.Create;
  IconEdit := TPortableNetworkGraphic.Create;
  IconFull := TPortableNetworkGraphic.Create;
  IconTable := TPortableNetworkGraphic.Create;
  IconRemove := TPortableNetworkGraphic.Create;
  IconAdd.LoadFromFile('./images/add.png');
  IconEdit.LoadFromFile('./images/edit.png');
  IconFull.LoadFromFile('./images/full.png');
  IconTable.LoadFromFile('./images/table.png');
  IconRemove.LoadFromFile('./images/remove.png');

finalization

  IconAdd.Free;
  IconEdit.Free;
  IconFull.Free;
  IconTable.Free;
  IconRemove.Free;

end.
