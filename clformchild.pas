unit CLFormChild;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  ExtCtrls, StdCtrls, sqldb, db, strutils;

type

  { TFormChild }

  TFormChild = class(TForm)
    procedure FormChangeBounds(Sender: TObject);
  private
    const MDIMargin = 25;
  protected
    FBookId, FRecordId: integer;
  public
    property BookId: integer read FBookId;
    property RecordId: integer read FRecordId;
    procedure UpdateSize;
    constructor Create(TheOwner: TComponent); virtual;
  end;

implementation

{$R *.lfm}

{ TFormChild }

procedure TFormChild.FormChangeBounds(Sender: TObject);
begin
  BringToFront;
end;

procedure TFormChild.UpdateSize;
begin
  if WindowState = wsMaximized Then begin
    //Visible:= False;
    WindowState := wsNormal;
    WindowState:= wsMaximized;
    //ShowWindow(Handle, 0);
    //InvalidatePreferredSize;
    //Invalidate;
    //Width:= Parent.Width-100;
    //Height:= Parent.Height-100;
  end;
  if Top > (Parent.Height-MDIMargin) Then Top := Parent.Height-MDIMargin;
  if Left > (Parent.Width-MDIMargin) Then Left := Parent.Width-MDIMargin;
end;

constructor TFormChild.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

end.

