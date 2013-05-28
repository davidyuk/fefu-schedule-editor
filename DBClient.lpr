program DBClient;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, CLFormMain, CLFormTable, CLFormContainer,
  CLFormChild, CLFormEdit, CLFilter, CLOLAPGrid, CLOLAPCell, clmetadata,
  cldatabase, CLExport, CLFormOLAP, CLOLAPCellButton, CLOLAPCellButtons, 
CLOLAPTypes, CLFilterPanel, CLFilterTypes;

{$R *.res}

begin
  //RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

