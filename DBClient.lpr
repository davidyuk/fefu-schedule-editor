program DBClient;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, CLFormMain, CLFormTable, CLFormContainer, CLFormChild, CLDatabase,
  CLMetadata, CLFormEdit, CLFilter, clschedulecell, clexporttohtml, CLSchedule;

{$R *.res}

begin
  //RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

