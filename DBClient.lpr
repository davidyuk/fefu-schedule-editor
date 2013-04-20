program DBClient;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, CLFormMain, CLFormTable, CLFormContainer, CLFormChild,
  CLDatabase, CLBooks, CLFormEdit
  { you can add units after this };

{$R *.res}

begin
  //RequireDerivedFormResource := True;
  Application.Initialize;

  Database := TDatabase.Create();
  Books := TBooks.Create();

  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

