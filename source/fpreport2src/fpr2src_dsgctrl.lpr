program fpr2src_dsgctrl;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, fpr2src_dsgctrl_main, fpr2src_reader_report, fpreportdesignobjectlist
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFrmfpr2src, Frmfpr2src);
  Application.Run;
end.

