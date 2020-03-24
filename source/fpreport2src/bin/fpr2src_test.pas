unit fpr2src_test
// Created with fp2srcexporter (C) Andreas Friess 2020
{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, fpReport;

  function CreateReport:TFPReport;

implementation


function CreateReport:TFPReport;
var
  p: TFPReportPage;
  TitleBand: TFPReportTitleBand;
  DataBand: TFPReportDataBand;
  Memo: TFPReportMemo;
  PageFooter: TFPReportPageFooterBand;
  ColumnBand: TFPReportColumnHeaderBand;
  DataFooterBand: TFPReportDataFooterBand;
begin
  //
end;

end.
