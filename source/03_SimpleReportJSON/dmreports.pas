unit dmreports;

{$mode objfpc}{$H+}

{$define ColorBands}

interface

uses
  Classes, SysUtils, FileUtil, process, Forms, Controls, Graphics, Dialogs,
  StdCtrls, fpreportlclexport, FPReportDesigner, ExtCtrls, ComCtrls, LCLIntf,
  fpreport, fpreportdata, fpreportdb, fpjsonreport, DB,
  csvdataset, jsonparser, LCLProc;

type

  { TReports }

  TReports = class(TDataModule)
    CSVDataset: TCSVDataset;
    DataSource: TDataSource;
    FPJSONReport: TFPJSONReport;
    FPReportDatasetData: TFPReportDatasetData;
    FPReportDesigner1: TFPReportDesigner;
    OpenDialog: TOpenDialog;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FInit: boolean;
    FReportPath: string;
    // Report
    FReport: TFPJSONReport;
    FDataParent: TComponent;
    lReportData: TFPReportDataManager;
    sl: TStringList;
    // canvas Exporter
    rptExporter: TFPReportExportCanvas;
    procedure ActivateReport;
    function CheckReport: boolean;
    procedure CreateDataReport(FN: string);
    procedure CleanUp;
  public
    procedure CSVtoPDFReport;
    procedure Extdesigner;
    property ReportPath: string read FReportPath write FReportPath;
  end;

var
  Reports: TReports;

implementation

uses
  fpjson,
  fpTTF,
  fpreportdatacsv,
  fpreportpdfexport;

{$R *.lfm}

{ TReports }

procedure TReports.DataModuleCreate(Sender: TObject);
begin
  FReport := nil;
  FDataParent := nil;
  lReportData := nil;
  rptExporter := nil;
  sl := nil;
  FInit := False;
end;

procedure TReports.DataModuleDestroy(Sender: TObject);
begin
  CleanUp;
end;

procedure TReports.ActivateReport;
begin
  ReportPath := Application.Location + 'DATA/Reports/Adressen';

  if not FInit then
  begin
    gTTFontCache.ReadStandardFonts;
    gTTFontCache.BuildFontCache;
    if PaperManager.PaperCount = 0 then
      PaperManager.RegisterStandardSizes;
    FInit := True;
  end;
end;

function TReports.CheckReport: boolean;
var
  Filename: string;
  fe: string;
begin
  OpenDialog.InitialDir := ReportPath;
  OpenDialog.Execute;
  Filename := OpenDialog.FileName;
  fe := ExtractFileExt(Filename);
  if FileExists(Filename) and (fe = '.json') then
  begin
    if not Assigned(Freport) then
      CreateDataReport(Filename);
    if not Freport.Prepared then
      FReport.RunReport;
    Result := True;
  end
  else
    Result := False;
end;

procedure TReports.CreateDataReport(FN: string);
var
  fs: TFileStream;
  AllJSON, DesignDataJSON: TJSONObject;
  t: string;
begin
  // Clear old reports
  CleanUp;

  fs := TFileStream.Create(FN, fmOpenRead or fmShareDenyNone);
  try
    // ReportData AND DesignData
    AllJSON := TJSONObject(GetJSON(fs));

    // Extract DesignData only
    DesignDataJSON := AllJSON.Get('DesignData', TJSONObject(nil));

    // Create the Data for the report before the report
    lReportData := TFPReportDataManager.Create(self);
    lReportData.LoadFromJSON(DesignDataJSON);

    // Read the report
    FReport := TFPJSONReport.Create(Self);
    FReport.DataManager := lReportData;
    FReport.LoadFromJSON(AllJSON);
  finally
    AllJSON.Free;
    fs.Free;
  end;
end;

procedure TReports.CleanUp;
begin
  if Assigned(rptExporter) then
    FreeAndNil(rptExporter);
  if Assigned(FReport) then
  begin
    FReport.Clear(True);
    FreeAndNil(FReport);
  end;
  if Assigned(FDataParent) then
    FreeAndNil(FDataParent);
  if Assigned(lReportData) then
  begin
    FreeAndNil(lReportData);
  end;
  if Assigned(sl) then
    FreeAndNil(sl);
end;

procedure TReports.CSVtoPDFReport;
var
  P: TFPReportExportPDF;
  output: string;
begin
  try
    ActivateReport;
    P := TFPReportExportPDF.Create(nil);
    if CheckReport then
    begin
      P.FileName := ExtractFileNameWithoutExt(OpenDialog.FileName) + '.pdf';
      P.Report := FReport;
      P.AutoRun := True;
      FReport.RenderReport(P);
      OpenDocument(P.FileName);
    end;
    CleanUp;
  finally
    P.Free;
  end;
end;

procedure TReports.Extdesigner;
var
  Filename: string;
  fe: string;
begin
  ActivateReport;
  OpenDialog.InitialDir := ReportPath;
  OpenDialog.Execute;
  Filename := OpenDialog.FileName;
  RunCommand(Application.Location + 'ReportDesign "' + Filename + '"', fe);
end;


end.




