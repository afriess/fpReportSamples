unit mainLCLReport;

{$mode objfpc}{$H+}

{$define ColorBands}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  fpreportlclexport, ExtCtrls, ComCtrls, fpreport, fpreportdata, fpreportdb, fpjsonreport, DB,
  csvdataset;

const
  TESTIMAGE = 'powered_by.png';

type

  { TFrmSimpleReportLCL }

  TFrmSimpleReportLCL = class(TForm)
    BuRenderCanvas: TButton;
    BuRenderPrerview: TButton;
    BuRenderPdf: TButton;
    CSVDataset: TCSVDataset;
    DataSource: TDataSource;
    FPJSONReport: TFPJSONReport;
    FPReportDatasetData: TFPReportDatasetData;
    lblPage: TLabel;
    PanelRender: TPanel;
    panelMain: TPanel;
    UpDown1: TUpDown;
    procedure BuRenderCanvasClick(Sender: TObject);
    procedure BuRenderPdfClick(Sender: TObject);
    procedure BuRenderPrerviewClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PanelRenderPaint(Sender: TObject);
    procedure UpDown1Changing(Sender: TObject; var AllowChange: Boolean);
  private
    FInit: Boolean;
    // Report
    FReport: TFPJSONReport;
    FDataParent : TComponent;
    lReportData : TFPReportDataManager;
    sl: TStringList;
    // canvas Exporter
    rptExporter : TFPReportExportCanvas;
    procedure CheckReport;
    procedure CreateDemoReport(FN: string);
    procedure CleanUp;
    procedure ButtonSet(state:Boolean);
  public

  end;

var
  FrmSimpleReportLCL: TFrmSimpleReportLCL;

implementation

uses
  fpjson,
  fpTTF,
  fpreportdatacsv,
  fpreportformexport,
  fpreportpdfexport;

{$R *.lfm}

procedure TFrmSimpleReportLCL.PanelRenderPaint(Sender: TObject);
begin
  rptExporter.Execute;
end;

procedure TFrmSimpleReportLCL.UpDown1Changing(Sender: TObject; var AllowChange: Boolean);
begin
  if (UpDown1.Position > UpDown1.Max)
     or (UpDown1.Position < UpDown1.Min) then begin
    AllowChange:=false;
    exit;
  end;
  TFPReportExportCanvas(rptExporter).PageIndex:=UpDown1.Position;
  lblPage.Caption:= 'Page: ' + IntToStr(TFPReportExportCanvas(rptExporter).PageIndex);
  Invalidate;
end;

procedure TFrmSimpleReportLCL.BuRenderCanvasClick(Sender: TObject);
begin
  ButtonSet(false);
  try
    CheckReport;
    if Not Assigned(rptExporter) then
      rptExporter:= TFPReportExportCanvas.Create(nil);
    rptExporter.Report:= FReport;
    rptExporter.AutoRun:=True;
    rptExporter.Canvas:= PanelRender.Canvas;
    FReport.RenderReport(rptExporter);
    UpDown1.Min:= 0;
    UpDown1.Max:= rptExporter.PageCount-1;
    UpDown1.Position:= 0;
    rptExporter.PageIndex:=UpDown1.Position;
    lblPage.Caption:= 'Page: ' + IntToStr(rptExporter.PageIndex);
    PanelRender.OnPaint:= @PanelRenderPaint;
    Invalidate;
  finally
    ButtonSet(true);
  end;
end;

procedure TFrmSimpleReportLCL.BuRenderPdfClick(Sender: TObject);

Var
   P : TFPReportExportPDF;

begin
  ButtonSet(false);
  try
    CheckReport;
    P:=TFPReportExportPDF.Create(nil);
    P.FileName:= ApplicationName+'-report.pdf';
    P.Report:= FReport;
    P.AutoRun:=True;
    FReport.RenderReport(P);
    ShowMessage('PDF created: '+P.FileName);
  finally
    P.Free;
    ButtonSet(true);
  end;
end;

procedure TFrmSimpleReportLCL.CheckReport;

begin
  if Not Assigned(Freport) then
    CreateDemoReport('Etiketten.json');
  if not Freport.Prepared then
    FReport.RunReport;
end;

procedure TFrmSimpleReportLCL.BuRenderPrerviewClick(Sender: TObject);

Var
  P : TFPreportPreviewExport;

begin
  P:=Nil;
  ButtonSet(false);
  try
    CheckReport;
    P:=TFPreportPreviewExport.Create(Self);
    P.Report:= FReport;
    P.AutoRun:=True;
    FReport.RenderReport(P);
  finally
    P.Free;
    ButtonSet(true);
  end;
end;

procedure TFrmSimpleReportLCL.FormActivate(Sender: TObject);
begin
  if not FInit then begin
    gTTFontCache.ReadStandardFonts;
    gTTFontCache.BuildFontCache;
    if PaperManager.PaperCount=0 then
      PaperManager.RegisterStandardSizes;
    FInit:= true;
  end;
end;

procedure TFrmSimpleReportLCL.FormCreate(Sender: TObject);
begin
  FReport:= nil;
  FDataParent:= nil;
  lReportData:= nil;
  rptExporter:= nil;
  sl:= nil;
  FInit:= False;
end;

procedure TFrmSimpleReportLCL.FormDestroy(Sender: TObject);
begin
  CleanUp;
end;


procedure TFrmSimpleReportLCL.CleanUp;
begin
  if Assigned(rptExporter) then FreeAndNil(rptExporter);
  if Assigned(FReport) then begin
    FReport.Clear(true);
    FreeAndNil(FReport);
  end;
  if Assigned(FDataParent) then FreeAndNil(FDataParent);
  if Assigned(lReportData) then begin
    FreeAndNil(lReportData);
  end;
  if Assigned(sl) then FreeAndNil(sl);
end;

procedure TFrmSimpleReportLCL.ButtonSet(state: Boolean);
begin
  BuRenderCanvas.Enabled:= state;
  BuRenderPrerview.Enabled:= state;
  BuRenderPdf.Enabled:= state;
  Application.ProcessMessages;
end;

// Hint:
//   The Designdata can use a own path or the path is relative
//   so it is possible not to load the data correct, because it is not found

procedure TFrmSimpleReportLCL.CreateDemoReport(FN: string);
var
  fs : TFileStream;
  AllJSON, DesignDataJSON: TJSONObject;
begin
  // Clear old reports
  CleanUp;

  fs:= TFileStream.Create(FN, fmOpenRead or fmShareDenyNone);
  try
    // ReportData AND DesignData
    AllJSON:= TJSONObject(GetJSON(fs));

    // Extract DesignData only
    DesignDataJSON:= AllJSON.Get('DesignData',TJSONObject(nil));

    // Create the Data for the report before the report
    lReportData:= TFPReportDataManager.Create(self);
    lReportData.LoadFromJSON(DesignDataJSON);

    // Read the report
    FReport:=TFPJSONReport.Create(Self);
    FReport.DataManager:= lReportData;
    FReport.LoadFromJSON(AllJSON);
  finally
    AllJSON.Free;
    fs.Free;
  end;
end;

end.

