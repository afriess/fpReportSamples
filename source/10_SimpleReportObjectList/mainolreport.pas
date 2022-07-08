unit mainOLreport;

{$mode objfpc}{$H+}

{$define ColorBands}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  fpreport, ExtCtrls, ComCtrls, fpreportcontnr, Contnrs{, fpreportlclexport};

const
  TESTIMAGE = 'powered_by.png';

type

  TDataMain = Class
  private
    FInfo: string;
  published
    property Info: string read FInfo write FInfo;
  end;

  { TOneA }

  TOneA = Class
  private
    FDateA: TDateTime;
    FInfoA: string;
    FValueA: Double;
  published
    property DateA: TDateTime read FDateA write FDateA;
    property InfoA: string read FInfoA write FInfoA;
    property ValueA: Double read FValueA write FValueA;
  end;

  { TOneB }

  TOneB = Class
  private
    FDateB: TDateTime;
    FInfoB: string;
    FValueB: Double;
  published
    property DateB: TDateTime read FDateB write FDateB;
    property InfoB: string read FInfoB write FInfoB;
    property ValueB: Double read FValueB write FValueB;
  end;

  { TFrmSimpleReporOL }

  TFrmSimpleReporOL = class(TForm)
    BuRenderCanvas: TButton;
    BuRenderPrerview: TButton;
    BuRenderPdf: TButton;
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
    FReport: TFPReport;
    FDataParent : TComponent;
    lReportData : TFPReportUserData;
    // ObjectList start
    lReportOLMain,
    lReportOLData1,
    lReportOLData2: TFPReportObjectListData; // in fpreportcontnr !!
    OL1,
    OL2: TFPObjectList;
    // Objectlist stop
    sl: TStringList;
    // canvas Exporter
    //rptExporter : TFPReportExportCanvas;
    procedure CheckReport;
    procedure CreateDemoReport;
    procedure InitialiseDataMain;
    procedure InitialiseDataA;
    procedure InitialiseDataB;
    procedure CleanUp;
    procedure ButtonSet(state:Boolean);
    procedure SaveDesignToFile(AFileName: string);
  public

  end;

var
  FrmSimpleReporOL: TFrmSimpleReporOL;

implementation

uses
  fpTTF,
  //fpreportformexport,
  fpreportpdfexport,
  fpReportStreamer,
  fpjson;

{$R *.lfm}

procedure TFrmSimpleReporOL.PanelRenderPaint(Sender: TObject);
begin
  //rptExporter.Execute;
end;

procedure TFrmSimpleReporOL.UpDown1Changing(Sender: TObject; var AllowChange: Boolean);
begin
  //if (UpDown1.Position > UpDown1.Max)
  //   or (UpDown1.Position < UpDown1.Min) then begin
  //  AllowChange:=false;
  //  exit;
  //end;
  //TFPReportExportCanvas(rptExporter).PageIndex:=UpDown1.Position;
  //lblPage.Caption:= 'Page: ' + IntToStr(TFPReportExportCanvas(rptExporter).PageIndex);
  //Invalidate;
end;

procedure TFrmSimpleReporOL.BuRenderCanvasClick(Sender: TObject);
begin
  //ButtonSet(false);
  //try
  //  CheckReport;
  //  if Not Assigned(rptExporter) then
  //    rptExporter:= TFPReportExportCanvas.Create(nil);
  //  rptExporter.Report:= FReport;
  //  rptExporter.AutoRun:=True;
  //  rptExporter.Canvas:= PanelRender.Canvas;
  //  FReport.RenderReport(rptExporter);
  //  UpDown1.Min:= 0;
  //  UpDown1.Max:= rptExporter.PageCount-1;
  //  UpDown1.Position:= 0;
  //  rptExporter.PageIndex:=UpDown1.Position;
  //  lblPage.Caption:= 'Page: ' + IntToStr(rptExporter.PageIndex);
  //  PanelRender.OnPaint:= @PanelRenderPaint;
  //  Invalidate;
  //finally
  //  ButtonSet(true);
  //end;
end;

procedure TFrmSimpleReporOL.BuRenderPdfClick(Sender: TObject);

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

procedure TFrmSimpleReporOL.CheckReport;

begin
  if Not Assigned(Freport) then
    CreateDemoReport;
  if not Freport.Prepared then
    FReport.RunReport;
end;

procedure TFrmSimpleReporOL.BuRenderPrerviewClick(Sender: TObject);

//Var
//  P : TFPreportPreviewExport;

begin
  //P:=Nil;
  //ButtonSet(false);
  //try
  //  CheckReport;
  //  P:=TFPreportPreviewExport.Create(Self);
  //  P.Report:= FReport;
  //  P.AutoRun:=True;
  //  FReport.RenderReport(P);
  //finally
  //  P.Free;
  //  ButtonSet(true);
  //end;
end;

procedure TFrmSimpleReporOL.FormActivate(Sender: TObject);
begin
  if not FInit then begin
    gTTFontCache.ReadStandardFonts;
    gTTFontCache.BuildFontCache;
    if PaperManager.PaperCount=0 then
      PaperManager.RegisterStandardSizes;
    FInit:= true;
  end;
end;

procedure TFrmSimpleReporOL.FormCreate(Sender: TObject);
begin
  FReport:= nil;
  FDataParent:= nil;
  lReportData:= nil;
  //rptExporter:= nil;
  sl:= nil;
  FInit:= False;
  // OL Data
  lReportOLData1:= nil;
  lReportOLData2:= nil;
  OL1:=nil;
  OL2:=nil;
end;

procedure TFrmSimpleReporOL.FormDestroy(Sender: TObject);
begin
  CleanUp;
end;


procedure TFrmSimpleReporOL.CleanUp;
begin
  //if Assigned(rptExporter) then FreeAndNil(rptExporter);
  if Assigned(FReport) then FreeAndNil(FReport);
  if Assigned(FDataParent) then FreeAndNil(FDataParent);
  if Assigned(lReportData) then FreeAndNil(lReportData);
  if Assigned(sl) then FreeAndNil(sl);
end;

procedure TFrmSimpleReporOL.ButtonSet(state: Boolean);
begin
  BuRenderCanvas.Enabled:= state;
  BuRenderPrerview.Enabled:= state;
  BuRenderPdf.Enabled:= state;
  Application.ProcessMessages;
end;

procedure TFrmSimpleReporOL.SaveDesignToFile(AFileName : string);

Var
  WS: TFPReportJSONStreamer;
  S : UTF8String;
  FS : TFileStream;
  DD : TJSONObject;

begin
  FS:=Nil;
  ws := TFPReportJSONStreamer.Create(nil);
  try
    // Write report
    WS.JSON:=TJSONObject.Create;
    FReport.WriteElement(WS);
    //if false then
    //  begin
    //  // Add design data
    //  DD:=TJSONObject.Create;
    //  WS.JSon.Add('DesignData',DD);
    //  FReportDesignData.SaveToJSON(DD);
    //  end;
    // Now save to file
    fs:=TFileStream.Create(AFilename, fmCreate);
    S:=WS.JSON.FormatJSON();
    fs.WriteBuffer(S[1],Length(S));
  finally
    FreeAndNil(fs);
    FreeAndNil(ws);
  end;
end;


procedure TFrmSimpleReporOL.CreateDemoReport;

const
{$ifdef Windows}
  defaultFont = 'ArialMT';
{$else}
  defaultFont = 'Ubuntu';
{$endif}

var
  p: TFPReportPage;
  TitleBand: TFPReportTitleBand;
  DataBandM, DataBand, Databand2: TFPReportDataBand;
  Memo, Memo2: TFPReportMemo;
  PageFooter: TFPReportPageFooterBand;
  ColumnBand: TFPReportColumnHeaderBand;
  DataFooterBand: TFPReportDataFooterBand;
  d1,d2: TFPReportDataItem;
begin
  CleanUp;

  //lReportData := TFPReportUserData.Create(Self);
  //lReportData.Name:='UserData';
  //lReportData.OnGetValue := @GetReportDataValue;
  //lReportData.OnGetEOF := @GetReportDataEOF;
  //lReportData.OnFirst := @GetReportDataFirst;
  //lReportData.OnGetNames := @GetReportDataNames;
  //lReportData.InitFieldDefs;
  //InitialiseData;

  InitialiseDataMain;
  InitialiseDataA;
  InitialiseDataB;

  FReport:=TFPReport.Create(Self);
  FReport.ReportData.AddReportData(lReportOLMain);
  FReport.ReportData.AddReportData(lReportOLData1);
  FReport.ReportData.AddReportData(lReportOLData2);
  FReport.Author := 'Andreas';
  FReport.Title := 'LCL Report Demo';
  FReport.Variables.AddVariable('Var1').AsString:='Value1';
  FReport.Variables.AddVariable('Var2').AsString:='Value2';

  p := TFPReportPage.Create(FReport);
  p.Orientation := poPortrait;
  p.PageSize.PaperName := 'A4';
  { page margins }
  p.Margins.Left := 20;
  p.Margins.Top := 20;
  p.Margins.Right := 10;
  p.Margins.Bottom := 20;
  p.Data := lReportOLMain;

  p.ColumnCount:= 2;
  p.ColumnGap:= 5;

  TitleBand := TFPReportTitleBand.Create(p);
  TitleBand.Name:= 'TitelBand01';
  TitleBand.Layout.Height := 30;
  TitleBand.Frame.Shape := fsRectangle;
  TitleBand.Frame.BackgroundColor := clNone;

  Memo := TFPReportMemo.Create(TitleBand);
  Memo.UseParentFont:=False;
  Memo.Layout.Top := 0;
  Memo.Layout.Height := 10;
  Memo.Layout.Left := Memo.Layout.Height + 5; // to make room for the image
  Memo.Layout.Width := TitleBand.Layout.Width - (Memo.Layout.Height + 5);
  Memo.Font.Name := defaultFont;
  Memo.Font.Size:= 20;
  Memo.Text := 'Demo Titleband Memo1';
  Memo.StretchMode := smActualHeight;
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taCentered;
  Memo.Frame.Shape := fsRectangle;
  Memo.Frame.BackgroundColor:= clLtGray;

  With TFPReportImage.Create(TitleBand) do
    begin
    Layout.Left := 0;
    Layout.Top := 0;
    Layout.Width := Memo.Layout.Height;
    Layout.Height := Memo.Layout.Height;
    Stretched:=True;
    LoadFromFile(ExtractFilePath(ParamStr(0))+TESTIMAGE);
    end;

  Memo := TFPReportMemo.Create(TitleBand);
  Memo.UseParentFont:=False;
  Memo.Layout.Left := 0;
  Memo.Layout.Top := 10;
  Memo.Layout.Width := TitleBand.Layout.Width;
  Memo.Layout.Height := 5;
  Memo.Font.Name := defaultFont;
  Memo.Font.Size:= 12;
  Memo.Text := 'Demo Titleband Memo2';
  Memo.StretchMode := smActualHeight;
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taLeftJustified;
  Memo.Frame.Shape := fsRectangle;

  Memo := TFPReportMemo.Create(TitleBand);
  Memo.UseParentFont:=False;
  Memo.Layout.Left := 0;
  Memo.Layout.Top := 15;
  Memo.Layout.Width := TitleBand.Layout.Width;
  Memo.Layout.Height := 5;
  Memo.Font.Name := defaultFont;
  Memo.Font.Size:= 12;
  Memo.Text := 'Demo Titleband Memo3';
  Memo.StretchMode := smActualHeight;
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taLeftJustified;
  Memo.Frame.Shape := fsRectangle;

  Memo := TFPReportMemo.Create(TitleBand);
  Memo.UseParentFont:=False;
  Memo.Layout.Left := 0;
  Memo.Layout.Top := 20;
  Memo.Layout.Width := TitleBand.Layout.Width;
  Memo.Layout.Height := 5;
  Memo.Font.Name := defaultFont;
  Memo.Font.Size:= 12;
  Memo.Text := 'Date: ' + FormatDateTime('DD.MM.YYYY HH:MM',now);
  Memo.StretchMode := smActualHeight;
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taLeftJustified;
  Memo.Frame.Shape := fsRectangle;

  //DataBandM := TFPReportDataBand.Create(p);
  //DataBandM.Name:= 'DBBandMain01';
  //DataBandM.Layout.Height := 10;
  //DataBandM.Data:= lReportOLMain;

  DataBand := TFPReportDataBand.Create(p);
  DataBand.Name:= 'DBBand01';
  DataBand.Layout.Height := 10;
  Databand.MasterBand:= nil;
  DataBand.Detached:= true;
  DataBand.Data:= lReportOLData1;

  Memo := TFPReportMemo.Create(DataBand);
  Memo.Layout.Left := 5;
  Memo.Layout.Top := 0;
  Memo.Layout.Width := 60;
  Memo.Layout.Height := 5;
  Memo.Font.Name := defaultFont;
  Memo.Text := '[Data1.InfoA] - [Data1.ValueA]';

  DataBand2 := TFPReportDataBand.Create(p);
  DataBand2.Name:= 'DBBand02';
  DataBand2.Layout.Height := 10;
  Databand2.MasterBand:= nil;
  Databand2.Detached:= true;
  DataBand2.Data:= lReportOLData2;

  Memo2 := TFPReportMemo.Create(DataBand2);
  Memo2.Layout.Left := 5;
  Memo2.Layout.Top := 0;
  Memo2.Layout.Width := 60;
  Memo2.Layout.Height := 5;
  Memo2.Font.Name := defaultFont;
  Memo2.Text := '[Data2.InfoB] - [Data2.ValueB]';

  PageFooter := TFPReportPageFooterBand.Create(p);
  PageFooter.Name:= 'PageFooter01';
  PageFooter.Layout.Height := 10;

  Memo := TFPReportMemo.Create(PageFooter);
  Memo.Layout.Left := 135;
  Memo.Layout.Top := 13;
  Memo.Layout.Width := 15;
  Memo.Layout.Height := 5;
  Memo.Options:=[moDisableWordWrap];
  Memo.Font.Name := defaultFont;
  Memo.Text := 'Page [PageNo] ';

  //SaveDesignToFile('./Test.json');
end;


procedure TFrmSimpleReporOL.InitialiseDataMain;
var
  o : TDataMain;
  i: Integer;
  OL : TFPObjectList;
begin
  if Assigned(lReportOLMain) then
    lReportOLMain.Free;
  //
  lReportOLMain:= TFPReportObjectListData.Create(self);
  lReportOLMain.OwnsList:= true;
  OL := TFPObjectList.Create;
  OL.OwnsObjects:=true;
  for I:= 1 to 1 do begin
    o := TDataMain.Create;
    o.Info:= Format('Data%d', [i]);
    OL.Add(o);
  end;
  lReportOLMain.List:=OL;
  lReportOLMain.Name:='Main';
end;


procedure TFrmSimpleReporOL.InitialiseDataA;
var
  o : TOneA;
  i: Integer;
begin
  if Assigned(lReportOLData1) then
    lReportOLData1.Free;
  //
  lReportOLData1:= TFPReportObjectListData.Create(self);
  lReportOLData1.OwnsList:= true;
  OL1 := TFPObjectList.Create;
  OL1.OwnsObjects:=true;
  for i:= 1 to 3 do begin
    o := TOneA.Create;
    o.DateA:= now;
    o.InfoA:= Format('Item %d', [i]);
    o.ValueA:= i * 7.5;
    OL1.Add(o);
  end;
  lReportOLData1.List:=OL1;
  lReportOLData1.Name:='Data1';
  //lReportOLData1.InitFieldDefs;

end;

procedure TFrmSimpleReporOL.InitialiseDataB;
var
  o : TOneB;
  i: Integer;
begin
  if Assigned(lReportOLData2) then
    lReportOLData2.Free;
  //
  lReportOLData2:= TFPReportObjectListData.Create(self);
  lReportOLData2.OwnsList:= true;
  OL2 := TFPObjectList.Create;
  OL2.OwnsObjects:=true;
  for i:= 1 to 5 do begin
    o := TOneB.Create;
    o.DateB:= now;
    o.InfoB:= Format('Value %d', [i]);
    o.ValueB:= i * 1.5;
    OL2.Add(o);
  end;
  lReportOLData2.List:=OL2;
  lReportOLData2.Name:='Data2';
  //lReportOLData2.InitFieldDefs;
end;





end.

