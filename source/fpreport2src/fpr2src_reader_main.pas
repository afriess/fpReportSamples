unit fpr2src_reader_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ActnList,
  Menus, ComCtrls, fpreport, fpr2src_reader_report;

type

  { TFrmfpr2src }

  TFrmfpr2src = class(TForm)
    ActConvertExporter: TAction;
    ActionList1: TActionList;
    BuExporter: TButton;
    EdtSrc: TEdit;
    EdtDest: TEdit;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem2: TMenuItem;
    StatusBar1: TStatusBar;
    procedure ActConvertExporterExecute(Sender: TObject);
  private
    FSrcReport: TReaderFPReport;
    procedure LoadExportReport(AFilename: string);
  public

  end;

var
  Frmfpr2src: TFrmfpr2src;

implementation

uses
  fpjson, FPReportStreamer;

{$R *.lfm}

{ TFrmfpr2src }

// ********************************************************
// ************         Exporter Test       ***************
// ********************************************************

procedure TFrmfpr2src.LoadExportReport(AFilename:string);
var
  rs: TFPReportJSONStreamer;
  fs: TFileStream;
  lJSON: TJSONObject;
  str : String;
begin
  // read from stream
  fs := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
  try
    lJSON := TJSONObject(GetJSON(fs));
  finally
    FreeAndNil(fs);
  end;
  // read report from JSON
  rs := TFPReportJSONStreamer.Create(nil);
  rs.JSON := lJSON; // rs takes ownership of lJSON
  try
    FSrcReport.ParentName:= 'Report';
    FSrcReport.ReaderName:= 'FReport';
    FSrcReport.ReaderElement(rs);
  finally
    FreeAndNil(rs);
  end;
  //
  FSrcReport.AutoSave:=false;
  Memo1.Append('');
  str:=Memo1.Lines.DelimitedText+FSrcReport.SourceCode;
  Memo1.Lines.DelimitedText:=str;
  Memo1.Append('');
end;


procedure TFrmfpr2src.ActConvertExporterExecute(Sender: TObject);
begin
  Memo1.Clear;
  Memo1.Append('Start Exporter');
  // Create a clear report
  if Assigned(FSrcReport) then
      FreeAndNil(FSrcReport);
  FSrcReport:= TReaderFPReport.Create(Self);
  //
  if not FileExists(EdtSrc.Text) then begin
    Memo1.Append('File:'+EdtSrc.Text+' not found');
  end;
  Memo1.Append('load report');
  Memo1.Append('---------- start ------------------');
  LoadExportReport(EdtSrc.Text);
  Memo1.Append('----------- end  -----------------');
  Memo1.Append('report name='+FSrcReport.Name);
  Memo1.Append('convert report');
  if FileExists(EdtDest.Text) then begin
    Memo1.Append('File:'+EdtSrc.Text+' exists - exit');
  end;
  Memo1.Append('exporter finished');
end;

end.

