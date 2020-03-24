unit fpr2src_streamer_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ActnList,
  Menus, ComCtrls, fpreport;

type

  { TFrmfpr2src }

  TFrmfpr2src = class(TForm)
    ActConvertStreamer: TAction;
    ActionList1: TActionList;
    BuStreamer: TButton;
    EdtSrc: TEdit;
    EdtDest: TEdit;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    StatusBar1: TStatusBar;
    procedure ActConvertStreamerExecute(Sender: TObject);
  private
    FReport: TFPReport;
    procedure ConvertReport(AFilename: string);
    procedure LoadReport(AFilename: string);
  public

  end;

var
  Frmfpr2src: TFrmfpr2src;

implementation

uses
  fpjson, fpReportStreamer, fpr2srcstreamer;

{$R *.lfm}

{ TFrmfpr2src }

// ********************************************************
// ************         Streamer Test       ***************
// ********************************************************

procedure TFrmfpr2src.ConvertReport(AFilename:string);
var
  ws : TFPReportSRCStreamer;
  t : String;
begin
  //
  ws:= TFPReportSRCStreamer.Create(nil);
  try
    FReport.WriteElement(ws);
    Memo1.Append('');
    Memo1.Append('-------------- generated Source start -------------- ');
    Memo1.Append('');
    t:= Memo1.Lines.DelimitedText;
    t:= t + ws.SourceCode;
    Memo1.Lines.DelimitedText:= t;
    Memo1.Append('');
    Memo1.Append('-------------- generated Source end  -------------- ');
    Memo1.Append('');
  finally
    ws.Free;
  end;
end;

procedure TFrmfpr2src.LoadReport(AFilename:string);
var
  rs: TFPReportJSONStreamer;
  fs: TFileStream;
  lJSON: TJSONObject;
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
    FReport.ReadElement(rs);
  finally
    FreeAndNil(rs);
  end;
end;

procedure TFrmfpr2src.ActConvertStreamerExecute(Sender: TObject);
begin
  Memo1.Clear;
  Memo1.Append('Start Streamer');
  // Create a clear report
  if Assigned(FReport) then
      FreeAndNil(FReport);
  FReport:= TFPReport.Create(Self);
  //
  if not FileExists(EdtSrc.Text) then begin
    Memo1.Append('File:'+EdtSrc.Text+' not found');
  end;
  Memo1.Append('load report');
  LoadReport(EdtSrc.Text);
  Memo1.Append('report name='+FReport.Name);
  Memo1.Append('convert report');
  if FileExists(EdtDest.Text) then begin
    Memo1.Append('File:'+EdtSrc.Text+' exists - exit');
  end;
  ConvertReport(EdtDest.Text);
  Memo1.Append('streamer finished');
end;


end.

