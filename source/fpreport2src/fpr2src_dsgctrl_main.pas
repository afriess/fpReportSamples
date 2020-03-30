{
    Copyright (c) 2020 Andreas Friess

    report definition to pascal source file.

    Licence LGPL with extension

    See the file COPYING.modified.LGPL for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpr2src_dsgctrl_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ActnList,
  Menus, ComCtrls, fpreport;

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
    FSrcReport: TFPReport;
    procedure LoadExportReport(AFilename: string);
  public

  end;

var
  Frmfpr2src: TFrmfpr2src;

implementation

uses
  fpjson, FPReportStreamer, fp2src_dsgnctrl;

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
  dsg: TSrcFPReportDesignerControl;
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
    //FSrcReport.ParentName:= 'Report';
    //FSrcReport.ReaderName:= 'FReport';
    FSrcReport.ReadElement(rs);
  finally
    FreeAndNil(rs);
  end;
  //
  Memo1.Append('Pages='+IntToStr(FSrcReport.PageCount));
  dsg := TSrcFPReportDesignerControl.Create(nil);
  try
    dsg.Page := FSrcReport.Pages[0];
  finally
    dsg.Free;
  end;
  //FSrcReport.AutoSave:=false;
  Memo1.Append('');
  str:=Memo1.Lines.DelimitedText+GetSourceCode;
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
  FSrcReport:= TFPReport.Create(Self);
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
