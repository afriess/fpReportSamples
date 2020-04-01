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
  Menus, ComCtrls, StdActns, mrumanager, fpreport;

type

  { TFrmfpr2src }

  TFrmfpr2src = class(TForm)
    ActConvertExporter: TAction;
    Action1: TAction;
    ActionList1: TActionList;
    BuExporter: TButton;
    CBShowSource: TCheckBox;
    EdtSrc: TEdit;
    EdtDest: TEdit;
    FileExit: TFileExit;
    FileOpen: TFileOpen;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MnuOpen: TMenuItem;
    MnuClose: TMenuItem;
    MnuMRU: TMenuItem;
    MnuMainFile: TMenuItem;
    MenuItem2: TMenuItem;
    MRUMenuManager: TMRUMenuManager;
    StatusBar1: TStatusBar;
    procedure ActConvertExporterExecute(Sender: TObject);
    procedure FileOpenAccept(Sender: TObject);
    procedure MRUMenuManagerRecentFile(Sender: TObject; const AFileName: String
      );
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
end;


procedure TFrmfpr2src.ActConvertExporterExecute(Sender: TObject);
var
  str : String;
  sl: TStringList;
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
  LoadExportReport(EdtSrc.Text);
  if CBShowSource.Checked then begin
    Memo1.Append('---------- start ------------------');
    Memo1.Append('');
    str:=Memo1.Lines.DelimitedText+GetSourceCode;
    Memo1.Lines.DelimitedText:=str;
    Memo1.Append('');
    Memo1.Append('----------- end  -----------------');
  end;
  Memo1.Append('report name='+FSrcReport.Name);
  Memo1.Append('convert report');
  if FileExists(EdtDest.Text) then begin
    Memo1.Append('File:'+EdtDest.Text+' exists - exit');
    Memo1.Append('exporter finished with error');
  end
  else begin
    sl := TStringList.Create;
    try
      sl.DelimitedText:= GetSourceCode;
      sl.SaveToFile(EdtDest.Text);
      Memo1.Append('Saved as:'+EdtDest.Text);
    finally
      sl.Free;
    end;
    Memo1.Append('exporter finished');
  end;
end;

procedure TFrmfpr2src.FileOpenAccept(Sender: TObject);
begin
  if Sender is TFileOpen then begin
    EdtSrc.Text:= TFileOpen(Sender).Dialog.FileName;
    MRUMenuManager.AddToRecent(EdtSrc.Text);
    EdtDest.Text:= ChangeFileExt(EdtSrc.Text,'.pas');
  end;
end;

procedure TFrmfpr2src.MRUMenuManagerRecentFile(Sender: TObject;
  const AFileName: String);
begin

end;

end.

