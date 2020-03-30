unit fpr2srcreport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpreport;

type

  { TSrcFPReport }

  TSrcFPReport = class(TFPCustomReport)

  private
    FSrc: TStringList;
    FExtAutoSave: boolean;
    FExtFileName: string;
    FExtUnitName: string;
    function GetSourceCode: string;
    procedure SetupSrcDocument;
  protected
    procedure SetupReport;
    procedure CloseReport;
    procedure SaveToFile;
    procedure WriteReport;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure MakeSrc(ATopElement: TFPReportElement);
  published
    property  AutoSave: boolean read FExtAutoSave write FExtAutoSave default True;
    property  FileName: string read FExtFileName write FExtFileName;
    property  UnitName: string read FExtUnitName write FExtUnitName;
    property  SourceCode: string read GetSourceCode;
  end;




implementation

function TSrcFPReport.GetSourceCode: string;
begin
  if FSrc <> nil then
    Result := FSrc.DelimitedText
  else
    Result := '';
end;

procedure TSrcFPReport.SetupSrcDocument;
begin
  if Assigned(FSrc) then
    FSrc.free;
  FSrc:= TStringList.Create;
  FSrc.Sorted:=false;
end;

procedure TSrcFPReport.SetupReport;
begin
  FSrc.Add('unit '+ FExtUnitName);
  FSrc.Add('// Created with fp2srcexporter (C) Andreas Friess 2020');
  FSrc.Add('{$mode objfpc}{$H+}');
  FSrc.Add('');
  FSrc.Add('interface');
  FSrc.Add('uses');
  FSrc.Add('  Classes, SysUtils, fpReport;');
  FSrc.Add('');
  FSrc.Add('  procedure CreateReport(var FReport: TFPReport);');
  FSrc.Add('');
  FSrc.Add('implementation');
  FSrc.Add('');
  //FSrc.Add('uses');
  //FSrc.Add('  ');
  FSrc.Add('');
  FSrc.Add('procedure CreateReport(var FReport: TFPReport);');
  FSrc.Add('var');
  FSrc.Add('  p: TFPReportPage;');
  FSrc.Add('  TitleBand: TFPReportTitleBand;');
  FSrc.Add('  DataBand: TFPReportDataBand;');
  FSrc.Add('  Memo: TFPReportMemo;');
  FSrc.Add('  PageFooter: TFPReportPageFooterBand;');
  FSrc.Add('  ColumnBand: TFPReportColumnHeaderBand;');
  FSrc.Add('  DataFooterBand: TFPReportDataFooterBand;');
  FSrc.Add('begin');
  FSrc.Add('  //');

end;

procedure TSrcFPReport.CloseReport;
begin
  FSrc.Add('end;');
  FSrc.Add('');
  FSrc.Add('end.');
  //FSrc.Add('');
  //FSrc.Add('');
  //FSrc.Add('');
end;

constructor TSrcFPReport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSrc:= nil;
  FExtUnitName:= ApplicationName + '_test';
  FExtFileName:= FExtUnitName+'.pas';
  FExtAutoSave:= true;
end;

destructor TSrcFPReport.Destroy;
begin
  if Assigned(FSrc) then
    FSrc.free;
  inherited Destroy;
end;

// borroed from fpreport
procedure TSrcFPReport.MakeSrc(ATopElement: TFPReportElement);
//var
//  i: integer;
begin
  SetupSrcDocument;
  SetupReport;
  WriteReport;
  //// ignore AOriginal here as we don't support whole report diffs, only element diffs
  //AWriter.PushElement('Report');
  //try
  //  inherited WriteElement(AWriter, ATopElement);
  //  // local properties
  //  AWriter.WriteString('Title', Title);
  //  AWriter.WriteString('Author', Author);
  //  AWriter.WriteBoolean('TwoPass',TwoPass);
  //  AWriter.WriteDateTime('DateCreated', DateCreated);
  //  // now the design-time images
  //  AWriter.PushElement('Images');
  //  try
  //    for i := 0 to Images.Count-1 do
  //    begin
  //      AWriter.PushElement(IntToStr(i)); // use image index as identifier
  //      try
  //        Images[i].WriteElement(AWriter);
  //      finally
  //        AWriter.PopElement;
  //      end;
  //    end;
  //  finally
  //    AWriter.PopElement;
  //  end;
  //  // now the pages
  //  AWriter.PushElement('Pages');
  //  try
  //    for i := 0 to PageCount - 1 do
  //    begin
  //      AWriter.PushElement(IntToStr(i)); // use page index as identifier
  //      try
  //        Pages[i].WriteElement(AWriter);
  //      finally
  //        AWriter.PopElement;
  //      end;
  //    end;
  //  finally
  //    AWriter.PopElement;
  //  end;
  //  AWriter.PushElement('Variables');
  //  try
  //    for i := 0 to Variables.Count - 1 do
  //      begin
  //        AWriter.PushElement(IntToStr(i)); // use variable index as identifier
  //        try
  //          Variables[i].WriteElement(AWriter);
  //        finally
  //          AWriter.PopElement;
  //        end;
  //      end;
  //  finally
  //    AWriter.PopElement;
  //  end;
  //finally
  //  AWriter.PopElement;
  //end;
  //// TODO: Implement writing OnRenderReport, OnBeginReport, OnEndReport
  CloseReport;
  if FExtAutoSave then
    SaveToFile;
end;

procedure TSrcFPReport.SaveToFile;
begin
  if not SameStr(FExtFileName, '') then begin
    { TODO -oAndi : Is autodelete of sourcefile Ok ?! }
    if FileExists(FExtFileName) then
      DeleteFile(FExtFileName);
    FSrc.SaveToFile(FExtFileName);
  end;
end;

procedure TSrcFPReport.WriteReport;
begin
  FSrc.Add('  FReport:=TFPReport.Create(Self);');
  FSrc.Add('  //FReport.ReportData.AddReportData(lReportData);');
  FSrc.Add('  FReport.Author:= '''+Author+''';');
  FSrc.Add('  FReport.DateCreated:= Now'+';');
  FSrc.Add('  FReport.Title:= '''+Title+''';');
  FSrc.Add('');
end;



end.

