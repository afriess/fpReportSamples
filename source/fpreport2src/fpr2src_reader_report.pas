{
    Copyright (c) 2020 Andreas Friess
    Parts are Copyright (c) 2008 Michael Van Canneyt

    report definition to pascal source file.

    Licence LGPL with extension

    See the file COPYING.modified.LGPL for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpr2src_reader_report;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpreport, FPReportStreamer;

type


  { TReaderFPReportTitleBand }

  TReaderFPReportTitleBand = class(TFPReportTitleBand)
  private
    FPN: string;
    FRN: string;
  public
    procedure   ReadElement(AReader: TFPReportStreamer); override;
  published
    property ParentName: string read FPN write FPN;
    property ReaderName: string read FRN write FRN;
  end;


  { TReaderFPReportMargins }

  TReaderFPReportMargins = class(TFPReportMargins)
  private
    FPN: string;
    FRN: string;
  public
    procedure   ReadElement(AReader: TFPReportStreamer); override;
  published
    property ParentName: string read FPN write FPN;
    property ReaderName: string read FRN write FRN;
  end;


  { TReaderFPReportPage }

  TReaderFPReportPage = class(TFPReportPage)
  private
    FPN: string;
    FRN: string;
  public
    procedure   ReadElement(AReader: TFPReportStreamer); override;
  published
    property ParentName: string read FPN write FPN;
    property ReaderName: string read FRN write FRN;
  end;



  { TReaderFPReport }

  TReaderFPReport = class(TFPCustomReport)

  private
    FExtAutoSave: boolean;
    FExtFileName: string;
    FExtUnitName: string;
    FPN: string;
    FRN: string;
    function GetSourceCode: string;
  protected
    procedure SaveToFile;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure ReadElement(AReader: TFPReportStreamer); override;
  published
    property ParentName: string read FPN write FPN;
    property ReaderName: string read FRN write FRN;
    property  AutoSave: boolean read FExtAutoSave write FExtAutoSave default True;
    property  FileName: string read FExtFileName write FExtFileName;
    property  UnitName: string read FExtUnitName write FExtUnitName;
    property  SourceCode: string read GetSourceCode;
  end;




implementation

var
  FSrc, FVari : TStringList;

  procedure RegisterReaderReportClasses; forward;
  procedure UnRegisterStandardReportClasses; forward;
  procedure RegisterStandardReportClasses; forward;

{ TReaderFPReportTitleBand }

procedure TReaderFPReportTitleBand.ReadElement(AReader: TFPReportStreamer);
var
  s : string;
begin
  s := AReader.ReadString('ChildBand', '');
  FSrc.Append('  // Childband-->'+s);
  //if (s<>'') then
  //  Page.Report.AddReference(Self, 'ChildBand', s);
  //FVisibleOnPage := StringToVisibleOnPage(AReader.ReadString('VisibleOnPage', 'vpAll'));
  //FKeepTogetherWithChildren := AReader.ReadBoolean('KeepTogetherWithChildren', FKeepTogetherWithChildren);
  //FBandPosition := StringToBandPosition(AReader.ReadString('BandPosition', 'bpNormal'));
  //UseParentFont := AReader.ReadBoolean('UseParentFont', UseParentFont);
  //if not UseParentFont then
  //  begin
  //  Font.Name := AReader.ReadString('FontName', Font.Name);
  //  Font.Size := AReader.ReadInteger('FontSize', Font.Size);
  //  Font.Color := QWordToReportColor(AReader.ReadQWord('FontColor', Font.Color));
  //  end
  //else
  //  ReAssignParentFont;
  //S:=AReader.ReadString('Data','');
  //if (S<>'') then
  //  SetDataFromName(S);
  // This must come last: e.g. the UseParentFont assumes the font is properly set up
  inherited ReadElement(AReader);
end;

{ TReaderFPReportMargins }

procedure TReaderFPReportMargins.ReadElement(AReader: TFPReportStreamer);
begin
  FSrc.Add(' '+FRN+' := TFPReportMargins.Create('+FPN+');');
  FVari.Add(' '+FRN+' : TFPReportMargins;');
  //Top := AReader.ReadFloat('Top', Top);
  FSrc.Add(' '+FRN+'.Top := '+FloatToStr(AReader.ReadFloat('Top', Top))+ ';');
  //Left := AReader.ReadFloat('Left', Left);
  FSrc.Add(' '+FRN+'.Left := '+FloatToStr(AReader.ReadFloat('Left', Left))+ ';');
  //Bottom := AReader.ReadFloat('Bottom', Bottom);
  FSrc.Add(' '+FRN+'.Bottom := '+FloatToStr(AReader.ReadFloat('Bottom', Bottom))+ ';');
  //Right := AReader.ReadFloat('Right', Right);
  FSrc.Add(' '+FRN+'.Right := '+FloatToStr(AReader.ReadFloat('Right', Right))+ ';');
  inherited ReadElement(AReader);
end;

{ TReaderFPReportPage }

procedure TReaderFPReportPage.ReadElement(AReader: TFPReportStreamer);
var
  E: TObject;
  XMargins: TReaderFPReportMargins;
begin
  FSrc.Add(' '+FRN+' := TFPReportPage.Create('+FPN+');');
  FVari.Add(' '+FRN+' : TFPReportPage;');
  //
  //ColumnCount := AReader.ReadInteger('ColumnCount', 1);
  FSrc.Add(' '+FRN+'.ColumnCount := '+IntToStr(AReader.ReadInteger('ColumnCount', 1))+ ';');
  //ColumnGap := AReader.ReadFloat('ColumnGap', 0);
  FSrc.Add(' '+FRN+'.ColumnGap := '+FloatToStr(AReader.ReadFloat('ColumnGap', 0))+ ';');
  //ColumnLayout := StringToColumnLayout(AReader.ReadString('ColumnLayout', 'clVertical'));
  FSrc.Add(' '+FRN+'.ColumnLayout := '+AReader.ReadString('ColumnLayout', 'clVertical')+ ';');
  //Orientation := StringToPaperOrientation(AReader.ReadString('Orientation', 'poPortrait'));
  FSrc.Add(' '+FRN+'.Orientation := '+AReader.ReadString('Orientation', 'poPortrait')+ ';');
  //Pagesize.PaperName := AReader.ReadString('PageSize.PaperName', 'A4');
  FSrc.Add(' '+FRN+'.Pagesize.PaperName := '''+AReader.ReadString('PageSize.PaperName', 'A4')+ ''';');
  //Pagesize.Width := AReader.ReadFloat('PageSize.Width', 210);
  FSrc.Add(' '+FRN+'.Pagesize.Width := '+FloatToStr(AReader.ReadFloat('PageSize.Width', 210))+ ';');
  //Pagesize.Height := AReader.ReadFloat('PageSize.Height', 297);
  FSrc.Add(' '+FRN+'.Pagesize.Height := '+FloatToStr(AReader.ReadFloat('PageSize.Height', 297))+ ';');
  //Font.Name := AReader.ReadString('FontName', Font.Name);
  FSrc.Add(' '+FRN+'.Font.Name := '''+AReader.ReadString('FontName', Font.Name)+ ''';');
  //Font.Size := AReader.ReadInteger('FontSize', Font.Size);
  FSrc.Add(' '+FRN+'.Font.Size := '+IntToStr(AReader.ReadInteger('FontSize', Font.Size))+ ';');
  //Font.Color := QWordToReportColor(AReader.ReadQWord('FontColor', Font.Color));
  FSrc.Add(' '+FRN+'.Font.Color := TFPReportColor($'+IntToHex(AReader.ReadQWord('FontColor', Font.Color),8)+ ');');
  //FDataName:=AReader.ReadString('Data','');
  //if FDataName<>'' then
  //  RestoreDataFromNames;
  if SameStr(AReader.ReadString('Data',''),'') then
    FVari.Add(' //'+FRN+' has no Data')
  else
    FSrc.Add(' '+FRN+'.Data := '''+AReader.ReadString('Data', '')+ ''';');
  E := AReader.FindChild('Margins');
  if Assigned(E) then
  begin
    AReader.PushElement(E);
    XMargins:= TReaderFPReportMargins.Create(nil);
    try
      XMargins.ParentName:= FRN;
      XMargins.ReaderName:= 'Margin_'+RightStr('0000',4);
      XMargins.ReadElement(AReader);
    finally
      XMargins.Free;
      AReader.PopElement;
    end;
  end;
  inherited ReadElement(AReader);
end;

function TReaderFPReport.GetSourceCode: string;
var
  TmpSrc : TStringList;
  TmpStr: String;
begin
  Result := 'Error - Empty';
  TmpSrc:= TStringList.Create;
  try
    TmpSrc.Sorted:=false;
    TmpSrc.Append('unit '+ FExtUnitName);
    TmpSrc.Append('// Created with fp2srcexporter (C) Andreas Friess 2020');
    TmpSrc.Append('{$mode objfpc}{$H+}');
    TmpSrc.Append('');
    TmpSrc.Append('interface');
    TmpSrc.Append('uses');
    TmpSrc.Append('  Classes, SysUtils, fpReport;');
    TmpSrc.Append('');
    TmpSrc.Append('  procedure CreateReport(var FReport: TFPReport);');
    TmpSrc.Append('');
    TmpSrc.Append('implementation');
    TmpSrc.Append('');
    //FSrc.Append('uses');
    //FSrc.Append('  ');
    TmpSrc.Append('');
    TmpSrc.Append('procedure CreateReport(var FReport: TFPReport);');
    if FVari.Count > 0 then begin
     TmpSrc.Append('var');
     TmpSrc.Append('');
     TmpStr:= TmpSrc.DelimitedText + FVari.DelimitedText;
     TmpSrc.DelimitedText:= TmpStr;
    end;
    TmpSrc.Append('begin');
    TmpSrc.Append('');
    TmpStr:= TmpSrc.DelimitedText + FSrc.DelimitedText;
    TmpSrc.DelimitedText:= TmpStr;
    TmpSrc.Append('end');
    TmpSrc.Append('');
    TmpSrc.Append('end.');
    TmpSrc.Append('');
    Result := TmpSrc.DelimitedText
  finally
    TmpSrc.Free;
  end;
end;

constructor TReaderFPReport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExtUnitName:= ApplicationName + '_test';
  FExtFileName:= FExtUnitName+'.pas';
  FExtAutoSave:= true;
  UnRegisterStandardReportClasses;
  RegisterReaderReportClasses;
end;

destructor TReaderFPReport.Destroy;
begin
  inherited Destroy;
end;

procedure TReaderFPReport.ReadElement(AReader: TFPReportStreamer);
var
  E: TObject;
  i: integer;
  p: TReaderFPReportPage;
  v : TFPReportVariable;
  lImgItem: TFPReportImageItem;
begin
  E := AReader.FindChild('Report');
  if Assigned(E) then
  begin
    AReader.PushElement(E);
    FSrc.Add(' // Report found');
    try
      inherited ReadElement(AReader);
      FSrc.Add(' '+FRN+'.Title := '''+AReader.ReadString('Title',Title)+''';');
      FSrc.Add(' '+FRN+'.Author := '''+AReader.ReadString('Author',Author)+''';');
      FSrc.Add(' '+FRN+'.TwoPass := '+ BoolToStr(AReader.ReadBoolean('TwoPass',TwoPass),'True','False')+';');
      FSrc.Add(' '+FRN+'.DateCreated := StrToDateTime('''+DateTimeToStr(AReader.ReadDateTime('DateCreated',DateCreated))+''');');
      E := AReader.FindChild('Images');
      if Assigned(E) then
      begin
        AReader.PushElement(E);
        FSrc.Add(' // Images found');
        for i := 0 to AReader.ChildCount-1 do
        begin
          E := AReader.GetChild(i);
          AReader.PushElement(E); // child index is the identifier
          FSrc.Add(' // Image_'+IntToStr(i));
          try
            lImgItem := Images.AddImageItem;
            lImgItem.ReadElement(AReader);
          finally
            AReader.PopElement;
          end;
        end; { for i }
        AReader.PopElement;
      end;  { images }
      E := AReader.FindChild('Pages');
      if Assigned(E) then
      begin
        AReader.PushElement(E);
        FSrc.Add(' // Pages found');
        for i := 0 to AReader.ChildCount-1 do
        begin
          E := AReader.GetChild(i);
          FSrc.Add(' // Page_'+IntToStr(i));
          AReader.PushElement(E); // child index is the identifier
          try
            p := TReaderFPReportPage.Create(self);
            p.ParentName:= FRN;
            p.ReaderName:= 'Page_'+RightStr('0000'+IntToStr(i),4);
            p.ReadElement(AReader);
          finally
            AReader.PopElement;
          end;
        end;  { for i }
        AReader.PopElement;
      end; { pages }
      E := AReader.FindChild('Variables');
      if Assigned(E) then
      begin
        Variables.Clear;
        AReader.PushElement(E);
        FSrc.Add(' // Variables found');
        for i := 0 to AReader.ChildCount-1 do
        begin
          E := AReader.GetChild(i);
          AReader.PushElement(E); // child index is the identifier
          FSrc.Add(' // Variable_'+IntToStr(i));
          try
            v := Variables.Add as TFPReportVariable;
            v.ReadElement(AReader);
          finally
            AReader.PopElement;
          end;
        end;  { for I }
        AReader.PopElement;
      end; { Variables }
    finally
      AReader.PopElement;
    end;
    FSrc.Add('');
    FSrc.Add(' // Report end');
  end;
end;

procedure TReaderFPReport.SaveToFile;
begin
  if not SameStr(FExtFileName, '') then begin
    { TODO -oAndi : Is autodelete of sourcefile Ok ?! }
    if FileExists(FExtFileName) then
      DeleteFile(FExtFileName);
    FSrc.SaveToFile(FExtFileName);
  end;
end;

Procedure UnRegisterStandardReportClasses;

begin
  TFPReportTitleBand.UnRegisterElement;
  //TFPReportSummaryBand.UnRegisterElement;
  //TFPReportGroupHeaderBand.UnRegisterElement;
  //TFPReportGroupFooterBand.UnRegisterElement;
  //TFPReportDataBand.UnRegisterElement;
  //TFPReportChildBand.UnRegisterElement;
  //TFPReportPageHeaderBand.UnRegisterElement;
  //TFPReportPageFooterBand.UnRegisterElement;
  //TFPReportDataHeaderBand.UnRegisterElement;
  //TFPReportDataFooterBand.UnRegisterElement;
  //TFPReportColumnHeaderBand.UnRegisterElement;
  //TFPReportColumnFooterBand.UnRegisterElement;
  //TFPReportMemo.UnRegisterElement;
  //TFPReportImage.UnRegisterElement;
  //TFPReportCheckbox.UnRegisterElement;
  //TFPReportShape.UnRegisterElement;
  TFPReportPage.UnRegisterElement;
end;

Procedure RegisterReaderReportClasses;
begin
  TReaderFPReportTitleBand.RegisterElement;
  //TReaderFPReportSummaryBand.RegisterElement;
  //TReaderFPReportGroupHeaderBand.RegisterElement;
  //TReaderFPReportGroupFooterBand.RegisterElement;
  //TReaderFPReportDataBand.RegisterElement;
  //TReaderFPReportChildBand.RegisterElement;
  //TReaderFPReportPageHeaderBand.RegisterElement;
  //TReaderFPReportPageFooterBand.RegisterElement;
  //TReaderFPReportDataHeaderBand.RegisterElement;
  //TReaderFPReportDataFooterBand.RegisterElement;
  //TReaderFPReportColumnHeaderBand.RegisterElement;
  //TReaderFPReportColumnFooterBand.RegisterElement;
  //TReaderFPReportMemo.RegisterElement.FStandard:=True;
  //TReaderFPReportImage.RegisterElement.FStandard:=True;
  //TReaderFPReportCheckbox.RegisterElement.FStandard:=True;
  //TReaderFPReportShape.RegisterElement.FStandard:=True;
  TReaderFPReportPage.RegisterElement;
end;

Procedure RegisterStandardReportClasses;
begin
  TFPReportTitleBand.RegisterElement;
  TFPReportSummaryBand.RegisterElement;
  TFPReportGroupHeaderBand.RegisterElement;
  TFPReportGroupFooterBand.RegisterElement;
  TFPReportDataBand.RegisterElement;
  TFPReportChildBand.RegisterElement;
  TFPReportPageHeaderBand.RegisterElement;
  TFPReportPageFooterBand.RegisterElement;
  TFPReportDataHeaderBand.RegisterElement;
  TFPReportDataFooterBand.RegisterElement;
  TFPReportColumnHeaderBand.RegisterElement;
  TFPReportColumnFooterBand.RegisterElement;
  TFPReportMemo.RegisterElement;
  TFPReportImage.RegisterElement;
  TFPReportCheckbox.RegisterElement;
  TFPReportShape.RegisterElement;
  TFPReportPage.RegisterElement;
end;



initialization
  FSrc:= TStringList.create;
  FVari:= TStringList.create;

finalization
  if FVari <> nil then
    FVari.Free;
  if FSrc <> nil then
    FSrc.Free;

end.

