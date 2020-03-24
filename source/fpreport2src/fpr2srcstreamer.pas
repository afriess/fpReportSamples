unit fpr2srcstreamer;

{$mode objfpc}{$H+}

{$ifdef DebugLvlAll}
  {$Define DebugLvlDbg}
  {$Define DebugLvlCreate}
{$endif}
{$ifdef DebugLvlCreate}
  {$Define DebugLvlInfo}
{$endif}
{$ifdef DebugLvlInfo}
  {$Define DebugLvlWarning}
{$endif}



interface

uses
  Classes, SysUtils, fpReportStreamer, fpreport;

type

  TFPReportReport = class(TFPReportElement)
  end;

  TFPReportChapter = class(TFPReportElementWithChildren)
  end;


  { TReportReport }

  TReportReport = class(TFPReportReport)
  public
    Class Function ElementType: String; override;
  end;

  { TReportChapterImages }

  TReportChapterImages = class(TFPReportChapter)
  public
    Class Function ElementType: String; override;
  end;


  { TReportChapterPages }

  TReportChapterPages = class(TFPReportChapter)
  public
    Class Function ElementType: String; override;
  end;


  { TReportChapterVariables }

  TReportChapterVariables = class(TFPReportChapter)
  public
    Class Function ElementType: String; override;
  end;


  { TStackElement }

  TStackElement = class(TComponent)
  private
    FCName: string;
    FParent: TComponent;
    FParentName: string;
    FRptElement: TFPReportElement;
    procedure SetRptElement(AValue: TFPReportElement);
  public
    Class Function ElementType : String; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(AParentName:string; AParent: TStackElement);
  published
    property CName : string read FCName write FCName;
    property Parent: TComponent read FParent;
    property ParentName: string read FParentName;
    property RptElement : TFPReportElement read FRptElement write SetRptElement;
  end;

  { TFPReportSRCStreamer }

  TFPReportSRCStreamer = class(TFPReportStreamer)
  private
    FExtAutoSave: boolean;
    FExtFileName: string;
    FExtUnitName: string;
    FSrc: TStringList;
    FVar: TStringList;
    FStE: TStackElement;
    FCurrentElement: TStackElement;
    FStack: TFPList;
    FIdent: integer;
    FChapterName, FChapterNr: string;
    FNextIsChapterNr: boolean;
    FElementName, FElementNr: string;
    FNextIsElementNr: boolean;
    function GetSourceCode: string;
    procedure SetCurrentElement(AValue: TStackElement);
    procedure InitialiseCurrentElement;
    procedure SetFStE(AValue: TStackElement);
    function StackToName(StartName: string): string;
    function FindChapter(AElementName: string): TFPReportElement;
    function FindMappings(AElementName: string): TFPReportElement;
    //
    procedure WriteNewReport;
    procedure WriteNewChapter;
    procedure WriteNewElement;
    procedure WriteNewValue;
    //
    procedure WriteHeader;
    procedure WriteVar;
  public
    function ChildCount: integer; override;
    function CurrentElementName: string; override;
    function FindChild(const AName: String): TObject; override;
    function GetChild(AIndex: Integer): TObject; override;
    function NewElement(const AName: String): TObject; override;
    function PopElement: TObject; override;
    function PushCurrentElement: TObject; override;
    function PushElement(const AName: String): TObject; override;
    function PushElement(AElement: TObject): TObject; override;
    // FPReportStreamer interface
    procedure   WriteInteger(AName: String; AValue: Integer); override;
    procedure   WriteInt64(AName: String; AValue: Int64); override;
    procedure   WriteQWord(AName: String; AValue: QWord); override;
    procedure   WriteFloat(AName: String; AValue: Extended); override;
    procedure   WriteString(AName: String; AValue: String); override;
    procedure   WriteBoolean(AName: String; AValue: Boolean); override;
    procedure   WriteDateTime(AName: String; AValue: TDateTime); override;
    procedure   WriteStream(AName: String; AValue: TStream); override;
    // must we support ?!
    procedure   WriteIntegerDiff(AName: String; AValue, AOriginal: Integer); override;
    procedure   WriteInt64Diff(AName: String; AValue, AOriginal: Int64); override;
    procedure   WriteQWordDiff(AName: String; AValue, AOriginal: QWord); override;
    procedure   WriteFloatDiff(AName: String; AValue, AOriginal: Extended); override;
    procedure   WriteStringDiff(AName: String; AValue, AOriginal: String); override;
    procedure   WriteBooleanDiff(AName: String; AValue, AOriginal: Boolean); override;
    procedure   WriteDateTimeDiff(AName: String; AValue, AOriginal: TDateTime); override;
    procedure   WriteStreamDiff(AName: String; AValue, AOriginal: TStream); override;
    // not supported
    function    ReadInteger(AName: String; ADefault: Integer): Integer; override;
    function    ReadInt64(AName: String; ADefault: Int64): Int64; override;
    function    ReadQWord(AName: String; ADefault: QWord): QWord; override;
    function    ReadFloat(AName: String; ADefault: Extended): Extended; override;
    function    ReadString(AName: String; ADefault: String): String; override;
    function    ReadDateTime(AName: String; ADefault: TDateTime): TDateTime; override;
    function    ReadBoolean(AName: String; ADefault: Boolean): Boolean; override;
    function    ReadStream(AName: String; AValue: TStream) : Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    CurrentElement: TStackElement read FCurrentElement write SetCurrentElement;

  published
    property STE: TStackElement read FStE write SetFStE;
    property  AutoSave: boolean read FExtAutoSave write FExtAutoSave default True;
    property  FileName: string read FExtFileName write FExtFileName;
    property  UnitName: string read FExtUnitName write FExtUnitName;
    property  SourceCode: string read GetSourceCode;
  end;


implementation
uses
  LazLogger;

{ TReportReport }

class function TReportReport.ElementType: String;
begin
  Result:='Report';
end;

{ TReportChapterVariables }

class function TReportChapterVariables.ElementType: String;
begin
  Result:= 'Variables';
end;

{ TReportChapterImages }

class function TReportChapterImages.ElementType: String;
begin
  Result:='Images';
end;

{ TReportChapterPages }

class function TReportChapterPages.ElementType: String;
begin
  Result:='Pages';
end;

{ TStackElement }

procedure TStackElement.SetRptElement(AValue: TFPReportElement);
begin
  if FRptElement=AValue then Exit;
  // Clear old Value
  if FRptElement <> nil then
    RptElement.Free;
  FRptElement:=AValue;
end;

class function TStackElement.ElementType: String;
begin
  Result:= 'Stack';
end;

constructor TStackElement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TStackElement.Destroy;
begin
  if FRptElement <> nil then
    RptElement.Free;
  inherited Destroy;
end;

procedure TStackElement.Add(AParentName: string; AParent: TStackElement);
begin
  {$ifdef DebugLvlDbg}DebugLn({$I %CURRENTROUTINE%}+ ' AParentName='+AParentName); {$endif}
  FParent:= AParent;
  FParentName:= AParentName;
end;

{ TFPReportSRCStreamer }

procedure TFPReportSRCStreamer.SetCurrentElement(AValue: TStackElement);
begin
  if FCurrentElement=AValue then Exit;
  FCurrentElement:=AValue;
end;

function TFPReportSRCStreamer.GetSourceCode: string;
begin
  if FSrc <> nil then
    Result := FSrc.DelimitedText
  else
    Result := '';
end;

procedure TFPReportSRCStreamer.InitialiseCurrentElement;
begin
  {$ifdef DebugLvlDbg}Debugln({$I %CURRENTROUTINE%}); {$endif}
  FCurrentElement:= FStE;
end;

procedure TFPReportSRCStreamer.SetFStE(AValue: TStackElement);
begin
  if FStE = AValue then
    Exit;
  if Assigned(FStE) then
    FreeAndNil(FStE);
  FStE := AValue;
  InitialiseCurrentElement;
end;

function TFPReportSRCStreamer.StackToName(StartName: string): string;
var
  i : integer;
  act: String;
begin
  Result := FCurrentElement.CName;
  for i := FStack.count-1 downTo 0 do begin
    act := TStackElement(FStack[i]).CName;
    if SameStr(act,StartName) then begin
      Result:= act;
      break;
    end
    else
      Result :=  act + '_' + Result;
  end;
end;

function TFPReportSRCStreamer.FindChapter(AElementName: string
  ): TFPReportElement;
begin
  Result := nil;

end;

function TFPReportSRCStreamer.FindMappings(AElementName: string): TFPReportElement;
var
  i: integer;
  map : TFPReportClassMapping;
  MapName: string;
begin
  Result := nil;
  for i := 0 to gElementFactory.MappingCount - 1 do
  begin
    MapName:= gElementFactory.Mappings[I].MappingName;
    if SameText(MapName, AElementName) then
    begin
      Result:= gElementFactory.CreateInstance(AElementName,nil);
      Break; //==>
    end;
  end;
end;

procedure TFPReportSRCStreamer.WriteNewReport;
begin
  FChapterName:= '';
  FElementName:= '';
  FSrc.Add('procedure CreateReport(var FReport: TFPReport);');
  //FSrc.Add('var');
  //FSrc.Add('  p: TFPReportPage;');
  //FSrc.Add('  TitleBand: TFPReportTitleBand;');
  //FSrc.Add('  DataBand: TFPReportDataBand;');
  //FSrc.Add('  Memo: TFPReportMemo;');
  //FSrc.Add('  PageFooter: TFPReportPageFooterBand;');
  //FSrc.Add('  ColumnBand: TFPReportColumnHeaderBand;');
  //FSrc.Add('  DataFooterBand: TFPReportDataFooterBand;');
  FSrc.Add('begin');
  FSrc.Add('  //');
end;

procedure TFPReportSRCStreamer.WriteNewChapter;
begin
  FChapterName:= FCurrentElement.CName;
  FNextIsChapterNr:= true;
  FElementName:= '';
  FSrc.Append('');
  FSrc.Append('  // New Chapter ' + FChapterName);
end;

procedure TFPReportSRCStreamer.WriteNewElement;
begin
  FElementName:= FCurrentElement.CName;
  FNextIsElementNr:= (FCurrentElement.FRptElement is TFPReportElementWithChildren);
  FSrc.Append('');
  FSrc.Append('  // New Element ' + FElementName);

end;

procedure TFPReportSRCStreamer.WriteNewValue;
var
  ele: TStackElement;
begin
  if FNextIsChapterNr then begin
    FChapterNr:= FCurrentElement.CName;
    FNextIsChapterNr:=false;
    FSrc.Append(' // '+ StackToName(FChapterName)+FChapterNr);
    ele := TStackElement(FCurrentElement.Parent);
    if (ele.FRptElement is TReportChapterPages) then begin
      FVar.Append('  '+StackToName(FChapterName)+FChapterNr+' : TFPReportPage;');
      FSrc.Append('  '+StackToName(FChapterName)+FChapterNr+' := TFPReportPage.Create(FPReport);');
    end;
  end
  else begin
    if FNextIsElementNr then begin
      FElementNr:= FCurrentElement.CName;
      FNextIsElementNr:= false;
      FSrc.Append(' // '+ StackToName(FElementName)+FElementNr);
    end
    else begin
      // normal value
      FSrc.Append('  // New Value ' + FCurrentElement.CName);
    end;
  end;
end;

procedure TFPReportSRCStreamer.WriteHeader;
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
end;

procedure TFPReportSRCStreamer.WriteVar;
begin

end;

function TFPReportSRCStreamer.ChildCount: integer;
begin
  Result := 0;
end;

function TFPReportSRCStreamer.CurrentElementName: string;
begin
  if Assigned(FCurrentElement) then
    Result := TStackElement(FCurrentElement).Name
  else
    Result := '';
end;

function TFPReportSRCStreamer.FindChild(const AName: String): TObject;
begin
  {$ifdef DebugLvlDbg}DebugLn({$I %CURRENTROUTINE%}+ ' AName='+AName); {$endif}
  Result := nil;
end;

function TFPReportSRCStreamer.GetChild(AIndex: Integer): TObject;
begin
  {$ifdef DebugLvlDbg}DebugLn({$I %CURRENTROUTINE%}+ ' AIndex='+IntToStr(AIndex)); {$endif}
  Result := nil;
end;

function TFPReportSRCStreamer.NewElement(const AName: String): TObject;
var
  obj : TStackElement;
  MapObj: TFPReportElement;
begin
  {$ifdef DebugLvlCreate}DebugLn({$I %CURRENTROUTINE%}+ ' AName='+AName); {$endif}
  obj:= TStackElement.Create(nil);
  obj.CName:=AName;
  obj.add(AName,FCurrentElement);
  FCurrentElement:=obj;
  //
  MapObj:= FindMappings(FCurrentElement.CName);
  FCurrentElement.FRptElement:= MapObj;
  {$ifdef DebugLvlCreate}
  if MapObj <> nil then
    DebugLn('    Mapping Found')
  else
    Debugln('    Mapping NOT found');
  {$endif}
  // Is a new Report ?!
  if (MapObj is TFPReportReport) then begin
    // Start a new Report
    FSrc.Clear;
    WriteNewReport;
  end
  else begin
    // Is a new Chapter ?!
    if MapObj is TFPReportChapter then begin
      // Start a new chapter
      WriteNewChapter;
    end
    else begin
      // Is a new Report Element ?!
      if MapObj is TFPReportElement then begin
        // Start a new Element
        WriteNewElement;
      end
      else begin
        // Insert only a new assigment
        WriteNewValue;
      end;
    end;
  end;
  Result := FCurrentElement;
end;

function TFPReportSRCStreamer.PopElement: TObject;
begin
  {$ifdef DebugLvlDbg}Debugln({$I %CURRENTROUTINE%}); {$endif}
  if (FStack = nil) or (FStack.Count = 0) then
    raise Exception.Create('Stack Empty');
  if FCurrentElement <> nil then
    FCurrentElement.Free;
  FCurrentElement := TStackElement(FStack[FStack.Count - 1]);
  FStack.Delete(FStack.Count - 1);
  if (FStack.Count = 0) then
    FreeAndNil(FStack);
  {$ifdef DebugLvlDbg}
  if FCurrentElement <> nil then DebugLnExit('    CName='+FCurrentElement.CName)
  else DebuglnExit('    Element=nil'); {$endif}
  Result := FCurrentElement;
end;

function TFPReportSRCStreamer.PushCurrentElement: TObject;
begin
  {$ifdef DebugLvlDbg}DebugLn({$I %CURRENTROUTINE%}+ ' Cname='+FCurrentElement.CName + ' Type='+FCurrentElement.ElementType); {$endif}
  if not Assigned(FStack) then begin
    FStack := TFPList.Create;
  end;
  FStack.Add(FCurrentElement);
  Result := FCurrentElement;
end;

function TFPReportSRCStreamer.PushElement(const AName: String): TObject;
begin
  {$ifdef DebugLvlDbg}DebugLnEnter({$I %CURRENTROUTINE%}+ ' AName='+AName); {$endif}
  PushCurrentElement;
  Result := NewElement(AName);
end;

function TFPReportSRCStreamer.PushElement(AElement: TObject): TObject;
begin
  {$ifdef DebugLvlDbg}DebugLnEnter({$I %CURRENTROUTINE%}+ ' Object='+AElement.ClassName); {$endif}
  PushCurrentElement;
  CurrentElement:=TStackElement(AElement);
  Result := CurrentElement;
end;

procedure TFPReportSRCStreamer.WriteInteger(AName: String; AValue: Integer);
begin
{$ifdef DebugLvlDbg}DebugLn({$I %CURRENTROUTINE%}+ 'AName='+AName+' AValue='+IntToStr(AValue)); {$endif}
{$ifdef DebugLvlDbg}DebugLn('  ' +StackToName('Report')+' Current='+FCurrentElement.CName);{$endif}
end;

procedure TFPReportSRCStreamer.WriteInt64(AName: String; AValue: Int64);
begin
{$ifdef DebugLvlDbg}DebugLn({$I %CURRENTROUTINE%}+ 'AName='+AName+' AValue='+IntToStr(AValue)); {$endif}
{$ifdef DebugLvlDbg}DebugLn('  ' +StackToName('Report')+' Current='+FCurrentElement.CName);{$endif}
end;

procedure TFPReportSRCStreamer.WriteQWord(AName: String; AValue: QWord);
begin
{$ifdef DebugLvlDbg}DebugLn({$I %CURRENTROUTINE%}+ 'AName='+AName+' AValue='+IntToStr(AValue)); {$endif}
{$ifdef DebugLvlDbg}DebugLn('  ' +StackToName('Report')+' Current='+FCurrentElement.CName);{$endif}
end;

procedure TFPReportSRCStreamer.WriteFloat(AName: String; AValue: Extended);
begin
{$ifdef DebugLvlDbg}DebugLn({$I %CURRENTROUTINE%}+ 'AName='+AName+' AValue='+FloatToStr(AValue)); {$endif}
{$ifdef DebugLvlDbg}DebugLn('  ' +StackToName('Report')+' Current='+FCurrentElement.CName);{$endif}
end;

procedure TFPReportSRCStreamer.WriteString(AName: String; AValue: String);
begin
{$ifdef DebugLvlDbg}DebugLn({$I %CURRENTROUTINE%}+ 'AName='+AName+' AValue='+AValue); {$endif}
{$ifdef DebugLvlDbg}DebugLn('  ' +StackToName('Report')+' Current='+FCurrentElement.CName);{$endif}
end;

procedure TFPReportSRCStreamer.WriteBoolean(AName: String; AValue: Boolean);
begin
{$ifdef DebugLvlDbg}DebugLn({$I %CURRENTROUTINE%}+ 'AName='+AName+' AValue='+BoolToStr(AValue)); {$endif}
{$ifdef DebugLvlDbg}DebugLn('  ' +StackToName('Report')+' Current='+FCurrentElement.CName);{$endif}
end;

procedure TFPReportSRCStreamer.WriteDateTime(AName: String; AValue: TDateTime);
begin
  {$ifdef DebugLvlDbg}DebugLn({$I %CURRENTROUTINE%}+ 'AName='+AName); {$endif}
  {$ifdef DebugLvlDbg}DebugLn('  ' +StackToName('Report')+' Current='+FCurrentElement.CName);{$endif}
end;

procedure TFPReportSRCStreamer.WriteStream(AName: String; AValue: TStream);
begin
  {$ifdef DebugLvlDbg}DebugLn({$I %CURRENTROUTINE%}+ 'AName='+AName); {$endif}
  {$ifdef DebugLvlDbg}DebugLn('  ' +StackToName('Report')+' Current='+FCurrentElement.CName);{$endif}
end;

procedure TFPReportSRCStreamer.WriteIntegerDiff(AName: String; AValue,
  AOriginal: Integer);
begin

end;

procedure TFPReportSRCStreamer.WriteInt64Diff(AName: String; AValue,
  AOriginal: Int64);
begin

end;

procedure TFPReportSRCStreamer.WriteQWordDiff(AName: String; AValue,
  AOriginal: QWord);
begin

end;

procedure TFPReportSRCStreamer.WriteFloatDiff(AName: String; AValue,
  AOriginal: Extended);
begin

end;

procedure TFPReportSRCStreamer.WriteStringDiff(AName: String; AValue,
  AOriginal: String);
begin

end;

procedure TFPReportSRCStreamer.WriteBooleanDiff(AName: String; AValue,
  AOriginal: Boolean);
begin

end;

procedure TFPReportSRCStreamer.WriteDateTimeDiff(AName: String; AValue,
  AOriginal: TDateTime);
begin

end;

procedure TFPReportSRCStreamer.WriteStreamDiff(AName: String; AValue,
  AOriginal: TStream);
begin

end;

function TFPReportSRCStreamer.ReadInteger(AName: String; ADefault: Integer
  ): Integer;
begin
  Result:= 0;
end;

function TFPReportSRCStreamer.ReadInt64(AName: String; ADefault: Int64): Int64;
begin
  Result:= 0;
end;

function TFPReportSRCStreamer.ReadQWord(AName: String; ADefault: QWord): QWord;
begin
  Result:= 0;
end;

function TFPReportSRCStreamer.ReadFloat(AName: String; ADefault: Extended
  ): Extended;
begin
  Result:= 0.0;
end;

function TFPReportSRCStreamer.ReadString(AName: String; ADefault: String
  ): String;
begin
  Result:= '';
end;

function TFPReportSRCStreamer.ReadDateTime(AName: String; ADefault: TDateTime
  ): TDateTime;
begin
  Result:= 0.0;
end;

function TFPReportSRCStreamer.ReadBoolean(AName: String; ADefault: Boolean
  ): Boolean;
begin
  Result:= false;
end;

function TFPReportSRCStreamer.ReadStream(AName: String; AValue: TStream
  ): Boolean;
begin
  Result:= false;
end;

constructor TFPReportSRCStreamer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //
  FIdent:=0;
  FSrc:= TStringList.Create;
  FVar:= TStringList.Create;
  FExtUnitName:= ApplicationName + '_test';
  FExtFileName:= FExtUnitName+'.pas';
  FExtAutoSave:= true;
  //
  FCurrentElement:=nil;
  FStE := TStackElement.Create(nil);
  InitialiseCurrentElement;
end;

destructor TFPReportSRCStreamer.Destroy;
var
  i: Integer;
begin
  FVar.free;
  FSrc.free;
  FStack.Free;
  FStE.Free;
  inherited Destroy;
end;

Procedure RegisterStandardReportClasses;

begin
  TReportReport.RegisterElement;
  TReportChapterImages.RegisterElement;
  TReportChapterPages.RegisterElement;
  TReportChapterVariables.RegisterElement;
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
  //TFPReportMemo.RegisterElement.FStandard:=True;
  //TFPReportImage.RegisterElement.FStandard:=True;
  //TFPReportCheckbox.RegisterElement.FStandard:=True;
  //TFPReportShape.RegisterElement.FStandard:=True;
  //TFPReportPage.RegisterElement.FStandard:=True;
  TFPReportMemo.RegisterElement;
  TFPReportImage.RegisterElement;
  TFPReportCheckbox.RegisterElement;
  TFPReportShape.RegisterElement;
  TFPReportPage.RegisterElement;
end;

initialization
  RegisterStandardReportClasses;

end.

