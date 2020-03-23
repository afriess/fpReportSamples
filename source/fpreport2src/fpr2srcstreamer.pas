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
    FJSON: TStackElement;
    FCurrentElement: TStackElement;
    FStack: TFPList;
    FIdent: integer;
    function GetSourceCode: string;
    procedure SetCurrentElement(AValue: TStackElement);
    procedure InitialiseCurrentElement;
    procedure SetJSON(AValue: TStackElement);
    function StackToName(StartName: string): string;
    function FindMappings(AElementName: string): TFPReportElement;
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
    property JSON: TStackElement read FJSON write SetJSON;
    property  AutoSave: boolean read FExtAutoSave write FExtAutoSave default True;
    property  FileName: string read FExtFileName write FExtFileName;
    property  UnitName: string read FExtUnitName write FExtUnitName;
    property  SourceCode: string read GetSourceCode;
  end;


implementation
uses
  LazLogger;

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
  FCurrentElement:= FJSON;
end;

procedure TFPReportSRCStreamer.SetJSON(AValue: TStackElement);
begin
  if Fjson = AValue then
    Exit;
  if Assigned(Fjson) then
    FreeAndNil(FJson);
  Fjson := AValue;
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
    if SameStr(act,StartName) then
      break
    else
      Result :=  act + '_' + Result;
  end;
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
begin
  {$ifdef DebugLvlCreate}DebugLn({$I %CURRENTROUTINE%}+ ' AName='+AName); {$endif}
  obj:= TStackElement.Create(nil);
  obj.CName:=AName;
  obj.add(AName,FCurrentElement);
  FCurrentElement:=obj;
  {$ifdef DebugLvlCreate}
  if FindMappings(FCurrentElement.CName) <> nil then DebugLn('    Mapping Found')
  else Debugln('    Mapping NOT found'); {$endif}
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
  FSrc:= nil;
  FExtUnitName:= ApplicationName + '_test';
  FExtFileName:= FExtUnitName+'.pas';
  FExtAutoSave:= true;
  //
  FCurrentElement:=nil;
  FJSON := TStackElement.Create(nil);
  InitialiseCurrentElement;
end;

destructor TFPReportSRCStreamer.Destroy;
var
  i: Integer;
begin
  if Assigned(FSrc) then
    FSrc.free;
  //if FStack <> nil then begin
  //  for i := FStack.Count-1 downto 0 do begin
  //    if FStack[i] <> nil then begin
  //      TObject(FStack[i]).Free;
  //      FStack[i]:= nil;
  //    end;
  //  end;
    FreeAndNil(FStack);
  //end;
  FreeAndNil(FJSON);
  inherited Destroy;
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

