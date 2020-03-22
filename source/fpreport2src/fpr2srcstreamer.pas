unit fpr2srcstreamer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpReportStreamer, fpreport;

type

  { TStackElement }

  TStackElement = class(TComponent)
  private
    FCName: string;
    FChildList: TStringList;
  public
    Class Function ElementType : String; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(AName:string; AElement: TStackElement);
  published
    property CName : string read FCName write FCName;
  end;

  { TFPReportSRCStreamer }

  TFPReportSRCStreamer = class(TFPReportStreamer)
  private
    FExtAutoSave: boolean;
    FExtFileName: string;
    FExtUnitName: string;
    FSrc: TStringList;
    FCurrentElement: TStackElement;
    FStack: TFPList;
    FIdent: integer;
    function GetSourceCode: string;
    procedure SetCurrentElement(AValue: TStackElement);
    procedure InitialiseCurrentElement;
    function StackToName(StartName: string): string;
    function FindMappings(AElementName: string): boolean;
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
    property  AutoSave: boolean read FExtAutoSave write FExtAutoSave default True;
    property  FileName: string read FExtFileName write FExtFileName;
    property  UnitName: string read FExtUnitName write FExtUnitName;
    property  SourceCode: string read GetSourceCode;
  end;


implementation
uses
  LazLogger;

{ TStackElement }

class function TStackElement.ElementType: String;
begin
  Result:= 'Stack';
end;

constructor TStackElement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChildList:= nil;
end;

destructor TStackElement.Destroy;
begin
  if Assigned(FChildList) then
    FChildList.Free;
  inherited Destroy;
end;

procedure TStackElement.Add(AName: string; AElement: TStackElement);
begin
  {$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}+ ' AName='+AName+' AElementType='+AElement.ElementType); {$endif}
  //if FChildList = nil then
  //  FChildList:= TStringList.Create;
  //FChildList.AddObject(AName,AElement);
  {$ifdef DebugStreamer}DebugLn('  AName='+AName+' Current='+AElement.CName);{$endif}
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
  {$ifdef DebugStreamer}Debugln({$I %CURRENTROUTINE%}); {$endif}
  if FCurrentElement = nil then begin
    FCurrentElement:= TStackElement.Create(nil);
  end;
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

function TFPReportSRCStreamer.FindMappings(AElementName: string): boolean;
var
  i: integer;
  map : TFPReportClassMapping;
begin
  Result := false;
  for i := 0 to gElementFactory.MappingCount - 1 do
  begin
    if SameText(gElementFactory.Mappings[I].MappingName, AElementName) then
    begin
      Result := true;
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
  {$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}+ ' AName='+AName); {$endif}
  Result := nil;
end;

function TFPReportSRCStreamer.GetChild(AIndex: Integer): TObject;
begin
  {$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}+ ' AIndex='+IntToStr(AIndex)); {$endif}
  Result := nil;
end;

function TFPReportSRCStreamer.NewElement(const AName: String): TObject;
var
  obj : TStackElement;
begin
  {$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}+ ' AName='+AName); {$endif}
  obj:= TStackElement.Create(nil);
  obj.CName:=AName;
  FCurrentElement.add(AName,FCurrentElement);
  FCurrentElement:=obj;
  Result := nil;
end;

function TFPReportSRCStreamer.PopElement: TObject;
begin
  {$ifdef DebugStreamer}Debugln({$I %CURRENTROUTINE%}); {$endif}
  if (FStack = nil) or (FStack.Count = 0) then
    raise Exception.Create('Stack Empty');
  Result := FCurrentElement;
  FCurrentElement := TStackElement(FStack[FStack.Count - 1]);
  FStack.Delete(FStack.Count - 1);
  if (FStack.Count = 0) then
    FreeAndNil(FStack);
  {$ifdef DebugStreamer}
  if FCurrentElement <> nil then DebugLnExit('    CName='+FCurrentElement.CName+' AElementType='+FCurrentElement.ElementType )
  else DebuglnExit('    Element=nil'); {$endif}
end;

function TFPReportSRCStreamer.PushCurrentElement: TObject;
begin
  {$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}+ ' Cname='+FCurrentElement.CName + ' Type='+FCurrentElement.ElementType); {$endif}
  if not Assigned(FStack) then begin
    FStack := TFPList.Create;
  end;
  FStack.Add(FCurrentElement);
  Result := FCurrentElement;
end;

function TFPReportSRCStreamer.PushElement(const AName: String): TObject;
begin
  {$ifdef DebugStreamer}DebugLnEnter({$I %CURRENTROUTINE%}+ ' AName='+AName); {$endif}
  PushCurrentElement;
  Result := NewElement(AName);
end;

function TFPReportSRCStreamer.PushElement(AElement: TObject): TObject;
begin
  {$ifdef DebugStreamer}DebugLnEnter({$I %CURRENTROUTINE%}+ ' Object='+AElement.ClassName); {$endif}
  PushCurrentElement;
  CurrentElement:=TStackElement(AElement);
  Result := CurrentElement;
end;

procedure TFPReportSRCStreamer.WriteInteger(AName: String; AValue: Integer);
begin
{$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}+ 'AName='+AName+' AValue='+IntToStr(AValue)); {$endif}
{$ifdef DebugStreamer}DebugLn('  ' +StackToName('Report')+' Current='+FCurrentElement.CName);{$endif}
end;

procedure TFPReportSRCStreamer.WriteInt64(AName: String; AValue: Int64);
begin
{$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}+ 'AName='+AName+' AValue='+IntToStr(AValue)); {$endif}
{$ifdef DebugStreamer}DebugLn('  ' +StackToName('Report')+' Current='+FCurrentElement.CName);{$endif}
end;

procedure TFPReportSRCStreamer.WriteQWord(AName: String; AValue: QWord);
begin
{$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}+ 'AName='+AName+' AValue='+IntToStr(AValue)); {$endif}
{$ifdef DebugStreamer}DebugLn('  ' +StackToName('Report')+' Current='+FCurrentElement.CName);{$endif}
end;

procedure TFPReportSRCStreamer.WriteFloat(AName: String; AValue: Extended);
begin
{$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}+ 'AName='+AName+' AValue='+FloatToStr(AValue)); {$endif}
{$ifdef DebugStreamer}DebugLn('  ' +StackToName('Report')+' Current='+FCurrentElement.CName);{$endif}
end;

procedure TFPReportSRCStreamer.WriteString(AName: String; AValue: String);
begin
{$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}+ 'AName='+AName+' AValue='+AValue); {$endif}
{$ifdef DebugStreamer}DebugLn('  ' +StackToName('Report')+' Current='+FCurrentElement.CName);{$endif}
end;

procedure TFPReportSRCStreamer.WriteBoolean(AName: String; AValue: Boolean);
begin
{$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}+ 'AName='+AName+' AValue='+BoolToStr(AValue)); {$endif}
{$ifdef DebugStreamer}DebugLn('  ' +StackToName('Report')+' Current='+FCurrentElement.CName);{$endif}
end;

procedure TFPReportSRCStreamer.WriteDateTime(AName: String; AValue: TDateTime);
begin
  {$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}+ 'AName='+AName); {$endif}
  {$ifdef DebugStreamer}DebugLn('  ' +StackToName('Report')+' Current='+FCurrentElement.CName);{$endif}
end;

procedure TFPReportSRCStreamer.WriteStream(AName: String; AValue: TStream);
begin
  {$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}+ 'AName='+AName); {$endif}
  {$ifdef DebugStreamer}DebugLn('  ' +StackToName('Report')+' Current='+FCurrentElement.CName);{$endif}
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
  InitialiseCurrentElement;
end;

destructor TFPReportSRCStreamer.Destroy;
var
  i: Integer;
begin
  if Assigned(FSrc) then
    FSrc.free;
  if FStack <> nil then begin
    for i := FStack.Count-1 downto 0 do begin
      if FStack[i] <> nil then begin
        TObject(FStack[i]).Free;
        FStack[i]:= nil;
      end;
    end;
    FreeAndNil(FStack);
  end;
  inherited Destroy;
end;

end.

