unit fpr2srcstreamer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpReportStreamer;

type
{ TFPReportJSONStreamer }

  { TFPReportSRCStreamer }

  TFPReportSRCStreamer = class(TFPReportStreamer)
  private
    FCurrentElement: TComponent;
    FStack: TFPList;
    procedure SetCurrentElement(AValue: TComponent);
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
    property    CurrentElement: TComponent read FCurrentElement write SetCurrentElement;

  end;


implementation
uses
  LazLogger;

{ TFPReportSRCStreamer }

procedure TFPReportSRCStreamer.SetCurrentElement(AValue: TComponent);
begin
  if FCurrentElement=AValue then Exit;
  FCurrentElement:=AValue;
end;

function TFPReportSRCStreamer.ChildCount: integer;
begin
  Result := 0;
end;

function TFPReportSRCStreamer.CurrentElementName: string;
begin
  if Assigned(FCurrentElement) then
    Result := TComponent(FCurrentElement).Name
  else
    Result := '';
end;

function TFPReportSRCStreamer.FindChild(const AName: String): TObject;
begin
  Result := nil;
end;

function TFPReportSRCStreamer.GetChild(AIndex: Integer): TObject;
begin
  Result := nil;
end;

function TFPReportSRCStreamer.NewElement(const AName: String): TObject;
begin
  {$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}); {$endif}
  Result := nil;
end;

function TFPReportSRCStreamer.PopElement: TObject;
begin
  {$ifdef DebugStreamer}Debugln({$I %CURRENTROUTINE%}); {$endif}
  if (FStack = nil) or (FStack.Count = 0) then
    raise Exception.Create('Stack Empty');
  Result := FCurrentElement;
  FCurrentElement := TComponent(FStack[FStack.Count - 1]);
  FStack.Delete(FStack.Count - 1);
  if (FStack.Count = 0) then
    FreeAndNil(FStack);
  {$ifdef DebugStreamer}
  if FCurrentElement <> nil then Debugln(' Element='+FCurrentElement.ClassName)
  else Debugln(' Element=nil'); {$endif}
end;

function TFPReportSRCStreamer.PushCurrentElement: TObject;
begin
  {$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}); {$endif}
  if not Assigned(FStack) then
    FStack := TFPList.Create;
  FStack.Add(FCurrentElement);
  Result := FCurrentElement;
end;

function TFPReportSRCStreamer.PushElement(const AName: String): TObject;
begin
  {$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}+ ' AName='+AName); {$endif}
  PushCurrentElement;
  Result := NewElement(AName);
end;

function TFPReportSRCStreamer.PushElement(AElement: TObject): TObject;
begin
  {$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}+ ' Object='+AElement.ClassName); {$endif}
  PushCurrentElement;
  CurrentElement:=TComponent(AElement);
  Result := CurrentElement;
end;

procedure TFPReportSRCStreamer.WriteInteger(AName: String; AValue: Integer);
begin
{$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}+ 'AName='+AName+' AValue='+IntToStr(AValue)); {$endif}

end;

procedure TFPReportSRCStreamer.WriteInt64(AName: String; AValue: Int64);
begin
{$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}+ 'AName='+AName+' AValue='+IntToStr(AValue)); {$endif}

end;

procedure TFPReportSRCStreamer.WriteQWord(AName: String; AValue: QWord);
begin
{$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}+ 'AName='+AName+' AValue='+IntToStr(AValue)); {$endif}

end;

procedure TFPReportSRCStreamer.WriteFloat(AName: String; AValue: Extended);
begin
{$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}+ 'AName='+AName+' AValue='+FloatToStr(AValue)); {$endif}

end;

procedure TFPReportSRCStreamer.WriteString(AName: String; AValue: String);
begin
{$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}+ 'AName='+AName+' AValue='+AValue); {$endif}

end;

procedure TFPReportSRCStreamer.WriteBoolean(AName: String; AValue: Boolean);
begin
{$ifdef DebugStreamer}DebugLn({$I %CURRENTROUTINE%}+ 'AName='+AName+' AValue='+BoolToStr(AValue)); {$endif}

end;

procedure TFPReportSRCStreamer.WriteDateTime(AName: String; AValue: TDateTime);
begin

end;

procedure TFPReportSRCStreamer.WriteStream(AName: String; AValue: TStream);
begin

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
  FCurrentElement:=nil;
end;

destructor TFPReportSRCStreamer.Destroy;
begin
  FreeAndNil(FStack);
  inherited Destroy;
end;

end.

