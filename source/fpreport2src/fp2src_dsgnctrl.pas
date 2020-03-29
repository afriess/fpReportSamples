unit fp2src_dsgnctrl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, controls, fpreport, graphics, lmessages, fpreportlclexport, lcltype, menus,
  fpreportdesignobjectlist, types;


type
  TSrcReportObject = Class(TReportObject)
  private

  public
    FPN, FRN: string;
  published

  end;



  { TSrcReportObjectList }

  TSrcReportObjectList = class(TReportObjectList)
  private
    FPage: TFPReportCustomPage;
    function ClassNameToName(CN: string): string;
    procedure WriteSrcPageFooterBand(AElement: TFPReportElement);
    procedure WriteSrcPageHeaderBand(AElement: TFPReportElement);
    procedure WriteSrcSummaryBand(AElement: TFPReportElement);
  protected
    procedure WriteSrcPage(AElement: TFPReportElement);
    procedure WriteSrcTitleBand(AElement: TFPReportElement);
    procedure WriteSrcMemo(AElement: TFPReportElement);
    procedure WriteSrcShape(AElement: TFPReportElement);
    procedure WriteSrcCheckBox(AElement: TFPReportElement);
    procedure WriteSrcUnkown(AElement: TFPReportElement);
  public
    procedure WriteSrc(AElement: TReportObject);
    Procedure LoadFromPage(APage : TFPReportCustomPage);reintroduce;
    function AddElement(AElement: TFPReportElement): TReportObject;reintroduce;
    function AddBand(ABand: TFPReportCustomBand): TReportObject;reintroduce;
  end;

  { TSrcFPReportDesignerControl }

  TSrcFPReportDesignerControl = class(TCustomControl)
  private
    FPage: TFPReportCustomPage;
    FObjects: TSrcReportObjectList;
    procedure SetPage(AValue: TFPReportCustomPage);

  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    Property Page : TFPReportCustomPage Read FPage Write SetPage;
  end;

function GetSourceCode(AUnitName:string = 'rptsrc'): string;

implementation
uses
  TypInfo;

var
  FSrc, FVari : TStringList;

function GetSourceCode(AUnitName:string = 'rptsrc'): string;
var
  TmpSrc : TStringList;
  TmpStr: String;
begin
  Result := 'Error - Empty';
  TmpSrc:= TStringList.Create;
  try
    TmpSrc.Sorted:=false;
    TmpSrc.Append('unit '+ AUnitName);
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


function GetSetName(const aSet:PTypeInfo; Value: Integer):string;
var
  vData1 : PTypeData;
  vData2 : PTypeData;
  vCntr  : Integer;
  v: Integer;
begin
  Result := '';
  if aSet^.Kind = tkSet then begin
    vData1 := GetTypeData(aSet);
    vData2 := GetTypeData(vData1^.CompType);
    for vCntr := vData2^.MinValue to vData2^.MaxValue do
      if (Value shr vCntr) and 1 <> 0 then
        Result := Result+ GetEnumName(vData1^.CompType,vCntr)+',';
    if Result <> '' then Delete(Result, Length(Result), 1);
  end;
end;

function GetSetValue(const aSet:PTypeInfo; Name: String): Integer;
var
  vData1 : PTypeData;
  vData2 : PTypeData;
  vCntr  : Integer;
  p      : Integer;
begin
  Result := 0;
  if aSet^.Kind = tkSet then begin
    vData1 := GetTypeData(aSet);
    vData2 := GetTypeData(vData1^.CompType);
    for vCntr := vData2^.MinValue to vData2^.MaxValue do begin
      p := pos(GetEnumName(vData1^.CompType, vCntr), Name);
      if p = 0 then
        Continue;
      if (p = 1) or (Name[p-1] = ',') then
        Result := Result or (1 shl vCntr);
    end;
  end;
end;


{ TSrcReportObjectList }

function TSrcReportObjectList.ClassNameToName(CN: string):string;
begin
  Result := RightStr(CN,Length(CN)-Length('TFPReport'));
end;

procedure TSrcReportObjectList.WriteSrcPage(AElement: TFPReportElement);
var
  FPN, FRN: string;
  aPage : TFPReportPage;
begin
  if not (AElement is TFPReportPage) then
    exit; //==>
  aPage:= TFPReportPage(AElement);
  if AElement.Parent = nil then
    FPN := 'FReport'
  else
    FPN := ClassNameToName(AElement.Parent.ClassName);
  FRN := ClassNameToName(AElement.ClassName);
  FSrc.Add(' '+FRN+' := TFPReportPage.Create('+FPN+');');
  FVari.Add(' '+FRN+' : TFPReportPage;');
  //
  FSrc.Add(' '+FRN+'.ColumnCount := '+IntToStr(aPage.ColumnCount)+ ';');
  FSrc.Add(' '+FRN+'.ColumnGap := '+FloatToStr(aPage.ColumnGap)+ ';');
  FSrc.Add(' '+FRN+'.ColumnLayout := '+ GetEnumName(TypeInfo(TFPReportColumnLayout),Ord(aPage.ColumnLayout))+ ';');
  FSrc.Add(' '+FRN+'.Orientation := '+GetEnumName(TypeInfo(TFPReportPaperOrientation),Ord(aPage.Orientation))+ ';');
  FSrc.Add(' '+FRN+'.Pagesize.PaperName := '''+aPage.PageSize.PaperName+ ''';');
  FSrc.Add(' '+FRN+'.Pagesize.Width := '+FloatToStr(aPage.PageSize.Width)+ ';');
  FSrc.Add(' '+FRN+'.Pagesize.Height := '+FloatToStr(aPage.PageSize.Height)+ ';');
  FSrc.Add(' '+FRN+'.Font.Name := '''+ aPage.Font.Name+ ''';');
  FSrc.Add(' '+FRN+'.Font.Size := '+IntToStr(aPage.Font.Size)+ ';');
  FSrc.Add(' '+FRN+'.Font.Color := TFPReportColor($'+IntToHex(aPage.Font.Color,8)+ ');');
  ////FDataName:=AReader.ReadString('Data','');
  ////if FDataName<>'' then
  ////  RestoreDataFromNames;
  if (aPage.Data = nil) or SameStr(aPage.Data.Name,'') then
    FSrc.Add(' //'+FRN+' has no Data')
  else
    FSrc.Add(' '+FRN+'.Data := '''+aPage.Data.Name+ ''';');
  FSrc.Add(' '+FRN+'.Margins.Top := '+FloatToStr(aPage.Margins.Top)+ ';');
  FSrc.Add(' '+FRN+'.Margins.Left := '+FloatToStr(aPage.Margins.Left)+ ';');
  FSrc.Add(' '+FRN+'.Margins.Bottom := '+FloatToStr(aPage.Margins.Bottom)+ ';');
  FSrc.Add(' '+FRN+'.Margins.Right := '+FloatToStr(aPage.Margins.Right)+ ';');
end;

procedure TSrcReportObjectList.WriteSrcTitleBand(AElement: TFPReportElement);
var
  FPN, FRN: string;
  aTitle : TFPReportTitleBand;
begin
  if not (AElement is TFPReportTitleBand) then
    exit; //==>
  aTitle:= TFPReportTitleBand(AElement);
  if AElement.Parent = nil then
    FPN := 'FB????'
  else
    FPN := ClassNameToName(AElement.Parent.ClassName);
  FRN := ClassNameToName(AElement.ClassName);
  FSrc.Append('');
  FSrc.Append('  // TitleBand    -- Parent='+FPN+' FRN='+FRN);
  FSrc.Add(' '+FRN+' := TFPReportTitleBand.Create('+FPN+');');
  FVari.Add(' '+FRN+' : TFPReportTitleBand;');
  FSrc.Add(' '+FRN+'.StretchMode := '+GetEnumName(TypeInfo(TFPReportStretchMode),Ord(aTitle.StretchMode))+ ';');
  FSrc.Add(' '+FRN+'.UseParentFont := '+BoolToStr(aTitle.UseParentFont,'True','False')+ ';');
  if not aTitle.UseParentFont then begin
    FSrc.Add(' '+FRN+'.Font.Name := '''+ aTitle.Font.Name+ ''';');
    FSrc.Add(' '+FRN+'.Font.Size := '+IntToStr(aTitle.Font.Size)+ ';');
    FSrc.Add(' '+FRN+'.Font.Color := TFPReportColor($'+IntToHex(aTitle.Font.Color,8)+ ');');
  end;
  FSrc.Add(' '+FRN+'.KeepTogetherWithChildren := '+BoolToStr(aTitle.KeepTogetherWithChildren,'True','False')+ ';');
  FSrc.Add(' //'+FRN+'.OnBeforePrint := @;');
end;

procedure TSrcReportObjectList.WriteSrcSummaryBand(AElement: TFPReportElement);
var
  FPN, FRN: string;
  aSummary : TFPReportSummaryBand;
begin
  if not (AElement is TFPReportMemo) then
    exit; //==>
  aSummary:= TFPReportSummaryBand(AElement);
  if AElement.Parent = nil then
    FPN := 'FB????'
  else
    FPN := ClassNameToName(AElement.Parent.ClassName);
  FRN := ClassNameToName(AElement.ClassName);
  FSrc.Append('');
  FSrc.Append('  // SummaryBand    -- Parent='+FPN+' FRN='+FRN);
  FSrc.Add(' '+FRN+' := TFPReportSummaryBand.Create('+FPN+');');
  FVari.Add(' '+FRN+' : TFPReportSummaryBand;');
  FSrc.Add(' '+FRN+'.StartNewPage := '+BoolToStr(aSummary.StartNewPage,'True','False')+ ';');
end;

procedure TSrcReportObjectList.WriteSrcPageHeaderBand(AElement: TFPReportElement);
var
  FPN, FRN: string;
  aHeader : TFPReportPageHeaderBand;
begin
  if not (AElement is TFPReportPageFooterBand) then
    exit; //==>
  aHeader:= TFPReportPageHeaderBand(AElement);
  if AElement.Parent = nil then
    FPN := 'FB????'
  else
    FPN := ClassNameToName(AElement.Parent.ClassName);
  FRN := ClassNameToName(AElement.ClassName);
  FSrc.Append('');
  FSrc.Append('  // PageHeaderBand    -- Parent='+FPN+' FRN='+FRN);
  //FSrc.Add(' '+FRN+' := TFPReportSummaryBand.Create('+FPN+');');
  //FVari.Add(' '+FRN+' : TFPReportSummaryBand;');
  //FSrc.Add(' '+FRN+'.StartNewPage := '+BoolToStr(aSummary.StartNewPage,'True','False')+ ';');
end;


procedure TSrcReportObjectList.WriteSrcPageFooterBand(AElement: TFPReportElement);
var
  FPN, FRN: string;
  aFooter : TFPReportPageFooterBand;
begin
  if not (AElement is TFPReportPageFooterBand) then
    exit; //==>
  aFooter:= TFPReportPageFooterBand(AElement);
  if AElement.Parent = nil then
    FPN := 'FB????'
  else
    FPN := ClassNameToName(AElement.Parent.ClassName);
  FRN := ClassNameToName(AElement.ClassName);
  FSrc.Append('');
  FSrc.Append('  // PageFootBand    -- Parent='+FPN+' FRN='+FRN);
  //FSrc.Add(' '+FRN+' := TFPReportSummaryBand.Create('+FPN+');');
  //FVari.Add(' '+FRN+' : TFPReportSummaryBand;');
  //FSrc.Add(' '+FRN+'.StartNewPage := '+BoolToStr(aSummary.StartNewPage,'True','False')+ ';');
end;


procedure TSrcReportObjectList.WriteSrcMemo(AElement: TFPReportElement);
var
  FPN, FRN: string;
  aMemo : TFPReportMemo;
  A: TFPReportMemoOptions;
begin
  if not (AElement is TFPReportMemo) then
    exit; //==>
  aMemo:= TFPReportMemo(AElement);
  if AElement.Parent = nil then
    FPN := 'F????'
  else
    FPN := ClassNameToName(AElement.Parent.ClassName);
  FRN := ClassNameToName(AElement.ClassName);
  FSrc.Append('');
  FSrc.Append('  // Memo    --  Parent='+FPN+' FRN='+FRN);
  FSrc.Add(' '+FRN+' := TFPReportMemo.Create('+FPN+');');
  FVari.Add(' '+FRN+' : TFPReportMemo;');
  FSrc.Add(' '+FRN+'.TextAlignment.Horizontal := '+GetEnumName(TypeInfo(TFPReportHorzTextAlignment),Ord(aMemo.TextAlignment.Horizontal))+ ';');
  FSrc.Add(' '+FRN+'.TextAlignment.Vertical := '+GetEnumName(TypeInfo(TFPReportVertTextAlignment),Ord(aMemo.TextAlignment.Vertical))+ ';');
  FSrc.Add(' '+FRN+'.TextAlignment.TopMargin := '+FloatToStr(aMemo.TextAlignment.TopMargin)+ ';');
  FSrc.Add(' '+FRN+'.TextAlignment.BottomMargin := '+FloatToStr(aMemo.TextAlignment.BottomMargin)+ ';');
  FSrc.Add(' '+FRN+'.TextAlignment.LeftMargin := '+FloatToStr(aMemo.TextAlignment.LeftMargin)+ ';');
  FSrc.Add(' '+FRN+'.TextAlignment.RightMargin := '+FloatToStr(aMemo.TextAlignment.RightMargin)+ ';');
  FSrc.Add(' '+FRN+'.UseParentFont := '+BoolToStr(aMemo.UseParentFont,'True','False')+ ';');
  if not aMemo.UseParentFont then begin
    FSrc.Add(' '+FRN+'.Font.Name := '''+ aMemo.Font.Name+ ''';');
    FSrc.Add(' '+FRN+'.Font.Size := '+IntToStr(aMemo.Font.Size)+ ';');
    FSrc.Add(' '+FRN+'.Font.Color := TFPReportColor($'+IntToHex(aMemo.Font.Color,8)+ ');');
  end;
  FSrc.Add(' '+FRN+'.Text := '''+ aMemo.Text+''';');
  FSrc.Add(' '+FRN+'.CullThreshold := '+IntToStr(aMemo.CullThreshold)+ ';');
  FSrc.Add(' '+FRN+'.LineSpacing := '+FloatToStr(aMemo.LineSpacing)+ ';');
  FSrc.Add(' '+FRN+'.LinkColor := TFPReportColor($'+IntToHex(aMemo.LinkColor,8)+ ');');

  //FSrc.Add(' '+FRN+'.Options := ['+ GetSetName(TFPReportMemoOptions, aMemo.Options)+ '];');
  //A := aMemo.Options;
  //FSrc.Add(' '+FRN+'.Options := '+ SetToString(TypeInfo(TFPReportMemoOptions),GetOrdProp(A,'Options'), true)+ ';');


  //FCullThreshold := AReader.ReadInteger('CullThreshold', CullThreshold);
  //FLineSpacing := AReader.ReadFloat('LineSpacing', LineSpacing);
  //FLinkColor := QWordToReportColor(AReader.ReadQWord('LinkColor', LinkColor));
  //Options := StringToMemoOptions(AReader.ReadString('Options', ''));

end;

procedure TSrcReportObjectList.WriteSrcShape(AElement: TFPReportElement);
var
  FPN, FRN: string;
  aShape : TFPReportShape;
begin
  if not (AElement is TFPReportShape) then
    exit; //==>
  aShape:= TFPReportShape(AElement);
  if AElement.Parent = nil then
    FPN := 'FB????'
  else
    FPN := ClassNameToName(AElement.Parent.ClassName);
  FRN := ClassNameToName(AElement.ClassName);
  FSrc.Append('');
  FSrc.Append('  // Shape    -- Parent='+FPN+' FRN='+FRN);
end;

procedure TSrcReportObjectList.WriteSrcCheckBox(AElement: TFPReportElement);
var
  FPN, FRN: string;
  aCB : TFPReportCheckbox;
begin
  if not (AElement is TFPReportCheckbox) then
    exit; //==>
  aCB:= TFPReportCheckbox(AElement);
  if AElement.Parent = nil then
    FPN := 'FB????'
  else
    FPN := ClassNameToName(AElement.Parent.ClassName);
  FRN := ClassNameToName(AElement.ClassName);
  FSrc.Append('');
  FSrc.Append('  // Checkbox    -- Parent='+FPN+' FRN='+FRN);
end;

procedure TSrcReportObjectList.WriteSrcUnkown(AElement: TFPReportElement);
var
  FPN, FRN: string;
  aEle : TFPReportElement;
begin
  if not (AElement is TFPReportElement) then
    exit; //==>
  aEle:= TFPReportElement(AElement);
  if AElement.Parent = nil then
    FPN := 'F????'
  else
    FPN := ClassNameToName(AElement.Parent.ClassName);
  FRN := ClassNameToName(AElement.ClassName);
  FSrc.Append('');
  FSrc.Append('  // Unkown Element -- '+ AElement.ClassName +' Parent='+FPN+' FRN='+FRN);
end;

procedure TSrcReportObjectList.WriteSrc(AElement: TReportObject);
begin
  if AElement.IsPage then begin
    WriteSrcPage(AElement.Element)
  end;
  if AElement.IsBand then begin
    if AElement.Element is TFPReportTitleBand then
      WriteSrcTitleBand(AElement.Element)
    else if AElement.Element is TFPReportSummaryBand then
      WriteSrcSummaryBand(AElement.Element)
    else if AElement.Element is TFPReportPageHeaderBand then
      WriteSrcPageHeaderBand(AElement.Element)
    else if AElement.Element is TFPReportPageFooterBand then
      WriteSrcPageFooterBand(AElement.Element)
    else
      WriteSrcUnkown(AElement.Element);
  end;
  if AElement.IsPlainElement then begin
    if AElement.Element is TFPReportMemo then
      WriteSrcMemo(AElement.Element)
    else if AElement.Element is TFPReportShape then
      WriteSrcShape(AElement.Element)
    else if AElement.Element is TFPReportCheckBox then
      WriteSrcCheckBox(AElement.Element)
    else WriteSrcUnkown(AElement.Element);
  end;
end;

procedure TSrcReportObjectList.LoadFromPage(APage: TFPReportCustomPage);
begin
  {$IFDEF DEBUGROL}Writeln('LoadFromPage');{$ENDIF}
  Clear;
  FSrc.Clear;
  FVari.Clear;
  AddElement(APage);
  FPage:=APage;
  ResetModified;
end;

function TSrcReportObjectList.AddElement(AElement: TFPReportElement
  ): TReportObject;
Var
  C : TFPReportElementWithChildren;
  I : Integer;
begin
{$IFDEF DEBUGROL}Writeln('AddElement  ',AElement.ClassName,' : ',AElement.Name);{$ENDIF}
  Result:=Add as TReportObject;
  Result.Element:=AElement;
  WriteSrc(Result);
  If AElement is TFPReportElementWithChildren then
    begin
    C:=AElement as TFPReportElementWithChildren;
    For I:=0 to C.ChildCount-1 do
      AddElement(C.Child[i]);
    end;
  ReportChanged;
end;

function TSrcReportObjectList.AddBand(ABand: TFPReportCustomBand
  ): TReportObject;
Var
  I : Integer;

begin
  {$IFDEF DEBUGROL}Writeln('AddBand  ',ABand.ClassName,' : ',ABand.Name);{$ENDIF}
  Result:=Add as TReportObject;
  Result.Element:=ABand;
  For I:=0 to ABand.ChildCount-1 do
    AddElement(ABand.Child[i]);
  ReportChanged;
end;

//function TSrcReportObjectList.AddSrcElement(AElement: TFPReportElement;
//                                            FPN, FRN: string
//  ): TReportObject;
//Var
//  C : TFPReportElementWithChildren;
//  I : Integer;
//  Name: string;
//begin
//{$IFDEF DEBUGROL}Writeln('Adding  ',AElement.ClassName,' : ',AElement.Name);{$ENDIF}
//{$IFDEF DEBUGROL}Writeln('Name=', ClassNameToName(AElement.ClassName), ' FPN=',FPN, ' FRN=',FRN);{$ENDIF}
//  Result:=Add as TReportObject;
//  Result.Element:=AElement;
//  If AElement is TFPReportElementWithChildren then
//    begin
//    C:=AElement as TFPReportElementWithChildren;
//    For I:=0 to C.ChildCount-1 do
//      Name := ClassNameToName(C.ClassName)+ RightStr('0000'+IntToStr(I),4);
//      AddSrcElement(C.Child[i],FRN,Name);
//    end;
//  ReportChanged;
//end;


{ TSrcFPReportDesignerControl }

procedure TSrcFPReportDesignerControl.SetPage(AValue: TFPReportCustomPage);
begin
  if FPage=AValue then Exit;
  FPage:=AValue;
  FObjects.LoadFromPage(AValue);
  //FObjects.OrderBands(12,96);
  Invalidate;
end;

constructor TSrcFPReportDesignerControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FObjects:= TSrcReportObjectList.Create(TReportObject);
end;

destructor TSrcFPReportDesignerControl.Destroy;
begin
  FObjects.Free;
  inherited Destroy;
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


