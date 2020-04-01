{
    Copyright (c) 2020 Andreas Friess
    Parts (from fpreport) are Copyright (c) 2008 Michael Van Canneyt

    report definition to pascal source file.

    Licence LGPL with extension

    See the file COPYING.modified.LGPL for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fp2src_dsgnctrl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, controls, fpreport, graphics, lcltype, menus,
  fpreportdesignobjectlist;


type

  TSrcFPReportCustomBand = class(TFPReportCustomBand)
  published
    property Font;
    property UseParentFont;
  end;


  { TSrcReportObjectList }

  TSrcReportObjectList = class(TReportObjectList)
  private
    FPage: TFPReportCustomPage;
    function ClassNameToName(CN: string): string;
    procedure MakeSrcReportFont(FRN: string; AAElement: TFPReportCustomBand);
    procedure MakeSrcReportFont(FRN: string; AAElement: TFPReportMemo);
    procedure MakeSrcReportFrame(FRN: string; AAElement: TFPReportElement);
    procedure WriteSrcPage(AElement: TFPReportElement);
    // Bands
    procedure WriteSrcTitleBand(AElement: TFPReportElement);
    procedure WriteSrcDateBand(AElement: TFPReportElement);
    procedure WriteSrcDataHeaderBand(AElement: TFPReportElement);
    procedure WriteSrcPageFooterBand(AElement: TFPReportElement);
    procedure WriteSrcPageHeaderBand(AElement: TFPReportElement);
    procedure WriteSrcSummaryBand(AElement: TFPReportElement);
    // Elements
    procedure WriteSrcMemo(AElement: TFPReportElement);
    procedure WriteSrcShape(AElement: TFPReportElement);
    procedure WriteSrcImage(AElement: TFPReportElement);
    procedure WriteSrcCheckBox(AElement: TFPReportElement);
    procedure WriteSrcUnkown(AElement: TFPReportElement);
    procedure WriteSrcUnkownBand(AElement: TFPReportElement);
    procedure WriteSrcUnkownPage(AElement: TFPReportElement);
    // Common
    procedure MakeSrcReportElement(FRN:string;AAElement: TFPReportElement);
  protected
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
  TypInfo, FPCanvas;

const
  coNoParent = 'NoParent';

var
  FSrc, FVari : TStringList;

procedure AddVari(aString: string);
begin
  if FVari.IndexOf(aString) < 0 then
    FVari.Append(aString);
end;

function GetSourceCode(AUnitName:string): string;
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
    TmpSrc.Append('uses');
    TmpSrc.Append('  FPCanvas;');
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
  FSrc.Append(' '+FRN+' := TFPReportPage.Create('+FPN+');');
  FVari.Add(' '+FRN+' : TFPReportPage;');
  //
  FSrc.Append(' '+FRN+'.ColumnCount := '+IntToStr(aPage.ColumnCount)+ ';');
  FSrc.Append(' '+FRN+'.ColumnGap := '+FloatToStr(aPage.ColumnGap)+ ';');
  FSrc.Append(' '+FRN+'.ColumnLayout := '+ GetEnumName(TypeInfo(TFPReportColumnLayout),Ord(aPage.ColumnLayout))+ ';');
  FSrc.Append(' '+FRN+'.Orientation := '+GetEnumName(TypeInfo(TFPReportPaperOrientation),Ord(aPage.Orientation))+ ';');
  FSrc.Append(' '+FRN+'.Pagesize.PaperName := '''+aPage.PageSize.PaperName+ ''';');
  FSrc.Append(' '+FRN+'.Pagesize.Width := '+FloatToStr(aPage.PageSize.Width)+ ';');
  FSrc.Append(' '+FRN+'.Pagesize.Height := '+FloatToStr(aPage.PageSize.Height)+ ';');
  FSrc.Append(' '+FRN+'.Font.Name := '''+ aPage.Font.Name+ ''';');
  FSrc.Append(' '+FRN+'.Font.Size := '+IntToStr(aPage.Font.Size)+ ';');
  FSrc.Append(' '+FRN+'.Font.Color := TFPReportColor($'+IntToHex(aPage.Font.Color,8)+ ');');
  ////FDataName:=AReader.ReadString('Data','');
  ////if FDataName<>'' then
  ////  RestoreDataFromNames;
  if (aPage.Data = nil) or SameStr(aPage.Data.Name,'') then
    FSrc.Append(' //'+FRN+' has no Data')
  else
    FSrc.Append(' '+FRN+'.Data := '''+aPage.Data.Name+ ''';');
  FSrc.Append(' '+FRN+'.Margins.Top := '+FloatToStr(aPage.Margins.Top)+ ';');
  FSrc.Append(' '+FRN+'.Margins.Left := '+FloatToStr(aPage.Margins.Left)+ ';');
  FSrc.Append(' '+FRN+'.Margins.Bottom := '+FloatToStr(aPage.Margins.Bottom)+ ';');
  FSrc.Append(' '+FRN+'.Margins.Right := '+FloatToStr(aPage.Margins.Right)+ ';');
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
    FPN := coNoParent
  else
    FPN := ClassNameToName(AElement.Parent.ClassName);
  FRN := ClassNameToName(AElement.ClassName);
  FSrc.Append('');
  FSrc.Append('  // TitleBand    -- Parent='+FPN+' FRN='+FRN);
  FSrc.Append(' '+FRN+' := TFPReportTitleBand.Create('+FPN+');');
  AddVari(' '+FRN+' : TFPReportTitleBand;');
  FSrc.Append(' '+FRN+'.StretchMode := '+GetEnumName(TypeInfo(TFPReportStretchMode),Ord(aTitle.StretchMode))+ ';');
  FSrc.Append(' '+FRN+'.UseParentFont := '+BoolToStr(aTitle.UseParentFont,'True','False')+ ';');
  if not aTitle.UseParentFont then begin
    FSrc.Append(' '+FRN+'.Font.Name := '''+ aTitle.Font.Name+ ''';');
    FSrc.Append(' '+FRN+'.Font.Size := '+IntToStr(aTitle.Font.Size)+ ';');
    FSrc.Append(' '+FRN+'.Font.Color := TFPReportColor($'+IntToHex(aTitle.Font.Color,8)+ ');');
  end;
  FSrc.Append(' '+FRN+'.KeepTogetherWithChildren := '+BoolToStr(aTitle.KeepTogetherWithChildren,'True','False')+ ';');
  FSrc.Append(' //'+FRN+'.OnBeforePrint := @;');
end;

procedure TSrcReportObjectList.WriteSrcDateBand(AElement: TFPReportElement);
var
  FPN, FRN: string;
  aData : TFPReportDataBand;
begin
  if not (AElement is TFPReportDataBand) then
    exit; //==>
  aData:= TFPReportDataBand(AElement);
  if AElement.Parent = nil then
    FPN := coNoParent
  else
    FPN := ClassNameToName(AElement.Parent.ClassName);
  FRN := ClassNameToName(AElement.ClassName);
  FSrc.Append('');
  FSrc.Append('  // DataBand    -- Parent='+FPN+' FRN='+FRN);
  FSrc.Append(' '+FRN+' := TFPReportDataBand.Create('+FPN+');');
  AddVari(' '+FRN+' : TFPReportDataBand;');
  FSrc.Append(' '+FRN+'.UseParentFont := '+BoolToStr(aData.UseParentFont,'True','False')+ ';');
  if not aData.UseParentFont then begin
    FSrc.Append(' '+FRN+'.Font.Name := '''+ aData.Font.Name+ ''';');
    FSrc.Append(' '+FRN+'.Font.Size := '+IntToStr(aData.Font.Size)+ ';');
    FSrc.Append(' '+FRN+'.Font.Color := TFPReportColor($'+IntToHex(aData.Font.Color,8)+ ');');
  end;
  FSrc.Append(' '+FRN+'.DisplayPosition := '+IntToStr(aData.DisplayPosition)+ ';');
  FSrc.Append(' '+FRN+'.StretchMode := '+GetEnumName(TypeInfo(TFPReportStretchMode),Ord(aData.StretchMode))+ ';');
  FSrc.Append(' '+FRN+'.VisibleOnPage := '+GetEnumName(TypeInfo(TFPReportVisibleOnPage),Ord(aData.VisibleOnPage))+ ';');
  FSrc.Append(' '+FRN+'.KeepTogetherWithChildren := '+BoolToStr(aData.KeepTogetherWithChildren,'True','False')+ ';');
  FSrc.Append(' //'+FRN+'.OnBeforePrint := @;');
end;

procedure TSrcReportObjectList.WriteSrcDataHeaderBand(AElement: TFPReportElement
  );
var
  FPN, FRN: string;
  aBand : TFPReportDataHeaderBand;
begin
  if not (AElement is TFPReportDataHeaderBand) then
    exit; //==>
  aBand:= TFPReportDataHeaderBand(AElement);
  if AElement.Parent = nil then
    FPN := coNoParent
  else
    FPN := ClassNameToName(AElement.Parent.ClassName);
  FRN := ClassNameToName(AElement.ClassName);
  FSrc.Append('');
  FSrc.Append('  // DataHeaderBand    -- Parent='+FPN+' FRN='+FRN);
  FSrc.Append(' '+FRN+' := TFPReportDataHeaderBand.Create('+FPN+');');
  AddVari(' '+FRN+' : TFPReportDataHeaderBand;');
  FSrc.Append(' '+FRN+'.UseParentFont := '+BoolToStr(aBand.UseParentFont,'True','False')+ ';');
  if not aBand.UseParentFont then begin
    FSrc.Append(' '+FRN+'.Font.Name := '''+ aBand.Font.Name+ ''';');
    FSrc.Append(' '+FRN+'.Font.Size := '+IntToStr(aBand.Font.Size)+ ';');
    FSrc.Append(' '+FRN+'.Font.Color := TFPReportColor($'+IntToHex(aBand.Font.Color,8)+ ');');
  end;
  FSrc.Append(' '+FRN+'.StretchMode := '+GetEnumName(TypeInfo(TFPReportStretchMode),Ord(aBand.StretchMode))+ ';');
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
    FPN := coNoParent
  else
    FPN := ClassNameToName(AElement.Parent.ClassName);
  FRN := ClassNameToName(AElement.ClassName);
  FSrc.Append('');
  FSrc.Append('  // SummaryBand    -- Parent='+FPN+' FRN='+FRN);
  FSrc.Append(' '+FRN+' := TFPReportSummaryBand.Create('+FPN+');');
  AddVari(' '+FRN+' : TFPReportSummaryBand;');
  FSrc.Append(' '+FRN+'.UseParentFont := '+BoolToStr(aSummary.UseParentFont,'True','False')+ ';');
  if not aSummary.UseParentFont then begin
    FSrc.Append(' '+FRN+'.Font.Name := '''+ aSummary.Font.Name+ ''';');
    FSrc.Append(' '+FRN+'.Font.Size := '+IntToStr(aSummary.Font.Size)+ ';');
    FSrc.Append(' '+FRN+'.Font.Color := TFPReportColor($'+IntToHex(aSummary.Font.Color,8)+ ');');
  end;
  FSrc.Append(' '+FRN+'.StartNewPage := '+BoolToStr(aSummary.StartNewPage,'True','False')+ ';');
end;

procedure TSrcReportObjectList.WriteSrcPageHeaderBand(AElement: TFPReportElement);
var
  FPN, FRN: string;
  aHeader : TFPReportPageHeaderBand;
begin
  if not (AElement is TFPReportPageHeaderBand) then
    exit; //==>
  aHeader:= TFPReportPageHeaderBand(AElement);
  if AElement.Parent = nil then
    FPN := coNoParent
  else
    FPN := ClassNameToName(AElement.Parent.ClassName);
  FRN := ClassNameToName(AElement.ClassName);
  FSrc.Append('');
  FSrc.Append('  // PageHeaderBand    -- Parent='+FPN+' FRN='+FRN);
  FSrc.Append(' '+FRN+' := TFPReportPageHeaderBand.Create('+FPN+');');
  AddVari(' '+FRN+' : TFPReportPageHeaderBand;');
  FSrc.Append(' '+FRN+'.UseParentFont := '+BoolToStr(aHeader.UseParentFont,'True','False')+ ';');
  if not aHeader.UseParentFont then begin
    FSrc.Append(' '+FRN+'.Font.Name := '''+ aHeader.Font.Name+ ''';');
    FSrc.Append(' '+FRN+'.Font.Size := '+IntToStr(aHeader.Font.Size)+ ';');
    FSrc.Append(' '+FRN+'.Font.Color := TFPReportColor($'+IntToHex(aHeader.Font.Color,8)+ ');');
  end;
  //FSrc.Append(' '+FRN+'.StartNewPage := '+BoolToStr(aSummary.StartNewPage,'True','False')+ ';');
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
    FPN := coNoParent
  else
    FPN := ClassNameToName(AElement.Parent.ClassName);
  FRN := ClassNameToName(AElement.ClassName);
  FSrc.Append('');
  FSrc.Append('  // PageFootBand    -- Parent='+FPN+' FRN='+FRN);
  FSrc.Append(' '+FRN+' := TFPReportPageFooterBand.Create('+FPN+');');
  AddVari(' '+FRN+' : TFPReportPageFooterBand;');
  FSrc.Append(' '+FRN+'.UseParentFont := '+BoolToStr(aFooter.UseParentFont,'True','False')+ ';');
  if not aFooter.UseParentFont then begin
    FSrc.Append(' '+FRN+'.Font.Name := '''+ aFooter.Font.Name+ ''';');
    FSrc.Append(' '+FRN+'.Font.Size := '+IntToStr(aFooter.Font.Size)+ ';');
    FSrc.Append(' '+FRN+'.Font.Color := TFPReportColor($'+IntToHex(aFooter.Font.Color,8)+ ');');
  end;
  FSrc.Append(' '+FRN+'.KeepTogetherWithChildren := '+BoolToStr(aFooter.KeepTogetherWithChildren,'True','False')+ ';');
end;


procedure TSrcReportObjectList.WriteSrcMemo(AElement: TFPReportElement);
var
  FPN, FRN: string;
  aMemo : TFPReportMemo;
begin
  if not (AElement is TFPReportMemo) then
    exit; //==>
  aMemo:= TFPReportMemo(AElement);
  if AElement.Parent = nil then
    FPN := coNoParent
  else
    FPN := ClassNameToName(AElement.Parent.ClassName);
  FRN := ClassNameToName(AElement.ClassName);
  FSrc.Append('');
  FSrc.Append('  // Memo    --  Parent='+FPN+' FRN='+FRN);
  FSrc.Append(' '+FRN+' := TFPReportMemo.Create('+FPN+');');
  AddVari(' '+FRN+' : TFPReportMemo;');
  MakeSrcReportElement(FRN,AElement);
  FSrc.Append(' '+FRN+'.TextAlignment.Horizontal := '+GetEnumName(TypeInfo(TFPReportHorzTextAlignment),Ord(aMemo.TextAlignment.Horizontal))+ ';');
  FSrc.Append(' '+FRN+'.TextAlignment.Vertical := '+GetEnumName(TypeInfo(TFPReportVertTextAlignment),Ord(aMemo.TextAlignment.Vertical))+ ';');
  FSrc.Append(' '+FRN+'.TextAlignment.TopMargin := '+FloatToStr(aMemo.TextAlignment.TopMargin)+ ';');
  FSrc.Append(' '+FRN+'.TextAlignment.BottomMargin := '+FloatToStr(aMemo.TextAlignment.BottomMargin)+ ';');
  FSrc.Append(' '+FRN+'.TextAlignment.LeftMargin := '+FloatToStr(aMemo.TextAlignment.LeftMargin)+ ';');
  FSrc.Append(' '+FRN+'.TextAlignment.RightMargin := '+FloatToStr(aMemo.TextAlignment.RightMargin)+ ';');
  MakeSrcReportFont(FRN,aMemo);
  FSrc.Append(' '+FRN+'.Text := '''+ aMemo.Text+''';');
  FSrc.Append(' '+FRN+'.CullThreshold := '+IntToStr(aMemo.CullThreshold)+ ';');
  FSrc.Append(' '+FRN+'.LineSpacing := '+FloatToStr(aMemo.LineSpacing)+ ';');
  FSrc.Append(' '+FRN+'.LinkColor := TFPReportColor($'+IntToHex(aMemo.LinkColor,8)+ ');');
  FSrc.Append(' '+FRN+'.Options := '+ SetToString(PtypeInfo(TypeInfo(TFPReportMemoOptions)),integer(aMemo.Options), true)+ ';');
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
    FPN := coNoParent
  else
    FPN := ClassNameToName(AElement.Parent.ClassName);
  FRN := ClassNameToName(AElement.ClassName);
  FSrc.Append('');
  FSrc.Append('  // Shape    -- Parent='+FPN+' FRN='+FRN);
  FSrc.Append(' '+FRN+' := TFPReportShape.Create('+FPN+');');
  AddVari(' '+FRN+' : TFPReportShape;');
  MakeSrcReportElement(FRN,AElement);
  FSrc.Append(' '+FRN+'.ShapeType := '+GetEnumName(TypeInfo(TFPReportShapeType),Ord(aShape.ShapeType))+ ';');
  FSrc.Append(' '+FRN+'.Orientation := '+GetEnumName(TypeInfo(TFPReportOrientation),Ord(aShape.Orientation))+ ';');
  FSrc.Append(' '+FRN+'.CornerRadius := '+FloatToStr(aShape.CornerRadius)+ ';');
  FSrc.Append(' '+FRN+'.Color := TFPReportColor($'+IntToHex(aShape.Color,8)+ ');');
  FSrc.Append(' '+FRN+'.StretchMode := '+GetEnumName(TypeInfo(TFPReportStretchMode),Ord(aShape.StretchMode))+ ';');
end;

procedure TSrcReportObjectList.WriteSrcImage(AElement: TFPReportElement);
var
  FPN, FRN: string;
  aImage : TFPReportImage;
begin
  if not (AElement is TFPReportShape) then
    exit; //==>
  aImage:= TFPReportImage(AElement);
  if AElement.Parent = nil then
    FPN := coNoParent
  else
    FPN := ClassNameToName(AElement.Parent.ClassName);
  FRN := ClassNameToName(AElement.ClassName);
  FSrc.Append('');
  FSrc.Append('  // Image    -- Parent='+FPN+' FRN='+FRN);
  FSrc.Append(' '+FRN+' := TFPReportImage.Create('+FPN+');');
  AddVari(' '+FRN+' : TFPReportImage;');
  MakeSrcReportElement(FRN,AElement);
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
    FPN := coNoParent
  else
    FPN := ClassNameToName(AElement.Parent.ClassName);
  FRN := ClassNameToName(AElement.ClassName);
  FSrc.Append('');
  FSrc.Append('  // Checkbox    -- Parent='+FPN+' FRN='+FRN);
  FSrc.Append(' '+FRN+' := TFPReportCheckbox.Create('+FPN+');');
  AddVari(' '+FRN+' : TFPReportCheckbox;');
  MakeSrcReportElement(FRN,AElement);
  FSrc.Append(' '+FRN+'.CornerRadius := '''+aCB.Expression+ ''';');
  FSrc.Append(' '+FRN+'.TrueImageID := '+IntToStr(aCB.TrueImageID)+ ';');
  FSrc.Append(' '+FRN+'.FalseImageID := '+IntToStr(aCB.FalseImageID)+ ';');
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
    FPN := coNoParent
  else
    FPN := ClassNameToName(AElement.Parent.ClassName);
  FRN := ClassNameToName(AElement.ClassName);
  FSrc.Append('');
  FSrc.Append('  // Unkown Element -- '+ AElement.ClassName +' Parent='+FPN+' FRN='+FRN);
end;

procedure TSrcReportObjectList.WriteSrcUnkownBand(AElement: TFPReportElement);
var
  FPN, FRN: string;
  aEle : TFPReportElement;
begin
  if not (AElement is TFPReportElement) then
    exit; //==>
  aEle:= TFPReportElement(AElement);
  if AElement.Parent = nil then
    FPN := coNoParent
  else
    FPN := ClassNameToName(AElement.Parent.ClassName);
  FRN := ClassNameToName(AElement.ClassName);
  FSrc.Append('');
  FSrc.Append('  // Unkown Band Element -- '+ AElement.ClassName +' Parent='+FPN+' FRN='+FRN);
end;

procedure TSrcReportObjectList.WriteSrcUnkownPage(AElement: TFPReportElement);
var
  FPN, FRN: string;
  aEle : TFPReportElement;
begin
  if not (AElement is TFPReportElement) then
    exit; //==>
  aEle:= TFPReportElement(AElement);
  if AElement.Parent = nil then
    FPN := coNoParent
  else
    FPN := ClassNameToName(AElement.Parent.ClassName);
  FRN := ClassNameToName(AElement.ClassName);
  FSrc.Append('');
  FSrc.Append('  // Unkown Page Element -- '+ AElement.ClassName +' Parent='+FPN+' FRN='+FRN);
end;

procedure TSrcReportObjectList.MakeSrcReportFont(FRN: string;
  AAElement: TFPReportCustomBand);
var
  aFontEl : TSrcFPReportCustomBand;
begin
  aFontEl := AAElement as TSrcFPReportCustomBand;
  // Font
  FSrc.Append(' '+FRN+'.UseParentFont := '+BoolToStr(aFontEl.UseParentFont,'True','False')+ ';');
  if not aFontEl.UseParentFont then begin
    FSrc.Append(' '+FRN+'.Font.Name := '''+ aFontEl.Font.Name+ ''';');
    FSrc.Append(' '+FRN+'.Font.Size := '+IntToStr(aFontEl.Font.Size)+ ';');
    FSrc.Append(' '+FRN+'.Font.Color := TFPReportColor($'+IntToHex(aFontEl.Font.Color,8)+ ');');
  end;
end;

procedure TSrcReportObjectList.MakeSrcReportFont(FRN: string;
  AAElement: TFPReportMemo);
begin
  // Font
  FSrc.Append(' '+FRN+'.UseParentFont := '+BoolToStr(AAElement.UseParentFont,'True','False')+ ';');
  if not AAElement.UseParentFont then begin
    FSrc.Append(' '+FRN+'.Font.Name := '''+ AAElement.Font.Name+ ''';');
    FSrc.Append(' '+FRN+'.Font.Size := '+IntToStr(AAElement.Font.Size)+ ';');
    FSrc.Append(' '+FRN+'.Font.Color := TFPReportColor($'+IntToHex(AAElement.Font.Color,8)+ ');');
  end;
end;


procedure TSrcReportObjectList.MakeSrcReportFrame(FRN: string;
  AAElement: TFPReportElement);
begin
  // Frame
  FSrc.Append(' '+FRN+'.Frame.Lines := '+ SetToString(PtypeInfo(TypeInfo(TFPReportFrameLines)),integer(AAElement.Frame.Lines), true)+ ';');
  FSrc.Append(' '+FRN+'.Frame.Shape := '+GetEnumName(TypeInfo(TFPReportShapeType),Ord(AAElement.Frame.Shape))+ ';');
  FSrc.Append(' '+FRN+'.Frame.Color := TFPReportColor($'+IntToHex(AAElement.Frame.Color,8)+ ');');
  FSrc.Append(' '+FRN+'.Frame.BackgroundColor := TFPReportColor($'+IntToHex(AAElement.Frame.BackgroundColor,8)+ ');');
  FSrc.Append(' '+FRN+'.Frame.Pen := '+GetEnumName(TypeInfo(TFPPenStyle),Ord(AAElement.Frame.Pen))+ ';');
  FSrc.Append(' '+FRN+'.Frame.Width := '+IntToStr(AAElement.Frame.Width)+ ';');
end;

procedure TSrcReportObjectList.MakeSrcReportElement(FRN: string;
  AAElement: TFPReportElement);
begin
  // Layout
  //property Layout: TFPReportLayout read FLayout write SetLayout;
  // Frame
  MakeSrcReportFrame(FRN,AAElement);
  //
  FSrc.Append(' '+FRN+'.Visible := '+BoolToStr(AAElement.Visible,'True','False')+ ';');
  FSrc.Append(' '+FRN+'.VisibleExpr := '''+AAElement.VisibleExpr+ ''';');
end;

procedure TSrcReportObjectList.WriteSrc(AElement: TReportObject);
begin
  if AElement.IsPage then begin
    if AElement.Element is TFPReportPage then
        WriteSrcPage(AElement.Element)
      else
        WriteSrcUnkownPage(AElement.Element);
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
    else if AElement.Element is TFPReportDataBand then
      WriteSrcDateBand(AElement.Element)
    else if AElement.Element is TFPReportDataHeaderBand then
      WriteSrcDataHeaderBand(AElement.Element)
    else
      WriteSrcUnkownBand(AElement.Element);
  end;
  if AElement.IsPlainElement then begin
    if AElement.Element is TFPReportMemo then
      WriteSrcMemo(AElement.Element)
    else if AElement.Element is TFPReportShape then
      WriteSrcShape(AElement.Element)
    else if AElement.Element is TFPReportImage then
      WriteSrcImage(AElement.Element)
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
end;

{ TSrcFPReportDesignerControl }

procedure TSrcFPReportDesignerControl.SetPage(AValue: TFPReportCustomPage);
begin
  if FPage=AValue then Exit;
  FPage:=AValue;
  FObjects.LoadFromPage(AValue);
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


