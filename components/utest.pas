unit uTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, csvdataset, DB, fpjsondataset, fpreport, Forms, Controls,
  Graphics, Dialogs, StdCtrls, DBGrids;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CSVDataset1: TCSVDataset;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    FPReport1: TFPReport;
    JSONDataSet1: TJSONDataSet;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses
  fpjson;


{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  fs : TFileStream;
  AllJSON, DesignDataJSON: TJSONObject;
  ArrayJSON : TJSONArray;
begin
  JSONDataSet1.Active:= false;
  fs:= TFileStream.Create('C:\Data\Pascal\GITHUB\fpReportSamples\source\03_SimpleReportJSON\testJSON.json', fmOpenRead or fmShareDenyNone);
  try
    // ReportData AND DesignData
    AllJSON:= TJSONObject(GetJSON(fs));
    ArrayJSON := AllJSON.Get('Invoice',TJSONArray(nil));
    Memo1.Append(ArrayJSON.ToString);
    JSONDataSet1.Rows:= ArrayJSON;
    JSONDataSet1.OwnsData:=true;
//    JSONDataSet1.Active:=true;
    //// Extract DesignData only
    //DesignDataJSON:= AllJSON.Get('DataItems',TJSONArray(nil));

  finally
    AllJSON.Free;
    fs.Free;
  end;
end;


end.

