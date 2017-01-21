unit frm_Report;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, OleCtrls, SHDocVw, ActnList, XPStyleActnCtrls,
  ActnMan, ToolWin, ActnCtrls;

type
  TfrmReport = class(TForm)
    Panel1: TPanel;
    Label11: TLabel;
    ReportName: TEdit;
    browser: TWebBrowser;
    ActionToolBar1: TActionToolBar;
    ActionManager1: TActionManager;
    acClose: TAction;
    ReportID: TEdit;
    Label15: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure browserNavigateComplete2(ASender: TObject; const pDisp: IDispatch;
      var URL: OleVariant);
    procedure acCloseExecute(Sender: TObject);
  private
    procedure LoadPage;
  public
    procedure LoadReport(FileName: String);
  end;

implementation

uses dm_GUI;

{$R *.dfm}

procedure TfrmReport.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmReport.browserNavigateComplete2(ASender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
begin
  LoadPage;
end;

procedure TfrmReport.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
end;

procedure TfrmReport.LoadPage;
var V,VV:Variant;
  ID:Integer;
begin
  V:=Browser.Document;
  VV:=V.open('text/html','replace');
  try
    VV.Write('<xmp>'#13#10+
             dmGUI.LoadFile('справки/събития',ReportName.Text,ID)+
             '</xmp>'#13#10);
    ReportID.Text:=IntToStr(ID);         
  finally
    VV.close();
  end;
  V.Close;
end;

procedure TfrmReport.LoadReport(FileName: String);
begin
  ReportName.Text:=FileName;
  Hint:=FileName;
  Browser.Navigate('about:blank');
end;

end.
