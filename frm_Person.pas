unit frm_Person;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, DBCtrls, ExtCtrls, Grids, DBGrids, ToolWin, ActnMan,
  ActnCtrls, ActnList, XPStyleActnCtrls, dblookup, DB, Buttons;

type
  TfrmPerson = class(TForm)
    ActionManager1: TActionManager;
    acSave: TAction;
    acAdd: TAction;
    acDelete: TAction;
    GridPanel1: TGridPanel;
    ActionToolBar1: TActionToolBar;
    Panel1: TPanel;
    DBGrid1: TDBGrid;
    Splitter1: TSplitter;
    FlowPanel1: TFlowPanel;
    Label1: TLabel;
    DBEdit1: TDBEdit;
    Panel2: TPanel;
    DBEdit2: TDBEdit;
    Label2: TLabel;
    srcPerson: TDataSource;
    srcWorkgroup: TDataSource;
    acClose: TAction;
    Panel6: TPanel;
    Label6: TLabel;
    DBEdit6: TDBEdit;
    Panel7: TPanel;
    Label7: TLabel;
    DBEdit7: TDBEdit;
    acUser: TAction;
    Panel3: TPanel;
    Label3: TLabel;
    btnDivision: TSpeedButton;
    Division: TDBLookupComboBox;
    Panel4: TPanel;
    Label4: TLabel;
    btnPosition: TSpeedButton;
    Position: TDBLookupComboBox;
    srcDivision: TDataSource;
    srcPosition: TDataSource;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acAddExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure acUserExecute(Sender: TObject);
    procedure btnPositionClick(Sender: TObject);
    procedure btnDivisionClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPerson: TfrmPerson;

implementation
uses dm_GUI, frm_Workgroup, frm_User, frm_Division, frm_Position;

{$R *.dfm}

procedure TfrmPerson.acAddExecute(Sender: TObject);
begin
  srcPerson.DataSet.Append;
end;

procedure TfrmPerson.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmPerson.acDeleteExecute(Sender: TObject);
begin
  if not srcPerson.DataSet.IsEmpty then
  begin
    if not dmGUI.ConfirmDelete then Exit;
    srcPerson.DataSet.Delete;
  end;
end;

procedure TfrmPerson.acSaveExecute(Sender: TObject);
begin
;
end;

procedure TfrmPerson.acUserExecute(Sender: TObject);
begin
  dmGUI.PersonEdit.CheckBrowseMode;
  if not dmGUI.PersonEdit.FieldByName('ID').IsNull then
    EditUser(dmGUI.PersonEdit.FieldByName('ID').AsInteger);
end;

procedure TfrmPerson.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  dmGUI.PersonEdit.CheckBrowseMode;
  dmGUI.PersonEdit.Close;
  Action:=caFree;
end;

procedure TfrmPerson.FormDeactivate(Sender: TObject);
begin
  if  dmGUI.PersonList.Active then dmGUI.PersonList.Requery;
end;

procedure TfrmPerson.FormDestroy(Sender: TObject);
begin
  frmPerson:=nil;
end;

procedure TfrmPerson.FormShow(Sender: TObject);
begin
  dmGUI.DivisionList.Open;
  dmGUI.PositionList.Open;
  dmGUI.PersonEdit.Open;
end;

procedure TfrmPerson.btnDivisionClick(Sender: TObject);
begin
  dmGUI.ShowForm(TfrmDivision,True,frmDivision);
end;

procedure TfrmPerson.btnPositionClick(Sender: TObject);
begin
  dmGUI.ShowForm(TfrmPosition,True,frmPosition,False);
  if frmPosition.srcPositionEdit.DataSet=nil then
  begin
    frmPosition.srcPositionEdit.DataSet:=dmGUI.PositionEdit;
  end;
  if frmPosition.srcPositionList.DataSet=nil then
    frmPosition.srcPositionList.DataSet:=dmGUI.PositionList;
  dmGUI.PositionEdit.Open;
  frmPosition.ShowModal;
end;

end.
