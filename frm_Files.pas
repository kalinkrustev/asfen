unit frm_Files;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, XPStyleActnCtrls, ActnMan, ToolWin, ActnCtrls, ComCtrls,
  DB, ADODB;

type
  TfrmFiles = class(TForm)
    ActionToolBar1: TActionToolBar;
    ActionManager1: TActionManager;
    acScript: TAction;
    acRecording: TAction;
    acNew: TAction;
    acDelete: TAction;
    Files: TListView;
    FileList: TADODataSet;
    acClose: TAction;
    acEdit: TAction;
    acEvent: TAction;
    acEventReport: TAction;
    acDetails: TAction;
    acRename: TAction;
    procedure acScriptExecute(Sender: TObject);
    procedure acRecordingExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FilesDblClick(Sender: TObject);
    procedure acNewExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acEditExecute(Sender: TObject);
    procedure FilesKeyPress(Sender: TObject; var Key: Char);
    procedure FilesEdited(Sender: TObject; Item: TListItem; var S: string);
    procedure FilesDeletion(Sender: TObject; Item: TListItem);
    procedure FilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure acEventExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure acEventReportExecute(Sender: TObject);
    procedure acDetailsExecute(Sender: TObject);
    procedure FilesCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure FilesColumnClick(Sender: TObject; Column: TListColumn);
    procedure FilesEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure acRenameExecute(Sender: TObject);
    procedure acRenameUpdate(Sender: TObject);
  private
    FPath:String;
    procedure ListFiles;
    procedure SetPath(const Value: String);
    procedure EditFile;
    procedure DeleteSelected;
  public
    property Path:String read FPath write SetPath;
  end;

var
  frmFilesScripts: TfrmFiles;
  frmFilesMessages: TfrmFiles;
  frmFilesEvents: TfrmFiles;
  frmFilesReports: TfrmFiles;

implementation
uses dm_GUI, frm_Script, frm_Wave, frm_Event, frm_Report, dm_Images, Util;
{$R *.dfm}

procedure TfrmFiles.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmFiles.acDeleteExecute(Sender: TObject);
begin
  DeleteSelected;
end;

procedure TfrmFiles.acEditExecute(Sender: TObject);
begin
  EditFile;
end;

procedure TfrmFiles.acNewExecute(Sender: TObject);
var F:String;
begin
  if Path='сценарии' then
  begin
    if Files.Items.Count >= Limits.ScriptCount then
    begin
      PopControlHint(Files,'Позволено е дефиниране на максимум '+IntToStr(Limits.ScriptCount)+' сценария в системата');
      Abort;
    end;
  end else
  if Path='съобщения' then
  begin
    if Files.Items.Count >= Limits.SoundCount then
    begin
      PopControlHint(Files,'Позволено е дефиниране на максимум '+IntToStr(Limits.SoundCount)+' съобщения в системата');
      Abort;
    end;
  end;
  F:='нов файл';
  if not InputQuery('Създаване на файл','Име на файла',F) then Exit;
  dmGUI.SaveFile(Path,F,'',False);
  ListFiles;
  Files.Selected:=Files.FindCaption(0,F,False,True,False);  
end;

procedure TfrmFiles.acRecordingExecute(Sender: TObject);
begin
  Path:='съобщения';
end;

procedure TfrmFiles.acScriptExecute(Sender: TObject);
begin
  Path:='сценарии';
end;

procedure TfrmFiles.acRenameExecute(Sender: TObject);
begin
  if Files.ItemFocused<>nil then
    Files.ItemFocused.EditCaption;
end;

procedure TfrmFiles.acRenameUpdate(Sender: TObject);
begin
  acRename.Visible:=acEdit.Visible;
end;

procedure TfrmFiles.acDetailsExecute(Sender: TObject);
begin
  if acDetails.Checked then
    Files.ViewStyle:=vsReport
  else
    Files.ViewStyle:=vsList;
end;

procedure TfrmFiles.acEventExecute(Sender: TObject);
begin
  Path:='събития';
end;

procedure TfrmFiles.acEventReportExecute(Sender: TObject);
begin
  Path:='справки/събития';
end;

procedure TfrmFiles.DeleteSelected;
begin
  if (Files.Selected<>nil)
  and dmGUI.DeleteDBFile(Path,Files.Selected.Caption) then
      ListFiles;
end;

procedure TfrmFiles.EditFile;
var F:TfrmScript;
    W:TfrmWave;
    E:TfrmEvent;
    R:TfrmReport;
begin
  if Files.ItemFocused=nil then Exit;

  if SameText(Path,'сценарии') then
  begin
    F:=nil;
    dmGUI.ShowForm(TfrmScript,false,F);
    F.LoadScript(Files.ItemFocused.Caption);
    dmImages.UpdateTab(F);
  end else
  if SameText(Path,'събития') then
  begin
    E:=nil;
    dmGUI.ShowForm(TfrmEvent,false,E);
    E.LoadEvent(Files.ItemFocused.Caption);
    dmImages.UpdateTab(E);
  end else
  if SameText(Path,'съобщения') then
  begin
    W:=nil;
    dmGUI.ShowForm(TfrmWave,false,W);
    W.LoadWave(Files.ItemFocused.Caption);
    dmImages.UpdateTab(W);
  end else
  if SameText(Path,'справки/събития') then
  begin
    R:=nil;
    dmGUI.ShowForm(TfrmReport,false,R);
    R.LoadReport(Files.ItemFocused.Caption);
    dmImages.UpdateTab(R);
  end;

end;

procedure TfrmFiles.FilesColumnClick(Sender: TObject; Column: TListColumn);
begin
  if Column.ID+1=Abs(Files.Tag) then
    Files.Tag:=-Files.Tag
  else
    Files.Tag:=Column.ID+1;
  Files.AlphaSort;
end;

procedure TfrmFiles.FilesCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
var S1,S2:String;
    C:Integer;
begin
  C:=Abs(Files.Tag)-1;
  if (C>=0) and (C<=3) then
  begin
    if C=0 then
    begin
      S1:=Item1.Caption;
      S2:=Item2.Caption;
    end else
    begin
      C:=C-1;
      if C>=Item1.SubItems.Count then
        S1:=''
      else
        S1:=Item1.SubItems[C];
      if C>=Item2.SubItems.Count then
        S2:=''
      else
        S2:=Item2.SubItems[C];
      if C=0 then
      begin
        while Length(S1)<Length(S2) do S1:='0'+S1;
        while Length(S2)<Length(S1) do S2:='0'+S2;
      end;
    end;
    Compare:=lstrcmp(PChar(S1), PChar(S2));
    if Files.Tag<0 then Compare:=-Compare;
  end
  else
    Compare:=0;
end;

procedure TfrmFiles.FilesDblClick(Sender: TObject);
begin
  EditFile;  
end;

procedure TfrmFiles.FilesDeletion(Sender: TObject; Item: TListItem);
begin
  if not dmGUI.DeleteDBFile(Path,Item.Caption) then Abort;
end;

procedure TfrmFiles.FilesEdited(Sender: TObject; Item: TListItem;
  var S: string);
begin
  if not dmGUI.RenameFile(Path,Item.Caption,S) then Abort;
end;

procedure TfrmFiles.FilesEditing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  AllowEdit:=acEdit.Visible;
end;

procedure TfrmFiles.FilesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_DELETE then
  begin
    DeleteSelected;
    Key:=0;
  end;
end;

procedure TfrmFiles.FilesKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then EditFile;
end;

procedure TfrmFiles.FormActivate(Sender: TObject);
begin
  Realign;
end;

procedure TfrmFiles.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
end;

procedure TfrmFiles.FormDestroy(Sender: TObject);
begin
  if Self=frmFilesEvents then frmFilesEvents:=nil;
  if Self=frmFilesScripts then frmFilesScripts:=nil;
  if Self=frmFilesMessages then frmFilesMessages:=nil;
  if Self=frmFilesReports then frmFilesReports:=nil;
end;

procedure TfrmFiles.FormShow(Sender: TObject);
begin
  acScript.Execute;
end;

procedure TfrmFiles.ListFiles;
var F:TListItem;
begin
  Files.Items.BeginUpdate;
  try
    Files.Clear;
    FileList.Close;
    FileList.CommandText:=Format('select f.FileName,f.FileID,f.LastChange,u.UserName from tblFiles f left join tblUser u on u.id=f.lastuser where f.FilePath=''%s'' order by 1',[Path]);
    FileList.Open;
    while not FileList.Eof do
    begin
      F:=Files.Items.Add;
      F.Caption:=FileList.FieldByName('FileName').AsString;
      F.SubItems.Text:=FileList.FieldByName('FileID').AsString+#13#10+
                       FileList.FieldByName('UserName').AsString+#13#10+
                       FileList.FieldByName('LastChange').AsString;
      if SameText(Path,'съобщения') then
        F.ImageIndex:=acRecording.ImageIndex
      else
      if SameText(Path,'сценарии') then
        F.ImageIndex:=acScript.ImageIndex
      else
      if SameText(Path,'събития') then
        F.ImageIndex:=acEvent.ImageIndex
      else
      if SameText(Path,'справки/събития') then
        F.ImageIndex:=acEventReport.ImageIndex;
      FileList.Next;
    end;
    Files.Tag:=1;
    Files.ViewStyle:=vsSmallIcon;
    if acDetails.Checked then
      Files.ViewStyle:=vsReport
    else
      Files.ViewStyle:=vsList;
  finally
    Files.Items.EndUpdate;
  end;
end;

procedure TfrmFiles.SetPath(const Value: String);
begin
  FPath := Value;
  Caption:=Value;
  ListFiles;
end;

end.
