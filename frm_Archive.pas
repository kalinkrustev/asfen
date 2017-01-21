unit frm_Archive;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, XPStyleActnCtrls, ActnMan, ToolWin, ActnCtrls, ComCtrls;

type
  TfrmArchive = class(TForm)
    ActionToolBar1: TActionToolBar;
    ActionManager1: TActionManager;
    acClose: TAction;
    acBackup: TAction;
    acRestore: TAction;
    FileList: TListView;
    procedure acCloseExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure acBackupExecute(Sender: TObject);
    procedure FileListColumnClick(Sender: TObject; Column: TListColumn);
    procedure FileListCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure acRestoreExecute(Sender: TObject);
    procedure FileListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    Dir:String;
    Imported:Boolean;
    procedure ListFiles;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmArchive: TfrmArchive;

implementation

uses dm_GUI,dm_AlarmDB,dm_Images;

{$R *.dfm}

procedure TfrmArchive.acBackupExecute(Sender: TObject);
begin
  Screen.Cursor:=crHourglass;
  try
    dmGUI.Connection.Execute(Format('backup database [asos%d] TO DISK = N''%sasos%d set(%s) org(%s) usr(%s) %s.bak'' with init',[
       dmGUI.DatabaseID,
       Dir,
       dmGUI.DatabaseID,
       dmAlarmDB.GetSetting('DBSettings'),
       dmAlarmDB.GetSetting('DBOrganisation'),
       dmAlarmDB.GetSetting('DBUsers'),
       FormatDateTime('yyyy-mm-dd hh-nn-ss',Now)
    ]));
  finally
    Screen.Cursor:=crDefault;
    Application.ProcessMessages;
  end;
  ListFiles;
end;

procedure TfrmArchive.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmArchive.acRestoreExecute(Sender: TObject);
begin
  if FileList.Selected=nil then Exit;
  if not dmGUI.Confirm('Възстановяването ще презапише текущата база данни.'#13#10'Желаете ли да продължите?',mtWarning) then Exit;
  dmGUI.Connection.Close;
  Application.ProcessMessages;
  dmAlarmDB.RestoreDB(Dir+FileList.Selected.Caption);
  Imported:=True;
  ShowMessage('Възстановяването завърши успешно');
end;

procedure TfrmArchive.FileListColumnClick(Sender: TObject; Column: TListColumn);
begin
  if Column.ID+1=Abs(FileList.Tag) then
    FileList.Tag:=-FileList.Tag
  else
    FileList.Tag:=Column.ID+1;
  FileList.AlphaSort;
end;

procedure TfrmArchive.FileListCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
var S1,S2:String;
    C:Integer;
begin
  C:=Abs(FileList.Tag)-1;
  if (C>=0) and (C<=3) then
  begin
    if C=0 then
    begin
      S1:=Item1.Caption;
      S2:=Item2.Caption;
    end else
    begin
      C:=C-1;
      if C=1 then C:=C+1;
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
    if FileList.Tag<0 then Compare:=-Compare;
  end
  else
    Compare:=0;
end;

procedure TfrmArchive.FileListSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  acRestore.Enabled:=FileList.Selected<>nil;
end;

procedure TfrmArchive.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
  if Imported then ModalResult:=mrOk;
end;

procedure TfrmArchive.FormCreate(Sender: TObject);
begin
  Dir:=ExtractFilePath(ParamStr(0))+'Archive\';
end;

procedure TfrmArchive.FormDestroy(Sender: TObject);
begin
  frmArchive:=nil;
end;

procedure TfrmArchive.FormShow(Sender: TObject);
begin
  ListFiles;
  FileList.Tag:=-2;
  FileList.AlphaSort;
end;

procedure TfrmArchive.ListFiles;
var F:TSearchRec;
    Found:Boolean;
    L:TListItem;
begin
  FileList.Items.Clear;
  Found:=FindFirst(Dir+'*.bak',faAnyFile and not faDirectory,F)=0;
  try
    while Found do
    begin
      L:=FileList.Items.Add;
      L.Caption:=ExtractFileName(F.Name);
      L.SubItems.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss',FileDateToDateTime(F.Time)));
      if F.Size<8192 then
        L.SubItems.Add(IntToStr(F.Size)+'   B')
      else
      if F.Size<8192*1024 then
        L.SubItems.Add(IntToStr(Round(F.Size/1024))+' KB')
      else
        L.SubItems.Add(IntToStr(Round(F.Size/1024/1024))+' MB');
      L.SubItems.Add(IntToStr(F.Size));
      L.ImageIndex:=-1;
      Found := SysUtils.FindNext(F) = 0;
    end;
  finally
    SysUtils.FindClose(F);
  end;
end;


end.
