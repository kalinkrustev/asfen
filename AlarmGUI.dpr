program AlarmGUI;

uses
  HtmlHelpViewer,
  SysUtils,
  TranslateFixed in 'TranslateFixed.pas',
  Forms,
  dm_GUI in 'dm_GUI.pas' {dmGUI: TDataModule},
  frm_Rights in 'frm_Rights.pas' {frmRights},
  frm_Menu in 'frm_Menu.pas' {frmMenu},
  frm_Event in 'frm_Event.pas' {frmEvent},
  frm_Files in 'frm_Files.pas' {frmFiles},
  frm_Wave in 'frm_Wave.pas' {frmWave},
  dm_AlarmDB in 'dm_AlarmDB.pas' {dmAlarmDB: TDataModule},
  dm_FixedConnPool in 'dm_FixedConnPool.pas' {dmFixedConnPool: TDataModule},
  frm_Workgroup in 'frm_Workgroup.pas' {frmWorkgroup},
  frm_Script in 'frm_Script.pas' {frmScript},
  frm_Settings in 'frm_Settings.pas' {frmSettings},
  frm_Report in 'frm_Report.pas' {frmReport},
  VirtualTrees in 'vtree\VirtualTrees.pas',
  VirtualDBTreeEx in 'vtree\VirtualDBTreeEx.pas',
  dm_Images in 'dm_Images.pas' {dmImages: TDataModule},
  frm_RingGroupTree in 'frm_RingGroupTree.pas' {frmRingGroupTree},
  frm_Organisation in 'frm_Organisation.pas' {frmOrganisation},
  XMLClipboard in 'XMLClipboard.pas',
  frm_Export in 'frm_Export.pas' {frmExport},
  Util in 'Util.pas',
  Logger in 'Logger.pas',
  Log4D in 'log4d\Log4D.pas',
  frm_Users in 'frm_Users.pas' {frmUsers},
  frm_City in 'frm_City.pas' {frmCity},
  frm_GenericGrid in 'frm_GenericGrid.pas' {frmGenericGrid},
  frm_Password in 'frm_Password.pas' {frmPassword},
  PerlRegEx in 'pcre\PerlRegEx.pas',
  pcre in 'pcre\pcre.pas',
  frm_Archive in 'frm_Archive.pas' {frmArchive},
  GWGExporterXLSTXT in 'GWGExporterXLSTXT.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.HelpFile := ExtractFilePath(Application.ExeName)+'asos.chm';
  Application.CreateForm(TdmGUI, dmGUI);
  Application.CreateForm(TdmImages, dmImages);
  Application.CreateForm(TdmAlarmDB, dmAlarmDB);
  Application.CreateForm(TfrmMenu, frmMenu);
  Application.CreateForm(TfrmExport, frmExport);
  Application.Run;
end.
