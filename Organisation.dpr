program Organisation;

uses
  Forms,
  SysUtils,
  frm_Organisation in 'frm_Organisation.pas' {frmOrganisation},
  VirtualTrees in 'vtree\VirtualTrees.pas',
  VirtualDBTreeEx in 'vtree\VirtualDBTreeEx.pas',
  dm_Images in 'dm_Images.pas' {dmImages: TDataModule},
  frm_City in 'frm_City.pas' {frmCity},
  dbClipImport in 'dbClipImport.pas',
  dm_Organisation in 'dm_Organisation.pas' {dmOrganisation: TDataModule},
  frm_Position in 'frm_Position.pas' {frmPosition},
  XMLClipboard in 'XMLClipboard.pas',
  JvSimpleXml in 'JvSimpleXml.pas',
  frm_GenericGrid in 'frm_GenericGrid.pas' {frmGenericGrid};

{$R *.res}

begin
  Application.Initialize;
  Application.ShowMainForm:=False;
  Application.CreateForm(TdmImages, dmImages);
  Application.CreateForm(TdmOrganisation, dmOrganisation);
  Application.CreateForm(TfrmOrganisation, frmOrganisation);
  Application.CreateForm(TfrmCity, frmCity);
  Application.CreateForm(TfrmPosition, frmPosition);
  Application.CreateForm(TfrmGenericGrid, frmGenericGrid);
  try
    frmCity.srcTree.DataSet:=dmOrganisation.Cities;
    frmCity.srcKind.DataSet:=dmOrganisation.Kind;
    frmCity.srcTSB.DataSet:=dmOrganisation.TSB;
    frmCity.srcAltitude.DataSet:=dmOrganisation.Altitude;
    frmCity.srcRegion.DataSet:=dmOrganisation.Region;
    frmOrganisation.btnCity1.Visible:=True;
    frmOrganisation.btnCity2.Visible:=True;
    frmOrganisation.btnPosition.Visible:=True;
    frmOrganisation.btnRaion1.Visible:=True;
    frmOrganisation.btnRaion2.Visible:=True;
    frmOrganisation.btnSetName.Visible:=True;
    frmOrganisation.acNewOrganisation.Visible:=True;
    frmOrganisation.acDelete.Visible:=True;
    frmOrganisation.acEdit.Visible:=True;
    frmOrganisation.acNewDivision.Visible:=True;
    frmOrganisation.acNewPosition.Visible:=True;
    frmOrganisation.srcCityList.DataSet:=dmOrganisation.CitiesClone;
    frmOrganisation.srcRaion.DataSet:=dmOrganisation.RaionClone;
    frmOrganisation.srcPosition.DataSet:=dmOrganisation.PositionsClone;
    frmOrganisation.srcTree.DataSet:=dmOrganisation.Organisations;
    frmPosition.srcPositionEdit.DataSet:=dmOrganisation.Positions;
    frmPosition.acPaste.Visible:=FindCmdLineSwitch('admin');
    frmCity.acPaste.Visible:=FindCmdLineSwitch('admin');
    frmOrganisation.acRecode.Visible:=FindCmdLineSwitch('admin');
  except
    on E:Exception do Application.HandleException(E);
  end;
  frmOrganisation.Show;
  Application.Run;
end.
