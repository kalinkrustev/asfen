//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("rtcSDK_C4.res");
USEPACKAGE("vcl40.bpi");
USEUNIT("rtcRegister.pas");
USEPACKAGE("vclx40.bpi");
USEUNIT("rtcService.pas");
USEUNIT("bcb6Hack.pas");
USEUNIT("rtcUdpSrv.pas");
USERES("rtcUdpSrv.dcr");
USEUNIT("rtcCliModule.pas");
USERES("rtcCliModule.dcr");
USEUNIT("rtcConn.pas");
USERES("rtcConn.dcr");
USEUNIT("rtcConnLimit.pas");
USEUNIT("rtcDataCli.pas");
USERES("rtcDataCli.dcr");
USEUNIT("rtcDataSrv.pas");
USERES("rtcDataSrv.dcr");
USEUNIT("rtcFunction.pas");
USERES("rtcFunction.dcr");
USEUNIT("rtcHttpCli.pas");
USERES("rtcHttpCli.dcr");
USEUNIT("rtcHttpSrv.pas");
USERES("rtcHttpSrv.dcr");
USEUNIT("rtcInfo.pas");
USEUNIT("rtcISAPIApp.pas");
USEUNIT("rtcISAPISrv.pas");
USERES("rtcISAPISrv.dcr");
USEUNIT("rtcLog.pas");
USEUNIT("rtcParse.pas");
USEUNIT("rtcSrvModule.pas");
USERES("rtcSrvModule.dcr");
USEUNIT("rtcTcpCli.pas");
USERES("rtcTcpCli.dcr");
USEUNIT("rtcTcpSrv.pas");
USERES("rtcTcpSrv.dcr");
USEUNIT("rtcThrPool.pas");
USEUNIT("rtcTimer.pas");
USEUNIT("rtcUdpCli.pas");
USERES("rtcUdpCli.dcr");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
	return 1;
}
//---------------------------------------------------------------------------
