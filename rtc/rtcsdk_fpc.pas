{  This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install
  the package rtcSDK_FPC 0.0.

  @exclude
}

unit rtcSDK_FPC; 

interface

uses
  rtcConn, rtcUdpSrv, rtcCliModule, rtcDataCli, rtcDataSrv, rtcFunction, 
    rtcHttpCli, rtcHttpSrv, rtcInfo, rtcRegister, rtcSrvModule, rtcTcpCli, 
    rtcTcpSrv, rtcUdpCli, rtcConnLimit, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('rtcRegister', @rtcRegister.Register); 
end; 

initialization
  RegisterPackage('rtcSDK_FPC', @Register); 
end.
