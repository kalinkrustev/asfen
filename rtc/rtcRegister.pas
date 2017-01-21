{
  @html(<b>)
  Component Registration
  @html(</b>)
  - Copyright (c) Danijel Tkalcec
  @html(<br><br>)

  All RealThinClient VCL and Non-VCL components are being
  registered to Delphi component palette using this unit.
}
unit rtcRegister;

{$INCLUDE rtcDefs.inc}

interface

// This procedure is being called by Delphi to register the components.
procedure Register;

implementation

uses
{$IFNDEF BCB5}
  {$IFNDEF FPC}
    {$IFDEF IDE_6up}
      TypInfo, Consts,
      DesignIntf, DesignEditors,
    {$ELSE}
      TypInfo, Consts,
      DsgnIntf,
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

  rtcTcpCli,  rtcTcpSrv,
  rtcUdpCli,  rtcUdpSrv,

  rtcDataCli, rtcDataSrv,
  rtcHttpSrv, rtcHttpCli,
  rtcMsgSrv, rtcMsgCli,

  {$IFNDEF FPC}
  rtcISAPISrv,
  {$ENDIF}

{$IFNDEF BCB5}
  rtcEditors,
{$ENDIF}

  rtcTransports,

  rtcCliModule, rtcSrvModule,
  rtcFunction,

  rtcScript,

  Classes;

{$IFNDEF BCB5}
  {$IFNDEF FPC}
  type
    TRtcMessageReceiverInterfacedComponentProperty = class(TRtcInterfacedComponentProperty)
      public
        function GetIID: TGUID; override;
      end;

  function TRtcMessageReceiverInterfacedComponentProperty.GetIID: TGUID;
    begin
    Result := IRTCMessageReceiverGUID;
    end;
  {$ENDIF}
{$ENDIF}

procedure Register;
  begin
  RegisterComponents('RTC Server',[TRtcHttpServer,
                                   {$IFNDEF FPC}TRtcISAPIServer,{$ENDIF}
                                   TRtcMessageServer,
                                   TRtcDataServerLink, TRtcDualDataServerLink,
                                   TRtcDataProvider,
                                   TRtcServerModule,
                                   TRtcFunctionGroup, TRtcFunction,
                                   TRtcScriptEngine]);

  RegisterComponents('RTC Client',[TRtcHttpClient,
                                   TRtcMessageClient,
                                   TRtcDataClientLink, TRtcDualDataClientLink,
                                   TRtcDataRequest,
                                   TRtcClientModule,
                                   TRtcResult]);

  RegisterComponents('RTC LowLevel',[TRtcTcpClient, TRtcUdpClient,
                                     TRtcTcpServer, TRtcUdpServer]);
{$IFNDEF BCB5}
  {$IFNDEF FPC}
  RegisterPropertyEditor(TComponent.ClassInfo,
                         TRtcMessageClient, 'Server',
                         TRtcMessageReceiverInterfacedComponentProperty);
  {$ENDIF}
{$ENDIF}
  end;

end.
