{
  @html(<b>)
  Encryption/Decryption Plug-in
  @html(</b>)
  - Copyright (c) Danijel Tkalcec
  @html(<br><br>)

  This unit defines a Plug-in class, which should be extended when implementing 
  third-party plug-ins for SSL/SSH encryption/decryption using RTC SDK components.
}
unit rtcPlugins;

interface

{$include rtcDefs.inc}

uses
  Classes;

type
  TRtcConnID = longint;

  { @abstract(Cryptography Plugin)
    This is a basic class for any kind of third-party encryption/decryption 
    plug-ins used by rtcHttpClient and rtcHttpServer components. }
  TRtcCryptPlugin = class(TComponent)
  public
    { Called after a new connection was established.
      OutData = data which has to be sent out immediately. }
    procedure AfterConnect(ID:TRtcConnID; var OutData:string); virtual; abstract;

    { Called before we do a graceful disconnect, in case some data has to be sent out. }
    procedure BeforeDisconnect(ID:TRtcConnID; var OutData:string); virtual; abstract;

    { Called after a connection was closed. }
    procedure AfterDisconnect(ID:TRtcConnID); virtual; abstract;

    { Called when data arrived.
      InData = data received from recipient (decode that!)
      OutData = data prepared by "decoder" for sending back to recipient (encoded data)
      OutPlainText = decrypted input data (for use by application) }
    procedure DataReceived(ID:TRtcConnID; const InData:string; var OutData:string; var OutPlainText:string); virtual; abstract;

    { Called when data needs to be sent.
      InData = application data which we want to be encoded for sending
      OutData = encoded data which should be sent out }
    procedure DataToSend(ID:TRtcConnID; const InData:string; var OutData:string); virtual; abstract;
    end;

implementation

end.
