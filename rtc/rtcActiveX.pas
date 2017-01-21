{ @exclude }
unit rtcActiveX;

interface

uses
  Classes, ActiveX, rtcThrPool;

type
  TRtcActiveX=class(TRtcThreadCallback)
    procedure AfterThreadStart; override;
    { Called from inside each Thread, before it will be stopped/destroyed }
    procedure BeforeThreadStop; override;
    { Callled after all threads have been stopped.
      This is the method from which you should destroy the object by calling "Free" }
    procedure DestroyCallback; override;
    end;

implementation

{ TRtcActiveX }

procedure TRtcActiveX.AfterThreadStart;
  begin
  CoInitialize(nil);
  end;

procedure TRtcActiveX.BeforeThreadStop;
  begin
  CoUninitialize;
  end;

procedure TRtcActiveX.DestroyCallback;
  begin
  Free;
  end;

initialization
AddThreadCallback( TRtcActiveX.Create );
finalization
end.
