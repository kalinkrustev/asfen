{ @html(<b>)
  ISAPI Project Template
  @html(</b>)
  - Copyright (c) Danijel Tkalcec
  @html(<br><br>)

  @exclude }
library rtcISAPIProject;

uses
  ActiveX,
  Forms,
  ComObj,
  rtcISAPIApp;

{$R *.res}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  Application.Initialize;
  Application.Run;
end.
