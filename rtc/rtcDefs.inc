{ If you need all your new clients and servers to stay compatible with all
  older RTC clients and servers, even if this means disabling some new
  features from the latest RTC SDK, you simply need to uncomment the
  below compiler directive: }

{.$DEFINE OldRtcCompatibilityMode}


{ RTC SDK versions up to 2.04 are expecting all Field names received with
  remote function calls to be in UpperCase. This is not the case with newer
  RTC SDK versions, which allow working with field names using mixed case,
  without internaly converting all field names to uppercase.

  If you are still using Clients or Servers compiled with older RTC SDK
  versions, you have to undeclare (comment out) the below declaration,
  to make sure all Field Names will be converted to UpperCase when sent
  out, so that older clients and servers won't have problems with the
  client/server compiled with THIS RTC SDK version.

  To send all Field Names in UpperCase and keep your new application
  compatible with apps compiled with all older RTC SDK versions,
  declare the compiler directive below (remove the dot): }

{.$DEFINE RtcUpperCaseFieldNames}

{$IFDEF OldRtcCompatibilityMode}
  {$DEFINE RtcUpperCaseFieldNames}
{$ENDIF}


{ RTC Clients compiled with RTC SDK versions *before* 2.27 do NOT send a
  control sum when using RTC functions with encryption, which makes it
  hard for the Server to check if the control key received from the client
  is a valid control key. Starting with RTC SDK 2.27, RTC Clients will be
  sending a control sum value along with each control key, which RTC Servers
  can use to check if the control key is valid and make sure at least the
  end of the request data was decrypted properly.

  But, since this control sum value is not being sent by RTC Clients compiled
  with RTC SDK versions older than 2.27, those clients would NOT work with
  a RTC Server compiled with the latest RTC SDK version if the Server would
  expect this control sum value to be sent by the Client.

  If you have RTC Clients compiled with RTC SDK 2.26 or older,
  you need to *uncomment* the below compiler directive, so that your RTC
  Server (when compiled with the laterst RTC SDK version) does NOT check
  for the control sum value, so it can work with older clients.
  To keep your Server backward-compatible, remove the dot: }

{.$DEFINE RtcDoNotCheckCryptControlSums}

{$IFDEF OldRtcCompatibilityMode}
  {$DEFINE RtcDoNotCheckCryptControlSums}
{$ENDIF}


{ When "UseGUIDs" is defined, CoCreateGUID() API function is used
  to create Session IDs. Otherwise, a short string counter is used.
  For security reasons, use the GDI option (keep the directive defined): }

{$DEFINE UseGUIDs}

{ By default, PHP5 is used. If you are using PHP 4,
  simply comment out (undeclare) this compiler directive: }

{$DEFINE PHP5}


{***********************************************************
 !!! Here come the options which you should NEVER change !!!
************************************************************}

{ VER80  => Delphi 1 }
{ VER90  => Delphi 2 }
{ VER93  => Bcb 1    }
{ VER100 => Delphi 3 }
{ VER110 => Bcb 3    }

{ VER120 => Delphi 4 }
{ VER125 => Bcb 4    }
{ VER130 => Delphi 5 }
{ VER140 => Delphi 6 }
{ VER150 => Delphi 7 }
{ VER160 => Delphi 8 }
{ VER170 => Delphi 9 = BDS 2005}
{ VER180 => Delphi 10 = BDS 2006}

{$V+}{$B-}{$X+}{$T-}{$P+}{$H+}{$J-}

{$DEFINE IDE_6up}

{$IFDEF VER120}
  {$UNDEF IDE_6up}
{$ENDIF}
{$IFDEF VER125}
  {$UNDEF IDE_6up}
{$ENDIF}
{$IFDEF VER130}
  {$UNDEF IDE_6up}
{$ENDIF}
{$IFDEF VER135}
  {$UNDEF IDE_6up}
{$ENDIF}

{$IFDEF VER180}
  {$DEFINE IDE_2006up}
{$ENDIF}


{$IFDEF VER120}
  {$DEFINE IDE_2}
  {$DEFINE IDE_1}
  {$DEFINE IDE_0}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE IDE_2}
  {$DEFINE IDE_1}
  {$DEFINE IDE_0}
{$ENDIF}
{$IFDEF VER130}
  {$DEFINE IDE_2}
  {$DEFINE IDE_1}
{$ENDIF}
{$IFDEF VER135}
  {$DEFINE IDE_2}
{$ENDIF}
{$IFDEF VER140}
  {$DEFINE IDE_2}
{$ENDIF}

{$IFNDEF FPC}
  {$DEFINE COMPRESS}
{$ENDIF}

{$IFNDEF IDE_1}
  {$IFNDEF FPC}
    {$WARN SYMBOL_PLATFORM OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
  {$ENDIF}
{$ENDIF}

{$IFNDEF IDE_2}
  {$IFNDEF FPC}
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_CAST OFF}
  {$ENDIF}
{$ENDIF}


