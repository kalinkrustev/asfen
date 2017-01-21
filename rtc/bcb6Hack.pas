{ @exclude 
  BCB 6 Hack to enable editing of Delphi Forms. }
unit bcb6Hack;

{$INCLUDE rtcDefs.inc}

interface

procedure Register;

implementation

{$ifdef VER140}
uses
  Windows, SysUtils;

const
  PackageName = 'coreide60.bpl';
  AllowFormEditsName = '@Pasmgr@AllowFormEdits';

var
  PackageHandle: HModule = 0;
  AllowFormEdits: PBoolean = nil;

procedure EnableFormEditing;

  procedure ConnectToPackageExport(var ProcVar; const ProcName: PChar);
  begin
    @TProcedure(ProcVar) := GetProcAddress(PackageHandle, ProcName);
    if not Assigned(TProcedure(ProcVar)) then
      raise Exception.CreateFmt('Cannot locate exported symbol %s', [ProcName]);
  end;

begin
  if PackageHandle = 0 then
  begin
    PackageHandle := GetModuleHandle(PackageName);
    if PackageHandle = 0 then
      raise Exception.CreateFmt('Unable to locate %s package in memory', [PackageName]);
    ConnectToPackageExport(AllowFormEdits, AllowFormEditsName);
  end;
  AllowFormEdits^ := True;
end;

procedure Register;
begin
  EnableFormEditing
end;

{$else}

procedure Register;
  begin
  // This Hack is only for BCB 6.0 to enable editing of Delphi forms
  end;

{$endif}

end.
