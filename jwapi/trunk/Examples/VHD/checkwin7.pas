unit checkwin7;

interface

uses Windows;

implementation

function Win7OrHigher: Boolean;
var
  OSVI: TOSVersionInfo;
begin
  Result:= False;
  ZeroMemory(@OSVI, SizeOf(TOSVersionInfo));
  OSVI.dwOSVersionInfoSize:= SizeOf(TOSVersionInfo);
  if GetVersionEx(OSVI) then
  begin
    if OSVI.dwMajorVersion = 6 then
       Result:= (OSVI.dwMinorVersion > 0)
    else
       Result:= (OSVI.dwMajorVersion > 6);
  end;
end;

initialization
  if not Win7OrHigher then
  begin
    MessageBox(0, 'Windows 7 or higher is required for VHD API.'#13#10'Program is terminated.', 'Windows Version Conflict!', MB_ICONERROR);
    Halt;
  end;

end.
