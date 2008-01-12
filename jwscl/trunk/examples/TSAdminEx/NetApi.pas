unit NetApi;

interface

uses Classes,
  JwaWindows,
  JwsclVersion;


type
  PDS_DOMAIN_TRUSTSARRAYW = ^DS_DOMAIN_TRUSTSARRAYW;
  DS_DOMAIN_TRUSTSARRAYW = Array[0..ANYSIZE_ARRAY-1] of DS_DOMAIN_TRUSTSW;

function EnumerateDomains: TStringList;
function PWArrayToStringList(const PWArray: PWideChar): TStringList;
function EnumerateMultiUserServers(Domain: PWideChar) : PWideChar; stdcall; external 'utildll.dll';

implementation

// This function converts a pwidechar array with elements seperated by #0 to
// a StringList
function PWArrayToStringList(const PWArray: PWideChar): TStringList;
var Current: PWideChar;
begin
  // Create TStringList
  Result := TStringList.Create;
  // Sort and ignore duplicates
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;

  if PWArray <> nil then
  begin
    Current := PWArray;
    while Current[0] <> #0 do
    begin
      Result.Add(Current);
      Current := Current + Length(Current) + 1;
    end;
  end;

  // Sort the StringList
  Result.Sort;
end;

//function MyDsEnumerateDomainTrustsW(ServerName: LPWSTR; Flags: ULONG;
//  Domains: PMyArray; var DomainCount: ULONG): DWORD; stdcall; external 'netapi32.dll' name 'DsEnumerateDomainTrustsW';

function EnumerateDomains: TStringList;
var Res: DWORD;
  DomainControllerInfo: PDOMAIN_CONTROLLER_INFOW;
  DomainNames: PWideChar;
  nts: NTSTATUS;
  PDomains: PDS_DOMAIN_TRUSTSARRAYW;
  DomainCount: DWORD;
  i: Integer;
  bufptr: Pointer;
begin
  // Os Windows 2000 or higher?
  if TJwWindowsVersion.IsWindows2000(True) then
  begin
    // Find a Domain Controller
    Res := DsGetDcNameW(nil, nil, nil, nil, DS_BACKGROUND_ONLY or
      DS_RETURN_FLAT_NAME, DomainControllerInfo);

    // Did we succeed?
    if Res = ERROR_SUCCESS then
    begin
      // Create TStringList
      Result := TStringList.Create;
      // Sort and ignore duplicates
      Result.Sorted := True;
      Result.Duplicates := dupIgnore;

      // Add the domain we are member of to the StringList
      // Note that DsEnumerateDomainTrust seems to return this anyway (where
      // NetEnumerateDomains does not. I do include however because I'm not sure
      // if this is always true (dupIgnore will prevent returning duplicates)
      Result.Add(DomainControllerInfo^.DomainName);

      // and now enumerate the trusted domains
      PDomains := nil;

      Res := DsEnumerateDomainTrustsW(DomainControllerInfo^.DomainControllerName,
        DS_DOMAIN_DIRECT_INBOUND or DS_DOMAIN_DIRECT_OUTBOUND or
        DS_DOMAIN_IN_FOREST, PDS_DOMAIN_TRUSTSW(PDomains), DomainCount);

      // Did we succeed?
      if Res = ERROR_SUCCESS then
      begin
        // Add the trusted domains to the StringList
        for i := 0 to DomainCount - 1 do
        begin
          Result.Add(PDomains^[i].NetbiosDomainName);
        end;

        // Free Memory
        NetApiBufferFree(PDomains);
      end;

      // Free Memory
      NetApiBufferFree(DomainControllerInfo);
    end
    else begin
      // Always create the stringlist so the caller doesn't need to check for nil!
      Result := TStringList.Create;
    end;
  end
  else begin
    // NT Version
    Res := NetServerGetInfo(nil, 503, PByte(bufptr));
    if Res = ERROR_SUCCESS then
    begin
      nts := NetEnumerateTrustedDomains(PSERVER_INFO_503(bufptr)^.sv503_domain,
        DomainNames);
      if nts = NERR_Success then
      begin
        // Function Creates a StringList and adds domain names
        Result := PWArrayToStringList(DomainNames);
      end
      else begin
        // Create TStringList
        Result := TStringList.Create;
        // Sort and ignore duplicates
        Result.Sorted := True;
        Result.Duplicates := dupIgnore;

        // Convert NTSTATUS to Error
        Res := RtlNtStatusToDosError(nts);
      end;

      // NetEnumerateTrustedDomains does not return the "current" domain
      // so we need to add the local domain:
      Result.Add(PSERVER_INFO_503(bufptr)^.sv503_domain);

      // Free Memory
      NetApiBufferFree(bufptr);

    end
    else begin
      // Create the StringList
      Result := TStringList.Create;
    end;
  end;

  // SetLastError
  SetLastError(Res);
end;

end.
