unit UserProfileImage;

interface
uses
  Classes,
  SysUtils,
  ActiveX,
  JwaWindows,
  comobj,
  graphics,
  JwsclToken,
  JwsclExceptions,
  JwsclUtils,
  JwsclVersion,
  JwsclComUtils;



function CreateUserProfileImage(const UserToken : TJwSecurityToken;
  out ImageType : WideString) : TMemoryStream;

implementation

function CreateUserProfileImage(const UserToken : TJwSecurityToken;
  out ImageType : WideString) : TMemoryStream;

  procedure FreeSRowSet(WabObject: IWabObject; var P: PSRowSet);
  var
    I: Integer;
  begin
    for I := 0 to P^.cRows - 1 do
      OleCheck(WabObject.FreeBuffer(P^.aRow[I].lpProps));
    OleCheck(WabObject.FreeBuffer(P));
    P := nil;
  end;

  function GetUserName(): string;
  var
    Buffer: array [0..MAX_COMPUTERNAME_LENGTH + 1] of Char;
    Size: DWord;
  begin
    Size := Pred(SizeOf(Buffer));
    JwaWindows.GetUserName(Buffer, Size);
    Result := StrPas(Buffer);
  end;

  function ShGetPath(ID : Integer) : WideString;
  var Path : Array[0..MAX_PATH] of Widechar;
  begin
    Result := '';
    if SUCCEEDED(SHGetFolderPathW(0,ID,0,SHGFP_TYPE_DEFAULT, @Path)) then
      result := IncludeTrailingBackslash(Path);  //Oopps, may convert unicode to ansicode
  end;


  function GetUserPicture(): TMemoryStream;
  var
    CommonDataPath, UserName: string;
    Bit : TBitmap;
  begin
    UserName := GetUserName;

    CommonDataPath := ShGetPath(CSIDL_COMMON_APPDATA) +
       '\Microsoft\User Account Pictures\' + UserName + '.bmp';

    if FileExists(CommonDataPath) then
    begin
      Bit := TBitmap.Create;
      try
        Bit.Handle := LoadImage(0, PChar(CommonDataPath), IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE);

        result := TMemoryStream.Create;
        Bit.SaveToStream(result);
      finally
        Bit.Free;
      end;
    end
    else
      Result := nil;
  end;


var
  WP: TWabParam;

  EntryID: PEntryID;
  EntryIDSize : ULONG;
  ObjType: ULONG;

  Container: IABContainer;
  AddrBook: IAddrBook;
  WabObject: IWabObject;

  Table: IMAPITable;
  TableRow: PSRowSet;

  MailUser: IMailUser;

  DetailsCount : DWORD;
  Details: PSPropsArray;

  PropType,
  PropID : DWORD;

  i : Integer;

  MessageSent : Boolean;
  Token : TJwSecurityToken;

  H : HRESULT;

  imp : TWabImportParam;
begin
  if not TJwWindowsVersion.IsWindowsVista(true) and
     not TJwWindowsVersion.IsWindows2008(true) then
  begin
    result := GetUserPicture;
    exit;
  end;

  try
    CoInitialize(nil);

    if (not WabApiLoaded) then
    begin
      if not LoadWabApi then
        raise EJwsclUnimplemented.Create('WAPI not implemented');
    end;

    if Assigned(UserToken) then
      UserToken.ImpersonateLoggedOnUser;
    try
      MessageSent := false;

      ImageType := '';

      ZeroMemory(@WP, Sizeof(WP));
      WP.cbSize := Sizeof(WP);

      WP.ulFlags := 0;
      WP.hwnd := 0;

      OleCheck(WabOpen(AddrBook, WabObject, @WP, 0));

      MailUser := nil;


    //      'TestBenutzer', MailUser));

     { OleCheck(WabObject.LDAPUrl(AddrBook, GetActiveWindow,
            0,//WABOBJECT_LDAPURL_RETURN_MAILUSER,
            PCHAR('ldap://CN=Chris'),
            MailUser));    }

      OleCheck(AddrBook.GetPAB(EntryIDSize, EntryID));
     {
      ZeroMemory(@imp, sizeof(imp));
      imp.cbSize := sizeof(imp);
      imp.lpAdrBook := AddrBook;
      imp.lpszFileName := PCHAR('C:\Users\Christian\Contacts\TestBenutzer.contact');
      H := WabObject.Import(@imp);
      }

      {H := (WabObject.VCardRetrieve(AddrBook, WAB_VCARD_FILE,
          PCHAR('C:\Users\Christian\Contacts\TestBenutzer.contact'),
          //PCHAR('C:\Users\UserAsAdmin\Contacts\TestBenutzer.contact'),
          //PCHAR('Contacts'),
          MailUser));}
      //OleCheck(H);

      OleCheck(AddrBook.OpenEntry(EntryIDSize, EntryID, nil, 0,
        ObjType, IUnknown(Container)));

      OleCheck(WabObject.FreeBuffer(EntryID));

      OleCheck(Container.GetContentsTable(0, Table));
      OleCheck(Table.SeekRow(BOOKMARK_BEGINNING, 0, nil));

      repeat
        OleCheck(Table.QueryRows(1,0,TableRow));

        if TableRow.cRows > 0 then
        begin
          //if (ULONG(TableRow.aRow[0].lpProps[4].Value.l) in [MAPI_MAILUSER, MAPI_DISTLIST]) then
          begin
            EntryID := TableRow.aRow[0].lpProps[3].Value.bin.lpb;
            EntryIDSize := TableRow.aRow[0].lpProps[3].Value.bin.cb;

            ObjType := 0;
            OleCheck(AddrBook.OpenEntry(EntryIDSize, EntryID, nil, 0,
                ObjType, IUnknown(MailUser)));
            OleCheck(MailUser.GetProps(nil, 0, @DetailsCount, PSPropValue(Details)));

            for i := 0 to DetailsCount -1 do
            begin
              PropType := PROP_TYPE(Details[i].ulPropTag);
              PropID := PROP_ID(Details[i].ulPropTag);

              if (PropId = PROP_ID(PR_ICON)) and
                 (Details[0].Value.bin.cb > 0) then
              begin
                Result := TMemoryStream.Create;

                Result.Write(Details[i].Value.bin.lpb^, Details[i].Value.bin.cb);
                Result.Seek(0,soFromBeginning);
              end
              else
              if PropId = $660d then
              begin
                if Length(ImageType) = 0 then
                  ImageType := Details[i].Value.lpszA;
              end;
            end;

            FreeSRowSet(WabObject,TableRow);


            break;
          end;
          FreeSRowSet(WabObject,TableRow);
        end
        else
          break;

      until false;
    except
      result := GetUserPicture;
    end;
  finally
    TJwSecurityToken.RevertToSelf;
  end;
{  if Assigned(Result) then
    result.SaveToFile('E:\Temp\_test.jpg');}
end;

end.
