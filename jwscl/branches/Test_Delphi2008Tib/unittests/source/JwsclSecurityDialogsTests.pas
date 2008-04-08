unit JwsclSecurityDialogsTests;

interface

uses
  JwsclSecurityDialogs,
  TestFrameWork,
  ActiveX,
  CommCtrl,
  Dialogs,
  
     jwaWindows,
     JwsclTypes, JwsclExceptions, JwsclSid, JwsclAcl, JwsclToken,
     JwsclMapping, JwsclKnownSid, JwsclSecureObjects,
     JwsclVersion, JwsclConstants, JwsclProcess, JwsclDescriptor,
     JwsclStrings; //JwsclStrings, must be at the end of uses list!!!

type
  TSecurityInformationImpTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

    function OnLookupSIDs(Sender : TJwSecurityDescriptorDialog; const SIDList : TJwSecurityIdList; var SIDInfoList : TJwSidInfoRecordArray) : Cardinal;
    function OnGetInheriteSource1(Sender : TJwSecurityDescriptorDialog; const Info : TJwSecurityInformationFlagSet; const ACL : TJwSecurityAccessControlList; var InheritanceArray : TJwInheritedFromArray) : Cardinal;
  published

    // Test methods
    procedure Test_SecDialog1;

  end;

implementation

uses Math, ComObj, SysUtils;

{ TSecurityInformationImpTests }

function TSecurityInformationImpTests.OnGetInheriteSource1(
  Sender: TJwSecurityDescriptorDialog; const Info: TJwSecurityInformationFlagSet;
  const ACL: TJwSecurityAccessControlList;
  var InheritanceArray: TJwInheritedFromArray) : Cardinal;
var i : Integer;
begin
//
  randomize;
  for i := low(InheritanceArray) to high(InheritanceArray) do
  begin

    InheritanceArray[i].AncestorName := ''; //IntToStr(random(100000))+':'+IntToStr(i);//WideString('123');
    //InheritanceArray[i].AncestorName := '123';
    InheritanceArray[i].GenerationGap := 0;
  end;

  result := S_OK;
end;



function TSecurityInformationImpTests.OnLookupSIDs(Sender: TJwSecurityDescriptorDialog;
  const SIDList: TJwSecurityIdList;
  var SIDInfoList: TJwSidInfoRecordArray): Cardinal;
var i : Integer;
begin
  //
  for i := 0 to Length(SIDInfoList)-1 do
    if SIDInfoList[i].Exception <> nil then
      SIDInfoList[i].sCommonName := 'IchBins';
end;

procedure TSecurityInformationImpTests.Test_SecDialog1;
var secDialogImp : TJwSecurityDescriptorDialog;
    SD : TJwSecurityDescriptor;
    s : TJwString;
    tag : tagINITCOMMONCONTROLSEX;
    Flags : TJwSecurityInformationFlagSet;
    p : Pointer;
begin
  secDialogImp := TJwSecurityDescriptorDialog.Create(GetActiveWindow);

  secDialogImp.Flags :=  [sdfEditDacl, sdfAdvanced, sdfEditOwner,
          sdfEditProperties,sdfEditEffective {sdfReadOnly,sdfOwnerReadOnly,   }
          ,sdfContainer
          ,sdfNoAdditionalPermission
          ,sdfOwnerRecurse
          ,sdfMayWrite

                        ];
  secDialogImp.Mapping := TJwSecurityGenericMapping;
  secDialogImp.PageTitle := 'Test';
  secDialogImp.ObjectName := ParamStr(0);
  secDialogImp.ServerName := '';

  secDialogImp.OnLookupSIDs := OnLookupSIDs;


 // secDialogImp.OnGetInheriteSource := OnGetInheriteSource1; 

//  secDialogImp.Fl

  Flags := [siOwnerSecurityInformation,
           siDaclSecurityInformation];
  try
    if JwIsPrivilegeSet(SE_SECURITY_NAME,pqt_Available) then
    begin
      JwEnablePrivilege(SE_SECURITY_NAME,pst_Enable);
      Include(Flags, siSaclSecurityInformation);
      secDialogImp.Flags := secDialogImp.Flags + [sdfEditSacl];
    end;

    SD :=
        TJwSecureGeneralObject.GetNamedSecurityInfo(ParamStr(0),SE_FILE_OBJECT,Flags);
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[afInheritedAce],GENERIC_ALL,TJwSecurityId.Create('S-1-1-12344')));

  {s := SD.DACL.Items[0].GetText;

  s := SD.Text;
  if S = '' then;}


    secDialogImp.SecurityDescriptor := SD;
    SD.Free;


    secDialogImp.ShowModal;

  finally
    secDialogImp.Free;

    if JwIsPrivilegeSet(SE_SECURITY_NAME,pqt_Enabled) then
      JwEnablePrivilege(SE_SECURITY_NAME,pst_Disable);
  end;


  //EditSecurity(GetActiveWindow,secDialogImp)
end;

initialization

  TestFramework.RegisterTest('JwsclSecurityDialogsTests Suite',
    TSecurityInformationImpTests.Suite);

end.
 