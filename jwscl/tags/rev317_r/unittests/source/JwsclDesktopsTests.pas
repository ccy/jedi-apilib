unit JwsclDesktopsTests;

interface

uses
  JwsclDesktops,
  TestFrameWork,
  ActiveX,
  CommCtrl,
  Dialogs,

     jwaWindows,
     JwsclTypes, JwsclExceptions, JwsclSid, JwsclAcl, JwsclToken,
     JwsclMapping, JwsclKnownSid, JwsclSecureObjects, JwsclSecurityDialogs,
     JwsclVersion, JwsclConstants, JwsclProcess, JwsclDescriptor,
     JwsclStrings; //JwsclStrings, must be at the end of uses list!!!

type
  TSecurityDesktopTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

  published

    // Test methods
    procedure TestCreate;
    procedure TestCreateUnInitialized;
    procedure TestDestroy;
    procedure TestOpenDesktopByName;
    procedure TestDesktopFlagsToInt;
    procedure TestGeTSecurityDesktopWindowHandles;
    procedure TestOpenInputDesktop;
    procedure TestClose;
    procedure TestSetThreadDesktop;
    procedure TestSwitchDesktop;
    procedure TestSwitchDesktopBack;

    procedure TestDesktopSecurity;

  end;

type
  TSecurityDesktopsTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

  published

    // Test methods
    procedure TestCreate;
    procedure TestDestroy;
    procedure TestClear;
    procedure TestFindDesktopByName;
    procedure TestFindDesktopByHandle;
    procedure TestFindInpuTSecurityDesktop;
    procedure TestUpdate;
    procedure TestCreateDesktop;
    procedure TestOpenDesktop;
    procedure TestOpenInpuTSecurityDesktop;
    procedure TestGetThreadDesktop;
    procedure TestGetDesktopName;
    procedure TestGetDesktops;
    procedure TestGeTSecurityDesktopHeapSize;

  end;

implementation

{ TSecurityDesktopTests }

procedure TSecurityDesktopTests.TestClose;
begin

end;

procedure TSecurityDesktopTests.TestCreate;
begin

end;

procedure TSecurityDesktopTests.TestCreateUnInitialized;
begin

end;

procedure TSecurityDesktopTests.TestDesktopFlagsToInt;
begin

end;

procedure TSecurityDesktopTests.TestDesktopSecurity;


var
    Desk : TJwSecurityDesktop;
    Desks : TJwSecurityDesktops;
    SD : TJwSecurityDescriptor;

procedure ShowACLEditor;
var secDialogImp : TJwSecurityDescriptorDialog;
begin
  secDialogImp := TJwSecurityDescriptorDialog.Create(GetActiveWindow);

  secDialogImp.Flags :=  [sdfEditDacl, sdfAdvanced, sdfEditOwner,
          sdfEditProperties,sdfEditEffective {, sdfReadOnly,sdfOwnerReadOnly    }
          ,sdfContainer
                        ];
  //secDialogImp.Mapping := TJwSecurityGenericMapping;

  //secDialogImp.Mapping := TJwSecurityDesktopMapping;
  secDialogImp.Mapping := TJwSecurityWinStationMapping;
  secDialogImp.PageTitle := 'Winsta0';
  secDialogImp.ObjectName := 'Winsta0';
  secDialogImp.ServerName := '';


  secDialogImp.SecurityDescriptor := SD;

  secDialogImp.ShowModal;
  secDialogImp.Free;

end;

var sSD : TJwSecurityDescriptor;
    str : TJwString;
begin
 // JwInitWellKnownSIDs;

  Desks := TJwSecurityDesktops.Create(nil);

  Desk := Desks.OpenInputDesktop([],READ_CONTROL,false);
  {SD := Desk.SecurityDescriptor[[siGroupSecurityInformation,
                              siDaclSecurityInformation]];
   }

   SD := TJwSecureGeneralObject.GetSecurityInfo(GetProcessWindowStation,
      SE_WINDOW_OBJECT,[siDaclSecurityInformation]);
  // str := SD.DACL.GetTextMap(TJwSecurityWinStationMapping);
 //  ShowMEssage(str);
 //  if str = '' then;

 { SD := TJwSecurityDescriptor.Create;
  SD.DACL := TJwDAccessControlList.Create;
  SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create
      (nil, [afInheritedAce],

        DESKTOP_READOBJECTS or DESKTOP_CREATEWINDOW or
        DESKTOP_CREATEWINDOW or DESKTOP_CREATEMENU or
        DESKTOP_HOOKCONTROL or DESKTOP_JOURNALRECORD or
        DESKTOP_JOURNALPLAYBACK or DESKTOP_ENUMERATE or
        DESKTOP_WRITEOBJECTS or DESKTOP_SWITCHDESKTOP or
        DESKTOP_SETTING or
        WRITE_DAC or WRITE_OWNER or
        DELETE or READ_CONTROL
        ,


      JwWorldSID));       }


  
  ShowACLEditor;

  //do not free SD because its freed by Desk
 // SD.Free;


  Desks.Free;
end;

procedure TSecurityDesktopTests.TestDestroy;
begin

end;

procedure TSecurityDesktopTests.TestGeTSecurityDesktopWindowHandles;
begin

end;

procedure TSecurityDesktopTests.TestOpenDesktopByName;
begin

end;

procedure TSecurityDesktopTests.TestOpenInputDesktop;
begin

end;

procedure TSecurityDesktopTests.TestSetThreadDesktop;
begin

end;

procedure TSecurityDesktopTests.TestSwitchDesktop;
begin

end;

procedure TSecurityDesktopTests.TestSwitchDesktopBack;
begin

end;

{ TSecurityDesktopsTests }

procedure TSecurityDesktopsTests.TestClear;
begin

end;

procedure TSecurityDesktopsTests.TestCreate;
begin

end;

procedure TSecurityDesktopsTests.TestCreateDesktop;
begin

end;

procedure TSecurityDesktopsTests.TestDestroy;
begin

end;

procedure TSecurityDesktopsTests.TestFindDesktopByHandle;
begin

end;

procedure TSecurityDesktopsTests.TestFindDesktopByName;
begin

end;

procedure TSecurityDesktopsTests.TestFindInpuTSecurityDesktop;
begin

end;

procedure TSecurityDesktopsTests.TestGetDesktopName;
begin

end;

procedure TSecurityDesktopsTests.TestGetDesktops;
begin

end;

procedure TSecurityDesktopsTests.TestGeTSecurityDesktopHeapSize;
begin

end;

procedure TSecurityDesktopsTests.TestGetThreadDesktop;
begin

end;

procedure TSecurityDesktopsTests.TestOpenDesktop;
begin

end;

procedure TSecurityDesktopsTests.TestOpenInpuTSecurityDesktop;
begin

end;

procedure TSecurityDesktopsTests.TestUpdate;
begin

end;

initialization

  TestFramework.RegisterTest('JwsclDesktopsTests Suite',
    TSecurityDesktopTests.Suite);
  TestFramework.RegisterTest('JwsclDesktopsTests Suite',
    TSecurityDesktopsTests.Suite);

end.
