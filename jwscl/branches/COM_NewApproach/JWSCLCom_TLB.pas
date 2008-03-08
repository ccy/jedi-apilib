unit JWSCLCom_TLB;

// ************************************************************************ //
// WARNUNG                                                                    
// -------                                                                    
// Die in dieser Datei deklarierten Typen wurden aus Daten einer Typbibliothek
// generiert. Wenn diese Typbibliothek explizit oder indirekt (über eine     
// andere Typbibliothek) reimportiert wird oder wenn die Anweisung            
// 'Aktualisieren' im Typbibliotheks-Editor während des Bearbeitens der     
// Typbibliothek aktiviert ist, wird der Inhalt dieser Datei neu generiert und 
// alle manuell vorgenommenen Änderungen gehen verloren.                           
// ************************************************************************ //

// PASTLWTR : 1.2
// Datei generiert am 08.03.2008 23:08:40 aus der unten beschriebenen Typbibliothek.

// ************************************************************************  //
// Typbib: P:\Eigene Dateien\Dezipaitor\Projekte\Delphi\7\jedi-api-lib\jwscl\trunk\COM\JWSCLCom.tlb (1)
// LIBID: {9EBCE2EF-4E69-4AC3-AA7F-F021E119E8BB}
// LCID: 0
// Hilfedatei: 
// Hilfe-String: JWSCLCom Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\system32\stdole2.tlb)
//   (2) v4.0 StdVCL, (C:\Windows\system32\stdvcl40.dll)
//   (3) v4.0 StdVCL, (C:\Windows\system32\stdvcl40.dll)
//   (4) v4.0 StdVCL, (C:\Windows\system32\stdvcl40.dll)
//   (5) v4.0 StdVCL, (C:\Windows\system32\stdvcl40.dll)
//   (6) v4.0 StdVCL, (C:\Windows\system32\stdvcl40.dll)
//   (7) v4.0 StdVCL, (C:\Windows\system32\stdvcl40.dll)
//   (8) v4.0 StdVCL, (C:\Windows\system32\stdvcl40.dll)
//   (9) v4.0 StdVCL, (C:\Windows\system32\stdvcl40.dll)
//   (10) v4.0 StdVCL, (C:\Windows\system32\stdvcl40.dll)
//   (11) v4.0 StdVCL, (C:\Windows\system32\stdvcl40.dll)
//   (12) v4.0 StdVCL, (C:\Windows\system32\stdvcl40.dll)
//   (13) v4.0 StdVCL, (C:\Windows\system32\stdvcl40.dll)
//   (14) v4.0 StdVCL, (C:\Windows\system32\stdvcl40.dll)
//   (15) v4.0 StdVCL, (C:\Windows\system32\stdvcl40.dll)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit muß ohne Typüberprüfung für Zeiger compiliert werden. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// In dieser Typbibliothek deklarierte GUIDS . Es werden folgende         
// Präfixe verwendet:                                                     
//   Typbibliotheken     : LIBID_xxxx                                     
//   CoClasses           : CLASS_xxxx                                     
//   DISPInterfaces      : DIID_xxxx                                      
//   Nicht-DISP-Schnittstellen: IID_xxxx                                       
// *********************************************************************//
const
  // Haupt- und Nebenversionen der Typbibliothek
  JWSCLComMajorVersion = 1;
  JWSCLComMinorVersion = 0;

  LIBID_JWSCLCom: TGUID = '{9EBCE2EF-4E69-4AC3-AA7F-F021E119E8BB}';

  IID_IJwSid: TGUID = '{5134BF4E-3D59-44FF-A273-C052BB9B64DE}';
  IID_IJwSidList: TGUID = '{C88787BF-0091-46F2-A732-0639244C54E5}';
  CLASS_JwSid: TGUID = '{95B55CCE-E93B-4DE1-AEBA-18E006FD06F6}';
  CLASS_JwSidList: TGUID = '{6F10E768-10B0-44E6-A14E-30C905B5B01E}';
  IID_IJwTest: TGUID = '{392DA373-0FE2-45D5-AF98-89BCC8D2D0CD}';
  CLASS_JwTest2: TGUID = '{6859DD54-E45C-4857-B63C-443D0B5D57E4}';
  IID_IJwAccessControlList: TGUID = '{41FB5FBD-E101-4A32-94AB-BD463E1536BE}';
  CLASS_JwAccessControlList: TGUID = '{99AF508A-33AA-439B-993E-4D771F0C4334}';
  IID_IJwAccessControlEntry: TGUID = '{FEB78113-1787-408A-B877-7063F23604ED}';
  IID_IJwSecurityDescriptor: TGUID = '{497C3246-F614-476B-874E-EB6FFC4571E3}';
  CLASS_JwSecurityDescriptor: TGUID = '{F59BD13A-5080-4027-8104-EFCF8A2B5F0A}';
  IID_IJwToken: TGUID = '{7115952F-3B76-4CE1-B052-E4D38D4CCC09}';
  CLASS_JwToken: TGUID = '{70CC88CE-7C38-4B8D-A782-D808E38A983D}';
  IID_IJwPrivilegeList: TGUID = '{D7B09407-170C-4407-A51B-500F9479CC15}';
  CLASS_JwPrivilegeList: TGUID = '{E6B61F1E-10E4-4F3E-9E69-98286ACD900B}';
  IID_IJwPrivilege: TGUID = '{FCA8F64B-B76D-45B4-A010-E3F1134AE1A9}';
  IID_IJwWindowsVersion: TGUID = '{632F08B7-7D3B-4865-B17A-00B97FBE3D9B}';
  CLASS_JwWindowsVersion: TGUID = '{8D6C4561-E2A7-4F91-8B28-2EB458B75318}';
  IID_IJwGenericMapping: TGUID = '{DD1015C5-9E4D-4062-8E0C-E2606705673A}';
  IID_IJwFileFolderMapping: TGUID = '{B713F708-5096-4A0B-868C-BF0B5451E54A}';
  CLASS_JwGenericMapping: TGUID = '{47DDE0D2-C980-4438-B7E2-DDE2B2A10179}';
  CLASS_JwFileFolderMapping: TGUID = '{EC6AEC6D-B278-46E6-8D70-51BB574680C3}';
  IID_IJwFileMapping: TGUID = '{8E4EE90E-1864-475D-842E-C1FA9099EF0A}';
  IID_IJwRegistryMapping: TGUID = '{4213A33D-F495-4C8F-A6A3-7CDD5E626B5D}';
  IID_IJwWinStationMapping: TGUID = '{AF3DA16B-BDD7-43A7-BA86-BAB06E05C848}';
  IID_IJwDesktopMapping: TGUID = '{C389C422-764A-4FF1-B02B-7DBAC4A255DE}';
  IID_IJwServiceMapping: TGUID = '{D71696A4-36C1-4D75-9829-D470AD5B7623}';
  IID_IJwServiceManagerMapping: TGUID = '{3714347D-12AA-424F-A525-254AD715EDD8}';
  IID_IJwPrinterMapping: TGUID = '{0BCBBD6C-1DBD-41D9-A2D4-DC9540F3ECEB}';
  IID_IJwShareMapping: TGUID = '{794FDFE5-4A60-4394-BEAB-D2FCFEE0984C}';
  IID_IJwProcessMapping: TGUID = '{993CDB31-A24C-4EEA-BDD0-10BA1AB3D7E1}';
  IID_IJwThreadMapping: TGUID = '{389BEEFC-7562-4E53-878E-6E3E61A510C0}';
  IID_IJwJobMapping: TGUID = '{D5B64FC6-3A49-483C-9D7B-34A04E6E97C2}';
  IID_IJwSemaphoreMapping: TGUID = '{28EEA5E2-21E7-4CB3-A585-6D4FF82BABB5}';
  IID_IJwEventMapping: TGUID = '{64357431-6565-494D-83E5-0FAB085A9FF5}';
  IID_IJwMutexMapping: TGUID = '{F15F1B47-4DB8-4F6F-A362-51B88DA5C5C4}';
  IID_IJwFileMapMapping: TGUID = '{991D7CAB-58A1-4575-9F3B-CC34D6E2DE00}';
  IID_IJwTimerMapping: TGUID = '{10F1574D-F097-4081-8DFD-F956581FBA2C}';
  IID_IJwTokenMapping: TGUID = '{8F1F8AF3-42EA-4E51-93B0-3C5E04B47C49}';
  IID_IJwPipeMapping: TGUID = '{48260C48-B275-4C7C-BDB5-4678E1E89160}';
  CLASS_JwFileMapping: TGUID = '{3DF532B4-755F-42C9-BC59-A21E3CC708B0}';
  CLASS_JwRegistryMapping: TGUID = '{FD3ECC15-5DCE-4C33-9AD8-3E1C684B0A92}';
  CLASS_JwWinStationMapping: TGUID = '{992432AD-18A3-4258-9CC0-57FEB7E6B436}';
  CLASS_JwDesktopMapping: TGUID = '{16F54095-1737-4096-B935-34EF65C5CDBC}';
  CLASS_JwServiceMapping: TGUID = '{CFC4C3BE-DABA-4D6B-8E8B-7975BFC45565}';
  CLASS_JwServiceMappingManager: TGUID = '{2527AF41-57A8-4155-A2A3-410CF38F2DD4}';
  CLASS_JwPrinterMapping: TGUID = '{E93AFFDE-D202-41F5-BA0A-D581B522BD30}';
  CLASS_JwShareMapping: TGUID = '{55E9D4B9-C7E8-42A0-A722-92F3D14C3F15}';
  CLASS_JwProcessMapping: TGUID = '{43CA611E-9A39-4D87-9502-52DE539B9550}';
  CLASS_JwThreadMapping: TGUID = '{BFEC9D3D-CF1C-4469-91CB-6A71B3FD5AA6}';
  CLASS_JwJobMapping: TGUID = '{591054A9-C9F7-4CF6-8828-332B57B67118}';
  CLASS_JwSemaphoreMapping: TGUID = '{C44DE038-2DCD-4110-A2F6-119765F1B831}';
  CLASS_JwEventMapping: TGUID = '{C04BC95B-17F1-4724-9D11-B30CC8096329}';
  CLASS_JwMutexMapping: TGUID = '{0B253007-A4B3-4C57-ABD4-E64369929B76}';
  CLASS_JwFileMapMapping: TGUID = '{52E77685-6C77-4997-A765-10CDF08D05BE}';
  CLASS_JwTimerMapping: TGUID = '{88D8E485-04C2-4CAD-BF71-0C16092C6615}';
  CLASS_JwTokenMapping: TGUID = '{919E566A-F30C-40EF-91F8-0432E3EF19CA}';
  CLASS_JwPipeMapping: TGUID = '{4BC9B8CC-9106-43D8-A2F4-4BD0D0D39678}';
  IID_IJwLogServer: TGUID = '{213E956A-7BF3-4007-8191-1D743C818435}';
  CLASS_JwLogServer: TGUID = '{348E3372-4F93-4548-B89B-4A6EACBF8DEC}';
  IID_IJwWriterClass: TGUID = '{E225CFE1-FCAB-4109-8D64-B845D0C8FDD5}';
  IID_IJwLogClient: TGUID = '{742092DA-9FD9-420A-84AF-56D6EA8508D4}';
  CLASS_JwLogClient: TGUID = '{CB1006B5-4F9F-48D6-98AD-FCFDC71BDB95}';
  IID_ITestInt: TGUID = '{D2B0D3F0-F029-4550-A7CC-07BC612BF10B}';
  CLASS_TestInt: TGUID = '{D5B908FF-FE50-49AB-AFB2-5930687F4E82}';
  IID_IJwXMLAttribute: TGUID = '{B7C4FFC9-32AF-4341-9434-4F33FEDF1707}';
  IID_IJwEventType: TGUID = '{B95E0B5E-F424-4AC4-98F0-B1C79C19AD11}';
  CLASS_JwXMLAttribute: TGUID = '{7AB2866B-0017-4B7D-89F2-B31EA7B70A3C}';
  CLASS_JwEventType: TGUID = '{5EEE6B61-5685-4DBD-9740-ABF3109825AA}';
  IID_IJwEncryptionApi: TGUID = '{4353CD92-7CD8-4D32-B9B4-DC920AC0751C}';
  CLASS_JwEncryptionApi: TGUID = '{FEF1CB19-B79E-4C08-8B81-B1D9AEEE299E}';
  IID_IJwEncryptMemory: TGUID = '{5A177321-4491-4CFA-950F-5CB6B362604A}';
  CLASS_JwEncryptMemory: TGUID = '{9AB932A1-A20F-4C2F-847D-6F830D7BD2D3}';
  IID_IJwEncryptData: TGUID = '{895D37EF-F714-405C-BAE6-D769CF71EFF2}';
  CLASS_JwEncryptData: TGUID = '{AA19666B-0982-494E-86E7-D259EF24075B}';
  IID_IJwRandomDataGenerator: TGUID = '{ECCDCFF9-E076-4F91-8650-BA249C1B9389}';
  CLASS_JwRandomDataGenerator: TGUID = '{84E7C592-60FE-4D09-8115-63A57D61BACB}';
  IID_IJwWindowStation: TGUID = '{78BA7899-943D-4A41-9727-661E618323B6}';
  CLASS_JwWindowStation: TGUID = '{CA67E385-3E40-440A-BF34-6F50959D0252}';
  IID_IJwWindowStations: TGUID = '{D97DA30D-F731-4500-ACAB-D3CEE4C17EAC}';
  CLASS_JwWindowStations: TGUID = '{1A728EA7-874B-422D-8B36-BF35483E130A}';
  IID_IJwDesktop: TGUID = '{CF634589-D54D-42C7-B1BD-623BBD8CF556}';
  CLASS_JwDesktop: TGUID = '{359E87B0-225F-47A4-B990-C747A5820D13}';
  IID_IJwDesktops: TGUID = '{6583CE41-64A2-4A2A-9BD5-5F4C4F90D21A}';
  CLASS_JwDesktops: TGUID = '{83AB4423-1C0A-4CF5-ACE0-F0A575C92F45}';
  IID_IJwCredentialsPrompt: TGUID = '{270A0E83-2D25-43A5-936F-C883AD0DAF05}';
  CLASS_JwCredentialsPrompt: TGUID = '{6211C035-FD1D-4D3A-A54B-C79D3234156A}';
  IID_IJwCredentialsTools: TGUID = '{C0605281-F33C-4B11-AFC3-A3D0B2CF633D}';
  CLASS_JwCredentialsTools: TGUID = '{9F4C752C-A91E-478C-A14B-9086E697C9B4}';
  IID_IJwCryptProvider: TGUID = '{1862AB82-BFAB-40CA-9FB7-DBB05CFE9605}';
  CLASS_JwCryptProvider: TGUID = '{F1CDA518-1B98-4470-88FB-7E609AF52077}';
  IID_IJwHash: TGUID = '{F4114A63-2B58-4560-B848-70EEA580BE7D}';
  CLASS_JwHash: TGUID = '{43647697-24A7-4793-8D81-CB7872D3156C}';
  IID_IJwCryptKey: TGUID = '{B7517765-DF7D-4407-83A5-D41C8E556988}';
  CLASS_JwCryptKey: TGUID = '{20E2A33C-474E-44C3-9E63-1264BBD52051}';
  IID_IJwImpersonation: TGUID = '{CB4ABE99-055F-4BBD-9676-2B2211DE1F1B}';
  CLASS_JwImpersonation: TGUID = '{7889E6A6-4B0E-49ED-BD50-81B214E40B3B}';
  IID_IJwKnownSIDs: TGUID = '{396C38B0-7628-424B-975A-98091E1EF70D}';
  CLASS_JwKnownSIDs: TGUID = '{36269DEA-8801-4043-B4D8-E4B50FE6A99A}';
  IID_IJwThreadUserSid: TGUID = '{30DB5A90-30C0-44E8-B248-CAD294BC3765}';
  CLASS_JwThreadUserSid: TGUID = '{D8F38922-D2ED-4625-BB75-4CDAED4B3AB4}';
  IID_IJwLSA: TGUID = '{3A2CBCD1-F335-4F05-9A4F-8424C6471994}';
  CLASS_JwLSA: TGUID = '{2E6D17C4-E32B-458C-81E8-535ABB03E9E5}';
  IID_IJwLSALogonSession: TGUID = '{15EC2F0C-301D-417D-8814-5555FA1DAB3B}';
  CLASS_JwLSALogonSession: TGUID = '{1EA990E2-0999-414A-80E4-B79D60F01344}';
  IID_IJwLSALogonSessionData: TGUID = '{8D30540B-8656-4427-A459-08B3B9873070}';
  CLASS_JwLSALogonSessionData: TGUID = '{0E04C10C-ED62-4A40-895B-05C5AA62DBC2}';
  IID_IJwPrivilegeScope: TGUID = '{8CD86140-4C99-4F30-8DA9-826CCD70B379}';
  CLASS_JwPrivilegeScope: TGUID = '{A7DF1ED6-BA4F-4440-89F6-5862401406F3}';
  IID_IJwProcessUtils: TGUID = '{D5D2ED6A-F5EF-4935-AD4C-643FEFF06C6F}';
  CLASS_JwProcessUtils: TGUID = '{37A95D4C-B449-4403-837D-EFEF7F17875B}';
  IID_IJwSecureBaseClass: TGUID = '{803DBAF9-3F7B-416F-AC5B-C55C8637ED38}';
  IID_IJwSecureFileObject: TGUID = '{5D361B6A-ABEC-4B78-83E4-DF3944055E8C}';
  IID_IJwSecureRegistryKey: TGUID = '{0762D432-FF95-400E-B80E-19ABC879B9D7}';
  CLASS_JwSecureRegistryKey: TGUID = '{FE8239CF-0E92-47F7-AAB3-E0C1285227F6}';
  CLASS_JwSecureFileObject: TGUID = '{D0A6C06B-3BB7-4E1C-908A-BCF7315E3517}';
  CLASS_JwSecureBaseClass: TGUID = '{DBCD4006-2808-4E2D-8CCA-B64B774271B7}';
  IID_IJwProgressCallback: TGUID = '{D3989545-CA0C-4883-941B-2AF9A6F54D6B}';
  CLASS_JwProgressCallback: TGUID = '{D9F01405-DEEA-4456-8666-EBC83F37C113}';
  IID_IJwAuthZAccessRequest: TGUID = '{C197E5D5-73F6-4658-904F-9E2AB42984E2}';
  CLASS_JwAuthZAccessRequest: TGUID = '{9C4AAA33-59E4-49F0-A82A-FA769F68492D}';
  IID_IJwAuthZAccessReply: TGUID = '{89269169-0B57-4E14-81F3-84AC738BFBD3}';
  CLASS_JwAuthZAccessReply: TGUID = '{5B82A1A7-2799-4FBC-BE15-C85B7C782E09}';
  IID_IJwAuthResourceManager: TGUID = '{328A999D-6055-4DEB-A091-1B2A299F5238}';
  CLASS_JwAuthResourceManager: TGUID = '{53054D78-E724-44B3-9F52-6FCD7DB2CCA1}';
  IID_IJwAuthContext: TGUID = '{7955406B-58EC-4ACF-9ADC-BD3CCA2FD379}';
  CLASS_JwAuthContext: TGUID = '{AE4DE43C-E682-4C71-B62D-3F7C3505F82D}';
  IID_IJwTerminalServer: TGUID = '{DA19C375-048E-40BA-99CC-F7DD98BFD76F}';
  CLASS_JwTerminalServer: TGUID = '{DE467135-A2D0-40A0-924D-E0BE602D1655}';
  IID_IJwTerminalServerList: TGUID = '{F085C70B-5BCA-4EBF-A8AA-D4551C17CEF7}';
  CLASS_JwTerminalServerList: TGUID = '{DED763BD-17CF-4520-9CAA-BC21338E5DAF}';
  IID_IJwWTSSession: TGUID = '{D512987D-F6DF-4B01-87FF-0611607FCD9C}';
  CLASS_JwWTSSession: TGUID = '{887C4E1E-F5EF-4F3E-94ED-6CEEA2E69EFD}';
  IID_IJwWTSSessionList: TGUID = '{C22216FB-9BC7-4707-A28C-D529492590C4}';
  CLASS_JwWTSSessionList: TGUID = '{D01D87D9-75CF-4E82-94B0-214A082A1E42}';
  IID_IJwWTSProcess: TGUID = '{B396DC6E-BEE7-4254-B4C8-A6101682685C}';
  CLASS_JwWTSProcess: TGUID = '{441B4992-6636-4171-9405-90C774395B81}';
  IID_IJwWTSProcessList: TGUID = '{B9E2DB7A-C45C-41F1-9323-8115BA49CAC1}';
  CLASS_JwWTSProcessList: TGUID = '{7BBF9C1B-9427-4EB1-95F8-809961F0C865}';
  IID_IJwWTSSessionShadow: TGUID = '{EA1DC736-38D8-4F63-B6DD-3AD9ED0208D0}';
  CLASS_JwWTSSessionShadow: TGUID = '{AEB18080-3314-4F95-889C-BC973278995A}';
  IID_IJwSecurityDescriptorDialogCallback: TGUID = '{BA33738E-2D3E-4214-BAC6-DD044DB5697C}';
  IID_IJwInheritTypeList: TGUID = '{B6FBCBDC-7941-414F-9D8E-464BA97FB183}';
  CLASS_JwInheritTypeList: TGUID = '{16B4A0DB-3C24-403E-8ADA-A1AA41623605}';
  IID_IJwTerminalServerCallback: TGUID = '{C7387934-4900-44DD-A4A1-449A3C0564BD}';
  IID_IJwUtils: TGUID = '{8FAE3B6B-F0BC-4F41-999B-8CC35AB90BE1}';
  CLASS_JwUtils: TGUID = '{FDBDFB43-4514-4EB8-BB33-A212852CF48F}';
  IID_IJwElevationClassFactory: TGUID = '{BDD24ED3-5200-4970-9B63-9965AF7F121F}';
  CLASS_JwElevationClassFactory: TGUID = '{FAB73335-B2C4-4D8A-8B9E-EC59B6E66AB4}';
  IID_IJwExplicitAccessArray: TGUID = '{381ADFDA-CF08-4FE8-B660-78588184524A}';
  CLASS_JwExplicitAccessArray: TGUID = '{7EC861B5-E7F9-41A3-870B-4AC8741A86BD}';
  IID_IJwExplicitAccess: TGUID = '{45379DC8-AC94-41DC-917E-4716063E1F38}';
  CLASS_JwExplicitAccess: TGUID = '{A0A24861-E8A4-4166-ABBA-BD82CC5DFBCA}';
  IID_IJwGuidArray: TGUID = '{039206C7-F45F-40E0-A942-D7CA7A1E65DE}';
  CLASS_JwGuidArray: TGUID = '{553B1557-951C-48A7-A49D-FBABB2594C88}';
  IID_IJwInheritedFromRecord: TGUID = '{DDFAF8BC-C644-4FBB-B5FF-041E1220F166}';
  CLASS_JwInheritedFromRecord: TGUID = '{5839878C-006F-435E-BCA4-C000A4A887CA}';
  IID_IJwInheritedFromArray: TGUID = '{174EA097-20D7-4D69-B37D-79AB8A182DDA}';
  CLASS_JwInheritedFromArray: TGUID = '{76F23915-511F-4220-918D-8BCF76FBDC8B}';
  IID_IJwRootTuple: TGUID = '{650D81E3-14D6-4EEF-B13D-3333FEFD6CAF}';
  CLASS_JwRootTuple: TGUID = '{3159B8CD-E5D4-4769-8476-F450B2B366FA}';
  IID_IJwKeyRootTupleArray: TGUID = '{89D1C10C-47E5-44D3-A9FC-48A99C565DA3}';
  CLASS_JwKeyRootTupleArray: TGUID = '{626C626E-7FB2-4100-9A28-1E8EBAA127BB}';
  IID_IJwAccessMaskArray: TGUID = '{EE079D32-8114-495A-A2B3-5449F12F0A8B}';
  CLASS_JwAccessMaskArray: TGUID = '{A28E5A78-8817-45A8-8DE6-7F407779CC1B}';
  IID_IJwObjectTypeList: TGUID = '{567A8ADD-41D4-4808-A982-620DA1313B75}';
  CLASS_JwObjectTypeList: TGUID = '{2C5BE478-7284-4C7D-BD75-B8C397D547AC}';
  IID_IJwObjectTypeListArray: TGUID = '{F395722F-3303-489D-AA27-A04944AC2456}';
  CLASS_JwObjectTypeListArray: TGUID = '{04EAF380-259C-4BAA-BCC0-F963A37277C9}';
  IID_IJwSIDInfoData: TGUID = '{2353CD1D-4E28-4CB9-B61A-1045049CF9A3}';
  CLASS_JwSIDInfoData: TGUID = '{6ABF9717-7F4F-4DB4-8A9D-0582FF8AD8D3}';
  IID_IJwSIDInfoDataArray: TGUID = '{1A82872F-EC91-475E-8520-E0C59F7D18CD}';
  CLASS_JwSIDInfoDataArray: TGUID = '{33130D2F-79A5-479C-9B70-D93473BB881E}';

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten Enumerationen         
// *********************************************************************//
// Konstanten für enum JwCoFacilities
type
  JwCoFacilities = TOleEnum;
const
  FACILITY_JWSCL = $00000065;

// Konstanten für enum JwEnumDescriptorElements
type
  JwEnumDescriptorElements = TOleEnum;
const
  deOwner = $00000000;
  deGroup = $00000001;
  deDacl = $00000002;
  deSacl = $00000003;

// Konstanten für enum JwEnumAclProtectionState
type
  JwEnumAclProtectionState = TOleEnum;
const
  psUnprotected = $00000000;
  psProtected = $00000001;
  psForceUnprotect = $00000002;

// Konstanten für enum JwIntegrityLevelType
type
  JwIntegrityLevelType = TOleEnum;
const
  ltNone = $00000000;
  ltLow = $00000001;
  ltMedium = $00000002;
  ltHigh = $00000003;
  ltSystem = $00000004;
  ltProtected = $00000005;

// Konstanten für enum JwEnumEnterType
type
  JwEnumEnterType = TOleEnum;
const
  etNone = $00000000;
  etFunction = $00000001;
  etMethod = $00000002;
  etThread = $00000003;

// Konstanten für enum JwEnumLogType
type
  JwEnumLogType = TOleEnum;
const
  lsNone = $00000000;
  lsMessage = $00000001;
  lsWarning = $00000002;
  lsError = $00000003;
  lsStop = $00000004;

// Konstanten für enum JwEnumSignalType
type
  JwEnumSignalType = TOleEnum;
const
  stNone = $00000000;
  stSend = $00000001;
  stReceived = $00000002;
  stWait = $00000003;

// Konstanten für enum JwEnumXMLTag
type
  JwEnumXMLTag = TOleEnum;
const
  xtLogFile = $00000000;
  xtLogProcess = $00000001;
  xtEnter = $00000002;
  xtLeave = $00000003;
  xtSignal = $00000004;
  xtMemory = $00000005;
  xtLog = $00000006;
  xtException = $00000007;
  xtType = $00000008;
  xtGuid = $00000009;
  xtGetLastError = $0000000A;
  xtWinApiFunction = $0000000B;
  xtLogString = $0000000C;
  xtComSource = $0000000D;
  xtStackTrace = $0000000E;
  xtMessage = $0000000F;
  xtLastErrorString = $00000010;
  xtSourceProc = $00000011;

// Konstanten für enum JwXMLLogTag
type
  JwXMLLogTag = TOleEnum;
const
  ltEnter = $00000000;
  ltLeave = $00000001;
  ltSignal = $00000002;
  ltMemory = $00000003;
  ltLog = $00000004;
  ltException = $00000005;
  ltDisabled = $00000006;

// Konstanten für enum JwXMLAttrTag
type
  JwXMLAttrTag = TOleEnum;
const
  atStart = $00000000;
  atEnd = $00000001;
  atType = $00000002;
  atMethod = $00000003;
  atClass = $00000004;
  atFile = $00000005;
  atThread = $00000006;
  atSource = $00000007;
  atTarget = $00000008;
  atMemType = $00000009;

// Konstanten für enum JwEnumMemoryType
type
  JwEnumMemoryType = TOleEnum;
const
  mtNone = $00000000;
  mtAlloc = $00000001;
  mtDeAlloc = $00000002;

// Konstanten für enum JwAceType
type
  JwAceType = TOleEnum;
const
  actAudit = $00000000;
  actAuditCallback = $00000001;
  actAuditObject = $00000002;
  actAuditCallbackObject = $00000003;
  actMandatory = $00000004;
  actAllow = $00000005;
  actAllowCallback = $00000006;
  actAllowCallbackObject = $00000007;
  actDeny = $00000008;
  actDenyCallback = $00000009;
  actDenyObject = $0000000A;
  actDenyCallbackObject = $0000000B;
  actUnknown = $0000000C;

// Konstanten für enum JwProgInvokeSetting
type
  JwProgInvokeSetting = TOleEnum;
const
  pis_0 = $00000000;
  pis_ProgressInvokeNever = $00000001;
  pis_ProgressInvokeEveryObject = $00000002;
  pis_ProgressInvokeOnError = $00000003;
  pis_ProgressCancelOperation = $00000004;
  pis_ProgressRetryOperation = $00000005;
  pis_1 = $00000006;
  pis_2 = $00000007;
  pis_3 = $00000008;
  pis_4 = $00000009;
  pis_5 = $0000000A;
  pis_6 = $0000000B;
  pis_7 = $0000000C;
  pis_8 = $0000000D;
  pis_ProgressFinished = $0000000E;

// Konstanten für enum JwAccessControlListType
type
  JwAccessControlListType = TOleEnum;
const
  acltDiscretionary = $00000000;
  acltAuditing = $00000001;
  acltMandatory = $00000002;

// Konstanten für enum JwEqualAceType
type
  JwEqualAceType = TOleEnum;
const
  eactSameSid = $00000000;
  eactSameFlags = $00000001;
  eactSameAccessMask = $00000002;
  eactSameType = $00000003;

// Konstanten für enum JwSecurityInformationFlag
type
  JwSecurityInformationFlag = TOleEnum;
const
  siOwnerSecurityInformation = $00000000;
  siGroupSecurityInformation = $00000001;
  siDACLSecurityInformation = $00000002;
  siSACLSecurityInforation = $00000003;
  siLabelSecurityInformation = $00000004;
  siProtectedDACLSecurityInformation = $00000005;
  siProtectedSACLSecurityInformation = $00000006;
  siUnprotectedDACLSecurityInformation = $00000007;
  siUnprotectedSACLSecurityInformation = $00000008;

// Konstanten für enum JwSecurityResetType
type
  JwSecurityResetType = TOleEnum;
const
  srNone = $00000000;
  srOwner = $00000001;
  srDACL = $00000002;
  srSACL = $00000003;

// Konstanten für enum JwTempResetEnum
type
  JwTempResetEnum = TOleEnum;
const
  treOwner = $00000000;
  treGroup = $00000001;
  treDACL = $00000002;
  treSACL = $00000003;

// Konstanten für enum JwSecurityDescriptorControl
type
  JwSecurityDescriptorControl = TOleEnum;
const
  sdcOwnerDefaulted = $00000000;
  sdcGroupDefaulted = $00000001;
  sdcDACLPresent = $00000002;
  sdcSACLPresent = $00000003;
  sdcDACLAutoInheritReq = $00000004;
  sdcSACLAutoInheritReq = $00000005;
  sdcDACLAutoInherited = $00000006;
  sdcSACLAutoInherited = $00000007;
  sdcDACLProtected = $00000008;
  sdcSACLProtected = $00000009;
  sdcRMControlValid = $0000000A;
  sdcSelfRelative = $0000000B;

// Konstanten für enum JwACLProtection
type
  JwACLProtection = TOleEnum;
const
  aclpUnprotected = $00000000;
  aclpProtected = $00000001;
  aclpForceUnprotect = $00000002;

// Konstanten für enum JwTreeSetType
type
  JwTreeSetType = TOleEnum;
const
  tstSet = $00000000;
  tstReset = $00000001;

// Konstanten für enum JwRootRegKey
type
  JwRootRegKey = TOleEnum;
const
  rrkString = $00000000;
  rrkLocalMachine = $00000001;
  rrkCurrentUser = $00000002;
  rrkUsers = $00000003;
  rrkCurrentConfig = $00000004;
  rrkClassesRoot = $00000005;

// Konstanten für enum JwRootKeyEnum
type
  JwRootKeyEnum = TOleEnum;
const
  rkeLocal = $00000000;
  rkeCurrentUser = $00000001;
  rkeUsers = $00000002;
  rkeConfig = $00000003;
  rkeClasses = $00000004;

// Konstanten für enum JwSecurityDialogFlag
type
  JwSecurityDialogFlag = TOleEnum;
const
  sdfEditDACL = $00000000;
  sdfEditSACL = $00000001;
  sdfEditOwner = $00000002;
  sdfContainer = $00000003;
  sdtReadOnly = $00000004;
  sdfAdvanced = $00000005;
  sdfReset = $00000006;
  sdfOwnerReadOnly = $00000007;
  sdfEditProperties = $00000008;
  sdfOwnerRecurse = $00000009;
  sdfNoACLProtect = $0000000A;
  sdfNoTreeApply = $0000000B;
  sdfServerIsDC = $0000000C;
  sdfResetDACLTree = $0000000D;
  sdfResetSACLTree = $0000000E;
  sdfObjectGUID = $0000000F;
  sdfEditEffective = $00000010;
  sdfResetDACL = $00000011;
  sdfResetSACL = $00000012;
  sdfResetOwner = $00000013;
  sdfNoAdditionalPermission = $00000014;
  sdfMayWrite = $00000015;
  sdfPageTitle = $00000016;

// Konstanten für enum JwSecurityPageType
type
  JwSecurityPageType = TOleEnum;
const
  sptPageDACL = $00000000;
  sptPageAdvPerm = $00000001;
  sptPageSACL = $00000002;
  sptPageOwner = $00000003;
  sptPageEffective = $00000004;
  sptPageTakeOwnerShip = $00000005;
  sptPad1 = $00000006;
  sptPad2 = $00000007;
  sptPad3 = $00000008;
  sptPad4 = $00000009;
  sptPad5 = $0000000A;
  sptPad6 = $0000000B;
  sptPad7 = $0000000C;
  sptPad8 = $0000000D;
  sptPad9 = $0000000E;
  sptACLWindow = $0000000F;
  sptAdvWindow = $00000010;

// Konstanten für enum JwSIDClassName
type
  JwSIDClassName = TOleEnum;
const
  scnNone = $00000000;
  scnComputer = $00000001;
  scnGroup = $00000002;
  scnUnknown = $00000003;

// Konstanten für enum JwDesktopCreationFlag
type
  JwDesktopCreationFlag = TOleEnum;
const
  dcfCreate = $00000000;
  dcfOpen = $00000001;

// Konstanten für enum JwDesktopFlag
type
  JwDesktopFlag = TOleEnum;
const
  dfPad0 = $00000000;
  dfAllowOtherAccountHook = $00000001;

// Konstanten für enum JwSIDAttribute
type
  JwSIDAttribute = TOleEnum;
const
  sidaUnknown = $00000000;
  sidaGroupMandatory = $00000001;
  sidaGroupEnabledByDefault = $00000002;
  sidaGroupEnabled = $00000003;
  sidaGroupOwner = $00000004;
  sidaGroupUseForDenyOnly = $00000005;
  sidaGroupLogonID = $00000006;
  sidaGroupResource = $00000007;
  sidaGroupIntegrity = $00000008;
  sidaGroupIntegrityEnabled = $00000009;
  sidaPad0 = $0000000A;
  sidaPad1 = $0000000B;
  sidaPad2 = $0000000C;
  sidaPad3 = $0000000D;
  sidaPad4 = $0000000E;
  sidaPad5 = $0000000F;

type

// *********************************************************************//
// Forward-Deklaration von in der Typbibliothek definierten Typen         
// *********************************************************************//
  IJwSid = interface;
  IJwSidDisp = dispinterface;
  IJwSidList = interface;
  IJwSidListDisp = dispinterface;
  IJwTest = interface;
  IJwTestDisp = dispinterface;
  IJwAccessControlList = interface;
  IJwAccessControlListDisp = dispinterface;
  IJwAccessControlEntry = interface;
  IJwAccessControlEntryDisp = dispinterface;
  IJwSecurityDescriptor = interface;
  IJwSecurityDescriptorDisp = dispinterface;
  IJwToken = interface;
  IJwTokenDisp = dispinterface;
  IJwPrivilegeList = interface;
  IJwPrivilegeListDisp = dispinterface;
  IJwPrivilege = interface;
  IJwPrivilegeDisp = dispinterface;
  IJwWindowsVersion = interface;
  IJwWindowsVersionDisp = dispinterface;
  IJwGenericMapping = interface;
  IJwGenericMappingDisp = dispinterface;
  IJwFileFolderMapping = interface;
  IJwFileFolderMappingDisp = dispinterface;
  IJwFileMapping = interface;
  IJwFileMappingDisp = dispinterface;
  IJwRegistryMapping = interface;
  IJwRegistryMappingDisp = dispinterface;
  IJwWinStationMapping = interface;
  IJwWinStationMappingDisp = dispinterface;
  IJwDesktopMapping = interface;
  IJwDesktopMappingDisp = dispinterface;
  IJwServiceMapping = interface;
  IJwServiceMappingDisp = dispinterface;
  IJwServiceManagerMapping = interface;
  IJwServiceManagerMappingDisp = dispinterface;
  IJwPrinterMapping = interface;
  IJwPrinterMappingDisp = dispinterface;
  IJwShareMapping = interface;
  IJwShareMappingDisp = dispinterface;
  IJwProcessMapping = interface;
  IJwProcessMappingDisp = dispinterface;
  IJwThreadMapping = interface;
  IJwThreadMappingDisp = dispinterface;
  IJwJobMapping = interface;
  IJwJobMappingDisp = dispinterface;
  IJwSemaphoreMapping = interface;
  IJwSemaphoreMappingDisp = dispinterface;
  IJwEventMapping = interface;
  IJwEventMappingDisp = dispinterface;
  IJwMutexMapping = interface;
  IJwMutexMappingDisp = dispinterface;
  IJwFileMapMapping = interface;
  IJwFileMapMappingDisp = dispinterface;
  IJwTimerMapping = interface;
  IJwTimerMappingDisp = dispinterface;
  IJwTokenMapping = interface;
  IJwTokenMappingDisp = dispinterface;
  IJwPipeMapping = interface;
  IJwPipeMappingDisp = dispinterface;
  IJwLogServer = interface;
  IJwLogServerDisp = dispinterface;
  IJwWriterClass = interface;
  IJwWriterClassDisp = dispinterface;
  IJwLogClient = interface;
  IJwLogClientDisp = dispinterface;
  ITestInt = interface;
  ITestIntDisp = dispinterface;
  IJwXMLAttribute = interface;
  IJwXMLAttributeDisp = dispinterface;
  IJwEventType = interface;
  IJwEventTypeDisp = dispinterface;
  IJwEncryptionApi = interface;
  IJwEncryptionApiDisp = dispinterface;
  IJwEncryptMemory = interface;
  IJwEncryptMemoryDisp = dispinterface;
  IJwEncryptData = interface;
  IJwEncryptDataDisp = dispinterface;
  IJwRandomDataGenerator = interface;
  IJwRandomDataGeneratorDisp = dispinterface;
  IJwWindowStation = interface;
  IJwWindowStationDisp = dispinterface;
  IJwWindowStations = interface;
  IJwWindowStationsDisp = dispinterface;
  IJwDesktop = interface;
  IJwDesktopDisp = dispinterface;
  IJwDesktops = interface;
  IJwDesktopsDisp = dispinterface;
  IJwCredentialsPrompt = interface;
  IJwCredentialsPromptDisp = dispinterface;
  IJwCredentialsTools = interface;
  IJwCredentialsToolsDisp = dispinterface;
  IJwCryptProvider = interface;
  IJwCryptProviderDisp = dispinterface;
  IJwHash = interface;
  IJwHashDisp = dispinterface;
  IJwCryptKey = interface;
  IJwCryptKeyDisp = dispinterface;
  IJwImpersonation = interface;
  IJwImpersonationDisp = dispinterface;
  IJwKnownSIDs = interface;
  IJwKnownSIDsDisp = dispinterface;
  IJwThreadUserSid = interface;
  IJwThreadUserSidDisp = dispinterface;
  IJwLSA = interface;
  IJwLSADisp = dispinterface;
  IJwLSALogonSession = interface;
  IJwLSALogonSessionDisp = dispinterface;
  IJwLSALogonSessionData = interface;
  IJwLSALogonSessionDataDisp = dispinterface;
  IJwPrivilegeScope = interface;
  IJwPrivilegeScopeDisp = dispinterface;
  IJwProcessUtils = interface;
  IJwProcessUtilsDisp = dispinterface;
  IJwSecureBaseClass = interface;
  IJwSecureBaseClassDisp = dispinterface;
  IJwSecureFileObject = interface;
  IJwSecureFileObjectDisp = dispinterface;
  IJwSecureRegistryKey = interface;
  IJwSecureRegistryKeyDisp = dispinterface;
  IJwProgressCallback = interface;
  IJwAuthZAccessRequest = interface;
  IJwAuthZAccessRequestDisp = dispinterface;
  IJwAuthZAccessReply = interface;
  IJwAuthZAccessReplyDisp = dispinterface;
  IJwAuthResourceManager = interface;
  IJwAuthResourceManagerDisp = dispinterface;
  IJwAuthContext = interface;
  IJwAuthContextDisp = dispinterface;
  IJwTerminalServer = interface;
  IJwTerminalServerDisp = dispinterface;
  IJwTerminalServerList = interface;
  IJwTerminalServerListDisp = dispinterface;
  IJwWTSSession = interface;
  IJwWTSSessionDisp = dispinterface;
  IJwWTSSessionList = interface;
  IJwWTSSessionListDisp = dispinterface;
  IJwWTSProcess = interface;
  IJwWTSProcessDisp = dispinterface;
  IJwWTSProcessList = interface;
  IJwWTSProcessListDisp = dispinterface;
  IJwWTSSessionShadow = interface;
  IJwWTSSessionShadowDisp = dispinterface;
  IJwSecurityDescriptorDialogCallback = interface;
  IJwSecurityDescriptorDialogCallbackDisp = dispinterface;
  IJwInheritTypeList = interface;
  IJwInheritTypeListDisp = dispinterface;
  IJwTerminalServerCallback = interface;
  IJwTerminalServerCallbackDisp = dispinterface;
  IJwUtils = interface;
  IJwUtilsDisp = dispinterface;
  IJwElevationClassFactory = interface;
  IJwElevationClassFactoryDisp = dispinterface;
  IJwExplicitAccessArray = interface;
  IJwExplicitAccessArrayDisp = dispinterface;
  IJwExplicitAccess = interface;
  IJwExplicitAccessDisp = dispinterface;
  IJwGuidArray = interface;
  IJwGuidArrayDisp = dispinterface;
  IJwInheritedFromRecord = interface;
  IJwInheritedFromRecordDisp = dispinterface;
  IJwInheritedFromArray = interface;
  IJwInheritedFromArrayDisp = dispinterface;
  IJwRootTuple = interface;
  IJwRootTupleDisp = dispinterface;
  IJwKeyRootTupleArray = interface;
  IJwKeyRootTupleArrayDisp = dispinterface;
  IJwAccessMaskArray = interface;
  IJwAccessMaskArrayDisp = dispinterface;
  IJwObjectTypeList = interface;
  IJwObjectTypeListDisp = dispinterface;
  IJwObjectTypeListArray = interface;
  IJwObjectTypeListArrayDisp = dispinterface;
  IJwSIDInfoData = interface;
  IJwSIDInfoDataDisp = dispinterface;
  IJwSIDInfoDataArray = interface;
  IJwSIDInfoDataArrayDisp = dispinterface;

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten CoClasses             
// (HINWEIS: Hier wird jede CoClass zu ihrer Standardschnittstelle        
// zugewiesen)                                                            
// *********************************************************************//
  JwSid = IJwSid;
  JwSidList = IJwSidList;
  JwTest2 = IJwTest;
  JwAccessControlList = IJwAccessControlList;
  JwSecurityDescriptor = IJwSecurityDescriptor;
  JwToken = IJwToken;
  JwPrivilegeList = IJwPrivilegeList;
  JwWindowsVersion = IJwWindowsVersion;
  JwGenericMapping = IJwGenericMapping;
  JwFileFolderMapping = IJwFileFolderMapping;
  JwFileMapping = IJwFileMapping;
  JwRegistryMapping = IJwRegistryMapping;
  JwWinStationMapping = IJwWinStationMapping;
  JwDesktopMapping = IJwDesktopMapping;
  JwServiceMapping = IJwServiceMapping;
  JwServiceMappingManager = IJwServiceManagerMapping;
  JwPrinterMapping = IJwPrinterMapping;
  JwShareMapping = IJwShareMapping;
  JwProcessMapping = IJwProcessMapping;
  JwThreadMapping = IJwThreadMapping;
  JwJobMapping = IJwJobMapping;
  JwSemaphoreMapping = IJwSemaphoreMapping;
  JwEventMapping = IJwEventMapping;
  JwMutexMapping = IJwMutexMapping;
  JwFileMapMapping = IJwFileMapMapping;
  JwTimerMapping = IJwTimerMapping;
  JwTokenMapping = IJwTokenMapping;
  JwPipeMapping = IJwPipeMapping;
  JwLogServer = IJwLogServer;
  JwLogClient = IJwLogClient;
  TestInt = ITestInt;
  JwXMLAttribute = IJwXMLAttribute;
  JwEventType = IJwEventType;
  JwEncryptionApi = IJwEncryptionApi;
  JwEncryptMemory = IJwEncryptMemory;
  JwEncryptData = IJwEncryptData;
  JwRandomDataGenerator = IJwRandomDataGenerator;
  JwWindowStation = IJwWindowStation;
  JwWindowStations = IJwWindowStations;
  JwDesktop = IJwDesktop;
  JwDesktops = IJwDesktops;
  JwCredentialsPrompt = IJwCredentialsPrompt;
  JwCredentialsTools = IJwCredentialsTools;
  JwCryptProvider = IJwCryptProvider;
  JwHash = IJwHash;
  JwCryptKey = IJwCryptKey;
  JwImpersonation = IJwImpersonation;
  JwKnownSIDs = IJwKnownSIDs;
  JwThreadUserSid = IJwThreadUserSid;
  JwLSA = IJwLSA;
  JwLSALogonSession = IJwLSALogonSession;
  JwLSALogonSessionData = IJwLSALogonSessionData;
  JwPrivilegeScope = IJwPrivilegeScope;
  JwProcessUtils = IJwProcessUtils;
  JwSecureRegistryKey = IJwSecureRegistryKey;
  JwSecureFileObject = IJwSecureFileObject;
  JwSecureBaseClass = IJwSecureBaseClass;
  JwProgressCallback = IJwProgressCallback;
  JwAuthZAccessRequest = IJwAuthZAccessRequest;
  JwAuthZAccessReply = IJwAuthZAccessReply;
  JwAuthResourceManager = IJwAuthResourceManager;
  JwAuthContext = IJwAuthContext;
  JwTerminalServer = IJwTerminalServer;
  JwTerminalServerList = IJwTerminalServerList;
  JwWTSSession = IJwWTSSession;
  JwWTSSessionList = IJwWTSSessionList;
  JwWTSProcess = IJwWTSProcess;
  JwWTSProcessList = IJwWTSProcessList;
  JwWTSSessionShadow = IJwWTSSessionShadow;
  JwInheritTypeList = IJwInheritTypeList;
  JwUtils = IJwUtils;
  JwElevationClassFactory = IJwElevationClassFactory;
  JwExplicitAccessArray = IJwExplicitAccessArray;
  JwExplicitAccess = IJwExplicitAccess;
  JwGuidArray = IJwGuidArray;
  JwInheritedFromRecord = IJwInheritedFromRecord;
  JwInheritedFromArray = IJwInheritedFromArray;
  JwRootTuple = IJwRootTuple;
  JwKeyRootTupleArray = IJwKeyRootTupleArray;
  JwAccessMaskArray = IJwAccessMaskArray;
  JwObjectTypeList = IJwObjectTypeList;
  JwObjectTypeListArray = IJwObjectTypeListArray;
  JwSIDInfoData = IJwSIDInfoData;
  JwSIDInfoDataArray = IJwSIDInfoDataArray;


// *********************************************************************// 
// Deklaration von  Strukturen, Unions und Aliasen.                        
// *********************************************************************// 
  PWideString1 = ^WideString; {*}

  PCoSid = PChar; 
  PCoSidAndAttributes = PChar; 
  PCoTokenGroups = PChar; 

  TGenericMapping = packed record
    GenericRead: LongWord;
    GenericWrite: LongWord;
    GenericExecute: LongWord;
    GenericAll: LongWord;
  end;

  JwTokenHandle = LongWord; 
  JwAccessMask = LongWord; 
  JwThreadHandle = LongWord; 
  JwProcessHandle = LongWord; 
  JwLastError = LongWord; 
  JwTokenAccessMask = LongWord; 
  JwCSPHandle = LongWord; 
  JwHandle = LongWord; 
  JwKeyHandle = JwHandle; 
  JwSessionID = JwHandle; 
  JwState = JwHandle; 
  JwProcessID = JwHandle; 
  JwBitMask = LongWord; 
  JwAceTypes = JwBitMask; 
  JwEqualAceTypes = JwBitMask; 
  JwSecurityInforationFlags = JwBitMask; 
  JwExtDWORD = Int64; 
  JwTempResetEnumSet = JwBitMask; 
  JwSecurityDescriptorControls = JwBitMask; 
  JwSecurityDialogFlags = JwBitMask; 
  JwDesktopFlags = JwBitMask; 

// *********************************************************************//
// Schnittstelle: IJwSid
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5134BF4E-3D59-44FF-A273-C052BB9B64DE}
// *********************************************************************//
  IJwSid = interface(IDispatch)
    ['{5134BF4E-3D59-44FF-A273-C052BB9B64DE}']
    procedure InitByBinarySid(const BinarySid: WideString); safecall;
    procedure InitByStream(const SidAsStream: IUnknown); safecall;
    procedure InitByName(const SystemName: WideString; const AccountName: WideString); safecall;
    procedure InitByJwSid(const Sid: IJwSid); safecall;
    procedure InitByWellKnownSid(SidType: SYSUINT); safecall;
    procedure InitByAuthorities(Authorities: OleVariant; Identifier: OleVariant); safecall;
    function IsStandardSid: WordBool; safecall;
    function GetInternalObject: PChar; safecall;
    function Get_SubAuthorityCount: SYSUINT; safecall;
    function Get_SubAuthorityArray: OleVariant; safecall;
    function Get_IdentifierAttributesCount: SYSUINT; safecall;
    function Get_IdentifierAttributesArray: OleVariant; safecall;
    function Get_IsWellKnownSidType: WordBool; safecall;
    function GetAccountName(const SystemName: WideString): WideString; safecall;
    function Get_CachedSystemName: WideString; safecall;
    procedure Set_CachedSystemName(const Value: WideString); safecall;
    function GetAccountDomainName(const SystemName: WideString): WideString; safecall;
    function GetAccountNameInUse(const SystemName: WideString): LongWord; safecall;
    function GetCachedUserName: WideString; safecall;
    function Get_Attributes: LongWord; safecall;
    procedure Set_Attributes(Value: LongWord); safecall;
    function Get_AttributesByType: OleVariant; safecall;
    procedure Set_AttributesByType(Value: OleVariant); safecall;
    function GetStream: IUnknown; safecall;
    function Get_UserName: WideString; safecall;
    function IsEqualSid(const Sid: IJwSid): WordBool; safecall;
    function Get_StringSid: WideString; safecall;
    function ToString: WideString; safecall;
    property SubAuthorityCount: SYSUINT read Get_SubAuthorityCount;
    property SubAuthorityArray: OleVariant read Get_SubAuthorityArray;
    property IdentifierAttributesCount: SYSUINT read Get_IdentifierAttributesCount;
    property IdentifierAttributesArray: OleVariant read Get_IdentifierAttributesArray;
    property IsWellKnownSidType: WordBool read Get_IsWellKnownSidType;
    property CachedSystemName: WideString read Get_CachedSystemName write Set_CachedSystemName;
    property Attributes: LongWord read Get_Attributes write Set_Attributes;
    property AttributesByType: OleVariant read Get_AttributesByType write Set_AttributesByType;
    property UserName: WideString read Get_UserName;
    property StringSid: WideString read Get_StringSid;
  end;

// *********************************************************************//
// DispIntf:  IJwSidDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5134BF4E-3D59-44FF-A273-C052BB9B64DE}
// *********************************************************************//
  IJwSidDisp = dispinterface
    ['{5134BF4E-3D59-44FF-A273-C052BB9B64DE}']
    procedure InitByBinarySid(const BinarySid: WideString); dispid 201;
    procedure InitByStream(const SidAsStream: IUnknown); dispid 202;
    procedure InitByName(const SystemName: WideString; const AccountName: WideString); dispid 203;
    procedure InitByJwSid(const Sid: IJwSid); dispid 204;
    procedure InitByWellKnownSid(SidType: SYSUINT); dispid 205;
    procedure InitByAuthorities(Authorities: OleVariant; Identifier: OleVariant); dispid 206;
    function IsStandardSid: WordBool; dispid 207;
    function GetInternalObject: {??PChar}OleVariant; dispid 208;
    property SubAuthorityCount: SYSUINT readonly dispid 209;
    property SubAuthorityArray: OleVariant readonly dispid 210;
    property IdentifierAttributesCount: SYSUINT readonly dispid 211;
    property IdentifierAttributesArray: OleVariant readonly dispid 213;
    property IsWellKnownSidType: WordBool readonly dispid 212;
    function GetAccountName(const SystemName: WideString): WideString; dispid 214;
    property CachedSystemName: WideString dispid 215;
    function GetAccountDomainName(const SystemName: WideString): WideString; dispid 216;
    function GetAccountNameInUse(const SystemName: WideString): LongWord; dispid 217;
    function GetCachedUserName: WideString; dispid 218;
    property Attributes: LongWord dispid 219;
    property AttributesByType: OleVariant dispid 220;
    function GetStream: IUnknown; dispid 221;
    property UserName: WideString readonly dispid 222;
    function IsEqualSid(const Sid: IJwSid): WordBool; dispid 223;
    property StringSid: WideString readonly dispid 224;
    function ToString: WideString; dispid 225;
  end;

// *********************************************************************//
// Schnittstelle: IJwSidList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C88787BF-0091-46F2-A732-0639244C54E5}
// *********************************************************************//
  IJwSidList = interface(IDispatch)
    ['{C88787BF-0091-46F2-A732-0639244C54E5}']
    procedure InitBySidList(const SidList: IUnknown); safecall;
    procedure Add(const Sid: IJwSid); safecall;
    procedure Insert(Index: Integer; const Sid: IJwSid); safecall;
    procedure Remove(const Sid: IJwSid); safecall;
    procedure Delete(Index: Integer); safecall;
    function Get_Count: LongWord; safecall;
    function Get_Item(Index: LongWord): IJwSid; safecall;
    function Get__NewEnum: OleVariant; safecall;
    procedure Clear; safecall;
    function Find(const Sid: IJwSid; StartPos: Integer; UsePreFix: WordBool): Integer; safecall;
    property Count: LongWord read Get_Count;
    property Item[Index: LongWord]: IJwSid read Get_Item; default;
    property _NewEnum: OleVariant read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IJwSidListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C88787BF-0091-46F2-A732-0639244C54E5}
// *********************************************************************//
  IJwSidListDisp = dispinterface
    ['{C88787BF-0091-46F2-A732-0639244C54E5}']
    procedure InitBySidList(const SidList: IUnknown); dispid 201;
    procedure Add(const Sid: IJwSid); dispid 202;
    procedure Insert(Index: Integer; const Sid: IJwSid); dispid 203;
    procedure Remove(const Sid: IJwSid); dispid 204;
    procedure Delete(Index: Integer); dispid 205;
    property Count: LongWord readonly dispid 206;
    property Item[Index: LongWord]: IJwSid readonly dispid 0; default;
    property _NewEnum: OleVariant readonly dispid -4;
    procedure Clear; dispid 207;
    function Find(const Sid: IJwSid; StartPos: Integer; UsePreFix: WordBool): Integer; dispid 208;
  end;

// *********************************************************************//
// Schnittstelle: IJwTest
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {392DA373-0FE2-45D5-AF98-89BCC8D2D0CD}
// *********************************************************************//
  IJwTest = interface(IDispatch)
    ['{392DA373-0FE2-45D5-AF98-89BCC8D2D0CD}']
    procedure Method1; safecall;
  end;

// *********************************************************************//
// DispIntf:  IJwTestDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {392DA373-0FE2-45D5-AF98-89BCC8D2D0CD}
// *********************************************************************//
  IJwTestDisp = dispinterface
    ['{392DA373-0FE2-45D5-AF98-89BCC8D2D0CD}']
    procedure Method1; dispid 201;
  end;

// *********************************************************************//
// Schnittstelle: IJwAccessControlList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {41FB5FBD-E101-4A32-94AB-BD463E1536BE}
// *********************************************************************//
  IJwAccessControlList = interface(IDispatch)
    ['{41FB5FBD-E101-4A32-94AB-BD463E1536BE}']
    procedure AddAllowAce(const Sid: IJwSid; Mask: LongWord; Flags: LongWord; ObjectType: TGUID; 
                          InheritedObjectType: TGUID); safecall;
    procedure AddDenyAce(const Sid: IJwSid; Mask: LongWord; Flags: LongWord; ObjectType: TGUID; 
                         InheritedObjectType: TGUID); safecall;
    procedure Item; safecall;
    function Get__NewEnum: IJwAccessControlEntry; safecall;
    function GetInternalObject: PChar; safecall;
    procedure Delete(Index: LongWord); safecall;
    procedure Remove(const Ace: IJwAccessControlEntry); safecall;
    function ToString: WideString; safecall;
    procedure Find(const Sid: IJwSid; Index: Integer); safecall;
    function Get_Revision: LongWord; safecall;
    procedure Set_Revision(Value: LongWord); safecall;
    procedure InitByAccessList(const List: IJwAccessControlList); safecall;
    procedure Clear; safecall;
    property _NewEnum: IJwAccessControlEntry read Get__NewEnum;
    property Revision: LongWord read Get_Revision write Set_Revision;
  end;

// *********************************************************************//
// DispIntf:  IJwAccessControlListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {41FB5FBD-E101-4A32-94AB-BD463E1536BE}
// *********************************************************************//
  IJwAccessControlListDisp = dispinterface
    ['{41FB5FBD-E101-4A32-94AB-BD463E1536BE}']
    procedure AddAllowAce(const Sid: IJwSid; Mask: LongWord; Flags: LongWord; 
                          ObjectType: {??TGUID}OleVariant; InheritedObjectType: {??TGUID}OleVariant); dispid 201;
    procedure AddDenyAce(const Sid: IJwSid; Mask: LongWord; Flags: LongWord; 
                         ObjectType: {??TGUID}OleVariant; InheritedObjectType: {??TGUID}OleVariant); dispid 202;
    procedure Item; dispid 203;
    property _NewEnum: IJwAccessControlEntry readonly dispid -4;
    function GetInternalObject: {??PChar}OleVariant; dispid 204;
    procedure Delete(Index: LongWord); dispid 205;
    procedure Remove(const Ace: IJwAccessControlEntry); dispid 206;
    function ToString: WideString; dispid 207;
    procedure Find(const Sid: IJwSid; Index: Integer); dispid 208;
    property Revision: LongWord dispid 209;
    procedure InitByAccessList(const List: IJwAccessControlList); dispid 210;
    procedure Clear; dispid 211;
  end;

// *********************************************************************//
// Schnittstelle: IJwAccessControlEntry
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FEB78113-1787-408A-B877-7063F23604ED}
// *********************************************************************//
  IJwAccessControlEntry = interface(IDispatch)
    ['{FEB78113-1787-408A-B877-7063F23604ED}']
    function Get_AccessMask: LongWord; safecall;
    procedure Set_AccessMask(Value: LongWord); safecall;
    function Get_Sid: IJwSid; safecall;
    procedure Set_Sid(const Value: IJwSid); safecall;
    function Get_Flags: LongWord; safecall;
    procedure Set_Flags(Value: LongWord); safecall;
    function Get_Revision: Word; safecall;
    procedure Set_Revision(Value: Word); safecall;
    function Get_ObjectType: TGUID; safecall;
    procedure Set_ObjectType(Value: TGUID); safecall;
    function Get_InheritedObjectType: TGUID; safecall;
    procedure Set_InheritedObjectType(Value: TGUID); safecall;
    function ToString(const Map: IUnknown): WideString; safecall;
    function Get_AceType: LongWord; safecall;
    procedure Init(AceType: LongWord); safecall;
    property AccessMask: LongWord read Get_AccessMask write Set_AccessMask;
    property Sid: IJwSid read Get_Sid write Set_Sid;
    property Flags: LongWord read Get_Flags write Set_Flags;
    property Revision: Word read Get_Revision write Set_Revision;
    property ObjectType: TGUID read Get_ObjectType write Set_ObjectType;
    property InheritedObjectType: TGUID read Get_InheritedObjectType write Set_InheritedObjectType;
    property AceType: LongWord read Get_AceType;
  end;

// *********************************************************************//
// DispIntf:  IJwAccessControlEntryDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FEB78113-1787-408A-B877-7063F23604ED}
// *********************************************************************//
  IJwAccessControlEntryDisp = dispinterface
    ['{FEB78113-1787-408A-B877-7063F23604ED}']
    property AccessMask: LongWord dispid 201;
    property Sid: IJwSid dispid 202;
    property Flags: LongWord dispid 203;
    property Revision: {??Word}OleVariant dispid 204;
    property ObjectType: {??TGUID}OleVariant dispid 205;
    property InheritedObjectType: {??TGUID}OleVariant dispid 206;
    function ToString(const Map: IUnknown): WideString; dispid 208;
    property AceType: LongWord readonly dispid 207;
    procedure Init(AceType: LongWord); dispid 209;
  end;

// *********************************************************************//
// Schnittstelle: IJwSecurityDescriptor
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {497C3246-F614-476B-874E-EB6FFC4571E3}
// *********************************************************************//
  IJwSecurityDescriptor = interface(IDispatch)
    ['{497C3246-F614-476B-874E-EB6FFC4571E3}']
    function Get_Owner: IJwSid; safecall;
    procedure Set_Owner(const Value: IJwSid); safecall;
    function Get_Group: IJwSid; safecall;
    procedure Set_Group(const Value: IJwSid); safecall;
    function Get_DACL: IJwAccessControlList; safecall;
    procedure Set_DACL(const Value: IJwAccessControlList); safecall;
    function Get_SACL: IJwAccessControlList; safecall;
    procedure Set_SACL(const Value: IJwAccessControlList); safecall;
    function ToString(const Map: IUnknown): WideString; safecall;
    function Get_Control: LongWord; safecall;
    procedure Set_Control(Value: LongWord); safecall;
    function Get_OwnerInherited: WordBool; safecall;
    procedure Set_OwnerInherited(Value: WordBool); safecall;
    function Get_GroupInherited: WordBool; safecall;
    procedure Set_GroupInherited(Value: WordBool); safecall;
    function Get_DACLInherited: WordBool; safecall;
    procedure Set_DACLInherited(Value: WordBool); safecall;
    function Get_InheritanceDACLProtection: LongWord; safecall;
    procedure Set_InheritanceDACLProtection(Value: LongWord); safecall;
    function Get_InheritanceSACLProtection: LongWord; safecall;
    procedure Set_InheritanceSACLProtection(Value: LongWord); safecall;
    procedure InitBySD(const SD: IJwSecurityDescriptor; Elements: LongWord); safecall;
    procedure InitByString(const SDString: WideString); safecall;
    procedure InitDefaultByToken(const Token: IUnknown; TokenType: LongWord); safecall;
    property Owner: IJwSid read Get_Owner write Set_Owner;
    property Group: IJwSid read Get_Group write Set_Group;
    property DACL: IJwAccessControlList read Get_DACL write Set_DACL;
    property SACL: IJwAccessControlList read Get_SACL write Set_SACL;
    property Control: LongWord read Get_Control write Set_Control;
    property OwnerInherited: WordBool read Get_OwnerInherited write Set_OwnerInherited;
    property GroupInherited: WordBool read Get_GroupInherited write Set_GroupInherited;
    property DACLInherited: WordBool read Get_DACLInherited write Set_DACLInherited;
    property InheritanceDACLProtection: LongWord read Get_InheritanceDACLProtection write Set_InheritanceDACLProtection;
    property InheritanceSACLProtection: LongWord read Get_InheritanceSACLProtection write Set_InheritanceSACLProtection;
  end;

// *********************************************************************//
// DispIntf:  IJwSecurityDescriptorDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {497C3246-F614-476B-874E-EB6FFC4571E3}
// *********************************************************************//
  IJwSecurityDescriptorDisp = dispinterface
    ['{497C3246-F614-476B-874E-EB6FFC4571E3}']
    property Owner: IJwSid dispid 201;
    property Group: IJwSid dispid 202;
    property DACL: IJwAccessControlList dispid 203;
    property SACL: IJwAccessControlList dispid 204;
    function ToString(const Map: IUnknown): WideString; dispid 205;
    property Control: LongWord dispid 206;
    property OwnerInherited: WordBool dispid 207;
    property GroupInherited: WordBool dispid 208;
    property DACLInherited: WordBool dispid 209;
    property InheritanceDACLProtection: LongWord dispid 210;
    property InheritanceSACLProtection: LongWord dispid 211;
    procedure InitBySD(const SD: IJwSecurityDescriptor; Elements: LongWord); dispid 212;
    procedure InitByString(const SDString: WideString); dispid 213;
    procedure InitDefaultByToken(const Token: IUnknown; TokenType: LongWord); dispid 214;
  end;

// *********************************************************************//
// Schnittstelle: IJwToken
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7115952F-3B76-4CE1-B052-E4D38D4CCC09}
// *********************************************************************//
  IJwToken = interface(IDispatch)
    ['{7115952F-3B76-4CE1-B052-E4D38D4CCC09}']
    function Get_TokenType: LongWord; safecall;
    function Get_AccessMask: LongWord; safecall;
    procedure Set_AccessMask(Value: LongWord); safecall;
    function Get_Owner: IJwSid; safecall;
    procedure Set_Owner(const Value: IJwSid); safecall;
    function Get_TokenUser: IJwSid; safecall;
    function Get_Group: IJwSid; safecall;
    procedure Set_Group(const Value: IJwSid); safecall;
    function Get_Groups: IJwSidList; safecall;
    procedure Set_Groups(const Value: IJwSidList); safecall;
    function Get_IsRestricted: WordBool; safecall;
    function Get_SessionID: LongWord; safecall;
    procedure Set_SessionID(Value: LongWord); safecall;
    function Get_Privileges: IUnknown; safecall;
    procedure Set_Privileges(const Value: IUnknown); safecall;
    function Get_LinkedToken: IJwToken; safecall;
    function Get_IntegrityLevel: IJwSidList; safecall;
    function Get_DefaultDacl: IJwSecurityDescriptor; safecall;
    procedure Set_DefaultDacl(const Value: IJwSecurityDescriptor); safecall;
    function Get_ImpersonationLevel: LongWord; safecall;
    procedure InitByProcess(ProcessHandle: LongWord; DesiredAccess: LongWord; Duplicate: WordBool); safecall;
    procedure InitByThread(ThreadHandle: LongWord; DesiredAccess: LongWord; Duplicate: WordBool); safecall;
    procedure InitByEffective(DesiredAccess: LongWord); safecall;
    procedure InitByProcessId(ProcessId: LongWord; DesiredAccess: LongWord); safecall;
    procedure InitByDuplicateToken(const Token: IJwToken; DesiredAccess: LongWord); safecall;
    procedure InitWTSQueryUserToken(SessionID: LongWord); safecall;
    procedure InitWTSQueryUserTokenEx(Server: LongWord; SessionID: LongWord); safecall;
    procedure InitByCompatibilityQueryUserToken(DesiredAccess: LongWord; 
                                                const ProcessName: WideString); safecall;
    procedure InitRestricted(const PreviousToken: IJwToken; AccessMask: LongWord; Flags: LongWord; 
                             const SidsToDisable: IJwSidList; const PrivilegesToDisable: IUnknown; 
                             const RestrictedSids: IJwSidList); safecall;
    procedure InitLogonUser(const Name: WideString; const Domain: WideString; 
                            const Password: WideString; LogonType: LongWord; LogonProvider: LongWord); safecall;
    procedure ConvertToImpersonatedToken(ImpLevel: LongWord; DesiredAccess: LongWord); safecall;
    procedure ConvertToPrimaryToken(DesiredAccess: LongWord); safecall;
    procedure Method1; safecall;
    function Get_IntegrityLevelType: JwIntegrityLevelType; safecall;
    procedure Set_IntegrityLevelType(Value: JwIntegrityLevelType); safecall;
    function Get_VirtualizationAllowed: WordBool; safecall;
    function Get_VirtualiationEnabled: WordBool; safecall;
    function Get_MandatoryPolicy: LongWord; safecall;
    function Get_RunElevation: LongWord; safecall;
    function Get_ElevationType: LongWord; safecall;
    property TokenType: LongWord read Get_TokenType;
    property AccessMask: LongWord read Get_AccessMask write Set_AccessMask;
    property Owner: IJwSid read Get_Owner write Set_Owner;
    property TokenUser: IJwSid read Get_TokenUser;
    property Group: IJwSid read Get_Group write Set_Group;
    property Groups: IJwSidList read Get_Groups write Set_Groups;
    property IsRestricted: WordBool read Get_IsRestricted;
    property SessionID: LongWord read Get_SessionID write Set_SessionID;
    property Privileges: IUnknown read Get_Privileges write Set_Privileges;
    property LinkedToken: IJwToken read Get_LinkedToken;
    property IntegrityLevel: IJwSidList read Get_IntegrityLevel;
    property DefaultDacl: IJwSecurityDescriptor read Get_DefaultDacl write Set_DefaultDacl;
    property ImpersonationLevel: LongWord read Get_ImpersonationLevel;
    property IntegrityLevelType: JwIntegrityLevelType read Get_IntegrityLevelType write Set_IntegrityLevelType;
    property VirtualizationAllowed: WordBool read Get_VirtualizationAllowed;
    property VirtualiationEnabled: WordBool read Get_VirtualiationEnabled;
    property MandatoryPolicy: LongWord read Get_MandatoryPolicy;
    property RunElevation: LongWord read Get_RunElevation;
    property ElevationType: LongWord read Get_ElevationType;
  end;

// *********************************************************************//
// DispIntf:  IJwTokenDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7115952F-3B76-4CE1-B052-E4D38D4CCC09}
// *********************************************************************//
  IJwTokenDisp = dispinterface
    ['{7115952F-3B76-4CE1-B052-E4D38D4CCC09}']
    property TokenType: LongWord readonly dispid 201;
    property AccessMask: LongWord dispid 202;
    property Owner: IJwSid dispid 203;
    property TokenUser: IJwSid readonly dispid 204;
    property Group: IJwSid dispid 205;
    property Groups: IJwSidList dispid 206;
    property IsRestricted: WordBool readonly dispid 207;
    property SessionID: LongWord dispid 208;
    property Privileges: IUnknown dispid 209;
    property LinkedToken: IJwToken readonly dispid 210;
    property IntegrityLevel: IJwSidList readonly dispid 211;
    property DefaultDacl: IJwSecurityDescriptor dispid 213;
    property ImpersonationLevel: LongWord readonly dispid 214;
    procedure InitByProcess(ProcessHandle: LongWord; DesiredAccess: LongWord; Duplicate: WordBool); dispid 215;
    procedure InitByThread(ThreadHandle: LongWord; DesiredAccess: LongWord; Duplicate: WordBool); dispid 216;
    procedure InitByEffective(DesiredAccess: LongWord); dispid 217;
    procedure InitByProcessId(ProcessId: LongWord; DesiredAccess: LongWord); dispid 218;
    procedure InitByDuplicateToken(const Token: IJwToken; DesiredAccess: LongWord); dispid 219;
    procedure InitWTSQueryUserToken(SessionID: LongWord); dispid 220;
    procedure InitWTSQueryUserTokenEx(Server: LongWord; SessionID: LongWord); dispid 221;
    procedure InitByCompatibilityQueryUserToken(DesiredAccess: LongWord; 
                                                const ProcessName: WideString); dispid 222;
    procedure InitRestricted(const PreviousToken: IJwToken; AccessMask: LongWord; Flags: LongWord; 
                             const SidsToDisable: IJwSidList; const PrivilegesToDisable: IUnknown; 
                             const RestrictedSids: IJwSidList); dispid 223;
    procedure InitLogonUser(const Name: WideString; const Domain: WideString; 
                            const Password: WideString; LogonType: LongWord; LogonProvider: LongWord); dispid 224;
    procedure ConvertToImpersonatedToken(ImpLevel: LongWord; DesiredAccess: LongWord); dispid 225;
    procedure ConvertToPrimaryToken(DesiredAccess: LongWord); dispid 226;
    procedure Method1; dispid 227;
    property IntegrityLevelType: JwIntegrityLevelType dispid 228;
    property VirtualizationAllowed: WordBool readonly dispid 212;
    property VirtualiationEnabled: WordBool readonly dispid 229;
    property MandatoryPolicy: LongWord readonly dispid 230;
    property RunElevation: LongWord readonly dispid 231;
    property ElevationType: LongWord readonly dispid 232;
  end;

// *********************************************************************//
// Schnittstelle: IJwPrivilegeList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D7B09407-170C-4407-A51B-500F9479CC15}
// *********************************************************************//
  IJwPrivilegeList = interface(IDispatch)
    ['{D7B09407-170C-4407-A51B-500F9479CC15}']
    function Get__NewEnum: Integer; safecall;
    function Get_Item: IJwPrivilege; safecall;
    procedure ToString(var Value: WideString); safecall;
    procedure AddByName(const Name: WideString); safecall;
    procedure AddByLuid(HighValue: LongWord; LowValue: LongWord); safecall;
    procedure InitReadOnly; safecall;
    function Get_ReadOnly: WordBool; safecall;
    procedure DoneReadOnly; safecall;
    procedure DeleteByLuid(High: LongWord; Low: LongWord); safecall;
    procedure DeleteByName(const Name: WideString); safecall;
    procedure DisableAllPrivileges(Value: Integer); safecall;
    function Get_Control: LongWord; safecall;
    procedure Set_Control(Value: LongWord); safecall;
    property _NewEnum: Integer read Get__NewEnum;
    property Item: IJwPrivilege read Get_Item;
    property ReadOnly: WordBool read Get_ReadOnly;
    property Control: LongWord read Get_Control write Set_Control;
  end;

// *********************************************************************//
// DispIntf:  IJwPrivilegeListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D7B09407-170C-4407-A51B-500F9479CC15}
// *********************************************************************//
  IJwPrivilegeListDisp = dispinterface
    ['{D7B09407-170C-4407-A51B-500F9479CC15}']
    property _NewEnum: Integer readonly dispid -4;
    property Item: IJwPrivilege readonly dispid 202;
    procedure ToString(var Value: WideString); dispid 203;
    procedure AddByName(const Name: WideString); dispid 204;
    procedure AddByLuid(HighValue: LongWord; LowValue: LongWord); dispid 205;
    procedure InitReadOnly; dispid 206;
    property ReadOnly: WordBool readonly dispid 201;
    procedure DoneReadOnly; dispid 207;
    procedure DeleteByLuid(High: LongWord; Low: LongWord); dispid 208;
    procedure DeleteByName(const Name: WideString); dispid 209;
    procedure DisableAllPrivileges(Value: Integer); dispid 210;
    property Control: LongWord dispid 211;
  end;

// *********************************************************************//
// Schnittstelle: IJwPrivilege
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FCA8F64B-B76D-45B4-A010-E3F1134AE1A9}
// *********************************************************************//
  IJwPrivilege = interface(IDispatch)
    ['{FCA8F64B-B76D-45B4-A010-E3F1134AE1A9}']
    function Get_HiValue: LongWord; safecall;
    procedure Set_HiValue(Value: LongWord); safecall;
    function Get_LoValue: LongWord; safecall;
    procedure Set_LoValue(Value: LongWord); safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_ReadOnly: WordBool; safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    function Get_Attributes: LongWord; safecall;
    procedure Set_Attributes(Value: LongWord); safecall;
    property HiValue: LongWord read Get_HiValue write Set_HiValue;
    property LoValue: LongWord read Get_LoValue write Set_LoValue;
    property Name: WideString read Get_Name write Set_Name;
    property ReadOnly: WordBool read Get_ReadOnly;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property Attributes: LongWord read Get_Attributes write Set_Attributes;
  end;

// *********************************************************************//
// DispIntf:  IJwPrivilegeDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FCA8F64B-B76D-45B4-A010-E3F1134AE1A9}
// *********************************************************************//
  IJwPrivilegeDisp = dispinterface
    ['{FCA8F64B-B76D-45B4-A010-E3F1134AE1A9}']
    property HiValue: LongWord dispid 201;
    property LoValue: LongWord dispid 202;
    property Name: WideString dispid 203;
    property ReadOnly: WordBool readonly dispid 204;
    property Enabled: WordBool dispid 205;
    property Attributes: LongWord dispid 206;
  end;

// *********************************************************************//
// Schnittstelle: IJwWindowsVersion
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {632F08B7-7D3B-4865-B17A-00B97FBE3D9B}
// *********************************************************************//
  IJwWindowsVersion = interface(IDispatch)
    ['{632F08B7-7D3B-4865-B17A-00B97FBE3D9B}']
    function Get_WindowsType: SYSINT; safecall;
    function Get_IsWindows2000(OrHigher: WordBool): WordBool; safecall;
    function Get_IsWindows2003(OrHigher: WordBool): WordBool; safecall;
    function Get_IsWindows2003R2(OrHigher: WordBool): WordBool; safecall;
    function Get_IsWindowsXP(OrHigher: WordBool): WordBool; safecall;
    function Get_IsWindowsVista(OrHigher: WordBool): WordBool; safecall;
    function Get_IsWindows2008(OrHigher: WordBool): WordBool; safecall;
    function Get_IsServer: WordBool; safecall;
    function Get_IsTerminalServiceRunning: WordBool; safecall;
    property WindowsType: SYSINT read Get_WindowsType;
    property IsWindows2000[OrHigher: WordBool]: WordBool read Get_IsWindows2000;
    property IsWindows2003[OrHigher: WordBool]: WordBool read Get_IsWindows2003;
    property IsWindows2003R2[OrHigher: WordBool]: WordBool read Get_IsWindows2003R2;
    property IsWindowsXP[OrHigher: WordBool]: WordBool read Get_IsWindowsXP;
    property IsWindowsVista[OrHigher: WordBool]: WordBool read Get_IsWindowsVista;
    property IsWindows2008[OrHigher: WordBool]: WordBool read Get_IsWindows2008;
    property IsServer: WordBool read Get_IsServer;
    property IsTerminalServiceRunning: WordBool read Get_IsTerminalServiceRunning;
  end;

// *********************************************************************//
// DispIntf:  IJwWindowsVersionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {632F08B7-7D3B-4865-B17A-00B97FBE3D9B}
// *********************************************************************//
  IJwWindowsVersionDisp = dispinterface
    ['{632F08B7-7D3B-4865-B17A-00B97FBE3D9B}']
    property WindowsType: SYSINT readonly dispid 201;
    property IsWindows2000[OrHigher: WordBool]: WordBool readonly dispid 202;
    property IsWindows2003[OrHigher: WordBool]: WordBool readonly dispid 203;
    property IsWindows2003R2[OrHigher: WordBool]: WordBool readonly dispid 204;
    property IsWindowsXP[OrHigher: WordBool]: WordBool readonly dispid 205;
    property IsWindowsVista[OrHigher: WordBool]: WordBool readonly dispid 206;
    property IsWindows2008[OrHigher: WordBool]: WordBool readonly dispid 207;
    property IsServer: WordBool readonly dispid 208;
    property IsTerminalServiceRunning: WordBool readonly dispid 209;
  end;

// *********************************************************************//
// Schnittstelle: IJwGenericMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DD1015C5-9E4D-4062-8E0C-E2606705673A}
// *********************************************************************//
  IJwGenericMapping = interface(IDispatch)
    ['{DD1015C5-9E4D-4062-8E0C-E2606705673A}']
    function GetMapping: TGenericMapping; safecall;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; safecall;
    function GetAccessNames(out Count: LongWord): PChar; safecall;
  end;

// *********************************************************************//
// DispIntf:  IJwGenericMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DD1015C5-9E4D-4062-8E0C-E2606705673A}
// *********************************************************************//
  IJwGenericMappingDisp = dispinterface
    ['{DD1015C5-9E4D-4062-8E0C-E2606705673A}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwFileFolderMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B713F708-5096-4A0B-868C-BF0B5451E54A}
// *********************************************************************//
  IJwFileFolderMapping = interface(IJwGenericMapping)
    ['{B713F708-5096-4A0B-868C-BF0B5451E54A}']
  end;

// *********************************************************************//
// DispIntf:  IJwFileFolderMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B713F708-5096-4A0B-868C-BF0B5451E54A}
// *********************************************************************//
  IJwFileFolderMappingDisp = dispinterface
    ['{B713F708-5096-4A0B-868C-BF0B5451E54A}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwFileMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8E4EE90E-1864-475D-842E-C1FA9099EF0A}
// *********************************************************************//
  IJwFileMapping = interface(IJwGenericMapping)
    ['{8E4EE90E-1864-475D-842E-C1FA9099EF0A}']
  end;

// *********************************************************************//
// DispIntf:  IJwFileMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8E4EE90E-1864-475D-842E-C1FA9099EF0A}
// *********************************************************************//
  IJwFileMappingDisp = dispinterface
    ['{8E4EE90E-1864-475D-842E-C1FA9099EF0A}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwRegistryMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4213A33D-F495-4C8F-A6A3-7CDD5E626B5D}
// *********************************************************************//
  IJwRegistryMapping = interface(IJwGenericMapping)
    ['{4213A33D-F495-4C8F-A6A3-7CDD5E626B5D}']
  end;

// *********************************************************************//
// DispIntf:  IJwRegistryMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4213A33D-F495-4C8F-A6A3-7CDD5E626B5D}
// *********************************************************************//
  IJwRegistryMappingDisp = dispinterface
    ['{4213A33D-F495-4C8F-A6A3-7CDD5E626B5D}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwWinStationMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {AF3DA16B-BDD7-43A7-BA86-BAB06E05C848}
// *********************************************************************//
  IJwWinStationMapping = interface(IJwGenericMapping)
    ['{AF3DA16B-BDD7-43A7-BA86-BAB06E05C848}']
  end;

// *********************************************************************//
// DispIntf:  IJwWinStationMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {AF3DA16B-BDD7-43A7-BA86-BAB06E05C848}
// *********************************************************************//
  IJwWinStationMappingDisp = dispinterface
    ['{AF3DA16B-BDD7-43A7-BA86-BAB06E05C848}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwDesktopMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C389C422-764A-4FF1-B02B-7DBAC4A255DE}
// *********************************************************************//
  IJwDesktopMapping = interface(IJwGenericMapping)
    ['{C389C422-764A-4FF1-B02B-7DBAC4A255DE}']
  end;

// *********************************************************************//
// DispIntf:  IJwDesktopMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C389C422-764A-4FF1-B02B-7DBAC4A255DE}
// *********************************************************************//
  IJwDesktopMappingDisp = dispinterface
    ['{C389C422-764A-4FF1-B02B-7DBAC4A255DE}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwServiceMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D71696A4-36C1-4D75-9829-D470AD5B7623}
// *********************************************************************//
  IJwServiceMapping = interface(IJwGenericMapping)
    ['{D71696A4-36C1-4D75-9829-D470AD5B7623}']
  end;

// *********************************************************************//
// DispIntf:  IJwServiceMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D71696A4-36C1-4D75-9829-D470AD5B7623}
// *********************************************************************//
  IJwServiceMappingDisp = dispinterface
    ['{D71696A4-36C1-4D75-9829-D470AD5B7623}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwServiceManagerMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3714347D-12AA-424F-A525-254AD715EDD8}
// *********************************************************************//
  IJwServiceManagerMapping = interface(IJwGenericMapping)
    ['{3714347D-12AA-424F-A525-254AD715EDD8}']
  end;

// *********************************************************************//
// DispIntf:  IJwServiceManagerMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3714347D-12AA-424F-A525-254AD715EDD8}
// *********************************************************************//
  IJwServiceManagerMappingDisp = dispinterface
    ['{3714347D-12AA-424F-A525-254AD715EDD8}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwPrinterMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0BCBBD6C-1DBD-41D9-A2D4-DC9540F3ECEB}
// *********************************************************************//
  IJwPrinterMapping = interface(IJwGenericMapping)
    ['{0BCBBD6C-1DBD-41D9-A2D4-DC9540F3ECEB}']
  end;

// *********************************************************************//
// DispIntf:  IJwPrinterMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0BCBBD6C-1DBD-41D9-A2D4-DC9540F3ECEB}
// *********************************************************************//
  IJwPrinterMappingDisp = dispinterface
    ['{0BCBBD6C-1DBD-41D9-A2D4-DC9540F3ECEB}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwShareMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {794FDFE5-4A60-4394-BEAB-D2FCFEE0984C}
// *********************************************************************//
  IJwShareMapping = interface(IJwGenericMapping)
    ['{794FDFE5-4A60-4394-BEAB-D2FCFEE0984C}']
  end;

// *********************************************************************//
// DispIntf:  IJwShareMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {794FDFE5-4A60-4394-BEAB-D2FCFEE0984C}
// *********************************************************************//
  IJwShareMappingDisp = dispinterface
    ['{794FDFE5-4A60-4394-BEAB-D2FCFEE0984C}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwProcessMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {993CDB31-A24C-4EEA-BDD0-10BA1AB3D7E1}
// *********************************************************************//
  IJwProcessMapping = interface(IJwGenericMapping)
    ['{993CDB31-A24C-4EEA-BDD0-10BA1AB3D7E1}']
  end;

// *********************************************************************//
// DispIntf:  IJwProcessMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {993CDB31-A24C-4EEA-BDD0-10BA1AB3D7E1}
// *********************************************************************//
  IJwProcessMappingDisp = dispinterface
    ['{993CDB31-A24C-4EEA-BDD0-10BA1AB3D7E1}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwThreadMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {389BEEFC-7562-4E53-878E-6E3E61A510C0}
// *********************************************************************//
  IJwThreadMapping = interface(IJwGenericMapping)
    ['{389BEEFC-7562-4E53-878E-6E3E61A510C0}']
  end;

// *********************************************************************//
// DispIntf:  IJwThreadMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {389BEEFC-7562-4E53-878E-6E3E61A510C0}
// *********************************************************************//
  IJwThreadMappingDisp = dispinterface
    ['{389BEEFC-7562-4E53-878E-6E3E61A510C0}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwJobMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D5B64FC6-3A49-483C-9D7B-34A04E6E97C2}
// *********************************************************************//
  IJwJobMapping = interface(IJwGenericMapping)
    ['{D5B64FC6-3A49-483C-9D7B-34A04E6E97C2}']
  end;

// *********************************************************************//
// DispIntf:  IJwJobMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D5B64FC6-3A49-483C-9D7B-34A04E6E97C2}
// *********************************************************************//
  IJwJobMappingDisp = dispinterface
    ['{D5B64FC6-3A49-483C-9D7B-34A04E6E97C2}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwSemaphoreMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {28EEA5E2-21E7-4CB3-A585-6D4FF82BABB5}
// *********************************************************************//
  IJwSemaphoreMapping = interface(IJwGenericMapping)
    ['{28EEA5E2-21E7-4CB3-A585-6D4FF82BABB5}']
  end;

// *********************************************************************//
// DispIntf:  IJwSemaphoreMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {28EEA5E2-21E7-4CB3-A585-6D4FF82BABB5}
// *********************************************************************//
  IJwSemaphoreMappingDisp = dispinterface
    ['{28EEA5E2-21E7-4CB3-A585-6D4FF82BABB5}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwEventMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {64357431-6565-494D-83E5-0FAB085A9FF5}
// *********************************************************************//
  IJwEventMapping = interface(IJwGenericMapping)
    ['{64357431-6565-494D-83E5-0FAB085A9FF5}']
  end;

// *********************************************************************//
// DispIntf:  IJwEventMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {64357431-6565-494D-83E5-0FAB085A9FF5}
// *********************************************************************//
  IJwEventMappingDisp = dispinterface
    ['{64357431-6565-494D-83E5-0FAB085A9FF5}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwMutexMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F15F1B47-4DB8-4F6F-A362-51B88DA5C5C4}
// *********************************************************************//
  IJwMutexMapping = interface(IJwGenericMapping)
    ['{F15F1B47-4DB8-4F6F-A362-51B88DA5C5C4}']
  end;

// *********************************************************************//
// DispIntf:  IJwMutexMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F15F1B47-4DB8-4F6F-A362-51B88DA5C5C4}
// *********************************************************************//
  IJwMutexMappingDisp = dispinterface
    ['{F15F1B47-4DB8-4F6F-A362-51B88DA5C5C4}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwFileMapMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {991D7CAB-58A1-4575-9F3B-CC34D6E2DE00}
// *********************************************************************//
  IJwFileMapMapping = interface(IJwGenericMapping)
    ['{991D7CAB-58A1-4575-9F3B-CC34D6E2DE00}']
  end;

// *********************************************************************//
// DispIntf:  IJwFileMapMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {991D7CAB-58A1-4575-9F3B-CC34D6E2DE00}
// *********************************************************************//
  IJwFileMapMappingDisp = dispinterface
    ['{991D7CAB-58A1-4575-9F3B-CC34D6E2DE00}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwTimerMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {10F1574D-F097-4081-8DFD-F956581FBA2C}
// *********************************************************************//
  IJwTimerMapping = interface(IJwGenericMapping)
    ['{10F1574D-F097-4081-8DFD-F956581FBA2C}']
  end;

// *********************************************************************//
// DispIntf:  IJwTimerMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {10F1574D-F097-4081-8DFD-F956581FBA2C}
// *********************************************************************//
  IJwTimerMappingDisp = dispinterface
    ['{10F1574D-F097-4081-8DFD-F956581FBA2C}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwTokenMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8F1F8AF3-42EA-4E51-93B0-3C5E04B47C49}
// *********************************************************************//
  IJwTokenMapping = interface(IJwGenericMapping)
    ['{8F1F8AF3-42EA-4E51-93B0-3C5E04B47C49}']
  end;

// *********************************************************************//
// DispIntf:  IJwTokenMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8F1F8AF3-42EA-4E51-93B0-3C5E04B47C49}
// *********************************************************************//
  IJwTokenMappingDisp = dispinterface
    ['{8F1F8AF3-42EA-4E51-93B0-3C5E04B47C49}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwPipeMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {48260C48-B275-4C7C-BDB5-4678E1E89160}
// *********************************************************************//
  IJwPipeMapping = interface(IJwGenericMapping)
    ['{48260C48-B275-4C7C-BDB5-4678E1E89160}']
  end;

// *********************************************************************//
// DispIntf:  IJwPipeMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {48260C48-B275-4C7C-BDB5-4678E1E89160}
// *********************************************************************//
  IJwPipeMappingDisp = dispinterface
    ['{48260C48-B275-4C7C-BDB5-4678E1E89160}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwLogServer
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {213E956A-7BF3-4007-8191-1D743C818435}
// *********************************************************************//
  IJwLogServer = interface(IDispatch)
    ['{213E956A-7BF3-4007-8191-1D743C818435}']
    function Get_WriterClass: IJwWriterClass; safecall;
    procedure Set_WriterClass(const Value: IJwWriterClass); safecall;
    function Get_LogTypes: OleVariant; safecall;
    procedure Set_LogTypes(Value: OleVariant); safecall;
    function Connect(EnterType: JwEnumEnterType; const ClassName: WideString; 
                     const MethodName: WideString; const FileName: WideString; 
                     const LogMessage: WideString): IJwLogClient; safecall;
    property WriterClass: IJwWriterClass read Get_WriterClass write Set_WriterClass;
    property LogTypes: OleVariant read Get_LogTypes write Set_LogTypes;
  end;

// *********************************************************************//
// DispIntf:  IJwLogServerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {213E956A-7BF3-4007-8191-1D743C818435}
// *********************************************************************//
  IJwLogServerDisp = dispinterface
    ['{213E956A-7BF3-4007-8191-1D743C818435}']
    property WriterClass: IJwWriterClass dispid 201;
    property LogTypes: OleVariant dispid 202;
    function Connect(EnterType: JwEnumEnterType; const ClassName: WideString; 
                     const MethodName: WideString; const FileName: WideString; 
                     const LogMessage: WideString): IJwLogClient; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwWriterClass
// Flags:     (320) Dual OleAutomation
// GUID:      {E225CFE1-FCAB-4109-8D64-B845D0C8FDD5}
// *********************************************************************//
  IJwWriterClass = interface(IUnknown)
    ['{E225CFE1-FCAB-4109-8D64-B845D0C8FDD5}']
    function WriteSingleTag(IndentLevel: SYSINT; const TagName: WideString; 
                            const Value: WideString; Attributes: OleVariant): WideString; safecall;
    function StartWriteMultipleTags(IndentLevel: SYSINT; const TagName: WideString; 
                                    Attributes: OleVariant): WideString; safecall;
    function EndWriteMultipleTags: WideString; safecall;
    procedure Done; safecall;
    function CreateObject: IJwWriterClass; safecall;
  end;

// *********************************************************************//
// DispIntf:  IJwWriterClassDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {E225CFE1-FCAB-4109-8D64-B845D0C8FDD5}
// *********************************************************************//
  IJwWriterClassDisp = dispinterface
    ['{E225CFE1-FCAB-4109-8D64-B845D0C8FDD5}']
    function WriteSingleTag(IndentLevel: SYSINT; const TagName: WideString; 
                            const Value: WideString; Attributes: OleVariant): WideString; dispid 201;
    function StartWriteMultipleTags(IndentLevel: SYSINT; const TagName: WideString; 
                                    Attributes: OleVariant): WideString; dispid 202;
    function EndWriteMultipleTags: WideString; dispid 203;
    procedure Done; dispid 204;
    function CreateObject: IJwWriterClass; dispid 101;
  end;

// *********************************************************************//
// Schnittstelle: IJwLogClient
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {742092DA-9FD9-420A-84AF-56D6EA8508D4}
// *********************************************************************//
  IJwLogClient = interface(IDispatch)
    ['{742092DA-9FD9-420A-84AF-56D6EA8508D4}']
    procedure Log(LogType: JwEnumLogType; const LogMessage: WideString); safecall;
    procedure Signal(LogType: JwEnumSignalType; const Source: WideString; const Target: WideString; 
                     const LogMessage: WideString); safecall;
    procedure Memory(LogType: JwEnumMemoryType; const MemType: WideString; 
                     const LogMessage: WideString); safecall;
    procedure Exception(Exception: OleVariant); safecall;
  end;

// *********************************************************************//
// DispIntf:  IJwLogClientDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {742092DA-9FD9-420A-84AF-56D6EA8508D4}
// *********************************************************************//
  IJwLogClientDisp = dispinterface
    ['{742092DA-9FD9-420A-84AF-56D6EA8508D4}']
    procedure Log(LogType: JwEnumLogType; const LogMessage: WideString); dispid 201;
    procedure Signal(LogType: JwEnumSignalType; const Source: WideString; const Target: WideString; 
                     const LogMessage: WideString); dispid 202;
    procedure Memory(LogType: JwEnumMemoryType; const MemType: WideString; 
                     const LogMessage: WideString); dispid 203;
    procedure Exception(Exception: OleVariant); dispid 204;
  end;

// *********************************************************************//
// Schnittstelle: ITestInt
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D2B0D3F0-F029-4550-A7CC-07BC612BF10B}
// *********************************************************************//
  ITestInt = interface(IDispatch)
    ['{D2B0D3F0-F029-4550-A7CC-07BC612BF10B}']
  end;

// *********************************************************************//
// DispIntf:  ITestIntDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D2B0D3F0-F029-4550-A7CC-07BC612BF10B}
// *********************************************************************//
  ITestIntDisp = dispinterface
    ['{D2B0D3F0-F029-4550-A7CC-07BC612BF10B}']
  end;

// *********************************************************************//
// Schnittstelle: IJwXMLAttribute
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B7C4FFC9-32AF-4341-9434-4F33FEDF1707}
// *********************************************************************//
  IJwXMLAttribute = interface(IDispatch)
    ['{B7C4FFC9-32AF-4341-9434-4F33FEDF1707}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Value: WideString; safecall;
    procedure Set_Value(const Value: WideString); safecall;
    function ToString: WideString; safecall;
    property Name: WideString read Get_Name write Set_Name;
    property Value: WideString read Get_Value write Set_Value;
  end;

// *********************************************************************//
// DispIntf:  IJwXMLAttributeDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B7C4FFC9-32AF-4341-9434-4F33FEDF1707}
// *********************************************************************//
  IJwXMLAttributeDisp = dispinterface
    ['{B7C4FFC9-32AF-4341-9434-4F33FEDF1707}']
    property Name: WideString dispid 201;
    property Value: WideString dispid 202;
    function ToString: WideString; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwEventType
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B95E0B5E-F424-4AC4-98F0-B1C79C19AD11}
// *********************************************************************//
  IJwEventType = interface(IDispatch)
    ['{B95E0B5E-F424-4AC4-98F0-B1C79C19AD11}']
    function Get_TagName: JwXMLLogTag; safecall;
    procedure Set_TagName(Value: JwXMLLogTag); safecall;
    function Get_TypeValue: SYSINT; safecall;
    procedure Set_TypeValue(Value: SYSINT); safecall;
    function ToString: WideString; safecall;
    property TagName: JwXMLLogTag read Get_TagName write Set_TagName;
    property TypeValue: SYSINT read Get_TypeValue write Set_TypeValue;
  end;

// *********************************************************************//
// DispIntf:  IJwEventTypeDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B95E0B5E-F424-4AC4-98F0-B1C79C19AD11}
// *********************************************************************//
  IJwEventTypeDisp = dispinterface
    ['{B95E0B5E-F424-4AC4-98F0-B1C79C19AD11}']
    property TagName: JwXMLLogTag dispid 201;
    property TypeValue: SYSINT dispid 202;
    function ToString: WideString; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwEncryptionApi
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4353CD92-7CD8-4D32-B9B4-DC920AC0751C}
// *********************************************************************//
  IJwEncryptionApi = interface(IDispatch)
    ['{4353CD92-7CD8-4D32-B9B4-DC920AC0751C}']
  end;

// *********************************************************************//
// DispIntf:  IJwEncryptionApiDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4353CD92-7CD8-4D32-B9B4-DC920AC0751C}
// *********************************************************************//
  IJwEncryptionApiDisp = dispinterface
    ['{4353CD92-7CD8-4D32-B9B4-DC920AC0751C}']
  end;

// *********************************************************************//
// Schnittstelle: IJwEncryptMemory
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5A177321-4491-4CFA-950F-5CB6B362604A}
// *********************************************************************//
  IJwEncryptMemory = interface(IDispatch)
    ['{5A177321-4491-4CFA-950F-5CB6B362604A}']
  end;

// *********************************************************************//
// DispIntf:  IJwEncryptMemoryDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5A177321-4491-4CFA-950F-5CB6B362604A}
// *********************************************************************//
  IJwEncryptMemoryDisp = dispinterface
    ['{5A177321-4491-4CFA-950F-5CB6B362604A}']
  end;

// *********************************************************************//
// Schnittstelle: IJwEncryptData
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {895D37EF-F714-405C-BAE6-D769CF71EFF2}
// *********************************************************************//
  IJwEncryptData = interface(IDispatch)
    ['{895D37EF-F714-405C-BAE6-D769CF71EFF2}']
  end;

// *********************************************************************//
// DispIntf:  IJwEncryptDataDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {895D37EF-F714-405C-BAE6-D769CF71EFF2}
// *********************************************************************//
  IJwEncryptDataDisp = dispinterface
    ['{895D37EF-F714-405C-BAE6-D769CF71EFF2}']
  end;

// *********************************************************************//
// Schnittstelle: IJwRandomDataGenerator
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {ECCDCFF9-E076-4F91-8650-BA249C1B9389}
// *********************************************************************//
  IJwRandomDataGenerator = interface(IDispatch)
    ['{ECCDCFF9-E076-4F91-8650-BA249C1B9389}']
  end;

// *********************************************************************//
// DispIntf:  IJwRandomDataGeneratorDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {ECCDCFF9-E076-4F91-8650-BA249C1B9389}
// *********************************************************************//
  IJwRandomDataGeneratorDisp = dispinterface
    ['{ECCDCFF9-E076-4F91-8650-BA249C1B9389}']
  end;

// *********************************************************************//
// Schnittstelle: IJwWindowStation
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {78BA7899-943D-4A41-9727-661E618323B6}
// *********************************************************************//
  IJwWindowStation = interface(IDispatch)
    ['{78BA7899-943D-4A41-9727-661E618323B6}']
  end;

// *********************************************************************//
// DispIntf:  IJwWindowStationDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {78BA7899-943D-4A41-9727-661E618323B6}
// *********************************************************************//
  IJwWindowStationDisp = dispinterface
    ['{78BA7899-943D-4A41-9727-661E618323B6}']
  end;

// *********************************************************************//
// Schnittstelle: IJwWindowStations
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D97DA30D-F731-4500-ACAB-D3CEE4C17EAC}
// *********************************************************************//
  IJwWindowStations = interface(IDispatch)
    ['{D97DA30D-F731-4500-ACAB-D3CEE4C17EAC}']
  end;

// *********************************************************************//
// DispIntf:  IJwWindowStationsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D97DA30D-F731-4500-ACAB-D3CEE4C17EAC}
// *********************************************************************//
  IJwWindowStationsDisp = dispinterface
    ['{D97DA30D-F731-4500-ACAB-D3CEE4C17EAC}']
  end;

// *********************************************************************//
// Schnittstelle: IJwDesktop
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CF634589-D54D-42C7-B1BD-623BBD8CF556}
// *********************************************************************//
  IJwDesktop = interface(IDispatch)
    ['{CF634589-D54D-42C7-B1BD-623BBD8CF556}']
  end;

// *********************************************************************//
// DispIntf:  IJwDesktopDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CF634589-D54D-42C7-B1BD-623BBD8CF556}
// *********************************************************************//
  IJwDesktopDisp = dispinterface
    ['{CF634589-D54D-42C7-B1BD-623BBD8CF556}']
  end;

// *********************************************************************//
// Schnittstelle: IJwDesktops
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6583CE41-64A2-4A2A-9BD5-5F4C4F90D21A}
// *********************************************************************//
  IJwDesktops = interface(IDispatch)
    ['{6583CE41-64A2-4A2A-9BD5-5F4C4F90D21A}']
  end;

// *********************************************************************//
// DispIntf:  IJwDesktopsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6583CE41-64A2-4A2A-9BD5-5F4C4F90D21A}
// *********************************************************************//
  IJwDesktopsDisp = dispinterface
    ['{6583CE41-64A2-4A2A-9BD5-5F4C4F90D21A}']
  end;

// *********************************************************************//
// Schnittstelle: IJwCredentialsPrompt
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {270A0E83-2D25-43A5-936F-C883AD0DAF05}
// *********************************************************************//
  IJwCredentialsPrompt = interface(IDispatch)
    ['{270A0E83-2D25-43A5-936F-C883AD0DAF05}']
  end;

// *********************************************************************//
// DispIntf:  IJwCredentialsPromptDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {270A0E83-2D25-43A5-936F-C883AD0DAF05}
// *********************************************************************//
  IJwCredentialsPromptDisp = dispinterface
    ['{270A0E83-2D25-43A5-936F-C883AD0DAF05}']
  end;

// *********************************************************************//
// Schnittstelle: IJwCredentialsTools
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C0605281-F33C-4B11-AFC3-A3D0B2CF633D}
// *********************************************************************//
  IJwCredentialsTools = interface(IDispatch)
    ['{C0605281-F33C-4B11-AFC3-A3D0B2CF633D}']
  end;

// *********************************************************************//
// DispIntf:  IJwCredentialsToolsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C0605281-F33C-4B11-AFC3-A3D0B2CF633D}
// *********************************************************************//
  IJwCredentialsToolsDisp = dispinterface
    ['{C0605281-F33C-4B11-AFC3-A3D0B2CF633D}']
  end;

// *********************************************************************//
// Schnittstelle: IJwCryptProvider
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1862AB82-BFAB-40CA-9FB7-DBB05CFE9605}
// *********************************************************************//
  IJwCryptProvider = interface(IDispatch)
    ['{1862AB82-BFAB-40CA-9FB7-DBB05CFE9605}']
  end;

// *********************************************************************//
// DispIntf:  IJwCryptProviderDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1862AB82-BFAB-40CA-9FB7-DBB05CFE9605}
// *********************************************************************//
  IJwCryptProviderDisp = dispinterface
    ['{1862AB82-BFAB-40CA-9FB7-DBB05CFE9605}']
  end;

// *********************************************************************//
// Schnittstelle: IJwHash
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F4114A63-2B58-4560-B848-70EEA580BE7D}
// *********************************************************************//
  IJwHash = interface(IDispatch)
    ['{F4114A63-2B58-4560-B848-70EEA580BE7D}']
  end;

// *********************************************************************//
// DispIntf:  IJwHashDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F4114A63-2B58-4560-B848-70EEA580BE7D}
// *********************************************************************//
  IJwHashDisp = dispinterface
    ['{F4114A63-2B58-4560-B848-70EEA580BE7D}']
  end;

// *********************************************************************//
// Schnittstelle: IJwCryptKey
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B7517765-DF7D-4407-83A5-D41C8E556988}
// *********************************************************************//
  IJwCryptKey = interface(IDispatch)
    ['{B7517765-DF7D-4407-83A5-D41C8E556988}']
  end;

// *********************************************************************//
// DispIntf:  IJwCryptKeyDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B7517765-DF7D-4407-83A5-D41C8E556988}
// *********************************************************************//
  IJwCryptKeyDisp = dispinterface
    ['{B7517765-DF7D-4407-83A5-D41C8E556988}']
  end;

// *********************************************************************//
// Schnittstelle: IJwImpersonation
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CB4ABE99-055F-4BBD-9676-2B2211DE1F1B}
// *********************************************************************//
  IJwImpersonation = interface(IDispatch)
    ['{CB4ABE99-055F-4BBD-9676-2B2211DE1F1B}']
  end;

// *********************************************************************//
// DispIntf:  IJwImpersonationDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CB4ABE99-055F-4BBD-9676-2B2211DE1F1B}
// *********************************************************************//
  IJwImpersonationDisp = dispinterface
    ['{CB4ABE99-055F-4BBD-9676-2B2211DE1F1B}']
  end;

// *********************************************************************//
// Schnittstelle: IJwKnownSIDs
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {396C38B0-7628-424B-975A-98091E1EF70D}
// *********************************************************************//
  IJwKnownSIDs = interface(IDispatch)
    ['{396C38B0-7628-424B-975A-98091E1EF70D}']
  end;

// *********************************************************************//
// DispIntf:  IJwKnownSIDsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {396C38B0-7628-424B-975A-98091E1EF70D}
// *********************************************************************//
  IJwKnownSIDsDisp = dispinterface
    ['{396C38B0-7628-424B-975A-98091E1EF70D}']
  end;

// *********************************************************************//
// Schnittstelle: IJwThreadUserSid
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {30DB5A90-30C0-44E8-B248-CAD294BC3765}
// *********************************************************************//
  IJwThreadUserSid = interface(IDispatch)
    ['{30DB5A90-30C0-44E8-B248-CAD294BC3765}']
  end;

// *********************************************************************//
// DispIntf:  IJwThreadUserSidDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {30DB5A90-30C0-44E8-B248-CAD294BC3765}
// *********************************************************************//
  IJwThreadUserSidDisp = dispinterface
    ['{30DB5A90-30C0-44E8-B248-CAD294BC3765}']
  end;

// *********************************************************************//
// Schnittstelle: IJwLSA
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3A2CBCD1-F335-4F05-9A4F-8424C6471994}
// *********************************************************************//
  IJwLSA = interface(IDispatch)
    ['{3A2CBCD1-F335-4F05-9A4F-8424C6471994}']
  end;

// *********************************************************************//
// DispIntf:  IJwLSADisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3A2CBCD1-F335-4F05-9A4F-8424C6471994}
// *********************************************************************//
  IJwLSADisp = dispinterface
    ['{3A2CBCD1-F335-4F05-9A4F-8424C6471994}']
  end;

// *********************************************************************//
// Schnittstelle: IJwLSALogonSession
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {15EC2F0C-301D-417D-8814-5555FA1DAB3B}
// *********************************************************************//
  IJwLSALogonSession = interface(IDispatch)
    ['{15EC2F0C-301D-417D-8814-5555FA1DAB3B}']
  end;

// *********************************************************************//
// DispIntf:  IJwLSALogonSessionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {15EC2F0C-301D-417D-8814-5555FA1DAB3B}
// *********************************************************************//
  IJwLSALogonSessionDisp = dispinterface
    ['{15EC2F0C-301D-417D-8814-5555FA1DAB3B}']
  end;

// *********************************************************************//
// Schnittstelle: IJwLSALogonSessionData
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8D30540B-8656-4427-A459-08B3B9873070}
// *********************************************************************//
  IJwLSALogonSessionData = interface(IDispatch)
    ['{8D30540B-8656-4427-A459-08B3B9873070}']
  end;

// *********************************************************************//
// DispIntf:  IJwLSALogonSessionDataDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8D30540B-8656-4427-A459-08B3B9873070}
// *********************************************************************//
  IJwLSALogonSessionDataDisp = dispinterface
    ['{8D30540B-8656-4427-A459-08B3B9873070}']
  end;

// *********************************************************************//
// Schnittstelle: IJwPrivilegeScope
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8CD86140-4C99-4F30-8DA9-826CCD70B379}
// *********************************************************************//
  IJwPrivilegeScope = interface(IDispatch)
    ['{8CD86140-4C99-4F30-8DA9-826CCD70B379}']
  end;

// *********************************************************************//
// DispIntf:  IJwPrivilegeScopeDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8CD86140-4C99-4F30-8DA9-826CCD70B379}
// *********************************************************************//
  IJwPrivilegeScopeDisp = dispinterface
    ['{8CD86140-4C99-4F30-8DA9-826CCD70B379}']
  end;

// *********************************************************************//
// Schnittstelle: IJwProcessUtils
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D5D2ED6A-F5EF-4935-AD4C-643FEFF06C6F}
// *********************************************************************//
  IJwProcessUtils = interface(IDispatch)
    ['{D5D2ED6A-F5EF-4935-AD4C-643FEFF06C6F}']
  end;

// *********************************************************************//
// DispIntf:  IJwProcessUtilsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D5D2ED6A-F5EF-4935-AD4C-643FEFF06C6F}
// *********************************************************************//
  IJwProcessUtilsDisp = dispinterface
    ['{D5D2ED6A-F5EF-4935-AD4C-643FEFF06C6F}']
  end;

// *********************************************************************//
// Schnittstelle: IJwSecureBaseClass
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {803DBAF9-3F7B-416F-AC5B-C55C8637ED38}
// *********************************************************************//
  IJwSecureBaseClass = interface(IDispatch)
    ['{803DBAF9-3F7B-416F-AC5B-C55C8637ED38}']
  end;

// *********************************************************************//
// DispIntf:  IJwSecureBaseClassDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {803DBAF9-3F7B-416F-AC5B-C55C8637ED38}
// *********************************************************************//
  IJwSecureBaseClassDisp = dispinterface
    ['{803DBAF9-3F7B-416F-AC5B-C55C8637ED38}']
  end;

// *********************************************************************//
// Schnittstelle: IJwSecureFileObject
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5D361B6A-ABEC-4B78-83E4-DF3944055E8C}
// *********************************************************************//
  IJwSecureFileObject = interface(IJwSecureBaseClass)
    ['{5D361B6A-ABEC-4B78-83E4-DF3944055E8C}']
  end;

// *********************************************************************//
// DispIntf:  IJwSecureFileObjectDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5D361B6A-ABEC-4B78-83E4-DF3944055E8C}
// *********************************************************************//
  IJwSecureFileObjectDisp = dispinterface
    ['{5D361B6A-ABEC-4B78-83E4-DF3944055E8C}']
  end;

// *********************************************************************//
// Schnittstelle: IJwSecureRegistryKey
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0762D432-FF95-400E-B80E-19ABC879B9D7}
// *********************************************************************//
  IJwSecureRegistryKey = interface(IJwSecureBaseClass)
    ['{0762D432-FF95-400E-B80E-19ABC879B9D7}']
  end;

// *********************************************************************//
// DispIntf:  IJwSecureRegistryKeyDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0762D432-FF95-400E-B80E-19ABC879B9D7}
// *********************************************************************//
  IJwSecureRegistryKeyDisp = dispinterface
    ['{0762D432-FF95-400E-B80E-19ABC879B9D7}']
  end;

// *********************************************************************//
// Schnittstelle: IJwProgressCallback
// Flags:     (256) OleAutomation
// GUID:      {D3989545-CA0C-4883-941B-2AF9A6F54D6B}
// *********************************************************************//
  IJwProgressCallback = interface(IUnknown)
    ['{D3989545-CA0C-4883-941B-2AF9A6F54D6B}']
  end;

// *********************************************************************//
// Schnittstelle: IJwAuthZAccessRequest
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C197E5D5-73F6-4658-904F-9E2AB42984E2}
// *********************************************************************//
  IJwAuthZAccessRequest = interface(IDispatch)
    ['{C197E5D5-73F6-4658-904F-9E2AB42984E2}']
  end;

// *********************************************************************//
// DispIntf:  IJwAuthZAccessRequestDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C197E5D5-73F6-4658-904F-9E2AB42984E2}
// *********************************************************************//
  IJwAuthZAccessRequestDisp = dispinterface
    ['{C197E5D5-73F6-4658-904F-9E2AB42984E2}']
  end;

// *********************************************************************//
// Schnittstelle: IJwAuthZAccessReply
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {89269169-0B57-4E14-81F3-84AC738BFBD3}
// *********************************************************************//
  IJwAuthZAccessReply = interface(IDispatch)
    ['{89269169-0B57-4E14-81F3-84AC738BFBD3}']
  end;

// *********************************************************************//
// DispIntf:  IJwAuthZAccessReplyDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {89269169-0B57-4E14-81F3-84AC738BFBD3}
// *********************************************************************//
  IJwAuthZAccessReplyDisp = dispinterface
    ['{89269169-0B57-4E14-81F3-84AC738BFBD3}']
  end;

// *********************************************************************//
// Schnittstelle: IJwAuthResourceManager
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {328A999D-6055-4DEB-A091-1B2A299F5238}
// *********************************************************************//
  IJwAuthResourceManager = interface(IDispatch)
    ['{328A999D-6055-4DEB-A091-1B2A299F5238}']
  end;

// *********************************************************************//
// DispIntf:  IJwAuthResourceManagerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {328A999D-6055-4DEB-A091-1B2A299F5238}
// *********************************************************************//
  IJwAuthResourceManagerDisp = dispinterface
    ['{328A999D-6055-4DEB-A091-1B2A299F5238}']
  end;

// *********************************************************************//
// Schnittstelle: IJwAuthContext
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7955406B-58EC-4ACF-9ADC-BD3CCA2FD379}
// *********************************************************************//
  IJwAuthContext = interface(IDispatch)
    ['{7955406B-58EC-4ACF-9ADC-BD3CCA2FD379}']
  end;

// *********************************************************************//
// DispIntf:  IJwAuthContextDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7955406B-58EC-4ACF-9ADC-BD3CCA2FD379}
// *********************************************************************//
  IJwAuthContextDisp = dispinterface
    ['{7955406B-58EC-4ACF-9ADC-BD3CCA2FD379}']
  end;

// *********************************************************************//
// Schnittstelle: IJwTerminalServer
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DA19C375-048E-40BA-99CC-F7DD98BFD76F}
// *********************************************************************//
  IJwTerminalServer = interface(IDispatch)
    ['{DA19C375-048E-40BA-99CC-F7DD98BFD76F}']
  end;

// *********************************************************************//
// DispIntf:  IJwTerminalServerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DA19C375-048E-40BA-99CC-F7DD98BFD76F}
// *********************************************************************//
  IJwTerminalServerDisp = dispinterface
    ['{DA19C375-048E-40BA-99CC-F7DD98BFD76F}']
  end;

// *********************************************************************//
// Schnittstelle: IJwTerminalServerList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F085C70B-5BCA-4EBF-A8AA-D4551C17CEF7}
// *********************************************************************//
  IJwTerminalServerList = interface(IDispatch)
    ['{F085C70B-5BCA-4EBF-A8AA-D4551C17CEF7}']
  end;

// *********************************************************************//
// DispIntf:  IJwTerminalServerListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F085C70B-5BCA-4EBF-A8AA-D4551C17CEF7}
// *********************************************************************//
  IJwTerminalServerListDisp = dispinterface
    ['{F085C70B-5BCA-4EBF-A8AA-D4551C17CEF7}']
  end;

// *********************************************************************//
// Schnittstelle: IJwWTSSession
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D512987D-F6DF-4B01-87FF-0611607FCD9C}
// *********************************************************************//
  IJwWTSSession = interface(IDispatch)
    ['{D512987D-F6DF-4B01-87FF-0611607FCD9C}']
  end;

// *********************************************************************//
// DispIntf:  IJwWTSSessionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D512987D-F6DF-4B01-87FF-0611607FCD9C}
// *********************************************************************//
  IJwWTSSessionDisp = dispinterface
    ['{D512987D-F6DF-4B01-87FF-0611607FCD9C}']
  end;

// *********************************************************************//
// Schnittstelle: IJwWTSSessionList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C22216FB-9BC7-4707-A28C-D529492590C4}
// *********************************************************************//
  IJwWTSSessionList = interface(IDispatch)
    ['{C22216FB-9BC7-4707-A28C-D529492590C4}']
  end;

// *********************************************************************//
// DispIntf:  IJwWTSSessionListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C22216FB-9BC7-4707-A28C-D529492590C4}
// *********************************************************************//
  IJwWTSSessionListDisp = dispinterface
    ['{C22216FB-9BC7-4707-A28C-D529492590C4}']
  end;

// *********************************************************************//
// Schnittstelle: IJwWTSProcess
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B396DC6E-BEE7-4254-B4C8-A6101682685C}
// *********************************************************************//
  IJwWTSProcess = interface(IDispatch)
    ['{B396DC6E-BEE7-4254-B4C8-A6101682685C}']
  end;

// *********************************************************************//
// DispIntf:  IJwWTSProcessDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B396DC6E-BEE7-4254-B4C8-A6101682685C}
// *********************************************************************//
  IJwWTSProcessDisp = dispinterface
    ['{B396DC6E-BEE7-4254-B4C8-A6101682685C}']
  end;

// *********************************************************************//
// Schnittstelle: IJwWTSProcessList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B9E2DB7A-C45C-41F1-9323-8115BA49CAC1}
// *********************************************************************//
  IJwWTSProcessList = interface(IDispatch)
    ['{B9E2DB7A-C45C-41F1-9323-8115BA49CAC1}']
  end;

// *********************************************************************//
// DispIntf:  IJwWTSProcessListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B9E2DB7A-C45C-41F1-9323-8115BA49CAC1}
// *********************************************************************//
  IJwWTSProcessListDisp = dispinterface
    ['{B9E2DB7A-C45C-41F1-9323-8115BA49CAC1}']
  end;

// *********************************************************************//
// Schnittstelle: IJwWTSSessionShadow
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EA1DC736-38D8-4F63-B6DD-3AD9ED0208D0}
// *********************************************************************//
  IJwWTSSessionShadow = interface(IDispatch)
    ['{EA1DC736-38D8-4F63-B6DD-3AD9ED0208D0}']
  end;

// *********************************************************************//
// DispIntf:  IJwWTSSessionShadowDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EA1DC736-38D8-4F63-B6DD-3AD9ED0208D0}
// *********************************************************************//
  IJwWTSSessionShadowDisp = dispinterface
    ['{EA1DC736-38D8-4F63-B6DD-3AD9ED0208D0}']
  end;

// *********************************************************************//
// Schnittstelle: IJwSecurityDescriptorDialogCallback
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BA33738E-2D3E-4214-BAC6-DD044DB5697C}
// *********************************************************************//
  IJwSecurityDescriptorDialogCallback = interface(IDispatch)
    ['{BA33738E-2D3E-4214-BAC6-DD044DB5697C}']
  end;

// *********************************************************************//
// DispIntf:  IJwSecurityDescriptorDialogCallbackDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BA33738E-2D3E-4214-BAC6-DD044DB5697C}
// *********************************************************************//
  IJwSecurityDescriptorDialogCallbackDisp = dispinterface
    ['{BA33738E-2D3E-4214-BAC6-DD044DB5697C}']
  end;

// *********************************************************************//
// Schnittstelle: IJwInheritTypeList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B6FBCBDC-7941-414F-9D8E-464BA97FB183}
// *********************************************************************//
  IJwInheritTypeList = interface(IDispatch)
    ['{B6FBCBDC-7941-414F-9D8E-464BA97FB183}']
  end;

// *********************************************************************//
// DispIntf:  IJwInheritTypeListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B6FBCBDC-7941-414F-9D8E-464BA97FB183}
// *********************************************************************//
  IJwInheritTypeListDisp = dispinterface
    ['{B6FBCBDC-7941-414F-9D8E-464BA97FB183}']
  end;

// *********************************************************************//
// Schnittstelle: IJwTerminalServerCallback
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C7387934-4900-44DD-A4A1-449A3C0564BD}
// *********************************************************************//
  IJwTerminalServerCallback = interface(IDispatch)
    ['{C7387934-4900-44DD-A4A1-449A3C0564BD}']
  end;

// *********************************************************************//
// DispIntf:  IJwTerminalServerCallbackDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C7387934-4900-44DD-A4A1-449A3C0564BD}
// *********************************************************************//
  IJwTerminalServerCallbackDisp = dispinterface
    ['{C7387934-4900-44DD-A4A1-449A3C0564BD}']
  end;

// *********************************************************************//
// Schnittstelle: IJwUtils
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8FAE3B6B-F0BC-4F41-999B-8CC35AB90BE1}
// *********************************************************************//
  IJwUtils = interface(IDispatch)
    ['{8FAE3B6B-F0BC-4F41-999B-8CC35AB90BE1}']
  end;

// *********************************************************************//
// DispIntf:  IJwUtilsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8FAE3B6B-F0BC-4F41-999B-8CC35AB90BE1}
// *********************************************************************//
  IJwUtilsDisp = dispinterface
    ['{8FAE3B6B-F0BC-4F41-999B-8CC35AB90BE1}']
  end;

// *********************************************************************//
// Schnittstelle: IJwElevationClassFactory
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BDD24ED3-5200-4970-9B63-9965AF7F121F}
// *********************************************************************//
  IJwElevationClassFactory = interface(IDispatch)
    ['{BDD24ED3-5200-4970-9B63-9965AF7F121F}']
  end;

// *********************************************************************//
// DispIntf:  IJwElevationClassFactoryDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BDD24ED3-5200-4970-9B63-9965AF7F121F}
// *********************************************************************//
  IJwElevationClassFactoryDisp = dispinterface
    ['{BDD24ED3-5200-4970-9B63-9965AF7F121F}']
  end;

// *********************************************************************//
// Schnittstelle: IJwExplicitAccessArray
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {381ADFDA-CF08-4FE8-B660-78588184524A}
// *********************************************************************//
  IJwExplicitAccessArray = interface(IDispatch)
    ['{381ADFDA-CF08-4FE8-B660-78588184524A}']
  end;

// *********************************************************************//
// DispIntf:  IJwExplicitAccessArrayDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {381ADFDA-CF08-4FE8-B660-78588184524A}
// *********************************************************************//
  IJwExplicitAccessArrayDisp = dispinterface
    ['{381ADFDA-CF08-4FE8-B660-78588184524A}']
  end;

// *********************************************************************//
// Schnittstelle: IJwExplicitAccess
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {45379DC8-AC94-41DC-917E-4716063E1F38}
// *********************************************************************//
  IJwExplicitAccess = interface(IDispatch)
    ['{45379DC8-AC94-41DC-917E-4716063E1F38}']
  end;

// *********************************************************************//
// DispIntf:  IJwExplicitAccessDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {45379DC8-AC94-41DC-917E-4716063E1F38}
// *********************************************************************//
  IJwExplicitAccessDisp = dispinterface
    ['{45379DC8-AC94-41DC-917E-4716063E1F38}']
  end;

// *********************************************************************//
// Schnittstelle: IJwGuidArray
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {039206C7-F45F-40E0-A942-D7CA7A1E65DE}
// *********************************************************************//
  IJwGuidArray = interface(IDispatch)
    ['{039206C7-F45F-40E0-A942-D7CA7A1E65DE}']
  end;

// *********************************************************************//
// DispIntf:  IJwGuidArrayDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {039206C7-F45F-40E0-A942-D7CA7A1E65DE}
// *********************************************************************//
  IJwGuidArrayDisp = dispinterface
    ['{039206C7-F45F-40E0-A942-D7CA7A1E65DE}']
  end;

// *********************************************************************//
// Schnittstelle: IJwInheritedFromRecord
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DDFAF8BC-C644-4FBB-B5FF-041E1220F166}
// *********************************************************************//
  IJwInheritedFromRecord = interface(IDispatch)
    ['{DDFAF8BC-C644-4FBB-B5FF-041E1220F166}']
  end;

// *********************************************************************//
// DispIntf:  IJwInheritedFromRecordDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DDFAF8BC-C644-4FBB-B5FF-041E1220F166}
// *********************************************************************//
  IJwInheritedFromRecordDisp = dispinterface
    ['{DDFAF8BC-C644-4FBB-B5FF-041E1220F166}']
  end;

// *********************************************************************//
// Schnittstelle: IJwInheritedFromArray
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {174EA097-20D7-4D69-B37D-79AB8A182DDA}
// *********************************************************************//
  IJwInheritedFromArray = interface(IDispatch)
    ['{174EA097-20D7-4D69-B37D-79AB8A182DDA}']
  end;

// *********************************************************************//
// DispIntf:  IJwInheritedFromArrayDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {174EA097-20D7-4D69-B37D-79AB8A182DDA}
// *********************************************************************//
  IJwInheritedFromArrayDisp = dispinterface
    ['{174EA097-20D7-4D69-B37D-79AB8A182DDA}']
  end;

// *********************************************************************//
// Schnittstelle: IJwRootTuple
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {650D81E3-14D6-4EEF-B13D-3333FEFD6CAF}
// *********************************************************************//
  IJwRootTuple = interface(IDispatch)
    ['{650D81E3-14D6-4EEF-B13D-3333FEFD6CAF}']
  end;

// *********************************************************************//
// DispIntf:  IJwRootTupleDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {650D81E3-14D6-4EEF-B13D-3333FEFD6CAF}
// *********************************************************************//
  IJwRootTupleDisp = dispinterface
    ['{650D81E3-14D6-4EEF-B13D-3333FEFD6CAF}']
  end;

// *********************************************************************//
// Schnittstelle: IJwKeyRootTupleArray
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {89D1C10C-47E5-44D3-A9FC-48A99C565DA3}
// *********************************************************************//
  IJwKeyRootTupleArray = interface(IDispatch)
    ['{89D1C10C-47E5-44D3-A9FC-48A99C565DA3}']
  end;

// *********************************************************************//
// DispIntf:  IJwKeyRootTupleArrayDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {89D1C10C-47E5-44D3-A9FC-48A99C565DA3}
// *********************************************************************//
  IJwKeyRootTupleArrayDisp = dispinterface
    ['{89D1C10C-47E5-44D3-A9FC-48A99C565DA3}']
  end;

// *********************************************************************//
// Schnittstelle: IJwAccessMaskArray
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EE079D32-8114-495A-A2B3-5449F12F0A8B}
// *********************************************************************//
  IJwAccessMaskArray = interface(IDispatch)
    ['{EE079D32-8114-495A-A2B3-5449F12F0A8B}']
  end;

// *********************************************************************//
// DispIntf:  IJwAccessMaskArrayDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EE079D32-8114-495A-A2B3-5449F12F0A8B}
// *********************************************************************//
  IJwAccessMaskArrayDisp = dispinterface
    ['{EE079D32-8114-495A-A2B3-5449F12F0A8B}']
  end;

// *********************************************************************//
// Schnittstelle: IJwObjectTypeList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {567A8ADD-41D4-4808-A982-620DA1313B75}
// *********************************************************************//
  IJwObjectTypeList = interface(IDispatch)
    ['{567A8ADD-41D4-4808-A982-620DA1313B75}']
  end;

// *********************************************************************//
// DispIntf:  IJwObjectTypeListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {567A8ADD-41D4-4808-A982-620DA1313B75}
// *********************************************************************//
  IJwObjectTypeListDisp = dispinterface
    ['{567A8ADD-41D4-4808-A982-620DA1313B75}']
  end;

// *********************************************************************//
// Schnittstelle: IJwObjectTypeListArray
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F395722F-3303-489D-AA27-A04944AC2456}
// *********************************************************************//
  IJwObjectTypeListArray = interface(IDispatch)
    ['{F395722F-3303-489D-AA27-A04944AC2456}']
  end;

// *********************************************************************//
// DispIntf:  IJwObjectTypeListArrayDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F395722F-3303-489D-AA27-A04944AC2456}
// *********************************************************************//
  IJwObjectTypeListArrayDisp = dispinterface
    ['{F395722F-3303-489D-AA27-A04944AC2456}']
  end;

// *********************************************************************//
// Schnittstelle: IJwSIDInfoData
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2353CD1D-4E28-4CB9-B61A-1045049CF9A3}
// *********************************************************************//
  IJwSIDInfoData = interface(IDispatch)
    ['{2353CD1D-4E28-4CB9-B61A-1045049CF9A3}']
  end;

// *********************************************************************//
// DispIntf:  IJwSIDInfoDataDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2353CD1D-4E28-4CB9-B61A-1045049CF9A3}
// *********************************************************************//
  IJwSIDInfoDataDisp = dispinterface
    ['{2353CD1D-4E28-4CB9-B61A-1045049CF9A3}']
  end;

// *********************************************************************//
// Schnittstelle: IJwSIDInfoDataArray
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1A82872F-EC91-475E-8520-E0C59F7D18CD}
// *********************************************************************//
  IJwSIDInfoDataArray = interface(IDispatch)
    ['{1A82872F-EC91-475E-8520-E0C59F7D18CD}']
  end;

// *********************************************************************//
// DispIntf:  IJwSIDInfoDataArrayDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1A82872F-EC91-475E-8520-E0C59F7D18CD}
// *********************************************************************//
  IJwSIDInfoDataArrayDisp = dispinterface
    ['{1A82872F-EC91-475E-8520-E0C59F7D18CD}']
  end;

// *********************************************************************//
// Die Klasse CoJwSid stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwSid, dargestellt von
// CoClass JwSid, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwSid = class
    class function Create: IJwSid;
    class function CreateRemote(const MachineName: string): IJwSid;
  end;

// *********************************************************************//
// Die Klasse CoJwSidList stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwSidList, dargestellt von
// CoClass JwSidList, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwSidList = class
    class function Create: IJwSidList;
    class function CreateRemote(const MachineName: string): IJwSidList;
  end;

// *********************************************************************//
// Die Klasse CoJwTest2 stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwTest, dargestellt von
// CoClass JwTest2, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwTest2 = class
    class function Create: IJwTest;
    class function CreateRemote(const MachineName: string): IJwTest;
  end;

// *********************************************************************//
// Die Klasse CoJwAccessControlList stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwAccessControlList, dargestellt von
// CoClass JwAccessControlList, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwAccessControlList = class
    class function Create: IJwAccessControlList;
    class function CreateRemote(const MachineName: string): IJwAccessControlList;
  end;

// *********************************************************************//
// Die Klasse CoJwSecurityDescriptor stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwSecurityDescriptor, dargestellt von
// CoClass JwSecurityDescriptor, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwSecurityDescriptor = class
    class function Create: IJwSecurityDescriptor;
    class function CreateRemote(const MachineName: string): IJwSecurityDescriptor;
  end;

// *********************************************************************//
// Die Klasse CoJwToken stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwToken, dargestellt von
// CoClass JwToken, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwToken = class
    class function Create: IJwToken;
    class function CreateRemote(const MachineName: string): IJwToken;
  end;

// *********************************************************************//
// Die Klasse CoJwPrivilegeList stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwPrivilegeList, dargestellt von
// CoClass JwPrivilegeList, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwPrivilegeList = class
    class function Create: IJwPrivilegeList;
    class function CreateRemote(const MachineName: string): IJwPrivilegeList;
  end;

// *********************************************************************//
// Die Klasse CoJwWindowsVersion stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwWindowsVersion, dargestellt von
// CoClass JwWindowsVersion, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwWindowsVersion = class
    class function Create: IJwWindowsVersion;
    class function CreateRemote(const MachineName: string): IJwWindowsVersion;
  end;

// *********************************************************************//
// Die Klasse CoJwGenericMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwGenericMapping, dargestellt von
// CoClass JwGenericMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwGenericMapping = class
    class function Create: IJwGenericMapping;
    class function CreateRemote(const MachineName: string): IJwGenericMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwFileFolderMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwFileFolderMapping, dargestellt von
// CoClass JwFileFolderMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwFileFolderMapping = class
    class function Create: IJwFileFolderMapping;
    class function CreateRemote(const MachineName: string): IJwFileFolderMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwFileMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwFileMapping, dargestellt von
// CoClass JwFileMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwFileMapping = class
    class function Create: IJwFileMapping;
    class function CreateRemote(const MachineName: string): IJwFileMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwRegistryMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwRegistryMapping, dargestellt von
// CoClass JwRegistryMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwRegistryMapping = class
    class function Create: IJwRegistryMapping;
    class function CreateRemote(const MachineName: string): IJwRegistryMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwWinStationMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwWinStationMapping, dargestellt von
// CoClass JwWinStationMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwWinStationMapping = class
    class function Create: IJwWinStationMapping;
    class function CreateRemote(const MachineName: string): IJwWinStationMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwDesktopMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwDesktopMapping, dargestellt von
// CoClass JwDesktopMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwDesktopMapping = class
    class function Create: IJwDesktopMapping;
    class function CreateRemote(const MachineName: string): IJwDesktopMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwServiceMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwServiceMapping, dargestellt von
// CoClass JwServiceMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwServiceMapping = class
    class function Create: IJwServiceMapping;
    class function CreateRemote(const MachineName: string): IJwServiceMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwServiceMappingManager stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwServiceManagerMapping, dargestellt von
// CoClass JwServiceMappingManager, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwServiceMappingManager = class
    class function Create: IJwServiceManagerMapping;
    class function CreateRemote(const MachineName: string): IJwServiceManagerMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwPrinterMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwPrinterMapping, dargestellt von
// CoClass JwPrinterMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwPrinterMapping = class
    class function Create: IJwPrinterMapping;
    class function CreateRemote(const MachineName: string): IJwPrinterMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwShareMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwShareMapping, dargestellt von
// CoClass JwShareMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwShareMapping = class
    class function Create: IJwShareMapping;
    class function CreateRemote(const MachineName: string): IJwShareMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwProcessMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwProcessMapping, dargestellt von
// CoClass JwProcessMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwProcessMapping = class
    class function Create: IJwProcessMapping;
    class function CreateRemote(const MachineName: string): IJwProcessMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwThreadMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwThreadMapping, dargestellt von
// CoClass JwThreadMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwThreadMapping = class
    class function Create: IJwThreadMapping;
    class function CreateRemote(const MachineName: string): IJwThreadMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwJobMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwJobMapping, dargestellt von
// CoClass JwJobMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwJobMapping = class
    class function Create: IJwJobMapping;
    class function CreateRemote(const MachineName: string): IJwJobMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwSemaphoreMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwSemaphoreMapping, dargestellt von
// CoClass JwSemaphoreMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwSemaphoreMapping = class
    class function Create: IJwSemaphoreMapping;
    class function CreateRemote(const MachineName: string): IJwSemaphoreMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwEventMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwEventMapping, dargestellt von
// CoClass JwEventMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwEventMapping = class
    class function Create: IJwEventMapping;
    class function CreateRemote(const MachineName: string): IJwEventMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwMutexMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwMutexMapping, dargestellt von
// CoClass JwMutexMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwMutexMapping = class
    class function Create: IJwMutexMapping;
    class function CreateRemote(const MachineName: string): IJwMutexMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwFileMapMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwFileMapMapping, dargestellt von
// CoClass JwFileMapMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwFileMapMapping = class
    class function Create: IJwFileMapMapping;
    class function CreateRemote(const MachineName: string): IJwFileMapMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwTimerMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwTimerMapping, dargestellt von
// CoClass JwTimerMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwTimerMapping = class
    class function Create: IJwTimerMapping;
    class function CreateRemote(const MachineName: string): IJwTimerMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwTokenMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwTokenMapping, dargestellt von
// CoClass JwTokenMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwTokenMapping = class
    class function Create: IJwTokenMapping;
    class function CreateRemote(const MachineName: string): IJwTokenMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwPipeMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwPipeMapping, dargestellt von
// CoClass JwPipeMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwPipeMapping = class
    class function Create: IJwPipeMapping;
    class function CreateRemote(const MachineName: string): IJwPipeMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwLogServer stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwLogServer, dargestellt von
// CoClass JwLogServer, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwLogServer = class
    class function Create: IJwLogServer;
    class function CreateRemote(const MachineName: string): IJwLogServer;
  end;

// *********************************************************************//
// Die Klasse CoJwLogClient stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwLogClient, dargestellt von
// CoClass JwLogClient, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwLogClient = class
    class function Create: IJwLogClient;
    class function CreateRemote(const MachineName: string): IJwLogClient;
  end;

// *********************************************************************//
// Die Klasse CoTestInt stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle ITestInt, dargestellt von
// CoClass TestInt, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoTestInt = class
    class function Create: ITestInt;
    class function CreateRemote(const MachineName: string): ITestInt;
  end;

// *********************************************************************//
// Die Klasse CoJwXMLAttribute stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwXMLAttribute, dargestellt von
// CoClass JwXMLAttribute, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwXMLAttribute = class
    class function Create: IJwXMLAttribute;
    class function CreateRemote(const MachineName: string): IJwXMLAttribute;
  end;

// *********************************************************************//
// Die Klasse CoJwEventType stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwEventType, dargestellt von
// CoClass JwEventType, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwEventType = class
    class function Create: IJwEventType;
    class function CreateRemote(const MachineName: string): IJwEventType;
  end;

// *********************************************************************//
// Die Klasse CoJwEncryptionApi stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwEncryptionApi, dargestellt von
// CoClass JwEncryptionApi, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwEncryptionApi = class
    class function Create: IJwEncryptionApi;
    class function CreateRemote(const MachineName: string): IJwEncryptionApi;
  end;

// *********************************************************************//
// Die Klasse CoJwEncryptMemory stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwEncryptMemory, dargestellt von
// CoClass JwEncryptMemory, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwEncryptMemory = class
    class function Create: IJwEncryptMemory;
    class function CreateRemote(const MachineName: string): IJwEncryptMemory;
  end;

// *********************************************************************//
// Die Klasse CoJwEncryptData stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwEncryptData, dargestellt von
// CoClass JwEncryptData, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwEncryptData = class
    class function Create: IJwEncryptData;
    class function CreateRemote(const MachineName: string): IJwEncryptData;
  end;

// *********************************************************************//
// Die Klasse CoJwRandomDataGenerator stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwRandomDataGenerator, dargestellt von
// CoClass JwRandomDataGenerator, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwRandomDataGenerator = class
    class function Create: IJwRandomDataGenerator;
    class function CreateRemote(const MachineName: string): IJwRandomDataGenerator;
  end;

// *********************************************************************//
// Die Klasse CoJwWindowStation stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwWindowStation, dargestellt von
// CoClass JwWindowStation, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwWindowStation = class
    class function Create: IJwWindowStation;
    class function CreateRemote(const MachineName: string): IJwWindowStation;
  end;

// *********************************************************************//
// Die Klasse CoJwWindowStations stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwWindowStations, dargestellt von
// CoClass JwWindowStations, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwWindowStations = class
    class function Create: IJwWindowStations;
    class function CreateRemote(const MachineName: string): IJwWindowStations;
  end;

// *********************************************************************//
// Die Klasse CoJwDesktop stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwDesktop, dargestellt von
// CoClass JwDesktop, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwDesktop = class
    class function Create: IJwDesktop;
    class function CreateRemote(const MachineName: string): IJwDesktop;
  end;

// *********************************************************************//
// Die Klasse CoJwDesktops stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwDesktops, dargestellt von
// CoClass JwDesktops, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwDesktops = class
    class function Create: IJwDesktops;
    class function CreateRemote(const MachineName: string): IJwDesktops;
  end;

// *********************************************************************//
// Die Klasse CoJwCredentialsPrompt stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwCredentialsPrompt, dargestellt von
// CoClass JwCredentialsPrompt, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwCredentialsPrompt = class
    class function Create: IJwCredentialsPrompt;
    class function CreateRemote(const MachineName: string): IJwCredentialsPrompt;
  end;

// *********************************************************************//
// Die Klasse CoJwCredentialsTools stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwCredentialsTools, dargestellt von
// CoClass JwCredentialsTools, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwCredentialsTools = class
    class function Create: IJwCredentialsTools;
    class function CreateRemote(const MachineName: string): IJwCredentialsTools;
  end;

// *********************************************************************//
// Die Klasse CoJwCryptProvider stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwCryptProvider, dargestellt von
// CoClass JwCryptProvider, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwCryptProvider = class
    class function Create: IJwCryptProvider;
    class function CreateRemote(const MachineName: string): IJwCryptProvider;
  end;

// *********************************************************************//
// Die Klasse CoJwHash stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwHash, dargestellt von
// CoClass JwHash, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwHash = class
    class function Create: IJwHash;
    class function CreateRemote(const MachineName: string): IJwHash;
  end;

// *********************************************************************//
// Die Klasse CoJwCryptKey stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwCryptKey, dargestellt von
// CoClass JwCryptKey, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwCryptKey = class
    class function Create: IJwCryptKey;
    class function CreateRemote(const MachineName: string): IJwCryptKey;
  end;

// *********************************************************************//
// Die Klasse CoJwImpersonation stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwImpersonation, dargestellt von
// CoClass JwImpersonation, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwImpersonation = class
    class function Create: IJwImpersonation;
    class function CreateRemote(const MachineName: string): IJwImpersonation;
  end;

// *********************************************************************//
// Die Klasse CoJwKnownSIDs stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwKnownSIDs, dargestellt von
// CoClass JwKnownSIDs, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwKnownSIDs = class
    class function Create: IJwKnownSIDs;
    class function CreateRemote(const MachineName: string): IJwKnownSIDs;
  end;

// *********************************************************************//
// Die Klasse CoJwThreadUserSid stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwThreadUserSid, dargestellt von
// CoClass JwThreadUserSid, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwThreadUserSid = class
    class function Create: IJwThreadUserSid;
    class function CreateRemote(const MachineName: string): IJwThreadUserSid;
  end;

// *********************************************************************//
// Die Klasse CoJwLSA stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwLSA, dargestellt von
// CoClass JwLSA, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwLSA = class
    class function Create: IJwLSA;
    class function CreateRemote(const MachineName: string): IJwLSA;
  end;

// *********************************************************************//
// Die Klasse CoJwLSALogonSession stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwLSALogonSession, dargestellt von
// CoClass JwLSALogonSession, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwLSALogonSession = class
    class function Create: IJwLSALogonSession;
    class function CreateRemote(const MachineName: string): IJwLSALogonSession;
  end;

// *********************************************************************//
// Die Klasse CoJwLSALogonSessionData stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwLSALogonSessionData, dargestellt von
// CoClass JwLSALogonSessionData, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwLSALogonSessionData = class
    class function Create: IJwLSALogonSessionData;
    class function CreateRemote(const MachineName: string): IJwLSALogonSessionData;
  end;

// *********************************************************************//
// Die Klasse CoJwPrivilegeScope stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwPrivilegeScope, dargestellt von
// CoClass JwPrivilegeScope, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwPrivilegeScope = class
    class function Create: IJwPrivilegeScope;
    class function CreateRemote(const MachineName: string): IJwPrivilegeScope;
  end;

// *********************************************************************//
// Die Klasse CoJwProcessUtils stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwProcessUtils, dargestellt von
// CoClass JwProcessUtils, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwProcessUtils = class
    class function Create: IJwProcessUtils;
    class function CreateRemote(const MachineName: string): IJwProcessUtils;
  end;

// *********************************************************************//
// Die Klasse CoJwSecureRegistryKey stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwSecureRegistryKey, dargestellt von
// CoClass JwSecureRegistryKey, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwSecureRegistryKey = class
    class function Create: IJwSecureRegistryKey;
    class function CreateRemote(const MachineName: string): IJwSecureRegistryKey;
  end;

// *********************************************************************//
// Die Klasse CoJwSecureFileObject stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwSecureFileObject, dargestellt von
// CoClass JwSecureFileObject, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwSecureFileObject = class
    class function Create: IJwSecureFileObject;
    class function CreateRemote(const MachineName: string): IJwSecureFileObject;
  end;

// *********************************************************************//
// Die Klasse CoJwSecureBaseClass stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwSecureBaseClass, dargestellt von
// CoClass JwSecureBaseClass, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwSecureBaseClass = class
    class function Create: IJwSecureBaseClass;
    class function CreateRemote(const MachineName: string): IJwSecureBaseClass;
  end;

// *********************************************************************//
// Die Klasse CoJwProgressCallback stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwProgressCallback, dargestellt von
// CoClass JwProgressCallback, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwProgressCallback = class
    class function Create: IJwProgressCallback;
    class function CreateRemote(const MachineName: string): IJwProgressCallback;
  end;

// *********************************************************************//
// Die Klasse CoJwAuthZAccessRequest stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwAuthZAccessRequest, dargestellt von
// CoClass JwAuthZAccessRequest, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwAuthZAccessRequest = class
    class function Create: IJwAuthZAccessRequest;
    class function CreateRemote(const MachineName: string): IJwAuthZAccessRequest;
  end;

// *********************************************************************//
// Die Klasse CoJwAuthZAccessReply stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwAuthZAccessReply, dargestellt von
// CoClass JwAuthZAccessReply, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwAuthZAccessReply = class
    class function Create: IJwAuthZAccessReply;
    class function CreateRemote(const MachineName: string): IJwAuthZAccessReply;
  end;

// *********************************************************************//
// Die Klasse CoJwAuthResourceManager stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwAuthResourceManager, dargestellt von
// CoClass JwAuthResourceManager, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwAuthResourceManager = class
    class function Create: IJwAuthResourceManager;
    class function CreateRemote(const MachineName: string): IJwAuthResourceManager;
  end;

// *********************************************************************//
// Die Klasse CoJwAuthContext stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwAuthContext, dargestellt von
// CoClass JwAuthContext, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwAuthContext = class
    class function Create: IJwAuthContext;
    class function CreateRemote(const MachineName: string): IJwAuthContext;
  end;

// *********************************************************************//
// Die Klasse CoJwTerminalServer stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwTerminalServer, dargestellt von
// CoClass JwTerminalServer, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwTerminalServer = class
    class function Create: IJwTerminalServer;
    class function CreateRemote(const MachineName: string): IJwTerminalServer;
  end;

// *********************************************************************//
// Die Klasse CoJwTerminalServerList stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwTerminalServerList, dargestellt von
// CoClass JwTerminalServerList, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwTerminalServerList = class
    class function Create: IJwTerminalServerList;
    class function CreateRemote(const MachineName: string): IJwTerminalServerList;
  end;

// *********************************************************************//
// Die Klasse CoJwWTSSession stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwWTSSession, dargestellt von
// CoClass JwWTSSession, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwWTSSession = class
    class function Create: IJwWTSSession;
    class function CreateRemote(const MachineName: string): IJwWTSSession;
  end;

// *********************************************************************//
// Die Klasse CoJwWTSSessionList stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwWTSSessionList, dargestellt von
// CoClass JwWTSSessionList, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwWTSSessionList = class
    class function Create: IJwWTSSessionList;
    class function CreateRemote(const MachineName: string): IJwWTSSessionList;
  end;

// *********************************************************************//
// Die Klasse CoJwWTSProcess stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwWTSProcess, dargestellt von
// CoClass JwWTSProcess, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwWTSProcess = class
    class function Create: IJwWTSProcess;
    class function CreateRemote(const MachineName: string): IJwWTSProcess;
  end;

// *********************************************************************//
// Die Klasse CoJwWTSProcessList stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwWTSProcessList, dargestellt von
// CoClass JwWTSProcessList, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwWTSProcessList = class
    class function Create: IJwWTSProcessList;
    class function CreateRemote(const MachineName: string): IJwWTSProcessList;
  end;

// *********************************************************************//
// Die Klasse CoJwWTSSessionShadow stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwWTSSessionShadow, dargestellt von
// CoClass JwWTSSessionShadow, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwWTSSessionShadow = class
    class function Create: IJwWTSSessionShadow;
    class function CreateRemote(const MachineName: string): IJwWTSSessionShadow;
  end;

// *********************************************************************//
// Die Klasse CoJwInheritTypeList stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwInheritTypeList, dargestellt von
// CoClass JwInheritTypeList, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwInheritTypeList = class
    class function Create: IJwInheritTypeList;
    class function CreateRemote(const MachineName: string): IJwInheritTypeList;
  end;

// *********************************************************************//
// Die Klasse CoJwUtils stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwUtils, dargestellt von
// CoClass JwUtils, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwUtils = class
    class function Create: IJwUtils;
    class function CreateRemote(const MachineName: string): IJwUtils;
  end;

// *********************************************************************//
// Die Klasse CoJwElevationClassFactory stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwElevationClassFactory, dargestellt von
// CoClass JwElevationClassFactory, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwElevationClassFactory = class
    class function Create: IJwElevationClassFactory;
    class function CreateRemote(const MachineName: string): IJwElevationClassFactory;
  end;

// *********************************************************************//
// Die Klasse CoJwExplicitAccessArray stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwExplicitAccessArray, dargestellt von
// CoClass JwExplicitAccessArray, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwExplicitAccessArray = class
    class function Create: IJwExplicitAccessArray;
    class function CreateRemote(const MachineName: string): IJwExplicitAccessArray;
  end;

// *********************************************************************//
// Die Klasse CoJwExplicitAccess stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwExplicitAccess, dargestellt von
// CoClass JwExplicitAccess, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwExplicitAccess = class
    class function Create: IJwExplicitAccess;
    class function CreateRemote(const MachineName: string): IJwExplicitAccess;
  end;

// *********************************************************************//
// Die Klasse CoJwGuidArray stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwGuidArray, dargestellt von
// CoClass JwGuidArray, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwGuidArray = class
    class function Create: IJwGuidArray;
    class function CreateRemote(const MachineName: string): IJwGuidArray;
  end;

// *********************************************************************//
// Die Klasse CoJwInheritedFromRecord stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwInheritedFromRecord, dargestellt von
// CoClass JwInheritedFromRecord, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwInheritedFromRecord = class
    class function Create: IJwInheritedFromRecord;
    class function CreateRemote(const MachineName: string): IJwInheritedFromRecord;
  end;

// *********************************************************************//
// Die Klasse CoJwInheritedFromArray stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwInheritedFromArray, dargestellt von
// CoClass JwInheritedFromArray, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwInheritedFromArray = class
    class function Create: IJwInheritedFromArray;
    class function CreateRemote(const MachineName: string): IJwInheritedFromArray;
  end;

// *********************************************************************//
// Die Klasse CoJwRootTuple stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwRootTuple, dargestellt von
// CoClass JwRootTuple, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwRootTuple = class
    class function Create: IJwRootTuple;
    class function CreateRemote(const MachineName: string): IJwRootTuple;
  end;

// *********************************************************************//
// Die Klasse CoJwKeyRootTupleArray stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwKeyRootTupleArray, dargestellt von
// CoClass JwKeyRootTupleArray, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwKeyRootTupleArray = class
    class function Create: IJwKeyRootTupleArray;
    class function CreateRemote(const MachineName: string): IJwKeyRootTupleArray;
  end;

// *********************************************************************//
// Die Klasse CoJwAccessMaskArray stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwAccessMaskArray, dargestellt von
// CoClass JwAccessMaskArray, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwAccessMaskArray = class
    class function Create: IJwAccessMaskArray;
    class function CreateRemote(const MachineName: string): IJwAccessMaskArray;
  end;

// *********************************************************************//
// Die Klasse CoJwObjectTypeList stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwObjectTypeList, dargestellt von
// CoClass JwObjectTypeList, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwObjectTypeList = class
    class function Create: IJwObjectTypeList;
    class function CreateRemote(const MachineName: string): IJwObjectTypeList;
  end;

// *********************************************************************//
// Die Klasse CoJwObjectTypeListArray stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwObjectTypeListArray, dargestellt von
// CoClass JwObjectTypeListArray, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwObjectTypeListArray = class
    class function Create: IJwObjectTypeListArray;
    class function CreateRemote(const MachineName: string): IJwObjectTypeListArray;
  end;

// *********************************************************************//
// Die Klasse CoJwSIDInfoData stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwSIDInfoData, dargestellt von
// CoClass JwSIDInfoData, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwSIDInfoData = class
    class function Create: IJwSIDInfoData;
    class function CreateRemote(const MachineName: string): IJwSIDInfoData;
  end;

// *********************************************************************//
// Die Klasse CoJwSIDInfoDataArray stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwSIDInfoDataArray, dargestellt von
// CoClass JwSIDInfoDataArray, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwSIDInfoDataArray = class
    class function Create: IJwSIDInfoDataArray;
    class function CreateRemote(const MachineName: string): IJwSIDInfoDataArray;
  end;

implementation

uses ComObj;

class function CoJwSid.Create: IJwSid;
begin
  Result := CreateComObject(CLASS_JwSid) as IJwSid;
end;

class function CoJwSid.CreateRemote(const MachineName: string): IJwSid;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwSid) as IJwSid;
end;

class function CoJwSidList.Create: IJwSidList;
begin
  Result := CreateComObject(CLASS_JwSidList) as IJwSidList;
end;

class function CoJwSidList.CreateRemote(const MachineName: string): IJwSidList;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwSidList) as IJwSidList;
end;

class function CoJwTest2.Create: IJwTest;
begin
  Result := CreateComObject(CLASS_JwTest2) as IJwTest;
end;

class function CoJwTest2.CreateRemote(const MachineName: string): IJwTest;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwTest2) as IJwTest;
end;

class function CoJwAccessControlList.Create: IJwAccessControlList;
begin
  Result := CreateComObject(CLASS_JwAccessControlList) as IJwAccessControlList;
end;

class function CoJwAccessControlList.CreateRemote(const MachineName: string): IJwAccessControlList;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwAccessControlList) as IJwAccessControlList;
end;

class function CoJwSecurityDescriptor.Create: IJwSecurityDescriptor;
begin
  Result := CreateComObject(CLASS_JwSecurityDescriptor) as IJwSecurityDescriptor;
end;

class function CoJwSecurityDescriptor.CreateRemote(const MachineName: string): IJwSecurityDescriptor;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwSecurityDescriptor) as IJwSecurityDescriptor;
end;

class function CoJwToken.Create: IJwToken;
begin
  Result := CreateComObject(CLASS_JwToken) as IJwToken;
end;

class function CoJwToken.CreateRemote(const MachineName: string): IJwToken;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwToken) as IJwToken;
end;

class function CoJwPrivilegeList.Create: IJwPrivilegeList;
begin
  Result := CreateComObject(CLASS_JwPrivilegeList) as IJwPrivilegeList;
end;

class function CoJwPrivilegeList.CreateRemote(const MachineName: string): IJwPrivilegeList;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwPrivilegeList) as IJwPrivilegeList;
end;

class function CoJwWindowsVersion.Create: IJwWindowsVersion;
begin
  Result := CreateComObject(CLASS_JwWindowsVersion) as IJwWindowsVersion;
end;

class function CoJwWindowsVersion.CreateRemote(const MachineName: string): IJwWindowsVersion;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwWindowsVersion) as IJwWindowsVersion;
end;

class function CoJwGenericMapping.Create: IJwGenericMapping;
begin
  Result := CreateComObject(CLASS_JwGenericMapping) as IJwGenericMapping;
end;

class function CoJwGenericMapping.CreateRemote(const MachineName: string): IJwGenericMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwGenericMapping) as IJwGenericMapping;
end;

class function CoJwFileFolderMapping.Create: IJwFileFolderMapping;
begin
  Result := CreateComObject(CLASS_JwFileFolderMapping) as IJwFileFolderMapping;
end;

class function CoJwFileFolderMapping.CreateRemote(const MachineName: string): IJwFileFolderMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwFileFolderMapping) as IJwFileFolderMapping;
end;

class function CoJwFileMapping.Create: IJwFileMapping;
begin
  Result := CreateComObject(CLASS_JwFileMapping) as IJwFileMapping;
end;

class function CoJwFileMapping.CreateRemote(const MachineName: string): IJwFileMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwFileMapping) as IJwFileMapping;
end;

class function CoJwRegistryMapping.Create: IJwRegistryMapping;
begin
  Result := CreateComObject(CLASS_JwRegistryMapping) as IJwRegistryMapping;
end;

class function CoJwRegistryMapping.CreateRemote(const MachineName: string): IJwRegistryMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwRegistryMapping) as IJwRegistryMapping;
end;

class function CoJwWinStationMapping.Create: IJwWinStationMapping;
begin
  Result := CreateComObject(CLASS_JwWinStationMapping) as IJwWinStationMapping;
end;

class function CoJwWinStationMapping.CreateRemote(const MachineName: string): IJwWinStationMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwWinStationMapping) as IJwWinStationMapping;
end;

class function CoJwDesktopMapping.Create: IJwDesktopMapping;
begin
  Result := CreateComObject(CLASS_JwDesktopMapping) as IJwDesktopMapping;
end;

class function CoJwDesktopMapping.CreateRemote(const MachineName: string): IJwDesktopMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwDesktopMapping) as IJwDesktopMapping;
end;

class function CoJwServiceMapping.Create: IJwServiceMapping;
begin
  Result := CreateComObject(CLASS_JwServiceMapping) as IJwServiceMapping;
end;

class function CoJwServiceMapping.CreateRemote(const MachineName: string): IJwServiceMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwServiceMapping) as IJwServiceMapping;
end;

class function CoJwServiceMappingManager.Create: IJwServiceManagerMapping;
begin
  Result := CreateComObject(CLASS_JwServiceMappingManager) as IJwServiceManagerMapping;
end;

class function CoJwServiceMappingManager.CreateRemote(const MachineName: string): IJwServiceManagerMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwServiceMappingManager) as IJwServiceManagerMapping;
end;

class function CoJwPrinterMapping.Create: IJwPrinterMapping;
begin
  Result := CreateComObject(CLASS_JwPrinterMapping) as IJwPrinterMapping;
end;

class function CoJwPrinterMapping.CreateRemote(const MachineName: string): IJwPrinterMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwPrinterMapping) as IJwPrinterMapping;
end;

class function CoJwShareMapping.Create: IJwShareMapping;
begin
  Result := CreateComObject(CLASS_JwShareMapping) as IJwShareMapping;
end;

class function CoJwShareMapping.CreateRemote(const MachineName: string): IJwShareMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwShareMapping) as IJwShareMapping;
end;

class function CoJwProcessMapping.Create: IJwProcessMapping;
begin
  Result := CreateComObject(CLASS_JwProcessMapping) as IJwProcessMapping;
end;

class function CoJwProcessMapping.CreateRemote(const MachineName: string): IJwProcessMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwProcessMapping) as IJwProcessMapping;
end;

class function CoJwThreadMapping.Create: IJwThreadMapping;
begin
  Result := CreateComObject(CLASS_JwThreadMapping) as IJwThreadMapping;
end;

class function CoJwThreadMapping.CreateRemote(const MachineName: string): IJwThreadMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwThreadMapping) as IJwThreadMapping;
end;

class function CoJwJobMapping.Create: IJwJobMapping;
begin
  Result := CreateComObject(CLASS_JwJobMapping) as IJwJobMapping;
end;

class function CoJwJobMapping.CreateRemote(const MachineName: string): IJwJobMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwJobMapping) as IJwJobMapping;
end;

class function CoJwSemaphoreMapping.Create: IJwSemaphoreMapping;
begin
  Result := CreateComObject(CLASS_JwSemaphoreMapping) as IJwSemaphoreMapping;
end;

class function CoJwSemaphoreMapping.CreateRemote(const MachineName: string): IJwSemaphoreMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwSemaphoreMapping) as IJwSemaphoreMapping;
end;

class function CoJwEventMapping.Create: IJwEventMapping;
begin
  Result := CreateComObject(CLASS_JwEventMapping) as IJwEventMapping;
end;

class function CoJwEventMapping.CreateRemote(const MachineName: string): IJwEventMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwEventMapping) as IJwEventMapping;
end;

class function CoJwMutexMapping.Create: IJwMutexMapping;
begin
  Result := CreateComObject(CLASS_JwMutexMapping) as IJwMutexMapping;
end;

class function CoJwMutexMapping.CreateRemote(const MachineName: string): IJwMutexMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwMutexMapping) as IJwMutexMapping;
end;

class function CoJwFileMapMapping.Create: IJwFileMapMapping;
begin
  Result := CreateComObject(CLASS_JwFileMapMapping) as IJwFileMapMapping;
end;

class function CoJwFileMapMapping.CreateRemote(const MachineName: string): IJwFileMapMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwFileMapMapping) as IJwFileMapMapping;
end;

class function CoJwTimerMapping.Create: IJwTimerMapping;
begin
  Result := CreateComObject(CLASS_JwTimerMapping) as IJwTimerMapping;
end;

class function CoJwTimerMapping.CreateRemote(const MachineName: string): IJwTimerMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwTimerMapping) as IJwTimerMapping;
end;

class function CoJwTokenMapping.Create: IJwTokenMapping;
begin
  Result := CreateComObject(CLASS_JwTokenMapping) as IJwTokenMapping;
end;

class function CoJwTokenMapping.CreateRemote(const MachineName: string): IJwTokenMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwTokenMapping) as IJwTokenMapping;
end;

class function CoJwPipeMapping.Create: IJwPipeMapping;
begin
  Result := CreateComObject(CLASS_JwPipeMapping) as IJwPipeMapping;
end;

class function CoJwPipeMapping.CreateRemote(const MachineName: string): IJwPipeMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwPipeMapping) as IJwPipeMapping;
end;

class function CoJwLogServer.Create: IJwLogServer;
begin
  Result := CreateComObject(CLASS_JwLogServer) as IJwLogServer;
end;

class function CoJwLogServer.CreateRemote(const MachineName: string): IJwLogServer;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwLogServer) as IJwLogServer;
end;

class function CoJwLogClient.Create: IJwLogClient;
begin
  Result := CreateComObject(CLASS_JwLogClient) as IJwLogClient;
end;

class function CoJwLogClient.CreateRemote(const MachineName: string): IJwLogClient;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwLogClient) as IJwLogClient;
end;

class function CoTestInt.Create: ITestInt;
begin
  Result := CreateComObject(CLASS_TestInt) as ITestInt;
end;

class function CoTestInt.CreateRemote(const MachineName: string): ITestInt;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_TestInt) as ITestInt;
end;

class function CoJwXMLAttribute.Create: IJwXMLAttribute;
begin
  Result := CreateComObject(CLASS_JwXMLAttribute) as IJwXMLAttribute;
end;

class function CoJwXMLAttribute.CreateRemote(const MachineName: string): IJwXMLAttribute;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwXMLAttribute) as IJwXMLAttribute;
end;

class function CoJwEventType.Create: IJwEventType;
begin
  Result := CreateComObject(CLASS_JwEventType) as IJwEventType;
end;

class function CoJwEventType.CreateRemote(const MachineName: string): IJwEventType;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwEventType) as IJwEventType;
end;

class function CoJwEncryptionApi.Create: IJwEncryptionApi;
begin
  Result := CreateComObject(CLASS_JwEncryptionApi) as IJwEncryptionApi;
end;

class function CoJwEncryptionApi.CreateRemote(const MachineName: string): IJwEncryptionApi;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwEncryptionApi) as IJwEncryptionApi;
end;

class function CoJwEncryptMemory.Create: IJwEncryptMemory;
begin
  Result := CreateComObject(CLASS_JwEncryptMemory) as IJwEncryptMemory;
end;

class function CoJwEncryptMemory.CreateRemote(const MachineName: string): IJwEncryptMemory;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwEncryptMemory) as IJwEncryptMemory;
end;

class function CoJwEncryptData.Create: IJwEncryptData;
begin
  Result := CreateComObject(CLASS_JwEncryptData) as IJwEncryptData;
end;

class function CoJwEncryptData.CreateRemote(const MachineName: string): IJwEncryptData;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwEncryptData) as IJwEncryptData;
end;

class function CoJwRandomDataGenerator.Create: IJwRandomDataGenerator;
begin
  Result := CreateComObject(CLASS_JwRandomDataGenerator) as IJwRandomDataGenerator;
end;

class function CoJwRandomDataGenerator.CreateRemote(const MachineName: string): IJwRandomDataGenerator;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwRandomDataGenerator) as IJwRandomDataGenerator;
end;

class function CoJwWindowStation.Create: IJwWindowStation;
begin
  Result := CreateComObject(CLASS_JwWindowStation) as IJwWindowStation;
end;

class function CoJwWindowStation.CreateRemote(const MachineName: string): IJwWindowStation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwWindowStation) as IJwWindowStation;
end;

class function CoJwWindowStations.Create: IJwWindowStations;
begin
  Result := CreateComObject(CLASS_JwWindowStations) as IJwWindowStations;
end;

class function CoJwWindowStations.CreateRemote(const MachineName: string): IJwWindowStations;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwWindowStations) as IJwWindowStations;
end;

class function CoJwDesktop.Create: IJwDesktop;
begin
  Result := CreateComObject(CLASS_JwDesktop) as IJwDesktop;
end;

class function CoJwDesktop.CreateRemote(const MachineName: string): IJwDesktop;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwDesktop) as IJwDesktop;
end;

class function CoJwDesktops.Create: IJwDesktops;
begin
  Result := CreateComObject(CLASS_JwDesktops) as IJwDesktops;
end;

class function CoJwDesktops.CreateRemote(const MachineName: string): IJwDesktops;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwDesktops) as IJwDesktops;
end;

class function CoJwCredentialsPrompt.Create: IJwCredentialsPrompt;
begin
  Result := CreateComObject(CLASS_JwCredentialsPrompt) as IJwCredentialsPrompt;
end;

class function CoJwCredentialsPrompt.CreateRemote(const MachineName: string): IJwCredentialsPrompt;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwCredentialsPrompt) as IJwCredentialsPrompt;
end;

class function CoJwCredentialsTools.Create: IJwCredentialsTools;
begin
  Result := CreateComObject(CLASS_JwCredentialsTools) as IJwCredentialsTools;
end;

class function CoJwCredentialsTools.CreateRemote(const MachineName: string): IJwCredentialsTools;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwCredentialsTools) as IJwCredentialsTools;
end;

class function CoJwCryptProvider.Create: IJwCryptProvider;
begin
  Result := CreateComObject(CLASS_JwCryptProvider) as IJwCryptProvider;
end;

class function CoJwCryptProvider.CreateRemote(const MachineName: string): IJwCryptProvider;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwCryptProvider) as IJwCryptProvider;
end;

class function CoJwHash.Create: IJwHash;
begin
  Result := CreateComObject(CLASS_JwHash) as IJwHash;
end;

class function CoJwHash.CreateRemote(const MachineName: string): IJwHash;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwHash) as IJwHash;
end;

class function CoJwCryptKey.Create: IJwCryptKey;
begin
  Result := CreateComObject(CLASS_JwCryptKey) as IJwCryptKey;
end;

class function CoJwCryptKey.CreateRemote(const MachineName: string): IJwCryptKey;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwCryptKey) as IJwCryptKey;
end;

class function CoJwImpersonation.Create: IJwImpersonation;
begin
  Result := CreateComObject(CLASS_JwImpersonation) as IJwImpersonation;
end;

class function CoJwImpersonation.CreateRemote(const MachineName: string): IJwImpersonation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwImpersonation) as IJwImpersonation;
end;

class function CoJwKnownSIDs.Create: IJwKnownSIDs;
begin
  Result := CreateComObject(CLASS_JwKnownSIDs) as IJwKnownSIDs;
end;

class function CoJwKnownSIDs.CreateRemote(const MachineName: string): IJwKnownSIDs;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwKnownSIDs) as IJwKnownSIDs;
end;

class function CoJwThreadUserSid.Create: IJwThreadUserSid;
begin
  Result := CreateComObject(CLASS_JwThreadUserSid) as IJwThreadUserSid;
end;

class function CoJwThreadUserSid.CreateRemote(const MachineName: string): IJwThreadUserSid;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwThreadUserSid) as IJwThreadUserSid;
end;

class function CoJwLSA.Create: IJwLSA;
begin
  Result := CreateComObject(CLASS_JwLSA) as IJwLSA;
end;

class function CoJwLSA.CreateRemote(const MachineName: string): IJwLSA;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwLSA) as IJwLSA;
end;

class function CoJwLSALogonSession.Create: IJwLSALogonSession;
begin
  Result := CreateComObject(CLASS_JwLSALogonSession) as IJwLSALogonSession;
end;

class function CoJwLSALogonSession.CreateRemote(const MachineName: string): IJwLSALogonSession;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwLSALogonSession) as IJwLSALogonSession;
end;

class function CoJwLSALogonSessionData.Create: IJwLSALogonSessionData;
begin
  Result := CreateComObject(CLASS_JwLSALogonSessionData) as IJwLSALogonSessionData;
end;

class function CoJwLSALogonSessionData.CreateRemote(const MachineName: string): IJwLSALogonSessionData;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwLSALogonSessionData) as IJwLSALogonSessionData;
end;

class function CoJwPrivilegeScope.Create: IJwPrivilegeScope;
begin
  Result := CreateComObject(CLASS_JwPrivilegeScope) as IJwPrivilegeScope;
end;

class function CoJwPrivilegeScope.CreateRemote(const MachineName: string): IJwPrivilegeScope;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwPrivilegeScope) as IJwPrivilegeScope;
end;

class function CoJwProcessUtils.Create: IJwProcessUtils;
begin
  Result := CreateComObject(CLASS_JwProcessUtils) as IJwProcessUtils;
end;

class function CoJwProcessUtils.CreateRemote(const MachineName: string): IJwProcessUtils;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwProcessUtils) as IJwProcessUtils;
end;

class function CoJwSecureRegistryKey.Create: IJwSecureRegistryKey;
begin
  Result := CreateComObject(CLASS_JwSecureRegistryKey) as IJwSecureRegistryKey;
end;

class function CoJwSecureRegistryKey.CreateRemote(const MachineName: string): IJwSecureRegistryKey;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwSecureRegistryKey) as IJwSecureRegistryKey;
end;

class function CoJwSecureFileObject.Create: IJwSecureFileObject;
begin
  Result := CreateComObject(CLASS_JwSecureFileObject) as IJwSecureFileObject;
end;

class function CoJwSecureFileObject.CreateRemote(const MachineName: string): IJwSecureFileObject;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwSecureFileObject) as IJwSecureFileObject;
end;

class function CoJwSecureBaseClass.Create: IJwSecureBaseClass;
begin
  Result := CreateComObject(CLASS_JwSecureBaseClass) as IJwSecureBaseClass;
end;

class function CoJwSecureBaseClass.CreateRemote(const MachineName: string): IJwSecureBaseClass;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwSecureBaseClass) as IJwSecureBaseClass;
end;

class function CoJwProgressCallback.Create: IJwProgressCallback;
begin
  Result := CreateComObject(CLASS_JwProgressCallback) as IJwProgressCallback;
end;

class function CoJwProgressCallback.CreateRemote(const MachineName: string): IJwProgressCallback;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwProgressCallback) as IJwProgressCallback;
end;

class function CoJwAuthZAccessRequest.Create: IJwAuthZAccessRequest;
begin
  Result := CreateComObject(CLASS_JwAuthZAccessRequest) as IJwAuthZAccessRequest;
end;

class function CoJwAuthZAccessRequest.CreateRemote(const MachineName: string): IJwAuthZAccessRequest;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwAuthZAccessRequest) as IJwAuthZAccessRequest;
end;

class function CoJwAuthZAccessReply.Create: IJwAuthZAccessReply;
begin
  Result := CreateComObject(CLASS_JwAuthZAccessReply) as IJwAuthZAccessReply;
end;

class function CoJwAuthZAccessReply.CreateRemote(const MachineName: string): IJwAuthZAccessReply;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwAuthZAccessReply) as IJwAuthZAccessReply;
end;

class function CoJwAuthResourceManager.Create: IJwAuthResourceManager;
begin
  Result := CreateComObject(CLASS_JwAuthResourceManager) as IJwAuthResourceManager;
end;

class function CoJwAuthResourceManager.CreateRemote(const MachineName: string): IJwAuthResourceManager;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwAuthResourceManager) as IJwAuthResourceManager;
end;

class function CoJwAuthContext.Create: IJwAuthContext;
begin
  Result := CreateComObject(CLASS_JwAuthContext) as IJwAuthContext;
end;

class function CoJwAuthContext.CreateRemote(const MachineName: string): IJwAuthContext;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwAuthContext) as IJwAuthContext;
end;

class function CoJwTerminalServer.Create: IJwTerminalServer;
begin
  Result := CreateComObject(CLASS_JwTerminalServer) as IJwTerminalServer;
end;

class function CoJwTerminalServer.CreateRemote(const MachineName: string): IJwTerminalServer;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwTerminalServer) as IJwTerminalServer;
end;

class function CoJwTerminalServerList.Create: IJwTerminalServerList;
begin
  Result := CreateComObject(CLASS_JwTerminalServerList) as IJwTerminalServerList;
end;

class function CoJwTerminalServerList.CreateRemote(const MachineName: string): IJwTerminalServerList;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwTerminalServerList) as IJwTerminalServerList;
end;

class function CoJwWTSSession.Create: IJwWTSSession;
begin
  Result := CreateComObject(CLASS_JwWTSSession) as IJwWTSSession;
end;

class function CoJwWTSSession.CreateRemote(const MachineName: string): IJwWTSSession;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwWTSSession) as IJwWTSSession;
end;

class function CoJwWTSSessionList.Create: IJwWTSSessionList;
begin
  Result := CreateComObject(CLASS_JwWTSSessionList) as IJwWTSSessionList;
end;

class function CoJwWTSSessionList.CreateRemote(const MachineName: string): IJwWTSSessionList;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwWTSSessionList) as IJwWTSSessionList;
end;

class function CoJwWTSProcess.Create: IJwWTSProcess;
begin
  Result := CreateComObject(CLASS_JwWTSProcess) as IJwWTSProcess;
end;

class function CoJwWTSProcess.CreateRemote(const MachineName: string): IJwWTSProcess;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwWTSProcess) as IJwWTSProcess;
end;

class function CoJwWTSProcessList.Create: IJwWTSProcessList;
begin
  Result := CreateComObject(CLASS_JwWTSProcessList) as IJwWTSProcessList;
end;

class function CoJwWTSProcessList.CreateRemote(const MachineName: string): IJwWTSProcessList;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwWTSProcessList) as IJwWTSProcessList;
end;

class function CoJwWTSSessionShadow.Create: IJwWTSSessionShadow;
begin
  Result := CreateComObject(CLASS_JwWTSSessionShadow) as IJwWTSSessionShadow;
end;

class function CoJwWTSSessionShadow.CreateRemote(const MachineName: string): IJwWTSSessionShadow;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwWTSSessionShadow) as IJwWTSSessionShadow;
end;

class function CoJwInheritTypeList.Create: IJwInheritTypeList;
begin
  Result := CreateComObject(CLASS_JwInheritTypeList) as IJwInheritTypeList;
end;

class function CoJwInheritTypeList.CreateRemote(const MachineName: string): IJwInheritTypeList;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwInheritTypeList) as IJwInheritTypeList;
end;

class function CoJwUtils.Create: IJwUtils;
begin
  Result := CreateComObject(CLASS_JwUtils) as IJwUtils;
end;

class function CoJwUtils.CreateRemote(const MachineName: string): IJwUtils;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwUtils) as IJwUtils;
end;

class function CoJwElevationClassFactory.Create: IJwElevationClassFactory;
begin
  Result := CreateComObject(CLASS_JwElevationClassFactory) as IJwElevationClassFactory;
end;

class function CoJwElevationClassFactory.CreateRemote(const MachineName: string): IJwElevationClassFactory;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwElevationClassFactory) as IJwElevationClassFactory;
end;

class function CoJwExplicitAccessArray.Create: IJwExplicitAccessArray;
begin
  Result := CreateComObject(CLASS_JwExplicitAccessArray) as IJwExplicitAccessArray;
end;

class function CoJwExplicitAccessArray.CreateRemote(const MachineName: string): IJwExplicitAccessArray;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwExplicitAccessArray) as IJwExplicitAccessArray;
end;

class function CoJwExplicitAccess.Create: IJwExplicitAccess;
begin
  Result := CreateComObject(CLASS_JwExplicitAccess) as IJwExplicitAccess;
end;

class function CoJwExplicitAccess.CreateRemote(const MachineName: string): IJwExplicitAccess;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwExplicitAccess) as IJwExplicitAccess;
end;

class function CoJwGuidArray.Create: IJwGuidArray;
begin
  Result := CreateComObject(CLASS_JwGuidArray) as IJwGuidArray;
end;

class function CoJwGuidArray.CreateRemote(const MachineName: string): IJwGuidArray;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwGuidArray) as IJwGuidArray;
end;

class function CoJwInheritedFromRecord.Create: IJwInheritedFromRecord;
begin
  Result := CreateComObject(CLASS_JwInheritedFromRecord) as IJwInheritedFromRecord;
end;

class function CoJwInheritedFromRecord.CreateRemote(const MachineName: string): IJwInheritedFromRecord;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwInheritedFromRecord) as IJwInheritedFromRecord;
end;

class function CoJwInheritedFromArray.Create: IJwInheritedFromArray;
begin
  Result := CreateComObject(CLASS_JwInheritedFromArray) as IJwInheritedFromArray;
end;

class function CoJwInheritedFromArray.CreateRemote(const MachineName: string): IJwInheritedFromArray;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwInheritedFromArray) as IJwInheritedFromArray;
end;

class function CoJwRootTuple.Create: IJwRootTuple;
begin
  Result := CreateComObject(CLASS_JwRootTuple) as IJwRootTuple;
end;

class function CoJwRootTuple.CreateRemote(const MachineName: string): IJwRootTuple;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwRootTuple) as IJwRootTuple;
end;

class function CoJwKeyRootTupleArray.Create: IJwKeyRootTupleArray;
begin
  Result := CreateComObject(CLASS_JwKeyRootTupleArray) as IJwKeyRootTupleArray;
end;

class function CoJwKeyRootTupleArray.CreateRemote(const MachineName: string): IJwKeyRootTupleArray;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwKeyRootTupleArray) as IJwKeyRootTupleArray;
end;

class function CoJwAccessMaskArray.Create: IJwAccessMaskArray;
begin
  Result := CreateComObject(CLASS_JwAccessMaskArray) as IJwAccessMaskArray;
end;

class function CoJwAccessMaskArray.CreateRemote(const MachineName: string): IJwAccessMaskArray;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwAccessMaskArray) as IJwAccessMaskArray;
end;

class function CoJwObjectTypeList.Create: IJwObjectTypeList;
begin
  Result := CreateComObject(CLASS_JwObjectTypeList) as IJwObjectTypeList;
end;

class function CoJwObjectTypeList.CreateRemote(const MachineName: string): IJwObjectTypeList;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwObjectTypeList) as IJwObjectTypeList;
end;

class function CoJwObjectTypeListArray.Create: IJwObjectTypeListArray;
begin
  Result := CreateComObject(CLASS_JwObjectTypeListArray) as IJwObjectTypeListArray;
end;

class function CoJwObjectTypeListArray.CreateRemote(const MachineName: string): IJwObjectTypeListArray;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwObjectTypeListArray) as IJwObjectTypeListArray;
end;

class function CoJwSIDInfoData.Create: IJwSIDInfoData;
begin
  Result := CreateComObject(CLASS_JwSIDInfoData) as IJwSIDInfoData;
end;

class function CoJwSIDInfoData.CreateRemote(const MachineName: string): IJwSIDInfoData;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwSIDInfoData) as IJwSIDInfoData;
end;

class function CoJwSIDInfoDataArray.Create: IJwSIDInfoDataArray;
begin
  Result := CreateComObject(CLASS_JwSIDInfoDataArray) as IJwSIDInfoDataArray;
end;

class function CoJwSIDInfoDataArray.CreateRemote(const MachineName: string): IJwSIDInfoDataArray;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwSIDInfoDataArray) as IJwSIDInfoDataArray;
end;

end.
