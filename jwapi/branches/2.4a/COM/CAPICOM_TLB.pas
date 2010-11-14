unit CAPICOM_TLB;

// ************************************************************************ //
// WARNUNG
// -------
// Die in dieser Datei deklarierten Typen wurden aus Daten einer Typbibliothek
// generiert. Wenn diese Typbibliothek explizit oder indirekt (über eine
// andere Typbibliothek) reimportiert wird oder wenn der Befehl
// 'Aktualisieren' im Typbibliotheks-Editor während des Bearbeitens der
// Typbibliothek aktiviert ist, wird der Inhalt dieser Datei neu generiert und
// alle manuell vorgenommenen Änderungen gehen verloren.
// ************************************************************************ //

// $Rev: 17244 $
// Datei am 11.07.2010 10:52:34 erzeugt aus der unten beschriebenen Typbibliothek.

// ************************************************************************  //
// Typbib.: C:\Program Files (x86)\Common Files\Microsoft Shared\CAPICOM\CapiCom.dll (1)
// LIBID: {BD26B198-EE42-4725-9B23-AFA912434229}
// LCID: 0
// Hilfedatei:
// Hilfe-String: CAPICOM v2.1 Type Library
// Liste der Abhäng.:
//   (1) v2.0 stdole, (C:\Windows\system32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit muss ohne Typüberprüfung für Zeiger compiliert werden.
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, StdVCL, Variants;

// *********************************************************************//
// In der Typbibliothek deklarierte GUIDS. Die folgenden Präfixe werden verwendet:
//   Typbibliotheken      : LIBID_xxxx
//   CoClasses            : CLASS_xxxx
//   DISPInterfaces       : DIID_xxxx
//   Nicht-DISP-Interfaces: IID_xxxx
// *********************************************************************//
const
  // Haupt- und Nebenversionen der Typbibliothek
  CAPICOMMajorVersion = 2;
  CAPICOMMinorVersion = 1;

  LIBID_CAPICOM: TGUID = '{BD26B198-EE42-4725-9B23-AFA912434229}';

  IID_ISettings: TGUID = '{A24104F5-46D0-4C0F-926D-665565908E91}';
  CLASS_Settings: TGUID = '{A996E48C-D3DC-4244-89F7-AFA33EC60679}';
  IID_IEKU: TGUID = '{976B7E6D-1002-4051-BFD4-824A74BD74E2}';
  CLASS_EKU: TGUID = '{8535F9A1-738A-40D0-8FB1-10CC8F74E7D3}';
  IID_IEKUs: TGUID = '{47C87CEC-8C4B-4E3C-8D22-34280274EFD1}';
  CLASS_EKUs: TGUID = '{F1800663-5BFC-4D1A-8D44-56CE02DDA34F}';
  IID_IKeyUsage: TGUID = '{41DD35A8-9FF9-45A6-9A7C-F65B2F085D1F}';
  CLASS_KeyUsage: TGUID = '{9226C95C-38BE-4CC4-B3A2-A867F5199C13}';
  IID_IExtendedKeyUsage: TGUID = '{7289D408-987D-45D1-8DEE-CF9E91C2E90E}';
  CLASS_ExtendedKeyUsage: TGUID = '{42C18607-1B4B-4126-8F1B-76E2DC7F631A}';
  IID_IBasicConstraints: TGUID = '{4E298C47-ABA6-459E-851B-993D6C626EAD}';
  CLASS_BasicConstraints: TGUID = '{C05AAC6E-3A58-45A9-A203-56952E961E48}';
  IID_ICertificateStatus: TGUID = '{AB769053-6D38-49D4-86EF-5FA85ED3AF27}';
  IID_ICertificateStatus2: TGUID = '{BF95660E-F743-4EAC-9DE5-960787A4606C}';
  IID_ICertificateStatus3: TGUID = '{A4EAB890-0786-406B-9B31-2746F31F8D87}';
  CLASS_CertificateStatus: TGUID = '{0EF24D18-BD9B-47D4-9458-E05B489FB7BA}';
  IID_IOIDs: TGUID = '{DA55E8FC-8E27-451B-AEA8-1470D80FAD42}';
  IID_IOID: TGUID = '{208E5E9B-58B1-4086-970F-161B582A846F}';
  IID_ICertificates: TGUID = '{68646716-BDA0-4046-AB82-4444BC93B84A}';
  IID_ICertificate: TGUID = '{0BBA0B86-766C-4755-A443-243FF2BD8D29}';
  IID_ICertificate2: TGUID = '{6FE450DC-AD32-48D4-A366-01EE7E0B1374}';
  IID_ICertContext: TGUID = '{9E7D3477-4F63-423E-8A45-E13B2BB851A2}';
  IID_ITemplate: TGUID = '{5F10FFCE-C922-476F-AA76-DF99D5BDFA2C}';
  IID_IPublicKey: TGUID = '{72BF9ADA-6817-4C31-B43E-25F7C7B091F4}';
  IID_IEncodedData: TGUID = '{D3D460F2-E7F3-4AF3-8EC6-8EB68C61C567}';
  IID_IPrivateKey: TGUID = '{659DEDC3-6C85-42DB-8527-EFCB21742862}';
  IID_IExtensions: TGUID = '{BC530D61-E692-4225-9E7A-07B90B45856A}';
  IID_IExtendedProperties: TGUID = '{3B096E87-6218-4A3B-A880-F6CB951E7805}';
  IID_IExtendedProperty: TGUID = '{ECB8A5C8-562C-4989-B49D-FA37D40F8FC4}';
  CLASS_Certificate: TGUID = '{9171C115-7DD9-46BA-B1E5-0ED50AFFC1B8}';
  IID_ICertificates2: TGUID = '{7B57C04B-1786-4B30-A7B6-36235CD58A14}';
  IID_ICCertificates: TGUID = '{EBDC6DC2-684D-4425-BBB7-CB4D15A088A7}';
  CLASS_Certificates: TGUID = '{3605B612-C3CF-4AB4-A426-2D853391DB2E}';
  IID_IChain: TGUID = '{77F6F881-5D3A-4F2F-AEF0-E4A2F9AA689D}';
  IID_IChain2: TGUID = '{CA65D842-2110-4073-AEE3-D0AA5F56C421}';
  IID_IChainContext: TGUID = '{B27FFB30-432E-4585-A3FD-72530108CBFD}';
  CLASS_Chain: TGUID = '{550C8FFB-4DC0-4756-828C-862E6D0AE74F}';
  IID_IStore: TGUID = '{E860EF75-1B63-4254-AF47-960DAA3DD337}';
  IID_IStore2: TGUID = '{4DA6ABC4-BDCD-4317-B650-262075B93A9C}';
  IID_IStore3: TGUID = '{F701F8EC-31C7-48FB-B621-5DE417C3A607}';
  IID_ICertStore: TGUID = '{BB3ECB9C-A83A-445C-BDB5-EFBEF691B731}';
  CLASS_Store: TGUID = '{91D221C4-0CD4-461C-A728-01D509321556}';
  IID_IAttribute: TGUID = '{B17A8D78-B5A6-45F7-BA21-01AB94B08415}';
  CLASS_Attribute: TGUID = '{54BA1E8F-818D-407F-949D-BAE1692C5C18}';
  IID_IAttributes: TGUID = '{6ADC653E-D5B9-422A-991A-A2B0119CEDAC}';
  CLASS_Attributes: TGUID = '{933013A9-64C8-4485-ACEF-4908C3692A33}';
  IID_ISigner: TGUID = '{51017B88-1913-49AD-82BE-6BB7C417DCF2}';
  IID_ISigner2: TGUID = '{625B1F55-C720-41D6-9ECF-BA59F9B85F17}';
  IID_ICSigner: TGUID = '{8F83F792-014C-4E22-BD57-5C381E622F34}';
  CLASS_Signer: TGUID = '{60A9863A-11FD-4080-850E-A8E184FC3A3C}';
  IID_ISigners: TGUID = '{5A0780F8-9E6B-4BB0-BF54-87CD9627A8B4}';
  CLASS_Signers: TGUID = '{1314C1D8-D3A8-4F8A-BED0-811FD7A8A633}';
  IID_ISignedData: TGUID = '{AE9C454B-FC65-4C10-B130-CD9B45BA948B}';
  CLASS_SignedData: TGUID = '{94AFFFCC-6C05-4814-B123-A941105AA77F}';
  IID_IAlgorithm: TGUID = '{BF3D04A9-B0DA-4153-B45E-6CCFA5AC715B}';
  CLASS_Algorithm: TGUID = '{A1EEF42F-5026-4A32-BC5C-2E552B70FD96}';
  IID_IRecipients: TGUID = '{A694C896-FC38-4C34-AE61-3B1A95984C14}';
  CLASS_Recipients: TGUID = '{96A1B8B0-8F9A-436A-84DE-E23CD6818DA5}';
  IID_IEnvelopedData: TGUID = '{F6CB6A20-CC18-4424-AE57-6F2AA3DC2059}';
  CLASS_EnvelopedData: TGUID = '{F3A12E08-EDE9-4160-8B51-334D982A9AD0}';
  IID_IEncryptedData: TGUID = '{C4778A66-972F-42E4-87C5-5CC16F7931CA}';
  CLASS_EncryptedData: TGUID = '{A440BD76-CFE1-4D46-AB1F-15F238437A3D}';
  CLASS_OID: TGUID = '{7BF3AC5C-CC84-429A-ACA5-74D916AD6B8C}';
  CLASS_OIDs: TGUID = '{FD661131-D716-4D15-A187-AEAAB161C8AD}';
  IID_INoticeNumbers: TGUID = '{EE2C051D-33A1-4157-86B4-9280E29782F2}';
  CLASS_NoticeNumbers: TGUID = '{A6FDF22A-8E00-464B-B15D-1A891D88B6ED}';
  IID_IQualifier: TGUID = '{3604C9DD-A22E-4A15-A469-8181C0C113DE}';
  CLASS_Qualifier: TGUID = '{E5F29B74-0902-4654-8A9A-21C5201DFA61}';
  IID_IQualifiers: TGUID = '{6B5A8AB6-597D-4398-AC63-1036EF546348}';
  CLASS_Qualifiers: TGUID = '{6C8006C0-F649-4783-B4A6-617DD0B270C7}';
  IID_IPolicyInformation: TGUID = '{8973710C-8411-4951-9E65-D45FD524FFDF}';
  CLASS_PolicyInformation: TGUID = '{0AAF88F4-1C22-4F65-A0E3-289D97DCE994}';
  IID_ICertificatePolicies: TGUID = '{CC7A72A7-C83A-4049-85F4-4292DE9DBFD3}';
  CLASS_CertificatePolicies: TGUID = '{988583C2-00C7-4D22-9241-E810E35EED1B}';
  CLASS_EncodedData: TGUID = '{7083C0AA-E7B9-48A4-8EFB-D6A109EBEC13}';
  IID_IExtension: TGUID = '{ED4E4ED4-FDD8-476E-AED9-5239E7948257}';
  CLASS_Extension: TGUID = '{D2359E2C-82D6-458F-BB6F-41559155E693}';
  CLASS_Extensions: TGUID = '{7C92E131-C1DC-4CA1-B02C-F513A08B41ED}';
  CLASS_ExtendedProperty: TGUID = '{9E7EA907-5810-4FCA-B817-CD0BBA8496FC}';
  CLASS_ExtendedProperties: TGUID = '{90E7143D-1A07-438D-8F85-3DBB0B73D314}';
  CLASS_Template: TGUID = '{61F0D2BD-373E-4F3C-962E-59B7C42C1B22}';
  CLASS_PublicKey: TGUID = '{301FC658-4055-4D76-9703-AA38E6D7236A}';
  IID_ICPrivateKey: TGUID = '{50F241B7-A8F2-4E0A-B982-4BD7DF0CCF3C}';
  CLASS_PrivateKey: TGUID = '{03ACC284-B757-4B8F-9951-86E600D2CD06}';
  IID_ISignedCode: TGUID = '{84FBCB95-5600-404C-9187-AC25B4CD6E94}';
  CLASS_SignedCode: TGUID = '{8C3E4934-9FA4-4693-9253-A29A05F99186}';
  IID_IHashedData: TGUID = '{9F7F23E8-06F4-42E8-B965-5CBD044BF27F}';
  CLASS_HashedData: TGUID = '{CE32ABF6-475D-41F6-BF82-D27F03E3D38B}';
  IID_IUtilities: TGUID = '{EB166CF6-2AE6-44DA-BD96-0C1635D183FE}';
  CLASS_Utilities: TGUID = '{22A85CE1-F011-4231-B9E4-7E7A0438F71B}';

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten Aufzählungen
// *********************************************************************//
// Konstanten für enum CAPICOM_ERROR_CODE
type
  CAPICOM_ERROR_CODE = TOleEnum;
const
  CAPICOM_E_ENCODE_INVALID_TYPE = $80880100;
  CAPICOM_E_EKU_INVALID_OID = $80880200;
  CAPICOM_E_EKU_OID_NOT_INITIALIZED = $80880201;
  CAPICOM_E_CERTIFICATE_NOT_INITIALIZED = $80880210;
  CAPICOM_E_CERTIFICATE_NO_PRIVATE_KEY = $80880211;
  CAPICOM_E_CHAIN_NOT_BUILT = $80880220;
  CAPICOM_E_STORE_NOT_OPENED = $80880230;
  CAPICOM_E_STORE_EMPTY = $80880231;
  CAPICOM_E_STORE_INVALID_OPEN_MODE = $80880232;
  CAPICOM_E_STORE_INVALID_SAVE_AS_TYPE = $80880233;
  CAPICOM_E_ATTRIBUTE_NAME_NOT_INITIALIZED = $80880240;
  CAPICOM_E_ATTRIBUTE_VALUE_NOT_INITIALIZED = $80880241;
  CAPICOM_E_ATTRIBUTE_INVALID_NAME = $80880242;
  CAPICOM_E_ATTRIBUTE_INVALID_VALUE = $80880243;
  CAPICOM_E_SIGNER_NOT_INITIALIZED = $80880250;
  CAPICOM_E_SIGNER_NOT_FOUND = $80880251;
  CAPICOM_E_SIGNER_NO_CHAIN = $80880252;
  CAPICOM_E_SIGNER_INVALID_USAGE = $80880253;
  CAPICOM_E_SIGN_NOT_INITIALIZED = $80880260;
  CAPICOM_E_SIGN_INVALID_TYPE = $80880261;
  CAPICOM_E_SIGN_NOT_SIGNED = $80880262;
  CAPICOM_E_INVALID_ALGORITHM = $80880270;
  CAPICOM_E_INVALID_KEY_LENGTH = $80880271;
  CAPICOM_E_ENVELOP_NOT_INITIALIZED = $80880280;
  CAPICOM_E_ENVELOP_INVALID_TYPE = $80880281;
  CAPICOM_E_ENVELOP_NO_RECIPIENT = $80880282;
  CAPICOM_E_ENVELOP_RECIPIENT_NOT_FOUND = $80880283;
  CAPICOM_E_ENCRYPT_NOT_INITIALIZED = $80880290;
  CAPICOM_E_ENCRYPT_INVALID_TYPE = $80880291;
  CAPICOM_E_ENCRYPT_NO_SECRET = $80880292;
  CAPICOM_E_NOT_SUPPORTED = $80880900;
  CAPICOM_E_UI_DISABLED = $80880901;
  CAPICOM_E_CANCELLED = $80880902;
  CAPICOM_E_NOT_ALLOWED = $80880903;
  CAPICOM_E_OUT_OF_RESOURCE = $80880904;
  CAPICOM_E_INTERNAL = $80880911;
  CAPICOM_E_UNKNOWN = $80880999;
  CAPICOM_E_PRIVATE_KEY_NOT_INITIALIZED = $80880300;
  CAPICOM_E_PRIVATE_KEY_NOT_EXPORTABLE = $80880301;
  CAPICOM_E_ENCODE_NOT_INITIALIZED = $80880320;
  CAPICOM_E_EXTENSION_NOT_INITIALIZED = $80880330;
  CAPICOM_E_PROPERTY_NOT_INITIALIZED = $80880340;
  CAPICOM_E_FIND_INVALID_TYPE = $80880350;
  CAPICOM_E_FIND_INVALID_PREDEFINED_POLICY = $80880351;
  CAPICOM_E_CODE_NOT_INITIALIZED = $80880360;
  CAPICOM_E_CODE_NOT_SIGNED = $80880361;
  CAPICOM_E_CODE_DESCRIPTION_NOT_INITIALIZED = $80880362;
  CAPICOM_E_CODE_DESCRIPTION_URL_NOT_INITIALIZED = $80880363;
  CAPICOM_E_CODE_INVALID_TIMESTAMP_URL = $80880364;
  CAPICOM_E_HASH_NO_DATA = $80880370;
  CAPICOM_E_INVALID_CONVERT_TYPE = $80880380;

// Konstanten für enum CAPICOM_ENCODING_TYPE
type
  CAPICOM_ENCODING_TYPE = TOleEnum;
const
  CAPICOM_ENCODE_BASE64 = $00000000;
  CAPICOM_ENCODE_BINARY = $00000001;
  CAPICOM_ENCODE_ANY = $FFFFFFFF;

// Konstanten für enum CAPICOM_EKU
type
  CAPICOM_EKU = TOleEnum;
const
  CAPICOM_EKU_OTHER = $00000000;
  CAPICOM_EKU_SERVER_AUTH = $00000001;
  CAPICOM_EKU_CLIENT_AUTH = $00000002;
  CAPICOM_EKU_CODE_SIGNING = $00000003;
  CAPICOM_EKU_EMAIL_PROTECTION = $00000004;
  CAPICOM_EKU_SMARTCARD_LOGON = $00000005;
  CAPICOM_EKU_ENCRYPTING_FILE_SYSTEM = $00000006;

// Konstanten für enum CAPICOM_CHECK_FLAG
type
  CAPICOM_CHECK_FLAG = TOleEnum;
const
  CAPICOM_CHECK_NONE = $00000000;
  CAPICOM_CHECK_TRUSTED_ROOT = $00000001;
  CAPICOM_CHECK_TIME_VALIDITY = $00000002;
  CAPICOM_CHECK_SIGNATURE_VALIDITY = $00000004;
  CAPICOM_CHECK_ONLINE_REVOCATION_STATUS = $00000008;
  CAPICOM_CHECK_OFFLINE_REVOCATION_STATUS = $00000010;
  CAPICOM_CHECK_COMPLETE_CHAIN = $00000020;
  CAPICOM_CHECK_NAME_CONSTRAINTS = $00000040;
  CAPICOM_CHECK_BASIC_CONSTRAINTS = $00000080;
  CAPICOM_CHECK_NESTED_VALIDITY_PERIOD = $00000100;
  CAPICOM_CHECK_ONLINE_ALL = $000001EF;
  CAPICOM_CHECK_OFFLINE_ALL = $000001F7;

// Konstanten für enum CAPICOM_CERT_INFO_TYPE
type
  CAPICOM_CERT_INFO_TYPE = TOleEnum;
const
  CAPICOM_CERT_INFO_SUBJECT_SIMPLE_NAME = $00000000;
  CAPICOM_CERT_INFO_ISSUER_SIMPLE_NAME = $00000001;
  CAPICOM_CERT_INFO_SUBJECT_EMAIL_NAME = $00000002;
  CAPICOM_CERT_INFO_ISSUER_EMAIL_NAME = $00000003;
  CAPICOM_CERT_INFO_SUBJECT_UPN = $00000004;
  CAPICOM_CERT_INFO_ISSUER_UPN = $00000005;
  CAPICOM_CERT_INFO_SUBJECT_DNS_NAME = $00000006;
  CAPICOM_CERT_INFO_ISSUER_DNS_NAME = $00000007;

// Konstanten für enum CAPICOM_STORE_LOCATION
type
  CAPICOM_STORE_LOCATION = TOleEnum;
const
  CAPICOM_MEMORY_STORE = $00000000;
  CAPICOM_LOCAL_MACHINE_STORE = $00000001;
  CAPICOM_CURRENT_USER_STORE = $00000002;
  CAPICOM_ACTIVE_DIRECTORY_USER_STORE = $00000003;
  CAPICOM_SMART_CARD_USER_STORE = $00000004;

// Konstanten für enum CAPICOM_STORE_OPEN_MODE
type
  CAPICOM_STORE_OPEN_MODE = TOleEnum;
const
  CAPICOM_STORE_OPEN_READ_ONLY = $00000000;
  CAPICOM_STORE_OPEN_READ_WRITE = $00000001;
  CAPICOM_STORE_OPEN_MAXIMUM_ALLOWED = $00000002;
  CAPICOM_STORE_OPEN_EXISTING_ONLY = $00000080;
  CAPICOM_STORE_OPEN_INCLUDE_ARCHIVED = $00000100;

// Konstanten für enum CAPICOM_ACTIVE_DIRECTORY_SEARCH_LOCATION
type
  CAPICOM_ACTIVE_DIRECTORY_SEARCH_LOCATION = TOleEnum;
const
  CAPICOM_SEARCH_ANY = $00000000;
  CAPICOM_SEARCH_GLOBAL_CATALOG = $00000001;
  CAPICOM_SEARCH_DEFAULT_DOMAIN = $00000002;

// Konstanten für enum CAPICOM_STORE_SAVE_AS_TYPE
type
  CAPICOM_STORE_SAVE_AS_TYPE = TOleEnum;
const
  CAPICOM_STORE_SAVE_AS_SERIALIZED = $00000000;
  CAPICOM_STORE_SAVE_AS_PKCS7 = $00000001;

// Konstanten für enum CAPICOM_ATTRIBUTE
type
  CAPICOM_ATTRIBUTE = TOleEnum;
const
  CAPICOM_AUTHENTICATED_ATTRIBUTE_SIGNING_TIME = $00000000;
  CAPICOM_AUTHENTICATED_ATTRIBUTE_DOCUMENT_NAME = $00000001;
  CAPICOM_AUTHENTICATED_ATTRIBUTE_DOCUMENT_DESCRIPTION = $00000002;

// Konstanten für enum CAPICOM_SIGNED_DATA_VERIFY_FLAG
type
  CAPICOM_SIGNED_DATA_VERIFY_FLAG = TOleEnum;
const
  CAPICOM_VERIFY_SIGNATURE_ONLY = $00000000;
  CAPICOM_VERIFY_SIGNATURE_AND_CERTIFICATE = $00000001;

// Konstanten für enum CAPICOM_ENCRYPTION_ALGORITHM
type
  CAPICOM_ENCRYPTION_ALGORITHM = TOleEnum;
const
  CAPICOM_ENCRYPTION_ALGORITHM_RC2 = $00000000;
  CAPICOM_ENCRYPTION_ALGORITHM_RC4 = $00000001;
  CAPICOM_ENCRYPTION_ALGORITHM_DES = $00000002;
  CAPICOM_ENCRYPTION_ALGORITHM_3DES = $00000003;
  CAPICOM_ENCRYPTION_ALGORITHM_AES = $00000004;

// Konstanten für enum CAPICOM_ENCRYPTION_KEY_LENGTH
type
  CAPICOM_ENCRYPTION_KEY_LENGTH = TOleEnum;
const
  CAPICOM_ENCRYPTION_KEY_LENGTH_MAXIMUM = $00000000;
  CAPICOM_ENCRYPTION_KEY_LENGTH_40_BITS = $00000001;
  CAPICOM_ENCRYPTION_KEY_LENGTH_56_BITS = $00000002;
  CAPICOM_ENCRYPTION_KEY_LENGTH_128_BITS = $00000003;
  CAPICOM_ENCRYPTION_KEY_LENGTH_192_BITS = $00000004;
  CAPICOM_ENCRYPTION_KEY_LENGTH_256_BITS = $00000005;

// Konstanten für enum CAPICOM_SECRET_TYPE
type
  CAPICOM_SECRET_TYPE = TOleEnum;
const
  CAPICOM_SECRET_PASSWORD = $00000000;

// Konstanten für enum CAPICOM_KEY_ALGORITHM
type
  CAPICOM_KEY_ALGORITHM = TOleEnum;
const
  CAPICOM_KEY_ALGORITHM_OTHER = $00000000;
  CAPICOM_KEY_ALGORITHM_RSA = $00000001;
  CAPICOM_KEY_ALGORITHM_DSS = $00000002;

// Konstanten für enum CAPICOM_OID
type
  CAPICOM_OID = TOleEnum;
const
  CAPICOM_OID_OTHER = $00000000;
  CAPICOM_OID_AUTHORITY_KEY_IDENTIFIER_EXTENSION = $00000001;
  CAPICOM_OID_KEY_ATTRIBUTES_EXTENSION = $00000002;
  CAPICOM_OID_CERT_POLICIES_95_EXTENSION = $00000003;
  CAPICOM_OID_KEY_USAGE_RESTRICTION_EXTENSION = $00000004;
  CAPICOM_OID_LEGACY_POLICY_MAPPINGS_EXTENSION = $00000005;
  CAPICOM_OID_SUBJECT_ALT_NAME_EXTENSION = $00000006;
  CAPICOM_OID_ISSUER_ALT_NAME_EXTENSION = $00000007;
  CAPICOM_OID_BASIC_CONSTRAINTS_EXTENSION = $00000008;
  CAPICOM_OID_SUBJECT_KEY_IDENTIFIER_EXTENSION = $00000009;
  CAPICOM_OID_KEY_USAGE_EXTENSION = $0000000A;
  CAPICOM_OID_PRIVATEKEY_USAGE_PERIOD_EXTENSION = $0000000B;
  CAPICOM_OID_SUBJECT_ALT_NAME2_EXTENSION = $0000000C;
  CAPICOM_OID_ISSUER_ALT_NAME2_EXTENSION = $0000000D;
  CAPICOM_OID_BASIC_CONSTRAINTS2_EXTENSION = $0000000E;
  CAPICOM_OID_NAME_CONSTRAINTS_EXTENSION = $0000000F;
  CAPICOM_OID_CRL_DIST_POINTS_EXTENSION = $00000010;
  CAPICOM_OID_CERT_POLICIES_EXTENSION = $00000011;
  CAPICOM_OID_POLICY_MAPPINGS_EXTENSION = $00000012;
  CAPICOM_OID_AUTHORITY_KEY_IDENTIFIER2_EXTENSION = $00000013;
  CAPICOM_OID_POLICY_CONSTRAINTS_EXTENSION = $00000014;
  CAPICOM_OID_ENHANCED_KEY_USAGE_EXTENSION = $00000015;
  CAPICOM_OID_CERTIFICATE_TEMPLATE_EXTENSION = $00000016;
  CAPICOM_OID_APPLICATION_CERT_POLICIES_EXTENSION = $00000017;
  CAPICOM_OID_APPLICATION_POLICY_MAPPINGS_EXTENSION = $00000018;
  CAPICOM_OID_APPLICATION_POLICY_CONSTRAINTS_EXTENSION = $00000019;
  CAPICOM_OID_AUTHORITY_INFO_ACCESS_EXTENSION = $0000001A;
  CAPICOM_OID_SERVER_AUTH_EKU = $00000064;
  CAPICOM_OID_CLIENT_AUTH_EKU = $00000065;
  CAPICOM_OID_CODE_SIGNING_EKU = $00000066;
  CAPICOM_OID_EMAIL_PROTECTION_EKU = $00000067;
  CAPICOM_OID_IPSEC_END_SYSTEM_EKU = $00000068;
  CAPICOM_OID_IPSEC_TUNNEL_EKU = $00000069;
  CAPICOM_OID_IPSEC_USER_EKU = $0000006A;
  CAPICOM_OID_TIME_STAMPING_EKU = $0000006B;
  CAPICOM_OID_CTL_USAGE_SIGNING_EKU = $0000006C;
  CAPICOM_OID_TIME_STAMP_SIGNING_EKU = $0000006D;
  CAPICOM_OID_SERVER_GATED_CRYPTO_EKU = $0000006E;
  CAPICOM_OID_ENCRYPTING_FILE_SYSTEM_EKU = $0000006F;
  CAPICOM_OID_EFS_RECOVERY_EKU = $00000070;
  CAPICOM_OID_WHQL_CRYPTO_EKU = $00000071;
  CAPICOM_OID_NT5_CRYPTO_EKU = $00000072;
  CAPICOM_OID_OEM_WHQL_CRYPTO_EKU = $00000073;
  CAPICOM_OID_EMBEDED_NT_CRYPTO_EKU = $00000074;
  CAPICOM_OID_ROOT_LIST_SIGNER_EKU = $00000075;
  CAPICOM_OID_QUALIFIED_SUBORDINATION_EKU = $00000076;
  CAPICOM_OID_KEY_RECOVERY_EKU = $00000077;
  CAPICOM_OID_DIGITAL_RIGHTS_EKU = $00000078;
  CAPICOM_OID_LICENSES_EKU = $00000079;
  CAPICOM_OID_LICENSE_SERVER_EKU = $0000007A;
  CAPICOM_OID_SMART_CARD_LOGON_EKU = $0000007B;
  CAPICOM_OID_PKIX_POLICY_QUALIFIER_CPS = $0000007C;
  CAPICOM_OID_PKIX_POLICY_QUALIFIER_USERNOTICE = $0000007D;

// Konstanten für enum CAPICOM_PROPID
type
  CAPICOM_PROPID = TOleEnum;
const
  CAPICOM_PROPID_UNKNOWN = $00000000;
  CAPICOM_PROPID_KEY_PROV_HANDLE = $00000001;
  CAPICOM_PROPID_KEY_PROV_INFO = $00000002;
  CAPICOM_PROPID_SHA1_HASH = $00000003;
  CAPICOM_PROPID_HASH_PROP = $00000003;
  CAPICOM_PROPID_MD5_HASH = $00000004;
  CAPICOM_PROPID_KEY_CONTEXT = $00000005;
  CAPICOM_PROPID_KEY_SPEC = $00000006;
  CAPICOM_PROPID_IE30_RESERVED = $00000007;
  CAPICOM_PROPID_PUBKEY_HASH_RESERVED = $00000008;
  CAPICOM_PROPID_ENHKEY_USAGE = $00000009;
  CAPICOM_PROPID_CTL_USAGE = $00000009;
  CAPICOM_PROPID_NEXT_UPDATE_LOCATION = $0000000A;
  CAPICOM_PROPID_FRIENDLY_NAME = $0000000B;
  CAPICOM_PROPID_PVK_FILE = $0000000C;
  CAPICOM_PROPID_DESCRIPTION = $0000000D;
  CAPICOM_PROPID_ACCESS_STATE = $0000000E;
  CAPICOM_PROPID_SIGNATURE_HASH = $0000000F;
  CAPICOM_PROPID_SMART_CARD_DATA = $00000010;
  CAPICOM_PROPID_EFS = $00000011;
  CAPICOM_PROPID_FORTEZZA_DATA = $00000012;
  CAPICOM_PROPID_ARCHIVED = $00000013;
  CAPICOM_PROPID_KEY_IDENTIFIER = $00000014;
  CAPICOM_PROPID_AUTO_ENROLL = $00000015;
  CAPICOM_PROPID_PUBKEY_ALG_PARA = $00000016;
  CAPICOM_PROPID_CROSS_CERT_DIST_POINTS = $00000017;
  CAPICOM_PROPID_ISSUER_PUBLIC_KEY_MD5_HASH = $00000018;
  CAPICOM_PROPID_SUBJECT_PUBLIC_KEY_MD5_HASH = $00000019;
  CAPICOM_PROPID_ENROLLMENT = $0000001A;
  CAPICOM_PROPID_DATE_STAMP = $0000001B;
  CAPICOM_PROPID_ISSUER_SERIAL_NUMBER_MD5_HASH = $0000001C;
  CAPICOM_PROPID_SUBJECT_NAME_MD5_HASH = $0000001D;
  CAPICOM_PROPID_EXTENDED_ERROR_INFO = $0000001E;
  CAPICOM_PROPID_RENEWAL = $00000040;
  CAPICOM_PROPID_ARCHIVED_KEY_HASH = $00000041;
  CAPICOM_PROPID_FIRST_RESERVED = $00000042;
  CAPICOM_PROPID_LAST_RESERVED = $00007FFF;
  CAPICOM_PROPID_FIRST_USER = $00008000;
  CAPICOM_PROPID_LAST_USER = $0000FFFF;

// Konstanten für enum CAPICOM_PROV_TYPE
type
  CAPICOM_PROV_TYPE = TOleEnum;
const
  CAPICOM_PROV_RSA_FULL = $00000001;
  CAPICOM_PROV_RSA_SIG = $00000002;
  CAPICOM_PROV_DSS = $00000003;
  CAPICOM_PROV_FORTEZZA = $00000004;
  CAPICOM_PROV_MS_EXCHANGE = $00000005;
  CAPICOM_PROV_SSL = $00000006;
  CAPICOM_PROV_RSA_SCHANNEL = $0000000C;
  CAPICOM_PROV_DSS_DH = $0000000D;
  CAPICOM_PROV_EC_ECDSA_SIG = $0000000E;
  CAPICOM_PROV_EC_ECNRA_SIG = $0000000F;
  CAPICOM_PROV_EC_ECDSA_FULL = $00000010;
  CAPICOM_PROV_EC_ECNRA_FULL = $00000011;
  CAPICOM_PROV_DH_SCHANNEL = $00000012;
  CAPICOM_PROV_SPYRUS_LYNKS = $00000014;
  CAPICOM_PROV_RNG = $00000015;
  CAPICOM_PROV_INTEL_SEC = $00000016;
  CAPICOM_PROV_REPLACE_OWF = $00000017;
  CAPICOM_PROV_RSA_AES = $00000018;

// Konstanten für enum CAPICOM_CERTIFICATE_SAVE_AS_TYPE
type
  CAPICOM_CERTIFICATE_SAVE_AS_TYPE = TOleEnum;
const
  CAPICOM_CERTIFICATE_SAVE_AS_PFX = $00000000;
  CAPICOM_CERTIFICATE_SAVE_AS_CER = $00000001;

// Konstanten für enum CAPICOM_CERTIFICATES_SAVE_AS_TYPE
type
  CAPICOM_CERTIFICATES_SAVE_AS_TYPE = TOleEnum;
const
  CAPICOM_CERTIFICATES_SAVE_AS_SERIALIZED = $00000000;
  CAPICOM_CERTIFICATES_SAVE_AS_PKCS7 = $00000001;
  CAPICOM_CERTIFICATES_SAVE_AS_PFX = $00000002;

// Konstanten für enum CAPICOM_CERTIFICATE_INCLUDE_OPTION
type
  CAPICOM_CERTIFICATE_INCLUDE_OPTION = TOleEnum;
const
  CAPICOM_CERTIFICATE_INCLUDE_CHAIN_EXCEPT_ROOT = $00000000;
  CAPICOM_CERTIFICATE_INCLUDE_WHOLE_CHAIN = $00000001;
  CAPICOM_CERTIFICATE_INCLUDE_END_ENTITY_ONLY = $00000002;

// Konstanten für enum CAPICOM_KEY_SPEC
type
  CAPICOM_KEY_SPEC = TOleEnum;
const
  CAPICOM_KEY_SPEC_KEYEXCHANGE = $00000001;
  CAPICOM_KEY_SPEC_SIGNATURE = $00000002;

// Konstanten für enum CAPICOM_KEY_LOCATION
type
  CAPICOM_KEY_LOCATION = TOleEnum;
const
  CAPICOM_CURRENT_USER_KEY = $00000000;
  CAPICOM_LOCAL_MACHINE_KEY = $00000001;

// Konstanten für enum CAPICOM_KEY_STORAGE_FLAG
type
  CAPICOM_KEY_STORAGE_FLAG = TOleEnum;
const
  CAPICOM_KEY_STORAGE_DEFAULT = $00000000;
  CAPICOM_KEY_STORAGE_EXPORTABLE = $00000001;
  CAPICOM_KEY_STORAGE_USER_PROTECTED = $00000002;

// Konstanten für enum CAPICOM_EXPORT_FLAG
type
  CAPICOM_EXPORT_FLAG = TOleEnum;
const
  CAPICOM_EXPORT_DEFAULT = $00000000;
  CAPICOM_EXPORT_IGNORE_PRIVATE_KEY_NOT_EXPORTABLE_ERROR = $00000001;

// Konstanten für enum CAPICOM_KEY_USAGE
type
  CAPICOM_KEY_USAGE = TOleEnum;
const
  CAPICOM_DIGITAL_SIGNATURE_KEY_USAGE = $00000080;
  CAPICOM_NON_REPUDIATION_KEY_USAGE = $00000040;
  CAPICOM_KEY_ENCIPHERMENT_KEY_USAGE = $00000020;
  CAPICOM_DATA_ENCIPHERMENT_KEY_USAGE = $00000010;
  CAPICOM_KEY_AGREEMENT_KEY_USAGE = $00000008;
  CAPICOM_KEY_CERT_SIGN_KEY_USAGE = $00000004;
  CAPICOM_OFFLINE_CRL_SIGN_KEY_USAGE = $00000002;
  CAPICOM_CRL_SIGN_KEY_USAGE = $00000002;
  CAPICOM_ENCIPHER_ONLY_KEY_USAGE = $00000001;
  CAPICOM_DECIPHER_ONLY_KEY_USAGE = $00008000;

// Konstanten für enum CAPICOM_CERTIFICATE_FIND_TYPE
type
  CAPICOM_CERTIFICATE_FIND_TYPE = TOleEnum;
const
  CAPICOM_CERTIFICATE_FIND_SHA1_HASH = $00000000;
  CAPICOM_CERTIFICATE_FIND_SUBJECT_NAME = $00000001;
  CAPICOM_CERTIFICATE_FIND_ISSUER_NAME = $00000002;
  CAPICOM_CERTIFICATE_FIND_ROOT_NAME = $00000003;
  CAPICOM_CERTIFICATE_FIND_TEMPLATE_NAME = $00000004;
  CAPICOM_CERTIFICATE_FIND_EXTENSION = $00000005;
  CAPICOM_CERTIFICATE_FIND_EXTENDED_PROPERTY = $00000006;
  CAPICOM_CERTIFICATE_FIND_APPLICATION_POLICY = $00000007;
  CAPICOM_CERTIFICATE_FIND_CERTIFICATE_POLICY = $00000008;
  CAPICOM_CERTIFICATE_FIND_TIME_VALID = $00000009;
  CAPICOM_CERTIFICATE_FIND_TIME_NOT_YET_VALID = $0000000A;
  CAPICOM_CERTIFICATE_FIND_TIME_EXPIRED = $0000000B;
  CAPICOM_CERTIFICATE_FIND_KEY_USAGE = $0000000C;

// Konstanten für enum CAPICOM_HASH_ALGORITHM
type
  CAPICOM_HASH_ALGORITHM = TOleEnum;
const
  CAPICOM_HASH_ALGORITHM_SHA1 = $00000000;
  CAPICOM_HASH_ALGORITHM_MD2 = $00000001;
  CAPICOM_HASH_ALGORITHM_MD4 = $00000002;
  CAPICOM_HASH_ALGORITHM_MD5 = $00000003;
  CAPICOM_HASH_ALGORITHM_SHA_256 = $00000004;
  CAPICOM_HASH_ALGORITHM_SHA_384 = $00000005;
  CAPICOM_HASH_ALGORITHM_SHA_512 = $00000006;

type

// *********************************************************************//
// Forward-Deklaration von in der Typbibliothek definierten Typen
// *********************************************************************//
  ISettings = interface;
  ISettingsDisp = dispinterface;
  IEKU = interface;
  IEKUDisp = dispinterface;
  IEKUs = interface;
  IEKUsDisp = dispinterface;
  IKeyUsage = interface;
  IKeyUsageDisp = dispinterface;
  IExtendedKeyUsage = interface;
  IExtendedKeyUsageDisp = dispinterface;
  IBasicConstraints = interface;
  IBasicConstraintsDisp = dispinterface;
  ICertificateStatus = interface;
  ICertificateStatusDisp = dispinterface;
  ICertificateStatus2 = interface;
  ICertificateStatus2Disp = dispinterface;
  ICertificateStatus3 = interface;
  ICertificateStatus3Disp = dispinterface;
  IOIDs = interface;
  IOIDsDisp = dispinterface;
  IOID = interface;
  IOIDDisp = dispinterface;
  ICertificates = interface;
  ICertificatesDisp = dispinterface;
  ICertificate = interface;
  ICertificateDisp = dispinterface;
  ICertificate2 = interface;
  ICertificate2Disp = dispinterface;
  ICertContext = interface;
  ITemplate = interface;
  ITemplateDisp = dispinterface;
  IPublicKey = interface;
  IPublicKeyDisp = dispinterface;
  IEncodedData = interface;
  IEncodedDataDisp = dispinterface;
  IPrivateKey = interface;
  IPrivateKeyDisp = dispinterface;
  IExtensions = interface;
  IExtensionsDisp = dispinterface;
  IExtendedProperties = interface;
  IExtendedPropertiesDisp = dispinterface;
  IExtendedProperty = interface;
  IExtendedPropertyDisp = dispinterface;
  ICertificates2 = interface;
  ICertificates2Disp = dispinterface;
  ICCertificates = interface;
  IChain = interface;
  IChainDisp = dispinterface;
  IChain2 = interface;
  IChain2Disp = dispinterface;
  IChainContext = interface;
  IStore = interface;
  IStoreDisp = dispinterface;
  IStore2 = interface;
  IStore2Disp = dispinterface;
  IStore3 = interface;
  IStore3Disp = dispinterface;
  ICertStore = interface;
  IAttribute = interface;
  IAttributeDisp = dispinterface;
  IAttributes = interface;
  IAttributesDisp = dispinterface;
  ISigner = interface;
  ISignerDisp = dispinterface;
  ISigner2 = interface;
  ISigner2Disp = dispinterface;
  ICSigner = interface;
  ISigners = interface;
  ISignersDisp = dispinterface;
  ISignedData = interface;
  ISignedDataDisp = dispinterface;
  IAlgorithm = interface;
  IAlgorithmDisp = dispinterface;
  IRecipients = interface;
  IRecipientsDisp = dispinterface;
  IEnvelopedData = interface;
  IEnvelopedDataDisp = dispinterface;
  IEncryptedData = interface;
  IEncryptedDataDisp = dispinterface;
  INoticeNumbers = interface;
  INoticeNumbersDisp = dispinterface;
  IQualifier = interface;
  IQualifierDisp = dispinterface;
  IQualifiers = interface;
  IQualifiersDisp = dispinterface;
  IPolicyInformation = interface;
  IPolicyInformationDisp = dispinterface;
  ICertificatePolicies = interface;
  ICertificatePoliciesDisp = dispinterface;
  IExtension = interface;
  IExtensionDisp = dispinterface;
  ICPrivateKey = interface;
  ISignedCode = interface;
  ISignedCodeDisp = dispinterface;
  IHashedData = interface;
  IHashedDataDisp = dispinterface;
  IUtilities = interface;
  IUtilitiesDisp = dispinterface;

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten CoClasses
// (HINWEIS: Hier wird jede CoClass ihrem Standard-Interface zugewiesen)
// *********************************************************************//
  Settings = ISettings;
  EKU = IEKU;
  EKUs = IEKUs;
  KeyUsage = IKeyUsage;
  ExtendedKeyUsage = IExtendedKeyUsage;
  BasicConstraints = IBasicConstraints;
  CertificateStatus = ICertificateStatus3;
  Certificate = ICertificate2;
  Certificates = ICertificates2;
  Chain = IChain2;
  Store = IStore3;
  Attribute = IAttribute;
  Attributes = IAttributes;
  Signer = ISigner2;
  Signers = ISigners;
  SignedData = ISignedData;
  Algorithm = IAlgorithm;
  Recipients = IRecipients;
  EnvelopedData = IEnvelopedData;
  EncryptedData = IEncryptedData;
  OID = IOID;
  OIDs = IOIDs;
  NoticeNumbers = INoticeNumbers;
  Qualifier = IQualifier;
  Qualifiers = IQualifiers;
  PolicyInformation = IPolicyInformation;
  CertificatePolicies = ICertificatePolicies;
  EncodedData = IEncodedData;
  Extension = IExtension;
  Extensions = IExtensions;
  ExtendedProperty = IExtendedProperty;
  ExtendedProperties = IExtendedProperties;
  Template = ITemplate;
  PublicKey = IPublicKey;
  PrivateKey = IPrivateKey;
  SignedCode = ISignedCode;
  HashedData = IHashedData;
  Utilities = IUtilities;


// *********************************************************************//
// Deklaration von Strukturen, Unions und Aliasen.
// *********************************************************************//
  PUserType1 = ^_CRYPT_KEY_PROV_INFO; {*}
  PUserType2 = ^_CERT_KEY_CONTEXT; {*}

  _CRYPT_KEY_PROV_PARAM = record
    dwParam: LongWord;
    pbData: ^Byte;
    cbData: LongWord;
    dwFlags: LongWord;
  end;

  _CRYPT_KEY_PROV_INFO = record
    pwszContainerName: PWideChar;
    pwszProvName: PWideChar;
    dwProvType: LongWord;
    dwFlags: LongWord;
    cProvParam: LongWord;
    rgProvParam: ^_CRYPT_KEY_PROV_PARAM;
    dwKeySpec: LongWord;
  end;

  __MIDL___MIDL_itf_capicom_0001_0064_0024 = record
    case Integer of
      0: (hCryptProv: LongWord);
      1: (hNCryptKey: LongWord);
  end;

  _CERT_KEY_CONTEXT = record
    cbSize: LongWord;
    __MIDL____MIDL_itf_capicom_0001_00640106: __MIDL___MIDL_itf_capicom_0001_0064_0024;
    dwKeySpec: LongWord;
  end;


// *********************************************************************//
// Interface: ISettings
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A24104F5-46D0-4C0F-926D-665565908E91}
// *********************************************************************//
  ISettings = interface(IDispatch)
    ['{A24104F5-46D0-4C0F-926D-665565908E91}']
    function Get_EnablePromptForCertificateUI: WordBool; safecall;
    procedure Set_EnablePromptForCertificateUI(pVal: WordBool); safecall;
    function Get_ActiveDirectorySearchLocation: CAPICOM_ACTIVE_DIRECTORY_SEARCH_LOCATION; safecall;
    procedure Set_ActiveDirectorySearchLocation(pVal: CAPICOM_ACTIVE_DIRECTORY_SEARCH_LOCATION); safecall;
    property EnablePromptForCertificateUI: WordBool read Get_EnablePromptForCertificateUI write Set_EnablePromptForCertificateUI;
    property ActiveDirectorySearchLocation: CAPICOM_ACTIVE_DIRECTORY_SEARCH_LOCATION read Get_ActiveDirectorySearchLocation write Set_ActiveDirectorySearchLocation;
  end;

// *********************************************************************//
// DispIntf:  ISettingsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A24104F5-46D0-4C0F-926D-665565908E91}
// *********************************************************************//
  ISettingsDisp = dispinterface
    ['{A24104F5-46D0-4C0F-926D-665565908E91}']
    property EnablePromptForCertificateUI: WordBool dispid 1;
    property ActiveDirectorySearchLocation: CAPICOM_ACTIVE_DIRECTORY_SEARCH_LOCATION dispid 2;
  end;

// *********************************************************************//
// Interface: IEKU
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {976B7E6D-1002-4051-BFD4-824A74BD74E2}
// *********************************************************************//
  IEKU = interface(IDispatch)
    ['{976B7E6D-1002-4051-BFD4-824A74BD74E2}']
    function Get_Name: CAPICOM_EKU; safecall;
    procedure Set_Name(pVal: CAPICOM_EKU); safecall;
    function Get_OID: WideString; safecall;
    procedure Set_OID(const pVal: WideString); safecall;
    property Name: CAPICOM_EKU read Get_Name write Set_Name;
    property OID: WideString read Get_OID write Set_OID;
  end;

// *********************************************************************//
// DispIntf:  IEKUDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {976B7E6D-1002-4051-BFD4-824A74BD74E2}
// *********************************************************************//
  IEKUDisp = dispinterface
    ['{976B7E6D-1002-4051-BFD4-824A74BD74E2}']
    property Name: CAPICOM_EKU dispid 0;
    property OID: WideString dispid 1;
  end;

// *********************************************************************//
// Interface: IEKUs
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {47C87CEC-8C4B-4E3C-8D22-34280274EFD1}
// *********************************************************************//
  IEKUs = interface(IDispatch)
    ['{47C87CEC-8C4B-4E3C-8D22-34280274EFD1}']
    function Get_Item(Index: Integer): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Item[Index: Integer]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IEKUsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {47C87CEC-8C4B-4E3C-8D22-34280274EFD1}
// *********************************************************************//
  IEKUsDisp = dispinterface
    ['{47C87CEC-8C4B-4E3C-8D22-34280274EFD1}']
    property Item[Index: Integer]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IKeyUsage
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {41DD35A8-9FF9-45A6-9A7C-F65B2F085D1F}
// *********************************************************************//
  IKeyUsage = interface(IDispatch)
    ['{41DD35A8-9FF9-45A6-9A7C-F65B2F085D1F}']
    function Get_IsPresent: WordBool; safecall;
    function Get_IsCritical: WordBool; safecall;
    function Get_IsDigitalSignatureEnabled: WordBool; safecall;
    function Get_IsNonRepudiationEnabled: WordBool; safecall;
    function Get_IsKeyEnciphermentEnabled: WordBool; safecall;
    function Get_IsDataEnciphermentEnabled: WordBool; safecall;
    function Get_IsKeyAgreementEnabled: WordBool; safecall;
    function Get_IsKeyCertSignEnabled: WordBool; safecall;
    function Get_IsCRLSignEnabled: WordBool; safecall;
    function Get_IsEncipherOnlyEnabled: WordBool; safecall;
    function Get_IsDecipherOnlyEnabled: WordBool; safecall;
    property IsPresent: WordBool read Get_IsPresent;
    property IsCritical: WordBool read Get_IsCritical;
    property IsDigitalSignatureEnabled: WordBool read Get_IsDigitalSignatureEnabled;
    property IsNonRepudiationEnabled: WordBool read Get_IsNonRepudiationEnabled;
    property IsKeyEnciphermentEnabled: WordBool read Get_IsKeyEnciphermentEnabled;
    property IsDataEnciphermentEnabled: WordBool read Get_IsDataEnciphermentEnabled;
    property IsKeyAgreementEnabled: WordBool read Get_IsKeyAgreementEnabled;
    property IsKeyCertSignEnabled: WordBool read Get_IsKeyCertSignEnabled;
    property IsCRLSignEnabled: WordBool read Get_IsCRLSignEnabled;
    property IsEncipherOnlyEnabled: WordBool read Get_IsEncipherOnlyEnabled;
    property IsDecipherOnlyEnabled: WordBool read Get_IsDecipherOnlyEnabled;
  end;

// *********************************************************************//
// DispIntf:  IKeyUsageDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {41DD35A8-9FF9-45A6-9A7C-F65B2F085D1F}
// *********************************************************************//
  IKeyUsageDisp = dispinterface
    ['{41DD35A8-9FF9-45A6-9A7C-F65B2F085D1F}']
    property IsPresent: WordBool readonly dispid 1;
    property IsCritical: WordBool readonly dispid 2;
    property IsDigitalSignatureEnabled: WordBool readonly dispid 3;
    property IsNonRepudiationEnabled: WordBool readonly dispid 4;
    property IsKeyEnciphermentEnabled: WordBool readonly dispid 5;
    property IsDataEnciphermentEnabled: WordBool readonly dispid 6;
    property IsKeyAgreementEnabled: WordBool readonly dispid 7;
    property IsKeyCertSignEnabled: WordBool readonly dispid 8;
    property IsCRLSignEnabled: WordBool readonly dispid 9;
    property IsEncipherOnlyEnabled: WordBool readonly dispid 10;
    property IsDecipherOnlyEnabled: WordBool readonly dispid 11;
  end;

// *********************************************************************//
// Interface: IExtendedKeyUsage
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7289D408-987D-45D1-8DEE-CF9E91C2E90E}
// *********************************************************************//
  IExtendedKeyUsage = interface(IDispatch)
    ['{7289D408-987D-45D1-8DEE-CF9E91C2E90E}']
    function Get_IsPresent: WordBool; safecall;
    function Get_IsCritical: WordBool; safecall;
    function Get_EKUs: IEKUs; safecall;
    property IsPresent: WordBool read Get_IsPresent;
    property IsCritical: WordBool read Get_IsCritical;
    property EKUs: IEKUs read Get_EKUs;
  end;

// *********************************************************************//
// DispIntf:  IExtendedKeyUsageDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7289D408-987D-45D1-8DEE-CF9E91C2E90E}
// *********************************************************************//
  IExtendedKeyUsageDisp = dispinterface
    ['{7289D408-987D-45D1-8DEE-CF9E91C2E90E}']
    property IsPresent: WordBool readonly dispid 1;
    property IsCritical: WordBool readonly dispid 2;
    property EKUs: IEKUs readonly dispid 3;
  end;

// *********************************************************************//
// Interface: IBasicConstraints
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4E298C47-ABA6-459E-851B-993D6C626EAD}
// *********************************************************************//
  IBasicConstraints = interface(IDispatch)
    ['{4E298C47-ABA6-459E-851B-993D6C626EAD}']
    function Get_IsPresent: WordBool; safecall;
    function Get_IsCritical: WordBool; safecall;
    function Get_IsCertificateAuthority: WordBool; safecall;
    function Get_IsPathLenConstraintPresent: WordBool; safecall;
    function Get_PathLenConstraint: Integer; safecall;
    property IsPresent: WordBool read Get_IsPresent;
    property IsCritical: WordBool read Get_IsCritical;
    property IsCertificateAuthority: WordBool read Get_IsCertificateAuthority;
    property IsPathLenConstraintPresent: WordBool read Get_IsPathLenConstraintPresent;
    property PathLenConstraint: Integer read Get_PathLenConstraint;
  end;

// *********************************************************************//
// DispIntf:  IBasicConstraintsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4E298C47-ABA6-459E-851B-993D6C626EAD}
// *********************************************************************//
  IBasicConstraintsDisp = dispinterface
    ['{4E298C47-ABA6-459E-851B-993D6C626EAD}']
    property IsPresent: WordBool readonly dispid 1;
    property IsCritical: WordBool readonly dispid 2;
    property IsCertificateAuthority: WordBool readonly dispid 3;
    property IsPathLenConstraintPresent: WordBool readonly dispid 4;
    property PathLenConstraint: Integer readonly dispid 5;
  end;

// *********************************************************************//
// Interface: ICertificateStatus
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {AB769053-6D38-49D4-86EF-5FA85ED3AF27}
// *********************************************************************//
  ICertificateStatus = interface(IDispatch)
    ['{AB769053-6D38-49D4-86EF-5FA85ED3AF27}']
    function Get_Result: WordBool; safecall;
    function Get_CheckFlag: CAPICOM_CHECK_FLAG; safecall;
    procedure Set_CheckFlag(pVal: CAPICOM_CHECK_FLAG); safecall;
    function EKU: IEKU; safecall;
    property Result: WordBool read Get_Result;
    property CheckFlag: CAPICOM_CHECK_FLAG read Get_CheckFlag write Set_CheckFlag;
  end;

// *********************************************************************//
// DispIntf:  ICertificateStatusDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {AB769053-6D38-49D4-86EF-5FA85ED3AF27}
// *********************************************************************//
  ICertificateStatusDisp = dispinterface
    ['{AB769053-6D38-49D4-86EF-5FA85ED3AF27}']
    property Result: WordBool readonly dispid 0;
    property CheckFlag: CAPICOM_CHECK_FLAG dispid 1;
    function EKU: IEKU; dispid 2;
  end;

// *********************************************************************//
// Interface: ICertificateStatus2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BF95660E-F743-4EAC-9DE5-960787A4606C}
// *********************************************************************//
  ICertificateStatus2 = interface(ICertificateStatus)
    ['{BF95660E-F743-4EAC-9DE5-960787A4606C}']
    function Get_VerificationTime: TDateTime; safecall;
    procedure Set_VerificationTime(pVal: TDateTime); safecall;
    function Get_UrlRetrievalTimeout: Integer; safecall;
    procedure Set_UrlRetrievalTimeout(pVal: Integer); safecall;
    function CertificatePolicies: IOIDs; safecall;
    function ApplicationPolicies: IOIDs; safecall;
    property VerificationTime: TDateTime read Get_VerificationTime write Set_VerificationTime;
    property UrlRetrievalTimeout: Integer read Get_UrlRetrievalTimeout write Set_UrlRetrievalTimeout;
  end;

// *********************************************************************//
// DispIntf:  ICertificateStatus2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BF95660E-F743-4EAC-9DE5-960787A4606C}
// *********************************************************************//
  ICertificateStatus2Disp = dispinterface
    ['{BF95660E-F743-4EAC-9DE5-960787A4606C}']
    property VerificationTime: TDateTime dispid 3;
    property UrlRetrievalTimeout: Integer dispid 4;
    function CertificatePolicies: IOIDs; dispid 5;
    function ApplicationPolicies: IOIDs; dispid 6;
    property Result: WordBool readonly dispid 0;
    property CheckFlag: CAPICOM_CHECK_FLAG dispid 1;
    function EKU: IEKU; dispid 2;
  end;

// *********************************************************************//
// Interface: ICertificateStatus3
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A4EAB890-0786-406B-9B31-2746F31F8D87}
// *********************************************************************//
  ICertificateStatus3 = interface(ICertificateStatus2)
    ['{A4EAB890-0786-406B-9B31-2746F31F8D87}']
    function Get_ValidationCertificates: ICertificates; safecall;
    property ValidationCertificates: ICertificates read Get_ValidationCertificates;
  end;

// *********************************************************************//
// DispIntf:  ICertificateStatus3Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A4EAB890-0786-406B-9B31-2746F31F8D87}
// *********************************************************************//
  ICertificateStatus3Disp = dispinterface
    ['{A4EAB890-0786-406B-9B31-2746F31F8D87}']
    property ValidationCertificates: ICertificates readonly dispid 7;
    property VerificationTime: TDateTime dispid 3;
    property UrlRetrievalTimeout: Integer dispid 4;
    function CertificatePolicies: IOIDs; dispid 5;
    function ApplicationPolicies: IOIDs; dispid 6;
    property Result: WordBool readonly dispid 0;
    property CheckFlag: CAPICOM_CHECK_FLAG dispid 1;
    function EKU: IEKU; dispid 2;
  end;

// *********************************************************************//
// Interface: IOIDs
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DA55E8FC-8E27-451B-AEA8-1470D80FAD42}
// *********************************************************************//
  IOIDs = interface(IDispatch)
    ['{DA55E8FC-8E27-451B-AEA8-1470D80FAD42}']
    function Get_Item(Index: OleVariant): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    procedure Add(const pVal: IOID); safecall;
    procedure Remove(Index: OleVariant); safecall;
    procedure Clear; safecall;
    property Item[Index: OleVariant]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IOIDsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DA55E8FC-8E27-451B-AEA8-1470D80FAD42}
// *********************************************************************//
  IOIDsDisp = dispinterface
    ['{DA55E8FC-8E27-451B-AEA8-1470D80FAD42}']
    property Item[Index: OleVariant]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Add(const pVal: IOID); dispid 2;
    procedure Remove(Index: OleVariant); dispid 3;
    procedure Clear; dispid 4;
  end;

// *********************************************************************//
// Interface: IOID
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {208E5E9B-58B1-4086-970F-161B582A846F}
// *********************************************************************//
  IOID = interface(IDispatch)
    ['{208E5E9B-58B1-4086-970F-161B582A846F}']
    function Get_Name: CAPICOM_OID; safecall;
    procedure Set_Name(pVal: CAPICOM_OID); safecall;
    function Get_FriendlyName: WideString; safecall;
    procedure Set_FriendlyName(const pVal: WideString); safecall;
    function Get_Value: WideString; safecall;
    procedure Set_Value(const pVal: WideString); safecall;
    property Name: CAPICOM_OID read Get_Name write Set_Name;
    property FriendlyName: WideString read Get_FriendlyName write Set_FriendlyName;
    property Value: WideString read Get_Value write Set_Value;
  end;

// *********************************************************************//
// DispIntf:  IOIDDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {208E5E9B-58B1-4086-970F-161B582A846F}
// *********************************************************************//
  IOIDDisp = dispinterface
    ['{208E5E9B-58B1-4086-970F-161B582A846F}']
    property Name: CAPICOM_OID dispid 0;
    property FriendlyName: WideString dispid 1;
    property Value: WideString dispid 2;
  end;

// *********************************************************************//
// Interface: ICertificates
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {68646716-BDA0-4046-AB82-4444BC93B84A}
// *********************************************************************//
  ICertificates = interface(IDispatch)
    ['{68646716-BDA0-4046-AB82-4444BC93B84A}']
    function Get_Item(Index: Integer): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Item[Index: Integer]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ICertificatesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {68646716-BDA0-4046-AB82-4444BC93B84A}
// *********************************************************************//
  ICertificatesDisp = dispinterface
    ['{68646716-BDA0-4046-AB82-4444BC93B84A}']
    property Item[Index: Integer]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ICertificate
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0BBA0B86-766C-4755-A443-243FF2BD8D29}
// *********************************************************************//
  ICertificate = interface(IDispatch)
    ['{0BBA0B86-766C-4755-A443-243FF2BD8D29}']
    function Get_Version: Integer; safecall;
    function Get_SerialNumber: WideString; safecall;
    function Get_SubjectName: WideString; safecall;
    function Get_IssuerName: WideString; safecall;
    function Get_ValidFromDate: TDateTime; safecall;
    function Get_ValidToDate: TDateTime; safecall;
    function Get_Thumbprint: WideString; safecall;
    function HasPrivateKey: WordBool; safecall;
    function GetInfo(InfoType: CAPICOM_CERT_INFO_TYPE): WideString; safecall;
    function IsValid: ICertificateStatus; safecall;
    function KeyUsage: IKeyUsage; safecall;
    function ExtendedKeyUsage: IExtendedKeyUsage; safecall;
    function BasicConstraints: IBasicConstraints; safecall;
    function Export(EncodingType: CAPICOM_ENCODING_TYPE): WideString; safecall;
    procedure Import(const EncodedCertificate: WideString); safecall;
    procedure Display; safecall;
    property Version: Integer read Get_Version;
    property SerialNumber: WideString read Get_SerialNumber;
    property SubjectName: WideString read Get_SubjectName;
    property IssuerName: WideString read Get_IssuerName;
    property ValidFromDate: TDateTime read Get_ValidFromDate;
    property ValidToDate: TDateTime read Get_ValidToDate;
    property Thumbprint: WideString read Get_Thumbprint;
  end;

// *********************************************************************//
// DispIntf:  ICertificateDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0BBA0B86-766C-4755-A443-243FF2BD8D29}
// *********************************************************************//
  ICertificateDisp = dispinterface
    ['{0BBA0B86-766C-4755-A443-243FF2BD8D29}']
    property Version: Integer readonly dispid 1;
    property SerialNumber: WideString readonly dispid 2;
    property SubjectName: WideString readonly dispid 3;
    property IssuerName: WideString readonly dispid 4;
    property ValidFromDate: TDateTime readonly dispid 5;
    property ValidToDate: TDateTime readonly dispid 6;
    property Thumbprint: WideString readonly dispid 7;
    function HasPrivateKey: WordBool; dispid 10;
    function GetInfo(InfoType: CAPICOM_CERT_INFO_TYPE): WideString; dispid 11;
    function IsValid: ICertificateStatus; dispid 12;
    function KeyUsage: IKeyUsage; dispid 13;
    function ExtendedKeyUsage: IExtendedKeyUsage; dispid 14;
    function BasicConstraints: IBasicConstraints; dispid 15;
    function Export(EncodingType: CAPICOM_ENCODING_TYPE): WideString; dispid 16;
    procedure Import(const EncodedCertificate: WideString); dispid 17;
    procedure Display; dispid 18;
  end;

// *********************************************************************//
// Interface: ICertificate2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6FE450DC-AD32-48D4-A366-01EE7E0B1374}
// *********************************************************************//
  ICertificate2 = interface(ICertificate)
    ['{6FE450DC-AD32-48D4-A366-01EE7E0B1374}']
    function Get_Archived: WordBool; safecall;
    procedure Set_Archived(pVal: WordBool); safecall;
    function Template: ITemplate; safecall;
    function PublicKey: IPublicKey; safecall;
    function Get_PrivateKey: IPrivateKey; safecall;
    procedure Set_PrivateKey(const pVal: IPrivateKey); safecall;
    function Extensions: IExtensions; safecall;
    function ExtendedProperties: IExtendedProperties; safecall;
    procedure Load(const FileName: WideString; const Password: WideString;
                   KeyStorageFlag: CAPICOM_KEY_STORAGE_FLAG; KeyLocation: CAPICOM_KEY_LOCATION); safecall;
    procedure Save(const FileName: WideString; const Password: WideString;
                   SaveAs: CAPICOM_CERTIFICATE_SAVE_AS_TYPE;
                   IncludeOption: CAPICOM_CERTIFICATE_INCLUDE_OPTION); safecall;
    property Archived: WordBool read Get_Archived write Set_Archived;
    property PrivateKey: IPrivateKey read Get_PrivateKey write Set_PrivateKey;
  end;

// *********************************************************************//
// DispIntf:  ICertificate2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6FE450DC-AD32-48D4-A366-01EE7E0B1374}
// *********************************************************************//
  ICertificate2Disp = dispinterface
    ['{6FE450DC-AD32-48D4-A366-01EE7E0B1374}']
    property Archived: WordBool dispid 19;
    function Template: ITemplate; dispid 20;
    function PublicKey: IPublicKey; dispid 21;
    property PrivateKey: IPrivateKey dispid 22;
    function Extensions: IExtensions; dispid 23;
    function ExtendedProperties: IExtendedProperties; dispid 24;
    procedure Load(const FileName: WideString; const Password: WideString;
                   KeyStorageFlag: CAPICOM_KEY_STORAGE_FLAG; KeyLocation: CAPICOM_KEY_LOCATION); dispid 25;
    procedure Save(const FileName: WideString; const Password: WideString;
                   SaveAs: CAPICOM_CERTIFICATE_SAVE_AS_TYPE;
                   IncludeOption: CAPICOM_CERTIFICATE_INCLUDE_OPTION); dispid 26;
    property Version: Integer readonly dispid 1;
    property SerialNumber: WideString readonly dispid 2;
    property SubjectName: WideString readonly dispid 3;
    property IssuerName: WideString readonly dispid 4;
    property ValidFromDate: TDateTime readonly dispid 5;
    property ValidToDate: TDateTime readonly dispid 6;
    property Thumbprint: WideString readonly dispid 7;
    function HasPrivateKey: WordBool; dispid 10;
    function GetInfo(InfoType: CAPICOM_CERT_INFO_TYPE): WideString; dispid 11;
    function IsValid: ICertificateStatus; dispid 12;
    function KeyUsage: IKeyUsage; dispid 13;
    function ExtendedKeyUsage: IExtendedKeyUsage; dispid 14;
    function BasicConstraints: IBasicConstraints; dispid 15;
    function Export(EncodingType: CAPICOM_ENCODING_TYPE): WideString; dispid 16;
    procedure Import(const EncodedCertificate: WideString); dispid 17;
    procedure Display; dispid 18;
  end;

// *********************************************************************//
// Interface: ICertContext
// Flags:     (0)
// GUID:      {9E7D3477-4F63-423E-8A45-E13B2BB851A2}
// *********************************************************************//
  ICertContext = interface(IUnknown)
    ['{9E7D3477-4F63-423E-8A45-E13B2BB851A2}']
    function Get_CertContext(out ppCertContext: Integer): HResult; stdcall;
    function Set_CertContext(ppCertContext: Integer): HResult; stdcall;
    function FreeContext(pCertContext: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITemplate
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5F10FFCE-C922-476F-AA76-DF99D5BDFA2C}
// *********************************************************************//
  ITemplate = interface(IDispatch)
    ['{5F10FFCE-C922-476F-AA76-DF99D5BDFA2C}']
    function Get_IsPresent: WordBool; safecall;
    function Get_IsCritical: WordBool; safecall;
    function Get_Name: WideString; safecall;
    function Get_OID: IOID; safecall;
    function Get_MajorVersion: Integer; safecall;
    function Get_MinorVersion: Integer; safecall;
    property IsPresent: WordBool read Get_IsPresent;
    property IsCritical: WordBool read Get_IsCritical;
    property Name: WideString read Get_Name;
    property OID: IOID read Get_OID;
    property MajorVersion: Integer read Get_MajorVersion;
    property MinorVersion: Integer read Get_MinorVersion;
  end;

// *********************************************************************//
// DispIntf:  ITemplateDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5F10FFCE-C922-476F-AA76-DF99D5BDFA2C}
// *********************************************************************//
  ITemplateDisp = dispinterface
    ['{5F10FFCE-C922-476F-AA76-DF99D5BDFA2C}']
    property IsPresent: WordBool readonly dispid 1;
    property IsCritical: WordBool readonly dispid 2;
    property Name: WideString readonly dispid 3;
    property OID: IOID readonly dispid 4;
    property MajorVersion: Integer readonly dispid 5;
    property MinorVersion: Integer readonly dispid 6;
  end;

// *********************************************************************//
// Interface: IPublicKey
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {72BF9ADA-6817-4C31-B43E-25F7C7B091F4}
// *********************************************************************//
  IPublicKey = interface(IDispatch)
    ['{72BF9ADA-6817-4C31-B43E-25F7C7B091F4}']
    function Get_Algorithm: IOID; safecall;
    function Get_Length: Integer; safecall;
    function Get_EncodedKey: IEncodedData; safecall;
    function Get_EncodedParameters: IEncodedData; safecall;
    property Algorithm: IOID read Get_Algorithm;
    property Length: Integer read Get_Length;
    property EncodedKey: IEncodedData read Get_EncodedKey;
    property EncodedParameters: IEncodedData read Get_EncodedParameters;
  end;

// *********************************************************************//
// DispIntf:  IPublicKeyDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {72BF9ADA-6817-4C31-B43E-25F7C7B091F4}
// *********************************************************************//
  IPublicKeyDisp = dispinterface
    ['{72BF9ADA-6817-4C31-B43E-25F7C7B091F4}']
    property Algorithm: IOID readonly dispid 0;
    property Length: Integer readonly dispid 1;
    property EncodedKey: IEncodedData readonly dispid 2;
    property EncodedParameters: IEncodedData readonly dispid 3;
  end;

// *********************************************************************//
// Interface: IEncodedData
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D3D460F2-E7F3-4AF3-8EC6-8EB68C61C567}
// *********************************************************************//
  IEncodedData = interface(IDispatch)
    ['{D3D460F2-E7F3-4AF3-8EC6-8EB68C61C567}']
    function Get_Value(EncodingType: CAPICOM_ENCODING_TYPE): WideString; safecall;
    function Format(bMultiLines: WordBool): WideString; safecall;
    function Decoder: IDispatch; safecall;
    property Value[EncodingType: CAPICOM_ENCODING_TYPE]: WideString read Get_Value; default;
  end;

// *********************************************************************//
// DispIntf:  IEncodedDataDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D3D460F2-E7F3-4AF3-8EC6-8EB68C61C567}
// *********************************************************************//
  IEncodedDataDisp = dispinterface
    ['{D3D460F2-E7F3-4AF3-8EC6-8EB68C61C567}']
    property Value[EncodingType: CAPICOM_ENCODING_TYPE]: WideString readonly dispid 0; default;
    function Format(bMultiLines: WordBool): WideString; dispid 1;
    function Decoder: IDispatch; dispid 2;
  end;

// *********************************************************************//
// Interface: IPrivateKey
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {659DEDC3-6C85-42DB-8527-EFCB21742862}
// *********************************************************************//
  IPrivateKey = interface(IDispatch)
    ['{659DEDC3-6C85-42DB-8527-EFCB21742862}']
    function Get_ContainerName: WideString; safecall;
    function Get_UniqueContainerName: WideString; safecall;
    function Get_ProviderName: WideString; safecall;
    function Get_ProviderType: CAPICOM_PROV_TYPE; safecall;
    function Get_KeySpec: CAPICOM_KEY_SPEC; safecall;
    function IsAccessible: WordBool; safecall;
    function IsProtected: WordBool; safecall;
    function IsExportable: WordBool; safecall;
    function IsRemovable: WordBool; safecall;
    function IsMachineKeyset: WordBool; safecall;
    function IsHardwareDevice: WordBool; safecall;
    procedure Open(const ContainerName: WideString; const ProviderName: WideString;
                   ProviderType: CAPICOM_PROV_TYPE; KeySpec: CAPICOM_KEY_SPEC;
                   StoreLocation: CAPICOM_STORE_LOCATION; bCheckExistence: WordBool); safecall;
    procedure Delete; safecall;
    property ContainerName: WideString read Get_ContainerName;
    property UniqueContainerName: WideString read Get_UniqueContainerName;
    property ProviderName: WideString read Get_ProviderName;
    property ProviderType: CAPICOM_PROV_TYPE read Get_ProviderType;
    property KeySpec: CAPICOM_KEY_SPEC read Get_KeySpec;
  end;

// *********************************************************************//
// DispIntf:  IPrivateKeyDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {659DEDC3-6C85-42DB-8527-EFCB21742862}
// *********************************************************************//
  IPrivateKeyDisp = dispinterface
    ['{659DEDC3-6C85-42DB-8527-EFCB21742862}']
    property ContainerName: WideString readonly dispid 0;
    property UniqueContainerName: WideString readonly dispid 1;
    property ProviderName: WideString readonly dispid 2;
    property ProviderType: CAPICOM_PROV_TYPE readonly dispid 3;
    property KeySpec: CAPICOM_KEY_SPEC readonly dispid 4;
    function IsAccessible: WordBool; dispid 5;
    function IsProtected: WordBool; dispid 6;
    function IsExportable: WordBool; dispid 7;
    function IsRemovable: WordBool; dispid 8;
    function IsMachineKeyset: WordBool; dispid 9;
    function IsHardwareDevice: WordBool; dispid 10;
    procedure Open(const ContainerName: WideString; const ProviderName: WideString;
                   ProviderType: CAPICOM_PROV_TYPE; KeySpec: CAPICOM_KEY_SPEC;
                   StoreLocation: CAPICOM_STORE_LOCATION; bCheckExistence: WordBool); dispid 11;
    procedure Delete; dispid 12;
  end;

// *********************************************************************//
// Interface: IExtensions
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BC530D61-E692-4225-9E7A-07B90B45856A}
// *********************************************************************//
  IExtensions = interface(IDispatch)
    ['{BC530D61-E692-4225-9E7A-07B90B45856A}']
    function Get_Item(Index: OleVariant): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Item[Index: OleVariant]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IExtensionsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BC530D61-E692-4225-9E7A-07B90B45856A}
// *********************************************************************//
  IExtensionsDisp = dispinterface
    ['{BC530D61-E692-4225-9E7A-07B90B45856A}']
    property Item[Index: OleVariant]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IExtendedProperties
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3B096E87-6218-4A3B-A880-F6CB951E7805}
// *********************************************************************//
  IExtendedProperties = interface(IDispatch)
    ['{3B096E87-6218-4A3B-A880-F6CB951E7805}']
    function Get_Item(Index: Integer): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    procedure Add(const pVal: IExtendedProperty); safecall;
    procedure Remove(PropID: CAPICOM_PROPID); safecall;
    property Item[Index: Integer]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IExtendedPropertiesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3B096E87-6218-4A3B-A880-F6CB951E7805}
// *********************************************************************//
  IExtendedPropertiesDisp = dispinterface
    ['{3B096E87-6218-4A3B-A880-F6CB951E7805}']
    property Item[Index: Integer]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Add(const pVal: IExtendedProperty); dispid 2;
    procedure Remove(PropID: CAPICOM_PROPID); dispid 3;
  end;

// *********************************************************************//
// Interface: IExtendedProperty
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {ECB8A5C8-562C-4989-B49D-FA37D40F8FC4}
// *********************************************************************//
  IExtendedProperty = interface(IDispatch)
    ['{ECB8A5C8-562C-4989-B49D-FA37D40F8FC4}']
    function Get_PropID: CAPICOM_PROPID; safecall;
    procedure Set_PropID(pVal: CAPICOM_PROPID); safecall;
    function Get_Value(EncodingType: CAPICOM_ENCODING_TYPE): WideString; safecall;
    procedure Set_Value(EncodingType: CAPICOM_ENCODING_TYPE; const pVal: WideString); safecall;
    property PropID: CAPICOM_PROPID read Get_PropID write Set_PropID;
    property Value[EncodingType: CAPICOM_ENCODING_TYPE]: WideString read Get_Value write Set_Value;
  end;

// *********************************************************************//
// DispIntf:  IExtendedPropertyDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {ECB8A5C8-562C-4989-B49D-FA37D40F8FC4}
// *********************************************************************//
  IExtendedPropertyDisp = dispinterface
    ['{ECB8A5C8-562C-4989-B49D-FA37D40F8FC4}']
    property PropID: CAPICOM_PROPID dispid 0;
    property Value[EncodingType: CAPICOM_ENCODING_TYPE]: WideString dispid 1;
  end;

// *********************************************************************//
// Interface: ICertificates2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7B57C04B-1786-4B30-A7B6-36235CD58A14}
// *********************************************************************//
  ICertificates2 = interface(ICertificates)
    ['{7B57C04B-1786-4B30-A7B6-36235CD58A14}']
    function Find(FindType: CAPICOM_CERTIFICATE_FIND_TYPE; varCriteria: OleVariant;
                  bFindValidOnly: WordBool): ICertificates2; safecall;
    function Select(const Title: WideString; const DisplayString: WideString; bMultiSelect: WordBool): ICertificates2; safecall;
    procedure Add(const pVal: ICertificate2); safecall;
    procedure Remove(Index: OleVariant); safecall;
    procedure Clear; safecall;
    procedure Save(const FileName: WideString; const Password: WideString;
                   SaveAs: CAPICOM_CERTIFICATES_SAVE_AS_TYPE; ExportFlag: CAPICOM_EXPORT_FLAG); safecall;
  end;

// *********************************************************************//
// DispIntf:  ICertificates2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7B57C04B-1786-4B30-A7B6-36235CD58A14}
// *********************************************************************//
  ICertificates2Disp = dispinterface
    ['{7B57C04B-1786-4B30-A7B6-36235CD58A14}']
    function Find(FindType: CAPICOM_CERTIFICATE_FIND_TYPE; varCriteria: OleVariant;
                  bFindValidOnly: WordBool): ICertificates2; dispid 2;
    function Select(const Title: WideString; const DisplayString: WideString; bMultiSelect: WordBool): ICertificates2; dispid 3;
    procedure Add(const pVal: ICertificate2); dispid 4;
    procedure Remove(Index: OleVariant); dispid 5;
    procedure Clear; dispid 6;
    procedure Save(const FileName: WideString; const Password: WideString;
                   SaveAs: CAPICOM_CERTIFICATES_SAVE_AS_TYPE; ExportFlag: CAPICOM_EXPORT_FLAG); dispid 7;
    property Item[Index: Integer]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ICCertificates
// Flags:     (512) Restricted
// GUID:      {EBDC6DC2-684D-4425-BBB7-CB4D15A088A7}
// *********************************************************************//
  ICCertificates = interface(IUnknown)
    ['{EBDC6DC2-684D-4425-BBB7-CB4D15A088A7}']
    function _ExportToStore(var hCertStore: Pointer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IChain
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {77F6F881-5D3A-4F2F-AEF0-E4A2F9AA689D}
// *********************************************************************//
  IChain = interface(IDispatch)
    ['{77F6F881-5D3A-4F2F-AEF0-E4A2F9AA689D}']
    function Get_Certificates: ICertificates; safecall;
    function Get_Status(Index: Integer): Integer; safecall;
    function Build(const pICertificate: ICertificate): WordBool; safecall;
    property Certificates: ICertificates read Get_Certificates;
    property Status[Index: Integer]: Integer read Get_Status;
  end;

// *********************************************************************//
// DispIntf:  IChainDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {77F6F881-5D3A-4F2F-AEF0-E4A2F9AA689D}
// *********************************************************************//
  IChainDisp = dispinterface
    ['{77F6F881-5D3A-4F2F-AEF0-E4A2F9AA689D}']
    property Certificates: ICertificates readonly dispid 0;
    property Status[Index: Integer]: Integer readonly dispid 1;
    function Build(const pICertificate: ICertificate): WordBool; dispid 2;
  end;

// *********************************************************************//
// Interface: IChain2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CA65D842-2110-4073-AEE3-D0AA5F56C421}
// *********************************************************************//
  IChain2 = interface(IChain)
    ['{CA65D842-2110-4073-AEE3-D0AA5F56C421}']
    function CertificatePolicies: IOIDs; safecall;
    function ApplicationPolicies: IOIDs; safecall;
    function ExtendedErrorInfo(Index: Integer): WideString; safecall;
  end;

// *********************************************************************//
// DispIntf:  IChain2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CA65D842-2110-4073-AEE3-D0AA5F56C421}
// *********************************************************************//
  IChain2Disp = dispinterface
    ['{CA65D842-2110-4073-AEE3-D0AA5F56C421}']
    function CertificatePolicies: IOIDs; dispid 3;
    function ApplicationPolicies: IOIDs; dispid 4;
    function ExtendedErrorInfo(Index: Integer): WideString; dispid 5;
    property Certificates: ICertificates readonly dispid 0;
    property Status[Index: Integer]: Integer readonly dispid 1;
    function Build(const pICertificate: ICertificate): WordBool; dispid 2;
  end;

// *********************************************************************//
// Interface: IChainContext
// Flags:     (0)
// GUID:      {B27FFB30-432E-4585-A3FD-72530108CBFD}
// *********************************************************************//
  IChainContext = interface(IUnknown)
    ['{B27FFB30-432E-4585-A3FD-72530108CBFD}']
    function Get_ChainContext(out pChainContext: Integer): HResult; stdcall;
    function Set_ChainContext(pChainContext: Integer): HResult; stdcall;
    function FreeContext(pChainContext: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IStore
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E860EF75-1B63-4254-AF47-960DAA3DD337}
// *********************************************************************//
  IStore = interface(IDispatch)
    ['{E860EF75-1B63-4254-AF47-960DAA3DD337}']
    function Get_Certificates: ICertificates; safecall;
    procedure Open(StoreLocation: CAPICOM_STORE_LOCATION; const StoreName: WideString;
                   OpenMode: CAPICOM_STORE_OPEN_MODE); safecall;
    procedure Add(const pVal: ICertificate); safecall;
    procedure Remove(const pVal: ICertificate); safecall;
    function Export(SaveAs: CAPICOM_STORE_SAVE_AS_TYPE; EncodingType: CAPICOM_ENCODING_TYPE): WideString; safecall;
    procedure Import(const EncodedStore: WideString); safecall;
    property Certificates: ICertificates read Get_Certificates;
  end;

// *********************************************************************//
// DispIntf:  IStoreDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E860EF75-1B63-4254-AF47-960DAA3DD337}
// *********************************************************************//
  IStoreDisp = dispinterface
    ['{E860EF75-1B63-4254-AF47-960DAA3DD337}']
    property Certificates: ICertificates readonly dispid 0;
    procedure Open(StoreLocation: CAPICOM_STORE_LOCATION; const StoreName: WideString;
                   OpenMode: CAPICOM_STORE_OPEN_MODE); dispid 1;
    procedure Add(const pVal: ICertificate); dispid 2;
    procedure Remove(const pVal: ICertificate); dispid 3;
    function Export(SaveAs: CAPICOM_STORE_SAVE_AS_TYPE; EncodingType: CAPICOM_ENCODING_TYPE): WideString; dispid 4;
    procedure Import(const EncodedStore: WideString); dispid 5;
  end;

// *********************************************************************//
// Interface: IStore2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4DA6ABC4-BDCD-4317-B650-262075B93A9C}
// *********************************************************************//
  IStore2 = interface(IStore)
    ['{4DA6ABC4-BDCD-4317-B650-262075B93A9C}']
    procedure Load(const FileName: WideString; const Password: WideString;
                   KeyStorageFlag: CAPICOM_KEY_STORAGE_FLAG); safecall;
  end;

// *********************************************************************//
// DispIntf:  IStore2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4DA6ABC4-BDCD-4317-B650-262075B93A9C}
// *********************************************************************//
  IStore2Disp = dispinterface
    ['{4DA6ABC4-BDCD-4317-B650-262075B93A9C}']
    procedure Load(const FileName: WideString; const Password: WideString;
                   KeyStorageFlag: CAPICOM_KEY_STORAGE_FLAG); dispid 6;
    property Certificates: ICertificates readonly dispid 0;
    procedure Open(StoreLocation: CAPICOM_STORE_LOCATION; const StoreName: WideString;
                   OpenMode: CAPICOM_STORE_OPEN_MODE); dispid 1;
    procedure Add(const pVal: ICertificate); dispid 2;
    procedure Remove(const pVal: ICertificate); dispid 3;
    function Export(SaveAs: CAPICOM_STORE_SAVE_AS_TYPE; EncodingType: CAPICOM_ENCODING_TYPE): WideString; dispid 4;
    procedure Import(const EncodedStore: WideString); dispid 5;
  end;

// *********************************************************************//
// Interface: IStore3
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F701F8EC-31C7-48FB-B621-5DE417C3A607}
// *********************************************************************//
  IStore3 = interface(IStore2)
    ['{F701F8EC-31C7-48FB-B621-5DE417C3A607}']
    function Get_Name: WideString; safecall;
    function Get_Location: CAPICOM_STORE_LOCATION; safecall;
    function Delete: WordBool; safecall;
    procedure Close; safecall;
    property Name: WideString read Get_Name;
    property Location: CAPICOM_STORE_LOCATION read Get_Location;
  end;

// *********************************************************************//
// DispIntf:  IStore3Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F701F8EC-31C7-48FB-B621-5DE417C3A607}
// *********************************************************************//
  IStore3Disp = dispinterface
    ['{F701F8EC-31C7-48FB-B621-5DE417C3A607}']
    property Name: WideString readonly dispid 7;
    property Location: CAPICOM_STORE_LOCATION readonly dispid 8;
    function Delete: WordBool; dispid 9;
    procedure Close; dispid 10;
    procedure Load(const FileName: WideString; const Password: WideString;
                   KeyStorageFlag: CAPICOM_KEY_STORAGE_FLAG); dispid 6;
    property Certificates: ICertificates readonly dispid 0;
    procedure Open(StoreLocation: CAPICOM_STORE_LOCATION; const StoreName: WideString;
                   OpenMode: CAPICOM_STORE_OPEN_MODE); dispid 1;
    procedure Add(const pVal: ICertificate); dispid 2;
    procedure Remove(const pVal: ICertificate); dispid 3;
    function Export(SaveAs: CAPICOM_STORE_SAVE_AS_TYPE; EncodingType: CAPICOM_ENCODING_TYPE): WideString; dispid 4;
    procedure Import(const EncodedStore: WideString); dispid 5;
  end;

// *********************************************************************//
// Interface: ICertStore
// Flags:     (0)
// GUID:      {BB3ECB9C-A83A-445C-BDB5-EFBEF691B731}
// *********************************************************************//
  ICertStore = interface(IUnknown)
    ['{BB3ECB9C-A83A-445C-BDB5-EFBEF691B731}']
    function Get_StoreHandle(out phCertStore: Integer): HResult; stdcall;
    function Set_StoreHandle(phCertStore: Integer): HResult; stdcall;
    function Get_StoreLocation(out pStoreLocation: CAPICOM_STORE_LOCATION): HResult; stdcall;
    function Set_StoreLocation(pStoreLocation: CAPICOM_STORE_LOCATION): HResult; stdcall;
    function CloseHandle(hCertStore: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IAttribute
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B17A8D78-B5A6-45F7-BA21-01AB94B08415}
// *********************************************************************//
  IAttribute = interface(IDispatch)
    ['{B17A8D78-B5A6-45F7-BA21-01AB94B08415}']
    function Get_Name: CAPICOM_ATTRIBUTE; safecall;
    procedure Set_Name(pVal: CAPICOM_ATTRIBUTE); safecall;
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(pVal: OleVariant); safecall;
    property Name: CAPICOM_ATTRIBUTE read Get_Name write Set_Name;
    property Value: OleVariant read Get_Value write Set_Value;
  end;

// *********************************************************************//
// DispIntf:  IAttributeDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B17A8D78-B5A6-45F7-BA21-01AB94B08415}
// *********************************************************************//
  IAttributeDisp = dispinterface
    ['{B17A8D78-B5A6-45F7-BA21-01AB94B08415}']
    property Name: CAPICOM_ATTRIBUTE dispid 0;
    property Value: OleVariant dispid 1;
  end;

// *********************************************************************//
// Interface: IAttributes
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6ADC653E-D5B9-422A-991A-A2B0119CEDAC}
// *********************************************************************//
  IAttributes = interface(IDispatch)
    ['{6ADC653E-D5B9-422A-991A-A2B0119CEDAC}']
    function Get_Item(Index: Integer): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    procedure Add(const pVal: IAttribute); safecall;
    procedure Remove(Index: Integer); safecall;
    procedure Clear; safecall;
    property Item[Index: Integer]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IAttributesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6ADC653E-D5B9-422A-991A-A2B0119CEDAC}
// *********************************************************************//
  IAttributesDisp = dispinterface
    ['{6ADC653E-D5B9-422A-991A-A2B0119CEDAC}']
    property Item[Index: Integer]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Add(const pVal: IAttribute); dispid 2;
    procedure Remove(Index: Integer); dispid 3;
    procedure Clear; dispid 4;
  end;

// *********************************************************************//
// Interface: ISigner
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {51017B88-1913-49AD-82BE-6BB7C417DCF2}
// *********************************************************************//
  ISigner = interface(IDispatch)
    ['{51017B88-1913-49AD-82BE-6BB7C417DCF2}']
    function Get_Certificate: ICertificate; safecall;
    procedure Set_Certificate(const pVal: ICertificate); safecall;
    function Get_AuthenticatedAttributes: IAttributes; safecall;
    property Certificate: ICertificate read Get_Certificate write Set_Certificate;
    property AuthenticatedAttributes: IAttributes read Get_AuthenticatedAttributes;
  end;

// *********************************************************************//
// DispIntf:  ISignerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {51017B88-1913-49AD-82BE-6BB7C417DCF2}
// *********************************************************************//
  ISignerDisp = dispinterface
    ['{51017B88-1913-49AD-82BE-6BB7C417DCF2}']
    property Certificate: ICertificate dispid 0;
    property AuthenticatedAttributes: IAttributes readonly dispid 1;
  end;

// *********************************************************************//
// Interface: ISigner2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {625B1F55-C720-41D6-9ECF-BA59F9B85F17}
// *********************************************************************//
  ISigner2 = interface(ISigner)
    ['{625B1F55-C720-41D6-9ECF-BA59F9B85F17}']
    function Get_Chain: IChain; safecall;
    function Get_Options: CAPICOM_CERTIFICATE_INCLUDE_OPTION; safecall;
    procedure Set_Options(pVal: CAPICOM_CERTIFICATE_INCLUDE_OPTION); safecall;
    procedure Load(const FileName: WideString; const Password: WideString); safecall;
    property Chain: IChain read Get_Chain;
    property Options: CAPICOM_CERTIFICATE_INCLUDE_OPTION read Get_Options write Set_Options;
  end;

// *********************************************************************//
// DispIntf:  ISigner2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {625B1F55-C720-41D6-9ECF-BA59F9B85F17}
// *********************************************************************//
  ISigner2Disp = dispinterface
    ['{625B1F55-C720-41D6-9ECF-BA59F9B85F17}']
    property Chain: IChain readonly dispid 2;
    property Options: CAPICOM_CERTIFICATE_INCLUDE_OPTION dispid 3;
    procedure Load(const FileName: WideString; const Password: WideString); dispid 4;
    property Certificate: ICertificate dispid 0;
    property AuthenticatedAttributes: IAttributes readonly dispid 1;
  end;

// *********************************************************************//
// Interface: ICSigner
// Flags:     (512) Restricted
// GUID:      {8F83F792-014C-4E22-BD57-5C381E622F34}
// *********************************************************************//
  ICSigner = interface(IUnknown)
    ['{8F83F792-014C-4E22-BD57-5C381E622F34}']
    function Get_AdditionalStore(out phAdditionalStore: Integer): HResult; stdcall;
    function Set_AdditionalStore(phAdditionalStore: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISigners
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5A0780F8-9E6B-4BB0-BF54-87CD9627A8B4}
// *********************************************************************//
  ISigners = interface(IDispatch)
    ['{5A0780F8-9E6B-4BB0-BF54-87CD9627A8B4}']
    function Get_Item(Index: Integer): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Item[Index: Integer]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ISignersDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5A0780F8-9E6B-4BB0-BF54-87CD9627A8B4}
// *********************************************************************//
  ISignersDisp = dispinterface
    ['{5A0780F8-9E6B-4BB0-BF54-87CD9627A8B4}']
    property Item[Index: Integer]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ISignedData
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {AE9C454B-FC65-4C10-B130-CD9B45BA948B}
// *********************************************************************//
  ISignedData = interface(IDispatch)
    ['{AE9C454B-FC65-4C10-B130-CD9B45BA948B}']
    procedure Set_Content(const pVal: WideString); safecall;
    function Get_Content: WideString; safecall;
    function Get_Signers: ISigners; safecall;
    function Get_Certificates: ICertificates; safecall;
    function Sign(const pSigner: ISigner; bDetached: WordBool; EncodingType: CAPICOM_ENCODING_TYPE): WideString; safecall;
    function CoSign(const pSigner: ISigner; EncodingType: CAPICOM_ENCODING_TYPE): WideString; safecall;
    procedure Verify(const SignedMessage: WideString; bDetached: WordBool;
                     VerifyFlag: CAPICOM_SIGNED_DATA_VERIFY_FLAG); safecall;
    property Content: WideString read Get_Content write Set_Content;
    property Signers: ISigners read Get_Signers;
    property Certificates: ICertificates read Get_Certificates;
  end;

// *********************************************************************//
// DispIntf:  ISignedDataDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {AE9C454B-FC65-4C10-B130-CD9B45BA948B}
// *********************************************************************//
  ISignedDataDisp = dispinterface
    ['{AE9C454B-FC65-4C10-B130-CD9B45BA948B}']
    property Content: WideString dispid 0;
    property Signers: ISigners readonly dispid 1;
    property Certificates: ICertificates readonly dispid 2;
    function Sign(const pSigner: ISigner; bDetached: WordBool; EncodingType: CAPICOM_ENCODING_TYPE): WideString; dispid 3;
    function CoSign(const pSigner: ISigner; EncodingType: CAPICOM_ENCODING_TYPE): WideString; dispid 4;
    procedure Verify(const SignedMessage: WideString; bDetached: WordBool;
                     VerifyFlag: CAPICOM_SIGNED_DATA_VERIFY_FLAG); dispid 5;
  end;

// *********************************************************************//
// Interface: IAlgorithm
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BF3D04A9-B0DA-4153-B45E-6CCFA5AC715B}
// *********************************************************************//
  IAlgorithm = interface(IDispatch)
    ['{BF3D04A9-B0DA-4153-B45E-6CCFA5AC715B}']
    function Get_Name: CAPICOM_ENCRYPTION_ALGORITHM; safecall;
    procedure Set_Name(pVal: CAPICOM_ENCRYPTION_ALGORITHM); safecall;
    function Get_KeyLength: CAPICOM_ENCRYPTION_KEY_LENGTH; safecall;
    procedure Set_KeyLength(pVal: CAPICOM_ENCRYPTION_KEY_LENGTH); safecall;
    property Name: CAPICOM_ENCRYPTION_ALGORITHM read Get_Name write Set_Name;
    property KeyLength: CAPICOM_ENCRYPTION_KEY_LENGTH read Get_KeyLength write Set_KeyLength;
  end;

// *********************************************************************//
// DispIntf:  IAlgorithmDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BF3D04A9-B0DA-4153-B45E-6CCFA5AC715B}
// *********************************************************************//
  IAlgorithmDisp = dispinterface
    ['{BF3D04A9-B0DA-4153-B45E-6CCFA5AC715B}']
    property Name: CAPICOM_ENCRYPTION_ALGORITHM dispid 0;
    property KeyLength: CAPICOM_ENCRYPTION_KEY_LENGTH dispid 1;
  end;

// *********************************************************************//
// Interface: IRecipients
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A694C896-FC38-4C34-AE61-3B1A95984C14}
// *********************************************************************//
  IRecipients = interface(IDispatch)
    ['{A694C896-FC38-4C34-AE61-3B1A95984C14}']
    function Get_Item(Index: Integer): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    procedure Add(const pVal: ICertificate); safecall;
    procedure Remove(Index: Integer); safecall;
    procedure Clear; safecall;
    property Item[Index: Integer]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IRecipientsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A694C896-FC38-4C34-AE61-3B1A95984C14}
// *********************************************************************//
  IRecipientsDisp = dispinterface
    ['{A694C896-FC38-4C34-AE61-3B1A95984C14}']
    property Item[Index: Integer]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Add(const pVal: ICertificate); dispid 2;
    procedure Remove(Index: Integer); dispid 3;
    procedure Clear; dispid 4;
  end;

// *********************************************************************//
// Interface: IEnvelopedData
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F6CB6A20-CC18-4424-AE57-6F2AA3DC2059}
// *********************************************************************//
  IEnvelopedData = interface(IDispatch)
    ['{F6CB6A20-CC18-4424-AE57-6F2AA3DC2059}']
    procedure Set_Content(const pVal: WideString); safecall;
    function Get_Content: WideString; safecall;
    function Get_Algorithm: IAlgorithm; safecall;
    function Get_Recipients: IRecipients; safecall;
    function Encrypt(EncodingType: CAPICOM_ENCODING_TYPE): WideString; safecall;
    procedure Decrypt(const EnvelopedMessage: WideString); safecall;
    property Content: WideString read Get_Content write Set_Content;
    property Algorithm: IAlgorithm read Get_Algorithm;
    property Recipients: IRecipients read Get_Recipients;
  end;

// *********************************************************************//
// DispIntf:  IEnvelopedDataDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F6CB6A20-CC18-4424-AE57-6F2AA3DC2059}
// *********************************************************************//
  IEnvelopedDataDisp = dispinterface
    ['{F6CB6A20-CC18-4424-AE57-6F2AA3DC2059}']
    property Content: WideString dispid 0;
    property Algorithm: IAlgorithm readonly dispid 1;
    property Recipients: IRecipients readonly dispid 2;
    function Encrypt(EncodingType: CAPICOM_ENCODING_TYPE): WideString; dispid 3;
    procedure Decrypt(const EnvelopedMessage: WideString); dispid 4;
  end;

// *********************************************************************//
// Interface: IEncryptedData
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C4778A66-972F-42E4-87C5-5CC16F7931CA}
// *********************************************************************//
  IEncryptedData = interface(IDispatch)
    ['{C4778A66-972F-42E4-87C5-5CC16F7931CA}']
    procedure Set_Content(const pVal: WideString); safecall;
    function Get_Content: WideString; safecall;
    function Get_Algorithm: IAlgorithm; safecall;
    procedure SetSecret(const newVal: WideString; SecretType: CAPICOM_SECRET_TYPE); safecall;
    function Encrypt(EncodingType: CAPICOM_ENCODING_TYPE): WideString; safecall;
    procedure Decrypt(const EncryptedMessage: WideString); safecall;
    property Content: WideString read Get_Content write Set_Content;
    property Algorithm: IAlgorithm read Get_Algorithm;
  end;

// *********************************************************************//
// DispIntf:  IEncryptedDataDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C4778A66-972F-42E4-87C5-5CC16F7931CA}
// *********************************************************************//
  IEncryptedDataDisp = dispinterface
    ['{C4778A66-972F-42E4-87C5-5CC16F7931CA}']
    property Content: WideString dispid 0;
    property Algorithm: IAlgorithm readonly dispid 1;
    procedure SetSecret(const newVal: WideString; SecretType: CAPICOM_SECRET_TYPE); dispid 2;
    function Encrypt(EncodingType: CAPICOM_ENCODING_TYPE): WideString; dispid 3;
    procedure Decrypt(const EncryptedMessage: WideString); dispid 4;
  end;

// *********************************************************************//
// Interface: INoticeNumbers
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EE2C051D-33A1-4157-86B4-9280E29782F2}
// *********************************************************************//
  INoticeNumbers = interface(IDispatch)
    ['{EE2C051D-33A1-4157-86B4-9280E29782F2}']
    function Get_Item(Index: Integer): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Item[Index: Integer]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  INoticeNumbersDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EE2C051D-33A1-4157-86B4-9280E29782F2}
// *********************************************************************//
  INoticeNumbersDisp = dispinterface
    ['{EE2C051D-33A1-4157-86B4-9280E29782F2}']
    property Item[Index: Integer]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IQualifier
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3604C9DD-A22E-4A15-A469-8181C0C113DE}
// *********************************************************************//
  IQualifier = interface(IDispatch)
    ['{3604C9DD-A22E-4A15-A469-8181C0C113DE}']
    function Get_OID: IOID; safecall;
    function Get_CPSPointer: WideString; safecall;
    function Get_OrganizationName: WideString; safecall;
    function Get_NoticeNumbers: INoticeNumbers; safecall;
    function Get_ExplicitText: WideString; safecall;
    property OID: IOID read Get_OID;
    property CPSPointer: WideString read Get_CPSPointer;
    property OrganizationName: WideString read Get_OrganizationName;
    property NoticeNumbers: INoticeNumbers read Get_NoticeNumbers;
    property ExplicitText: WideString read Get_ExplicitText;
  end;

// *********************************************************************//
// DispIntf:  IQualifierDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3604C9DD-A22E-4A15-A469-8181C0C113DE}
// *********************************************************************//
  IQualifierDisp = dispinterface
    ['{3604C9DD-A22E-4A15-A469-8181C0C113DE}']
    property OID: IOID readonly dispid 0;
    property CPSPointer: WideString readonly dispid 1;
    property OrganizationName: WideString readonly dispid 2;
    property NoticeNumbers: INoticeNumbers readonly dispid 3;
    property ExplicitText: WideString readonly dispid 4;
  end;

// *********************************************************************//
// Interface: IQualifiers
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6B5A8AB6-597D-4398-AC63-1036EF546348}
// *********************************************************************//
  IQualifiers = interface(IDispatch)
    ['{6B5A8AB6-597D-4398-AC63-1036EF546348}']
    function Get_Item(Index: Integer): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Item[Index: Integer]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IQualifiersDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6B5A8AB6-597D-4398-AC63-1036EF546348}
// *********************************************************************//
  IQualifiersDisp = dispinterface
    ['{6B5A8AB6-597D-4398-AC63-1036EF546348}']
    property Item[Index: Integer]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IPolicyInformation
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8973710C-8411-4951-9E65-D45FD524FFDF}
// *********************************************************************//
  IPolicyInformation = interface(IDispatch)
    ['{8973710C-8411-4951-9E65-D45FD524FFDF}']
    function Get_OID: IOID; safecall;
    function Get_Qualifiers: IQualifiers; safecall;
    property OID: IOID read Get_OID;
    property Qualifiers: IQualifiers read Get_Qualifiers;
  end;

// *********************************************************************//
// DispIntf:  IPolicyInformationDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8973710C-8411-4951-9E65-D45FD524FFDF}
// *********************************************************************//
  IPolicyInformationDisp = dispinterface
    ['{8973710C-8411-4951-9E65-D45FD524FFDF}']
    property OID: IOID readonly dispid 0;
    property Qualifiers: IQualifiers readonly dispid 1;
  end;

// *********************************************************************//
// Interface: ICertificatePolicies
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CC7A72A7-C83A-4049-85F4-4292DE9DBFD3}
// *********************************************************************//
  ICertificatePolicies = interface(IDispatch)
    ['{CC7A72A7-C83A-4049-85F4-4292DE9DBFD3}']
    function Get_Item(Index: Integer): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Item[Index: Integer]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ICertificatePoliciesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CC7A72A7-C83A-4049-85F4-4292DE9DBFD3}
// *********************************************************************//
  ICertificatePoliciesDisp = dispinterface
    ['{CC7A72A7-C83A-4049-85F4-4292DE9DBFD3}']
    property Item[Index: Integer]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IExtension
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {ED4E4ED4-FDD8-476E-AED9-5239E7948257}
// *********************************************************************//
  IExtension = interface(IDispatch)
    ['{ED4E4ED4-FDD8-476E-AED9-5239E7948257}']
    function Get_OID: IOID; safecall;
    function Get_IsCritical: WordBool; safecall;
    function Get_EncodedData: IEncodedData; safecall;
    property OID: IOID read Get_OID;
    property IsCritical: WordBool read Get_IsCritical;
    property EncodedData: IEncodedData read Get_EncodedData;
  end;

// *********************************************************************//
// DispIntf:  IExtensionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {ED4E4ED4-FDD8-476E-AED9-5239E7948257}
// *********************************************************************//
  IExtensionDisp = dispinterface
    ['{ED4E4ED4-FDD8-476E-AED9-5239E7948257}']
    property OID: IOID readonly dispid 0;
    property IsCritical: WordBool readonly dispid 1;
    property EncodedData: IEncodedData readonly dispid 2;
  end;

// *********************************************************************//
// Interface: ICPrivateKey
// Flags:     (512) Restricted
// GUID:      {50F241B7-A8F2-4E0A-B982-4BD7DF0CCF3C}
// *********************************************************************//
  ICPrivateKey = interface(IUnknown)
    ['{50F241B7-A8F2-4E0A-B982-4BD7DF0CCF3C}']
    function _GetKeyProvInfo(out pKeyProvInfo: PUserType1): HResult; stdcall;
    function _GetKeyContext(out pKeyContext: PUserType2): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISignedCode
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {84FBCB95-5600-404C-9187-AC25B4CD6E94}
// *********************************************************************//
  ISignedCode = interface(IDispatch)
    ['{84FBCB95-5600-404C-9187-AC25B4CD6E94}']
    function Get_FileName: WideString; safecall;
    procedure Set_FileName(const pVal: WideString); safecall;
    function Get_Description: WideString; safecall;
    procedure Set_Description(const pVal: WideString); safecall;
    function Get_DescriptionURL: WideString; safecall;
    procedure Set_DescriptionURL(const pVal: WideString); safecall;
    function Get_Signer: ISigner2; safecall;
    function Get_TimeStamper: ISigner2; safecall;
    function Get_Certificates: ICertificates2; safecall;
    procedure Sign(const pISigner2: ISigner2); safecall;
    procedure Timestamp(const URL: WideString); safecall;
    procedure Verify(bUIAllowed: WordBool); safecall;
    property FileName: WideString read Get_FileName write Set_FileName;
    property Description: WideString read Get_Description write Set_Description;
    property DescriptionURL: WideString read Get_DescriptionURL write Set_DescriptionURL;
    property Signer: ISigner2 read Get_Signer;
    property TimeStamper: ISigner2 read Get_TimeStamper;
    property Certificates: ICertificates2 read Get_Certificates;
  end;

// *********************************************************************//
// DispIntf:  ISignedCodeDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {84FBCB95-5600-404C-9187-AC25B4CD6E94}
// *********************************************************************//
  ISignedCodeDisp = dispinterface
    ['{84FBCB95-5600-404C-9187-AC25B4CD6E94}']
    property FileName: WideString dispid 0;
    property Description: WideString dispid 1;
    property DescriptionURL: WideString dispid 2;
    property Signer: ISigner2 readonly dispid 3;
    property TimeStamper: ISigner2 readonly dispid 4;
    property Certificates: ICertificates2 readonly dispid 5;
    procedure Sign(const pISigner2: ISigner2); dispid 6;
    procedure Timestamp(const URL: WideString); dispid 7;
    procedure Verify(bUIAllowed: WordBool); dispid 8;
  end;

// *********************************************************************//
// Interface: IHashedData
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9F7F23E8-06F4-42E8-B965-5CBD044BF27F}
// *********************************************************************//
  IHashedData = interface(IDispatch)
    ['{9F7F23E8-06F4-42E8-B965-5CBD044BF27F}']
    function Get_Value: WideString; safecall;
    function Get_Algorithm: CAPICOM_HASH_ALGORITHM; safecall;
    procedure Set_Algorithm(pVal: CAPICOM_HASH_ALGORITHM); safecall;
    procedure Hash(const newVal: WideString); safecall;
    property Value: WideString read Get_Value;
    property Algorithm: CAPICOM_HASH_ALGORITHM read Get_Algorithm write Set_Algorithm;
  end;

// *********************************************************************//
// DispIntf:  IHashedDataDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9F7F23E8-06F4-42E8-B965-5CBD044BF27F}
// *********************************************************************//
  IHashedDataDisp = dispinterface
    ['{9F7F23E8-06F4-42E8-B965-5CBD044BF27F}']
    property Value: WideString readonly dispid 0;
    property Algorithm: CAPICOM_HASH_ALGORITHM dispid 1;
    procedure Hash(const newVal: WideString); dispid 2;
  end;

// *********************************************************************//
// Interface: IUtilities
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EB166CF6-2AE6-44DA-BD96-0C1635D183FE}
// *********************************************************************//
  IUtilities = interface(IDispatch)
    ['{EB166CF6-2AE6-44DA-BD96-0C1635D183FE}']
    function GetRandom(Length: Integer; EncodingType: CAPICOM_ENCODING_TYPE): WideString; safecall;
    function Base64Encode(const SrcString: WideString): WideString; safecall;
    function Base64Decode(const EncodedString: WideString): WideString; safecall;
    function BinaryToHex(const BinaryString: WideString): WideString; safecall;
    function HexToBinary(const HexString: WideString): WideString; safecall;
    function BinaryStringToByteArray(const BinaryString: WideString): OleVariant; safecall;
    function ByteArrayToBinaryString(varByteArray: OleVariant): WideString; safecall;
    function LocalTimeToUTCTime(LocalTime: TDateTime): TDateTime; safecall;
    function UTCTimeToLocalTime(UTCTime: TDateTime): TDateTime; safecall;
  end;

// *********************************************************************//
// DispIntf:  IUtilitiesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EB166CF6-2AE6-44DA-BD96-0C1635D183FE}
// *********************************************************************//
  IUtilitiesDisp = dispinterface
    ['{EB166CF6-2AE6-44DA-BD96-0C1635D183FE}']
    function GetRandom(Length: Integer; EncodingType: CAPICOM_ENCODING_TYPE): WideString; dispid 1;
    function Base64Encode(const SrcString: WideString): WideString; dispid 2;
    function Base64Decode(const EncodedString: WideString): WideString; dispid 3;
    function BinaryToHex(const BinaryString: WideString): WideString; dispid 4;
    function HexToBinary(const HexString: WideString): WideString; dispid 5;
    function BinaryStringToByteArray(const BinaryString: WideString): OleVariant; dispid 6;
    function ByteArrayToBinaryString(varByteArray: OleVariant): WideString; dispid 7;
    function LocalTimeToUTCTime(LocalTime: TDateTime): TDateTime; dispid 8;
    function UTCTimeToLocalTime(UTCTime: TDateTime): TDateTime; dispid 9;
  end;

// *********************************************************************//
// Die Klasse CoSettings stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface ISettings, dargestellt
// von CoClass Settings, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoSettings = class
    class function Create: ISettings;
    class function CreateRemote(const MachineName: string): ISettings;
  end;

// *********************************************************************//
// Die Klasse CoEKU stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IEKU, dargestellt
// von CoClass EKU, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoEKU = class
    class function Create: IEKU;
    class function CreateRemote(const MachineName: string): IEKU;
  end;

// *********************************************************************//
// Die Klasse CoEKUs stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IEKUs, dargestellt
// von CoClass EKUs, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoEKUs = class
    class function Create: IEKUs;
    class function CreateRemote(const MachineName: string): IEKUs;
  end;

// *********************************************************************//
// Die Klasse CoKeyUsage stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IKeyUsage, dargestellt
// von CoClass KeyUsage, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoKeyUsage = class
    class function Create: IKeyUsage;
    class function CreateRemote(const MachineName: string): IKeyUsage;
  end;

// *********************************************************************//
// Die Klasse CoExtendedKeyUsage stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IExtendedKeyUsage, dargestellt
// von CoClass ExtendedKeyUsage, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoExtendedKeyUsage = class
    class function Create: IExtendedKeyUsage;
    class function CreateRemote(const MachineName: string): IExtendedKeyUsage;
  end;

// *********************************************************************//
// Die Klasse CoBasicConstraints stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IBasicConstraints, dargestellt
// von CoClass BasicConstraints, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoBasicConstraints = class
    class function Create: IBasicConstraints;
    class function CreateRemote(const MachineName: string): IBasicConstraints;
  end;

// *********************************************************************//
// Die Klasse CoCertificateStatus stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface ICertificateStatus3, dargestellt
// von CoClass CertificateStatus, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoCertificateStatus = class
    class function Create: ICertificateStatus3;
    class function CreateRemote(const MachineName: string): ICertificateStatus3;
  end;

// *********************************************************************//
// Die Klasse CoCertificate stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface ICertificate2, dargestellt
// von CoClass Certificate, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoCertificate = class
    class function Create: ICertificate2;
    class function CreateRemote(const MachineName: string): ICertificate2;
  end;

// *********************************************************************//
// Die Klasse CoCertificates stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface ICertificates2, dargestellt
// von CoClass Certificates, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoCertificates = class
    class function Create: ICertificates2;
    class function CreateRemote(const MachineName: string): ICertificates2;
  end;

// *********************************************************************//
// Die Klasse CoChain stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IChain2, dargestellt
// von CoClass Chain, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoChain = class
    class function Create: IChain2;
    class function CreateRemote(const MachineName: string): IChain2;
  end;

// *********************************************************************//
// Die Klasse CoStore stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IStore3, dargestellt
// von CoClass Store, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoStore = class
    class function Create: IStore3;
    class function CreateRemote(const MachineName: string): IStore3;
  end;

// *********************************************************************//
// Die Klasse CoAttribute stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IAttribute, dargestellt
// von CoClass Attribute, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoAttribute = class
    class function Create: IAttribute;
    class function CreateRemote(const MachineName: string): IAttribute;
  end;

// *********************************************************************//
// Die Klasse CoAttributes stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IAttributes, dargestellt
// von CoClass Attributes, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoAttributes = class
    class function Create: IAttributes;
    class function CreateRemote(const MachineName: string): IAttributes;
  end;

// *********************************************************************//
// Die Klasse CoSigner stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface ISigner2, dargestellt
// von CoClass Signer, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoSigner = class
    class function Create: ISigner2;
    class function CreateRemote(const MachineName: string): ISigner2;
  end;

// *********************************************************************//
// Die Klasse CoSigners stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface ISigners, dargestellt
// von CoClass Signers, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoSigners = class
    class function Create: ISigners;
    class function CreateRemote(const MachineName: string): ISigners;
  end;

// *********************************************************************//
// Die Klasse CoSignedData stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface ISignedData, dargestellt
// von CoClass SignedData, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoSignedData = class
    class function Create: ISignedData;
    class function CreateRemote(const MachineName: string): ISignedData;
  end;

// *********************************************************************//
// Die Klasse CoAlgorithm stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IAlgorithm, dargestellt
// von CoClass Algorithm, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoAlgorithm = class
    class function Create: IAlgorithm;
    class function CreateRemote(const MachineName: string): IAlgorithm;
  end;

// *********************************************************************//
// Die Klasse CoRecipients stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IRecipients, dargestellt
// von CoClass Recipients, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoRecipients = class
    class function Create: IRecipients;
    class function CreateRemote(const MachineName: string): IRecipients;
  end;

// *********************************************************************//
// Die Klasse CoEnvelopedData stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IEnvelopedData, dargestellt
// von CoClass EnvelopedData, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoEnvelopedData = class
    class function Create: IEnvelopedData;
    class function CreateRemote(const MachineName: string): IEnvelopedData;
  end;

// *********************************************************************//
// Die Klasse CoEncryptedData stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IEncryptedData, dargestellt
// von CoClass EncryptedData, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoEncryptedData = class
    class function Create: IEncryptedData;
    class function CreateRemote(const MachineName: string): IEncryptedData;
  end;

// *********************************************************************//
// Die Klasse CoOID stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IOID, dargestellt
// von CoClass OID, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoOID = class
    class function Create: IOID;
    class function CreateRemote(const MachineName: string): IOID;
  end;

// *********************************************************************//
// Die Klasse CoOIDs stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IOIDs, dargestellt
// von CoClass OIDs, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoOIDs = class
    class function Create: IOIDs;
    class function CreateRemote(const MachineName: string): IOIDs;
  end;

// *********************************************************************//
// Die Klasse CoNoticeNumbers stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface INoticeNumbers, dargestellt
// von CoClass NoticeNumbers, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoNoticeNumbers = class
    class function Create: INoticeNumbers;
    class function CreateRemote(const MachineName: string): INoticeNumbers;
  end;

// *********************************************************************//
// Die Klasse CoQualifier stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IQualifier, dargestellt
// von CoClass Qualifier, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoQualifier = class
    class function Create: IQualifier;
    class function CreateRemote(const MachineName: string): IQualifier;
  end;

// *********************************************************************//
// Die Klasse CoQualifiers stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IQualifiers, dargestellt
// von CoClass Qualifiers, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoQualifiers = class
    class function Create: IQualifiers;
    class function CreateRemote(const MachineName: string): IQualifiers;
  end;

// *********************************************************************//
// Die Klasse CoPolicyInformation stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IPolicyInformation, dargestellt
// von CoClass PolicyInformation, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoPolicyInformation = class
    class function Create: IPolicyInformation;
    class function CreateRemote(const MachineName: string): IPolicyInformation;
  end;

// *********************************************************************//
// Die Klasse CoCertificatePolicies stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface ICertificatePolicies, dargestellt
// von CoClass CertificatePolicies, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoCertificatePolicies = class
    class function Create: ICertificatePolicies;
    class function CreateRemote(const MachineName: string): ICertificatePolicies;
  end;

// *********************************************************************//
// Die Klasse CoEncodedData stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IEncodedData, dargestellt
// von CoClass EncodedData, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoEncodedData = class
    class function Create: IEncodedData;
    class function CreateRemote(const MachineName: string): IEncodedData;
  end;

// *********************************************************************//
// Die Klasse CoExtension stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IExtension, dargestellt
// von CoClass Extension, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoExtension = class
    class function Create: IExtension;
    class function CreateRemote(const MachineName: string): IExtension;
  end;

// *********************************************************************//
// Die Klasse CoExtensions stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IExtensions, dargestellt
// von CoClass Extensions, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoExtensions = class
    class function Create: IExtensions;
    class function CreateRemote(const MachineName: string): IExtensions;
  end;

// *********************************************************************//
// Die Klasse CoExtendedProperty stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IExtendedProperty, dargestellt
// von CoClass ExtendedProperty, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoExtendedProperty = class
    class function Create: IExtendedProperty;
    class function CreateRemote(const MachineName: string): IExtendedProperty;
  end;

// *********************************************************************//
// Die Klasse CoExtendedProperties stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IExtendedProperties, dargestellt
// von CoClass ExtendedProperties, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoExtendedProperties = class
    class function Create: IExtendedProperties;
    class function CreateRemote(const MachineName: string): IExtendedProperties;
  end;

// *********************************************************************//
// Die Klasse CoTemplate stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface ITemplate, dargestellt
// von CoClass Template, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoTemplate = class
    class function Create: ITemplate;
    class function CreateRemote(const MachineName: string): ITemplate;
  end;

// *********************************************************************//
// Die Klasse CoPublicKey stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IPublicKey, dargestellt
// von CoClass PublicKey, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoPublicKey = class
    class function Create: IPublicKey;
    class function CreateRemote(const MachineName: string): IPublicKey;
  end;

// *********************************************************************//
// Die Klasse CoPrivateKey stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IPrivateKey, dargestellt
// von CoClass PrivateKey, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoPrivateKey = class
    class function Create: IPrivateKey;
    class function CreateRemote(const MachineName: string): IPrivateKey;
  end;

// *********************************************************************//
// Die Klasse CoSignedCode stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface ISignedCode, dargestellt
// von CoClass SignedCode, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoSignedCode = class
    class function Create: ISignedCode;
    class function CreateRemote(const MachineName: string): ISignedCode;
  end;

// *********************************************************************//
// Die Klasse CoHashedData stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IHashedData, dargestellt
// von CoClass HashedData, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoHashedData = class
    class function Create: IHashedData;
    class function CreateRemote(const MachineName: string): IHashedData;
  end;

// *********************************************************************//
// Die Klasse CoUtilities stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IUtilities, dargestellt
// von CoClass Utilities, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoUtilities = class
    class function Create: IUtilities;
    class function CreateRemote(const MachineName: string): IUtilities;
  end;

implementation

uses ComObj;

class function CoSettings.Create: ISettings;
begin
  Result := CreateComObject(CLASS_Settings) as ISettings;
end;

class function CoSettings.CreateRemote(const MachineName: string): ISettings;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Settings) as ISettings;
end;

class function CoEKU.Create: IEKU;
begin
  Result := CreateComObject(CLASS_EKU) as IEKU;
end;

class function CoEKU.CreateRemote(const MachineName: string): IEKU;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_EKU) as IEKU;
end;

class function CoEKUs.Create: IEKUs;
begin
  Result := CreateComObject(CLASS_EKUs) as IEKUs;
end;

class function CoEKUs.CreateRemote(const MachineName: string): IEKUs;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_EKUs) as IEKUs;
end;

class function CoKeyUsage.Create: IKeyUsage;
begin
  Result := CreateComObject(CLASS_KeyUsage) as IKeyUsage;
end;

class function CoKeyUsage.CreateRemote(const MachineName: string): IKeyUsage;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_KeyUsage) as IKeyUsage;
end;

class function CoExtendedKeyUsage.Create: IExtendedKeyUsage;
begin
  Result := CreateComObject(CLASS_ExtendedKeyUsage) as IExtendedKeyUsage;
end;

class function CoExtendedKeyUsage.CreateRemote(const MachineName: string): IExtendedKeyUsage;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ExtendedKeyUsage) as IExtendedKeyUsage;
end;

class function CoBasicConstraints.Create: IBasicConstraints;
begin
  Result := CreateComObject(CLASS_BasicConstraints) as IBasicConstraints;
end;

class function CoBasicConstraints.CreateRemote(const MachineName: string): IBasicConstraints;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_BasicConstraints) as IBasicConstraints;
end;

class function CoCertificateStatus.Create: ICertificateStatus3;
begin
  Result := CreateComObject(CLASS_CertificateStatus) as ICertificateStatus3;
end;

class function CoCertificateStatus.CreateRemote(const MachineName: string): ICertificateStatus3;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CertificateStatus) as ICertificateStatus3;
end;

class function CoCertificate.Create: ICertificate2;
begin
  Result := CreateComObject(CLASS_Certificate) as ICertificate2;
end;

class function CoCertificate.CreateRemote(const MachineName: string): ICertificate2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Certificate) as ICertificate2;
end;

class function CoCertificates.Create: ICertificates2;
begin
  Result := CreateComObject(CLASS_Certificates) as ICertificates2;
end;

class function CoCertificates.CreateRemote(const MachineName: string): ICertificates2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Certificates) as ICertificates2;
end;

class function CoChain.Create: IChain2;
begin
  Result := CreateComObject(CLASS_Chain) as IChain2;
end;

class function CoChain.CreateRemote(const MachineName: string): IChain2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Chain) as IChain2;
end;

class function CoStore.Create: IStore3;
begin
  Result := CreateComObject(CLASS_Store) as IStore3;
end;

class function CoStore.CreateRemote(const MachineName: string): IStore3;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Store) as IStore3;
end;

class function CoAttribute.Create: IAttribute;
begin
  Result := CreateComObject(CLASS_Attribute) as IAttribute;
end;

class function CoAttribute.CreateRemote(const MachineName: string): IAttribute;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Attribute) as IAttribute;
end;

class function CoAttributes.Create: IAttributes;
begin
  Result := CreateComObject(CLASS_Attributes) as IAttributes;
end;

class function CoAttributes.CreateRemote(const MachineName: string): IAttributes;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Attributes) as IAttributes;
end;

class function CoSigner.Create: ISigner2;
begin
  Result := CreateComObject(CLASS_Signer) as ISigner2;
end;

class function CoSigner.CreateRemote(const MachineName: string): ISigner2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Signer) as ISigner2;
end;

class function CoSigners.Create: ISigners;
begin
  Result := CreateComObject(CLASS_Signers) as ISigners;
end;

class function CoSigners.CreateRemote(const MachineName: string): ISigners;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Signers) as ISigners;
end;

class function CoSignedData.Create: ISignedData;
begin
  Result := CreateComObject(CLASS_SignedData) as ISignedData;
end;

class function CoSignedData.CreateRemote(const MachineName: string): ISignedData;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SignedData) as ISignedData;
end;

class function CoAlgorithm.Create: IAlgorithm;
begin
  Result := CreateComObject(CLASS_Algorithm) as IAlgorithm;
end;

class function CoAlgorithm.CreateRemote(const MachineName: string): IAlgorithm;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Algorithm) as IAlgorithm;
end;

class function CoRecipients.Create: IRecipients;
begin
  Result := CreateComObject(CLASS_Recipients) as IRecipients;
end;

class function CoRecipients.CreateRemote(const MachineName: string): IRecipients;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Recipients) as IRecipients;
end;

class function CoEnvelopedData.Create: IEnvelopedData;
begin
  Result := CreateComObject(CLASS_EnvelopedData) as IEnvelopedData;
end;

class function CoEnvelopedData.CreateRemote(const MachineName: string): IEnvelopedData;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_EnvelopedData) as IEnvelopedData;
end;

class function CoEncryptedData.Create: IEncryptedData;
begin
  Result := CreateComObject(CLASS_EncryptedData) as IEncryptedData;
end;

class function CoEncryptedData.CreateRemote(const MachineName: string): IEncryptedData;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_EncryptedData) as IEncryptedData;
end;

class function CoOID.Create: IOID;
begin
  Result := CreateComObject(CLASS_OID) as IOID;
end;

class function CoOID.CreateRemote(const MachineName: string): IOID;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_OID) as IOID;
end;

class function CoOIDs.Create: IOIDs;
begin
  Result := CreateComObject(CLASS_OIDs) as IOIDs;
end;

class function CoOIDs.CreateRemote(const MachineName: string): IOIDs;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_OIDs) as IOIDs;
end;

class function CoNoticeNumbers.Create: INoticeNumbers;
begin
  Result := CreateComObject(CLASS_NoticeNumbers) as INoticeNumbers;
end;

class function CoNoticeNumbers.CreateRemote(const MachineName: string): INoticeNumbers;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_NoticeNumbers) as INoticeNumbers;
end;

class function CoQualifier.Create: IQualifier;
begin
  Result := CreateComObject(CLASS_Qualifier) as IQualifier;
end;

class function CoQualifier.CreateRemote(const MachineName: string): IQualifier;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Qualifier) as IQualifier;
end;

class function CoQualifiers.Create: IQualifiers;
begin
  Result := CreateComObject(CLASS_Qualifiers) as IQualifiers;
end;

class function CoQualifiers.CreateRemote(const MachineName: string): IQualifiers;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Qualifiers) as IQualifiers;
end;

class function CoPolicyInformation.Create: IPolicyInformation;
begin
  Result := CreateComObject(CLASS_PolicyInformation) as IPolicyInformation;
end;

class function CoPolicyInformation.CreateRemote(const MachineName: string): IPolicyInformation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_PolicyInformation) as IPolicyInformation;
end;

class function CoCertificatePolicies.Create: ICertificatePolicies;
begin
  Result := CreateComObject(CLASS_CertificatePolicies) as ICertificatePolicies;
end;

class function CoCertificatePolicies.CreateRemote(const MachineName: string): ICertificatePolicies;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CertificatePolicies) as ICertificatePolicies;
end;

class function CoEncodedData.Create: IEncodedData;
begin
  Result := CreateComObject(CLASS_EncodedData) as IEncodedData;
end;

class function CoEncodedData.CreateRemote(const MachineName: string): IEncodedData;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_EncodedData) as IEncodedData;
end;

class function CoExtension.Create: IExtension;
begin
  Result := CreateComObject(CLASS_Extension) as IExtension;
end;

class function CoExtension.CreateRemote(const MachineName: string): IExtension;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Extension) as IExtension;
end;

class function CoExtensions.Create: IExtensions;
begin
  Result := CreateComObject(CLASS_Extensions) as IExtensions;
end;

class function CoExtensions.CreateRemote(const MachineName: string): IExtensions;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Extensions) as IExtensions;
end;

class function CoExtendedProperty.Create: IExtendedProperty;
begin
  Result := CreateComObject(CLASS_ExtendedProperty) as IExtendedProperty;
end;

class function CoExtendedProperty.CreateRemote(const MachineName: string): IExtendedProperty;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ExtendedProperty) as IExtendedProperty;
end;

class function CoExtendedProperties.Create: IExtendedProperties;
begin
  Result := CreateComObject(CLASS_ExtendedProperties) as IExtendedProperties;
end;

class function CoExtendedProperties.CreateRemote(const MachineName: string): IExtendedProperties;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ExtendedProperties) as IExtendedProperties;
end;

class function CoTemplate.Create: ITemplate;
begin
  Result := CreateComObject(CLASS_Template) as ITemplate;
end;

class function CoTemplate.CreateRemote(const MachineName: string): ITemplate;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Template) as ITemplate;
end;

class function CoPublicKey.Create: IPublicKey;
begin
  Result := CreateComObject(CLASS_PublicKey) as IPublicKey;
end;

class function CoPublicKey.CreateRemote(const MachineName: string): IPublicKey;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_PublicKey) as IPublicKey;
end;

class function CoPrivateKey.Create: IPrivateKey;
begin
  Result := CreateComObject(CLASS_PrivateKey) as IPrivateKey;
end;

class function CoPrivateKey.CreateRemote(const MachineName: string): IPrivateKey;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_PrivateKey) as IPrivateKey;
end;

class function CoSignedCode.Create: ISignedCode;
begin
  Result := CreateComObject(CLASS_SignedCode) as ISignedCode;
end;

class function CoSignedCode.CreateRemote(const MachineName: string): ISignedCode;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SignedCode) as ISignedCode;
end;

class function CoHashedData.Create: IHashedData;
begin
  Result := CreateComObject(CLASS_HashedData) as IHashedData;
end;

class function CoHashedData.CreateRemote(const MachineName: string): IHashedData;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_HashedData) as IHashedData;
end;

class function CoUtilities.Create: IUtilities;
begin
  Result := CreateComObject(CLASS_Utilities) as IUtilities;
end;

class function CoUtilities.CreateRemote(const MachineName: string): IUtilities;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Utilities) as IUtilities;
end;

end.
