{@abstract(Contains Security Descriptor classes that are used by the units of JWSCL)
@author(Christian Wimmer)
@created(03/23/2007)
@lastmod(09/10/2007)

Project JEDI Windows Security Code Library (JWSCL)

The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License. You may obtain a copy of the
License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the  
GNU Lesser General Public License (the  "LGPL License"), in which case the   
provisions of the LGPL License are applicable instead of those above.        
If you wish to allow use of your version of this file only under the terms   
of the LGPL License and not to allow others to use your version of this file 
under the MPL, indicate your decision by deleting  the provisions above and  
replace  them with the notice and other provisions required by the LGPL      
License.  If you do not delete the provisions above, a recipient may use     
your version of this file under either the MPL or the LGPL License.          
                                                                             
For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html 

The Original Code is JwsclSid.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

Description:
This unit contains ansi- and unicode string types that are used by the units of JWSCL.
You can define UNICODE to use unicode strings. Otherwise ansicode will be used.

}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclSid;
{$INCLUDE Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $

interface

uses
  SysUtils, Contnrs, Classes,
  JwaWindows, JwaVista, JwsclUtils, JwsclResource,
  JwsclTypes, JwsclExceptions, JwsclEnumerations,
  JwsclVersion, JwsclConstants, JwsclProcess,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
type
  TJwSecurityId = class;

    {@Name is a class that contains a list of SIDs instances.
     The SID instances can be freed automatically at the end of the list instance.}
  TJwSecurityIdList = class(TObjectList)
  protected
    function GetItem(idx: integer): TJwSecurityId;
  public
      {@Name creates a list of SIDs that are provided in a TOKEN_GROUPS structure.
       The SIDs are copied.
       @param(ownSIDs defines whether the SIDs are freed on the end of the list instance (true) or not)
       @param(token_groups contains a list of SIDs to be copied into new instances)

       @raises(EJwsclWinCallFailedException will be raised if a call to CopySID failed)
      }
    constructor Create(ownSIDs: boolean; token_groups: PTOKEN_GROUPS);
      overload;

    constructor Create(SidAndAttributesArray : PSidAndAttributesArray);
      overload;

      {@Name creates an empty list.
      @param(ownSIDs defines whether the SIDs are freed on the end of the list instance (true) or not)}
    constructor Create(ownSIDs: boolean); overload;

      {@Name adds a SID instance to end end of the list
       @return(@Name returns the index in the list where the SID was added)
       @raises(EJwsclDuplicateListEntryException will be raised if the given SID is already in list)}
    function Add(AObject: TJwSecurityId): integer;

      {@Name returns the first SID from the list.
       @return(The first SID or if the list is empty @Name returns nil.)
       }
    function First: TJwSecurityId;

      {@Name returns the list index of a SID from the list.
       The SID instance is searched not the ID. To find an equal SID use FindSid.
       @return(Index of list. If the SID is not in list the return value is -1.)
       }
    function IndexOf(AObject: TJwSecurityId): integer;

      {@Name seeks for a given SID using its identifier.
       The function does not raise an exception.

       @param aSID a SID instance which identifier is searsched in the list
       @param startpos defines the start index of the search excluding the given index.
       @param usePrefix defines whether EqualPrefixSid (true) or EqualSid (false) is used to search the SID
       @return Returns the index of the found SID in the list; otherwise -1 if the SID could not be found.

      }
    function FindSid(aSID: TJwSecurityId; startpos: integer = -1;
      usePrefix: boolean = False): integer;

      {@Name inserts a SID into the list.
       @param(Index defines a list index where the SID is to be inserted .
              If the Index is not between 0 and Count-1 an exception will be raised.)
       }
    procedure Insert(Index: integer; AObject: TJwSecurityId);

      {@Name returns the last SID of the list.
       If the list is empty the return value is nil.
       }
    function Last: TJwSecurityId;

      {@Name removes a SID from the list. If the List owns the object
       the SID will also be freed.
       @param(aObject contains the SID to be removed)
       @return(@Name returns the index of the SID in the list before it was removed. If
                 the SID could not be found the return value is -1.) }
    function Remove(AObject: TJwSecurityId): integer;

      {@Name creates an array of "SID and Attributes" structures.
       The return value must be freed by  Free_PSID_Array.
        The attributes of the SIDs are also saved in the newly created structure.

       The array consists of two structues :
        1. SidAndAttributes
        2. Pointers to SIDs in SidAndAttributes.SID

      .}
    function Create_PSID_Array: PSidAndAttributesArray; virtual;

      {@Name creates a token groups structure using the SIDs in the list of the instance.
       The attributes of the SIDs are also saved in the newly created structure.
        Use Free_PTOKEN_GROUPS to free the structure.}
    function Create_PTOKEN_GROUPS: PTOKEN_GROUPS; virtual;

      {@Name frees a "SID and Attributes" structures created by Create_PSID_Array.
       @param(sids contains the "SID and Attributes" structures to be freed. If nil nothing happens.
             The parameter will be nil afterwards.)
      }
    class procedure Free_PSID_Array(var sids: PSidAndAttributesArray); virtual;

      {@Name frees the memory of a token groups structure that was created by Create_PTOKEN_GROUPS.
         The parameter will be nil afterwards.
      }
    class procedure Free_PTOKEN_GROUPS(var sids: PTOKEN_GROUPS); virtual;

      {@Name returns a string that contains for each list entry (SID)
       a text that contains domain, account name and humand readable SID structure.
       The form is "[domain@]name (S-1-XXXXXX)" where [] is optional.
       Every SID string is seperated by #13#10.
      }
    function GetText(ignoreExceptions: boolean = False): TJwString;

  public
      {@Name gives the possibility to access every SID in the list.
       However the entries cannot be reset.

       Do not free a SID through this property. Use instead Remove.
       }
    property Items[Index: integer]: TJwSecurityId Read GetItem; default;

  end;

  {@Name defines a type that represents a sub authority. It is a dynamic array of Cardinal values.}
  TJwSubAuthorityArray = array of Cardinal;

    {
    The security identifier (SID) structure is a variable-length structure used to uniquely identify users or groups.

    @ClassName encapsulates a SID structures and provides functions to create, read and convert SID data.
    Once an instance is created the SID data cannot be changed. Therefore there is no Assign function to copy data.

    MSDN on http://msdn2.microsoft.com/en-us/library/aa379594.aspx
                     http://msdn2.microsoft.com/en-us/library/aa379597.aspx
    }
  TJwSecurityId = class(TObject)
  private
    fWellKnownSidType: TWellKnownSidType;


    fSID: PSID;

    fAttributes: Cardinal;

    {@Name contains the values of the properties of the instance for debugging purposes.}
    fDbgData: string;

      {@Name disables the raising of exceptions in the following methods :
        +GetAccountSidString
       It is used for debugging purposes (hiding internal exceptions) only.
      }
    fDbgDisableException: boolean;

      {@Name updates the variable @link(fDbgData) for debugging purposes.
       It is called in the constructors of @classname.
        }
    procedure UpdateDbgData; virtual;
  public
      {@Name allocates memory for a SID and returns the pointer to it.
       The size of allocated memory is always the maximum possible size of a
        SID. The constant SECURITY_MAX_SID_SIZE is used.
       The data must be freed by @link(FreeSID).
       @return(@Name returns a pointer to the newly created SID)
       @raises(EJwsclNotEnoughMemory will be raised if not enough memory is available)
       }
    class function NewSID: PSID;

      {@Name frees a SID allocated by @Link(NewSID).
       @param(SID contains the sid memory to be freed. If the variable is nil nothing will happen.
              The parameter will be nil after a succesfull call)
       }
    class procedure FreeSID(var SID: PSID);

      {@Name creates a new SID initilized sid. The values are copied from this SID instance.
       The SID must be freed by @link(FreeSID).
       }
    function CreateCopyOfSID: PSID;
  protected
      {@Name returns the well known sid type of the sid instancel.
        The value is the parameter value given in CreateWellKnownSid as parameter WellKnownSidType.
        Otherwise the value is always WinNullSid;

       @return(Returns the well known sid type. The following types are available :
    @unorderedlist(
    @item WinNullSid,
    @item WinWorldSid,
    @item WinLocalSid,
    @item WinCreatorOwnerSid,
    @item WinCreatorGroupSid,
    @item WinCreatorOwnerServerSid,
    @item WinCreatorGroupServerSid,
    @item WinNtAuthoritySid,
    @item WinDialupSid,
    @item WinNetworkSid,
    @item WinBatchSid,
    @item WinInteractiveSid,
    @item WinServiceSid,
    @item WinAnonymousSid,
    @item WinProxySid,
    @item WinEnterpriseControllersSid,
    @item WinSelfSid,
    @item WinAuthenticatedUserSid,
    @item WinRestrictedCodeSid,
    @item WinTerminalServerSid,
    @item WinRemoteLogonIdSid,
    @item WinLogonIdsSid,
    @item WinLocalSystemSid,
    @item WinLocalServiceSid,
    @item WinNetworkServiceSid,
    @item WinBuiltinDomainSid,
    @item WinBuiltinAdministratorsSid,
    @item WinBuiltinUsersSid,
    @item WinBuiltinGuestsSid,
    @item WinBuiltinPowerUsersSid,
    @item WinBuiltinAccountOperatorsSid,
    @item WinBuiltinSystemOperatorsSid,
    @item WinBuiltinPrintOperatorsSid,
    @item WinBuiltinBackupOperatorsSid,
    @item WinBuiltinReplicatorSid,
    @item WinBuiltinPreWindows2000CompatibleAccessSid,
    @item WinBuiltinRemoteDesktopUsersSid,
    @item WinBuiltinNetworkConfigurationOperatorsSid,
    @item WinAccountAdministratorSid,
    @item WinAccountGuestSid,
    @item WinAccountKrbtgtSid,
    @item WinAccountDomainAdminsSid,
    @item WinAccountDomainUsersSid,
    @item WinAccountDomainGuestsSid,
    @item WinAccountComputersSid,
    @item WinAccountControllersSid,
    @item WinAccountCertAdminsSid,
    @item WinAccountSchemaAdminsSid,
    @item WinAccountEnterpriseAdminsSid,
    @item WinAccountPolicyAdminsSid,
    @item WinAccountRasAndIasServersSid,
    @item WinNTLMAuthenticationSid,
    @item WinDigestAuthenticationSid,
    @item WinSChannelAuthenticationSid,
    @item WinThisOrganizationSid,
    @item WinOtherOrganizationSid,
    @item WinBuiltinIncomingForestTrustBuildersSid,
    @item WinBuiltinPerfMonitoringUsersSid,
    @item WinBuiltinPerfLoggingUsersSid,
    @item WinBuiltinAuthorizationAccessSid,
    @item WinBuiltinTerminalServerLicenseServersSid
    )
    )
      }
    function GetWellKnownSidType: TWellKnownSidType;

      {@Name returns whether this instance contains a well known sid.

      }
    function GetWellKnownSid: boolean;

    {@seealso(SubAuthorityCount)}
    function GetSidSubAuthorityCount: Cardinal;
    {@seealso(SubAuthorityArray)}
    function GetSidSubAuthorityArray: TJwSubAuthorityArray;
    {@seealso(SubAuthority)}
    function GetSidSubAuthority(Index: Cardinal): Cardinal;

    {@seealso(IdentifierAuthority)}
    function GetSidIdentifierAuthority: TSidIdentifierAuthority;

    {@seealso(SubAuthority)}
    function GetLengthSID: Cardinal;

    {@seealso(SubAuthority)}
    function GetStringSID: TJwString;

    function GetTrustee: TTrusteeEx; virtual;

      {@Name initialises internal SID structures.
       However this constructor is not public because the SID structure is NULL
        and cannot be changed from outside.
       }
    constructor Create; overload;
  public
      {@Name returns the domain sid of the sid instance.
       @return(@Name returns the sid domain of the sid instance)
       @raises(EJwsclWinCallFailedException if the call to a winapi function failed)
       }
    function GetWindowsAccountDomainSid: TJwSecurityId; overload;
      {@Name returns the domain sid of the sid instance given as parameter.
        The actual SID instance will be checked with CheckSID. Also the
         the parameter aSID and the result will be checked in this way.

       @param(aSID contains the sid to be examined.)
       @return(@Name returns the sid domain of the sid instance.)
       @raises(EJwsclNILParameterException if parameter aSID is nil)
       @raises(EJwsclWinCallFailedException if the call to a winapi function failed)
       @raises(EJwsclSecurityException See @link(CheckSID) for more exceptions)

       }
    class function GetWindowsAccountDomainSid(aSID: TJwSecurityId): TJwSecurityId;
      overload;

      {@Name tests two security-identifier (SID) prefix values for equality.
       A SID prefix is the entire SID except for the last subauthority value
       @param(pSid1 the SID to be compared with the actual sid instance)
       @return(If the SID prefixes are equal, the return value is true; otherwise false)
       @raises(EJwsclNILParameterException if parameter pSID1 is nil)
       @raises(EJwsclWinCallFailedException if the call to a winapi function failed)

      }
    function EqualPrefixSid(pSid1: TJwSecurityId): boolean;virtual;
      {@Name tests two security-identifier (SID) values for equality.
       @param(pSid1 the SID to be compared with the actual sid instance)
       @return(If the SID prefixes are equal, the return value is true; otherwise false)
       @raises(EJwsclNILParameterException if parameter pSID1 is nil)
       @raises(EJwsclWinCallFailedException if the call to a winapi function failed)
      }
    function EqualSid(pSid1: TJwSecurityId): boolean;virtual;

      {@Name determines whether two SIDs are from the same domain.
       @param(pSid1 the SID to be compared with the actual sid instance)
       @return(If the SID domains are equal, the return value is true; otherwise false)
       @raises(EJwsclNILParameterException if parameter pSID1 is nil)
       @raises(EJwsclWinCallFailedException if the call to a winapi function failed)
      }
    function EqualDomainSid(pSid1: TJwSecurityId): boolean; virtual;

      {@Name gets the domain and acount name of the SID. It also returns the type of Sid Name.
       @param(SystemName contains the target computer name. It can be null to use the local system.)
       @param(DomainName [out] gets the domain name of this SID)
       @param(SidNameUse [out] gets the SID use type. It will be one of the following constant :
          @unorderedlist(
            @item SidTypeUser           = 1;
            @item SidTypeGroup          = 2;
            @item SidTypeDomain         = 3;
            @item SidTypeAlias          = 4;
            @item SidTypeWellKnownGroup = 5;
            @item SidTypeDeletedAccount = 6;
            @item SidTypeInvalid        = 7;
            @item SidTypeUnknown        = 8;
            @item SidTypeComputer       = 9;
        ))
       @return(@Name returns the account name associated by this SID.)
       @raises(EJwsclWinCallFailedException if the call to a winapi function failed)
       @raises(EJwsclSecurityException See @link(CheckSID) for more exceptions)
       }
    function GetAccountSidString(const SystemName: TJwString;
      out DomainName: TJwString; out SidNameUse: TSidNameUse): TJwString; virtual;

      {@Name returns the account name of the SID on the computer given in SystemName.
       For more information see the see also section.
       @raises(EJwsclWinCallFailedException if the call to a winapi function failed)
       @Seealso(GetAccountSidString);
       }
    function GetAccountName(SystemName: TJwString): TJwString; virtual;

    function GetCachedUserFromSid : WideString; virtual;

      {@Name returns the domain account name of the SID on the computer given in SystemName.
       For more information see the see also section.
       @raises(EJwsclWinCallFailedException if the call to a winapi function failed)
       @Seealso(GetAccountSidString);
       }
    function GetAccountDomainName(SystemName: TJwString): TJwString; virtual;

      {@Name returns the account name use of the SID on the computer given in SystemName.
       For more information see the see also section.
       @raises(EJwsclWinCallFailedException if the call to a winapi function failed)
       @Seealso(GetAccountSidString);
       }
    function GetAccountNameUse(SystemName: TJwString): TSidNameUse; virtual;

    {@Name creates a TSidIdentifierAuthority structure from values.}
    class function CreateSidIdentifierAuthority(Value1, Value2,
      Value3, Value4, Value5, Value6: byte): TSidIdentifierAuthority;virtual;

      {@Name allocates memory for a SID and Attributes structure.
       The SID instance will be copied into a new SID structure that is attached to the returned structure in its SID value.
       The new created structure must be freed by Free_PSID_AND_ATTRIBUTES.

       Actually two memory blocks will be allocated and initialized:
         1. PSID_AND_ATTRIBUTES
         2. PSID

       @param(attributes contains a value for the attribute componente of the SID and Attributes structure.)
       @raises(EJwsclNotEnoughMemory will be raised if not enough memory is available)
       }
    function Create_PSID_AND_ATTRIBUTES(attributes: Cardinal = 0):
      PSID_AND_ATTRIBUTES; virtual;

      {@Name frees the memory allocated by Create_PSID_AND_ATTRIBUTES.

       @param(sids contains the PSID_AND_ATTRIBUTES structure to be freed.
            If sids is nil nothing happens. If the memory could be freed successfully the parameter sids will be set to nil)
       )
      }
    procedure Free_PSID_AND_ATTRIBUTES(var sids: PSID_AND_ATTRIBUTES);
      virtual;

      {@Name checks if the SID instance contains a valid SID structure.
       If SID is not a correct SID, an exception will be raised.

       @raises(EJwsclInvalidSIDException if the property SID is nil OR
                if the SID structure is invalid due to a call to MSDN function IsValidSid (http://msdn2.microsoft.com/en-us/library/aa379151.aspx).
                  The IsValidSid function validates a security identifier (SID) by verifying that the revision number is within a known range, and that the number of subauthorities is less than the maximum.
                 )
      }
    procedure CheckSID;

      {@Name creates a text that contains domain, account name and humand readable SID structure.
       The form is "[domain@]name (S-1-XXXXXX)" where [] is optional.
       @param(ignoreExceptions if set to true ignores exceptions that are thrown)
       @return(Returns the string value)
      }
    function GetText(ignoreExceptions: boolean = False): TJwString; virtual;

      {@Name checks if the SID is a SID defined in unit JwsclKnownSid.
       If this SID is a standard SID it must not be freed!
       @return Returns true if the SID instance is a instance from JwsclKnownSid.
      }
    function IsStandardSID: boolean; virtual;
  public
       {@Name copies the given SID instance into a newly created instance.
        The given SID must be a correct SID.
        The resulted SID will be checked by @link(CheckSID).

        @param(SecurityID receives a SID instance to be copied)
        @raises(EJwsclSecurityException See @link(CheckSID) for exceptions description) }
    constructor Create(const SecurityID: TJwSecurityId); overload;

       {@Name copies the given SID structure into a newly created instance.
        The resulted SID will be checked by @link(CheckSID).

        @param(SID receives a SID structure to be copied)
        @raises(EJwsclWinCallFailedException will be raised if a call to CopySID failed)
        @raises(EJwsclSecurityException See @link(CheckSID) for exceptions description) }

    constructor Create(const SID: PSID); overload;

       {@Name copies the given TSidAndAttributes structure into a newly created instance.
        The component attributes will be ignored.
        The resulted SID will be checked by @link(CheckSID).

        @param(SID receives a TSidAndAttributes structure to be copied)
        @raises(EJwsclWinCallFailedException will be raised if a call to CopySID failed)
        @raises(EJwsclSecurityException See @link(CheckSID) for exceptions description) }

    constructor Create(const SID: PSidAndAttributes); overload;

       {@Name copies the given TSidAndAttributes structure into a newly created instance.
        The component attributes will be ignored.
        The resulted SID will be checked by @link(CheckSID).

        @param(SID receives a TSidAndAttributes structure to be copied)

        @raises(EJwsclWinCallFailedException will be raised if a call to AllocateAndInitializeSid failed)
        @raises(EJwsclSecurityException See @link(CheckSID) for exceptions description) }

    constructor Create(const Authorities: TJwSubAuthorityArray;
      Identifier: TSidIdentifierAuthority); overload;

       {@Name copies the given parameters into a newly created instance.
        The resulted SID will be checked by @link(CheckSID).

        This constructor calls Create(const Authorities : TJwSubAuthorityArray; Identifier: TSidIdentifierAuthority);
        @param(Authorities contains an array of authorities values copied into the authority structure of the SID.
         It can be an array of Cardinal values. However only the first 8 values are used.)
        @param(Identifier contains a TSidIdentifierAuthority structure to be copied into the new SID instance.
              Authorities must contain 8 subauthority values.)

        @raises(EJwsclWinCallFailedException will be raised if a call to AllocateAndInitializeSid failed)
        @raises(EJwsclSecurityException See @link(CheckSID) for exceptions description) }

    constructor Create(const Authorities: array of Cardinal;
      Identifier: TSidIdentifierAuthority); overload;

       {@Name creates a SID instance from a known well sid type.
        The resulted SID will be checked by @link(CheckSID).

        @param(WellKnownSidType contains the sid type that is to be created)
        @param(DomainSid contains the domain SID where the new SID is associated. It can be nil to use the local computer.
               The domain SID. This value is required for the following WellKnownSidType values. This parameter is ignored for any other WellKnownSidType values.
               @unorderedlist(
               @item WinAccountAdministratorSid
               @item WinAccountGuestSid
               @item WinAccountKrbtgtSid
               @item WinAccountDomainAdminsSid
               @item WinAccountDomainUsersSid
               @item WinAccountDomainGuestsSid
               @item WinAccountComputersSid
               @item WinAccountControllersSid
               @item WinAccountCertAdminsSid
               @item WinAccountSchemaAdminsSid
               @item WinAccountEnterpriseAdminsSid
               @item WinAccountPolicyAdminsSid
               @item WinAccountRasAndIasServersSid)
               )
        @raises(EJwsclWinCallFailedException will be raised if a call to CreateWellKnownSid failed)
        @raises(EJwsclSecurityException See @link(CheckSID) for exceptions description)
       }
    constructor CreateWellKnownSid(WellKnownSidType: TWellKnownSidType;
      DomainSid: TJwSecurityId = nil); overload;

       {@Name creates a SID instance from a string that represents a SID.
        The resulted SID will be checked by @link(CheckSID).

        @param(BinarySID must be in form "S-1-III-SSS[-SSS]^7".
               Where I = identifier authority and S = subautority (from 1 to 8))
        @raises(EJwsclWinCallFailedException will be raised if a call to ConvertStringSidToSid failed)
        @raises(EJwsclSecurityException See @link(CheckSID) for exceptions description)
       }
    constructor Create(const BinarySID: TJwString); overload;

       {@Name creates a SID instance from a user and domain account.
        The resulted SID will be checked by @link(CheckSID).

        @param(SystemName can be a computer or domain name. Can also be empty so the local computer will be used.)
        @param(AccountName contains the user account name to be converted into a SID)
        @raises(EJwsclWinCallFailedException will be raised if a call to ConvertStringSidToSid failed)
        @raises(EJwsclSecurityException See @link(CheckSID) for exceptions description)
       }
    constructor Create(const SystemName, AccountName: TJwString); overload;


    function GetAttributesType: TJwSidAttributeSet; virtual;
    procedure SetAttributesType(Attributes: TJwSidAttributeSet); virtual;

    function GetAttributeString(
      const arrStrings: TJwSidAttributesStringArray): TJwString; overload; virtual;
    function GetAttributeString(
      const Attributes: TJwSidAttributeSet): TJwString; overload;  virtual;

    {@Name converts a TSidIdentifierAuthority into an integer.
     @return(Returns the converted value. The value is always smaller or equal than
      $FFFFFFFFFFFF ((2^48) -1)) }
    class function SidAuthToInt(const Value : TSidIdentifierAuthority) : Int64;

    {@Name converts a value into a sid identifier structure.
     @param(Value defines a value that is to be converted.)
     @raises(EJwsclInvalidSidAuthorityValue will be raised if the given value
       is greater than $FFFFFFFFFFFF ((2^48) -1))}
    class function IntToSidAuth(const Value : Int64) : TSidIdentifierAuthority;


    {@Name destroys the instance and frees the SID.}
    destructor Destroy; override;
  public
       {@Name contains a pointer to the internal SID structure.
        The SID structure must not be freed by CloseHandle otherwise the behavior of the instance is undefined.
        }
    property SID: PSID Read fSid;

       {@Name returns the count of authority values.
        A EJwsclWinCallFailedException exception will be raised if the count could not be retrieved.}
    property SubAuthorityCount: Cardinal Read GetSidSubAuthorityCount;

       {@Name returns a copy of the sub authority values as a dynamic array.
        EJwsclWinCallFailedException will be raised if the array could not be retrieved because a call
         to  GetSidSubAuthority failed
        }
    property SubAuthorityArray: TJwSubAuthorityArray
      Read GetSidSubAuthorityArray;

       {@Name returns the sub authority value specified by an index.
        The exception EJwsclIndexOutOfBoundsException will be raised if parameter index
        is smaller than 0 or greater equal than SubAuthorityCount.

        EJwsclWinCallFailedException will be raised if the call to GetSidSubAuthority failed.
        }
    property SubAuthority[Index: Cardinal]: Cardinal Read GetSidSubAuthority;

       {@Name returns a copy of the sid identifier authority of the SID.
        You can use SidAuthToInt to convert the array into a 48bit number as a 64bit integer.
       EJwsclWinCallFailedException will be raised if the call to GetSidIdentifierAuthority failed.
        }
    property IdentifierAuthority: TSidIdentifierAuthority
      Read GetSidIdentifierAuthority;

       {@Name returns the length of the SID structure.
       EJwsclWinCallFailedException will be raised if the call to GetLengthSID failed.
       }
    property SIDLength: Cardinal Read GetLengthSID;

       {@Name returns the SID type as a well known sid type.

        The value is the parameter value given in CreateWellKnownSid as parameter WellKnownSidType.
        Otherwise the value is always WinNullSid;
       }
    property WellKnownSidType: TWellKnownSidType Read GetWellKnownSidType;

       {@Name returns whether this instance contains a well known sid (true) or not (false).
        }
    property IsWellKnownSid: boolean Read GetWellKnownSid;

       {@Name returns a string representation of the SID.
         The string has the form :
           "S-1-III-SSS[-SSS]^7"
           Where I = identifier authority and S = subautority (from 1 to 8 values))

        EJwsclWinCallFailedException will be raised if the call to GetStringSID failed.
         }
    property StringSID: TJwString Read GetStringSID;

       {@Name returns the account name of the SID on the computer given in SystemName.
       For more information see the see also section.
       EJwsclWinCallFailedException if the call to a winapi function failed
       @Seealso(GetAccountSidString);
       }
    property AccountName[SystemName: TJwString]: TJwString Read GetAccountName;

    property ChachedUserFromSid : WideString read GetCachedUserFromSid;

       {@Name returns the domain account name of the SID on the computer given in SystemName.
       For more information see the see also section.
       EJwsclWinCallFailedException if the call to a winapi function failed
       @Seealso(GetAccountSidString GetAccountName);
       }
    property AccountDomainName[SystemName: TJwString]: TJwString
      Read GetAccountDomainName;

       {@Name returns the account name use of the SID on the computer given in SystemName.
       For more information see the see also section.
       EJwsclWinCallFailedException if the call to a winapi function failed
       @Seealso(GetAccountSidString);
       }
    property AccountNameUse[SystemName: TJwString]: TSidNameUse
      Read GetAccountNameUse;

       {@Name can be used to set specific attributes to the SID.
        It is also used by TJwSecurityIdList to save attributes from PTokenGroups

        The attributes is also filled by token groups. However changing this
        value has no effect on the token sid. Use instead TJwSecurityToken methods.
       }
    property Attributes: Cardinal Read fAttributes Write fAttributes;

       {@Name is the same as Attributes but using specific attribute types (TJwSidAttributeSet)
        The constant sidaUnknown is flaged in the result if a bit in property
        Attributes could not be converted.
        Setting a value does only support values that are listed in TJwSidAttribute
        but without sidaUnknown and SA_PADx (where x is between 0 and 5).
        Changing the value does also affect property Attributes.
       }
    property AttributesType: TJwSidAttributeSet
      Read GetAttributesType Write SetAttributesType;

    property Trustee: TTrusteeEx Read GetTrustee;
  end;


{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation

{$ENDIF SL_OMIT_SECTIONS}


{$IFNDEF SL_INTERFACE_SECTION}

class function TJwSecurityId.NewSID: PSID;
begin
  //Result := PSID(GlobalAlloc(GMEM_FIXED or GMEM_ZEROINIT,SECURITY_MAX_SID_SIZE));
  GetMem(Result, SECURITY_MAX_SID_SIZE); //GetMem is compatible to FastMM4
  FillChar(Result^, SECURITY_MAX_SID_SIZE, 0);

  if Result = nil then
    raise EJwsclNotEnoughMemory.CreateFmtEx(
      RsSidNotEnoughMemoryPSid, 'NewSID',
      ClassName, RsUNSid, 0, True, []);
end;

class procedure TJwSecurityId.FreeSID(var SID: PSID);
begin
  if SID <> nil then
  begin
    FillChar(SID^, sizeof(SID^), 0);
    //GlobalFree(Cardinal(SID));
    FreeMem(SID);
  end;

  SID := nil;
end;

constructor TJwSecurityId.Create;
begin
  inherited Create;
  fSID := nil;
  fDbgDisableException := False;
  fWellKnownSidType := WinNullSid;
end;

constructor TJwSecurityId.Create(const SID: PSID);
var
  len: integer;
begin
  if SID = nil then
  begin
    //raise Exception.Create('');
    raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter, 'Create(const SID : TSid)',
      ClassName, RsUNSid, 0, False, ['Sid']);
    Exit;
  end;


  Create;

  fSID := NewSID; //getmem
  len  := SECURITY_MAX_SID_SIZE; //sizeof(fSID^);//GlobalSize(Cardinal(fSid));

  if not CopySid(len, fSid, SID) then
    raise EJwsclWinCallFailedException.CreateFmtEx(RsWinCallFailed,
      'Create(const SID : TSid)', ClassName, RsUNSid, 0, True,
      ['CopySid']);

  CheckSID;

  UpdateDbgData;
end;

constructor TJwSecurityId.Create(const SecurityID: TJwSecurityId);
begin
  if SecurityID = nil then
    raise EJwsclNILParameterException.CreateFmtEx(
     RsNilParameter, 'Create(const SecurityID : TJwSecurityId)',
     ClassName, RsUNSid, 0, False, ['SecurityId']);

  Create(SecurityID.SID);

  CheckSID;

  UpdateDbgData;
end;

constructor TJwSecurityId.CreateWellKnownSid(WellKnownSidType:
  TWellKnownSidType;
  DomainSid: TJwSecurityId);
var
  pDomainSid: PSID;
  dwSize:  Cardinal;
  tempSID: PSID;
begin
  Create;

  pDomainSid := nil;
  if Assigned(DomainSid) then
    pDomainSid := DomainSid.SID;

  tempSID := PSID(LocalAlloc(GMEM_FIXED or GMEM_ZEROINIT,
    SECURITY_MAX_SID_SIZE));

  dwSize := SECURITY_MAX_SID_SIZE;
  if not JwaWindows.CreateWellKnownSid(JwaWindows.WELL_KNOWN_SID_TYPE(WellKnownSidType),
    pDomainSid, tempSID, dwSize) then
  begin
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed, 'CreateWellKnownSid',
      ClassName, RsUNSid, 0, True, ['CreateWellKnownSid']);
  end;

  {
   WARNING:
     I changed the memory allocation to GetMem so Memory Leak Finders (like FastMM4) can work.
     However some of the API functions like AllocateAndInitializeSid,ConvertStringSidToSid,LookupAccountName and InitializeSid use other
     memory allocation methods which are truly incompatible.
      We can see the results in the FreeSID method. In that case the memory is freed incorrectly.
  }

  //Copy the SID
  Create(tempSID);

  //free the SID
  LocalFree(HLOCAL(tempSID));

  fWellKnownSidType := WellKnownSidType;

  CheckSID;

  UpdateDbgData;
end;

constructor TJwSecurityId.Create(const SID: PSidAndAttributes);
begin
  if SID = nil then
  begin
    raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter, 'Create(const SID : PSidAndAttributes)',
      ClassName, RsUNSid, 0, False, ['Sid']);
    Exit;
  end
  else
    Create(SID.Sid);
end;

constructor TJwSecurityId.Create(const Authorities: array of Cardinal;
  Identifier: TSidIdentifierAuthority);
(*var A : TJwSubAuthorityArray;
    i : Integer;
    aIdentifier: PSidIdentifierAuthority;
begin
  Create;
  SetLength(A,Length(Authorities));

  for i := low(Authorities) to high(Authorities) do
  begin
    A[i] := Authorities[i];
  end;

  GetMem(aIdentifier, sizeof(TSidIdentifierAuthority));

  aIdentifier^ := Identifier;

  Create(TJwSubAuthorityArray(A),aIdentifier);

  FreeMem(aIdentifier);

  UpdateDbgData;
end;   *)

var
  i: integer;

begin
  Create;

  if Length(Authorities) <> 8 then
    raise EJwsclIndexOutOfBoundsException.CreateFmtEx(
      RsSidInvalidAuthoritiesLength,
      'Create', ClassName, RsUNSid, 0, False, []);

  fSID := NewSID;

  fSID.Revision := 1;
  fSID.IdentifierAuthority := Identifier;
  fSID.SubAuthorityCount := length(Authorities);

  for i := low(Authorities) to high(Authorities) do
    fSID.SubAuthority[i] := Authorities[i];

  {
   WARNING:
     I changed the memory allocation to GetMem so Memory Leak Finders (like FastMM4) can work.
     However some of the API functions like AllocateAndInitializeSid and InitializeSid use other
     memory allocation methods which are truly incompatible.
      We can see the results in the FreeSID method. In that case the memory is freed incorrectly.

   if not AllocateAndInitializeSid(aIdentifier,length(Authorities),Authorities[0],Authorities[1],Authorities[2],Authorities[3],Authorities[4],Authorities[5],Authorities[6],Authorities[7],fSID) then
    raise EJwsclWinCallFailedException.CreateFmtEx('Call to AllocateAndInitializeSid.', 'Create',ClassName,RsUNSid, 0,true,[]);
  }

  CheckSID;

  UpdateDbgData;
end;

constructor TJwSecurityId.Create(const Authorities: TJwSubAuthorityArray;
  Identifier: TSidIdentifierAuthority);
var
  i: integer;

begin
  Create;

  if Length(Authorities) <> 8 then
    raise EJwsclIndexOutOfBoundsException.CreateFmtEx(
      RsSidInvalidAuthoritiesLength,
      'Create', ClassName, RsUNSid, 0, False, []);

  fSID := NewSID;

  fSID.Revision := 1;
  fSID.IdentifierAuthority := Identifier;
  fSID.SubAuthorityCount := length(Authorities);

  for i := low(Authorities) to high(Authorities) do
    fSID.SubAuthority[i] := Authorities[i];

  {
    WARNING:
     I changed the memory allocation to GetMem so Memory Leak Finders (like FastMM4) can work.
     However some of the API functions like AllocateAndInitializeSid and InitializeSid use other
     memory allocation methods which are truly incompatible.
      We can see the results in the FreeSID method. In that case the memory is freed incorrectly.

  if not AllocateAndInitializeSid(Identifier,length(A),A[0],A[1],A[2],A[3],A[4],A[5],A[6],A[7],fSID) then
    raise EJwsclWinCallFailedException.CreateFmtEx('Call to AllocateAndInitializeSid.', 'Create',ClassName,RsUNSid, 0,true,[]);

  }

  CheckSID;

  UpdateDbgData;
end;

constructor TJwSecurityId.Create(const BinarySID: TJwString);
var
  c: TJwPChar;
  tempSID: PSID;
begin
  c := TJwPChar(BinarySID);

  tempSID := nil;
  if not
    {$IFDEF UNICODE}ConvertStringSidToSidW{$ELSE}
    ConvertStringSidToSidA
{$ENDIF}
    (c, jwaWindows_PSID(tempSID)) then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsSidCallFailedCreateBinarySid,
      'Create(const BinarySID : TJwString)', ClassName, RsUNSid, 0, True,
      [string(BinarySID)]);
         
  {
   WARNING:
     I changed the memory allocation to GetMem so Memory Leak Finders (like FastMM4) can work.
     However some of the API functions like AllocateAndInitializeSid,ConvertStringSidToSid and InitializeSid use other
     memory allocation methods which are truly incompatible.
      We can see the results in the FreeSID method. In that case the memory is freed incorrectly.
  }

  //Copy the SID
  Self.Create(tempSID);

  //free the SID
  LocalFree(HLOCAL(tempSID));

  UpdateDbgData;
end;

constructor TJwSecurityId.Create(const SystemName, AccountName: TJwString);
var
  pDomainName: TJwPChar;
  iSidSize, iDomainName: Cardinal;

  SidNameUse: TSidNameUse;
  tempSID: PSID;
begin

  SidNameUse := SidTypeInvalid;

  iSidSize := SECURITY_MAX_SID_SIZE;
  iDomainName := 0;

  if not JwaWindows.
    {$IFDEF UNICODE}LookupAccountNameW{$ELSE}
    LookupAccountNameA
{$ENDIF}
    (TJwPChar(SystemName), TJwPChar(AccountName), nil, iSidSize,
    nil, iDomainName, SidNameUse) and (GetLastError =
    ERROR_INSUFFICIENT_BUFFER) then
  begin

    tempSID := PSID(LocalAlloc(GMEM_FIXED or GMEM_ZEROINIT,
      SECURITY_MAX_SID_SIZE));

    pDomainName := TJwPChar(LocalAlloc(GMEM_FIXED or GMEM_ZEROINIT,
      iDomainName * sizeof(TJwChar)));
    //fSID := NewSID;

    iSidSize := SECURITY_MAX_SID_SIZE;
    if not JwaWindows.
      {$IFDEF UNICODE}LookupAccountNameW{$ELSE}
      LookupAccountNameA
{$ENDIF}
      (TJwPChar(SystemName), TJwPChar(AccountName), tempSID, iSidSize,
      TJwPChar(pDomainName), iDomainName, SidNameUse) then
    begin
      LocalFree(Cardinal(pDomainName));
      raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed,
        'Create(const SystemName, AccountName : TJwString);', ClassName,
        RsUNSid, 0, True, ['LookupAccountName']);
    end;

  {
   WARNING:
     I changed the memory allocation to GetMem so Memory Leak Finders (like FastMM4) can work.
     However some of the API functions like AllocateAndInitializeSid,ConvertStringSidToSid,LookupAccountName and InitializeSid use other
     memory allocation methods which are truly incompatible.
      We can see the results in the FreeSID method. In that case the memory is freed incorrectly.
  }

    //Copy the SID
    Create(tempSID);

    //free the SID
    LocalFree(HLOCAL(tempSID));

    LocalFree(Cardinal(pDomainName));
  end
  else
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsSidCallLookupAccountNameFailed,
      'Create(const SystemName, AccountName : TJwString);', ClassName,
      RsUNSid, 0, True, []);

  UpdateDbgData;
end;

destructor TJwSecurityId.Destroy;
begin
  FreeSID(fSID);

  inherited Destroy;
end;

function TJwSecurityId.GetStringSID: TJwString;
var
  sSid: TJwPChar;
begin
  if not
    {$IFDEF UNICODE}ConvertSidToStringSidW{$ELSE}
    ConvertSidToStringSidA
{$ENDIF}
    (jwaWindows_PSID(fSID), TJwPChar(sSid)) then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed, 'Create(const BinarySID : TJwString)',
      ClassName, RsUNSid, 0, True, ['ConvertStringSidToSid']);

  Result := TJwString(sSid);

  LocalFree(Cardinal(sSid));
end;

function TJwSecurityId.GetTrustee: TTrusteeEx;
begin
  {$IFDEF UNICODE}
  BuildTrusteeWithSidW
    {$ELSE}
  BuildTrusteeWithSidA
    {$ENDIF}
  (@Result, SID);

end;

class function TJwSecurityId.GetWindowsAccountDomainSid(aSID: TJwSecurityId):
TJwSecurityId;
var
  pDomainSID: PSID;
  dwSize: Cardinal;
begin
  if not Assigned(aSID) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter, 'GetWindowsAccountDomainSid',
      ClassName, RsUNSid, 0, False, ['Sid']);

  aSID.CheckSID;

  pDomainSID := NewSID;

  dwSize := SECURITY_MAX_SID_SIZE;

  if not JwaWindows.GetWindowsAccountDomainSid(aSID.SID,
    pDomainSID, dwSize) then
  begin
    FreeSID(pDomainSID);
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed, 'GetWindowsAccountDomainSid',
      ClassName, RsUNSid, 0, True, ['GetWindowsAccountDomainSid']);
  end;

  Result := TJwSecurityId.Create(pDomainSID);
  FreeSID(pDomainSID);

  Result.CheckSID;
end;

function TJwSecurityId.GetWindowsAccountDomainSid: TJwSecurityId;
begin
  Result := GetWindowsAccountDomainSid(Self);
end;

function TJwSecurityId.EqualPrefixSid(pSid1: TJwSecurityId): boolean;
begin
  if not Assigned(pSid1) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter, 'EqualPrefixSid', ClassName,
      RsUNSid, 0, False, ['Sid']);

  pSid1.CheckSID;

  Result := JwaWindows.EqualPrefixSid(Self.SID, pSid1.SID);
  if GetLastError() <> 0 then
  begin
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed, 'EqualPrefixSid', ClassName,
      RsUNSid, 0, True, ['EqualPrefixSid']);
  end;
end;

function TJwSecurityId.EqualSid(pSid1: TJwSecurityId): boolean;
begin
  if not Assigned(pSid1) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter, 'EqualSid', ClassName,
      RsUNSid, 0, False, ['Sid']);

  pSid1.CheckSID;

  Result := JwaWindows.EqualSid(Self.SID, pSid1.SID);
  if GetLastError() <> 0 then
  begin
    raise EJwsclWinCallFailedException.CreateFmtEx(RsWinCallFailed,
      'EqualSid', ClassName, RsUNSid, 0, True, ['EqualSid']);
  end;
end;

function TJwSecurityId.EqualDomainSid(pSid1: TJwSecurityId): boolean;
var
  r: boolean;
begin
  if not Assigned(pSid1) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter, 'EqualDomainSid', ClassName,
      RsUNSid, 0, False, ['Sid']);

  pSid1.CheckSID;

  r := JwaWindows.EqualDomainSid(Self.SID, pSid1.SID, @Result);
  if not r then
  begin
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed, 'EqualDomainSid', ClassName,
      RsUNSid, 0, True, ['EqualDomainSid']);
  end;
end;

function TJwSecurityId.GetAccountSidString(const SystemName: TJwString;
  out DomainName: TJwString; out SidNameUse: TSidNameUse): TJwString;
var
  pSIDName, pDomainName: TJwPChar;
  iSIDName, iDomainName: Cardinal;
begin
  CheckSID;

  DomainName := '';
  SidNameUse := SidTypeInvalid;

  iDomainName := 0;
  iSIDName := 0;

  if not JwaWindows.
    {$IFDEF UNICODE}LookupAccountSidW{$ELSE}
    LookupAccountSidA
{$ENDIF}
    (TJwPChar(SystemName), Self.SID, nil, iSIDName, nil, iDomainName,
    SidNameUse) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
  begin
    pSIDName := TJwPChar(LocalAlloc(GMEM_FIXED or GMEM_ZEROINIT,
      iSIDName * sizeof(TJwChar)));
    pDomainName := TJwPChar(LocalAlloc(GMEM_FIXED or GMEM_ZEROINIT,
      iDomainName * sizeof(TJwChar)));

    if not JwaWindows.
      {$IFDEF UNICODE}LookupAccountSidW{$ELSE}
      LookupAccountSidA
{$ENDIF}
      (TJwPChar(SystemName), Self.SID, TJwPChar(pSIDName), iSIDName,
      TJwPChar(pDomainName), iDomainName, SidNameUse) then
    begin
      LocalFree(Cardinal(pSIDName));
      LocalFree(Cardinal(pDomainName));

      if fDbgDisableException then
        Exit;

      raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed, 'GetAccountSidString',
        ClassName, RsUNSid, 0, True, ['LookupAccountSid']);
    end;

    DomainName := TJwString(pDomainName);
    Result := TJwString(pSIDName);

    LocalFree(Cardinal(pSIDName));
    LocalFree(Cardinal(pDomainName));
  end
  else
  if not fDbgDisableException then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsSidCallLookupAccountSidFailed,
      'GetAccountSidString', ClassName, RsUNSid, 0, True, []);

end;

function TJwSecurityId.GetCachedUserFromSid : WideString;
var pwUserName : PWideChar;
    cbUserName : DWORD;
begin
  CheckSID;

  //Buffer overlow issue fixed
  cbUserName := UNLen;
  GetMem(pwUserName, cbUserName * sizeOf(WideChar));
  CachedGetUserFromSid(fSID, pwUserName, cbUserName);

  Result := pwUserName;

  FreeMem(pwUserName);
end;

function TJwSecurityId.GetAccountName(SystemName: TJwString): TJwString;
var
  DomainName: TJwString;
  SidNameUse: TSidNameUse;
begin
  Result := GetAccountSidString(SystemName, DomainName, SidNameUse);
end;

function TJwSecurityId.GetAccountDomainName(SystemName: TJwString): TJwString;
var
  SidNameUse: TSidNameUse;
begin
  GetAccountSidString(SystemName, Result, SidNameUse);
end;

function TJwSecurityId.GetAccountNameUse(SystemName: TJwString): TSidNameUse;
var
  DomainName: TJwString;

begin
  GetAccountSidString(SystemName, DomainName, Result);
end;

class function TJwSecurityId.CreateSidIdentifierAuthority(
  Value1, Value2, Value3, Value4, Value5, Value6: byte):
TSidIdentifierAuthority;
begin
  Result.Value[0] := Value1;
  Result.Value[1] := Value2;
  Result.Value[2] := Value3;
  Result.Value[3] := Value4;
  Result.Value[4] := Value5;
  Result.Value[5] := Value6;
end;

function TJwSecurityId.IsStandardSID: boolean;
begin
  Result := False;
end;





function TJwSecurityId.GetText(ignoreExceptions: boolean = False): TJwString;
var
  sDomain, sName, sSID: TJwString;
begin
  fDbgDisableException := ignoreExceptions;
  try
    sDomain := AccountDomainName[''];
  except
    on E: EJwsclSecurityException do
      sDomain := JwFormatString(RsSidUnknownDomain,[E.Message]);
  end;

  if Length(sDomain) > 0 then
    sDomain := sDomain + '@';

  try
    sName := AccountName[''];
  except
    on E: EJwsclSecurityException do
      sName := JwFormatString(RsSidUnknownName,[E.Message]);
  end;

  try
    sSID := StringSID;
  except
    on E: EJwsclSecurityException do
      sSID := JwFormatString(RsSidUnknownSid,[E.Message]);
  end;


  Result := JwFormatString(RsSidTextString,[sDomain,sName,sSid,GetAttributeString(AttributesType)]);
  //Result := sDomain + sName + '  (' + sSID + ')';

  fDbgDisableException := False;
end;

function TJwSecurityId.GetSidSubAuthorityCount: Cardinal;
var
  p: PUCHAR;
begin
  CheckSID;

  SetLastError(0);
  p := JwaWindows.GetSidSubAuthorityCount(fSID);
  if GetLastError <> 0 then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed, 'GetSidSubAuthority',
      ClassName, RsUNSid, 0, True, ['GetSidSubAuthorityCount']);

  Result := p^;
end;

function TJwSecurityId.GetSidIdentifierAuthority: TSidIdentifierAuthority;
var
  p: PSidIdentifierAuthority;
begin
  CheckSID;

  SetLastError(0);
  p := JwaWindows.GetSidIdentifierAuthority(fSID);

  if GetLastError <> 0 then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed, 'GetSidIdentifierAuthority',
      ClassName, RsUNSid, 0, True, ['GetSidIdentifierAuthority']);

  Result := p^;
end;


class function TJwSecurityId.IntToSidAuth(const Value : Int64) : TSidIdentifierAuthority;
var i : Byte;
    Bits : Cardinal;
begin
  ZeroMemory(@result.Value, sizeof(result.Value));

  if Value > $FFFFFFFFFFFF then //> 2^48
    raise EJwsclInvalidSidAuthorityValue.CreateFmtEx(
      RsInvalidSidAuthorityValue, 'IntToSidAuth',
      ClassName, RsUNSid, 0, false, []);

  Bits := Length(result.Value) shl 3;
 {
  result.Value[5] := (Value shl 40) shr 40;
  result.Value[4] := (Value shl 32) shr 40;
  result.Value[3] := (Value shl 24) shr 40;
  result.Value[2] := (Value shl 16) shr 40;
  result.Value[1] := (Value shl 8) shr 40;
  result.Value[0] := (Value shl 0) shr 40;
  ===
 }
  for i := low(result.Value) to high(result.Value) do
    result.Value[high(result.Value)-i] := (Value shl (Bits-((i+1) shl 3 ))) shr (Bits - 8);

end;
class function TJwSecurityId.SidAuthToInt(const Value : TSidIdentifierAuthority) : Int64;
var i : Byte;
begin
  result := 0;

  for i := low(Value.Value) to high(Value.Value) do
    result := (result shl 8) or Value.Value[i];
end;

function TJwSecurityId.GetLengthSID: Cardinal;

begin
  CheckSID;

  Result := JwaWindows.GetLengthSID(fSID);
  if GetLastError() <> 0 then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed, 'GetGetLengthSid', ClassName,
      RsUNSid, 0, True, ['GetLengthSID']);
end;

procedure TJwSecurityId.CheckSID;
begin
  if fSID = nil then
    raise EJwsclInvalidSIDException.CreateFmtEx('Invalid SID.',
      'GetSidSubAuthority', ClassName, RsUNSid, 0, False, []);

  if not IsValidSid(PSID(fSID)) then
    raise EJwsclInvalidSIDException.CreateFmtEx('Invalid SID.',
      'GetSidSubAuthority', ClassName, RsUNSid, 0, False, []);
end;

function TJwSecurityId.GetSidSubAuthorityArray: TJwSubAuthorityArray;
var
  i, c: integer;
begin
  CheckSID;

  c := SubAuthorityCount;
  SetLength(Result, c);
  for i := 0 to c - 1 do
  begin
    try
      SetLastError(0);
      Result[i] := JwaWindows.GetSidSubAuthority(fSID, i)^;
    finally
      if GetLastError() <> 0 then
        raise EJwsclWinCallFailedException.CreateFmtEx(
          RsWinCallFailed, 'GetSidSubAuthorityArray',
          ClassName, RsUNSid, 0, False, ['GetSidSubAuthority']);
    end;
  end;
end;

function TJwSecurityId.GetSidSubAuthority(Index: Cardinal): Cardinal;
begin
  CheckSID;

  if (Index >= SubAuthorityCount) then
    raise EJwsclIndexOutOfBoundsException.CreateFmtEx(
      RsSidSubAuthorityOutOfBound, 'GetSidSubAuthority',
      ClassName, RsUNSid, 0, False, [Index]);

  try
    SetLastError(0);
    Result := JwaWindows.GetSidSubAuthority(fSID, Index)^;
  finally
    if GetLastError() <> 0 then
      raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed, 'GetSidSubAuthorityArray',
        ClassName, RsUNSid, 0, False, ['GetSidSubAuthority']);
  end;
end;

function TJwSecurityId.GetWellKnownSidType: TWellKnownSidType;
begin
  Result := fWellKnownSidType;
end;

function TJwSecurityId.GetWellKnownSid: boolean;
begin
  Result := JwaWindows.IsWellKnownSid(fSID, JwaWindows.TWellKnownSidType(GetWellKnownSidType));
end;

function TJwSecurityId.CreateCopyOfSID: PSID;
var
  i: integer;
  c: Cardinal;
begin
  Result := NewSID;
  Result.Revision := SID_REVISION;

  c := SubAuthorityCount;
  Result.SubAuthorityCount := c;
  Result.IdentifierAuthority := IdentifierAuthority;

  for i := 0 to SubAuthorityCount - 1 do
    Result.SubAuthority[i] := SubAuthorityArray[i];
end;

function TJwSecurityId.Create_PSID_AND_ATTRIBUTES(attributes: Cardinal = 0):
PSID_AND_ATTRIBUTES;
begin
  //Result := PSID_AND_ATTRIBUTES(GlobalAlloc(GMEM_FIXED or GMEM_ZEROINIT,SECURITY_MAX_SID_SIZE));
  GetMem(Result, SECURITY_MAX_SID_SIZE); //Getmem is compatible to FastMM4
  FillChar(Result^, SECURITY_MAX_SID_SIZE, 0);

  if Result = nil then
    raise EJwsclNotEnoughMemory.CreateFmtEx(
      RsSidAndAttributesMemoryAllocationFailed,
      'Create_PSID_AND_ATTRIBUTES', ClassName, RsUNSid, 0, True, []);

  if Attributes = 0 then
    Result.Attributes := Self.Attributes
  else
    Result.Attributes := attributes;
  Result.Sid := CreateCopyOfSID;
end;

procedure TJwSecurityId.Free_PSID_AND_ATTRIBUTES(var sids: PSID_AND_ATTRIBUTES);
begin
  if sids = nil then
    Exit;

  FreeSID(Sids.Sid);

  //GlobalFree(Cardinal(sids));
  FillChar(sids^, sizeof(sids^), 0);
  FreeMem(sids);

  sids := nil;
end;

procedure TJwSecurityId.UpdateDbgData;
var
  Data: array[1..10] of string;
  i: integer;
  ident: TSidIdentifierAuthority;
begin

  fDbgDisableException := True;


  Data[1] := JwFormatString(RsSidSubAuthCountText,[SubAuthorityCount]);

  Data[2] := RsSidSubAuthorityLabel;

  try
    for i := 0 to SubAuthorityCount - 1 do
    begin
      Data[2] := Data[2] + '-' + IntToStr(SubAuthority[i]);
    end;
  except
    Data[2] := Data[2] + '???';
  end;
  System.Delete(Data[2], 1, 1);

  try
    ident := IdentifierAuthority;
  except
    FillChar(ident, sizeof(ident), 0);
  end;
  Data[3] := RsSidIdentifierAuthorityLabel + char(ident.Value[0]) +
    '-' + char(ident.Value[1]) + '-' + char(ident.Value[2]) +
    '-' + char(ident.Value[3]) + '-' + char(ident.Value[4]) +
    '-' + char(ident.Value[5]);

  Data[4] := RsSidSIDLengthLabel + IntToStr(SIDLength);


  Data[5] := RsSidStringSIDLabel + StringSID;

  Data[6] := RsSidAccountNameLabel;
  try
    Data[6] := Data[6] + AccountName[''];
  except
    on E : ESetSecurityException do
      Data[6] := Data[6] + '???';
  end;

  Data[7] := RsSidAccountDomainNameLabel;
  try
    Data[7] :=  Data[7] + AccountDomainName[''];
  except
    on E : ESetSecurityException do
      Data[7] := Data[7] + '???';
  end;

  Data[8] := RsAccountNameUseLabel;
  try
    Data[8] := Data[8] + IntToStr(AccountNameUse['']);
  except
    Data[8] := Data[8] + '???';
  end;

  fDbgDisableException := False;

  fDbgData := '';
  for i := low(Data) to high(Data) do
  begin
    fDbgData := fDbgData + Data[i] + #13#10;
  end;

end;

{*********** TJwSecurityIdList *************}

constructor TJwSecurityIdList.Create(ownSIDs: boolean;
  token_groups: PTOKEN_GROUPS);
var
  i: integer;
  aSID: TJwSecurityId;
begin
  inherited Create(ownSIDs);

  if token_groups = nil then
    Exit;

  for i := 0 to token_groups.GroupCount - 1 do
  begin
    aSID := TJwSecurityId.Create(token_groups.Groups[i].Sid);
    aSID.Attributes := token_groups.Groups[i].Attributes;
    Add(aSID);
  end;
end;

constructor TJwSecurityIdList.Create(ownSIDs: boolean);
begin
  inherited Create(ownSIDs);
end;


constructor TJwSecurityIdList.Create(
  SidAndAttributesArray: PSidAndAttributesArray);
var
  i: integer;
  aSID: TJwSecurityId;
begin
  inherited Create(true);

  if SidAndAttributesArray = nil then
    Exit;

  for i := 0 to Length(SidAndAttributesArray^) - 1 do
  begin
    aSID := TJwSecurityId.Create(SidAndAttributesArray^[i].Sid);
    aSID.Attributes := SidAndAttributesArray^[i].Attributes;
    Add(aSID);
  end;
end;


function TJwSecurityIdList.Create_PSID_Array(): PSidAndAttributesArray;
var
  i: integer;
begin
  //Result := PSidAndAttributesArray(GlobalAlloc(GMEM_FIXED or GMEM_ZEROINIT,sizeof(TSidAndAttributes) * Count));
  GetMem(Result, sizeof(TSidAndAttributes) * Count);
  //Getmem is compatible to FastMM4
  //FillChar(Result^,sizeof(TSidAndAttributes) * Count,0);

  for i := 0 to Count - 1 do
  begin
    Result[i].Sid := Self.Items[i].CreateCopyOfSID;
    Result[i].Attributes := Self.Items[i].Attributes;
  end;
end;

function TJwSecurityIdList.Create_PTOKEN_GROUPS(): PTOKEN_GROUPS;
var
  i: integer;
begin
  GetMem(Result, sizeof(Cardinal) + sizeof(TSidAndAttributes) * Count);
  //Getmem is compatible to FastMM4
  //FillChar(Result^,sizeof(TSidAndAttributes) * Count,0);

  Result.GroupCount := Count;
  for i := 0 to Count - 1 do
  begin
    Result.Groups[i].Sid := Self.Items[i].CreateCopyOfSID;
    Result.Groups[i].Attributes := Self.Items[i].Attributes;
  end;
end;

class procedure TJwSecurityIdList.Free_PTOKEN_GROUPS(var sids: PTOKEN_GROUPS);
var
  i: integer;
begin
  if Sids = nil then
    Exit;

  for i := 0 to sids.GroupCount - 1 do
  begin
    TJwSecurityId.FreeSID(sids.Groups[i].Sid);
  end;

  FreeMem(sids);
  Sids := nil;
end;

class procedure TJwSecurityIdList.Free_PSID_Array(var sids: PSidAndAttributesArray);
var
  i: integer;
begin
  if Sids = nil then
    Exit;
  for i := 0 to High(sids^) do
  begin
    TJwSecurityId.FreeSID(sids[i].Sid);
  end;

  //GlobalFree(Cardinal(Sids));
  //FillChar(sids^,sizeof(sids^),0);
  FreeMem(sids);
  Sids := nil;
end;

function TJwSecurityIdList.GetText(ignoreExceptions: boolean = False): TJwString;
var
  i: integer;
begin
  {Result := 'Class : ' + ClassName + #13#10;
  Result := 'Count ' + IntToStr(Count);}
  result := JwFormatString(RsSidListGetText, [ClassName, Count]);
  for i := 0 to Count - 1 do
  begin
    Result := Result + #13#10 + '#' + IntToStr(i) + #13#10;
    Result := Result + Items[i].GetText(ignoreExceptions);
  end;
  Result := Result + #13#10;
end;

function TJwSecurityIdList.GetItem(idx: integer): TJwSecurityId;
begin
  Result := inherited Get(idx);
end;

function TJwSecurityIdList.Add(AObject: TJwSecurityId): integer;
begin
  if IndexOf(AObject) >= 0 then
    raise EJwsclDuplicateListEntryException.CreateFmtEx(RsSidAlreadyInList,
      'Add', ClassName, RsUNSid, 0, True, []);

  Result := inherited Add(AObject);
end;

function TJwSecurityIdList.First: TJwSecurityId;
begin
  Result := TJwSecurityId(inherited First);
end;

function TJwSecurityIdList.IndexOf(AObject: TJwSecurityId): integer;
begin
  Result := inherited IndexOf(AObject);
end;

function TJwSecurityIdList.FindSid(aSID: TJwSecurityId;
  startpos: integer = -1; usePrefix: boolean = False): integer;
var
  i: integer;
  b: boolean;
begin
  Result := -1;
  if not Assigned(aSID) then
    Exit;

  Inc(StartPos);
  for i := startPos to Count - 1 do
  begin
    if usePrefix then
      b := aSID.EqualPrefixSid(Items[i])
    else
      b := aSID.EqualSid(Items[i]);
    if b then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure TJwSecurityIdList.Insert(Index: integer; AObject: TJwSecurityId);
begin
  if IndexOf(AObject) >= 0 then
    raise EJwsclDuplicateListEntryException.CreateFmtEx(RsSidAlreadyInList,
      'Insert', ClassName, RsUNSid, 0, True, []);

  inherited Insert(Index, AObject);
end;

function TJwSecurityIdList.Last: TJwSecurityId;
begin
  Result := TJwSecurityId(inherited Last);
end;

function TJwSecurityIdList.Remove(AObject: TJwSecurityId): integer;
begin
  Result := inherited Remove(AObject);
end;

function TJwSecurityId.GetAttributesType: TJwSidAttributeSet;
begin
  Result := TJwEnumMap.ConvertAttributes(fAttributes);
  if (TJwEnumMap.ConvertAttributes(Result) <> fAttributes) then
    Include(Result, sidaUnknown);
end;

procedure TJwSecurityId.SetAttributesType(Attributes: TJwSidAttributeSet);
begin
  fAttributes := TJwEnumMap.ConvertAttributes(Attributes);
end;

function TJwSecurityId.GetAttributeString(
      const Attributes: TJwSidAttributeSet): TJwString;
var
  i: TJwSidAttribute;
begin
  Result := '';
  for i := low(TJwSidAttribute) to high(TJwSidAttribute) do
  begin
    if i in Attributes then
      Result := ', ' + JwSidAttributeStrings[i];
  end;
  System.Delete(Result, 1, 2);
end;


function TJwSecurityId.GetAttributeString(
  const arrStrings: TJwSidAttributesStringArray): TJwString;
var
  i: TJwSidAttribute;
  attr: TJwSidAttributeSet;
begin
  Result := '';
  attr := AttributesType;
  for i := low(TJwSidAttribute) to high(TJwSidAttribute) do
  begin
    if i in attr then
      Result := ', ' + arrStrings[i];
  end;
  System.Delete(Result, 1, 2);
end;


{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}


end.
{$ENDIF SL_OMIT_SECTIONS}(*
 BOOL IsUserAdmin(VOID)
/*++
Routine Description: This routine returns TRUE if the caller's
process is a member of the Administrators local group. Caller is NOT
expected to be impersonating anyone and is expected to be able to
open its own process and process token.
Arguments: None.
Return Value:
   TRUE - Caller has Administrators local group.
   FALSE - Caller does not have Administrators local group. --
*/
{
BOOL b;
SID_IDENTIFIER_AUTHORITY NtAuthority = SECURITY_NT_AUTHORITY;
PSID AdministratorsGroup;
b = AllocateAndInitializeSid(
    &NtAuthority,
    2,
    SECURITY_BUILTIN_DOMAIN_RID,
    DOMAIN_ALIAS_RID_ADMINS,
    0, 0, 0, 0, 0, 0,
    &AdministratorsGroup);
if(b)
{
    if (!CheckTokenMembership( NULL, AdministratorsGroup, &b))
    {
         b = FALSE;
    }
    FreeSid(AdministratorsGroup);
}

return(b);
}

Access check
http://msdn2.microsoft.com/en-us/library/aa379648.aspx

Determine Whether the File System Supports ACLs
You can use the following code to determine whether a given file system supports ACLs. All you need to do is change the szVol variable to point to the volume.

#include <stdio.h>
#include <windows.h>
void main() {
    char *szVol = "c:\\";
    DWORD dwFlags = 0;

    if (GetVolumeInformation(szVol,
                             NULL,
                             0,
                             NULL,
                             NULL,
                             &dwFlags,
                             NULL,
                             0)) {
        printf("Volume %s does%s support ACLs.",
               szVol,
               (dwFlags & FS_PERSISTENT_ACLS) ? "" : " not");
    } else {
        printf("Error %d",GetLastError());
    }
}

*)