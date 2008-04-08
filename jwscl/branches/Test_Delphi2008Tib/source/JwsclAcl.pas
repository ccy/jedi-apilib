{@abstract(Contains access control classes that are used by the units of JWSCL)
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

The Original Code is JwsclAcl.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.



Description:

Unsupported structures :
 SYSTEM_ALARM_ACE
 SYSTEM_ALARM_CALLBACK_ACE
 SYSTEM_ALARM_CALLBACK_OBJECT_ACE
 SYSTEM_ALARM_OBJECT_ACE




}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclAcl;
{$INCLUDE Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $


interface

uses
  SysUtils, Contnrs, Classes,
{DEBUG}
  Dialogs,
{DEBUG}
  jwaWindows, JwaVista,
  JwsclResource, JwsclUtils,

  JwsclTypes, JwsclExceptions, JwsclMapping,
  JwsclVersion, JwsclConstants, JwsclSid,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
type
  TJwSecurityAccessControlEntry = class;
  TJwAuditAccessControlEntry    = class;
  TJwSystemMandatoryAccessControlEntry = class;

  TJwSecurityAccessControlEntryClass = class of TJwSecurityAccessControlEntry;

  {@Name provides methods for an access control list.
   Do not make instances of this class. Instead use

    TJwSAccessControlList for audit control lists
    TJwDAccessControlList for Discretionary control lists

  }
  TJwSecurityAccessControlList = class(TObjectList)
  protected
    mDoNoRaiseException: boolean;
    fRevision : Cardinal;
    function GetItem(idx: integer): TJwSecurityAccessControlEntry;
    procedure RemoveOwners;
  protected


    {@Name creates an empty access control list.
     @param(ownACEs receives whether the items shall be freed on destruction (True) or not.)}
    constructor Create(OwnAceEntries: boolean); overload;

    {@Name creates a new access control list from an existing access control list.
     All entries a copied and the list owns these objects (entry.ListOwner is set to this instance).

     @raises(EJwsclWinCallFailedException will be raised if the aAPCL is not a valid access control list)
     @raises(EJwsclWinCallFailedException if an win api function failed)

     }
    constructor Create(AclPointerList: PACL); overload;

    {@Name creates a new access control list from an explicit access array.

     @raises(EJwsclNILParameterException will be raised if parameter Accesses is nil)
     @raises(EJwsclWinCallFailedException will be raised if the call to SetEntriesInAcl failed.)
     }
    constructor Create(const Accesses : TJwExplicitAccessArray); overload;

    function GetText: TJwString; virtual;
    procedure SetRevision(const Revision : Cardinal); virtual;


  public
    {@Name creates an empty access control list. The list items will not be owned.}
    constructor Create; overload;

    {@Name destroys the list and all it items.}
    destructor Destroy; override;

    {@Name returns the ACL content in a new string. Additionally
     the access rights are mapped into string using a defined mapping.}
    function GetTextMap(const Mapping: TJwSecurityGenericMappingClass = nil):
      TJwString; virtual;

    {@Name creates a new access control list for using in winapi functions.
     The created memory block must be freed by Free_PACL.
     The list order in the new ACL will be the same like the list in @link(Items).

     @raises(EJwsclNotEnoughMemory will be raised if the new memory block for ACL could not be allocated.)
     @raises(EJwsclUnsupportedACE will be raised if a given ACE in the list is not supported by this function.)
     @raises(EJwsclFailedAddACE will be raised if a AddXXX winapi call for a given ACE in the list failed. )
    }
    function Create_PACL: PACL;

    {@Name frees an access control list created by Create_PACL.

     @param(anACL receives and retrieves the access control list. It will be set to nil afterwards. If anACL is nil nothing happens.)
     }
    class procedure Free_PACL(var AclPointerList: PACL);

    {@Name clears all ACEs in the instance and adds new instances of then ACEs
     from AclInstance. All ACE SIDs are copied.
     If an exception is raised the old ACEs are removed but the newly added ACEs are preserved.
     @param(AclInstance receives the list.)
     @raises(EJwsclNILParameterException will be raised if AclInstance is nil)
    }
    procedure Assign(AclInstance: TJwSecurityAccessControlList); virtual;

    {@Name removes all ACEs from the list and frees them if ownObject is True.
    }
    procedure Clear; override;

    {@Name creates an array of explicit access structure that represents
     the ACEs.

     @raises EJwsclWinCallFailedException will be raised if the call to GetExplicitEntriesFromAcl failed.
    }

    function GetExplicitAccessArray: TJwExplicitAccessArray; virtual;

    {@Name adds ACEs from another list to this one.
     The ACEs will be added using @Link(Add) so the ACL order will be correct.

     @param(AclInstance contains an access control list to be duplicated)
     @param(KeepOriginalOrder defines whether the order of elements in AclInstance
      will be kept. In this case the list will be added at the top of the target list. This may
      disturb the canonical order of elements.
      If set to false, the elements are added into the target list using canonical order.)

     @raises(EJwsclNILParameterException will be raised if aObject is nil)
     @raises(EJwsclInvalidACEException will be raised if the classtype of this list instance is not the same of aObject)
     }
    procedure AddACEs(AclInstance: TJwSecurityAccessControlList; KeepOriginalOrder : Boolean = true); virtual;

    {@Name adds an ACE instance to into the list. The ACE property ListOwner will be set to this list.

     Where the new item is inserted depends on its type:
      @orderedlist(
      @item If the ACE is a direct allow ACE it is added at the bottom of the list
      @item If the ACE is a inherited allow ACE it is added after the last deny ACE or at the top of list if no deny ACE exists
      @item If the ACE is a deny ACE it is added at the top of list
     )


     The following list shows a full access control list
      @orderedlist(
      @item deny ACE
      @item allow ACE  3 (direct)
      @item deny ACE   1 (inherited)
      @item allow ACE  2 (inherited)
      )The numbers shows the order the ACEs were added.

      Audit items are always added to the end of list and cannot be added to discretionary lists.

      Do not call this function without a pointer to the ACE :
        ACLList.Add(TJwAuditAccessControlEntry.Create(nil,[afObjectInheritAce],GENERIC_READ,aSID,False));
      If the Add method raises an Exception, the TJwAuditAccessControlEntry instance is not freed.
      Instead use a pointer
        try
          anACE := TJwAuditAccessControlEntry.Create(nil,[afObjectInheritAce],GENERIC_READ,aSID,False)
          try
            ACLList.Add(anACE);
          except
            anACE.Free;
          end;
        except

        end;

    @return(@Name returns the index in the list where the ACE was added)
    @raises(EJwsclNILParameterException will be raised if aObject is nil)
    @raises(EJwsclInvalidACEException will be raised if
              @unorderedlist(
               @item TJwDiscretionaryAccessControlEntry was given but the list is not an instance of TJwDAccessControlList
               @item TJwAuditAccessControlEntry was given but the list is not an instance of TJwSAccessControlList
               )
           )
    @raises(EJwsclDuplicateListEntryException will be raised if the given ACE is already in list)
    @raises(EJwsclInvalidSecurityListException)
    }
    function Add(AccessEntry: TJwSecurityAccessControlEntry): integer;

    {@Name computes the effective access rights of a user and the ACL.
     A better and more secure way to check access rights is to use
     AccessCheck with MAXIMUM_ALLOWED desired access rights.

     Known bug in internal Windows call:
       This function does/may not work with inherited access control elements and
       also can fail on some combination with deny entries!

     @param(User defines a trustee that is used to get effective access rights)
     @return(@Name returns the rights which are granted to User)
     @raises(EJwsclWinCallFailedException will be raised if a call to
       GetEffectiveRightsFromAcl failed)
     }
    function GetEffectiveRights(const User :
       {$IFDEF UNICODE}TTrusteeW{$ELSE}TTrusteeA{$ENDIF}) : TJwAccessMask; overload;


    {@Name computes the effective access rights of a user and the ACL.
     A better and more secure way to check access rights is to use
     AccessCheck with MAXIMUM_ALLOWED desired access rights.

     Known bug in internal Windows call:
       This function does/may not work with inherited access control elements and
       also can fail on some combination with deny entries!

     @param(User defines a SecurityID that is used to get effective access rights.
       This parameter can be nil to use the current user of thread or process instead.)
     @return(@Name returns the rights which are granted to User)
     @raises(EJwsclWinCallFailedException will be raised if a call to
       GetEffectiveRightsFromAcl failed)
     }
    function GetEffectiveRights(const User : TJwSecurityId) : TJwAccessMask; overload;

    {@Name returns the first ACE from the list.
     @return(The first ACE or if the list is empty @Name returns nil.)
     }
    function First: TJwSecurityAccessControlEntry;

    {@Name returns the list index of a ACE from the list.
     @return(Index of list. If the ACE is not in list the return value is -1.)
     }
    function IndexOf(AccessEntry: TJwSecurityAccessControlEntry): integer;

    {@Name inserts a ACE into the list.
     @param(Index List index where the ACE shall be inserted before.
            If the Index is not between 0 and Count-1 an exception will be raised.)
     }
    procedure Insert(Index: integer; AccessEntry: TJwSecurityAccessControlEntry);

    {@Name returns the last ACE of the list.
     If the list is empty the return value is nil.
     }
    function Last: TJwSecurityAccessControlEntry;

    {@Name searches for a SID in a access control list.
     @param SID defines the SID to be searched for.
     @param iPos defines the start position in the list. If -1 the search begins from
      the beginning. The search always starts with the next ACE given by index iPos.
     @return Returns the index of the found ACE with the given SID. If the index
       is out of range, invalid or the SID could not be found the return value is -1 .
     }
    function FindSID(const SidInstance: TJwSecurityId;
      const StartIndex: integer = -1): integer;

    {@Name seeks for a ACE in the ACList.

     @param ACE defines the ACE to be searched for in the list.
     @param EqualACETypeSet defines the criterias that are used to compare the ACE.
       The following criterias are available and can be combined in a set.
          @unorderedlist(
           @item(eactSameSid    The SID is used to compare (EqualSID) and must be equal)
           @item(eactSameFlags The Flags are compared and must be equal)
           @item(eactSameAccessMask The AccessMasks are compared and must be equal)
           @item(eactSameType The ACE type (deny, allow) are compared and must be equal)
           )
     @param iPos defines the start position for the search in the ACL list starting from zero (0).

     @return Returns the position of the found ACE in the list starting from 0.
             If the value iPos is out of bounds, or the ACE could not be found the return value is -1
     }
    function FindEqualACE(const AccessEntry: TJwSecurityAccessControlEntry;
      EqualAceTypeSet: TJwEqualAceTypeSet; const StartIndex: integer = -1): integer;

    {@Name removes the inheritance flag from all ACEs.
      This is useful if a DACL with inherited ACEs must be converted into a DACL with
       only explicit ACEs. This stops the inheritance flow.
      }
    procedure ConvertInheritedToExplicit;

    {@Name removes all explicit entries from the list using Remove. See @link(Remove)
     for information about how the entries are removed.}
    procedure RemoveExplicits;

    {@Name removes all inherited entries from the list using Remove. See @link(Remove)
     for information about how the entries are removed.}
    procedure RemoveInherited;

    {@Name checks whether the ACL is in canonical order.

     The following list shows a access control list in canonical order:
      @orderedlist(
      @item deny ACE   (direct)
      @item allow ACE  (direct)
      @item deny ACE   (inherited)
      @item allow ACE  (inherited)
      )
    }
    function IsCanonical: boolean;



    {@Name compares two ACL and returns true if they are equal.
     This method uses FindEqualACE to compare two access control entries.

     @param(AccessControlListInstance defines the second ACL. Cannot be nil.)
     @param(EqualAceTypeSet defines the ACE comparision done by FindEqualACE.
            By default all ACE members must be equal to return a positive result)
     @return(Returns true if both ACL are equal; otherwise False. It also
          returns false if AccessControlListInstance is nil)
    }
    function IsEqual(const AccessControlListInstance: TJwSecurityAccessControlList;
                     const EqualAceTypeSet: TJwEqualAceTypeSet = JwAllEqualAceTypes): boolean;

    {@Name removes a ACE from the list. If the List owns the object
     the ACE will also be freed.
     Do not call the Free method of the object to be removed if property OwnObjects is True otherwise
      an exception will be raised.
     @param(aObject contains the ACE to be removed)
     @return(@Name returns the index of the ACE in the list before it was removed. If
               the ACE could not be found the return value is -1.) }
    function Remove(AccessEntry: TJwSecurityAccessControlEntry): integer; overload;

    {@Name removes an object give by index from the list.
     The object will be freed automatically if OwnsObject is true.

    @return(Index receives the zero based index of the object to be removed.
             Valid values are 0 to Count -1 .)  
    @return(@Name returns the index of the ACE in the list before it was removed. If
               the ACE could not be found the return value is -1.)
    }
    function Remove(Index: integer): integer; overload;

    property Items[Index: integer]: TJwSecurityAccessControlEntry Read GetItem;
      default;

    {@Name returns a humand readable text that contains information about this ACL.}
    property Text: TJwString Read GetText;

    {@Name gets or sets or gets the revision of the ACL.
     The version is used to set the ACL structure revision in Create_PACL.
     The revision version is always updated to the highest ACE revision version.

     Can be one of the revision levels:
     ACL_REVISION, ACL_REVISION1, ACL_REVISION2, ACL_REVISION3, ACL_REVISION4 or ACL_REVISION_DS

     Default value is ACL_REVISION.

     This property raises an exception EJwsclInvalidRevision on setting
      if the Revision is not in range of 1..4
     }
    property Revision : Cardinal read fRevision write SetRevision;
  end;

  {@Name provides methods for an discretionary access control list.

   For more information on the methods see TJwSecurityAccessControlList.
  }
  TJwDAccessControlList = class(TJwSecurityAccessControlList)
  public
    {@Name creates an empty access control list. The list items will not be owned.}
    constructor Create; overload;

    {@Name creates an empty access control list.
     @param(ownACEs receives whether the items shall be freed on destruction (True) or not.)}
    constructor Create(ownACEs: boolean); overload;

    {@Name creates a new list from an existing access control list.
     All entries a copied and the list owns these objects (entry.ListOwner is set to this instance).

     @param(aAPCL receives the discretionary access control list to be copied)
     @raises(EJwsclWinCallFailedException will be raised if the aAPCL is not a valid access control list)
     @raises(EJwsclWinCallFailedException if an win api function failed)
    }
    constructor Create(AclPointerList: PACL); overload;

    {@Name creates a new access control list from an explicit access array.

     @raises(EJwsclNILParameterException will be raised if parameter Accesses is nil)
     @raises(EJwsclWinCallFailedException will be raised if the call to SetEntriesInAcl failed.)
     }
    constructor Create(const Accesses : TJwExplicitAccessArray); overload;

    {@Name creates a copy of the ACL that only contains inherited ACEs.
     The ACEs are copied and the list owns them.
     The caller must free the newly created DACL.
    }
    function GetInheritance: TJwDAccessControlList;

    {@Name creates a copy of the ACL that only contains explicit ACEs.
     The ACEs are copied and the list owns them.
     The caller must free the newly created DACL.
    }
    function GetExplicit: TJwDAccessControlList;

    {@Name rearranges the entries of the list to make the list canonical.

     The following list shows a access control list in canonical order:
      @orderedlist(
      @item deny ACE   (direct)
      @item allow ACE  (direct)
      @item deny ACE   (inherited)
      @item allow ACE  (inherited)
      )
    }
    procedure MakeCanonical;
  end;

  {@Name provides methods for an audit access control list.
   For more information on the methods see TJwSecurityAccessControlList .
  }
  TJwSAccessControlList = class(TJwSecurityAccessControlList)
  protected
    function GetItem(idx: integer): TJwAuditAccessControlEntry;

    {@Name returns the first mandatory label in the SACL if any
     @return(Returns the first mandatory label or nil if none exists)
    }
    function GetMandatoryLabel : TJwSystemMandatoryAccessControlEntry; virtual;
    procedure SetMandatoryLabelEx(
      const NewLabel : TJwSystemMandatoryAccessControlEntry);  virtual;
  public
    {@Name creates an empty access control list. The list items will not be owned.}
    constructor Create; overload;

     {@Name creates an empty access control list.
      @param(ownACEs receives whether the items shall be freed on destruction (True) or not.)}
    constructor Create(OwnAceEntries: boolean); overload;

     {@Name creates a new list from an existing access control list.
      All entries a copied and the list owns these objects (entry.ListOwner is set to this instance).

      @param(aAPCL receives the discretionary access control list to be copied)
      @raises(EJwsclWinCallFailedException will be raised if the aAPCL is not a valid access control list)
      @raises(EJwsclWinCallFailedException if an win api function failed)

      }
    constructor Create(AclPointerList: PACL); overload;

     {@Name creates an array of explicit access structure that represents
      the ACEs.

      @raises EJwsclWinCallFailedException will be raised if the call to GetExplicitEntriesFromAcl failed.
     }
    function GetExplicitAccessArray: TJwExplicitAccessArray; override;

    {@Name removes, adds or replaces a mandatory label.
     @unorderedlist(
     @item(If a label already exists in the SACL the new label will replace the old one.)
     @item(If no label exists the label will be added to the list. There is no order
       in a system acl so do not depend on it.)
     @item(If parameter NewLabel is nil the old label will be removed)
     )

     Removing:
     The old label will be erased depending on how OwnObjects. If OwnObjects is
     true the old label instance will be freed otherwise it will just be removed from list.

     @param(NewLabel defines the new label instance. It is copied or direclty added
      to the list depending on parameter CopyFlag)
     @param(CopyFlag defines how the new label is treated.
      @unorderedlist(
       @item(cfCopyInstance creates a new copy of the instance and adds it to the list.
          This is the default behavior if property MandatoryLabel is used.
          You should set OwnObjects to true so the new instance will be released when
          the SACL is freed.
          )
       @item(cfPointAtInstance simply adds the given instance to the list.
         If OwnObjects is true the instance will also be freed.)
       ))
     }
    procedure SetMandatoryLabel(
      const NewLabel : TJwSystemMandatoryAccessControlEntry;
      const CopyFlag: TJwCopyFlag);  virtual;

    {@Name checks whether the SACL has a mandatory level.

     @return(Returns true if a mandatory level exists otherwise false.)}
    function HasMandatoryLabel : boolean; virtual;

    {@Name contains the audit ACEs.}
    property Items[Index: integer]: TJwAuditAccessControlEntry Read GetItem;
      default;

    {@Name gets or sets the mandatory label of this SACL.
     If the mandatory label is retrieved it returns the mandatory label instance
     directly. Do not free it!
     If no label was found (HasMandatoryLabel = false) the return value is nil.

     The label can be changed whithout regarding access checks.
     Access checks are performed in SetSecurityInfo.
     Only the first label is changed. Any additional labels are ignored.
     The given mandatory label instance will be copied into a new instance
     and added automatically to the list. Do not use the parameter ListOwner
     of the Create constructor.
     Do not write this:
     @longcode(#
       audit.MandatoryLabel := TJwSystemMandatoryAccessControlEntry.Create(MandatoryLevelHigh);
       #)
     Instead use a variable to prevent a memory leak:
     @longcode(#
     var ML : TJwSystemMandatoryAccessControlEntry;
       ML := TJwSystemMandatoryAccessControlEntry.Create(MandatoryLevelHigh);
       audit1.MandatoryLabel := ML;
       ML.Free;
       audit1.OwnsObject := true;
     #)
     Also set OwnsObject to true so the copied instance will be freed automatically.

     You can also use SetMandatoryLabel to define how the instance will be treated.
     }
    property MandatoryLabel : TJwSystemMandatoryAccessControlEntry
        read GetMandatoryLabel write SetMandatoryLabelEx;
  end;


  {@Name contains data that describes how an object is accessed.
   The class provides methods to access these properties.

   To create an instance you can use the public constructors.
   However some constructors are protected and only visible in sub classes:
    @unorderedlist(
        @item TJwDiscretionaryAccessControlEntryAllow.Create(...)
        @item TJwDiscretionaryAccessControlEntryDeny.Create(...)
        @item TJwAuditAccessControlEntry.Create(...)
      )

   The ACE can be added to a list (called ACL - access control list).

  }
  TJwSecurityAccessControlEntry = class(TObject)
  protected
    fListOwner:  TJwSecurityAccessControlList;
    fSID:        TJwSecurityId;
    fFlags:      TJwAceFlags;
    fAccessMask: TJwAccessMask;
    fownSID:     boolean;
    fIgnore:     boolean;
    fUserData : Pointer;
    fHeader : TAceHeader;
    fRevision : Cardinal;

    fObjectType,
    fInheritedObjectType : TGuid;

    procedure SetListOwner(ListOwner: TJwSecurityAccessControlList);
    procedure SetFlags(FlagSet: TJwAceFlags);
    procedure SetAccessMask(anAccessMask: TJwAccessMask);
    function GetAccessMask : TJwAccessMask; virtual;
    procedure SetSID(SidInstance: TJwSecurityId);
    procedure CheckReadOnly;
    function GetAceType: TJwAceType; virtual;
    procedure SetOwnSid(const OwnSid : Boolean); Virtual;
    function GetOwnSid : Boolean;
    function GetObjectFlags : Cardinal; virtual;

  public
   {@Name creates a new ACE.
    Do not use this constructor.
    Instead use
      @unorderedlist(
        @item TJwDiscretionaryAccessControlEntryAllow.Create(...)
        @item TJwDiscretionaryAccessControlEntryDeny.Create(...)
        @item TJwAuditAccessControlEntry.Create(...)
      )

    @param(aListOwner retrieves the list owner  (including nil). If it is set to a list (not nil) the ACE is added to the list automatically.)
    @param(aFlags retrieves the ACE flags as a set)
    @param(anAccessMask retrieves the access mask like GENERIC_ALL)
    @param(aSID retrieves the ACE to be allowed or denied. It cannot be nil)
    @param(ownSID defines whether the SID given in parameter aSID should be freed automatically.
            If this SID is a well known SID from unit JwsclKnownSid this parameter is ignored)

    }
    constructor Create(const aListOwner: TJwSecurityAccessControlList;
      const aFlags: TJwAceFlags;
      const anAccessMask: TJwAccessMask;
      const aSID: TJwSecurityId;
      ownSID: boolean = True); overload;

    {@Name creates a new ACE applying a revision level.
    Do not use this constructor.
    Instead use
      @unorderedlist(
        @item TJwDiscretionaryAccessControlEntryAllow.Create(...)
        @item TJwDiscretionaryAccessControlEntryDeny.Create(...)
        @item TJwAuditAccessControlEntry.Create(...)
      )

    @param(ListOwner retrieves the list owner  (including nil). If it is set to a list (not nil) the ACE is added to the list automatically.)
    @param(Flags retrieves the ACE flags as a set)
    @param(AccessMask retrieves the access mask like GENERIC_ALL)
    @param(SID retrieves the ACE to be allowed or denied. It can be nil)
    @param(Revision Defines the revision level of the ACE. Can be one of the revision levels:
          ACL_REVISION, ACL_REVISION1, ACL_REVISION2, ACL_REVISION3, ACL_REVISION4 or ACL_REVISION_DS)
    @param(ownSID defines whether the SID given in parameter aSID should be freed automatically.
            If this SID is a well known SID from unit JwsclKnownSid this parameter is ignored)

    }
    constructor Create(const ListOwner: TJwSecurityAccessControlList;
      const Flags: TJwAceFlags;
      const AccessMask: TJwAccessMask;
      const SID: TJwSecurityId;
      const Revision : Cardinal;
      ownSID: boolean = True); overload;

    {@Name creates a copy of another ACE.
     All properties are copied except ListOwner. The ACE is not added to any list.

    Do not use this constructor.
    Instead use
      TJwDiscretionaryAccessControlEntryAllow.Create(...)
      TJwDiscretionaryAccessControlEntryDeny.Create(...)
      TJwAuditAccessControlEntry.Create(...)
    @param(aACE retrieves an existing ACE. It cannot be nil)
    @raises(EJwsclNILParameterException if aACE is nil)
    }
    constructor Create(const AccessEntry: TJwSecurityAccessControlEntry); overload;

    {@Name creates an access allowed structure from an existing one.
    @param(AccessEntryPointer contains a pointer to an PAccessDeniedAce structure.
     @unorderedlist(
       @item(ACCESS_ALLOWED_ACE returns class type TJwDiscretionaryAccessControlEntryAllow)
       @item(ACCESS_ALLOWED_CALLBACK_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackAllow)
       @item(ACCESS_ALLOWED_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryObjectAllow)
       @item(ACCESS_ALLOWED_CALLBACK_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackObjectAllow)

       @item(ACCESS_DENIED_ACE returns class type TJwDiscretionaryAccessControlEntryDeny)
       @item(ACCESS_DENIED_CALLBACK_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackDeny)
       @item(ACCESS_DENIED_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryObjectDeny)
       @item(ACCESS_DENIED_CALLBACK_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackObjectDeny)

       @item(SYSTEM_AUDIT_ACE returns class type TJwAuditAccessControlEntry)
       @item(SYSTEM_AUDIT_CALLBACK_ACE returns class type TJwAuditAccessControlEntryCallback)
       @item(SYSTEM_AUDIT_OBJECT_ACE returns class type TJwAuditAccessControlEntryObject)
       @item(SYSTEM_AUDIT_CALLBACK_OBJECT_ACE returns class type TJwAuditAccessControlEntryCallbackObject)

       @item(SYSTEM_MANDATORY_LABEL_ACE returns class type TJwSystemMandatoryAccessControlEntry)
     )
     Other types will raise an exception EJwsclInvalidACEException.
    )
    @return(Returns a derived class of TJwSecurityAccessControlEntry)
    @raises(EJwsclNILParameterException if parameter AccessEntryPointer is nil)
    @raises(EJwsclInvalidACEException will be raised if parameter
      AccessEntryPointer does not contain a supported ACE type structure)
    @raises(EJwsclInvalidAceMismatch will be raised if the header type of parameter AccessEntryPointer
      does not match this ACE instance type)
    }
    constructor Create(const AccessEntryPointer: PAccessAllowedAce); overload;

    {@Name creates an access denied structure from an existing one.

    @param(AccessEntryPointer contains a pointer to an PAccessDeniedAce structure.
     @unorderedlist(
       @item(ACCESS_ALLOWED_ACE returns class type TJwDiscretionaryAccessControlEntryAllow)
       @item(ACCESS_ALLOWED_CALLBACK_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackAllow)
       @item(ACCESS_ALLOWED_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryObjectAllow)
       @item(ACCESS_ALLOWED_CALLBACK_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackObjectAllow)

       @item(ACCESS_DENIED_ACE returns class type TJwDiscretionaryAccessControlEntryDeny)
       @item(ACCESS_DENIED_CALLBACK_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackDeny)
       @item(ACCESS_DENIED_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryObjectDeny)
       @item(ACCESS_DENIED_CALLBACK_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackObjectDeny)

       @item(SYSTEM_AUDIT_ACE returns class type TJwAuditAccessControlEntry)
       @item(SYSTEM_AUDIT_CALLBACK_ACE returns class type TJwAuditAccessControlEntryCallback)
       @item(SYSTEM_AUDIT_OBJECT_ACE returns class type TJwAuditAccessControlEntryObject)
       @item(SYSTEM_AUDIT_CALLBACK_OBJECT_ACE returns class type TJwAuditAccessControlEntryCallbackObject)

       @item(SYSTEM_MANDATORY_LABEL_ACE returns class type TJwSystemMandatoryAccessControlEntry)
     )
     Other types will raise an exception EJwsclInvalidACEException.
    )
    @return(Returns a derived class of TJwSecurityAccessControlEntry)
    @raises(EJwsclNILParameterException if parameter AccessEntryPointer is nil)
    @raises(EJwsclInvalidACEException will be raised if parameter
      AccessEntryPointer does not contain a supported ACE type structure)
    @raises(EJwsclInvalidAceMismatch will be raised if the header type of parameter AccessEntryPointer
      does not match this ACE instance type)
    }
    constructor Create(const AccessEntryPointer: PAccessDeniedAce); overload;

    {@Name creates an access allowed class from an existing structure and returns
     a compatible class instance.

    @param(AccessEntryPointer contains a pointer to an AccessAllowedACE structure.
      @unorderedlist(
       @item(ACCESS_ALLOWED_ACE returns class type TJwDiscretionaryAccessControlEntryAllow)
       @item(ACCESS_ALLOWED_CALLBACK_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackAllow)
       @item(ACCESS_ALLOWED_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryObjectAllow)
       @item(ACCESS_ALLOWED_CALLBACK_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackObjectAllow)

       @item(ACCESS_DENIED_ACE returns class type TJwDiscretionaryAccessControlEntryDeny)
       @item(ACCESS_DENIED_CALLBACK_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackDeny)
       @item(ACCESS_DENIED_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryObjectDeny)
       @item(ACCESS_DENIED_CALLBACK_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackObjectDeny)

       @item(SYSTEM_AUDIT_ACE returns class type TJwAuditAccessControlEntry)
       @item(SYSTEM_AUDIT_CALLBACK_ACE returns class type TJwAuditAccessControlEntryCallback)
       @item(SYSTEM_AUDIT_OBJECT_ACE returns class type TJwAuditAccessControlEntryObject)
       @item(SYSTEM_AUDIT_CALLBACK_OBJECT_ACE returns class type TJwAuditAccessControlEntryCallbackObject)

       @item(SYSTEM_MANDATORY_LABEL_ACE returns class type TJwSystemMandatoryAccessControlEntry)
     )
    )
    @return(Returns a derived class of TJwSecurityAccessControlEntry)
    @raises(EJwsclNILParameterException if parameter AccessEntryPointer is nil)
    @raises(EJwsclInvalidACEException will be raised if parameter
      AccessEntryPointer does not contain a supported ACE type structure)
    }
    class function CreateACE(const AccessEntryPointer: PAccessAllowedAce):
      TJwSecurityAccessControlEntry; overload;

    {@Name creates an access denied structure from an existing one.
    @param(AccessEntryPointer contains a pointer to an AccessDeniedACE structure.)
    @return(Returns an ACE of type TJwDiscretionaryAccessControlEntryDeny)
    @raises(EJwsclNILParameterException if aACE is nil)
    }
    class function CreateACE(const AccessEntryPointer: PAccessDeniedAce):
      TJwSecurityAccessControlEntry; overload;

  
    {@Name creates an with 0 initialized ACE and returns the corresponding class.
    @param(anACEType receives an @link(TJwAceType) type.)
    @return(The return value is one of the listed class types

     @unorderedlist(
      @item(TjwAceType member - returned class type)

      @item(actAudit - TJwAuditAccessControlEntry)
      @item(actAuditCallback - TJwAuditAccessControlEntryCallback)
      @item(actAuditObject - TJwAuditAccessControlEntryObject)
      @item(actAuditCallbackObject - TJwAuditAccessControlEntryCallbackObject)

      @item(actAllow - TJwDiscretionaryAccessControlEntryAllow)
      @item(actAllowCallback - TJwDiscretionaryAccessControlEntryCallbackAllow)
      @item(actAllowObject - TJwDiscretionaryAccessControlEntryObjectAllow)
      @item(actAllowCallbackObject - TJwDiscretionaryAccessControlEntryCallbackObjectAllow)
      @item(actDeny - TJwDiscretionaryAccessControlEntryDeny)
      @item(actDenyCallback - TJwDiscretionaryAccessControlEntryCallbackDeny)
      @item(actDenyObject - TJwDiscretionaryAccessControlEntryObjectDeny)
      @item(actDenyCallbackObject - TJwDiscretionaryAccessControlEntryCallbackObjectDeny)
      
      @item(actMandatory - TJwSystemMandatoryAccessControlEntry)
     )
    )
     @raises(EJwsclInvalidACEException will be raised if the ACE type of
      the instance is not supported)
    }
    class function CreateACE(anAceType: TJwAceType):
      TJwSecurityAccessControlEntry;
      overload;


    {@Name initializes a ACE. It is used internally}
    constructor Create; overload;

  public
    {@Name destroys an ACE.
     @raises(EJwsclReadOnlyPropertyException will be raised if the ACE is in a list (ListOwner is not nil))
     }
    destructor Destroy; override;

    {@Name frees the memory of an ACE if it does not belong to a list (ListOwner = nil); otherwise nothing happens.
     Instead use Remove of TJwSecurityAccessControlList .

     @raises(EJwsclInvalidACEException will be raised if the ACE is not in the list of ListOwner) 
    }
    procedure Free;
  protected

    {@Name creates a memory block filled with an ACE structure.
     The structure is points to PAccessAllowedAce structure;
     It must be freed by Free_PACE.

     The following types (value of property AceType) are supported:
     @unorderedlist(
      @item(actAudit)
      @item(actAuditCallback)
      @item(actMandatory)
      @item(actAllow)
      @item(actAllowCallback)
      @item(actDeny)
      @item(actDenyCallback)
     )
     @raises(EJwsclInvalidACEException will be raised if the ACE type of
      the instance is not supported)
    }
    function Create_AllowACE: PAccessAllowedAce; overload;


    {@Name creates a memory block filled with an ACE structure.
     The structure is points to PAccessDeniedAce structure;
     It must be freed by Free_PACE.

     The following types (value of property AceType) are supported:
     @unorderedlist(
      @item(actAudit)
      @item(actAuditCallback)
      @item(actMandatory)
      @item(actAllow)
      @item(actAllowCallback)
      @item(actDeny)
      @item(actDenyCallback)
     )

     @raises(EJwsclInvalidACEException will be raised if the ACE type of
      the instance is not supported)
     }
    function Create_DenyACE: PAccessDeniedAce; overload;


    {@Name creates an ACE WinAPI structure depending on the Sid type (property AceType) of
     the instance.

     The following types are supported:
     @unorderedlist(
      @item(TjwAceType member - returned pointer to structure)
      @item(actAudit - SYSTEM_AUDIT_ACE)
      @item(actAuditCallback - SYSTEM_AUDIT_CALLBACK_ACE)
      @item(actAuditObject - SYSTEM_AUDIT_OBJECT_ACE)
      @item(actAuditCallbackObject - SYSTEM_AUDIT_CALLBACK_OBJECT_ACE)
      @item(actMandatory - SYSTEM_MANDATORY_LABEL_ACE)
      @item(actAllow - ACCESS_ALLOWED_ACE)
      @item(actAllowCallback - ACCESS_ALLOWED_CALLBACK_ACE)
      @item(actAllowObject - ACCESS_ALLOWED_OBJECT_ACE)
      @item(actAllowCallbackObject - ACCESS_ALLOWED_CALLBACK_OBJECT_ACE)
      @item(actDeny - ACCESS_DENIED_ACE)
      @item(actDenyCallback - ACCESS_DENIED_CALLBACK_ACE)
      @item(actDenyObject - ACCESS_DENIED_OBJECT_ACE)
      @item(actDenyCallbackObject - ACCESS_DENIED_CALLBACK_OBJECT_ACE)
     )
     @param(Size returns the memory size of the newly allocated block in bytes.)
     @return(The return value is a pointer to a ACE Winapi structure. The supported
       types can be read from the description above.
       The memory block must be freed with GlobalFree.)
     @raises(EJwsclInvalidACEException will be raised if the ACE type of
      the instance is not supported)
    }
    function CreateDynamicACE(out Size : Cardinal) : Pointer;


    {@Name returns the size of an ace structure without the additional
     space for the Sid memory.

     The following types are supported:
     @unorderedlist(
      @item(actAudit)
      @item(actAuditCallback)
      @item(actAuditObject)
      @item(actAuditCallbackObject)
      @item(actMandatory)
      @item(actAllow)
      @item(actAllowCallback)
      @item(actAllowObject)
      @item(actAllowCallbackObject)
      @item(actDeny)
      @item(actDenyCallback)
      @item(actDenyObject)
      @item(actDenyCallbackObject)
     )
     @raises(EJwsclInvalidACEException will be raised if the ACE type of
      the instance is not supported)

    }
    function GetDynamicTypeSize : Cardinal;

    {@Name frees a PAccessAllowedAce access control list.
     It can free ACE memory created by Create_AllowACE .

     @param(aPACE a PAccessAllowedAce or PAccessDeniedAce to be freed. It will be set to nil afterwards.
            If aPACE is nil nothing happens.)

     }
    procedure Free_PACE(var AccessEntryPointer: PAccessAllowedAce); overload;

    {@Name frees a PAccessAllowedAce access control list.
     It can free ACE memory created by Create_DenyACE .

     @param(aPACE a PAccessAllowedAce or PAccessDeniedAce to be freed. It will be set to nil afterwards.
            If aPACE is nil nothing happens.)

     }
    procedure Free_PACE(var AccessEntryPointer: PAccessDeniedAce); overload;

    {@Name creates an Explicit Access structure.

     If the ACE is an audit ace the grfAccessMode value is set to SET_AUDIT_FAILURE even if the flag AuditFailure is not set.
      You have to create two ExplicitAccess structures and change them to use them.

     @raises(EJwsclInvalidSIDException will be raised if the property SID is nil or invalid.)
     }
    function GetExplicitAccess: TJwExplicitAccess;

  public
    {@Name returns an ACE class type depending on the give ACE type.
     To create a class instance @link(CreateACE) can be used.

     @param(AceType defines the wished ace type.
          Supported types are
          @unorderedlist(
            @item(actAudit)
            @item(actAuditCallback)
            @item(actAuditObject)
            @item(actAuditCallbackObject)
            @item(actMandatory)
            @item(actAllow)
            @item(actAllowCallback)
            @item(actAllowObject)
            @item(actAllowCallbackObject)
            @item(actDeny)
            @item(actDenyCallback)
            @item(actDenyObject)
            @item(actDenyCallbackObject)
          ))
     @raises(EJwsclInvalidACEException will be raised if the given ACE type
        is not supported.)
    }
    class function GetClassAceType(const AceType: TJwAceType)
      : TJwSecurityAccessControlEntryClass;

    {@Name copies all properties from another ACE.
     This method does not add this instance to any list or change ListOwner.

     @raises(EJwsclNILParameterException will be raised if one of these
        objects are nil:
        @orderedlist(
          @item(AccessEntry)
          @item(AccessEntry.SID)
        )
     )

    }
    procedure Assign(AccessEntry: TJwSecurityAccessControlEntry); virtual;

    {@Name returns information about the SID instance using human readable
     description. This function shows bit states for the AccessMask.
     Use @link(GetTextMap) to define readable access rights mapping.}
    function GetText: TJwString;
  public
    {@Name returns information about the SID instance using human readable
     description. This function can convert the AccessMask bits into strings
     using a mapping class.
     @param(Mapping defines a class thats provides the mapping implementation.
       If this parameter is nil the AccessMask will be shown as decimal and
       as hex number.
        )
    }
    function GetTextMap(const Mapping: TJwSecurityGenericMappingClass =
      nil): TJwString;

    {@Name contains the owner of this ACE. It cannot be set.
     If ListOwner is nil it can be set once. If it set to a
      TJwSecurityAccessControlList instance the ACE will automatically added
      to the list.

     To change the value a copy must be created of the instance.  }
    property ListOwner: TJwSecurityAccessControlList
      Read fListOwner Write SetListOwner;

    {@Name contains the flags of the ACE}
    property Flags: TJwAceFlags Read fFlags Write SetFlags;

    {@Name contains the access mask of the ACE.}
    property AccessMask: TJwAccessMask Read GetAccessMask Write SetAccessMask;

    {@Name contains the security identifier. It can be nil.
     }
    property SID: TJwSecurityId Read fSid Write SetSID;

    {@Name returns the type of the access control entry.
     It can return the following values :
     @unorderedlist(
       @item TJwDiscretionaryAccessControlEntry        - actAllow
       @item TJwDiscretionaryAccessControlEntryAllow   - actAllow
       @item TJwDiscretionaryAccessControlEntryDeny    - actDeny
       @item TJwAuditAccessControlEntry                - actAudit
       )
       }
    property AceType: TJwAceType Read GetAceType;

    {@Name defines whether the TJwSecurityId SID will be freed (True) or not (False).
     If property SID is a well known SID from unit JwsclKnownSid this property will be ignored)
    }
    property OwnSID: boolean Read GetOwnSID Write SetownSID;

    property Header : TAceHeader read fHeader write fHeader;

    {@Name gets or sets or gets the revision of the ACE
     Can be one of the revision levels:
          ACL_REVISION, ACL_REVISION1, ACL_REVISION2, ACL_REVISION3, ACL_REVISION4 or ACL_REVISION_DS
    }
    property Revision : Cardinal read fRevision write fRevision;

    {not used}
    property Ignore: boolean Read fIgnore Write fIgnore;

    {@Name defines user data that can be used to attach used defined data}
    property UserData : Pointer read fUserData write fUserData;

    {@Name returns the object flags of the ACE.
     This property depends on property ObjectType and InheritedObjectType.

     It returns a combination of these flags if parameter...
      @unorderedlist(
       @item(ACE_OBJECT_TYPE_PRESENT ...ObjectType is not a NULL GUID)
       @item(ACE_INHERITED_OBJECT_TYPE_PRESENT ...InheritedObjectType is not a NULL GUID)
      )
    }
    property ObjectFlags : Cardinal read GetObjectFlags;

    {@Name contains the object or property specific guid.
     It is only of use if the instance of this ACE is an object of one of these types
      @unorderedlist(
        @item(TJwAuditAccessControlEntryObject)
        @item(TJwAuditAccessControlEntryCallbackObject)
        @item(TJwDiscretionaryAccessControlEntryObjectAllow)
        @item(TJwDiscretionaryAccessControlEntryCallbackObjectAllow)
        @item(TJwDiscretionaryAccessControlEntryObjectDeny)
        @item(TJwDiscretionaryAccessControlEntryCallbackObjectDeny)
      )

      See also http://msdn2.microsoft.com/en-us/library/aa374917(VS.85).aspx
     }
    property ObjectType : TGuid read fObjectType write fObjectType;

     {@Name contains the object or property specific guid.
     It is only of use if the instance of this ACE is an object of one of these types
      @unorderedlist(
        @item(TJwAuditAccessControlEntryObject)
        @item(TJwAuditAccessControlEntryCallbackObject)
        @item(TJwDiscretionaryAccessControlEntryObjectAllow)
        @item(TJwDiscretionaryAccessControlEntryCallbackObjectAllow)
        @item(TJwDiscretionaryAccessControlEntryObjectDeny)
        @item(TJwDiscretionaryAccessControlEntryCallbackObjectDeny)
      )

      See also http://msdn2.microsoft.com/en-us/library/aa374917(VS.85).aspx
     }
    property InheritedObjectType : TGuid read fInheritedObjectType write fInheritedObjectType;
  end;

  {@Name defines a discretionary access control entry.
   Use @link(TJwDiscretionaryAccessControlEntryAllow) or @link(TJwDiscretionaryAccessControlEntryDeny) for creating
    ACEs.
   }
  TJwDiscretionaryAccessControlEntry = class(TJwSecurityAccessControlEntry);

  {@Name is a class that defines a positve/allow access control entry.}
  TJwDiscretionaryAccessControlEntryAllow =
    class(TJwDiscretionaryAccessControlEntry)
  public
    {@Name creates a new positive ACE.

    @param(aListOwner retrieves the list owner  (including nil). If it is set to a list (not nil) the ACE is added to the list automatically.)
    @param(aFlags retrieves the ACE flags as a set)
    @param(anAccessMask retrieves the access mask like GENERIC_ALL.
           If you want to set file or folder security use FILE_ALL_ACCESS or similar instead of GENERIC_XXX.
           Some flags are discarded when written to disk and would differ after read from disk.
              )
    @param(aSID retrieves the SID to be allowed or denied. It can be nil)
    @param(ownSID defines whether the SID given in parameter aSID should be freed automatically.
            If this SID is a well known SID from unit JwsclKnownSid this parameter is ignored)


    }

    constructor Create(const aListOwner: TJwSecurityAccessControlList;
      const aFlags: TJwAceFlags;
      const anAccessMask: TJwAccessMask;
      const aSID: TJwSecurityId;
      ownSID: boolean = True); overload;

    {@Name creates a new positive ACE and applies an ACE revision level.

    @param(aListOwner retrieves the list owner  (including nil). If it is set to a list (not nil) the ACE is added to the list automatically.)
    @param(aFlags retrieves the ACE flags as a set)
    @param(anAccessMask retrieves the access mask like GENERIC_ALL.
           If you want to set file or folder security use FILE_ALL_ACCESS or similar instead of GENERIC_XXX.
           Some flags are discarded when written to disk and would differ after read from disk.
              )
    @param(Revision Defines the revision level of the ACE. Can be one of the revision levels.
          ACL_REVISION, ACL_REVISION1, ACL_REVISION2, ACL_REVISION3, ACL_REVISION4 or ACL_REVISION_DS)
    @param(aSID retrieves the SID to be allowed or denied. It can be nil)
    @param(ownSID defines whether the SID given in parameter aSID should be freed automatically.
            If this SID is a well known SID from unit JwsclKnownSid this parameter is ignored)


    }
    constructor Create(
      const ListOwner: TJwSecurityAccessControlList;
      const Flags: TJwAceFlags;
      const AccessMask: TJwAccessMask;
      const SID: TJwSecurityId;
      const Revision : Cardinal;
      ownSID: boolean = True); overload;


    {@Name creates a positive copy of another positive ACE.
     All properties are copied except ListOwner. The ACE is not added to any list.
     It is possible to provide a negative ACE (by type conversion). In this case
      the ACE is converted to a positive one.

    @param(aACE retrieves an existing positive ACE. It cannot be nil)
    @raises(EJwsclNILParameterException if aACE is nil)
    }
    constructor Create(const AccessEntry: TJwDiscretionaryAccessControlEntryAllow); overload;


    {@Name creates an access allowed structure from an existing one.
    @param(accessACE contains a pointer to an AccessAllowedACE structure.)
    @raises(EJwsclNILParameterException if aACE is nil)
    }
    constructor Create(const AccessEntryPointer: PAccessAllowedAce); overload;

    {@Name creates a memory block filled with an ACE structure.
     The structure is points to PAccessAllowedAce structure;
     It must be freed by Free_PACE.

     The following types (value of property AceType) are supported:
     @unorderedlist(
      @item(actAudit)
      @item(actAuditCallback)
      @item(actMandatory)
      @item(actAllow)
      @item(actAllowCallback)
      @item(actDeny)
      @item(actDenyCallback)
     )
     @raises(EJwsclInvalidACEException will be raised if the ACE type of
      the instance is not supported)
     }
    function Create_AllowACE: PAccessAllowedAce; overload;

    {@Name frees a PAccessAllowedAce access control list.
     It can free ACE memory created by Create_AllowACE .

     @param(aPACE a PAccessAllowedAce or PAccessDeniedAce to be freed. It will be set to nil afterwards.
            If aPACE is nil nothing happens.)

     }
    procedure Free_PACE(var AccessEntryPointer: PAccessAllowedAce); overload;
  end;

  TJwDiscretionaryAccessControlEntryCallbackAllow =
    class(TJwDiscretionaryAccessControlEntryAllow)
  end;

  TJwDiscretionaryAccessControlEntryObjectAllow =
    class(TJwDiscretionaryAccessControlEntryAllow)
  end;

  TJwDiscretionaryAccessControlEntryCallbackObjectAllow =
    class(TJwDiscretionaryAccessControlEntryAllow)
  end;


  {@Name is a class that defines a negative/deny access control entry.}
  TJwDiscretionaryAccessControlEntryDeny =
    class(TJwDiscretionaryAccessControlEntry)
  public
    {@Name creates a new negative ACE.

    @param(aListOwner retrieves the list owner  (including nil). If it is set to a list (not nil) the ACE is added to the list automatically.)
    @param(aFlags retrieves the ACE flags as a set)
    @param(anAccessMask retrieves the access mask like GENERIC_ALL)
    @param(aSID retrieves the SID to be allowed or denied. It can be nil)
    @param(ownSID defines whether the SID given in parameter aSID should be freed automatically.
            If this SID is a well known SID from unit JwsclKnownSid this parameter is ignored)


    }

    constructor Create(const aListOwner: TJwSecurityAccessControlList;
      const aFlags: TJwAceFlags;
      const anAccessMask: TJwAccessMask;
      const aSID: TJwSecurityId;
      ownSID: boolean = True); overload;

    {@Name creates a new negative ACE and applies an ACE revision level.

    @param(aListOwner retrieves the list owner  (including nil). If it is set to a list (not nil) the ACE is added to the list automatically.)
    @param(aFlags retrieves the ACE flags as a set)
    @param(anAccessMask retrieves the access mask like GENERIC_ALL.
           If you want to set file or folder security use FILE_ALL_ACCESS or similar instead of GENERIC_XXX.
           Some flags are discarded when written to disk and would differ after read from disk.
              )
    @param(Revision Defines the revision level of the ACE. Can be one of the revision levels.
          ACL_REVISION, ACL_REVISION1, ACL_REVISION2, ACL_REVISION3, ACL_REVISION4 or ACL_REVISION_DS)
    @param(aSID retrieves the SID to be allowed or denied. It can be nil)
    @param(ownSID defines whether the SID given in parameter aSID should be freed automatically.
            If this SID is a well known SID from unit JwsclKnownSid this parameter is ignored)


    }
    constructor Create(
      const ListOwner: TJwSecurityAccessControlList;
      const Flags: TJwAceFlags;
      const AccessMask: TJwAccessMask;
      const SID: TJwSecurityId;
      const Revision : Cardinal;
      ownSID: boolean = True); overload;


    {@Name creates a negative copy of another negative ACE.
     All properties are copied except ListOwner. The ACE is not added to any list.
     It is possible to provide a positive ACE (by type conversion). In this case
      the ACE is converted to a negative one.

    @param(aACE retrieves an existing positive ACE. It cannot be nil)
    @raises(EJwsclNILParameterException if aACE is nil)
    }
    constructor Create(AccessEntry: TJwDiscretionaryAccessControlEntryDeny); overload;

    {@Name creates an access denied structure from an existing one.
    @param(accessACE contains a pointer to an AccessDeniedACE structure.)
    @raises(EJwsclNILParameterException if aACE is nil)
    }
    constructor Create(const AccessEntryPointer: PAccessDeniedAce); overload;

    {@Name creates a memory block filled with an ACE structure.
     The structure is points to PAccessDeniedAce structure;
     It must be freed by Free_PACE.
     }
    function Create_DenyACE: PAccessDeniedAce; overload;

    {@Name frees a PAccessDeniedAce access control list.
     It can free ACE memory created by Create_DenyACE .

     @param(aPACE a PAccessAllowedAce or PAccessDeniedAce to be freed. It will be set to nil afterwards.
            If aPACE is nil nothing happens.)
     }
    procedure Free_PACE(var AccessEntryPointer: PAccessDeniedAce); overload;
  end;


  TJwDiscretionaryAccessControlEntryCallbackDeny =
    class(TJwDiscretionaryAccessControlEntryDeny)
  end;

  TJwDiscretionaryAccessControlEntryObjectDeny =
    class(TJwDiscretionaryAccessControlEntryDeny)
  end;


  TJwDiscretionaryAccessControlEntryCallbackObjectDeny =
    class(TJwDiscretionaryAccessControlEntryDeny)
  end;


  {@Name provides function for a system control entry.
   The flags property is ignored.
  }
  TJwAuditAccessControlEntry = class(TJwSecurityAccessControlEntry)
  private
  protected
    fAuditSuccess, fAuditFailure: boolean;
    fFlagsIgnored: TJwAceFlags;

    constructor Create; overload;
    function GetAuditSuccess: boolean; virtual;
    function GetAuditFailure: boolean; virtual;

    procedure SetAuditFailure(const Value: boolean); virtual;
    procedure SetAuditSuccess(const Value: boolean); virtual;

    function GetAccessMask : TJwAccessMask; override;
  public
    {@Name creates a new audit ACE.  (SACL).
     AuditSuccess, AuditFailure are set to False and can be changed afterwards.

    @param(aListOwner retrieves the list owner (including nil). If it is set to a list (not nil) the ACE is added to the list automatically. )
    @param(aFlags retrieves the ACE flags as a set)
    @param(anAccessMask retrieves the access mask like GENERIC_ALL)
    @param(aSID retrieves the SID to be allowed or denied. It can be nil)
    @param(ownSID defines whether the SID given in parameter aSID should be freed automatically.
            If this SID is a well known SID from unit JwsclKnownSid this parameter is ignored)


    }
    constructor Create(const aListOwner: TJwSecurityAccessControlList;
      const aFlags: TJwAceFlags;
      const anAccessMask: TJwAccessMask;
      const aSID: TJwSecurityId;
      ownSID: boolean = True); overload;

    {@Name creates a new audit ACE.  (SACL).

    @param(aListOwner retrieves the list owner (including nil). If it is set to a list (not nil) the ACE is added to the list automatically. )
    @param(aFlags retrieves the ACE flags as a set)
    @param(anAccessMask retrieves the access mask like GENERIC_ALL)
    @param(anAuditSuccess receives the state of success audit flag. If true the ACE will
        audit successfull access.)
    @param(aAuditFailure receives the state of failure audit flag. If true the ACE will
        audit failed access.)
    @param(aSID retrieves the SID to be allowed or denied. It can be nil)
    @param(ownSID defines whether the SID given in parameter aSID should be freed automatically.
            If this SID is a well known SID from unit JwsclKnownSid this parameter is ignored)


    }
    constructor Create(const aListOwner: TJwSecurityAccessControlList;
      aAuditSuccess, aAuditFailure: boolean;
      const anAccessMask: TJwAccessMask;
      const aSID: TJwSecurityId;
      ownSID: boolean); overload;

    {@Name creates a copy of another audit ACE (SACL).
     All properties are copied except ListOwner. The ACE is not added to any list.
     Internally the audit object is converted to a positive ACE.

    @param(aACE retrieves an existing positive ACE. It cannot be nil)
    @raises(EJwsclNILParameterException if aACE is nil)
    }
    constructor Create(AccessEntry: TJwAuditAccessControlEntry); overload;

    {@Name creates an audit access structure from an existing one.
     The properties AuditSuccess and AuditFailure are retrieved from the accessACE.Flags value.

    @param(accessACE contains a pointer to an PSystemAuditAce structure.)
    @raises(EJwsclNILParameterException if aACE is nil)
    }
    constructor Create(const AccessEntryPointer: PSystemAuditAce); overload;

    {@Name creates a memory block filled with an ACE audit structure.
     The properties AuditSuccess and AuditFailure are set from the accessACE.Flags value.

     The structure is points to PSystemAuditAce structure;
     It must be freed by Free_PACE.
     }
    function Create_AuditACE: PSystemAuditAce; overload;

    {@Name frees a PSystemAuditAce access control list.
     It can free ACE memory created by Create_AuditACE .

     @param(aPACE a PSystemAuditAce to be freed. It will be set to nil afterwards.
            If aPACE is nil nothing happens.)
     }
    procedure Free_PACE(var AccessEntryPointer: PSystemAuditAce); overload;

    {@Name copies all properties from another audit ACE.
     The instance @classname must not be already added to a list (ListOwner must be nil).
      However aObject can be in a list.
     @Name creates copies of all properties. It also makes a copy of the SID and sets
      ownSID to True so the SID will be freed on destruction.
     ListOwner will be set to nil. You have to add the ACE manually.
    }
    procedure Assign(AccessEntry: TJwAuditAccessControlEntry); reintroduce; virtual;
  public
    {@Name defines whether an positive access is audited (True) or not (False).}
    property AuditSuccess: boolean Read GetAuditSuccess Write SetAuditSuccess;

    {@Name defines whether an negative access is audited (True) or not (False).}
    property AuditFailure: boolean Read GetAuditFailure Write SetAuditFailure;

    //@Name is ignored and cannot be changed - the value is empty
    //  property Flags: TJwAceFlags read fFlagsIgnored;
  end;

  TJwAuditAccessControlEntryCallback =
    class(TJwAuditAccessControlEntry)
  end;


  TJwAuditAccessControlEntryObject =
    class(TJwAuditAccessControlEntry)
  end;

  TJwAuditAccessControlEntryCallbackObject =
    class(TJwAuditAccessControlEntry)
  end;


  {@Name defines a mandatory label ACE in a SACL.
   }
  TJwSystemMandatoryAccessControlEntry = class(TJwSecurityAccessControlEntry)
  public
    constructor Create(const MandatoryLevel : TMandatoryLevel;
      const ListOwner: TJwSAccessControlList = nil);overload;

    {@Name creates a mandatory label structure from an existing one.
    @param(accessACE contains a pointer to an MandatoryLabel structure.)
    @raises(EJwsclNILParameterException if aACE is nil)
    }
    constructor Create(const MandatoryLabel: PSystemMandatoryLabelAce); overload;


    {@Name creates a copy of another ACE.
     All properties are copied except ListOwner. The ACE is not added to any list.

    @param(aACE retrieves an existing ACE. It cannot be nil)
    @raises(EJwsclNILParameterException if aACE is nil)
    }
    constructor Create(const MandatoryLabel: TJwSystemMandatoryAccessControlEntry); overload;

    {@Name returns the mandatory level type.}
    function GetMandatoryLevelType : TMandatoryLevel; virtual;

    {@Name returns the policy of the mandatory label.
     This policy defines how access is defined to lower or higher objects.
     This value is an interpreted value from property AccessMask.}
    function GetMandatoryPolicy : TJwMandatoryPolicyFlagSet; virtual;

    {@Name sets the mandatory policy.
     This function simply changes the property AccessMask.
    }
    procedure SetMandatoryPolicy(const Level : TJwMandatoryPolicyFlagSet); virtual;


    {@Name compare the mandatory ACE with another one.

     @param(MandatoryLabel defines a label to be compared)
     @return(The return value is smaller than zero if the actual label is less
      privileged than the given label. If the return value is greater than zero
      the given label has a higher level than the given one.
      The returned value is the difference of
       "Self.GetMandatoryLevelType - MandatoryLabel.GetMandatoryLevelType"

      If the return value is zero both labels are equal.)
     @raises(EJwsclNILParameterException The exception is raised if parameter
       MandatoryLabel is nil)
     }
    function Compare(const MandatoryLabel : TJwSystemMandatoryAccessControlEntry) : Integer;

    {@Name sets or gets the mandatory policy.
     See http://msdn2.microsoft.com/en-us/library/aa965848.aspx}
    property MandatoryPolicy : TJwMandatoryPolicyFlagSet read GetMandatoryPolicy
      write SetMandatoryPolicy;
  end;

  function JwFormatAccessRights(const Access : Cardinal;
     RightsMapping : Array of TJwRightsMapping) : TJwString; overload;
  function JwFormatAccessRights(
    const GrantedAccess : TJwAccessMaskArray;
    const AccessStatus : TJwCardinalArray;
    RightsMapping : Array of TJwRightsMapping) : TJwString; overload;

{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation

uses Math, JwsclEnumerations;



{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}

procedure RaiseOnInvalidRevision(const Revision: Cardinal; const MethodName, ClassName : AnsiString);
begin
 if (Revision < MIN_ACL_REVISION) or (Revision > MAX_ACL_REVISION) then
   raise EJwsclInvalidRevision.CreateFmtEx(RsInvalidRevision,
      MethodName, ClassName, RsUNAcl, 0, false, [Revision]);
end;


function JwFormatAccessRights(const Access : Cardinal;
     RightsMapping : Array of TJwRightsMapping) : TJwString;
var i : Integer;
begin
  for i := low(RightsMapping) to high(RightsMapping) do
  begin
    if Access and RightsMapping[i].Right =
      RightsMapping[i].Right then
      Result := Result + '[X] '
    else
      Result := Result + '[ ] ';

    //names may vary depeding on resource string contents
    Result := Result + RightsMapping[i].Name + #13#10;
  end;
end;

function JwFormatAccessRights(
    const GrantedAccess : TJwAccessMaskArray;
    const AccessStatus : TJwCardinalArray;
    RightsMapping : Array of TJwRightsMapping) : TJwString;
var i,i2 : Integer;
begin
  result := '- generic'#13#10;
  for i := low(RightsMapping) to high(RightsMapping) do
  begin
    if i = 7 then
      result := result + '- specific' + #13#10;
    if i = 17 then
      result := result + '- standard' + #13#10;

    for i2 := 0 to High(GrantedAccess) do
    begin
      if GrantedAccess[i2] and RightsMapping[i].Right =
        RightsMapping[i].Right then
        result := result + '[X]'
      else
      if (AccessStatus[i2] <> ERROR_SUCCESS) and
         (AccessStatus[i2] <> 5)
        then
        result := result + '['+IntToStr(AccessStatus[i2])+']'
      else
        result := result + '[ ]';
    end;

    //names may vary depeding on resource string contents
    result := result + ' ' + RightsMapping[i].Name + #13#10;
  end;
end;


constructor TJwDAccessControlList.Create;
begin
  inherited;
end;

constructor TJwDAccessControlList.Create(ownACEs: boolean);
begin
  inherited Create(ownACEs);
end;

constructor TJwDAccessControlList.Create(AclPointerList: PACL);
begin
  inherited Create(AclPointerList);
end;

constructor TJwDAccessControlList.Create(const Accesses : TJwExplicitAccessArray);
begin
  inherited;
end;

function TJwDAccessControlList.GetInheritance: TJwDAccessControlList;
begin
  Result := TJwDAccessControlList.Create;
  Result.Assign(Self);
  Result.RemoveExplicits;
end;

function TJwDAccessControlList.GetExplicit: TJwDAccessControlList;
begin
  Result := TJwDAccessControlList.Create;
  Result.Assign(Self);
  Result.RemoveInherited;
end;




constructor TJwSecurityAccessControlList.Create(OwnAceEntries: boolean);
begin
  //  raise EJwsclInvalidSecurityListException.CreateFmtEx('Do not call TJwSecurityAccessControlList.Create .', 'Create()',ClassName,'JwsclAcl.pas', 0,False,[]);
  inherited Create(OwnAceEntries);
  fRevision := ACL_REVISION;
end;

constructor TJwSecurityAccessControlList.Create;
begin
  //  raise EJwsclInvalidSecurityListException.CreateFmtEx('Do not call TJwSecurityAccessControlList.Create .', 'Create()',ClassName,'JwsclAcl.pas', 0,False,[]);
  inherited;
  fRevision := ACL_REVISION;
end;


constructor TJwSAccessControlList.Create;
begin
  inherited;
end;

constructor TJwSAccessControlList.Create(OwnAceEntries: boolean);
begin
  inherited Create(OwnAceEntries);
end;

constructor TJwSAccessControlList.Create(AclPointerList: PACL);
begin
  inherited Create(AclPointerList);
end;

function TJwSAccessControlList.GetItem(idx: integer): TJwAuditAccessControlEntry;
begin
  Result := TJwAuditAccessControlEntry(inherited GetItem(idx));
end;

function TJwSAccessControlList.GetExplicitAccessArray: TJwExplicitAccessArray;
begin
  Result := inherited GetExplicitAccessArray;
end;

function TJwSAccessControlList.GetMandatoryLabel : TJwSystemMandatoryAccessControlEntry;
var i : Integer;
begin
  result := nil;
  for i := 0 to Count-1 do
  begin
    if (Items[i].AceType = actMandatory) and
       (TJwSecurityAccessControlEntry(Items[i]) is TJwSystemMandatoryAccessControlEntry) then
    begin
      result := TJwSystemMandatoryAccessControlEntry(Items[i]);
      break;
    end;
  end;
end;

procedure TJwSAccessControlList.SetMandatoryLabel(
      const NewLabel : TJwSystemMandatoryAccessControlEntry;
      const CopyFlag: TJwCopyFlag);
{this const declaration prevents changing the type TJwCopyFlag
without notice of this implementation. The compiler will
show an error and source code must be adapted.
This code will be removed by the optimizer
}
const CopyFlagCheck : array[TJwCopyFlag] of byte = (0,1);
begin
  if (CopyFlag = cfCopyInstance) then
    SetMandatoryLabelEx(NewLabel)
  else
  begin
    SetMandatoryLabelEx(nil); //remove old label
    if Assigned(NewLabel) then
      Add(NewLabel);
  end;
end;

procedure TJwSAccessControlList.SetMandatoryLabelEx(
  const NewLabel : TJwSystemMandatoryAccessControlEntry);
var i : Integer;
    p : TJwSystemMandatoryAccessControlEntry;
begin
  for i := 0 to Count-1 do
  begin
    if (Items[i].AceType = actMandatory) and
       (TJwSecurityAccessControlEntry(Items[i]) is TJwSystemMandatoryAccessControlEntry) then
    begin
      //p := Items[i];
      Remove(Items[i]);
      break;
    end;
  end;

  if Assigned(NewLabel) then 
    Add(TJwSystemMandatoryAccessControlEntry.Create(
        TJwSecurityAccessControlEntry(NewLabel)));
end;

function TJwSAccessControlList.HasMandatoryLabel : boolean;
begin
  result := GetMandatoryLabel <> nil;
end;



constructor TJwSecurityAccessControlList.Create(AclPointerList: PACL);

var
  s: ACL_SIZE_INFORMATION;
  aPACE: PACE;
  i: integer;

begin
  Self.Create(True);

  if (AclPointerList = nil) then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsACLClassNilACLPointer,
      'Create(ownACEs: Boolean; aAPCL: PACL)', ClassName, RsUNAcl, 0, False, []);

  if not IsValidAcl(AclPointerList) then
    raise EJwsclWinCallFailedException.CreateFmtEx(RsACLClassInvalidACL,
      'Create(ownACEs: Boolean; aAPCL: PACL)', ClassName, RsUNAcl, 0, True, []);


  {if not GetAclInformation(aAPCL, @s, sizeof(s),AclSizeInformation) then
    raise EJwsclWinCallFailedException.CreateFmtEx('Call to GetAclInformation failed.', 'Create(ownACEs: Boolean; aAPCL: PACL)',ClassName,'JwsclAcl.pas', 0,True,[]);}

  fRevision := AclPointerList.AclRevision; //fastest way
  s.AceCount := AclPointerList.AceCount;

  for i := 0 to s.AceCount - 1 do
  begin
    aPACE := nil;

    if GetAce(AclPointerList, i, Pointer(aPACE)) then
    begin
      //keep order of original ACL
      Self.Insert(i, TJwSecurityAccessControlEntry.CreateACE(
        PAccessAllowedAce(aPACE)));
    end
    else
      raise EJwsclWinCallFailedException.CreateFmtEx(
        RsACLClassGetAceFailed,
        'Create(ownACEs: Boolean; aAPCL: PACL)',
        ClassName, RsUNAcl, 0, True, [i]);
  end;
end;



constructor TJwSecurityAccessControlList.Create(const Accesses : TJwExplicitAccessArray);
var ppACL : PACL;
begin
  JwRaiseOnNilParameter(
    Accesses, 'Accessess', 'Create', ClassName, RsUNAcl);

  if SetEntriesInAcl(1, @Accesses[0], nil, ppACL) <> ERROR_SUCCESS then
     raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed,
        'Create',
        ClassName, RsUNAcl, 0, True, []);

  if ppACL = nil then
    Create
  else
    try
      Create(ppACL);
    finally
      LocalFree(Cardinal(ppACL));
    end;
end;


procedure TJwSecurityAccessControlList.RemoveOwners;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].fListOwner = Self then
      Items[i].fListOwner := nil;
  end;
end;

procedure TJwSecurityAccessControlList.Clear;
begin
  RemoveOwners;
  inherited;
end;

destructor TJwSecurityAccessControlList.Destroy;
begin
  RemoveOwners;
  inherited;

end;

procedure TJwSecurityAccessControlList.Assign(AclInstance:
  TJwSecurityAccessControlList);
var
  i: integer;
  ACE: TJwSecurityAccessControlEntry;
begin
  if not Assigned(AclInstance) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsACLClassNilParameter, 'Assign', ClassName, RsUNAcl,
      0, False, ['AclInstance']);

  Clear;
  fRevision := AclInstance.fRevision;

  for i := 0 to AclInstance.Count - 1 do
  begin
    ACE := TJwSecurityAccessControlEntry.CreateACE(AclInstance.Items[i].AceType);
    ACE.Assign(AclInstance.Items[i]);

    try
      Insert(i,ACE); //keep order of original ACL
    except
      //free the ACE it will not be published
      ACE.Free;
      raise;
    end;
  end;
end;


procedure TJwSecurityAccessControlList.AddACEs(
  AclInstance: TJwSecurityAccessControlList; KeepOriginalOrder : Boolean = true);
var
  i: integer;
  E: TJwSecurityAccessControlEntry;
begin
  if not Assigned(AclInstance) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsACLClassNilParameter, 'Assign', ClassName, RsUNAcl,
      0, False, ['AclInstance']);

  if (Self.ClassType <> AclInstance.ClassType) then
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsACLClassClassMismatch,
      'AddACEs', ClassName, RsUNAcl, 0, False, [Self.ClassName,
      AclInstance.ClassName]);

  for i := 0 to AclInstance.Count - 1 do
  begin
    E := TJwSecurityAccessControlEntry.CreateACE(AclInstance[i].AceType);
    E.Assign(AclInstance[i]);

    //keep order of original ACL
    if KeepOriginalOrder then
      Self.Insert(i, E)
    else
      Self.Add(E);
  end;
end;



function TJwSecurityAccessControlList.Create_PACL: PACL;


  function AddAceToList(ACL : PACL;
    ACE : TJwSecurityAccessControlEntry;
    const Size : Cardinal) : Boolean;

  var ppACE : PACE;
      AceSize : Cardinal;
//      p : Pointer;
//      s : AnsiString;
  begin
    //result := RtlValidAcl(Acl);
    //result := RtlFirstFreeAce( Acl, p);
    {Warning:
      AddAce cannot add object ACEs properly so we use the special functions here
    }
    case ACE.AceType of
      actAuditObject :
       result := AddAuditAccessObjectAce(ACL, ACE.Revision,
          ACE.ObjectFlags, ACE.AccessMask, @ACE.ObjectType, @ACE.InheritedObjectType, ACE.SID.Sid,
          (ACE as TJwAuditAccessControlEntry).AuditSuccess,
          (ACE as TJwAuditAccessControlEntry).AuditFailure);


     actAllowObject :
        result := AddAccessAllowedObjectAce(ACL, ACE.Revision,
            ACE.ObjectFlags, ACE.AccessMask, @ACE.ObjectType, @ACE.InheritedObjectType, ACE.SID.Sid);

     actDenyObject  :
        result := AddAccessDeniedObjectAce(ACL,ACE.Revision,
            ACE.ObjectFlags, ACE.AccessMask, @ACE.ObjectType, @ACE.InheritedObjectType, ACE.SID.Sid);
    else
      ppACE := ACE.CreateDynamicACE(AceSize);


      result := AddAce(
          ACL,//__inout  PACL pAcl,
          ACE.Revision,//__in     DWORD dwAceRevision,
          MAXDWORD,//__in     DWORD dwStartingAceIndex,
          ppACE,//__in     LPVOID pAceList,
          AceSize//__in     DWORD nAceListLength
        );

      GlobalFree(Cardinal(ppACE));
//      s := ACe.ClassName;
    end;
  end;


var
  i: integer;

  s : AnsiString;

  aAudit:  TJwAuditAccessControlEntry;
  Mandatory : TJwSystemMandatoryAccessControlEntry;
  bResult: boolean;

  iSize: Cardinal;
begin
  RaiseOnInvalidRevision(fRevision, 'Create_PACL', ClassName);

  for i := 0 to Count - 1 do
  begin
    if not Assigned(Items[i].SID) or
      (Assigned(Items[i].SID) and (Items[i].SID.SID = nil)) then
      raise EJwsclInvalidSIDException.CreateFmtEx(
        RsACLClassNilSid,
        'Create_PACL', ClassName, RsUNAcl, 0, True, [i]);
  end;

  iSize := sizeof(TACL); //header size for ACL

  for i := 0 to Count - 1 do
  begin
    Inc(iSize, Items[i].GetDynamicTypeSize); //get size of ACE structure

    if Assigned(Items[i].SID) and (Items[i].SID.SID <> nil) then
      try
        Inc(iSize, Items[i].SID.SIDLength); //can throw exception!

      except
        on E: EJwsclSecurityException do
        begin
          raise EJwsclInvalidSIDException.CreateFmtEx(
            RsACLClassNilSid, 'Create_PACL', ClassName, RsUNAcl,
            0, True, [i]);
        end;
      end;
  end;

  //iSize := 1000;
  Result := PACL(GlobalAlloc(GMEM_FIXED or GMEM_ZEROINIT, iSize));

  if Result = nil then
    raise EJwsclNotEnoughMemory.CreateFmtEx(
      RsACLClassNewAclNotEnoughMemory,
      'Create_PACL', ClassName, RsUNAcl, 0, True, []);



  if not InitializeAcl(Result, iSize, fRevision) then
  begin
    GlobalFree(HRESULT(Result));

    raise EJwsclWinCallFailedException.CreateFmtEx(
          RsWinCallFailed,
           'Create_PACL', ClassName, RsUNAcl, 0, true,
           ['InitializeAcl']);
  end;

  for i := 0 to Count - 1 do
  begin
    if Assigned(Items[i].SID) and (Items[i].SID.SID <> nil) then
    begin
      //bResult := AddAccessAllowedAce(result, Items[i].Revision, Items[i].AccessMask, Items[i].SID.Sid);
      bResult := AddAceToList(Result, Items[i], iSize);

      if not bResult then
      begin
        GlobalFree(HRESULT(Result));

        raise EJwsclFailedAddACE.CreateFmtEx(
          RsACLClassAddXAccessAceFailed,
          'Create_PACL', ClassName, RsUNAcl, 0, True, [i]);
      end;
    end;

  end;
end;

class procedure TJwSecurityAccessControlList.Free_PACL(var AclPointerList: PACL);
begin
  if AclPointerList = nil then
    Exit;

  GlobalFree(Cardinal(AclPointerList));

  AclPointerList := nil;
end;


function TJwSecurityAccessControlList.GetItem(idx: integer):
TJwSecurityAccessControlEntry;
begin
  Result := inherited Get(idx);
end;

function TJwSecurityAccessControlList.GetEffectiveRights(const User :
       {$IFDEF UNICODE}TTrusteeW{$ELSE}TTrusteeA{$ENDIF}) : TJwAccessMask;
var ACL : PACL;
    i : Integer;
begin
  result := 0;

  ACL := Self.Create_PACL;
  try
    if {$IFDEF UNICODE}GetEffectiveRightsFromAclW{$ELSE}GetEffectiveRightsFromAclA{$ENDIF}
      (ACL,//__in   PACL pacl,
      @User,//__in   PTRUSTEE pTrustee,
      result//__out  PACCESS_MASK pAccessRights
      ) <> ERROR_SUCCESS then
    begin
       raise EJwsclWinCallFailedException.CreateFmtEx(
          RsWinCallFailed,
           'GetEffectiveRights', ClassName, RsUNAcl, 0, true,
           ['GetEffectiveRightsFromAcl']);
    end;
  finally
    Free_PACL(ACL);
  end;
end;

function TJwSecurityAccessControlList.GetEffectiveRights
  (const User : TJwSecurityId) : TJwAccessMask;
var Trust : {$IFDEF UNICODE}TTrusteeW{$ELSE}TTrusteeA{$ENDIF};
    Sid : PSID;
begin
  ZeroMemory(@Trust, sizeof(Trust));

  if not Assigned(User) then
    {$IFDEF UNICODE}BuildTrusteeWithNameW{$ELSE}BuildTrusteeWithNameA{$ENDIF}
     (@Trust, 'CURRENT_USER')
  else
  {$IFDEF UNICODE}BuildTrusteeWithSidW{$ELSE}BuildTrusteeWithSidA{$ENDIF}
   (@Trust, User.Sid);

  result := GetEffectiveRights(Trust);
end;

function TJwSecurityAccessControlList.Add(AccessEntry:
  TJwSecurityAccessControlEntry): integer;

procedure UpdateACLRevision;
begin
  //update ACL revision to the newer ACE revision
  //ACE structure does not change during revisions
  //so it is safe
  if AccessEntry.Revision > Self.fRevision then
    Self.fRevision := AccessEntry.Revision;
end;

var
  i:  integer;
  bInserted: boolean;
  s, s2: AnsiString;
  b1,
  //[Hint] b2,
  b3: boolean;
begin
  s := ClassName;

  if not Assigned(AccessEntry) then
    raise EJwsclNILParameterException.CreateFmtEx(RsACLClassNilParameter,
      'Add', ClassName, RsUNAcl, 0, False, ['AObject']);

  i := IndexOf(AccessEntry);
  if (i >= 0) or (Assigned(AccessEntry.ListOwner)) then

    raise EJwsclDuplicateListEntryException.CreateFmtEx(
      RsACLClassAceAlreadyInList, 'Add', ClassName, RsUNAcl,
      0, False, []);

  if ((Self is TJwDAccessControlList) and not
    (AccessEntry is TJwDiscretionaryAccessControlEntry)) then
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsACLClassInvalidAceInDacl,
      'Add', ClassName, RsUNAcl, 0, False, [AccessEntry.ClassName]);

  s2 := AccessEntry.ClassName;
  b1 := (Self is TJwSAccessControlList);



  //  b2 := (Self is TJwSecurityAccessControlList);
  //[Hint] b2 := True;

  b3 := (AccessEntry is TJwAuditAccessControlEntry) or
        (AccessEntry is TJwSystemMandatoryAccessControlEntry);
  {If this list is a SACL the entry must be an audit ace or
   a mandatory ace.}
  if ((b1) and (not b3)) then
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsACLClassInvaldAceInSacl,
      'Add', ClassName, RsUNAcl, 0, False, []);


  if (Self is TJwSAccessControlList) then
  begin
    AccessEntry.fListOwner := Self;
    Result := inherited Add(AccessEntry);
    //do not call our Self Add -> otherwise we get a recursion

    UpdateACLRevision;
    Exit;
  end;

  Result := -1;

  if (Self is TJwDAccessControlList) and
    ((AccessEntry is TJwDiscretionaryAccessControlEntryDeny) or
     (AccessEntry is TJwDiscretionaryAccessControlEntryCallbackDeny) or
     (AccessEntry is TJwDiscretionaryAccessControlEntryCallbackObjectDeny)
     )
    then
  begin
    if not (afInheritedAce in AccessEntry.Flags) then
    begin
      //a safe way to add a deny ACE is to add it directly at the beginning of list
      inherited Insert(0, AccessEntry);
      //do not call our Self Insert -> otherwise we get a recursion
      AccessEntry.fListOwner := Self;
      Result := 0;

      UpdateACLRevision;
      Exit;
    end;

    bInserted := False;
    for i := 0 to Count - 1 do
    begin
      if (afInheritedAce in Items[i].Flags) then
      begin
        inherited Insert(i, AccessEntry);
        //do not call our Self Insert -> otherwise we get a recursion
        Result := i;
        bInserted := True;
        AccessEntry.fListOwner := Self;

        UpdateACLRevision;
        break;
      end;
    end;

    if not bInserted then
    begin
      AccessEntry.fListOwner := Self;

      Result := inherited Add(AccessEntry);
      //do not call our Self Add -> otherwise we get a recursion

      UpdateACLRevision;
    end;

    Exit;
  end;

  if (Self is TJwDAccessControlList) and
    ((AccessEntry is TJwDiscretionaryAccessControlEntryAllow) or
     (AccessEntry is TJwDiscretionaryAccessControlEntryCallbackAllow) or
     (AccessEntry is TJwDiscretionaryAccessControlEntryCallbackObjectAllow)
     )
    then
  begin
    if (afInheritedAce in AccessEntry.Flags) then
    begin
      Result := inherited Add(AccessEntry);
      //do not call our Self Add -> otherwise we get a recursion
      AccessEntry.fListOwner := Self;

      UpdateACLRevision;
      Exit;
    end;

    bInserted := False;
    for i := 0 to Count - 1 do
    begin
      if (afInheritedAce in Items[i].Flags) then
      begin
        inherited Insert(i, AccessEntry);
        //do not call our Self Insert -> otherwise we get a recursion
        Result := i;
        //[Hint] bInserted := True;
        AccessEntry.fListOwner := Self;

        UpdateACLRevision;
        Exit;
      end;
    end;

    if not bInserted then
    begin
      AccessEntry.fListOwner := Self;
      Result := inherited Add(AccessEntry);
      //do not call our Self Add -> otherwise we get a recursion

      UpdateACLRevision;
    end;

    Exit;
  end;


  if (Result = -1) then
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsACLClassInvalidAce,
      'Add', ClassName, RsUNAcl, 0, False, []);

 (*
  if (Self is TJwDAccessControlList) and (AObject is TJwDiscretionaryAccessControlEntryDeny) then
  begin
    //a safe way to add a deny ACE is to add it directly at the beginning of list
    inherited Insert(0,AObject); //do not call our Self Insert -> otherwise we get a recursion
    Result := 0;
  end
  else
  if (Self is TJwDAccessControlList) and (AObject is TJwDiscretionaryAccessControlEntryAllow) then
  begin
    bInserted := False;
    for i := 0 to Count -1 do
    begin
      if (Items[i] is TJwDiscretionaryAccessControlEntryAllow) and
        (afInheritedAce in Items[i].Flags) then
      begin
        inherited Insert(i,aObject); //do not call our Self Insert -> otherwise we get a recursion
        Result := i;
        bInserted := True;
      end;
    end;
    if not bInserted then
      Result := inherited Add(aObject);//do not call our Self Add -> otherwise we get a recursion
  end
  else
    Result := inherited Add(AObject); //do not call our Self Add -> otherwise we get a recursion
   *)

end;

function TJwSecurityAccessControlList.GetText: TJwString;
begin
  Result := GetTextMap(TJwSecurityGenericMapping);
end;

procedure TJwSecurityAccessControlList.SetRevision(const Revision : Cardinal);
begin
  RaiseOnInvalidRevision(Revision, 'SetRevision', ClassName);
  fRevision := Revision;
end;


function TJwSecurityAccessControlList.GetTextMap(
  const Mapping: TJwSecurityGenericMappingClass): TJwString;
var
  i: integer;
  anACE: TJwSecurityAccessControlEntry;
begin
  Result := '';

  if Self is TJwDAccessControlList then
    Result := RsACLClassDaclName;

  if Self is TJwSAccessControlList then
    Result := RsACLClassSaclName;

  Result := JwFormatString(RsACLClassAceCount,[Count]);


  for i := 0 to Count - 1 do
  begin
    anACE := Items[i];

    Result := Result + '#' + IntToStr(i) + #13#10 +
      anACE.GetTextMap(Mapping) + #13#10;
  end;
end;



procedure TJwSecurityAccessControlList.ConvertInheritedToExplicit;
var
  i: integer;
  x: TJwAceFlags;
begin
  for i := 0 to Count - 1 do
  begin
    x := Items[i].Flags;
    Exclude(x, afInheritedAce);
    Items[i].Flags := x;
  end;
end;

procedure TJwSecurityAccessControlList.RemoveExplicits;
var
  i: integer;
begin
  for i := Count - 1 downto 0 do
  begin
    if not (afInheritedAce in Items[i].Flags) then
      Remove(i);
  end;
end;

procedure TJwSecurityAccessControlList.RemoveInherited;
var
  i: integer;
begin
  for i := Count - 1 downto 0 do
  begin
    if afInheritedAce in Items[i].Flags then
      Remove(i);
  end;
end;


function TJwSecurityAccessControlList.First: TJwSecurityAccessControlEntry;
begin
  if Count = 0 then
    Result := nil
  else
    Result := TJwSecurityAccessControlEntry(inherited First);
end;

function TJwSecurityAccessControlList.IndexOf(AccessEntry:
  TJwSecurityAccessControlEntry): integer;
begin
  Result := inherited IndexOf(AccessEntry);
end;

procedure TJwSecurityAccessControlList.Insert(Index: integer;
  AccessEntry: TJwSecurityAccessControlEntry);
begin
  if not Assigned(AccessEntry) then
    raise EJwsclNILParameterException.CreateFmtEx(RsACLClassNilParameter,
      'Insert', ClassName, RsUNAcl, 0, False, ['AccessEntry']);

  if (IndexOf(AccessEntry) >= 0) or (Assigned(AccessEntry.ListOwner)) then
    raise EJwsclDuplicateListEntryException.CreateFmtEx(
      RsACLClassAceAlreadyInList, 'Insert', ClassName, RsUNAcl,
      0, False, []);

  if ((Self is TJwDAccessControlList) and not
    (AccessEntry is TJwDiscretionaryAccessControlEntry)
    ) then
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsACLClassInvalidAceInDacl,
      'Add', ClassName, RsUNAcl, 0, False, [AccessEntry.ClassName]);

  if ((Self is TJwSAccessControlList) and not
    (
     (AccessEntry is TJwAuditAccessControlEntry) or
     (AccessEntry is TJwAuditAccessControlEntryObject) or
     (AccessEntry is TJwAuditAccessControlEntryCallback) or
     (AccessEntry is TJwAuditAccessControlEntryCallbackObject) or
     (AccessEntry is TJwSystemMandatoryAccessControlEntry)
    )) then
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsACLClassInvaldAceInSacl,
      'Add', ClassName, RsUNAcl, 0, False, []);


  inherited Insert(Index, AccessEntry);

  AccessEntry.fListOwner := Self;

  //update ACL revision to the newer ACE revision
  //ACE structure does not change during revisions
  //so it is safe
  if AccessEntry.Revision > Self.fRevision then
    Self.fRevision := AccessEntry.Revision;

end;


function TJwSecurityAccessControlList.FindEqualACE(
  const AccessEntry: TJwSecurityAccessControlEntry; EqualAceTypeSet: TJwEqualAceTypeSet;
  const StartIndex: integer = -1): integer;
var
  i: integer;
  ACEi: TJwSecurityAccessControlEntry;
  B: boolean;
begin
  Result := -1;
  for i := StartIndex + 1 to Count - 1 do
  begin
    try
      ACEi := GetItem(i);
    except
      raise;
    end;

    B := True;

    if (eactSameSid in EqualAceTypeSet) then
      B := B and ACEi.SID.EqualSid(AccessEntry.SID);

    if (eactSameFlags in EqualAceTypeSet) then
      B := B and (ACEi.Flags = AccessEntry.Flags);

    if (eactSameAccessMask in EqualAceTypeSet) then
      B := B and (ACEi.AccessMask = AccessEntry.AccessMask);

    if (eactSameType in EqualAceTypeSet) then
      B := B and (ACEi.AceType = AccessEntry.AceType);


    if B then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TJwSecurityAccessControlList.FindSID(const SidInstance: TJwSecurityId;
  const StartIndex: integer = -1): integer;
var
  i: integer;
  ACE: TJwSecurityAccessControlEntry;
begin
  Result := -1;
  for i := StartIndex + 1 to Count - 1 do
  begin
    try
      ACE := GetItem(i);
      if Assigned(ACE) and ACE.SID.EqualSid(SidInstance) then
      begin
        Result := i;
        Exit;
      end;

    except
      on E: EListError do
        Exit; //exit with return -1
    end;
  end;
end;

function TJwSecurityAccessControlList.Last: TJwSecurityAccessControlEntry;
begin
  if Count = 0 then
    Result := nil
  else
    Result := TJwSecurityAccessControlEntry(inherited Last);
end;

function TJwSecurityAccessControlList.Remove(Index: integer): integer;
var
  P: TJwSecurityAccessControlEntry;
begin
  P := GetItem(Index);

  Result := Remove(P);
end;

function TJwSecurityAccessControlList.Remove(AccessEntry:
  TJwSecurityAccessControlEntry): integer;
var
  idx: integer;
  b: boolean;
begin
  if not Assigned(AccessEntry) then
    raise EJwsclNILParameterException.CreateFmtEx(RsACLClassNilParameter,
      'Insert', ClassName, RsUNAcl, 0, False, ['AObject']);

  idx := IndexOf(AccessEntry);
  b := AccessEntry.ListOwner = Self;
  if (idx < 0) or not b then
  begin
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsACLClassAceNotInList, 'Remove', ClassName, RsUNAcl,
      0, False, []);
    Exit;
  end;

  //set owner to nil so it can be removed safely without an exception  
  AccessEntry.fListOwner := nil;

  Result := inherited Remove(AccessEntry);
end;


function TJwSecurityAccessControlList.GetExplicitAccessArray:
TJwExplicitAccessArray;
var
  i: integer;
  iErr, iCount: Cardinal;
  pExpAccess: PEXPLICIT_ACCESS;
  pSACL: PACL;
  arr: TJwExplicitAccessArray;

begin
  Result := nil;
  arr := nil;

  pSACL := Create_PACL;

  try
    pExpAccess := nil;
    iCount := 0;
    iErr := GetExplicitEntriesFromAcl(jwaWindows_PACL(pSACL),
      iCount, pExpAccess);
    if iErr <> ERROR_SUCCESS then
      raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailedWithNTStatus,
         'GetExplicitAccessArray', ClassName, RsUNAcl, 0, False,
         ['GetExplicitEntriesFromAcl',iErr]);

    if iCount > 0 then
    begin
      arr := TJwExplicitAccessArray(pExpAccess);

      SetLength(Result, iCount);
      for i := 0 to iCount - 1 do
      begin
        Result[i] := arr[i];
      end;
    end;

  finally
    Free_PACL(pSACL);
  end;

 { if pExpAccess <> nil then
    LocalFree(Cardinal(pExpAccess));
  }

end;


constructor TJwSecurityAccessControlEntry.Create(
  const aListOwner: TJwSecurityAccessControlList;
  const aFlags: TJwAceFlags;
  const anAccessMask: TJwAccessMask;
  const aSID: TJwSecurityId;
  ownSID: boolean = True);
begin
  JwRaiseOnNilParameter(aSid,'aSID','Create',ClassName,RsUNACL);
  
  fListOwner := nil;

  fFlags := aFlags;
  fAccessMask := anAccessMask;
  fSID := aSID;
  fRevision := ACL_REVISION;

  if Assigned(aSID) and (aSID.IsStandardSID) then
    ownSID := False;

  fownSID := ownSID;
  fIgnore := False;

  if Assigned(aListOwner) then
    aListOwner.Add(Self);

  fListOwner := aListOwner;

  ObjectType    := NULL_GUID;
  InheritedObjectType := NULL_GUID;
end;

constructor TJwSecurityAccessControlEntry.Create(
      const ListOwner: TJwSecurityAccessControlList;
      const Flags: TJwAceFlags;
      const AccessMask: TJwAccessMask;
      const SID: TJwSecurityId;
      const Revision : Cardinal;
      ownSID: boolean = True);
begin
  Self.Create(ListOwner, Flags, AccessMask, SID, ownSID);
  fRevision := Revision;
end;

constructor TJwSecurityAccessControlEntry.Create(
  const AccessEntry: TJwSecurityAccessControlEntry);
begin
  inherited Create;
  Assign(AccessEntry);
  ownSid := True;

  ObjectType    := NULL_GUID;
  InheritedObjectType := NULL_GUID;
end;


procedure TJwSecurityAccessControlEntry.Assign(AccessEntry:
  TJwSecurityAccessControlEntry);
var
  S: TJwSecurityId;
begin
  JwRaiseOnNilParameter(AccessEntry,'AccessEntry','Assign',ClassName,RsUNACL);
  JwRaiseOnNilParameter(AccessEntry.SID,'AccessEntry.SID','Assign',ClassName,RsUNACL);

  //free SID automatically
  OwnSID := True;

  //make a copy of SID for our entry
  if Assigned(AccessEntry.SID) then
  begin
    if (AccessEntry.SID.IsStandardSID) then
    begin
      //do not duplicate a well known sid from unit JwsclKnownSid
      S := AccessEntry.SID;
      ownSID := False;
    end
    else
      S := TJwSecurityId.Create(AccessEntry.SID)
  end
  else
    S := nil;

  fSID := S;

  begin
    (*
    do not change fListOwner
    because this ace instance could already be added
    *)
    fFlags := AccessEntry.Flags;
    fAccessMask := AccessEntry.AccessMask;
    fHeader := AccessEntry.Header;
    fRevision := AccessEntry.fRevision;
    fObjectType := AccessEntry.fObjectType;
    fInheritedObjectType := AccessEntry.fInheritedObjectType;
  end;
end;


constructor TJwSecurityAccessControlEntry.Create;
begin
  fListOwner := nil;
  fFlags := [];
  fAccessMask := 0;
  fSID := nil;
  fownSID := False;
  fUserData := nil;
  fRevision := ACL_REVISION;
  ZeroMemory(@fHeader, sizeof(fHeader));

  ObjectType    := NULL_GUID;
  InheritedObjectType := NULL_GUID;
end;


destructor TJwSecurityAccessControlEntry.Destroy;
begin
  if Assigned(ListOwner) then
  begin
    raise EJwsclReadOnlyPropertyException.CreateFmtEx(
      RsACLClassAceRemovingDenied,
      'Destroy', ClassName, RsUNAcl, 0, False, []);
  end;

  if fownSID and Assigned(fSID)
    and not fSID.IsStandardSID then
    FreeAndNil(fSID);

  fUserData := nil;


  inherited;
end;

procedure TJwSecurityAccessControlEntry.Free;
var
  B: boolean;
begin
  B := False;
  if Assigned(ListOwner) then
  begin
    if ListOwner.OwnsObjects then
      Exit;
    try
      ListOwner.Remove(Self);
    except
      //not correct list
      on E: EJwsclInvalidACEException do
      begin
        ListOwner.OwnsObjects := B;
        raise;
      end;
    end;

  end;

  inherited;
end;

class function TJwSecurityAccessControlEntry.CreateACE(anAceType: TJwAceType):
TJwSecurityAccessControlEntry;
begin
  Result := nil;
  case anACEType of
    actAllow         : Result := TJwDiscretionaryAccessControlEntryAllow.Create;
    actAllowCallback :
      Result := TJwDiscretionaryAccessControlEntryCallbackAllow.Create;
    actAllowObject :
      Result := TJwDiscretionaryAccessControlEntryObjectAllow.Create;
    actAllowCallbackObject :
      Result := TJwDiscretionaryAccessControlEntryCallbackObjectAllow.Create;

    actDeny          : Result  := TJwDiscretionaryAccessControlEntryDeny.Create;
    actDenyCallback :
      Result := TJwDiscretionaryAccessControlEntryCallbackDeny.Create;
    actDenyObject :
      Result := TJwDiscretionaryAccessControlEntryObjectDeny.Create;
    actDenyCallbackObject :
      Result := TJwDiscretionaryAccessControlEntryCallbackObjectDeny.Create;

    actAudit : Result := TJwAuditAccessControlEntry.Create;
    actAuditCallback  :
      Result := TJwAuditAccessControlEntryCallback.Create;
    actAuditObject  :
      Result := TJwAuditAccessControlEntryObject.Create;
    actAuditCallbackObject  :
      Result := TJwAuditAccessControlEntryCallbackObject.Create;

    actMandatory     : Result := TJwSystemMandatoryAccessControlEntry.Create;
  else
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsUnsupportedACE, 'CreateACE', ClassName, RsUNAcl,
      0, False, []);
  end;
end;

class function TJwSecurityAccessControlEntry.
  GetClassAceType(const AceType: TJwAceType) : TJwSecurityAccessControlEntryClass;
begin
  case AceType of
    actAllow :
      result := TJwDiscretionaryAccessControlEntryAllow;
    actAllowCallback:
      Result := TJwDiscretionaryAccessControlEntryCallbackAllow;
    actAllowObject:
      Result := TJwDiscretionaryAccessControlEntryObjectAllow;
    actAllowCallbackObject :
      Result := TJwDiscretionaryAccessControlEntryCallbackObjectAllow;


    actDeny :
      Result := TJwDiscretionaryAccessControlEntryDeny;
    actDenyCallback :
      Result := TJwDiscretionaryAccessControlEntryCallbackDeny;
    actDenyObject:
      Result := TJwDiscretionaryAccessControlEntryObjectDeny;
    actDenyCallbackObject :
      Result := TJwDiscretionaryAccessControlEntryCallbackObjectDeny;

    actAudit :
      Result := TJwAuditAccessControlEntry;
    actAuditCallback :
      Result := TJwAuditAccessControlEntryCallback;
    actAuditObject:
      Result := TJwAuditAccessControlEntryObject;
    actAuditCallbackObject :
      Result := TJwAuditAccessControlEntryCallbackObject;


    actMandatory :
      Result := TJwSystemMandatoryAccessControlEntry;
  else
   raise EJwsclInvalidACEException.CreateFmtEx(
      RsUnsupportedACE, 'GetClassAceType', ClassName, RsUNAcl,
      0, False, []);
  end;
end;


constructor TJwSecurityAccessControlEntry.Create(
  const AccessEntryPointer: PAccessAllowedAce);

begin
  if not Assigned(AccessEntryPointer) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsACLClassNilParameter, 'Create', ClassName, RsUNAcl,
      0, False, ['accessACE']);

  if (Self.ClassType <>
    GetClassAceType(TJwEnumMap.ConvertAceType(AccessEntryPointer.Header.AceType))) then
   raise EJwsclInvalidAceMismatch.CreateFmtEx(
      RsACEMismatch, 'Create', ClassName, RsUNAcl,
      0, False, []);

  Self.Create;
  fFlags := TJwEnumMap.ConvertAceFlags(AccessEntryPointer.Header.AceFlags);
  fAccessMask := AccessEntryPointer.Mask;
  fRevision := ACL_REVISION;
  fHeader := AccessEntryPointer.Header;


  OwnSID := True;
  fSID := TJwSecurityId.Create(PSID(@AccessEntryPointer.SidStart));

  //get other data
  case AccessEntryPointer.Header.AceType of
    SYSTEM_AUDIT_OBJECT_ACE_TYPE,
    SYSTEM_AUDIT_CALLBACK_OBJECT_ACE_TYPE,
    ACCESS_ALLOWED_OBJECT_ACE_TYPE,
    ACCESS_ALLOWED_CALLBACK_OBJECT_ACE_TYPE,
    ACCESS_DENIED_CALLBACK_ACE_TYPE,
    ACCESS_DENIED_OBJECT_ACE_TYPE,
    ACCESS_DENIED_CALLBACK_OBJECT_ACE_TYPE :
      begin
        fObjectType :=
          PACCESS_ALLOWED_OBJECT_ACE(AccessEntryPointer).ObjectType;
        fInheritedObjectType :=
          PACCESS_ALLOWED_OBJECT_ACE(AccessEntryPointer).InheritedObjectType;
      end;
  end;

  fUserData := nil;
end;

constructor TJwSecurityAccessControlEntry.Create(
  const AccessEntryPointer: PAccessDeniedAce);
begin
  Self.Create(PAccessAllowedAce(AccessEntryPointer));
end;

class function TJwSecurityAccessControlEntry.CreateACE(
const  AccessEntryPointer: PAccessAllowedAce): TJwSecurityAccessControlEntry;
begin
  Result := nil;

  if not Assigned(AccessEntryPointer) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsACLClassNilParameter, 'CreateACE', ClassName, RsUNAcl,
      0, False, ['accessACE']);

  {if AccessEntryPointer.Header.AceType = ACCESS_ALLOWED_ACE_TYPE then
    Result := TJwDiscretionaryAccessControlEntryAllow.Create(AccessEntryPointer)
  else
  if AccessEntryPointer.Header.AceType = ACCESS_ALLOWED_CALLBACK_ACE_TYPE then
    result := TJwDiscretionaryAccessControlEntryCallbackAllow.Create(AccessEntryPointer)
  else
  if AccessEntryPointer.Header.AceType = ACCESS_DENIED_ACE_TYPE then
    Result := TJwDiscretionaryAccessControlEntryDeny.Create(AccessEntryPointer)
  else
  if AccessEntryPointer.Header.AceType = ACCESS_DENIED_CALLBACK_ACE_TYPE then
    result := TJwDiscretionaryAccessControlEntryCallbackDeny.Create(AccessEntryPointer)
  else
  if AccessEntryPointer.Header.AceType = SYSTEM_AUDIT_ACE_TYPE then
    Result := TJwAuditAccessControlEntry.Create(PSystemAuditAce(AccessEntryPointer))
  else
  if AccessEntryPointer.Header.AceType = SYSTEM_AUDIT_CALLBACK_ACE_TYPE then
    Result := TJwAuditAccessControlEntryCallback.Create(PSystemAuditAce(AccessEntryPointer))
  else
  if AccessEntryPointer.Header.AceType = SYSTEM_MANDATORY_LABEL_ACE_TYPE then
    Result := TJwSystemMandatoryAccessControlEntry.Create(
        (AccessEntryPointer))
  else
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsUnsupportedACE, 'CreateACE', ClassName, RsUNAcl,
      0, False, []);   }


  result := GetClassAceType(TJwEnumMap.ConvertAceType(AccessEntryPointer.Header.AceType))
       .Create(AccessEntryPointer);

end;

class function TJwSecurityAccessControlEntry.CreateACE(
const  AccessEntryPointer: PAccessDeniedAce): TJwSecurityAccessControlEntry;
begin
  Result := CreateACE(PAccessAllowedAce(AccessEntryPointer));
end;

function TJwSecurityAccessControlEntry.GetDynamicTypeSize : Cardinal;
begin
  result := 0;
  case GetAceType of
    actAudit         : result := sizeof(SYSTEM_AUDIT_ACE);
    actAuditCallback : result := sizeof(SYSTEM_AUDIT_CALLBACK_ACE);
    actAuditObject   : result := sizeof(SYSTEM_AUDIT_OBJECT_ACE);
    actAuditCallbackObject : result := sizeof(SYSTEM_AUDIT_CALLBACK_OBJECT_ACE);

    actMandatory : result := sizeof(SYSTEM_MANDATORY_LABEL_ACE);

    actAllow : result := sizeof(ACCESS_ALLOWED_ACE);
    actAllowCallback : result := sizeof(ACCESS_ALLOWED_CALLBACK_ACE);
    actAllowObject : result := sizeof(ACCESS_ALLOWED_OBJECT_ACE);
    actAllowCallbackObject : result := sizeof(ACCESS_ALLOWED_CALLBACK_OBJECT_ACE);

    actDeny : result := sizeof(ACCESS_DENIED_ACE);
    actDenyCallback : result := sizeof(ACCESS_DENIED_CALLBACK_ACE);
    actDenyObject : result := sizeof(ACCESS_DENIED_OBJECT_ACE);
    actDenyCallbackObject : result := sizeof(ACCESS_DENIED_CALLBACK_OBJECT_ACE);
  else
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsInvalidAceType,
      'GetDynamicTypeSize', ClassName, RsUNAcl, 0, False, []);
  end;
end;                                

function TJwSecurityAccessControlEntry.CreateDynamicACE(out Size : Cardinal) : Pointer;

 procedure SetSidStart(var SidStart : Cardinal);
 var aPSID: PSID;
 begin
   if Assigned(SID) then
   begin
     aPSID := SID.CreateCopyOfSID;
     if aPSID <> nil then
     begin
       CopyMemory(@SidStart, aPSID, SID.SIDLength);
       SID.FreeSID(aPSID);
     end;
   end;
 end;

 function CompareGUID(const G1, G2: TGUID): boolean;
 begin
   Result := CompareMem(@G1, @G2, Sizeof(TGUID));
 end;


var p1 : PACCESS_ALLOWED_CALLBACK_ACE;
    p2 : PSYSTEM_MANDATORY_LABEL_ACE;
    p3 : PACCESS_DENIED_OBJECT_ACE;
    p4 : PACCESS_ALLOWED_CALLBACK_OBJECT_ACE;

    AceType : TJwAceType;
{$IFDEF DEBUG}
type TB = array[0..27] of byte;
var
    Data : ^TB;
{$ENDIF DEBUG}

begin
  Size := GetDynamicTypeSize;

  if Assigned(SID) and (SID.SID <> nil) then
    Inc(Size, SID.SIDLength);

  //GetMem(result, Size);
  //ZeroMemory(result,Size);
  Result := Pointer(GlobalAlloc(GMEM_FIXED or GMEM_ZEROINIT, Size));


{$IFDEF DEBUG}
  Data := result;
{$ENDIF DEBUG}

  PACCESS_ALLOWED_ACE(result).Header.AceType
    := TJwEnumMap.ConvertAceType(Self.AceType);
  PACCESS_ALLOWED_ACE(result).Header.AceFlags
    := TJwEnumMap.ConvertAceFlags(Self.Flags);
  PACCESS_ALLOWED_ACE(result).Header.AceSize
    := Size;

  AceType := GetAceType;
  case AceType of
    actMandatory,

    actAudit,
    actAuditCallback,

    actAllow,
    actAllowCallback,

    actDeny,
    actDenyCallback:
    begin
      p1 := result;

      p1.Mask := AccessMask;
      SetSidStart(p1.SidStart);
    end;

    actAuditObject,
    actAuditCallbackObject,

    actAllowObject,
    actAllowCallbackObject,
    actDenyObject,
    actDenyCallbackObject:
    begin
      p3 := result;

      p3.Mask := AccessMask;

      p3.Flags := 0;
      if CompareGUID(NULL_GUID, Self.ObjectType) then
        p3.Flags := p3.Flags or ACE_OBJECT_TYPE_PRESENT;
      if CompareGUID(NULL_GUID, Self.InheritedObjectType) then
        p3.Flags := p3.Flags or ACE_INHERITED_OBJECT_TYPE_PRESENT;

      p3.ObjectType := Self.ObjectType;
      p3.InheritedObjectType := Self.InheritedObjectType;

      SetSidStart(p3.SidStart);
    end;
  end;
end;

function TJwSecurityAccessControlEntry.Create_AllowACE: PAccessAllowedAce;
var
  aPSID: PSID;
  Size : Cardinal;
begin
  if not Assigned(SID) then
    Result := PAccessAllowedAce(GlobalAlloc(GMEM_FIXED or
      GMEM_ZEROINIT, sizeof(TAccessAllowedAce)))
  else
    Result := PAccessAllowedAce(GlobalAlloc(GMEM_FIXED or
      GMEM_ZEROINIT, sizeof(TAccessAllowedAce) + SID.SIDLength));

  //only allow return structures that are compatible to result type
  if
  not
  (
  (Self is TJwDiscretionaryAccessControlEntryCallbackAllow) or
  (Self is TJwDiscretionaryAccessControlEntryAllow) or
  (Self is TJwDiscretionaryAccessControlEntryCallbackDeny) or
  (Self is TJwDiscretionaryAccessControlEntryDeny) or
  (Self is TJwAuditAccessControlEntryCallback) or
  (Self is TJwAuditAccessControlEntry) or
  (Self is TJwSystemMandatoryAccessControlEntry)
  ) then
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsUnsupportedACE, 'Create_XXXXXACE', ClassName, RsUNAcl,
      0, False, []);

  result := CreateDynamicACE(Size);

  (*
  Result.Header.AceFlags := TJwEnumMap.ConvertAceFlags(Flags);


  Result.Mask := AccessMask;
  Result.Header.AceSize := GlobalSize(Cardinal(Result));
  Result.SidStart := 0;


  if Assigned(SID) then
  begin
    aPSID := SID.CreateCopyOfSID;

    if aPSID <> nil then
    begin
      //mem := @Result.SidStart;
      //FillChar(mem^, SID.SIDLength, 8);
      CopyMemory(@Result.SidStart, aPSID, SID.SIDLength);
      //mem := @Result.SidStart;

      //if mem = nil then;

      SID.FreeSID(aPSID);
    end;
  end;    *)

end;



function TJwSecurityAccessControlEntry.Create_DenyACE: PAccessDeniedAce;
begin
  Result := PAccessDeniedAce(Create_AllowACE);
end;

procedure TJwSecurityAccessControlEntry.Free_PACE(var AccessEntryPointer: PAccessAllowedAce);
begin
  Free_PACE(PAccessDeniedAce(AccessEntryPointer));
end;

procedure TJwSecurityAccessControlEntry.Free_PACE(var AccessEntryPointer: PAccessDeniedAce);
begin
  if AccessEntryPointer <> nil then
    GlobalFree(Cardinal(AccessEntryPointer));
  AccessEntryPointer := nil;
end;

procedure TJwSecurityAccessControlEntry.CheckReadOnly;
begin
  if Assigned(fListOwner) then
    raise EJwsclReadOnlyPropertyException.CreateFmtEx(RsACLClassPropertyReadOnly,
      'SetListOwner', ClassName, RsUNAcl, 0, False, []);
end;

procedure TJwSecurityAccessControlEntry.SetListOwner(ListOwner:
  TJwSecurityAccessControlList);
begin
  CheckReadOnly;

  if Assigned(ListOwner) then
  begin
    ListOwner.Add(Self);
  end;

  fListOwner := ListOwner;
end;

procedure TJwSecurityAccessControlEntry.SetFlags(FlagSet: TJwAceFlags);
begin
  fFlags := FlagSet;
end;

function TJwSecurityAccessControlEntry.GetAccessMask : TJwAccessMask;
begin
  result := fAccessMask;
end;

procedure TJwSecurityAccessControlEntry.SetAccessMask(
  anAccessMask: TJwAccessMask);
begin

  fAccessMask := anAccessMask;
end;

procedure TJwSecurityAccessControlEntry.SetSID(SidInstance: TJwSecurityId);
begin
  fSID := SidInstance;
end;




{ TJwDiscretionaryAccessControlEntryDeny }

constructor TJwDiscretionaryAccessControlEntryDeny.Create(
  const aListOwner: TJwSecurityAccessControlList; const aFlags: TJwAceFlags;
  const anAccessMask: TJwAccessMask; const aSID: TJwSecurityId; ownSID: boolean);
begin
  inherited Create(aListOwner, aFlags, anAccessMask, aSID, ownSID);
end;

constructor TJwDiscretionaryAccessControlEntryDeny.Create(
      const ListOwner: TJwSecurityAccessControlList;
      const Flags: TJwAceFlags;
      const AccessMask: TJwAccessMask;
      const SID: TJwSecurityId;
      const Revision : Cardinal;
      ownSID: boolean = True);
begin
  inherited Create(ListOwner, Flags, AccessMask, SID, Revision, ownSID);
end;

constructor TJwDiscretionaryAccessControlEntryDeny.Create(
  const AccessEntryPointer: PAccessDeniedAce);
begin
  inherited Create(AccessEntryPointer);
end;

constructor TJwDiscretionaryAccessControlEntryDeny.Create(
  AccessEntry: TJwDiscretionaryAccessControlEntryDeny);
begin
  inherited Create(AccessEntry);
end;

function TJwDiscretionaryAccessControlEntryDeny.Create_DenyACE:
PAccessDeniedAce;
begin
  Result := inherited Create_DenyACE;
end;

procedure TJwDiscretionaryAccessControlEntryDeny.Free_PACE(
  var AccessEntryPointer: PAccessDeniedAce);
begin
  inherited Free_PACE(AccessEntryPointer);
end;


{ TJwDiscretionaryAccessControlEntryAllow }

constructor TJwDiscretionaryAccessControlEntryAllow.Create(
  const aListOwner: TJwSecurityAccessControlList; const aFlags: TJwAceFlags;
  const anAccessMask: TJwAccessMask; const aSID: TJwSecurityId; ownSID: boolean);
begin
  inherited Create(aListOwner, aFlags, anAccessMask, aSID, ownSID);
end;

constructor TJwDiscretionaryAccessControlEntryAllow.Create(
      const ListOwner: TJwSecurityAccessControlList;
      const Flags: TJwAceFlags;
      const AccessMask: TJwAccessMask;
      const SID: TJwSecurityId;
      const Revision : Cardinal;
      ownSID: boolean = True);
begin
  Self.Create(ListOwner, Flags, AccessMask, SID, ownSID);
  fRevision := Revision;
end;

constructor TJwDiscretionaryAccessControlEntryAllow.Create(
  const AccessEntryPointer: PAccessAllowedAce);
begin
  inherited Create(AccessEntryPointer);
end;

constructor TJwDiscretionaryAccessControlEntryAllow.Create(
  const AccessEntry: TJwDiscretionaryAccessControlEntryAllow);
begin
  inherited Create(AccessEntry);
end;

function TJwDiscretionaryAccessControlEntryAllow.Create_AllowACE:
PAccessAllowedAce;
begin
  Result := inherited Create_AllowACE;
end;

procedure TJwDiscretionaryAccessControlEntryAllow.Free_PACE(
  var AccessEntryPointer: PAccessAllowedAce);
begin
  inherited Free_PACE(AccessEntryPointer);
end;

{ TJwAuditAccessControlEntry }

constructor TJwAuditAccessControlEntry.Create(
  const aListOwner: TJwSecurityAccessControlList; const aFlags: TJwAceFlags;
  const anAccessMask: TJwAccessMask; const aSID: TJwSecurityId; ownSID: boolean);
begin
  inherited Create(aListOwner, aFlags, anAccessMask, aSID, ownSID);
  AuditSuccess := (afSuccessfulAccessAceFlag in aFlags);
  AuditFailure := (afFailedAccessAceFlag in aFlags);
end;

constructor TJwAuditAccessControlEntry.Create(
  const aListOwner: TJwSecurityAccessControlList;
  aAuditSuccess, aAuditFailure: boolean; const anAccessMask: TJwAccessMask;
  const aSID: TJwSecurityId; ownSID: boolean);
begin
  inherited Create(aListOwner, [], anAccessMask, aSID, ownSID);
  AuditSuccess := aAuditSuccess;
  AuditFailure := aAuditFailure;
end;

constructor TJwAuditAccessControlEntry.Create(AccessEntry: TJwAuditAccessControlEntry);
begin
  inherited Create(AccessEntry);

  AuditSuccess := AccessEntry.AuditSuccess;
  AuditFailure := AccessEntry.AuditFailure;
end;

constructor TJwAuditAccessControlEntry.Create(
  const AccessEntryPointer: PSystemAuditAce);
begin
  inherited Create(PAccessAllowedAce(AccessEntryPointer));

  AuditSuccess := afSuccessfulAccessAceFlag in Flags;
  //Exclude(fFlags,afSuccessfulAccessAceFlag);

  AuditFailure := afFailedAccessAceFlag in Flags;
  //Exclude(fFlags,afFailedAccessAceFlag);
end;

constructor TJwAuditAccessControlEntry.Create;
begin
  inherited;

  fFlagsIgnored := [];
end;

function TJwAuditAccessControlEntry.Create_AuditACE: PSystemAuditAce;
var
  s1, s2, s3: TJwString;
begin
  Result := PSystemAuditAce(inherited Create_AllowACE);

  s1 := GetText;
  if s1 = '' then;

  //Result.Header.AceFlags := Result.Header.AceFlags and not SUCCESSFUL_ACCESS_ACE_FLAG;
  //Result.Header.AceFlags := Result.Header.AceFlags and not FAILED_ACCESS_ACE_FLAG;

  s2 := GetText;
  if s2 = '' then;

  if AuditSuccess then
    Result.Header.AceFlags :=
      Result.Header.AceFlags or SUCCESSFUL_ACCESS_ACE_FLAG;
  if AuditFailure then
    Result.Header.AceFlags :=
      Result.Header.AceFlags or FAILED_ACCESS_ACE_FLAG;

  s3 := GetText;
  if s3 = '' then;
end;

procedure TJwAuditAccessControlEntry.Free_PACE(var AccessEntryPointer: PSystemAuditAce);
begin

  inherited Free_PACE(PAccessAllowedAce(AccessEntryPointer));
end;


function TJwAuditAccessControlEntry.GetAccessMask : TJwAccessMask;
begin
  result := 0;
  if AuditSuccess then
    result := result or SUCCESSFUL_ACCESS_ACE_FLAG;
  if AuditFailure then
    result := result or FAILED_ACCESS_ACE_FLAG;
end;

procedure TJwAuditAccessControlEntry.SetAuditSuccess(const Value: boolean);
begin
  if Value then
    Include(fFlags, afSuccessfulAccessAceFlag)
  else
    Exclude(fFlags, afSuccessfulAccessAceFlag);

  AuditFailure := afFailedAccessAceFlag in Flags;
  //Exclude(fFlags,afFailedAccessAceFlag);
end;


procedure TJwAuditAccessControlEntry.SetAuditFailure(const Value: boolean);
begin
  if Value then
    Include(fFlags, afFailedAccessAceFlag)
  else
    Exclude(fFlags, afFailedAccessAceFlag);
end;

function TJwAuditAccessControlEntry.GetAuditSuccess: boolean;
begin
  Result := afSuccessfulAccessAceFlag in Flags;
end;

function TJwAuditAccessControlEntry.GetAuditFailure: boolean;
begin
  Result := afFailedAccessAceFlag in Flags;
end;

procedure TJwAuditAccessControlEntry.Assign(AccessEntry: TJwAuditAccessControlEntry);
begin
  inherited Assign(AccessEntry);
  AuditSuccess := AccessEntry.AuditSuccess;
  AuditFailure := AccessEntry.AuditFailure;
end;

function TJwSecurityAccessControlEntry.GetObjectFlags : Cardinal;
 function CompareGUID(const G1, G2: TGUID): boolean;
 begin
   Result := CompareMem(@G1, @G2, Sizeof(TGUID));
 end;
begin
  result := 0;
  if CompareGUID(NULL_GUID, Self.ObjectType) then
    result := result or ACE_OBJECT_TYPE_PRESENT;
  if CompareGUID(NULL_GUID, Self.InheritedObjectType) then
    result := result or ACE_INHERITED_OBJECT_TYPE_PRESENT;
end;

function TJwSecurityAccessControlEntry.GetOwnSid : Boolean;
begin
  if Assigned(SID) and (SID.IsStandardSID) then
    fOwnSid := False;

  result := fOwnSid;
end;

procedure TJwSecurityAccessControlEntry.SetOwnSid(const OwnSid: Boolean);
begin
  if Assigned(SID) and (SID.IsStandardSID) then
    fOwnSid := False
  else
    fOwnSid := OwnSid;
end;


function TJwSecurityAccessControlEntry.GetAceType: TJwAceType;
begin
  Result := actUnknown;

  {Order must be correct
    CallbackX
    CallbackObjectX
    X
  }

  if Self is TJwDiscretionaryAccessControlEntryCallbackAllow then
    Result := actAllowCallback
  else
  if Self is TJwDiscretionaryAccessControlEntryObjectAllow then
    Result := actAllowObject
  else
  if Self is TJwDiscretionaryAccessControlEntryCallbackObjectAllow then
    Result := actAllowCallbackObject
  else
  if Self is TJwDiscretionaryAccessControlEntryAllow then
    Result := actAllow
  else

  if Self is TJwDiscretionaryAccessControlEntryCallbackDeny then
    Result := actDenyCallback
  else
  if Self is TJwDiscretionaryAccessControlEntryObjectDeny then
    Result := actDenyObject
  else
  if Self is TJwDiscretionaryAccessControlEntryCallbackObjectDeny then
    Result := actDenyCallbackObject
  else
  if Self is TJwDiscretionaryAccessControlEntryDeny then
    Result := actDeny
  else

  if Self is TJwAuditAccessControlEntryCallback then
    Result := actAuditCallback
  else
  if Self is TJwAuditAccessControlEntryCallback  then
    Result := actAuditCallbackObject
  else
  if Self is TJwAuditAccessControlEntryObject  then
    Result := actAuditObject
  else
  if Self is TJwAuditAccessControlEntry then
    Result := actAudit
  else


  if Self is TJwSystemMandatoryAccessControlEntry then
    Result := actMandatory
  else
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsUnsupportedACE, 'GetAceType', ClassName, RsUNAcl,
      0, False, []);
end;




function TJwSecurityAccessControlEntry.GetExplicitAccess: TJwExplicitAccess;
begin
  if not Assigned(SID) or (ASsigned(SID) and
    (not IsValidSid(Sid.SID))) then
    raise EJwsclInvalidSIDException.CreateFmtEx(
      RsACLClassInvalidAceSid,
      'GetExplicitAccess', ClassName, RsUNAcl,
      0, False, []);

  Result.grfAccessPermissions := AccessMask;
  Result.grfInheritance := TJwEnumMap.ConvertAceFlags(Flags);
  //  Result.grfAccessMode := TAccessMode(Integer(ConvertAceFlagsToCardinal(Flags)));

  case GetAceType of
    actAllowCallback,
    actAllowObject,
    actAllowCallbackObject,
    actAllow: Result.grfAccessMode := GRANT_ACCESS;

    actDenyCallback,
    actDenyObject,
    actDenyCallbackObject,
    actDeny: Result.grfAccessMode  := DENY_ACCESS;


    actMandatory : Result.grfAccessMode := TAccessMode(AccessMask);

    actAudit,
    actAuditCallback,
    actAuditObject,
    actAuditCallbackObject:
    begin
      Result.grfAccessMode  := SET_ACCESS;
      Result.grfInheritance := 0;
      if TJwAuditAccessControlEntry(Self).AuditSuccess then
        Result.grfInheritance :=
          Result.grfInheritance or SUCCESSFUL_ACCESS_ACE_FLAG;
      if TJwAuditAccessControlEntry(Self).AuditFailure then
        Result.grfInheritance :=
          Result.grfInheritance or FAILED_ACCESS_ACE_FLAG;
    end;
  end;


  FillChar(Result.Trustee, sizeof(Result.Trustee), 0);
  Result.Trustee.MultipleTrusteeOperation := NO_MULTIPLE_TRUSTEE;
  Result.Trustee.TrusteeForm := TRUSTEE_IS_SID;
  Result.Trustee.TrusteeType := TRUSTEE_IS_UNKNOWN;
  Result.Trustee.ptstrName := TJwPChar(SID.SID);
end;

function TJwSecurityAccessControlEntry.GetText: TJwString;
begin
  Result := GetTextMap(TJwSecurityGenericMapping);
end;



function TJwSecurityAccessControlEntry.GetTextMap(
  const Mapping: TJwSecurityGenericMappingClass): TJwString;
var
  i: TJwAceFlag;
  SidText,
  FlagString : TJwString;
  sMap : TJwString;
begin
  FlagString := '';
  for i := low(TJwAceFlag) to high(TJwAceFlag) do
  begin
    if i in Flags then
      FlagString := FlagString + TJwAceFlagStrings[i]+',';
  end;
  System.Delete(FlagString, Length(FlagString), 1);



  if Assigned(Mapping) then
  begin
    //map generics to specific rights and then convert
    //all remaining specific rights to string
    sMap := Mapping.MapAccessMaskToString(Mapping.GenericMap(AccessMask))
  end
  else
    sMap := RsMapNoMapGiven + ' ' + IntToStr(Accessmask) +', 0x' + IntToHex(AccessMask,4);



  if not Assigned(SID) then
    SidText := RsBracketNil
  else
    SidText := Sid.GetText(True);

  result := JwFormatString(
    RsACLClassGetTextMap,
     [ClassName,
     TAceTypeString[AceType],
     FlagString,
     sMap,
     SidText
     ]);
end;

function TJwSecurityAccessControlList.IsEqual(
  const AccessControlListInstance: TJwSecurityAccessControlList;
  const EqualAceTypeSet: TJwEqualAceTypeSet = JwAllEqualAceTypes): boolean;
var
  i, iPos: integer;
  tempACL1, tempACL2: TJwSecurityAccessControlList;
begin
  Result := False;

  if not Assigned(AccessControlListInstance) then
    Exit;

  if Count <> AccessControlListInstance.Count then
    Exit;

  tempACL1 := TJwSecurityAccessControlList.Create;
  tempACL1.Assign(Self);

  if tempACL1 is TJwDAccessControlList then
   (tempACL1 as TJwDAccessControlList).MakeCanonical;

  tempACL2 := TJwSecurityAccessControlList.Create;
  tempACL2.Assign(AccessControlListInstance);

  if tempACL1 is TJwDAccessControlList then
    (tempACL2 as TJwDAccessControlList).MakeCanonical;

  for i := 0 to tempACL1.Count - 1 do
  begin
    iPos := tempACL1.FindEqualACE(tempACL2.Items[i], EqualAceTypeSet, i - 1);
    if iPos <> i then
      Exit;
  end;

  Result := True;
end;

function TJwSecurityAccessControlList.IsCanonical: boolean;
var //[Hint] hasDirectNegative,
  hasDirectPositive, hasInheritedNegative,
  hasInheritedPositive: boolean;
  i: integer;
begin
  Result := False;

  //[Hint] hasDirectNegative     := False;
  hasDirectPositive := False;
  hasInheritedNegative := False;
  hasInheritedPositive := False;

  if Count = 0 then
  begin
    Result := True;
    Exit;
  end;

  if Items[0].AceType = actAudit then
  begin
    Result := True;
    Exit;
  end;


  for i := 0 to Count - 1 do
  begin

    if afInheritedAce in Items[i].Flags then
    begin
      if Items[i].AceType = actAllow then
      begin
        //inherited allow
        hasInheritedPositive := True;
      end
      else
      begin
        //inherited deny
        if hasInheritedPositive then
          Exit;

        hasInheritedNegative := True;
      end;
    end
    else
    if Items[i].AceType = actAllow then
    begin
      //direct allow
      if hasInheritedNegative or hasInheritedPositive then
        Exit;

      hasDirectPositive := True;
    end
    else
    begin
      //direct deny
      if hasInheritedNegative or hasInheritedPositive or
        hasDirectPositive then
        Exit;

      //[Hint] hasDirectNegative := True;
    end;
  end;

  Result := True;
end;

procedure TJwDAccessControlList.MakeCanonical;
var
  ACL: TJwSecurityAccessControlList;

  i: integer;

  ACE : TJwSecurityAccessControlEntry;
  OldList : TJwSecurityAccessControlList;
  OldOwns : Boolean;
begin
  ACL := TJwDAccessControlList.Create;

  OldOwns := OwnsObjects;
  OwnsObjects := false;
  for i := Count - 1 downto 0 do
  begin
    ACE := Items[i];
    //detach from old list
    Remove(i);

    ACL.Add(ACE); //rearrange ACE in ACL
  end;
  OwnsObjects := OldOwns;

  Assign(ACL);
  ACL.Free;
end;


{ TJwSystemMandatoryAccessControlEntry }

constructor TJwSystemMandatoryAccessControlEntry.Create(
  const MandatoryLabel: PSystemMandatoryLabelAce);
begin
  inherited Create(TJwSecurityAccessControlEntry(MandatoryLabel));
end;
constructor TJwSystemMandatoryAccessControlEntry.Create(
  const MandatoryLabel: TJwSystemMandatoryAccessControlEntry);
begin
  inherited Create(TJwSecurityAccessControlEntry(MandatoryLabel));
end;

constructor TJwSystemMandatoryAccessControlEntry.Create(
  const MandatoryLevel : TMandatoryLevel;
  const ListOwner: TJwSAccessControlList = nil);
var Sid : TJwSecurityId;
    SidStr : TJwString;
begin
  case MandatoryLevel of
    MandatoryLevelUntrusted : SidStr := LowIL;
    MandatoryLevelLow       : SidStr := LowIL;
    MandatoryLevelMedium    : SidStr := MediumIL;
    MandatoryLevelHigh      : SidStr := HighIL;
    MandatoryLevelSystem    : SidStr := SystemIL;
    MandatoryLevelSecureProcess : SidStr := ProtectedProcessIL;
  else
    raise EJwsclInvalidMandatoryLevelException.CreateFmtEx(
      RsInvalidMandatoryLevelType,
      'Create', ClassName, RsUNAcl,
      0, False, []);
  end;
  Sid := TJwSecurityId.Create(SidStr);

  inherited Create(ListOwner,[],0,Sid, true);
end;

function TJwSystemMandatoryAccessControlEntry.Compare(
  const MandatoryLabel: TJwSystemMandatoryAccessControlEntry): Integer;
begin
  if not Assigned(MandatoryLabel) then
     raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter,
      'Compare', ClassName, RsUNAcl,
      0, False, ['MandatoryLabel']);
      
  result := Integer(Self.GetMandatoryLevelType) -
              Integer(MandatoryLabel.GetMandatoryLevelType);
end;

function TJwSystemMandatoryAccessControlEntry.GetMandatoryPolicy : TJwMandatoryPolicyFlagSet;
begin
  result := TJwEnumMap.ConvertMandatoryPolicyFlags(AccessMask);
end;

procedure TJwSystemMandatoryAccessControlEntry.
  SetMandatoryPolicy(const Level : TJwMandatoryPolicyFlagSet);
begin
  AccessMask := TJwEnumMap.ConvertMandatoryPolicyFlags(Level);
end;


function TJwSystemMandatoryAccessControlEntry.GetMandatoryLevelType: TMandatoryLevel;
begin
  if Sid.SubAuthorityCount = 0 then
    raise EJwsclInvalidMandatoryLevelException.CreateFmtEx(
      RsInvalidMandatoryLevelType,
      'GetMandatoryLevelType', ClassName, RsUNAcl,
      0, False, []);

  if Sid.SubAuthorityArray[0] = SECURITY_MANDATORY_UNTRUSTED_RID then
    result := MandatoryLevelUntrusted
  else
  if Sid.SubAuthorityArray[0] = SECURITY_MANDATORY_LOW_RID then
    result := MandatoryLevelLow
  else
  if Sid.SubAuthorityArray[0] = SECURITY_MANDATORY_MEDIUM_RID then
    result := MandatoryLevelMedium
  else
  if Sid.SubAuthorityArray[0] = SECURITY_MANDATORY_HIGH_RID then
    result := MandatoryLevelHigh
  else
  if Sid.SubAuthorityArray[0] = SECURITY_MANDATORY_SYSTEM_RID then
    result := MandatoryLevelSystem
  else
  if Sid.SubAuthorityArray[0] = SECURITY_MANDATORY_PROTECTED_PROCESS_RID then
   result := MandatoryLevelSecureProcess
  else
    raise EJwsclInvalidMandatoryLevelException.CreateFmtEx(
      RsInvalidMandatoryLevelType,
      'GetMandatoryLevelType', ClassName, RsUNAcl,
      0, False, []);
end;


{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}


end.
{$ENDIF SL_OMIT_SECTIONS}