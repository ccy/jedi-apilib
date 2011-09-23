{******************************************************************************}
{                                                                              }
{ Virtual Disk API interface Unit for Object Pascal                            }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2010 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ Portions created by Michael Schloeder are Copyright (C) 2010                 }
{ Michael Schloeder. All Rights Reserved.                                      }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project JEDI         }
{ APILIB home page, located at http://jedi-apilib.sourceforge.net              }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaVirtDisk;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

{$IFDEF JWA_WINDOWS}
  uses JwaWindows;
{$ELSE}
  uses JwaWinType, JwaWinBase, JwaWinNT;
{$ENDIF JWA_WINDOWS}

{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

const
  //Device type is unknown or not valid.
  VIRTUAL_STORAGE_TYPE_DEVICE_UNKNOWN = 0;
  {$EXTERNALSYM VIRTUAL_STORAGE_TYPE_DEVICE_UNKNOWN}
  //Internal use only. Not supported.
  VIRTUAL_STORAGE_TYPE_DEVICE_ISO = 1;
  {$EXTERNALSYM VIRTUAL_STORAGE_TYPE_DEVICE_ISO}
  //Virtual hard disk device type.
  VIRTUAL_STORAGE_TYPE_DEVICE_VHD = 2;
  {$EXTERNALSYM VIRTUAL_STORAGE_TYPE_DEVICE_VHD}

  VIRTUAL_STORAGE_TYPE_VENDOR_MICROSOFT: TGUID = (
    D1:$EC984AEC;
    D2:$A0F9;
    D3:$47E9;
    D4:($90, $1F, $71, $41, $5A, $66, $34, $5B);
  );
  {$EXTERNALSYM VIRTUAL_STORAGE_TYPE_VENDOR_MICROSOFT}
  VIRTUAL_STORAGE_TYPE_VENDOR_UNKNOWN  : TGUID = (
    D1:$00000000;
    D2:$0000;
    D3:$0000;
    D4:($00, $00, $00, $00, $00, $00, $00, $00);
  );
  {$EXTERNALSYM VIRTUAL_STORAGE_TYPE_VENDOR_UNKNOWN}

  CREATE_VIRTUAL_DISK_PARAMETERS_DEFAULT_SECTOR_SIZE = $200;
  {$EXTERNALSYM CREATE_VIRTUAL_DISK_PARAMETERS_DEFAULT_SECTOR_SIZE}
  CREATE_VIRTUAL_DISK_PARAMETERS_DEFAULT_BLOCK_SIZE = $0;
  {$EXTERNALSYM CREATE_VIRTUAL_DISK_PARAMETERS_DEFAULT_BLOCK_SIZE}

  OPEN_VIRTUAL_DISK_RW_DEPTH_DEFAULT = 1;
  {$EXTERNALSYM OPEN_VIRTUAL_DISK_RW_DEPTH_DEFAULT}

  GET_STORAGE_DEPENDENCY_FLAG_PARENTS = $00000001;
  {$EXTERNALSYM GET_STORAGE_DEPENDENCY_FLAG_PARENTS}

  // Versioned parameter structure for MergeVirtualDisk
  MERGE_VIRTUAL_DISK_DEFAULT_MERGE_DEPTH = 1;
  {$EXTERNALSYM MERGE_VIRTUAL_DISK_DEFAULT_MERGE_DEPTH}

type
  _VIRTUAL_STORAGE_TYPE = record
    DeviceId: ULONG;
    VendorId: TGUID;
  end;
  {$EXTERNALSYM _VIRTUAL_STORAGE_TYPE}
  VIRTUAL_STORAGE_TYPE = _VIRTUAL_STORAGE_TYPE;
  {$EXTERNALSYM VIRTUAL_STORAGE_TYPE}
  PVIRTUAL_STORAGE_TYPE = ^_VIRTUAL_STORAGE_TYPE;
  {$EXTERNALSYM PVIRTUAL_STORAGE_TYPE}
  TVirtualStorageType = VIRTUAL_STORAGE_TYPE;
  PVirtualStorageType = PVIRTUAL_STORAGE_TYPE;


  _VIRTUAL_DISK_ACCESS_MASK = (
    VIRTUAL_DISK_ACCESS_ATTACH_RO   = $10000,
    VIRTUAL_DISK_ACCESS_ATTACH_RW   = $20000,
    VIRTUAL_DISK_ACCESS_DETACH      = $40000,
    VIRTUAL_DISK_ACCESS_GET_INFO    = $80000,
    VIRTUAL_DISK_ACCESS_CREATE      = $100000,
    VIRTUAL_DISK_ACCESS_METAOPS     = $200000,
    VIRTUAL_DISK_ACCESS_READ        = $d0000,
    VIRTUAL_DISK_ACCESS_ALL         = $3f0000,
    VIRTUAL_DISK_ACCESS_WRITABLE    = $320000
  );
  {$EXTERNALSYM _VIRTUAL_DISK_ACCESS_MASK}
  VIRTUAL_DISK_ACCESS_MASK = _VIRTUAL_DISK_ACCESS_MASK;
  {$EXTERNALSYM VIRTUAL_DISK_ACCESS_MASK}
  PVIRTUAL_DISK_ACCESS_MASK = ^_VIRTUAL_DISK_ACCESS_MASK;
  {$EXTERNALSYM PVIRTUAL_DISK_ACCESS_MASK}
  TVirtualDiskAccessMask = VIRTUAL_DISK_ACCESS_MASK;
  PVirtualDiskAccessMask = PVIRTUAL_DISK_ACCESS_MASK;

///////////////////////////////////////////////////////////
///  OpenVirtualDisk
///

  _OPEN_VIRTUAL_DISK_FLAG = (
    //No flag specified.
    OPEN_VIRTUAL_DISK_FLAG_NONE         = $00000000,
    //Open the VHD file (backing store) without opening any
    //differencing-chain parents. Used to correct broken parent links.
    OPEN_VIRTUAL_DISK_FLAG_NO_PARENTS   = $00000001,
    //Reserved
    OPEN_VIRTUAL_DISK_FLAG_BLANK_FILE   = $00000002,
    //Reserved
    OPEN_VIRTUAL_DISK_FLAG_BOOT_DRIVE   = $00000004
  );
  {$EXTERNALSYM _OPEN_VIRTUAL_DISK_FLAG}
  OPEN_VIRTUAL_DISK_FLAG = _OPEN_VIRTUAL_DISK_FLAG;
  {$EXTERNALSYM OPEN_VIRTUAL_DISK_FLAG}
  POPEN_VIRTUAL_DISK_FLAG = ^_OPEN_VIRTUAL_DISK_FLAG;
  {$EXTERNALSYM POPEN_VIRTUAL_DISK_FLAG}
  TOpenVirtualDiskFlag = OPEN_VIRTUAL_DISK_FLAG;
  POpenVirtualDiskFlag = POPEN_VIRTUAL_DISK_FLAG;

  _OPEN_VIRTUAL_DISK_VERSION = (
    OPEN_VIRTUAL_DISK_VERSION_UNSPECIFIED   = 0,
    OPEN_VIRTUAL_DISK_VERSION_1             = 1
  );
  {$EXTERNALSYM _OPEN_VIRTUAL_DISK_VERSION}
  OPEN_VIRTUAL_DISK_VERSION = _OPEN_VIRTUAL_DISK_VERSION;
  {$EXTERNALSYM OPEN_VIRTUAL_DISK_VERSION}
  POPEN_VIRTUAL_DISK_VERSION = ^_OPEN_VIRTUAL_DISK_VERSION;
  {$EXTERNALSYM POPEN_VIRTUAL_DISK_VERSION}
  TOpenVirtualDiskVerion = OPEN_VIRTUAL_DISK_VERSION;
  POpenVirtualDiskVerion = POPEN_VIRTUAL_DISK_VERSION;

  __OPEN_VIRTUAL_DISK_VERSION1 = record
    RWDepth: ULong;
  end;
  TOpenVirtualDiskVerion1 = __OPEN_VIRTUAL_DISK_VERSION1;

  ///    RWDepth
  ///
  ///    0 = Do not open for read/write at any depth. This value
  ///    should be used for read-only operations.
  ///
  ///    OPEN_VIRTUAL_DISK_RW_DEPTH_DEFAULT = 1 = Default value to use
  ///    if no other value is desired.
  ///
  ///    n = user-defined = This integer value should be the number of
  ///    merge levels plus one, if a merge operation is intended.
  ///


  _OPEN_VIRTUAL_DISK_PARAMETERS =  record
    Version: OPEN_VIRTUAL_DISK_VERSION;
    case DWORD of
       0: (Version1: __OPEN_VIRTUAL_DISK_VERSION1);
  end;
  {$EXTERNALSYM _OPEN_VIRTUAL_DISK_PARAMETERS}
  OPEN_VIRTUAL_DISK_PARAMETERS = _OPEN_VIRTUAL_DISK_PARAMETERS;
  {$EXTERNALSYM OPEN_VIRTUAL_DISK_PARAMETERS}
  POPEN_VIRTUAL_DISK_PARAMETERS = ^_OPEN_VIRTUAL_DISK_PARAMETERS;
  {$EXTERNALSYM POPEN_VIRTUAL_DISK_PARAMETERS}
  TOpenVirtualDiskParameters = OPEN_VIRTUAL_DISK_PARAMETERS;
  POpenVirtualDiskParameters = POPEN_VIRTUAL_DISK_PARAMETERS;

//////////////////////////////////////////////////////////////////////////
///  AttachVirtualDisk
///

  _ATTACH_VIRTUAL_DISK_VERSION = (
    ATTACH_VIRTUAL_DISK_VERSION_UNSPECIFIED   = 0,
    ATTACH_VIRTUAL_DISK_VERSION_1             = 1
  );
  {$EXTERNALSYM _ATTACH_VIRTUAL_DISK_VERSION}
  ATTACH_VIRTUAL_DISK_VERSION = _ATTACH_VIRTUAL_DISK_VERSION;
  {$EXTERNALSYM ATTACH_VIRTUAL_DISK_VERSION}
  PATTACH_VIRTUAL_DISK_VERSION = ^ATTACH_VIRTUAL_DISK_VERSION;
  {$EXTERNALSYM PATTACH_VIRTUAL_DISK_VERSION}
  TAttachVirtualDiskVersion = ATTACH_VIRTUAL_DISK_VERSION;
  PAttachVirtualDiskVersion = PATTACH_VIRTUAL_DISK_VERSION;

  _ATTACH_VIRTUAL_DISK_FLAG = (
    ////////////////////////////////
    //No flags. Use system defaults.
    ATTACH_VIRTUAL_DISK_FLAG_NONE                 = $00000000,
    ////////////////////////////////
    //Attach the virtual disk as read-only.
    ATTACH_VIRTUAL_DISK_FLAG_READ_ONLY            = $00000001,
    ////////////////////////////////
    //No drive letters are assigned to the disk's volumes.
    ATTACH_VIRTUAL_DISK_FLAG_NO_DRIVE_LETTER      = $00000002,
    ///////////////////////////////
    //Will decouple the virtual disk lifetime from that of the
    //VirtualDiskHandle. The virtual disk will be attached until
    //the DetachVirtualDisk function is called, even if all open
    //handles to the virtual disk are closed.
    ATTACH_VIRTUAL_DISK_FLAG_PERMANENT_LIFETIME   = $00000004,
    //////////////////////////////
    //Reserved.
    ATTACH_VIRTUAL_DISK_FLAG_NO_LOCAL_HOST        = $00000008
  );
  {$EXTERNALSYM _ATTACH_VIRTUAL_DISK_FLAG}
  ATTACH_VIRTUAL_DISK_FLAG = _ATTACH_VIRTUAL_DISK_FLAG;
  {$EXTERNALSYM ATTACH_VIRTUAL_DISK_FLAG}
  PATTACH_VIRTUAL_DISK_FLAG = ^_ATTACH_VIRTUAL_DISK_FLAG;
  {$EXTERNALSYM PATTACH_VIRTUAL_DISK_FLAG}
  TAttachVirtualDiskFlag = ATTACH_VIRTUAL_DISK_FLAG;
  PAttachVirtualDiskFlag = PATTACH_VIRTUAL_DISK_FLAG;

  __ATTACH_VIRTUAL_DISK_VERSION1 = record
    Reserved: ULong;
  end;
  TAttachVirtualDiskVersion1 = __ATTACH_VIRTUAL_DISK_VERSION1;

  _ATTACH_VIRTUAL_DISK_PARAMETERS =  record
    Version: ATTACH_VIRTUAL_DISK_VERSION;
    case DWORD of
         0: (Version1: __ATTACH_VIRTUAL_DISK_VERSION1);
  end;
  {$EXTERNALSYM _ATTACH_VIRTUAL_DISK_PARAMETERS}
  ATTACH_VIRTUAL_DISK_PARAMETERS = _ATTACH_VIRTUAL_DISK_PARAMETERS;
  {$EXTERNALSYM ATTACH_VIRTUAL_DISK_PARAMETERS}
  PATTACH_VIRTUAL_DISK_PARAMETERS = ^_ATTACH_VIRTUAL_DISK_PARAMETERS;
  {$EXTERNALSYM PATTACH_VIRTUAL_DISK_PARAMETERS}
  TAttachVirtualDiskParameters = ATTACH_VIRTUAL_DISK_PARAMETERS;
  PAttachVirtualDiskParameters = PATTACH_VIRTUAL_DISK_PARAMETERS;

///////////////////////////////////////////////////////////////////////
///    DetachVirtualDisk
///

  _DETACH_VIRTUAL_DISK_FLAG = (
    DETACH_VIRTUAL_DISK_FLAG_NONE   = $00000000
  );
  {$EXTERNALSYM _DETACH_VIRTUAL_DISK_FLAG}
  DETACH_VIRTUAL_DISK_FLAG = _DETACH_VIRTUAL_DISK_FLAG;
  {$EXTERNALSYM DETACH_VIRTUAL_DISK_FLAG}
  PDETACH_VIRTUAL_DISK_FLAG = ^_DETACH_VIRTUAL_DISK_FLAG;
  {$EXTERNALSYM PDETACH_VIRTUAL_DISK_FLAG}
  TDetachVirtualDiskFlag = DETACH_VIRTUAL_DISK_FLAG;
  PDetachVirtualDiskFlag = PDETACH_VIRTUAL_DISK_FLAG;

///////////////////////////////////////////////////////////////////////
///  CreateVirtualDisk
///

  _CREATE_VIRTUAL_DISK_FLAG = (
    ///////////////////////////////////
    //No special creation conditions; system defaults are used.
    CREATE_VIRTUAL_DISK_FLAG_NONE                       = 0,
    ///////////////////////////////////
    //Pre-allocate all physical space necessary for the size of
    //the virtual disk.
    CREATE_VIRTUAL_DISK_FLAG_FULL_PHYSICAL_ALLOCATION   = 1
  );
  {$EXTERNALSYM _CREATE_VIRTUAL_DISK_FLAG}
  CREATE_VIRTUAL_DISK_FLAG = _CREATE_VIRTUAL_DISK_FLAG;
  {$EXTERNALSYM CREATE_VIRTUAL_DISK_FLAG}
  PCREATE_VIRTUAL_DISK_FLAG = ^_CREATE_VIRTUAL_DISK_FLAG;
  {$EXTERNALSYM PCREATE_VIRTUAL_DISK_FLAG}
  TCreateVirtualDiskFlag = CREATE_VIRTUAL_DISK_FLAG;
  PCreateVirtualDiskFlag = PCREATE_VIRTUAL_DISK_FLAG;

  _CREATE_VIRTUAL_DISK_VERSION = (
    CREATE_VIRTUAL_DISK_VERSION_UNSPECIFIED   = 0,
    CREATE_VIRTUAL_DISK_VERSION_1             = 1
  );
  {$EXTERNALSYM _CREATE_VIRTUAL_DISK_VERSION}
  CREATE_VIRTUAL_DISK_VERSION = _CREATE_VIRTUAL_DISK_VERSION;
  {$EXTERNALSYM CREATE_VIRTUAL_DISK_VERSION}
  PCREATE_VIRTUAL_DISK_VERSION= ^_CREATE_VIRTUAL_DISK_VERSION;
  {$EXTERNALSYM PCREATE_VIRTUAL_DISK_VERSION}
  TCreateVirtualDiskVersion = CREATE_VIRTUAL_DISK_VERSION;
  PCreateVirtualDiskVersion = PCREATE_VIRTUAL_DISK_VERSION;

  __CREATE_VIRTUAL_DISK_VERSION1 = packed record
    /////////////////////////////////////////
    // Unique identifier to assign to the virtual disk object.
    // If this member is set to zero, a unique identifier is
    // created by the system.
    UniqueId: TGUID;
    /////////////////////////////////////////
    // MaximumSize - The maximum virtual size, in bytes, of
    // the virtual disk object. Must be a multiple of 512.
    // If a ParentPath is specified, this value must be zero.
    // If a SourcePath is specified, this value can be zero
    // to specify the size of the source virtual disk to be used,
    // otherwise the size specified must be greater than or equal
    // to the size of the source disk.
    MaximumSize: Int64;
    /////////////////////////////////////////
    // Internal size of the virtual disk object blocks. This must
    // be set to CREATE_VIRTUAL_DISK_PARAMETERS_DEFAULT_BLOCK_SIZE (0),
    // which represents a block size of 2 MB (0x200000).
    BlockSizeInBytes: ULONG;
    /////////////////////////////////////////
    // Internal size of the virtual disk object sectors. Must be set to 512.
    SectorSizeInBytes: ULONG;
    /////////////////////////////////////////
    // Optional fully qualified path to a parent virtual disk object.
    // Associates the new virtual disk with an existing virtual disk.
    // If this parameter is not NULL, SourcePath must be NULL.
    ParentPath: PWideChar;
    /////////////////////////////////////////
    // Optional fully qualified path to pre-populate the new virtual
    // disk object with block data from an existing disk. This path
    // may refer to a virtual disk or a physical disk.
    // If this parameter is not NULL, ParentPath must be NULL.
    SourcePath: PWideChar;
  end;
  TCreateVirtualDiskVersion1 = __CREATE_VIRTUAL_DISK_VERSION1;

  _CREATE_VIRTUAL_DISK_PARAMETERS = packed record
    Version: _CREATE_VIRTUAL_DISK_VERSION;
    case Union:DWORD of
         0: (Version1: __CREATE_VIRTUAL_DISK_VERSION1);
  end;
  {$EXTERNALSYM _CREATE_VIRTUAL_DISK_PARAMETERS}
  CREATE_VIRTUAL_DISK_PARAMETERS = _CREATE_VIRTUAL_DISK_PARAMETERS;
  {$EXTERNALSYM CREATE_VIRTUAL_DISK_PARAMETERS}
  PCREATE_VIRTUAL_DISK_PARAMETERS = ^_CREATE_VIRTUAL_DISK_PARAMETERS;
  {$EXTERNALSYM PCREATE_VIRTUAL_DISK_PARAMETERS}
  TCreateVirtualDiskParameters = CREATE_VIRTUAL_DISK_PARAMETERS;
  PCreateVirtualDiskParameters = PCREATE_VIRTUAL_DISK_PARAMETERS;

//
// GetStorageDependencyInformation
//

// Flags for dependent disks

    _DEPENDENT_DISK_FLAG = (
      DEPENDENT_DISK_FLAG_NONE                 = $00000000,
      //
      // Multiple files backing the virtual storage device
      //
      DEPENDENT_DISK_FLAG_MULT_BACKING_FILES   = $00000001,
      DEPENDENT_DISK_FLAG_FULLY_ALLOCATED      = $00000002,
      DEPENDENT_DISK_FLAG_READ_ONLY            = $00000004,
      //
      //Backing file of the virtual storage device is not local to the machine
      //
      DEPENDENT_DISK_FLAG_REMOTE               = $00000008,
      //
      // Volume is the system volume
      //
      DEPENDENT_DISK_FLAG_SYSTEM_VOLUME        = $00000010,
      //
      // Volume backing the virtual storage device file is the system volume
      //
      DEPENDENT_DISK_FLAG_SYSTEM_VOLUME_PARENT = $00000020,
      DEPENDENT_DISK_FLAG_REMOVABLE            = $00000040,
      //
      // Drive letters are not assigned to the volumes
      // on the virtual disk automatically.
      //
      DEPENDENT_DISK_FLAG_NO_DRIVE_LETTER      = $00000080,
      DEPENDENT_DISK_FLAG_PARENT               = $00000100,
      //
      // Virtual disk is not attached on the local host
      // (instead attached on a guest VM for instance)
      //
      DEPENDENT_DISK_FLAG_NO_HOST_DISK         = $00000200,
      //
      // Indicates the lifetime of the disk is not tied
      // to any system handles
      //
      DEPENDENT_DISK_FLAG_PERMANENT_LIFETIME   = $00000400
    );
    {$EXTERNALSYM _DEPENDENT_DISK_FLAG}
    DEPENDENT_DISK_FLAG = _DEPENDENT_DISK_FLAG;
    {$EXTERNALSYM DEPENDENT_DISK_FLAG}
    TDependentDiskFlag = DEPENDENT_DISK_FLAG;

// Version definitions
    _STORAGE_DEPENDENCY_INFO_VERSION = (
      STORAGE_DEPENDENCY_INFO_VERSION_UNSPECIFIED = 0,
      STORAGE_DEPENDENCY_INFO_VERSION_1           = 1,
      STORAGE_DEPENDENCY_INFO_VERSION_2           = 2
    );
    {$EXTERNALSYM _STORAGE_DEPENDENCY_INFO_VERSION}
    STORAGE_DEPENDENCY_INFO_VERSION = _STORAGE_DEPENDENCY_INFO_VERSION;
    {$EXTERNALSYM STORAGE_DEPENDENCY_INFO_VERSION}
    TStorageDependencyInfoVersion = STORAGE_DEPENDENCY_INFO_VERSION;

// Parameter structure for GetStorageDependencyInformation
   _STORAGE_DEPENDENCY_INFO_TYPE_1 = record
     DependencyTypeFlags  : _DEPENDENT_DISK_FLAG;
     ProviderSpecificFlags: ULONG;
     VirtualStorageType: VIRTUAL_STORAGE_TYPE;
   end;
   {$EXTERNALSYM _STORAGE_DEPENDENCY_INFO_TYPE_1}
   STORAGE_DEPENDENCY_INFO_TYPE_1 = _STORAGE_DEPENDENCY_INFO_TYPE_1;
   {$EXTERNALSYM STORAGE_DEPENDENCY_INFO_TYPE_1}
   PSTORAGE_DEPENDENCY_INFO_TYPE_1 = ^_STORAGE_DEPENDENCY_INFO_TYPE_1;
   {$EXTERNALSYM PSTORAGE_DEPENDENCY_INFO_TYPE_1}
   TStorageDependencyInfoType1 = STORAGE_DEPENDENCY_INFO_TYPE_1;
   PStorageDependencyInfoType1 = PSTORAGE_DEPENDENCY_INFO_TYPE_1;

// Parameter structure for GetStorageDependencyInformation
   _STORAGE_DEPENDENCY_INFO_TYPE_2 = record
      DependencyTypeFlags: DEPENDENT_DISK_FLAG;
      ProviderSpecificFlags: ULONG;
      VirtualStorageType: VIRTUAL_STORAGE_TYPE;
      AncestorLevel: ULONG;
      DependencyDeviceName: PWideChar;
      HostVolumeName: PWideChar;
      DependentVolumeName: PWideChar;
      DependentVolumeRelativePath: PWideChar;
   end;
   {$EXTERNALSYM _STORAGE_DEPENDENCY_INFO_TYPE_2}
   STORAGE_DEPENDENCY_INFO_TYPE_2 = _STORAGE_DEPENDENCY_INFO_TYPE_2;
   {$EXTERNALSYM STORAGE_DEPENDENCY_INFO_TYPE_2}
   PSTORAGE_DEPENDENCY_INFO_TYPE_2 = ^_STORAGE_DEPENDENCY_INFO_TYPE_2;
   {$EXTERNALSYM PSTORAGE_DEPENDENCY_INFO_TYPE_2}
   TStorageDependencyInfoType2 = STORAGE_DEPENDENCY_INFO_TYPE_2;
   PStorageDependencyInfoType2 = PSTORAGE_DEPENDENCY_INFO_TYPE_2;
   

// Parameter structure for GetStorageDependencyInformation
   _STORAGE_DEPENDENCY_INFO = record

    Version: STORAGE_DEPENDENCY_INFO_VERSION;
    NumberEntries: ULONG;
    case DWORD of
         0: (Version1Entries: array[0..0] of STORAGE_DEPENDENCY_INFO_TYPE_1);
         1: (Version2Entries: array[0..0] of STORAGE_DEPENDENCY_INFO_TYPE_2);
   end;
   {$EXTERNALSYM _STORAGE_DEPENDENCY_INFO}
   STORAGE_DEPENDENCY_INFO = _STORAGE_DEPENDENCY_INFO;
   {$EXTERNALSYM STORAGE_DEPENDENCY_INFO}
   PSTORAGE_DEPENDENCY_INFO = ^_STORAGE_DEPENDENCY_INFO;
   {$EXTERNALSYM PSTORAGE_DEPENDENCY_INFO}
   TStorageDependencyInfo = STORAGE_DEPENDENCY_INFO;
   PStorageDependencyInfo = PSTORAGE_DEPENDENCY_INFO;

// Flags for GetStorageDependencyInformation
   _GET_STORAGE_DEPENDENCY_FLAG = (
     GET_STORAGE_DEPENDENCY_FLAG_NONE         = $00000000,
     // Return information for volumes or disks hosting the volume specified
     // If not set, returns info about volumes or disks being hosted by
     // the volume or disk specified
     GET_STORAGE_DEPENDENCY_FLAG_HOST_VOLUMES = $00000001,

    //  The handle provided is to a disk, not volume or file
    GET_STORAGE_DEPENDENCY_FLAG_DISK_HANDLE  = $00000002
   );
   {$EXTERNALSYM _GET_STORAGE_DEPENDENCY_FLAG}
   GET_STORAGE_DEPENDENCY_FLAG = _GET_STORAGE_DEPENDENCY_FLAG;
   {$EXTERNALSYM GET_STORAGE_DEPENDENCY_FLAG}
   TGetStorageDependencyFlag = GET_STORAGE_DEPENDENCY_FLAG;

///////////////////////////////////////////////////////////////////////////////
///  GetVirtualDiskInformation
///

  _GET_VIRTUAL_DISK_INFO_VERSION = (
    GET_VIRTUAL_DISK_INFO_UNSPECIFIED            = 0,
    GET_VIRTUAL_DISK_INFO_SIZE                   = 1,
    GET_VIRTUAL_DISK_INFO_IDENTIFIER             = 2,
    GET_VIRTUAL_DISK_INFO_PARENT_LOCATION        = 3,
    GET_VIRTUAL_DISK_INFO_PARENT_IDENTIFIER      = 4,
    GET_VIRTUAL_DISK_INFO_PARENT_TIMESTAMP       = 5,
    GET_VIRTUAL_DISK_INFO_VIRTUAL_STORAGE_TYPE   = 6,
    GET_VIRTUAL_DISK_INFO_PROVIDER_SUBTYPE       = 7
  );
  {$EXTERNALSYM _GET_VIRTUAL_DISK_INFO_VERSION}
  GET_VIRTUAL_DISK_INFO_VERSION = _GET_VIRTUAL_DISK_INFO_VERSION;
  {$EXTERNALSYM GET_VIRTUAL_DISK_INFO_VERSION}
  PGET_VIRTUAL_DISK_INFO_VERSION = ^_GET_VIRTUAL_DISK_INFO_VERSION;
  {$EXTERNALSYM PGET_VIRTUAL_DISK_INFO_VERSION}
  TGetVirtualDiskInfoVersion = GET_VIRTUAL_DISK_INFO_VERSION;
  PGetVirtualDiskInfoVersion = PGET_VIRTUAL_DISK_INFO_VERSION;

  __GET_VIRTUAL_DISK_INFO_SIZE = record
    VirtualSize: ULONGLONG;
    PhysicalSize: ULONGLONG;
    Blocksize: ULONG;
    SectorSize: ULONG;
  end;
  TGetVirtualDiskInfoSize = __GET_VIRTUAL_DISK_INFO_SIZE;

  __GET_VIRTUAL_DISK_INFO_PARENT_LOCATION = record
    ParentResolved: BOOL;
    ParentLocationBuffer: array[0..0] of WideChar;
  end;
  TGetVirtualDiskInfoParentLocation = __GET_VIRTUAL_DISK_INFO_PARENT_LOCATION;

  _GET_VIRTUAL_DISK_INFO = record
     Version: GET_VIRTUAL_DISK_INFO_VERSION;
     case Integer of
          0: (Size: __GET_VIRTUAL_DISK_INFO_SIZE);
          1: (Identifier: TGUID);
          2: (ParentLocation: __GET_VIRTUAL_DISK_INFO_PARENT_LOCATION);
          3: (ParentIdentifier: TGUID);
          4: (ParentTimeStamp: ULONG);
          5: (VirtualStorageType:VIRTUAL_STORAGE_TYPE);
          6: (ProviderSubtype: ULONG)
  end;
  {$EXTERNALSYM _GET_VIRTUAL_DISK_INFO}
  GET_VIRTUAL_DISK_INFO = _GET_VIRTUAL_DISK_INFO;
  {$EXTERNALSYM GET_VIRTUAL_DISK_INFO}
  PGET_VIRTUAL_DISK_INFO = ^_GET_VIRTUAL_DISK_INFO;
  {$EXTERNALSYM PGET_VIRTUAL_DISK_INFO}
  TGetVirtualDiskInfo = GET_VIRTUAL_DISK_INFO;
  PGetVirtualDiskInfo = PGET_VIRTUAL_DISK_INFO;

//
// SetVirtualDiskInformation
//

// Version definitions
  _SET_VIRTUAL_DISK_INFO_VERSION = (
    SET_VIRTUAL_DISK_INFO_UNSPECIFIED       = 0,
    SET_VIRTUAL_DISK_INFO_PARENT_PATH       = 1,
    SET_VIRTUAL_DISK_INFO_IDENTIFIER        = 2
  );
  {$EXTERNALSYM _SET_VIRTUAL_DISK_INFO_VERSION}
  SET_VIRTUAL_DISK_INFO_VERSION = _SET_VIRTUAL_DISK_INFO_VERSION;
  {$EXTERNALSYM SET_VIRTUAL_DISK_INFO_VERSION}
  PSET_VIRTUAL_DISK_INFO_VERSION = ^_SET_VIRTUAL_DISK_INFO_VERSION;
  {$EXTERNALSYM PSET_VIRTUAL_DISK_INFO_VERSION}
  TSetVirtualDiskInfoVersion = SET_VIRTUAL_DISK_INFO_VERSION;
  PSetVirtualDiskInfoVersion = PSET_VIRTUAL_DISK_INFO_VERSION;

// Versioned parameter structure for SetVirtualDiskInformation
  _SET_VIRTUAL_DISK_INFO = record
    Version: SET_VIRTUAL_DISK_INFO_VERSION;
    case DWORD of
         0: (ParentFilePath: PWideChar);
         1: (UniqueIdentifier: TGUID);
  end;
  {$EXTERNALSYM _SET_VIRTUAL_DISK_INFO}
  SET_VIRTUAL_DISK_INFO = _SET_VIRTUAL_DISK_INFO;
  {$EXTERNALSYM SET_VIRTUAL_DISK_INFO}
  PSET_VIRTUAL_DISK_INFO = ^_SET_VIRTUAL_DISK_INFO;
  {$EXTERNALSYM PSET_VIRTUAL_DISK_INFO}
  TSetVirtualDiskInfo = SET_VIRTUAL_DISK_INFO;
  PSetVirtualDiskInfo = PSET_VIRTUAL_DISK_INFO;

//
// GetVirtualDiskOperationProgress
//

  _VIRTUAL_DISK_PROGRESS = record
    OperationStatus: DWORD;
    CurrentValue: ULONGLONG;
    CompletionValue: ULONGLONG;
  end;
  {$EXTERNALSYM _VIRTUAL_DISK_PROGRESS}
  VIRTUAL_DISK_PROGRESS = _VIRTUAL_DISK_PROGRESS;
  {$EXTERNALSYM VIRTUAL_DISK_PROGRESS}
  PVIRTUAL_DISK_PROGRESS = ^_VIRTUAL_DISK_PROGRESS;
  {$EXTERNALSYM PVIRTUAL_DISK_PROGRESS}
  TVirtualDiskProgress = VIRTUAL_DISK_PROGRESS;
  PVirtualDiskProgress = PVIRTUAL_DISK_PROGRESS;

//
// CompactVirtualDisk
//

// Version definitions
  _COMPACT_VIRTUAL_DISK_VERSION = (
    COMPACT_VIRTUAL_DISK_VERSION_UNSPECIFIED    = 0,
    COMPACT_VIRTUAL_DISK_VERSION_1              = 1
  );
  {$EXTERNALSYM _COMPACT_VIRTUAL_DISK_VERSION}
  COMPACT_VIRTUAL_DISK_VERSION = _COMPACT_VIRTUAL_DISK_VERSION;
  {$EXTERNALSYM COMPACT_VIRTUAL_DISK_VERSION}
  TCompactVirtualDiskVersion = COMPACT_VIRTUAL_DISK_VERSION;

// Versioned structure for CompactVirtualDisk
  _COMPACT_VIRTUAL_DISK_PARAMETERS = record
    Version: COMPACT_VIRTUAL_DISK_VERSION;
    case Union:DWORD of
       0: (Reserved: ULONG);
  end;
  {$EXTERNALSYM _COMPACT_VIRTUAL_DISK_PARAMETERS}
  COMPACT_VIRTUAL_DISK_PARAMETERS = _COMPACT_VIRTUAL_DISK_PARAMETERS;
  {$EXTERNALSYM COMPACT_VIRTUAL_DISK_PARAMETERS}
  PCOMPACT_VIRTUAL_DISK_PARAMETERS = ^_COMPACT_VIRTUAL_DISK_PARAMETERS;
  {$EXTERNALSYM PCOMPACT_VIRTUAL_DISK_PARAMETERS}
  TCompactVirtualDiskParameters = COMPACT_VIRTUAL_DISK_PARAMETERS;
  PCompactVirtualDiskParameters = PCOMPACT_VIRTUAL_DISK_PARAMETERS;

// Flags for CompactVirtualDisk
  _COMPACT_VIRTUAL_DISK_FLAG = (
    COMPACT_VIRTUAL_DISK_FLAG_NONE                 = $00000000
  );
  {$EXTERNALSYM _COMPACT_VIRTUAL_DISK_FLAG}
  COMPACT_VIRTUAL_DISK_FLAG = _COMPACT_VIRTUAL_DISK_FLAG;
  {$EXTERNALSYM COMPACT_VIRTUAL_DISK_FLAG}
  TCompactVirtualDiskFlag = COMPACT_VIRTUAL_DISK_FLAG;

//
// MergeVirtualDisk
//

// Version definitions
  _MERGE_VIRTUAL_DISK_VERSION = (
    MERGE_VIRTUAL_DISK_VERSION_UNSPECIFIED    = 0,
    MERGE_VIRTUAL_DISK_VERSION_1              = 1
  );
  {$EXTERNALSYM _MERGE_VIRTUAL_DISK_VERSION}
  MERGE_VIRTUAL_DISK_VERSION = _MERGE_VIRTUAL_DISK_VERSION;
  {$EXTERNALSYM MERGE_VIRTUAL_DISK_VERSION}
  TMergeVirtualDiskVerion = MERGE_VIRTUAL_DISK_VERSION;

  _MERGE_VIRTUAL_DISK_PARAMETERS = record
    Version: MERGE_VIRTUAL_DISK_VERSION;
    case DWORD of
         0: (MergeDepth: ULONG);
  end;
  {$EXTERNALSYM _MERGE_VIRTUAL_DISK_PARAMETERS}
  MERGE_VIRTUAL_DISK_PARAMETERS = _MERGE_VIRTUAL_DISK_PARAMETERS;
  {$EXTERNALSYM MERGE_VIRTUAL_DISK_PARAMETERS}
  PMERGE_VIRTUAL_DISK_PARAMETERS = ^_MERGE_VIRTUAL_DISK_PARAMETERS;
  {$EXTERNALSYM PMERGE_VIRTUAL_DISK_PARAMETERS}
  TMergeVirtualDiskParameters = MERGE_VIRTUAL_DISK_PARAMETERS;
  PMergeVirtualDiskParameters = PMERGE_VIRTUAL_DISK_PARAMETERS;

// Flags for MergeVirtualDisk
  _MERGE_VIRTUAL_DISK_FLAG = (
    MERGE_VIRTUAL_DISK_FLAG_NONE                 = $00000000
  );
  {$EXTERNALSYM _MERGE_VIRTUAL_DISK_FLAG}
  MERGE_VIRTUAL_DISK_FLAG = _MERGE_VIRTUAL_DISK_FLAG;
  {$EXTERNALSYM MERGE_VIRTUAL_DISK_FLAG}
  TMergeVirtualDiskFlag = MERGE_VIRTUAL_DISK_FLAG;

//
// ExpandVirtualDisk
//

// Version definitions
  _EXPAND_VIRTUAL_DISK_VERSION = (
    EXPAND_VIRTUAL_DISK_VERSION_UNSPECIFIED    = 0,
    EXPAND_VIRTUAL_DISK_VERSION_1              = 1
  );
  {$EXTERNALSYM _EXPAND_VIRTUAL_DISK_VERSION}
  EXPAND_VIRTUAL_DISK_VERSION = _EXPAND_VIRTUAL_DISK_VERSION;
  {$EXTERNALSYM EXPAND_VIRTUAL_DISK_VERSION}
  TExpandVirtualDiskVersion = EXPAND_VIRTUAL_DISK_VERSION;

// Versioned parameter structure for ExpandVirtualDisk
  _EXPAND_VIRTUAL_DISK_PARAMETERS = packed record
    Version: EXPAND_VIRTUAL_DISK_VERSION;
    case Union:DWORD of
         0: (NewSize: ULONGLONG);
  end;
  {$EXTERNALSYM _EXPAND_VIRTUAL_DISK_PARAMETERS}
  EXPAND_VIRTUAL_DISK_PARAMETERS = _EXPAND_VIRTUAL_DISK_PARAMETERS;
  {$EXTERNALSYM EXPAND_VIRTUAL_DISK_PARAMETERS}
  PEXPAND_VIRTUAL_DISK_PARAMETERS = ^_EXPAND_VIRTUAL_DISK_PARAMETERS;
  {$EXTERNALSYM PEXPAND_VIRTUAL_DISK_PARAMETERS}
  TExpandVirtualDiskParameters = EXPAND_VIRTUAL_DISK_PARAMETERS;
  PExpandVirtualDiskParameters = PEXPAND_VIRTUAL_DISK_PARAMETERS;

// Flags for ExpandVirtualDisk
  _EXPAND_VIRTUAL_DISK_FLAG = (
    EXPAND_VIRTUAL_DISK_FLAG_NONE                 = $00000000
  );
  {$EXTERNALSYM _EXPAND_VIRTUAL_DISK_FLAG}
  EXPAND_VIRTUAL_DISK_FLAG = _EXPAND_VIRTUAL_DISK_FLAG;
  {$EXTERNALSYM EXPAND_VIRTUAL_DISK_FLAG}
  TExpandVirtualDiskFlag = EXPAND_VIRTUAL_DISK_FLAG;

//  Dll Imports

  function OpenVirtualDisk(VirtualStorageType: PVirtualStorageType;
                           Path: PWideChar;
                           VirtualDiskAccessMask: TVirtualDiskAccessMask;
                           Flags: TOpenVirtualDiskFlag;
                           Parameters: POpenVirtualDiskParameters;
                           Handle: PHandle): Cardinal; stdcall;
  {$EXTERNALSYM OpenVirtualDisk}
  function AttachVirtualDisk(Handle: THandle;
                             SecurityDescriptor: PSecurityDescriptor;
                             Flags: TAttachVirtualDiskFlag;
                             ProviderSpecificFlags: ULong;
	                           Parameters: PAttachVirtualDiskParameters;
                             Overlapped: POVERLAPPED): Cardinal; stdcall;
  {$EXTERNALSYM AttachVirtualDisk}
  function DetachVirtualDisk(AHandle: THandle;
                             Flags: TDetachVirtualDiskFlag;
                             ProviderSpecificFlags: ULONG): Cardinal; stdcall;
  {$EXTERNALSYM DetachVirtualDisk}
  function GetVirtualDiskPhysicalPath(VirtualDiskHandle: THandle;
                                      DiskPathSizeInBytes: PULONG;
                                      DiskPath: PWideChar): Cardinal; stdcall;
  {$EXTERNALSYM GetVirtualDiskPhysicalPath}
  function CreateVirtualDisk(VirtualStorageType: PVirtualStorageType;
                             Path: PWideChar;
                             VirtualDiskAccessMask: TVirtualDiskAccessMask;
                             SecurityDescriptor: PSecurityDescriptor;
                             Flags: TCreateVirtualDiskFlag;
                             ProviderSpecificFlags: ULong;
                             Parameters: PCreateVirtualDiskParameters;
                             Overlapped: POverlapped;
                             Handle: PHandle): Cardinal; stdcall;
  {$EXTERNALSYM CreateVirtualDisk}
  function GetStorageDependencyInformation(ObjectHandle: THandle;
                                 Flags: TGetStorageDependencyFlag;
                                 StorageDependencyInfoSize: ULONG;
                                 StorageDependencyInfo: PStorageDependencyInfo;
                                 SizeUsed: PULONG): Cardinal; stdcall;
  {$EXTERNALSYM GetStorageDependencyInformation}
  function GetVirtualDiskInformation(VirtualDiskHandle: THandle;
                                     VirtualDiskInfoSize: PULONG;
                                     VirtualDiskInfo: PGetVirtualDiskInfo;
                                     SizeUsed: PULONG): Cardinal; stdcall;
  {$EXTERNALSYM GetVirtualDiskInformation}
  function SetVirtualDiskInformation(VirtualDiskHandle: THandle;
                                     VirtualDiskInfo: PSetVirtualDiskInfo
                                     ): Cardinal; stdcall;
  {$EXTERNALSYM SetVirtualDiskInformation}
  function GetVirtualDiskOperationProgress(VirtualDiskHandle: THandle;
                                           Overlapped: POverlapped;
                                           Progress: PVirtualDiskProgress
                                           ): Cardinal; stdcall;
  {$EXTERNALSYM GetVirtualDiskOperationProgress}
  function CompactVirtualDisk(VirtualDiskHandle: THandle;
                              Flags: TCompactVirtualDiskFlag;
                              Parameters: PCompactVirtualDiskParameters;
                              Overlapped: POverlapped): Cardinal; stdcall;
  {$EXTERNALSYM CompactVirtualDisk}
  function MergeVirtualDisk(VirtualDiskHandle: THandle;
                            Flags: TMergeVirtualDiskFlag;
                            Parameters: PMergeVirtualDiskParameters;
                            Overlapped: POverlapped): Cardinal; stdcall;
  {$EXTERNALSYM MergeVirtualDisk}
  function ExpandVirtualDisk(VirtualDiskHandle: THandle;
                             Flags: TExpandVirtualDiskFlag;
                             Parameters: PExpandVirtualDiskParameters;
                             Overlapped: POverlapped): Cardinal; stdcall;
  {$EXTERNALSYM ExpandVirtualDisk}


{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation
uses JwaWinDllNames;
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

  var
    _OpenVirtualDisk: Pointer;

  function OpenVirtualDisk;
  begin
    GetProcedureAddress(_OpenVirtualDisk, VirtDisklib, 'OpenVirtualDisk');
    asm
          MOV     ESP, EBP
          POP     EBP
          JMP     [_OpenVirtualDisk]
    end;
  end;

  var
    _AttachVirtualDisk: Pointer;

  function AttachVirtualDisk;
  begin
    GetProcedureAddress(_AttachVirtualDisk, VirtDisklib, 'AttachVirtualDisk');
    asm
          MOV     ESP, EBP
          POP     EBP
          JMP     [_AttachVirtualDisk]
    end;
  end;

  var
    _DetachVirtualDisk: Pointer;

  function DetachVirtualDisk;
  begin
    GetProcedureAddress(_DetachVirtualDisk, VirtDisklib, 'DetachVirtualDisk');
    asm
          MOV     ESP, EBP
          POP     EBP
          JMP     [_DetachVirtualDisk]
    end;
  end;

  var
    _GetVirtualDiskPhysicalPath: Pointer;

  function GetVirtualDiskPhysicalPath;
  begin
    GetProcedureAddress(_GetVirtualDiskPhysicalPath,
                        VirtDisklib, 'GetVirtualDiskPhysicalPath');
    asm
          MOV     ESP, EBP
          POP     EBP
          JMP     [_GetVirtualDiskPhysicalPath]
    end;
  end;

  var
    _CreateVirtualDisk: Pointer;

  function CreateVirtualDisk;
  begin
    GetProcedureAddress(_CreateVirtualDisk, VirtDisklib, 'CreateVirtualDisk');
    asm
          MOV     ESP, EBP
          POP     EBP
          JMP     [_CreateVirtualDisk]
    end;
  end;

  var
    _GetStorageDependencyInformation: Pointer;

  function GetStorageDependencyInformation;
  begin
    GetProcedureAddress(_GetStorageDependencyInformation, VirtDisklib, 'GetStorageDependencyInformation');
    asm
          MOV     ESP, EBP
          POP     EBP
          JMP     [_GetStorageDependencyInformation]
    end;
  end;

  var
    _GetVirtualDiskInformation: Pointer;

  function GetVirtualDiskInformation;
  begin
    GetProcedureAddress(_GetVirtualDiskInformation, VirtDisklib, 'GetVirtualDiskInformation');
    asm
          MOV     ESP, EBP
          POP     EBP
          JMP     [_GetVirtualDiskInformation]
    end;
  end;

  var
    _SetVirtualDiskInformation: Pointer;

  function SetVirtualDiskInformation;
  begin
    GetProcedureAddress(_SetVirtualDiskInformation, VirtDisklib, 'SetVirtualDiskInformation');
    asm
          MOV     ESP, EBP
          POP     EBP
          JMP     [_SetVirtualDiskInformation]
    end;
  end;

  var
    _GetVirtualDiskOperationProgress: Pointer;

  function GetVirtualDiskOperationProgress;
  begin
    GetProcedureAddress(_GetVirtualDiskOperationProgress, VirtDisklib, 'GetVirtualDiskOperationProgress');
    asm
          MOV     ESP, EBP
          POP     EBP
          JMP     [_GetVirtualDiskOperationProgress]
    end;
  end;

  var
    _CompactVirtualDisk: Pointer;

  function CompactVirtualDisk;
  begin
    GetProcedureAddress(_CompactVirtualDisk, VirtDisklib, 'CompactVirtualDisk');
    asm
          MOV     ESP, EBP
          POP     EBP
          JMP     [_CompactVirtualDisk]
    end;
  end;

  var
    _MergeVirtualDisk: Pointer;

  function MergeVirtualDisk;
  begin
    GetProcedureAddress(_MergeVirtualDisk, VirtDisklib, 'MergeVirtualDisk');
    asm
          MOV     ESP, EBP
          POP     EBP
          JMP     [_MergeVirtualDisk]
    end;
  end;

  var
    _ExpandVirtualDisk: Pointer;

  function ExpandVirtualDisk;
  begin
    GetProcedureAddress(_ExpandVirtualDisk, VirtDisklib, 'ExpandVirtualDisk');
    asm
          MOV     ESP, EBP
          POP     EBP
          JMP     [_ExpandVirtualDisk]
    end;
  end;

{$ELSE}

  function OpenVirtualDisk; external VirtDisklib {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'OpenVirtualDisk';
  {$EXTERNALSYM OpenVirtualDisk}
  function AttachVirtualDisk; external VirtDisklib {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'AttachVirtualDisk';
  {$EXTERNALSYM AttachVirtualDisk}
  function DetachVirtualDisk; external VirtDisklib {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'DetachVirtualDisk';
  {$EXTERNALSYM DetachVirtualDisk}
  function GetVirtualDiskPhysicalPath; external VirtDisklib {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'GetVirtualDiskPhysicalPath';
  {$EXTERNALSYM GetVirtualDiskPhysicalPath}
  function CreateVirtualDisk; external VirtDisklib {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'CreateVirtualDisk';
  {$EXTERNALSYM CreateVirtualDisk}
  function GetStorageDependencyInformation; external VirtDisklib {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'GetStorageDependencyInformation';
  {$EXTERNALSYM GetStorageDependencyInformation}
  function GetVirtualDiskInformation; external VirtDisklib {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'GetVirtualDiskInformation';
  {$EXTERNALSYM GetVirtualDiskInformation}
  function SetVirtualDiskInformation; external VirtDisklib {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'SetVirtualDiskInformation';
  {$EXTERNALSYM SetVirtualDiskInformation}
  function GetVirtualDiskOperationProgress; external VirtDisklib {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'GetVirtualDiskOperationProgress';
  {$EXTERNALSYM GetVirtualDiskOperationProgress}
  function CompactVirtualDisk; external VirtDisklib {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'CompactVirtualDisk';
  {$EXTERNALSYM CompactVirtualDisk}
  function MergeVirtualDisk; external VirtDisklib {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'MergeVirtualDisk';
  {$EXTERNALSYM MergeVirtualDisk}
  function ExpandVirtualDisk; external VirtDisklib {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'ExpandVirtualDisk';
  {$EXTERNALSYM ExpandVirtualDisk}
//ExpandVirtualDisk returns ERROR_INVALID_ARGUMENT if NewSize is <= actual size

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
