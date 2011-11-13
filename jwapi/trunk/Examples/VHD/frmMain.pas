{******************************************************************************}
{ JEDI Virtual Disk API example                                                }
{ http://jedi-apilib.sourceforge.net                                           }
{ http://wiki.delphi-jedi.org/                                                 }
{ http://blog.delphi-jedi.net/                                                 }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ Author(s):                                                                   }
{ Creation date: November 4th 2010                                             }
{ Last modification date: November 28th 2010                                   }
{                                                                              }
{ Description: Demonstrates how to use the virtual disk API                    }
{              with Windows 7                                                  }
{                                                                              }
{ Preparations: JWA must be ready to use.                                      }
{                                                                              }
{ Version history: November 4th 2010: initial version                          }
{                                                                              }
{ No license. Use this example with no warranty at all and on your own risk.   }
{ This example is just for learning purposes and should not be used in         }
{ production environments.                                                     }
{******************************************************************************}
unit frmMain;

interface

uses
  SysUtils, Controls, Classes, Dialogs, Forms, JwaVirtDisk, JwaWinIoctl,
  JwaWinNT, JwaWinBase, JwaWinType, JwaWinError, StdCtrls;

type
  TMainForm = class(TForm)
    btnMount: TButton;
    OpenDialogVHD: TOpenDialog;
    btnDismount: TButton;
    MemoInfo: TMemo;
    btnInfo: TButton;
    procedure btnMountClick(Sender: TObject);
    procedure btnDismountClick(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}
{$R userlevel.res} // Admin rights are required for moun and dismount actions

// more information http://msdn.microsoft.com/en-us/library/dd323700%28v=VS.85%29.aspx

// verify the disksize to be a multiple of CREATE_VIRTUAL_DISK_PARAMETERS_DEFAULT_SECTOR_SIZE (512)
function ValidateDiskSize(VirtualDiskSize: ULONGLONG): ULONGLONG;
begin
  Result:= (VirtualDiskSize div CREATE_VIRTUAL_DISK_PARAMETERS_DEFAULT_SECTOR_SIZE) *
            CREATE_VIRTUAL_DISK_PARAMETERS_DEFAULT_SECTOR_SIZE;
end;

// create a new vhd file
function VirtualDiskCreate(path: PWideChar; size: ULONGLONG; accessMask: TVirtualDiskAccessMask; var AHandle: THandle;
                           Flags: TCreateVirtualDiskFlag = CREATE_VIRTUAL_DISK_FLAG_NONE; source: PWideChar = nil;
                           parent: PWideChar = nil; securityDescriptor: Pointer = nil; overlapped: POverlapped = nil): Cardinal;
var
  pParam: TCreateVirtualDiskParameters;
  pVST: TVirtualStorageType;
begin
  pVST.DeviceId:= VIRTUAL_STORAGE_TYPE_DEVICE_VHD;
  pVST.VendorId:= VIRTUAL_STORAGE_TYPE_VENDOR_MICROSOFT;

  pParam.Version:= CREATE_VIRTUAL_DISK_VERSION_1;
  pParam.Version1.BlockSizeInBytes:= CREATE_VIRTUAL_DISK_PARAMETERS_DEFAULT_BLOCK_SIZE;
  pParam.Version1.SectorSizeInBytes:= CREATE_VIRTUAL_DISK_PARAMETERS_DEFAULT_SECTOR_SIZE;
  //the values above must be set like this

  pParam.Version1.MaximumSize:= size;
  //  The maximum virtual size, in bytes, of the virtual disk object. Must be a multiple of 512.
  //  If a ParentPath is specified, this value must be zero.
  //  If a SourcePath is specified, this value can be zero to specify the size of the source virtual
  //  disk to be used, otherwise the size specified must be greater than or equal to the size of the source disk.

  pParam.Version1.ParentPath:= parent;
  //  Optional fully qualified path to a parent virtual disk object. Associates the new virtual disk with an existing virtual disk.
  //  If this parameter is not NULL, SourcePath must be NULL.

  pParam.Version1.SourcePath:= source;
  //  Optional fully qualified path to pre-populate the new virtual disk object with block data from an existing disk.
  //  This path may refer to a virtual disk or a physical disk.
  //  If this parameter is not NULL, ParentPath must be NULL.

  pParam.Version1.UniqueId:= StringToGUid('{00000000-0000-0000-0000-000000000000}');
  //Unique identifier to assign to the virtual disk object. If this member is set to zero, a unique identifier is created by the system.

  Result:= CreateVirtualDisk(@pVST,
                             path,
                             accessMask,
                             securityDescriptor,
                             Flags,
                             0,
                             @pParam,
                             overlapped,
                             @AHandle);

  //Flags

  //  CREATE_VIRTUAL_DISK_FLAG_NONE
  //  No special creation conditions; system defaults are used.

  //  CREATE_VIRTUAL_DISK_FLAG_FULL_PHYSICAL_ALLOCATION
  //  Pre-allocate all physical space necessary for the size of the virtual disk.

  if Result <> ERROR_SUCCESS then
     AHandle:= INVALID_HANDLE_VALUE;
end;

// open a vhd file
function VirtualDiskOpen(path: PWideChar; accessMask: TVirtualDiskAccessMask; var AHandle: THandle;
                         Flags: TOpenVirtualDiskFlag = OPEN_VIRTUAL_DISK_FLAG_NONE; RWDepth: Cardinal = 1): Cardinal;
var
  pParam: TOpenVirtualDiskParameters;
  pVST: TVirtualStorageType;
begin
  pVST.DeviceId:= VIRTUAL_STORAGE_TYPE_DEVICE_VHD;
  pVST.VendorId:= VIRTUAL_STORAGE_TYPE_VENDOR_MICROSOFT;

  pParam.Version:= OPEN_VIRTUAL_DISK_VERSION_1;
  //the values above must be set like this

  pParam.Version1.RWDepth:= RWDepth;

  ///    RWDepth
  ///
  ///    Indicates the number of stores, beginning with the child, of the backing store chain to open as read/write.
  ///    The remaining stores in the differencing chain will be opened read-only. This is necessary for merge operations to succeed.
  ///
  ///    0 = Do not open for read/write at any depth. This value should be used for read-only operations.
  ///
  ///    OPEN_VIRTUAL_DISK_RW_DEPTH_DEFAULT = 1 = Default value to use if no other value is desired.
  ///
  ///    n = user-defined = This integer value should be the number of merge levels plus one, if a merge operation is intended.
  ///

  Result:= OpenVirtualDisk(@pVST, path, accessMask, Flags, @pParam, @AHandle);

  if Result <> ERROR_SUCCESS then
     AHandle:= INVALID_HANDLE_VALUE;
end;

// attach vhd file as local drive (this will need admin rights)
function VirtualDiskAttach(AHandle: THandle; Flags: TAttachVirtualDiskFlag = ATTACH_VIRTUAL_DISK_FLAG_PERMANENT_LIFETIME; securityDescriptor: Pointer = nil; overlapped: POverlapped = nil): Cardinal;
var
  pParam: TAttachVirtualDiskParameters;
begin
  pParam.Version:= ATTACH_VIRTUAL_DISK_VERSION_1;
  pParam.Version1.Reserved:= 0;
  //the values above must be set like this

  Result:= AttachVirtualDisk(AHandle, SecurityDescriptor, Flags, 0, @pParam, Overlapped);
end;

function VirtualDiskDetach(AHandle: THandle): Cardinal;
begin
  Result:= DetachVirtualDisk(AHandle, DETACH_VIRTUAL_DISK_FLAG_NONE, 0);
end;

// get vhd path from handle
function VirtualDiskGetDiskPath(AHandle: THandle; var APath: WideString): Cardinal;
var
  buf: PWideChar;
  nSize: ULONG;
begin
  APath:= '';
  nSize:= MAX_PATH * sizeof(WideChar);
  GetMem(buf, nSize);
  ZeroMemory(buf, nSize);

  Result:= GetVirtualDiskPhysicalPath(AHandle, @nSize, buf);
  if Result = ERROR_SUCCESS then
     SetString(APath, buf, nSize);
  FreeMem(buf);
end;

function VirtualDiskQueryInformation(AHandle: THandle; var VirtualDiskInfo: PGetVirtualDiskInfo; Version: TGetVirtualDiskInfoVersion): Cardinal;
var
  nSize: ULONG;
  nSizeUsed: ULONG;
begin
  nSize:= sizeof(GET_VIRTUAL_DISK_INFO);
  VirtualDiskInfo:= AllocMem(nSize);

  VirtualDiskInfo.Version:= Version; //the type of information you want to receive
                                     //see TGetVirtualDiskInfoVersion
  nSizeUsed:= 0;

  Result:= GetVirtualDiskInformation(AHandle, @nSize, VirtualDiskInfo, @nSizeUsed);
  if (Result = ERROR_INSUFFICIENT_BUFFER) then
  begin
    //if the buffersize was to small realloc the buffer to nsize and call again
    ReAllocMem(VirtualDiskInfo, nSize);
    Result:= GetVirtualDiskInformation(AHandle, @nSize, VirtualDiskInfo, @nSizeUsed);
  end;
end;

procedure TMainForm.btnInfoClick(Sender: TObject);
var
  dwRet: Cardinal;
  hVirtualDisk: THandle;
  gvdi: PGET_VIRTUAL_DISK_INFO;
  szDiskPath: WideString;
begin
  if OpenDialogVHD.Execute then
  begin
    MemoInfo.Lines.Clear;
    // open then vhd file with full access rights and default open flags
    dwRet:= VirtualDiskOpen(PWideChar(OpenDialogVHD.FileName), VIRTUAL_DISK_ACCESS_ALL, hVirtualDisk, OPEN_VIRTUAL_DISK_FLAG_NONE);
    if dwRet = ERROR_SUCCESS then
    begin
      MemoInfo.Lines.Add('Filename'#9#9': '+OpenDialogVHD.FileName);
      // get the drive if the vhd is attached
      dwRet:= VirtualDiskGetDiskPath(hVirtualDisk, szDiskPath);
      if dwRet = ERROR_SUCCESS then
      begin
        MemoInfo.Lines.Add('DiskPath'#9#9': '+szDiskPath);
      end else
      begin
        MemoInfo.Lines.Add('DiskPath'#9#9': '+SysErrorMessage(dwRet));
      end;

      // get size infos
      dwRet:= VirtualDiskQueryInformation(hVirtualDisk, gvdi, GET_VIRTUAL_DISK_INFO_SIZE);
      if dwRet = ERROR_SUCCESS then
      begin
        MemoInfo.Lines.Add(Format('VirtualSize'#9': %d', [gvdi^.Size.VirtualSize]));
        MemoInfo.Lines.Add(Format('PhysicalSize'#9': %d', [gvdi^.Size.PhysicalSize]));
        MemoInfo.Lines.Add(Format('Blocksize'#9#9': %d', [gvdi^.Size.Blocksize]));
        MemoInfo.Lines.Add(Format('SectorSize'#9': %d', [gvdi^.Size.SectorSize]));
      end else
      begin
        MemoInfo.Lines.Add('GET_VIRTUAL_DISK_INFO_SIZE'#9': '+SysErrorMessage(dwRet));
      end;

      // get identifier
      dwRet:= VirtualDiskQueryInformation(hVirtualDisk, gvdi, GET_VIRTUAL_DISK_INFO_IDENTIFIER);
      if dwRet = ERROR_SUCCESS then
      begin
        MemoInfo.Lines.Add(Format('Identifier'#9#9': %s', [GuidToString(gvdi^.Identifier)]));
      end else
      begin
        MemoInfo.Lines.Add('GET_VIRTUAL_DISK_INFO_IDENTIFIER'#9': '+SysErrorMessage(dwRet));
      end;

      //  get storage type
      dwRet:= VirtualDiskQueryInformation(hVirtualDisk, gvdi, GET_VIRTUAL_DISK_INFO_VIRTUAL_STORAGE_TYPE);
      if dwRet = ERROR_SUCCESS then
      begin
        MemoInfo.Lines.Add(Format('Device ID'#9#9': %d', [gvdi^.VirtualStorageType.DeviceId]));
        MemoInfo.Lines.Add(Format('Vendor ID'#9': %s', [GuidToString(gvdi^.VirtualStorageType.VendorId)]));
      end else
      begin
        MemoInfo.Lines.Add('GET_VIRTUAL_DISK_INFO_VIRTUAL_STORAGE_TYPE'#9': '+SysErrorMessage(dwRet));
      end;


      // the following is only supported if the disk has a parent
      dwRet:= VirtualDiskQueryInformation(hVirtualDisk, gvdi, GET_VIRTUAL_DISK_INFO_PARENT_LOCATION);
      if dwRet = ERROR_SUCCESS then
      begin
        if gvdi^.ParentLocation.ParentResolved then
        begin
           MemoInfo.Lines.Add('Parentlocation : '+ PWideChar(@gvdi^.ParentLocation.ParentLocationBuffer));
        end else
           MemoInfo.Lines.Add('Parentlocation : none');
      end else
      begin
        MemoInfo.Lines.Add('GET_VIRTUAL_DISK_INFO_PARENT_LOCATION'#9': '+SysErrorMessage(dwRet));
      end;

      dwRet:= VirtualDiskQueryInformation(hVirtualDisk, gvdi, GET_VIRTUAL_DISK_INFO_PARENT_IDENTIFIER);
      if dwRet = ERROR_SUCCESS then
      begin
        MemoInfo.Lines.Add(Format('Identifier : %s', [GuidToString(gvdi^.Identifier)]));
      end else
      begin
        MemoInfo.Lines.Add('GET_VIRTUAL_DISK_INFO_PARENT_IDENTIFIER'#9': '+SysErrorMessage(dwRet));
      end;

    end else
    begin
      raise Exception.Create('VirtualDiskOpen: '+SysErrorMessage(dwRet));
    end;
  end;
  // close the handle
  if hVirtualDisk <> INVALID_HANDLE_VALUE then
     CloseHandle(hVirtualDisk);
end;

procedure TMainForm.btnMountClick(Sender: TObject);
var
  dwRet: Cardinal;
  hVirtualDisk: THandle;
begin
  if OpenDialogVHD.Execute then
  begin
    MemoInfo.Lines.Clear;
    // open then vhd file with full access rights and default open flags
    dwRet:= VirtualDiskOpen(PWideChar(OpenDialogVHD.FileName), VIRTUAL_DISK_ACCESS_ALL, hVirtualDisk, OPEN_VIRTUAL_DISK_FLAG_NONE);
    if dwRet = ERROR_SUCCESS then
    begin
      // mount the image
      // if the image is an empty Virtual Disk you have to inititialize,
      // partition and format the disk before you can use it (see disk management or WinIoctls)
      dwRet:= VirtualDiskAttach(hVirtualDisk);
      if dwRet = ERROR_SUCCESS then
      begin
        MemoInfo.Lines.Add('Successfully mounted Image '+OpenDialogVHD.FileName);
      end else
      begin
        MemoInfo.Lines.Add('VirtualDiskAttach: '+SysErrorMessage(dwRet));
      end;
    end else
    begin
      MemoInfo.Lines.Add('VirtualDiskOpen: '+SysErrorMessage(dwRet));
    end;
  end;
  // close the handle
  if hVirtualDisk <> INVALID_HANDLE_VALUE then
     CloseHandle(hVirtualDisk);
end;

procedure TMainForm.btnDismountClick(Sender: TObject);
var
  dwRet: Cardinal;
  hVirtualDisk: THandle;
begin
  if OpenDialogVHD.Execute then
  begin
    MemoInfo.Lines.Clear;
    // open then vhd file with full access rights and default open flags
    dwRet:= VirtualDiskOpen(PWideChar(OpenDialogVHD.FileName), VIRTUAL_DISK_ACCESS_ALL, hVirtualDisk, OPEN_VIRTUAL_DISK_FLAG_NONE);
    if dwRet = ERROR_SUCCESS then
    begin
      // dismount the image
      dwRet:= VirtualDiskDetach(hVirtualDisk);
      if dwRet = ERROR_SUCCESS then
      begin
        MemoInfo.Lines.Add('Successfully dismounted Image '+OpenDialogVHD.FileName);
      end else
      begin
        MemoInfo.Lines.Add('VirtualDiskDetach: '+SysErrorMessage(dwRet));
      end;
    end else
    begin
      MemoInfo.Lines.Add('VirtualDiskOpen: '+SysErrorMessage(dwRet));
    end;
  end;
  // close the handle
  if hVirtualDisk <> INVALID_HANDLE_VALUE then
     CloseHandle(hVirtualDisk);
end;

end.
