{******************************************************************************}
{ JEDI FileIsInUse Example Project                                             }
{ http://jedi-apilib.sourceforge.net                                           }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ Author(s): Christian Wimmer                                                  }
{ Creation date: 6th October 2010                                              }
{ Last modification date: 6th October 2010                                     }
{                                                                              }
{ Description: Shows how to use the IFileIsInUse API                           }
{                                                                              }
{ Preparations: JWA must be ready to use.                                      }
{               Requires at least Windows Vista                                }
{                                                                              }
{ Version history: 14th November 2010 initial release                          }
{                                                                              }
{ No license. Use this example with no warranty at all and on your own risk.   }
{ This example is just for learning purposes and should not be used in         }
{ productive environments.                                                     }
{ The code has surely some errors that need to be fixed. In such a case        }
{ you can contact the author(s) through the JEDI API hompage, the mailinglist  }
{ or via the article link.                                                     }
{                                                                              }
{******************************************************************************}
unit MainForm;

interface

{Define this switch to use the definition of the IFileIsInUse interface from
 the JEDI API units.
 Undefine it, to use it from the file here.
}
{$DEFINE JWA_BUILTIN_IFILEISINUSE}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActiveX, ComObj,
  JwaWinType,
  JwaWinUser,
  JwaShlObj,
  JwaShellAPI,
  JwaActiveX, //just make sure JwaActiveX is following ActiveX
  JwaStrSafe,

  StdCtrls, ExtCtrls;

type
  TFormMain = class(TForm, IFileIsInUse, IDropTarget)
    btnOpen: TButton;
    btnClose: TButton;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    edtFileName: TEdit;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
  private
    { Private-Deklarationen }
    fFile : TFileStream;
    FRefCount : Integer;
    fDropHelper : IDropTargetHelper;
    ROTCookie : Integer;
  public
    constructor Create(AOwner: TComponent); override;

    { Interface-Deklarationen }

    function GetAppName(out ppszName: LPWSTR) : HRESULT; stdcall;
    function GetUsage(out pfut : FILE_USAGE_TYPE) : HRESULT; stdcall;
    function GetCapabilities(out pdwCapFlags : DWORD) : HRESULT; stdcall;
    function GetSwitchToHWND(out phwnd : HWND) : HRESULT; stdcall;
    function CloseFile() : HRESULT; stdcall;

    function DragEnter(const dataObj: IDataObject; grfKeyState: DWORD;
      pt: TPoint; var dwEffect: DWORD): HResult; stdcall;
    function DragOver(grfKeyState: DWORD; pt: TPoint;
      var dwEffect: DWORD): HResult; reintroduce; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint;
      var dwEffect: DWORD): HResult; stdcall;

    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure RegisterObjectInROT(const FileName : String);
    procedure OpenFile(const FileName : String);
    procedure CloseTheFile;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}



procedure TFormMain.btnCloseClick(Sender: TObject);
begin
  CloseFile;
end;

procedure TFormMain.btnOpenClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    OpenFile(OpenDialog1.FileName);
  end;
end;

procedure TFormMain.CloseTheFile;
var
  ROT : ActiveX.IRunningObjectTable;
begin
  FreeAndNil(fFile);
  edtFileName.Clear;

  GetRunningObjectTable(0, ROT);
  if Assigned(ROT) and (ROTCookie <> 0) then
  begin
    ROT.Revoke(ROTCookie);
    ROTCookie := 0;
  end;
end;

constructor TFormMain.Create(AOwner: TComponent);
begin
  inherited;
  FRefCount := 0;
end;

function TFormMain.CloseFile: HRESULT;
begin
  CloseTheFile;
  result := S_OK;
end;


procedure TFormMain.FormCreate(Sender: TObject);
begin
  fDropHelper := CreateComObject(CLSID_DragDropHelper) as IDropTargetHelper;

  RegisterDragDrop(Handle, Self);
  ROTCookie := 0;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  CloseTheFile;
end;

function TFormMain.GetAppName(out ppszName: LPWSTR): HRESULT;
begin
  ppszName := LPWSTR(CoTaskMemAlloc((Length(Self.Caption) +1) * sizeof(WCHAR)));
  result := StringCchCopy(ppszName, Length(Self.Caption)+1, PWideChar(Self.Caption));
  if Failed(result) then
  begin
    CoTaskMemFree(ppszName);
    ppszName := nil;
  end;
end;

function TFormMain.GetCapabilities(out pdwCapFlags: DWORD): HRESULT;
begin
  pdwCapFlags := OF_CAP_CANSWITCHTO or OF_CAP_CANCLOSE;
  result := S_OK;
end;

function TFormMain.GetSwitchToHWND(out phwnd: HWND): HRESULT;
begin
  phwnd := Handle;
  result := S_OK;
end;

function TFormMain.GetUsage(out pfut: FILE_USAGE_TYPE): HRESULT;
begin
  pfut := FUT_EDITING;
  result := S_OK;
end;

procedure TFormMain.OpenFile(const FileName: String);
begin
  fFile.Free;
  fFile := TFileStream.Create(FileName, fmOpenRead or fmShareExclusive);
  edtFileName.Text := FileName;
  try
    RegisterObjectInROT(FileName);
  except
    CloseTheFile;
    raise;
  end;
end;



procedure TFormMain.RegisterObjectInROT(const FileName : String);
var
  ROT : ActiveX.IRunningObjectTable;
  FileMoniker : ActiveX.IMoniker;
  hres : HRESULT;
begin
  OleCheck(GetRunningObjectTable(0, ROT));
  OleCheck(CreateFileMoniker(PWideChar(FileName), FileMoniker));

  hres := ROT.Register(ROTFLAGS_REGISTRATIONKEEPSALIVE or ROTFLAGS_ALLOWANYCLIENT, Self, FileMoniker, ROTCookie);
  if (hres = CO_E_WRONG_SERVER_IDENTITY) then
  begin
    OleCheck(ROT.Register(ROTFLAGS_REGISTRATIONKEEPSALIVE, Self, FileMoniker, ROTCookie));
  end
  else
    OleCheck(hres);
end;



// *** Implementation of IDropTarget

function TFormMain.DragEnter(const dataObj: IDataObject; grfKeyState: DWORD;
  pt: TPoint; var dwEffect: DWORD): HResult;
begin
  result := fDropHelper.DragEnter(Handle, dataObj, pt, dwEffect or DROPEFFECT_COPY or DROPEFFECT_LINK or DROPEFFECT_MOVE);
end;

function TFormMain.DragLeave: HResult;
begin
  result := fDropHelper.DragLeave;
end;

function TFormMain.DragOver(grfKeyState: DWORD; pt: TPoint;
  var dwEffect: DWORD): HResult;
begin
  result := fDropHelper.DragOver(pt, dwEffect);
end;

function TFormMain.Drop(const dataObj: IDataObject; grfKeyState: DWORD;
  pt: TPoint; var dwEffect: DWORD): HResult;
var
  aFmtEtc   : TFORMATETC;
  aStgMed   : TSTGMEDIUM;

  szBuffer  : array[0..MAX_PATH] of Char;

begin
  fDropHelper.Drop(dataObj, pt, dwEffect);

  with aFmtEtc do
  begin
    cfFormat := CF_HDROP;
    ptd      := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex   := -1;
    tymed    := TYMED_HGLOBAL;
  end;

  OleCheck(dataObj.GetData(aFmtEtc, aStgMed));
  try
    FillChar(szBuffer, SizeOf(szBuffer), #0);
    DragQueryFile(aStgMed.hGlobal, $FFFFFFFF, @szBuffer, MAX_PATH);

    DragQueryFile(aStgMed.hGlobal, 0, @szBuffer, MAX_PATH);

    OpenFile(szBuffer);
  finally
    ReleaseStgMedium(aStgMed);
  end;

  Result := S_OK;
end;



// *** Implementation of IUnknown

function TFormMain.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  result := inherited;
end;

function TFormMain._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TFormMain._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;



end.
