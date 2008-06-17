unit ProcessList;

interface
uses JwaWindows, Classes;

type
  THandles = array of THandle;

  TOnCloseAppsPrePrep = procedure(out NewProcessHandle : DWORD) of object;
  TOnCloseAppsPostPrep = procedure(const Handles : THandles) of object;

  TProcessList = class
  protected
    fCloseAll : Boolean;
    fOnCloseAppsPrePrep  : TOnCloseAppsPrePrep;
    fOnCloseAppsPostPrep : TOnCloseAppsPostPrep;
    fList : TList;

    function GetProcessHandle(Index : Integer) : THandle;
    function GetProcessID(Index : Integer) : DWORD;

  public
    constructor Create;
    destructor Destroy;

    function Add(const Handle : THandle; const Duplicate : Boolean = false) : Integer;
    procedure Remove(const Index : Integer; const Close : Boolean = true);

    procedure CloseAll;

    class procedure SendEndSessionMessage(const Wnd : HWND);    

  public
    property CloseAllOnDestroy : Boolean read fCloseAll write fCloseAll;
    property Process[Index : Integer] : THandle read GetProcessHandle; default;
    property ProcessID[Index : Integer] : DWORD read GetProcessID;

    property OnCloseAppsPrePrep  : TOnCloseAppsPrePrep read fOnCloseAppsPrePrep write fOnCloseAppsPrePrep;
    property OnCloseAppsPostPrep : TOnCloseAppsPostPrep read fOnCloseAppsPostPrep write fOnCloseAppsPostPrep;
  end;

implementation
uses SysUtils;

{ TProcessList }

function TProcessList.Add(const Handle: THandle;
  const Duplicate: Boolean): Integer;
begin

end;

procedure TProcessList.CloseAll;
begin

end;

constructor TProcessList.Create;
begin
  fList := TList.Create;
end;

destructor TProcessList.Destroy;
begin
  FreeAndNil(fList);
end;

function TProcessList.GetProcessHandle(Index: Integer): THandle;
begin

end;

function TProcessList.GetProcessID(Index: Integer): DWORD;
begin

end;

procedure TProcessList.Remove(const Index: Integer; const Close: Boolean);
begin

end;

class procedure TProcessList.SendEndSessionMessage(const Wnd: HWND);
begin

end;

end.
