unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, ActnList, ImgList, ToolWin, Menus,
  StdCtrls, Math, StrUtils, CommCtrl,
  JWsclTerminalServer, JwaWindows, AppEvnts,
  VirtualTrees, Contnrs;

type
  PServerNodeData = ^TServerNodeData;
  TServerNodeData = record
    Index: Integer;
    Caption: String;
    PTerminalServerList: PJwTerminalServerList;
  end;

  PUserNodeData = ^TUserNodeData;
  TUserNodeData = record
    Index: Integer;
    List: PJwWTSSessionList;
  end;

// Class below is used to store imageindex of icons in the Imagelist
TIconRec = record
  Server: WORD;
  User: WORD;
  Network: WORD;
  Process: WORD;
  Console: WORD;
end;

type
  TMainForm = class(TForm)
    VSTUser: TVirtualStringTree;
    VSTServer: TVirtualStringTree;
    MainMenu1: TMainMenu;
    ToolBar1: TToolBar;
    ImageList1: TImageList;
    ActionList1: TActionList;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    actConnect: TAction;
    actDisconnect: TAction;
    actSendMessage: TAction;
    actRemoteControl: TAction;
    actReset: TAction;
    actStatus: TAction;
    ActLogoff: TAction;
    ActEndProcess: TAction;
    actRefresh: TAction;
    Actions1: TMenuItem;
    View1: TMenuItem;
    ools1: TMenuItem;
    Help1: TMenuItem;
    Connect1: TMenuItem;
    Disconnect1: TMenuItem;
    SendMessage1: TMenuItem;
    RemoteControl1: TMenuItem;
    Reset1: TMenuItem;
    Reset2: TMenuItem;
    N1: TMenuItem;
    Logoff1: TMenuItem;
    N2: TMenuItem;
    EndProcess1: TMenuItem;
    Disconnectfromallserversindomain1: TMenuItem;
    N3: TMenuItem;
    Connecttocomputer1: TMenuItem;
    RefreshServerinAllDomains1: TMenuItem;
    DisconnectfromAllServers1: TMenuItem;
    Emptyfavorites1: TMenuItem;
    N4: TMenuItem;
    Exit1: TMenuItem;
    ToolButton14: TToolButton;
    StatusBar1: TStatusBar;
    Splitter1: TSplitter;
    Panel1: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    ApplicationEvents1: TApplicationEvents;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure VSTUserGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure VSTUserGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure VSTUserGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTUserCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTUserHeaderClick(Sender: TVTHeader;
      Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure VSTServerGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTServerGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure VSTServerColumnDblClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
    procedure VSTServerFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    TerminalServers: TJwTerminalServerList;
    pThisComputerNode: PVirtualNode;
    pAllListedServersNode: PVirtualNode;
    procedure UpdateVirtualTree(const AVirtualTree: TBaseVirtualTree;
      const PSessionList: PJwWTSSessionList; PrevCount: Integer);
    procedure OnTerminalServerEvent(Sender: TObject);
    procedure OnEnumerateServersDone(Sender: TObject);
  public
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  IconRec: TIconRec;
implementation

{$R *.dfm}
procedure TMainForm.UpdateVirtualTree(const AVirtualTree: TBaseVirtualTree;
  const PSessionList: PJwWTSSessionList; PrevCount: Integer);
var i: Integer;
  pNode: PVirtualNode;
  pPrevNode: PVirtualNode;
  pData: PUserNodeData;
  NewCount: Integer;
begin
  // Get the last node
  pNode := AVirtualTree.GetLast;

  // Now iterate from last to first node
  repeat
    pData := AVirtualTree.GetNodeData(pNode);
    // Get the previous node and store the pointer, because in the next step
    // we might delete the current node ;-)
    pPrevNode := AVirtualTree.GetPrevious(pNode);

    // Is the node data pointing to PSessionList and do we need to delete it?
    if (pData^.List = PSessionList) and
      (pData^.Index > PSessionList^.Count-1) then
    begin
      // Delete the node (we have no Session Data for it)
      AVirtualTree.DeleteNode(pNode);
    end
    else begin
      // Invalidating the node will trigger the GetText event which will update
      // our data
      AVirtualTree.InvalidateNode(pNode);
    end;
    pNode := pPrevNode;
  until pNode = nil;

  // How many new sessions are there?
  NewCount := PSessionList^.Count - PrevCount;

  // Create a new node for each new session
  for i := 0 to NewCount-1 do
  begin
    pNode := AVirtualTree.AddChild(nil);
    pData := AVirtualTree.GetNodeData(pNode);
    pData^.Index := PrevCount;
    pData^.List := PSessionList;
  end;
end;

procedure TMainForm.OnTerminalServerEvent(Sender: TObject);
var PrevCount: Integer;
begin
  with (Sender as TJwTerminalServer) do
  begin
    // Get the previous session count
    PrevCount := Sessions.Count;

    // Enumerate sessions
    EnumerateSessions;

    // Sychronize tree with SessionList
    UpdateVirtualTree(VSTUser, @Sessions, PrevCount);
  end;
end;

procedure TMainForm.OnEnumerateServersDone(Sender: TObject);
var i: Integer;
  pNode: PVirtualNode;
  pData: PServerNodeData;
begin
  with Sender as TJwTerminalServer do
  begin
    for i := 0 to ServerList.Count-1 do
    begin
      pNode := VSTServer.AddChild(pAllListedServersNode);
      pData := VSTServer.GetNodeData(pNode);
      pData^.Index := -1;
      pData^.Caption := ServerList.Strings[i];
      pData^.PTerminalServerList := nil;
    end;
  end;

  VSTServer.FullExpand(pAllListedServersNode);
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  TerminalServers[0].EnumerateServersEx;
end;

function GetVKText(VK: UINT): String;
var Buf: array[0..255] of char;
  sc: Integer;
  asc: Boolean;
  Temp: Integer;
begin
  sc := MapVirtualKey(VK, 0);
  ZeroMemory(@Buf, SizeOf(Buf));

  // Is vk ASCII?
  asc := VK <= 32;
  if (not asc) and (vk <> VK_DIVIDE) then
  begin
    asc := ToAscii(vk, sc, @buf, @Temp, 1) > 0;
  end;

  // Set bits
  sc := sc shl 16;
  sc := sc or $1 shl 25; // <- don't care

  if (not asc) then
  begin
    sc := sc or $1 shl 24; // <- extended bit
  end;

  // Convert to string
  if GetKeyNameText(sc, buf, 256) > 0 then
  begin
    Result := Buf;
  end;
end;

procedure ClientResize(const ClientHandle: HWND; const IncWidthBy: Integer;
  const IncHeightBy: Integer);
var lpRect: RECT;
  ParentHandle: HWND;
  NewWidth: Integer;
  NewHeight: Integer;
begin
  // Get Parent Window Handle
  ParentHandle := GetParent(ClientHandle);
  GetWindowRect(ClientHandle, lpRect);

  // Convert lpRect to client-area coordinates
  ScreenToClient(ParentHandle, tagPoint(lpRect.TopLeft));
  ScreenToClient(ParentHandle, tagPoint(lpRect.BottomRight));

  // Calculate New Width and Height
  NewWidth := (lpRect.Right-lpRect.Left) + IncWidthBy;
  NewHeight := (lpRect.Bottom-lpRect.Top) + IncHeightBy;

  // Resize the Window
  MoveWindow(ClientHandle, lpRect.Left, lpRect.Top, NewWidth, NewHeight, True);
end;

procedure InitComboBox(hCB: HWND);
var Index: Integer;
  i: Integer;
  Default: Integer;
  lpRect: TRect;
  ItemHeight: Integer;
  IncHeight: Integer;
function AddComboBoxItem(const s: string; VK: UINT): Integer;
var t: string;
begin
  if (Length(s) = 1) and ((s > '/') and (s < '[')) then
  begin
    t := s;
  end
  else begin
    t := '{' + s + '}';
  end;

  SendMessage(hCB, CB_ADDSTRING, 0, Longint(PChar(t)));
  SendMessage(hCB, CB_SETITEMDATA, Index, VK);
  Result := Index;
  Inc(Index);
end;
begin
  for i := 0 to 9 do
  begin
    AddComboBoxItem(IntToStr(i), 30);
  end;

  for i := Ord('A') to Ord('Z') do
  begin
    AddComboBoxItem(Chr(i), 41 + i);
  end;

  AddComboBoxItem('backspace', VK_BACK);
  AddComboBoxItem('delete', VK_DELETE);
  AddComboBoxItem('down', VK_DOWN);
  AddComboBoxItem('enter', VK_RETURN);

  for i := 2 to 12 do
  begin
    AddComboBoxItem('F' + IntToStr(i), VK_F2 + i);
  end;

  AddComboBoxItem('home', VK_HOME);
  AddComboBoxItem('insert', VK_INSERT);
  AddComboBoxItem('left', VK_LEFT);
  AddComboBoxItem('-', VK_SUBTRACT);
  AddComboBoxItem('pagedown', VK_NEXT);
  AddComboBoxItem('pageup', VK_PRIOR);
  AddComboBoxItem('+', VK_ADD);
  AddComboBoxItem('prtscrn', VK_SNAPSHOT);
  AddComboBoxItem('right', VK_RIGHT);
  AddComboBoxItem('spacebar', VK_SPACE);
  Default := AddComboBoxItem('*', VK_MULTIPLY);
  AddComboBoxItem('tab', VK_TAB);
  Index := AddComboBoxItem('up', VK_UP);

  // Get the height of a single item
  ItemHeight := SendMessage(hCB, CB_GETITEMHEIGHT, 0, 0);

  // We want to show 28 extra items
  IncHeight := ItemHeight * 28;

  // Resize the combobox drop down height
  ClientResize(hCB, 0, IncHeight);

  // Select Default Combobox Item
  SendMessage(hCB, CB_SETCURSEL, Default, 0);
end;

function MyDialogProc(hwndDlg: HWND; uMsg: UINT; wParam: WPARAM;
  lParam: LPARAM): BOOL; stdcall;
var i: Integer;
  s: String;
  c: Char;
  hComboBox: THandle;
  ShiftChecked: Boolean;
  ControlChecked: Boolean;
  AltChecked: Boolean;
begin
  Result := False;
  case uMsg of
    WM_INITDIALOG:
    begin
      Result := True;
      InitComboBox(GetDlgItem(hwndDlg, 241));
    end;
    WM_SYSCOMMAND:
    begin
      if wParam = SC_CLOSE then
      begin
        EndDialog(hwndDlg, IDCANCEL);// same as cancel button
      end;
    end;
    WM_COMMAND:
    begin
      if HiWord(wParam) = BN_CLICKED then
      begin
        Result := True;
        case LoWord(wParam) of
          1:
          begin
            ShiftChecked := SendMessage(GetDlgItem(hwndDlg, 242), BM_GETCHECK, 0, 0) = BST_CHECKED;
            ControlChecked := SendMessage(GetDlgItem(hwndDlg, 243), BM_GETCHECK, 0, 0) = BST_CHECKED;
            AltChecked := SendMessage(GetDlgItem(hwndDlg, 244), BM_GETCHECK, 0, 0) = BST_CHECKED;
            ShowMessageFmt('Shift: %d Control: %d Alt: %d', [Ord(ShiftChecked), Ord(ControlChecked), Ord(AltChecked)]);
//            EndDialog(hwndDlg, IDOK);
          end;
          2: EndDialog(hwndDlg, IDCANCEL);
        end;
      end;
    end;
  end;
end;


procedure TMainForm.Button2Click(Sender: TObject);
var TaskMgr : HModule;
  Buffer: array[0..255] of char;
  sc: Integer;
  asc: Boolean;
  s: string;
  Res: Integer;
begin
  TaskMgr := LoadLibrary('taskmgr.exe');
  if TaskMgr <> 0 then
  begin
    try
      Res := DialogBox(TaskMgr, MakeIntResource(240), Handle, @MyDialogProc);
      if Res = IDOK then
      begin
        ShowMessage('shadow');
      end
      else begin
        ShowMessage('no shadow');
      end;
      
    finally
      FreeLibrary(TaskMgr);
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var pNode: PVirtualNode;
  pData: PServerNodeData;
begin
  ReportMemoryLeaksOnShutDown := DebugHook <> 0;
  TerminalServers := TJwTerminalServerList.Create;
  TerminalServers.Owner := Self;

  // Create the 'This Computer' parent node
  pThisComputerNode := VSTServer.AddChild(nil);
  pData := VSTServer.GetNodeData(pThisComputerNode);

  // This is a node without a Terminal Server instance attached so we set
  // Index to -2 (never add a Terminal Server instance to it) and Pointer to nil
  pData^.Caption := 'This Computer';
  pData^.Index := -2;
  pData^.PTerminalServerList := nil;

  // Add a child node (local computer)
  pNode := VSTServer.AddChild(pThisComputerNode);
  pData := VSTServer.GetNodeData(pNode);

  // Point the node data to a Terminal Server instance
  pData^.Index := TerminalServers.Add(TjwTerminalServer.Create);
  pData^.PTerminalServerList := @TerminalServers;

  // Expand the This Computer Node
  VSTServer.FullExpand(pThisComputerNode);

  // Create the 'All Listed Servers' parent node
  pAllListedServersNode := VSTServer.AddChild(nil);
  pData := VSTServer.GetNodeData(pAllListedServersNode);

  // This is a node without a Terminal Server instance attached so we set
  // Index to -2 (never add a Terminal Server instance to it) and Pointer to nil
  pData^.Caption := 'All Listed Servers';
  pData^.Index := -2;
  pData^.PTerminalServerList := nil;
end;

procedure TMainForm.VSTUserGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  pData: PUserNodeData;
begin
  pData := Sender.GetNodeData(Node);

  // Do we have data for this session?
  if pData^.List^.Count > pData^.Index then
  begin
    case Column of
      0: CellText := pData^.List^.Items[pData^.Index].Owner.Owner.Server;
      1: CellText := pData^.List^.Items[pData^.Index].Username;
      2: CellText := pData^.List^.Items[pData^.Index].WinStationName;
      3: CellText := IntToStr(pData^.List^.Items[pData^.Index].SessionId);
      4: CellText := pData^.List^.Items[pData^.Index].ConnectStateStr;
      5: CellText := pData^.List^.Items[pData^.Index].IdleTimeStr;
      6: CellText := pData^.List^.Items[pData^.Index].LogonTimeStr;
    end;
  end;
end;

procedure TMainForm.VSTUserHeaderClick(Sender: TVTHeader;
  Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  with Sender do
  begin
    SortColumn := Column;
    Treeview.SortTree(Column, SortDirection);
    if SortDirection = sdAscending then
    begin
      SortDirection := sdDescending;
    end
    else begin
      SortDirection := sdAscending;
    end;
  end;
end;

procedure TMainForm.VSTUserGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  pData: PUserNodeData;
begin
  if Kind in [ikNormal, ikSelected] then begin
//    pData := Sender.GetNodeData(Node);
  end;
end;

procedure TMainForm.VSTServerGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TServerNodeData);
end;

procedure TMainForm.VSTUserGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TUserNodeData);
end;

procedure TMainForm.VSTUserCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var Data1: PUserNodeData;
  Data2: PUserNodeData;
  Index1: Integer;
  Index2: Integer;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  if (not Assigned(Data1)) or (not Assigned(Data2)) then
  begin
    Result := 0;
  end
  else begin
    Index1 := Data1^.Index;
    Index2 := Data2^.Index;

    // Do we have data of these sessions?
    if (Data1^.List^.Count > Index1) and (Data2^.List^.Count > Index2) then
    begin
      case Column of
        0: begin
          Result := CompareText(Data1^.List^.Items[Index1].Owner.Owner.Server,
            Data2^.List^.Items[Index2].Owner.Owner.Server);
        end;
        1: begin
          Result := CompareText(Data1^.List^.Items[Index1].Username,
            Data2^.List^.Items[Index2].Username);
        end;
        2: begin
          Result := CompareText(Data1^.List^.Items[Index1].WinStationName,
            Data2^.List^.Items[Index2].WinStationName);
        end;
        3: begin
          if Data1^.List^.Items[Index1].SessionId >
            Data2^.List^.Items[Index2].SessionId then
          begin
            Result := 1;
          end
          else if Data1^.List^.Items[Index1].SessionId =
            Data2^.List^.Items[Index2].SessionId then
          begin
            Result := 0;
          end
          else begin
            Result := -1;
          end;
        end;
        4: begin
          Result := CompareText(Data1^.List^.Items[Index1].ConnectStateStr,
            Data2^.List^.Items[Index2].ConnectStateStr);
        end;
        5: begin
          Result := CompareText(Data1^.List^.Items[Index1].IdleTimeStr,
            Data2^.List^.Items[Index2].IdleTimeStr);
        end;
        6: begin
          Result := CompareText(Data1^.List^.Items[Index1].LogonTimeStr,
            Data2^.List^.Items[Index2].LogonTimeStr);
        end;
      end;
    end
    else begin
      // We have no data, so return equal, should not occur!
      Result := 0;
    end;
  end;
end;

procedure TMainForm.VSTServerGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var pData: PServerNodeData;
begin
  pData := Sender.GetNodeData(Node);
  if pData^.PTerminalServerList <> nil then
  begin
    CellText := pData^.PTerminalServerList^[pData^.Index].Server;
  end
  else begin
    CellText := pData^.Caption;
  end;
end;

procedure TMainForm.VSTServerColumnDblClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
var pServerData: PServerNodeData;
  pUserNode: PVirtualNode;
  pUserData: PUserNodeData;
  i: Integer;
begin
  pServerData := Sender.GetNodeData(Sender.FocusedNode);

  // Is this a server node?
  if pServerData^.Index > -2 then
  begin
    // Is a Terminal Server instance assigned?
    if pServerData^.PTerminalServerList = nil then
    begin
      // Create a Terminal Server instance
      pServerData^.Index := TerminalServers.Add(TjwTerminalServer.Create);
      // Set the servername
      TerminalServers[pServerData^.Index].Server := pServerData^.Caption;
      // Point the node data to a Terminal Server instance
      pServerData^.PTerminalServerList := @TerminalServers;
    end;

    with pServerData^.PTerminalServerList^[pServerData^.Index] do
    begin
      // EnumerateSessions
      if EnumerateSessions then
      begin
        for i := 0 to Sessions.Count - 1 do
        begin
          // Create a node for the session
          pUserNode := VSTUser.AddChild(nil);
          // and add the data
          pUserData := VSTUser.GetNodeData(pUserNode);
          // Set the Index
          pUserData^.Index := i;
          // Point to TerminalServerList.TerminalServer[Index].SessionList
          pUserData^.List := @Sessions;
        end;
      end;

      // Assign Event Handler
      OnSessionEvent := OnTerminalServerEvent;
    end;
  end;
end;

procedure TMainForm.VSTServerFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var pData: PServerNodeData;
begin
  pData := Sender.GetNodeData(Node);
  // Free the string data by setting it to ''
  pData^.Caption := '';
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Prevent updates to the Virtual String Grids
  VSTUser.OnGetText := nil;
  VSTServer.OnGetText := nil;

  // Now Free the Terminal Server Instances
  TerminalServers.Free;
end;

end.
