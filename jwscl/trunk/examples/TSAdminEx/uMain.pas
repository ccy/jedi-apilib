unit uMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, ActnList, ImgList, ToolWin, Menus,
  StdCtrls, StrUtils, CommCtrl,
  VirtualTrees,
  JwaWindows,
  JWsclTerminalServer;
//  VirtualTrees;// Contnrs, RpcWinsta, JwsclEncryption, JwsclTypes,
//  JwsclEnumerations, Internal;

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

  PSessionNodeData = PUserNodeData;
  TSessionNodeData = TUserNodeData;

  PProcessNodeData = ^TProcessNodeData;
  TProcessNodeData = record
    Index: Integer;
    List: PJwWTSProcessList;
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
    VSTSession: TVirtualStringTree;
    VSTServer: TVirtualStringTree;
    VSTProcess: TVirtualStringTree;
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
    Button1: TButton;
    Button5: TButton;
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
//    procedure VSTServerColumnDblClick(Sender: TBaseVirtualTree;
//      Column: TColumnIndex; Shift: TShiftState);
    procedure VSTServerFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure Button1Click(Sender: TObject);
    procedure VSTServerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VSTSessionGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure VSTSessionGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTProcessGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure VSTProcessGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
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

{function I_RpcBindingIsClientLocal(BindingHandle: RPC_BINDING_HANDLE;
  out ClientLocalFlag: Integer): RPC_STATUS; external 'rpcrt4.dll';}

procedure NTCheck(Res: Cardinal);
begin
  If (Res <>0) then
    ShowMessage(SysErrorMessage(RtlNtStatusToDosError(Res)));
end;
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

    // Sychronize User and Session tree's with the SessionList
    UpdateVirtualTree(VSTUser, @Sessions, PrevCount);
    UpdateVirtualTree(VSTSession, @Sessions, PrevCount);
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


procedure TMainForm.FormCreate(Sender: TObject);
var pNode: PVirtualNode;
  pData: PServerNodeData;
  i: Integer;
  pUserNode: PVirtualNode;
  pUserData: PUserNodeData;
  pSessionNode: PVirtualNode;
  pSessionData: PSessionNodeData;
  pProcessNode: PVirtualNode;
  pProcessData: PProcessNodeData;
begin
{.$IFDEF FASTMM}
  ReportMemoryLeaksOnShutDown := DebugHook <> 0;
{.$ENDIF FASTMM}
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

  with pData^.PTerminalServerList^[pData^.Index] do
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

          // Create a node for the session in the Sessions VST
          pSessionNode := VSTSession.AddChild(nil);
          // and add the data
          pSessionData := VSTSession.GetNodeData(pSessionNode);
          // Set the Index
          pSessionData^.Index := i;
          // Point to TerminalServerList.TerminalServer[Index].SessionList
          pSessionData^.List := @Sessions;
        end;
      end;

      // Assign Session Event Handler
      OnSessionEvent := OnTerminalServerEvent;

      if GetAllProcesses then
      begin
        for i := 0 to Processes.Count - 1 do
        begin
          // Create a node for the session
          pProcessNode := VSTProcess.AddChild(nil);
          // and add the data
          pProcessData := VSTProcess.GetNodeData(pProcessNode);
          // Set the Index
          pProcessData^.Index := i;
          // Point to TerminalServerList.TerminalServer[Index].SessionList
          pProcessData^.List := @Processes;
        end;
      end;
    end;

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
    if pData^.List^.Items[pData^.Index].Username <> '' then
    begin
      // show the node!
      if not (vsVisible in Node.States) then
      begin
        Sender.IsVisible[Node] := True;
      end;

      case Column of
        0: CellText := pData^.List^.Items[pData^.Index].Owner.Owner.Server;
        1: CellText := pData^.List^.Items[pData^.Index].Username;
        2: CellText := pData^.List^.Items[pData^.Index].WinStationName;
        3: CellText := IntToStr(pData^.List^.Items[pData^.Index].SessionId);
        4: CellText := pData^.List^.Items[pData^.Index].ConnectStateStr;
        5: CellText := pData^.List^.Items[pData^.Index].IdleTimeStr;
        6: CellText := pData^.List^.Items[pData^.Index].LogonTimeStr;
      end;
    end
    else begin
      // Users Listview only shows sessions that have a user attached!
      Sender.IsVisible[Node] := False;
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

procedure TMainForm.VSTServerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var pServerData: PServerNodeData;
  pUserNode: PVirtualNode;
  pUserData: PUserNodeData;
  pSessionNode: PVirtualNode;
  pSessionData: PSessionNodeData;
  i: Integer;
begin
  with Sender as TVirtualStringTree do
  begin

    pServerData := GetNodeData(GetNodeAt(X, Y));
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
          // Create a node for the session in the Users VST
          pUserNode := VSTUser.AddChild(nil);
          // and add the data
          pUserData := VSTUser.GetNodeData(pUserNode);
          // Set the Index
          pUserData^.Index := i;
          // Point to TerminalServerList.TerminalServer[Index].SessionList
          pUserData^.List := @Sessions;

          // Create a node for the session in the Sessions VST
          pSessionNode := VSTSession.AddChild(nil);
          // and add the data
          pSessionData := VSTSession.GetNodeData(pSessionNode);
          // Set the Index
          pSessionData^.Index := i;
          // Point to TerminalServerList.TerminalServer[Index].SessionList
          pSessionData^.List := @Sessions;
        end;
      end;

      // Assign Event Handler
      OnSessionEvent := OnTerminalServerEvent;
    end;
  end;

  end;
end;

procedure TMainForm.VSTSessionGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TSessionNodeData);
end;

procedure TMainForm.VSTSessionGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var pData: PSessionNodeData;
begin
  pData := Sender.GetNodeData(Node);

  // Do we have data for this session?
  if pData^.List^.Count > pData^.Index then
  begin
    case Column of
      0: CellText := pData^.List^.Items[pData^.Index].Owner.Owner.Server;
      1: CellText := pData^.List^.Items[pData^.Index].Username;
      2: CellText := IntToStr(pData^.List^.Items[pData^.Index].SessionId);
      3: CellText := pData^.List^.Items[pData^.Index].ConnectStateStr;
      4: CellText := pData^.List^.Items[pData^.Index].WinStationDriverName;
      5: CellText := pData^.List^.Items[pData^.Index].ClientName;
      6: CellText := pData^.List^.Items[pData^.Index].IdleTimeStr;
      7: CellText := pData^.List^.Items[pData^.Index].LogonTimeStr;
      8: CellText := pData^.List^.Items[pData^.Index].RemoteAddress;
    end;
    // Show Session Counters only for Active and non-console sessions:
    if (pData^.List^.Items[pData^.Index].ConnectState = WTSActive) and
      (pData^.List^.Items[pData^.Index].WdFlag > WD_FLAG_CONSOLE) then
    begin
      case Column of
        9: CellText := IntToStr(pData^.List^.Items[pData^.Index].IncomingBytes);
        10: CellText := IntToStr(pData^.List^.Items[pData^.Index].OutgoingBytes);
        11: CellText := pData^.List^.Items[pData^.Index].CompressionRatio;
      end;
    end
    // Set empty value for In- and OutgoingBytes and CompressionRatio
    else if Column > 8 then
    begin
      CellText := '';
    end;
  end;
end;

{procedure TMainForm.VSTServerColumnDblClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
var pServerData: PServerNodeData;
  pUserNode: PVirtualNode;
  pUserData: PUserNodeData;
  pSessionNode: PVirtualNode;
  pSessionData: PSessionNodeData;
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

          // Create a node for the session in the Sessions VST
          pSessionNode := VSTSession.AddChild(nil);
          // and add the data
          pSessionData := VSTSession.GetNodeData(pSessionNode);
          // Set the Index
          pSessionData^.Index := i;
          // Point to TerminalServerList.TerminalServer[Index].SessionList
          pSessionData^.List := @Sessions;
        end;
      end;

      // Assign Event Handler
      OnSessionEvent := OnTerminalServerEvent;
    end;
  end;
end;}

procedure TMainForm.VSTProcessGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TProcessNodeData);
end;

procedure TMainForm.VSTProcessGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var pData: PProcessNodeData;
  FormatSettings: TFormatSettings;
begin
  // Get LocaleFormatSettings, we use them later on to format DWORD
  // values with bytes as KByte values with thousand seperators a la taskmgr
  // (we query this every time because the user may have changed settings)
  GetLocaleFormatSettings(LOCALE_USER_DEFAULT, FormatSettings);
  pData := Sender.GetNodeData(Node);

  // Do we have data for this session?
  if pData^.List^.Count > pData^.Index then
  begin
    case Column of
      0: CellText := pData^.List^.Items[pData^.Index].Owner.Owner.Server;
      1: CellText := pData^.List^.Items[pData^.Index].Username;
      2: CellText := pData^.List^.Items[pData^.Index].WinStationName;
      3: CellText := IntToStr(pData^.List^.Items[pData^.Index].SessionId);
      4: CellText := IntToStr(pData^.List^.Items[pData^.Index].ProcessId);
      5: CellText := pData^.List^.Items[pData^.Index].ProcessName;
      6: CellText := pData^.List^.Items[pData^.Index].ProcessName;
      7: CellText := pData^.List^.Items[pData^.Index].ProcessAge;
      8: CellText := pData^.List^.Items[pData^.Index].ProcessCPUTime;
      9: CellText := pData^.List^.Items[pData^.Index].ProcessCPUTime;
      10: CellText := Format('%.0n K', [
        pData^.List^.Items[pData^.Index].ProcessMemUsage / 1024], FormatSettings);
      11: CellText := Format('%.0n K', [
        pData^.List^.Items[pData^.Index].ProcessVMSize / 1024], FormatSettings);
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
