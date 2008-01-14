unit uMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, ActnList, ImgList, ToolWin, Menus,
  StdCtrls, StrUtils, CommCtrl, Math, NetApi,
  VirtualTrees,
  JwaWindows, JwaVista,
  JwsclSid, JwsclTerminalServer;
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
TIconIndex = (icThisComputer, icWorld, icServers, icServersSel, icServer,
  icServerSel, icUserGhosted, icUser, IcNetworkUser, icNetwork, icComputer,
  icProcess, icChip, icMemory, icListener, icVirtual, icCPUTime, icClock,
  icService, icNetworkService, icSystem);

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
    Button2: TButton;
    Timer1: TTimer;
    StateImagesList: TImageList;
    ImageList2: TImageList;
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
    procedure VSTServerGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTServerGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
//    procedure VSTServerColumnDblClick(Sender: TBaseVirtualTree;
//      Column: TColumnIndex; Shift: TShiftState);
    procedure VSTServerFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTSessionGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure VSTSessionGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTProcessGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure VSTProcessGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure Button2Click(Sender: TObject);
    procedure VSTHeaderClick(Sender: TVTHeader;
      Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure VSTSessionCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTProcessCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure VSTServerGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure VSTServerChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTProcessGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure VSTServerCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTServerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
//    procedure VSTServerDblClick(Sender: TObject);
  private
    { Private declarations }
    TerminalServers: TJwTerminalServerList;
    pThisComputerNode: PVirtualNode;
    pAllListedServersNode: PVirtualNode;
    JwNetworkServiceSid: TJwSecurityId;
    JwLocalServiceSid: TJwSecurityId;
    JwLocalSystemSid: TJwSecurityId;
    procedure UpdateVirtualTree(const AVirtualTree: TBaseVirtualTree;
      const PSessionList: PJwWTSSessionList; PrevCount: Integer);
    procedure UpdateProcessVirtualTree(const AVirtualTree: TBaseVirtualTree;
      const PProcessList: PJwWTSProcessList; PrevCount: Integer);
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

{function I_RpcBindingIsClientLocal(BindingHandle: RPC_BINDING_HANDLE;
  out ClientLocalFlag: Integer): RPC_STATUS; external 'rpcrt4.dll';}

function CompareInteger(const int1: Integer; const int2: Integer): Integer; overload;
begin
  if Int1 > Int2 then
  begin
    Result := 1;
  end
  else if Int1 < Int2 then
  begin
    Result := -1;
  end
  else begin
    Result := 0;
  end;
end;

function CompareInteger(const int1: Int64; const int2: Int64): Integer; overload;
begin
  if Int1 > Int2 then
  begin
    Result := 1;
  end
  else if Int1 < Int2 then
  begin
    Result := -1;
  end
  else begin
    Result := 0;
  end;
end;

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

procedure TMainForm.UpdateProcessVirtualTree(const AVirtualTree: TBaseVirtualTree; const PProcessList: PJwWTSProcessList; PrevCount: Integer);
var i: Integer;
  pNode: PVirtualNode;
  pPrevNode: PVirtualNode;
  pData: PProcessNodeData;
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
    if (pData^.List = PProcessList) and
      (pData^.Index > PProcessList^.Count-1) then
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
  NewCount := PProcessList^.Count - PrevCount;

  // Create a new node for each new session
  for i := 0 to NewCount-1 do
  begin
    pNode := AVirtualTree.AddChild(nil);
    pData := AVirtualTree.GetNodeData(pNode);
    pData^.Index := PrevCount;
    pData^.List := PProcessList;
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

procedure TMainForm.Timer1Timer(Sender: TObject);
var i: Integer;
  PrevCount: Integer;
begin
  (Sender as TTimer).Enabled := False;
  for i := 0 to TerminalServers.Count - 1 do
  begin
    PrevCount := TerminalServers[i].Processes.Count;
    TerminalServers[i].EnumerateProcesses;
    UpdateProcessVirtualTree(VSTProcess, @TerminalServers[i].Processes,
      PrevCount);
  (Sender as TTimer).Enabled := True;
  end;
end;

procedure TMainForm.OnEnumerateServersDone(Sender: TObject);
var i: Integer;
  pDomainNode: PVirtualNode;
  pNode: PVirtualNode;
  pData: PServerNodeData;
begin
  with Sender as TJwTerminalServer do
  begin
    pDomainNode := Data;
    for i := 0 to ServerList.Count-1 do
    begin
      pNode := VSTServer.AddChild(pDomainNode);
      pData := VSTServer.GetNodeData(pNode);
      pData^.Index := -1;
      pData^.Caption := ServerList.Strings[i];
      pData^.PTerminalServerList := nil;
      // Assign a checkbox
      pNode^.CheckType := ctCheckBox;
    end;
  end;

  VSTServer.FullExpand(pDomainNode);
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  Timer1.Enabled := not Timer1.Enabled;
end;


procedure AutoSizeVST(const AVirtualTree: TVirtualStringTree);
var i: Integer;
begin
  for i := 0 to AVirtualTree.Header.Columns.Count-1 do
  begin
    AVirtualTree.Header.Columns[i].Width :=
      Max(AVirtualTree.Header.Columns[i].Width, AVirtualTree.GetMaxColumnWidth(i));
  end;
    
end;

procedure TMainForm.FormCreate(Sender: TObject);
var pNode: PVirtualNode;
  pData: PServerNodeData;
  DomainList: TStringList;
  i: Integer;
begin
{$IFDEF FASTMM}
  ReportMemoryLeaksOnShutDown := DebugHook <> 0;
{$ENDIF FASTMM}
  TerminalServers := TJwTerminalServerList.Create;
  TerminalServers.Owner := Self;

  JwLocalSystemSid := TJwSecurityId.CreateWellKnownSid(WinLocalSystemSid);
  JwLocalServiceSid := TJwSecurityId.CreateWellKnownSid(WinLocalServiceSid);
  JwNetworkServiceSid := TJwSecurityId.CreateWellKnownSid(WinNetworkServiceSid);


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

//  VSTServer.CheckState[pThisComputerNode] := csCheckedNormal;
//  pData := VSTServer.GetNodeData(pNode);

  // This node can be checked
  pNode^.CheckType := ctCheckBox;
  // The this computer node is checked by default
  pNode^.CheckState := csCheckedNormal;

  // Trigger the OnChecked Event, this will fill the listviews for this computer
  VSTServer.OnChecked(VSTServer, pNode);

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

  DomainList := EnumerateDomains;

  for i := 0 to DomainList.Count - 1 do
  begin
    pData := VSTServer.GetNodeData(VSTServer.AddChild(pAllListedServersNode));
    pData^.Index := -2;
    pData^.Caption := DomainList[i];
    pData^.PTerminalServerList := nil;
  end;

  VSTServer.FullExpand(pAllListedServersNode);
  DomainList.Free;

  AutoSizeVST(VSTUser);
  AutoSizeVST(VSTSession);
  AutoSizeVST(VSTProcess);
end;

procedure TMainForm.VSTUserGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  pUserData: PUserNodeData;
  CurrentItem: TJwWTSSession;
begin
  pUserData := Sender.GetNodeData(Node);

  // Sometimes VST asks for Column -1 (Autosort??)
  if Column < 0 then Exit;

  // Do we have data for this session?
  if pUserData^.List^.Count > pUserData^.Index then
  begin
    CurrentItem := pUserData^.List^.Items[pUserData^.Index];
    if CurrentItem.Username <> '' then
    begin
      // Show the node if it was InVisible
      if not (vsVisible in Node.States) then
      begin
        Sender.IsVisible[Node] := True;
      end;

      case Column of
        0: CellText := CurrentItem.Owner.Owner.Server;
        1: CellText := CurrentItem.Username;
        2: CellText := CurrentItem.WinStationName;
        3: CellText := IntToStr(CurrentItem.SessionId);
        4: CellText := Format('%s %d %d', [CurrentItem.ConnectStateStr,
          (Ord(CurrentItem.ShadowInformation.ShadowMode)), (Ord(CurrentItem.ShadowInformation.ShadowState))]);
        5: CellText := CurrentItem.IdleTimeStr;
        6: CellText := CurrentItem.LogonTimeStr;
      end;
    end
    else begin
      // Users Listview only shows sessions that have a user attached!
      Sender.IsVisible[Node] := False;
    end;
  end;
end;

procedure TMainForm.VSTHeaderClick(Sender: TVTHeader;
  Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var LastSortColumn: Integer;
begin
  with Sender do
  begin
    LastSortColumn := SortColumn;
    SortColumn := Column;
    if Column = LastSortColumn then
    begin
      if SortDirection = sdAscending then
      begin
        SortDirection := sdDescending;
      end
      else begin
        SortDirection := sdAscending;
      end;
    end;
    // Sort
    Treeview.SortTree(Column, SortDirection);
  end;
end;

procedure TMainForm.VSTUserGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  pData: PUserNodeData;
  Username: string;
  ConnectState: TWtsConnectStateClass;
  IsConsole: Boolean;
  JwUserSid: TjwSecurityId;
begin
  pData := Sender.GetNodeData(Node);

  // Do we have data for this session?
  if pData^.List^.Count > pData^.Index then
  begin
    Username := pData^.List^.Items[pData^.Index].Username;
    ConnectState := pData^.List^.Items[pData^.Index].ConnectState;
    IsConsole := pData^.List^.Items[pData^.Index].WdFlag < WD_FLAG_RDP;

    JwUserSid := TJwSecurityId.Create('', Username);


    if Kind in [ikNormal, ikSelected] then begin
      case column of
        0: ImageIndex := Integer(icServer);
        1: begin
          if Username = '' then
          begin
            ImageIndex := -1; // No Icon!
          end
          else if JwUserSid.EqualSid(jwLocalSystemSid) then
          begin
            ImageIndex := Integer(icSystem);
          end
          else if JwUserSid.EqualSid(jwLocalServiceSid) then
          begin
            ImageIndex := Integer(icService);
          end
          else if JwUserSid.EqualSid(JwNetworkServiceSid) then
          begin
            ImageIndex := Integer(icNetworkService);
          end
          else if ConnectState = WTSActive then
          begin
            ImageIndex := Integer(icUser);
          end
          else begin
            ImageIndex := Integer(icUserGhosted);
          end;
        end;
        2: begin
          if IsConsole then
          begin
            ImageIndex := Integer(icComputer);
          end
          else if ConnectState = WTSListen then
          begin
            ImageIndex := Integer(icListener);
          end
          else if username <> '' then begin
            ImageIndex := Integer(icNetworkUser);
          end
          else if ConnectState = WTSActive then begin
            ImageIndex := Integer(icNetwork);
          end
          else begin
            ImageIndex := -1;
          end;
        end;
      end;
    end;

    // Cleanup
    JwUserSid.Free;
  end;
end;

procedure TMainForm.VSTServerGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  case Kind of
    ikNormal, ikSelected:
    begin
//      if Node^.ChildCount > 0 then
      if Node^.Parent = Sender.RootNode  then
      begin
        // If it is a rootnode then the index in the tree eq imageindex
        ImageIndex := Node^.Index;
      end
      else if Node^.Parent = pAllListedServersNode then
      begin
        ImageIndex := Integer(icServers);
      end
      else begin
        ImageIndex := Integer(icServer);
      end;
    end;
    ikState:
    begin
      if Sender.CheckState[Node] = csCheckedNormal then
      begin
        ImageIndex := 4;
      end
      else begin
        ImageIndex := 1;
      end;
    end;
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
  Session1: TJwWTSSession;
  Session2: TJwWTSSession;
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
    Session1 := Data1^.List^[Index1];
    Session2 := Data2^.List^[Index2];

    // Do we have data of these sessions?
    if (Data1^.List^.Count > Index1) and (Data2^.List^.Count > Index2) then
    begin
      case Column of
        0: Result := CompareText(Session1.Owner.Owner.Server,
            Session2.Owner.Owner.Server);
        1: Result := CompareText(Session1.Username, Session2.UserName);
        2: Result := CompareText(Session1.WinStationName, Session2.WinStationName);
        3: Result := CompareInteger(Session1.SessionId, Session2.SessionId);
        4: Result := CompareText(Session1.ConnectStateStr, Session2.ConnectStateStr);
        5: Result := CompareInteger(Session1.IdleTime, Session2.IdleTime);
        6: Result := CompareInteger(Session1.LogonTime, Session2.LogonTime);
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
  if pData^.Caption = '' then
  begin
    CellText := ThisComputerName;
  end
  else begin
    CellText := pData^.Caption;
  end;

end;

procedure TMainForm.VSTServerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pNode: PVirtualNode;
  pServerData: PServerNodeData;
  strDomain: string;
  ATerminalServer: TJwTerminalServer;
begin
  pNode := VSTServer.GetNodeAt(X, Y);
  if pNode^.Parent = pAllListedServersNode then
  begin
    if pNode^.ChildCount = 0 then
    begin
      pServerData := VSTServer.GetNodeData(pNode);
      strDomain := pServerData^.Caption;

      ATerminalServer := TJwTerminalServer.Create;
      ATerminalServer.OnServersEnumerated := OnEnumerateServersDone;
      // store the node pointer
      ATerminalServer.Data := pNode;

      ATerminalServer.EnumerateServers(strDomain);
    end;
  end;
end;

procedure TMainForm.VSTSessionCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var Data1: PSessionNodeData;
  Data2: PSessionNodeData;
  Index1: Integer;
  Index2: Integer;
  Session1: TJwWTSSession;
  Session2: TJwWTSSession;
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
    Session1 := Data1^.List^[Index1];
    Session2 := Data2^.List^[Index2];

    // Do we have data of these sessions?
    if (Data1^.List^.Count > Index1) and (Data2^.List^.Count > Index2) then
    begin
      case Column of
        0: Result := CompareText(Session1.Owner.Owner.Server,
            Session2.Owner.Owner.Server);
        1: Result := CompareText(Session1.Username, Session2.UserName);
        2: Result := CompareInteger(Session1.SessionId, Session2.SessionId);
        3: Result := CompareText(Session1.ConnectStateStr,
            Session2.ConnectStateStr);
        4: Result := CompareText(Session1.WinStationDriverName,
            Session2.WinStationDriverName);
        5: Result := CompareText(Session1.ClientName, Session2.ClientName);
        6: Result := CompareInteger(Session1.IdleTime, Session2.IdleTime);
        7: Result := CompareInteger(Session1.LogonTime, Session2.LogonTime);
        8: Result := CompareText(Session1.RemoteAddress, Session2.RemoteAddress);
        9: Result := CompareInteger(Session1.IncomingBytes,
            Session2.IncomingBytes);
        10: Result := CompareInteger(Session1.OutgoingBytes,
              Session2.OutgoingBytes);
        11: Result := CompareText(Session1.CompressionRatio,
              Session2.CompressionRatio);
      end;
    end
    else begin
      // We have no data, so return equal, (should not occur)!
      Result := 0;
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
  CurrentItem: TJwWTSSession;
begin
  pData := Sender.GetNodeData(Node);

  // Do we have data for this session?
  if pData^.List^.Count > pData^.Index then
  begin
    CurrentItem := pData^.List^.Items[pData^.Index];
    case Column of
      0: CellText := CurrentItem.Owner.Owner.Server;
      1: CellText := CurrentItem.Username;
      2: CellText := IntToStr(CurrentItem.SessionId);
      3: CellText := CurrentItem.ConnectStateStr;
      4: CellText := CurrentItem.WinStationDriverName;
      5: CellText := CurrentItem.ClientName;
      6: CellText := CurrentItem.IdleTimeStr;
      7: CellText := CurrentItem.LogonTimeStr;
      8: CellText := CurrentItem.RemoteAddress;
    end;
    // Show Session Counters only for Active and non-console sessions:
    if (CurrentItem.ConnectState = WTSActive) and
      (CurrentItem.WdFlag > WD_FLAG_CONSOLE) then
    begin
      case Column of
        9: CellText := IntToStr(CurrentItem.IncomingBytes);
        10: CellText := IntToStr(CurrentItem.OutgoingBytes);
        11: CellText := CurrentItem.CompressionRatio;
      end;
    end
    // Set empty value for In- and OutgoingBytes and CompressionRatio
    else if Column > 8 then
    begin
      CellText := '';
    end;
  end;
end;

procedure TMainForm.VSTProcessCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var Data1: PProcessNodeData;
  Data2: PProcessNodeData;
  Index1: Integer;
  Index2: Integer;
  Process2: TJwWTSProcess;
  Process1: TJwWTSProcess;
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
    Process1 := Data1^.List^[Index1];
    Process2 := Data2^.List^[Index2];

    // Do we have data of these sessions?
    if (Data1^.List^.Count > Index1) and (Data2^.List^.Count > Index2) then
    begin
      case Column of
        0: Result := CompareText(Process1.Owner.Owner.Server, Process2.Owner.Owner.Server);
        1: Result := CompareText(Process1.Username, Process2.Username);
        2: Result := CompareText(Process1.WinStationName, Process2.WinStationName);
        3: Result := CompareInteger(Process1.SessionId, Process2.SessionId);
        4: Result := CompareInteger(Process1.ProcessId, Process2.ProcessId);
        5: Result := CompareText(Process1.ProcessName, Process2.ProcessName);
        6: Result := CompareInteger(Process1.ProcessAge, Process2.ProcessAge);
        7: Result := CompareText(Process1.ProcessCPUTime, Process2.ProcessCPUTime);
        8: Result := CompareInteger(Process1.ProcessMemUsage, Process2.ProcessMemUsage);
        9: Result := CompareInteger(Process1.ProcessVMSize, Process2.ProcessVMSize);
      end;
    end
    else begin
      // We have no data, so return equal, should not occur!
      Result := 0;
    end;
  end;
end;

procedure TMainForm.VSTProcessGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var pData: PProcessNodeData;
  Username: string;
  IsConsole: Boolean;
  JwUserSid: TjwSecurityId;
begin
  pData := Sender.GetNodeData(Node);

  // Do we have data for this session?
  if pData^.List^.Count > pData^.Index then
  begin
    Username := pData^.List^.Items[pData^.Index].Username;
    IsConsole := pData^.List^.Items[pData^.Index].WinStationName = 'Console';
    JwUserSid := TjwSecurityId.Create('', Username);

    if Kind in [ikNormal, ikSelected] then begin
      case column of
        0: ImageIndex := Integer(icServer);
        1: begin
          if Username = '' then
          begin
            ImageIndex := -1; // No Icon!
          end
          else if JwUserSid.EqualSid(jwLocalSystemSid) then
          begin
            ImageIndex := Integer(icSystem);
          end
          else if JwUserSid.EqualSid(jwLocalServiceSid) then
          begin
            ImageIndex := Integer(icService);
          end
          else if JwUserSid.EqualSid(jwNetworkServiceSid) then
          begin
            ImageIndex := Integer(icNetworkService);
          end
          else begin
            ImageIndex := Integer(icUser);
          end;
          end;
          2: begin
            if IsConsole then
            begin
              ImageIndex := Integer(icComputer);
            end
            else begin
              ImageIndex := Integer(icNetwork);
            end;
        end;
        5: ImageIndex := Integer(icProcess);
        7: ImageIndex := Integer(icCPUTime);
        8: ImageIndex := Integer(icMemory);
      end;
    end;
  end;
end;

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
  CurrentItem: TJwWTSProcess;
begin
  // Get LocaleFormatSettings, we use them later on to format DWORD
  // values with bytes as KByte values with thousand seperators a la taskmgr
  // (we query this every time because the user may have changed settings)
  GetLocaleFormatSettings(LOCALE_USER_DEFAULT, FormatSettings);
  pData := Sender.GetNodeData(Node);

  // Do we have data for this session?
  if pData^.List^.Count > pData^.Index then
  begin
    CurrentItem := pData^.List^.Items[pData^.Index];
    case Column of
      0: CellText := CurrentItem.Owner.Owner.Server;
      1: CellText := CurrentItem.Username;
      2: CellText := CurrentItem.WinStationName;
      3: CellText := IntToStr(CurrentItem.SessionId);
      4: CellText := IntToStr(CurrentItem.ProcessId);
      5: CellText := CurrentItem.ProcessName;
      6: CellText := CurrentItem.ProcessAgeStr;
      7: CellText := CurrentItem.ProcessCPUTime;
      8: CellText := Format('%.0n K', [ CurrentItem.ProcessMemUsage / 1024],
        FormatSettings);
      9: CellText := Format('%.0n K', [CurrentItem.ProcessVMSize / 1024],
        FormatSettings);
    end;
  end;
end;

procedure TMainForm.VSTServerChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var pServerData: PServerNodeData;
  pUserNode: PVirtualNode;
  pUserData: PUserNodeData;
  pSessionNode: PVirtualNode;
  pSessionData: PSessionNodeData;
  i: Integer;
  pProcessNode: PVirtualNode;
  pProcessData: PProcessNodeData;
  PrevCount: Integer;
begin
  // Get the Node Data
  pServerData := Sender.GetNodeData(Node);
  if Node^.CheckState = csUncheckedNormal then
  begin
    if pServerData^.PTerminalServerList <> nil then
    begin
      with pServerData^.PTerminalServerList.Items[pServerData^.Index] do
      begin
        // Store the current session count
        PrevCount := Sessions.Count;
        // Clear the SessionList
        Sessions.Clear;
        // Update User and Session VST
        UpdateVirtualTree(VSTUser, @Sessions, PrevCount);
        UpdateVirtualTree(VSTSession, @Sessions, PrevCount);
        // Store the current process count
        PrevCount := Processes.Count;
        // Clear the ProcessList
        Processes.Clear;
        // and update Process VST
        UpdateVirtualTree(VSTProcess, @Processes, PrevCount);

        pServerData^.PTerminalServerList.Delete(pServerData^.Index);
        // nil it
        pServerData^.PTerminalServerList := nil;

      end;
    end;
  end
  else if Node^.CheckState = csCheckedNormal then
  begin
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

        // Assign Session Event Handler
        OnSessionEvent := OnTerminalServerEvent;
        if EnumerateProcesses then
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
    end;
  end;
end;

procedure TMainForm.VSTServerCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  s1: string;
  s2: string;
begin
  s1 := PServerNodeData(Sender.GetNodeData(Node1))^.Caption;
  s2 := PServerNodeData(Sender.GetNodeData(Node2))^.Caption;
  Result := CompareText(s1, s2); 
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
//  VSTServer.Header.SaveToStream();
  // Prevent updates to the Virtual String Grids
  VSTUser.OnGetText := nil;
  VSTServer.OnGetText := nil;

  // Free Sid's
  JwLocalSystemSid.Free;
  JwLocalServiceSid.Free;
  JwNetworkServiceSid.Free;

  // Now Free the Terminal Server Instances
  TerminalServers.Free;
end;

end.
