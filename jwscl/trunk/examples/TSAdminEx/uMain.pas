unit uMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ComCtrls, ExtCtrls, ActnList, ImgList, ToolWin, Menus,
  StdCtrls, StrUtils{, CommCtrl}, Math, Dialogs,
  VirtualTrees,
  NetApi, uAbout,
  JwaWindows, JwaVista,
  JwsclSid, JwsclTerminalServer;

type
  PServerNodeData = ^TServerNodeData;
  TServerNodeData = record
    Index: Integer;
    OverlayIndex: Integer;
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

  // This class is used to refresh a process list on each interval
  TEnumerateProcessThread = class(TThread)
  protected
    FIndex: Integer;
    FInterval: DWORD;
    FOwner: TJwTerminalServerList;
    FServerList: TStringList;
    FTerminalServer: TJwTerminalServer;
    FTerminatedEvent: THandle;
    procedure Update;
  public
    constructor Create(CreateSuspended: Boolean; Owner: TJwTerminalServerList);
    property Interval: DWORD read FInterval write FInterval;
    procedure Execute; override;
  end;

  // Class below is used to store imageindex of icons in the Imagelist
  TIconIndex = (icThisComputer, icFavorite, icWorld, icServers, icServersSel,
    icServer, icServerSel, icUserGhosted, icUser, IcNetworkUser, icNetwork,
    icComputer, icProcess, icChip, icMemory, icListener, icVirtual, icCPUTime,
    icClock, icService, icNetworkService, icSystem, icHourGlass, icExclMark);

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
    PopupMenu1: TPopupMenu;
    actAddServer: TAction;
    AddServer1: TMenuItem;
    About1: TMenuItem;
    actAbout: TAction;
    Button1: TButton;
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
    procedure VSTServerColumnDblClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
    procedure VSTUserGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: WideString);
    procedure VSTSessionGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: WideString);
    procedure actAddServerExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure actRefreshExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure VSTProcessIncrementalSearch(Sender: TBaseVirtualTree;
      Node: PVirtualNode; const SearchText: WideString; var Result: Integer);
    procedure VSTUserIncrementalSearch(Sender: TBaseVirtualTree;
      Node: PVirtualNode; const SearchText: WideString; var Result: Integer);
    procedure VSTSessionIncrementalSearch(Sender: TBaseVirtualTree;
      Node: PVirtualNode; const SearchText: WideString; var Result: Integer);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    TerminalServers: TJwTerminalServerList;
    pThisComputerNode: PVirtualNode;
    pFavoritesNode: PVirtualNode;
    pAllListedServersNode: PVirtualNode;
    LocalSystemName: string;
    NetworkServiceName: string;
    LocalServiceName: string;
    procedure UpdateVirtualTree(const AVirtualTree: TBaseVirtualTree;
      const PSessionList: PJwWTSSessionList; PrevCount: Integer);
    procedure UpdateProcessVirtualTree(const AVirtualTree: TBaseVirtualTree;
      const PProcessList: PJwWTSProcessList; const PrevCount: Integer; const ComparePointer:PJwWTSProcessList=nil);
    function GetCurrentSession(AVST: TBaseVirtualTree): TJwWTSSession; overload;
    function GetCurrentSession(AVST: TBaseVirtualTree; Node: PVirtualNode): TJwWTSSession; overload;
    function GetCurrentProcess(AVST: TBaseVirtualTree): TJwWTSProcess; overload;
    function GetCurrentProcess(AVST: TBaseVirtualTree; Node: PVirtualNode): TJwWTSProcess; overload;
    procedure OnTerminalServerEvent(Sender: TObject);
    procedure OnEnumerateServersDone(Sender: TObject);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  IconRec: TIconRec;

implementation
{$R *.dfm}
constructor TEnumerateProcessThread.Create(CreateSuspended: Boolean; Owner: TJwTerminalServerList);
var
  i: Integer;
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;

  FInterval := 5000; // Default interval = 5 seconds
  FOwner := Owner;

  FServerList := TStringList.Create;
  for i := 0 to Owner.Count - 1 do
  begin
    if FOwner[i].ComputerName <> FOwner[i].Server then
    begin
      FServerList.Add(Owner[i].Server);
    end
    else begin
      FServerList.Add('');
    end;
  end;

  // This event is used to signal termination
  FTerminatedEvent := CreateEvent(nil, False, False, nil);
end;

procedure TEnumerateProcessThread.Execute;
var i: Integer;
begin
  while not Terminated do
  begin

    for i := 0 to FServerList.Count - 1 do
    begin
      // Did we terminate meanwhile?
      if Terminated then Exit;

      FTerminalServer := TJwTerminalServer.Create;
      FTerminalServer.Server := FServerList[i];

      FTerminalServer.Processes.Free;
      FTerminalServer.Processes := TJwWTSProcessList.Create(False);

      if FTerminalServer.EnumerateProcesses then
      begin
        // Did we terminate meanwhile?
        if Terminated then Exit;

        // Store Index and Update
        FIndex := i;
        Synchronize(Update);
      end;

//      FTerminalServer.Disconnect;
      FTerminalServer.Free;

    end;
    // Do nothing until either the interval expires or a Termination Event is
    // sent
    WaitForSingleObject(FTerminatedEvent, Interval);
  end;
end;

procedure TEnumerateProcessThread.Update;
var
  PrevCount: Integer;
  i: Integer;
  OldProcessList: TJwWTSProcessList;
  NewProcessList: TJwWTSProcessList;
  TempProcessList: TJwWTSProcessList;
begin
  OldProcessList := FOwner[Findex].Processes;
{  NewProcessList := FTerminalServer.Processes;

  TempProcessList := FOwner[Findex].Processes;

//  OutputDebugString(PChar(Format('Oldlist1 count: %d', [OldProcessList^.Count])));
//  OutputDebugString(PChar(Format('Newlist1 count: %d', [NewProcessList^.Count])));

  // swap the processlists
  PrevCount := OldProcessList.Count;
{  OldProcessList := NewProcessList;
  NewProcessList := TempProcessList;
  MainForm.TerminalServers[FIndex].Processes := OldProcessList^;
}

//  OutputDebugString(PChar(Format('Oldlist2 count: %d', [OldProcessList^.Count])));
//  OutputDebugString(PChar(Format('Newlist2 count: %d', [NewProcessList^.Count])));
  PrevCount := FOwner[Findex].Processes.Count;
  FOwner[FIndex].Processes.Free;

  FOwner[FIndex].Processes := FTerminalServer.Processes;
  FTerminalServer.Processes := nil;
//  FOwner[Findex].Processes.Assign(FTerminalServer.Processes);

  FOwner[Findex].Processes.Owner := FOwner[Findex];

  for i := 0 to FOwner[Findex].Processes.Count - 1 do
  begin
    FOwner[Findex].Processes[i].Owner := FOwner[FIndex].Processes;
//    OutputDebugString(PChar(FOwner[Findex].Processes[i].Server));
  end;

  with MainForm do
  begin
    // we only update if the process tab is visible...
    if PageControl1.ActivePageIndex = 2 then
    begin
      UpdateProcessVirtualTree(VSTProcess, @FOwner[Findex].Processes, PrevCount, @OldProcessList);
    end;
  end;
//  FTerminalServer.Processes := nil;
end;

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

//procedure TMainForm.UpdateProcessVirtualTree(const AVirtualTree: TBaseVirtualTree; const PProcessList: PJwWTSProcessList; PrevCount: Integer;  const ComparePointer:PJwWTSProcessList=nil);
procedure TMainForm.UpdateProcessVirtualTree(const AVirtualTree: TBaseVirtualTree;
      const PProcessList: PJwWTSProcessList; const PrevCount: Integer; const ComparePointer:PJwWTSProcessList=nil);
var i: Integer;
  pNode: PVirtualNode;
  pPrevNode: PVirtualNode;
  pData: PProcessNodeData;
  NewCount: Integer;
  CompareTo: PJwWTSProcessList;
begin
  if ComparePointer <> nil then
  begin
    CompareTo := ComparePointer;
  end
  else begin
    CompareTo := PProcessList;
  end;

  // Get the last node
  pNode := AVirtualTree.GetLast;

  // Now iterate from last to first node
  repeat
    pData := AVirtualTree.GetNodeData(pNode);
    // Get the previous node and store the pointer, because in the next step
    // we might delete the current node ;-)
    pPrevNode := AVirtualTree.GetPrevious(pNode);

    // Is the node data pointing to PSessionList and do we need to delete it?
    if pData^.List = CompareTo then
    begin
      if pData^.Index > PProcessList^.Count-1 then
      begin
        // Delete the node (we have no Session Data for it)
        AVirtualTree.DeleteNode(pNode);
      end
      else begin
        // Invalidating the node will trigger the GetText event which will update
        // our data
        if ComparePointer <> nil then
        begin
          pData^.List := PProcessList;
        end;

        AVirtualTree.InvalidateNode(pNode);
      end;
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

procedure UpdateSessions(ATerminalServer: TJwTerminalServer);
var PrevCount: Integer;
begin
  // Get the previous session count
  PrevCount := ATerminalServer.Sessions.Count;

  // Enumerate sessions
  ATerminalServer.EnumerateSessions;

  // Sychronize User and Session tree's with the SessionList
  MainForm.UpdateVirtualTree(MainForm.VSTUser, @ATerminalServer.Sessions, PrevCount);
  MainForm.UpdateVirtualTree(MainForm.VSTSession, @ATerminalServer.Sessions, PrevCount);
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

procedure UpdateProcesses(ATerminalServer: TJwTerminalServer);
var
  PrevCount: Integer;
begin
  PrevCount := ATerminalServer.Processes.Count;
  ATerminalServer.EnumerateProcesses;
  MainForm.UpdateProcessVirtualTree(MainForm.VSTProcess, @ATerminalServer.Processes, PrevCount);
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
    pData := VSTServer.GetNodeData(pDomainNode);

    if ServerList.Count > 0 then
    begin
      // No overlay
      pData^.OverlayIndex := -1;
    end
    else begin
      // Warning overlay
      pData^.OverlayIndex := Integer(icExclMark);
    end;
  end;

  VSTServer.FullExpand(pDomainNode);
end;

procedure TMainForm.actAboutExecute(Sender: TObject);
var AboutDialog: TAboutDialog;
begin
  AboutDialog := TAboutDialog.Create(Self);
  AboutDialog.ShowModal;
  AboutDialog.Free;
end;

procedure TMainForm.actAddServerExecute(Sender: TObject);
var s: string;
  pNode: PVirtualNode;
  pData: PServerNodeData;
begin
  s := InputBox('Add Server', 'Type the name of the server you wish to add', '');;
  pNode := VSTServer.AddChild(pFavoritesNode);
  pData := VSTServer.GetNodeData(pNode);
  ASSERT(pData <> nil);

  pData^.Index := -1;
  pData^.Caption := s;
  pData^.PTerminalServerList := nil;
  pNode.CheckType := ctCheckBox;
end;

function TMainForm.GetCurrentSession(AVST: TBaseVirtualTree; Node: PVirtualNode): TJwWTSSession;
var UserNodeData: PUserNodeData;
begin
  Result := nil;

  if Node = nil then
  begin
    Exit;
  end;

  // Get Node Data
  UserNodeData := AVST.GetNodeData(Node);

  // Do we have data for this session?
  if UserNodeData^.Index < UserNodeData^.List.Count then
  begin
    Result := UserNodeData^.List^[UserNodeData^.Index];
  end;
end;

function TMainForm.GetCurrentSession(AVST: TBaseVirtualTree): TJwWTSSession;
var UserNodeData: PUserNodeData;
  Node: PVirtualNode;
begin
  Result := nil;

  Node := AVST.FocusedNode;

  if Node = nil then
  begin
    Exit;
  end;

  // Get Node Data
  UserNodeData := AVST.GetNodeData(Node);

  // Do we have data for this session?
  if UserNodeData^.Index < UserNodeData^.List.Count then
  begin
    Result := UserNodeData^.List^[UserNodeData^.Index];
  end;
end;

function TMainForm.GetCurrentProcess(AVST: TBaseVirtualTree; Node: PVirtualNode): TJwWTSProcess;
var
  ProcessNodeData: PProcessNodeData;
begin
  Result := nil;

  if Node = nil then
  begin
    Exit;
  end;

  // Get Node Data
  ProcessNodeData := AVST.GetNodeData(Node);

  // Do we have data for this process?
  if ProcessNodeData^.Index < ProcessNodeData^.List.Count then
  begin
    Result := ProcessNodeData^.List^[ProcessNodeData^.Index];
  end;
end;

function TMainForm.GetCurrentProcess(AVST: TBaseVirtualTree): TJwWTSProcess;
var ProcessNodeData: PProcessNodeData;
  Node: PVirtualNode;
begin
  Result := nil;

  Node := AVST.FocusedNode;
  if Node = nil then
  begin
    Exit;
  end;

  // Get Node Data
  ProcessNodeData := AVST.GetNodeData(Node);

  // Do we have data for this process?
  if ProcessNodeData^.Index < ProcessNodeData^.List.Count then
  begin
    Result := ProcessNodeData^.List^[ProcessNodeData^.Index];
  end;
end;


// This procedure takes care of updating the actionlist. It enables actions
// based upon the visible tab (user, session or process) and session connect
// state.
procedure TMainForm.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
var CurrentSession: TJwWTSSession;
begin
  CurrentSession := nil;

  actEndProcess.Enabled := (PageControl1.ActivePageIndex = 2) and
    (VSTProcess.FocusedNode <> nil);

  case PageControl1.ActivePageIndex of
    0: CurrentSession := GetCurrentSession(VSTUser);
    1: CurrentSession := GetCurrentSession(VSTSession);
  end;

  if CurrentSession <> nil then
  begin
    actConnect.Enabled :=
      CurrentSession.ConnectState in [WTSActive, WTSConnected, WTSDisconnected];
    actDisconnect.Enabled :=
      CurrentSession.ConnectState in [WTSActive, WTSConnected, WTSShadow];
    actSendMessage.Enabled :=
      CurrentSession.ConnectState in [WTSActive, WTSConnected, WTSShadow];
    actRemoteControl.Enabled :=
      CurrentSession.ConnectState in [WTSActive, WTSConnected];
    actReset.Enabled := True;
    actStatus.Enabled := True;
    actLogoff.Enabled :=
      CurrentSession.ConnectState in [WTSActive, WTSDisconnected, WTSShadow];
  end
  else begin
    actConnect.Enabled := False;
    actDisconnect.Enabled := False;
    actSendMessage.Enabled := False;
    actRemoteControl.Enabled := False;
    actReset.Enabled := False;
    actStatus.Enabled := False;
    actLogoff.Enabled := False;
  end;

  // Set Handled (we don't need to do this for every action!)
  Handled := True;
end;

procedure TMainForm.actRefreshExecute(Sender: TObject);
var
  i: Integer;
  Update: procedure(ATerminalServer: TJwTerminalServer);
begin
  if PageControl1.ActivePageIndex = 2 then
  begin
    Update := UpdateProcesses;
  end
  else begin
    Update := UpdateSessions;
  end;

  for i := 0 to TerminalServers.Count - 1 do
  begin
    Update(TerminalServers[i]);
  end;

  case PageControl1.ActivePageIndex of
    0: VSTUser.SortTree(VSTUser.Header.SortColumn, VSTUser.Header.SortDirection);
    1: VSTSession.SortTree(VSTSession.Header.SortColumn, VSTSession.Header.SortDirection);
    2: VSTProcess.SortTree(VSTProcess.Header.SortColumn, VSTProcess.Header.SortDirection);
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var TS1, TS2: TJwTerminalServer;
  i: Integer;
  j: Integer;
  TempProcessList: TJwWTSProcessList;
begin
  TEnumerateProcessThread.Create(False, TerminalServers);
{  TS1 := TJwTerminalServer.Create;
  TS2 := TJwTerminalServer.Create;

  for j := 0 to 10 do
  begin
    TS1.EnumerateProcesses;
    TempProcessList := TS2.Processes;

    TS2.Processes := TS1.Processes;
    TS1.Processes := TempProcessList;

    OutputDebugString(PChar(Format('Round: %d, Count: %d', [j, TS2.Processes.Count])));

    for i := 0 to TS2.Processes.Count - 1 do
    begin
      OutputDebugString(PChar(Format('Round: %d Count %d: %s', [j, i, TS2.Processes[i].ProcessName])));
    end;
  end;

  TS1.Free;
  for i := 0 to TS2.Processes.Count - 1 do
  begin
    OutputDebugString(PChar(Format('Final Round: %d Count %d: %s', [j, i, TS2.Processes[i].ProcessName])));
  end;

  Sleep(2000);}

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
  JwSid: TJwSecurityId;
begin
{$IFDEF FASTMM}
  ReportMemoryLeaksOnShutDown := DebugHook <> 0;
{$ENDIF FASTMM}
  TerminalServers := TJwTerminalServerList.Create;
  TerminalServers.Owner := Self;

  // The system accounts LocalSystem, LocalServer and NetworkService are
  // localized. Therefore we retreive the name from the SID and store this
  // in var's. Later on we use the var's to compare.
  JwSid := TJwSecurityId.CreateWellKnownSid(WinLocalSystemSid);
  LocalSystemName := JwSid.GetCachedUserFromSid;
  JwSid.Free;

  JwSid := TJwSecurityId.CreateWellKnownSid(WinLocalServiceSid);
  LocalServiceName := JwSid.GetCachedUserFromSid;
  JwSid.Free;

  JwSid := TJwSecurityId.CreateWellKnownSid(WinNetworkServiceSid);
  NetworkServiceName := JwSid.GetCachedUserFromSid;
  JwSid.Free;

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

  // This node can be checked
  pNode^.CheckType := ctCheckBox;
  // The this computer node is checked by default
  pNode^.CheckState := csCheckedNormal;

  // Trigger the OnChecked Event, this will fill the listviews for this computer
  VSTServer.OnChecked(VSTServer, pNode);

  // Expand the This Computer Node
  VSTServer.FullExpand(pThisComputerNode);

  // Create the Favorites node
  pFavoritesNode := VSTServer.AddChild(nil);
  pData := VSTServer.GetNodeData(pFavoritesNode);
  // This is a node without a Terminal Server instance attached so we set
  // Index to -2 (never add a Terminal Server instance to it) and Pointer to nil
  pData^.Caption := 'Favorites';
  pData^.Index := -2;
  pData^.PTerminalServerList := nil;

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

  DomainList.Free;
  VSTServer.FullExpand(pAllListedServersNode);

  AutoSizeVST(VSTUser);
  AutoSizeVST(VSTSession);
  AutoSizeVST(VSTProcess);
end;

procedure TMainForm.VSTUserGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var CurrentSession: TJwWTSSession;
begin
  // Sometimes VST asks for Column -1 (Autosort??)
  if Column < 0 then Exit;

  CurrentSession := GetCurrentSession(Sender, Node);

  // Delete this node if we don't have data for it!
  if CurrentSession = nil then
  begin
    Sender.DeleteNode(Node);
    Exit;
  end;

  if CurrentSession.Username <> '' then
  begin

    // Show the node if it was InVisible
    if not (vsVisible in Node.States) then
    begin
      Sender.IsVisible[Node] := True;
    end;

    case Column of
      0: CellText := CurrentSession.Server;
      1: CellText := CurrentSession.Username;
      2: CellText := CurrentSession.WinStationName;
      3: CellText := IntToStr(CurrentSession.SessionId);
      4: CellText := CurrentSession.ConnectStateStr;
      5: CellText := CurrentSession.IdleTimeStr;
      6: CellText := CurrentSession.LogonTimeStr;
    end;
  end
  else begin
    // Users Listview only shows sessions that have a user attached!
    Sender.IsVisible[Node] := False;
  end;
end;

procedure TMainForm.VSTUserIncrementalSearch(Sender: TBaseVirtualTree;
  Node: PVirtualNode; const SearchText: WideString; var Result: Integer);
var CurrentSession: TJwWTSSession;
  NodeValue: string;
begin
  CurrentSession := GetCurrentSession(Sender, Node);

  if CurrentSession = nil then Exit;

  case (Sender as TVirtualStringTree).Header.SortColumn of
    0: NodeValue := CurrentSession.Server;
    1: NodeValue := CurrentSession.Username;
    2: NodeValue := CurrentSession.WinStationName;
    3: NodeValue := IntToStr(CurrentSession.SessionId);
    4: NodeValue := CurrentSession.ConnectStateStr;
    5: NodeValue := CurrentSession.IdleTimeStr;
    6: NodeValue := CurrentSession.LogonTimeStr;
  end;

  Result := StrLIComp(PChar(SearchText), PChar(NodeValue), Length(SearchText));
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

function GetShadowHint(const ShadowInformation: TJwWTSSessionShadow): WideString;
begin
  case ShadowInformation.ShadowState of
    ssShadowing: Result := 'This session is currently shadowing another session';
    ssBeingShadowed: Result := 'This session is currently being shadowed by another session';
    else begin
      case ShadowInformation.ShadowMode of
        smNoneAllowed: Result := 'This session cannot be viewed or shadowed';
        smFullControlWithPermission: Result := 'This session can be shadowed but needs the user''s permission';
        smFullControlWithoutPermission: Result := 'This session can be shadowed and does not need the user''s permission';
        smViewOnlyWithPermission: Result := 'This session can be viewed but needs the user''s permission';
        smViewOnlyWithoutPermission: Result := 'This session can be viewed and does not need the user''s permission';
      end;
    end;
  end;
end;

procedure TMainForm.VSTUserGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
  var HintText: WideString);
var CurrentSession: TJwWTSSession;
begin
  CurrentSession := GetCurrentSession(Sender, Node);

  // Delete this node if we don't have data for it!
  if CurrentSession = nil then
  begin
    Sender.DeleteNode(Node);
    Exit;
  end;

  case Column of
    4: HintText := GetShadowHint(CurrentSession.ShadowInformation);
    8: HintText := Format('%s:%d', [CurrentSession.RemoteAddress, CurrentSession.RemotePort]);
  end;
end;

procedure TMainForm.VSTUserGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var CurrentSession: TJwWTSSession;
  IsConsole: Boolean;
begin
  CurrentSession := GetCurrentSession(Sender, Node);

  // Delete this node if we don't have data for it!
  if CurrentSession = nil then
  begin
    Sender.DeleteNode(Node);
    Exit;
  end;

  IsConsole := CurrentSession.WdFlag < WD_FLAG_RDP;

  if Kind in [ikNormal, ikSelected] then begin
    case column of
      0: ImageIndex := Integer(icServer);
      1:
      begin
        if CurrentSession.Username = '' then
        begin
          ImageIndex := -1; // No Icon!
        end
        else if CurrentSession.Username = LocalSystemName then
        begin
          ImageIndex := Integer(icSystem);
        end
        else if CurrentSession.Username = LocalServiceName then
        begin
          ImageIndex := Integer(icService);
        end
        else if CurrentSession.Username = NetworkServiceName then
        begin
          ImageIndex := Integer(icNetworkService);
        end
        else if CurrentSession.ConnectState = WTSActive then
        begin
          ImageIndex := Integer(icUser);
        end
        else begin
          ImageIndex := Integer(icUserGhosted);
        end;
      end;
      2:
      begin
        if IsConsole then
        begin
          ImageIndex := Integer(icComputer);
        end
        else if CurrentSession.ConnectState = WTSListen then
        begin
          ImageIndex := Integer(icListener);
        end
        else if CurrentSession.Username <> '' then begin
          ImageIndex := Integer(icNetworkUser);
        end
        else if CurrentSession.ConnectState = WTSActive then begin
          ImageIndex := Integer(icNetwork);
        end
        else begin
          ImageIndex := -1;
        end;
      end;
    end;
  end;
end;

procedure TMainForm.VSTServerGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var pData: PServerNodeData;
begin
  case Kind of
    ikNormal, ikSelected:
    begin
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
    ikOverlay:
    begin
      pData := Sender.GetNodeData(Node);
      ImageIndex := pData^.OverlayIndex;// * Integer(icHourGlass);
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
  Session1 := GetCurrentSession(Sender, Node1);
  Session2 := GetCurrentSession(Sender, Node2);

  if (Session1 = nil) or (Session2 = nil) then
  begin
    Result := 0;
    Exit;
  end;

  case Column of
    0: Result := CompareText(Session1.Server, Session2.Server);
    1: Result := CompareText(Session1.Username, Session2.UserName);
    2: Result := CompareText(Session1.WinStationName, Session2.WinStationName);
    3: Result := CompareInteger(Session1.SessionId, Session2.SessionId);
    4: Result := CompareText(Session1.ConnectStateStr, Session2.ConnectStateStr);
    5: Result := CompareInteger(Session1.IdleTime, Session2.IdleTime);
    6: Result := CompareInteger(Session1.LogonTime, Session2.LogonTime);
  end;
end;

procedure TMainForm.VSTServerGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var pData: PServerNodeData;
begin
  pData := Sender.GetNodeData(Node);
  if pData = nil then
    exit;

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
  if pNode = nil then
    exit;

  if pNode^.Parent = pAllListedServersNode then
  begin
    if pNode^.ChildCount = 0 then
    begin
      pServerData := VSTServer.GetNodeData(pNode);
      // Add the hourglass
      pServerData^.OverlayIndex := Integer(icHourGlass);

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
  Session1 := GetCurrentSession(Sender, Node1);
  Session2 := GetCurrentSession(Sender, Node2);

  if (Session1 = nil) or (Session2 = nil) then
  begin
    Result := 0;
    Exit;
  end;

  case Column of
    0: Result := CompareText(Session1.Server, Session2.Server);
    1: Result := CompareText(Session1.Username, Session2.UserName);
    2: Result := CompareInteger(Session1.SessionId, Session2.SessionId);
    3: Result := CompareText(Session1.ConnectStateStr, Session2.ConnectStateStr);
    4: Result := CompareText(Session1.WinStationDriverName, Session2.WinStationDriverName);
    5: Result := CompareText(Session1.ClientName, Session2.ClientName);
    6: Result := CompareInteger(Session1.IdleTime, Session2.IdleTime);
    7: Result := CompareInteger(Session1.LogonTime, Session2.LogonTime);
    8: Result := CompareText(Session1.RemoteAddress, Session2.RemoteAddress);
    9: Result := CompareInteger(Session1.IncomingBytes, Session2.IncomingBytes);
    10: Result := CompareInteger(Session1.OutgoingBytes, Session2.OutgoingBytes);
    11: Result := CompareText(Session1.CompressionRatio, Session2.CompressionRatio);
  end;
end;

procedure TMainForm.VSTSessionGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString);
var CurrentSession: TJwWTSSession;
begin
  CurrentSession := GetCurrentSession(Sender, Node);

  // Delete this node if we don't have data for it!
  if CurrentSession = nil then
  begin
    Sender.DeleteNode(Node);
    Exit;
  end;

  case Column of
    3: HintText := GetShadowHint(CurrentSession.ShadowInformation);
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
var CurrentSession: TJwWTSSession;
begin
  CurrentSession := GetCurrentSession(Sender, Node);

  // Delete this node if we don't have data for it!
  if CurrentSession = nil then
  begin
    Sender.DeleteNode(Node);
    Exit;
  end;

  case Column of
    0: CellText := CurrentSession.Server;
    1: CellText := CurrentSession.Username;
    2: CellText := IntToStr(CurrentSession.SessionId);
    3: CellText := CurrentSession.ConnectStateStr;
    4: CellText := CurrentSession.WinStationDriverName;
    5: CellText := CurrentSession.ClientName;
    6: CellText := CurrentSession.IdleTimeStr;
    7: CellText := CurrentSession.LogonTimeStr;
    8: CellText := CurrentSession.RemoteAddress;
  end;

  // Show Session Counters only for Active and non-console sessions:
  if (CurrentSession.ConnectState = WTSActive) and
    (CurrentSession.WdFlag > WD_FLAG_CONSOLE) then
  begin
    case Column of
      9: CellText := IntToStr(CurrentSession.IncomingBytes);
      10: CellText := IntToStr(CurrentSession.OutgoingBytes);
      11: CellText := CurrentSession.CompressionRatio;
    end;
  end
  // Set empty value for In- and OutgoingBytes and CompressionRatio
  else if Column > 8 then
  begin
    CellText := '';
  end;
end;

procedure TMainForm.VSTSessionIncrementalSearch(Sender: TBaseVirtualTree;
  Node: PVirtualNode; const SearchText: WideString; var Result: Integer);
var CurrentSession: TJwWTSSession;
  NodeValue: string;
begin
  CurrentSession := GetCurrentSession(Sender, Node);

  if CurrentSession = nil then Exit;

  case (Sender as TVirtualStringTree).Header.SortColumn of
    0: NodeValue := CurrentSession.Server;
    1: NodeValue := CurrentSession.Username;
    2: NodeValue := IntToStr(CurrentSession.SessionId);
    3: NodeValue := CurrentSession.ConnectStateStr;
    4: NodeValue := CurrentSession.WinStationDriverName;
    5: NodeValue := CurrentSession.ClientName;
    6: NodeValue := CurrentSession.IdleTimeStr;
    7: NodeValue := CurrentSession.LogonTimeStr;
    8: NodeValue := CurrentSession.RemoteAddress;
    9: NodeValue := IntToStr(CurrentSession.IncomingBytes);
    10: NodeValue := IntToStr(CurrentSession.OutgoingBytes);
    11: NodeValue := CurrentSession.CompressionRatio;
  end;

  Result := StrLIComp(PChar(SearchText), PChar(NodeValue), Length(SearchText));
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
  Process1 := GetCurrentProcess(Sender, Node1);
  Process2 := GetCurrentProcess(Sender, Node2);

  if (Process1 = nil) or (Process2 = nil) then
  begin
    Result := 0;
    Exit;
  end;
  
  case Column of
    0: Result := CompareText(Process1.Server, Process2.Server);
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
end;

procedure TMainForm.VSTProcessGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var pData: PProcessNodeData;
  Username: string;
  IsConsole: Boolean;
  CurrentProcess: TJwWTSProcess;
begin
  CurrentProcess := GetCurrentProcess(Sender, Node);

  if CurrentProcess = nil then
  begin
    ImageIndex := -1;
    Exit;
  end;
  // Do we have data for this session?

  Username := CurrentProcess.Username;
  IsConsole := CurrentProcess.WinStationName = 'Console';

  if Kind in [ikNormal, ikSelected] then begin
    case column of
      0: ImageIndex := Integer(icServer);
      1:
      begin
        if Username = '' then
        begin
          ImageIndex := -1; // No Icon!
        end
        else if Username = LocalSystemName then
        begin
          ImageIndex := Integer(icSystem);
          end
        else if Username = LocalServiceName then
        begin
          ImageIndex := Integer(icService);
        end
        else if Username = NetworkServiceName then
        begin
          ImageIndex := Integer(icNetworkService);
        end
        else begin
          ImageIndex := Integer(icUser);
        end;
      end;
      2:
      begin
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

procedure TMainForm.VSTProcessGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TProcessNodeData);
end;

procedure TMainForm.VSTProcessGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var FormatSettings: TFormatSettings;
  CurrentProcess: TJwWTSProcess;
begin
  // Get LocaleFormatSettings, we use them later on to format DWORD
  // values with bytes as KByte values with thousand seperators a la taskmgr
  // (we query this every time because the user may have changed settings)
  GetLocaleFormatSettings(LOCALE_USER_DEFAULT, FormatSettings);

  // Get Current Process
  CurrentProcess := GetCurrentProcess(Sender, Node);

  if CurrentProcess = nil then
  begin
//    Sender.DeleteNode(Node);
    Exit;
  end;

  case Column of
    0: CellText := CurrentProcess.Server;
    1: CellText := CurrentProcess.Username;
    2: CellText := CurrentProcess.WinStationName;
    3: CellText := IntToStr(CurrentProcess.SessionId);
    4: CellText := IntToStr(CurrentProcess.ProcessId);
    5: CellText := CurrentProcess.ProcessName;
    6: CellText := CurrentProcess.ProcessAgeStr;
    7: CellText := CurrentProcess.ProcessCPUTime;
    8: CellText := Format('%.0n K', [ CurrentProcess.ProcessMemUsage / 1024],
      FormatSettings);
    9: CellText := Format('%.0n K', [CurrentProcess.ProcessVMSize / 1024],
      FormatSettings);
  end;
end;

procedure TMainForm.VSTProcessIncrementalSearch(Sender: TBaseVirtualTree;
  Node: PVirtualNode; const SearchText: WideString; var Result: Integer);
var CurrentProcess: TJwWTSProcess;
  NodeValue: string;
begin
  CurrentProcess := GetCurrentProcess(Sender, Node);

  if CurrentProcess = nil then Exit;

  case (Sender as TVirtualStringTree).Header.SortColumn of
    0: NodeValue := CurrentProcess.Server;
    1: NodeValue := CurrentProcess.Username;
    2: NodeValue := CurrentProcess.WinStationName;
    3: NodeValue := IntToStr(CurrentProcess.SessionId);
    4: NodeValue := IntToStr(CurrentProcess.ProcessId);
    5: NodeValue := CurrentProcess.ProcessName;
    6: NodeValue := CurrentProcess.ProcessAgeStr;
    7: NodeValue := CurrentProcess.ProcessCPUTime;
    8: NodeValue := IntToStr(CurrentProcess.ProcessMemUsage);
    9: NodeValue := IntToStr(CurrentProcess.ProcessVMSize);
  end;

  Result := StrLIComp(PChar(String(SearchText)), PChar(NodeValue), Length(SearchText));
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
        // Create a Terminal Server instance and add it to the list
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

procedure TMainForm.VSTServerColumnDblClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
var
  pNode: PVirtualNode;
  pServerData: PServerNodeData;
  strDomain: string;
  ATerminalServer: TJwTerminalServer;
begin
  pNode := VSTServer.FocusedNode;
  if pNode = nil then
    exit;

  if pNode^.Parent = pAllListedServersNode then
  begin
    if pNode^.ChildCount = 0 then
    begin
      pServerData := VSTServer.GetNodeData(pNode);
      // Add the hourglass overlay
      pServerData^.OverlayIndex := 1;

      strDomain := pServerData^.Caption;

      ATerminalServer := TJwTerminalServer.Create;
      ATerminalServer.OnServersEnumerated := OnEnumerateServersDone;
      // store the node pointer
      ATerminalServer.Data := pNode;

      ATerminalServer.EnumerateServers(strDomain);
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
  if pData = nil then
    exit;
  // Free the string data by setting it to ''
  pData^.Caption := '';
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
//  VSTServer.Header.SaveToStream();
  // Prevent updates to the Virtual String Grids
  VSTUser.OnGetText := nil;
  VSTServer.OnGetText := nil;

  // Now Free the Terminal Server Instances
  TerminalServers.Free;
end;

end.
