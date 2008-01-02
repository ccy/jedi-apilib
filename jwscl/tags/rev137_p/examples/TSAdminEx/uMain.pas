unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, ActnList, ImgList, ToolWin, Menus,
  StdCtrls, Math, StrUtils, CommCtrl,
  JWsclTerminalServer, JwaWindows, AppEvnts;

type

TColumnSortType = (stAlpha, stNumeric, stDateTime, stData);

TColumnClicked = class
  Ascending: boolean;
  Column: TListColumn;
  Parent: TListView;
  constructor Create;
end;

// Class below is used to store bytes values, they are used when sorting the
// ListView columns
THiddenProcessData = class(TObject)
  MemUsage: DWORD;
  VMSize: DWORD;
end;

// Class below is used to store connectstate in ListView (not visible)
THiddenSessionData = class(TObject)
  ConnectState: TWtsConnectStateClass;
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
    ServerTreeView: TTreeView;
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
    UsersListView: TListView;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    ProcessesListView: TListView;
    SessionsListView: TListView;
    Button1: TButton;
    Button3: TButton;
    Button4: TButton;
    ApplicationEvents1: TApplicationEvents;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure SessionsListViewInsert(Sender: TObject; Item: TListItem);
    procedure SessionsListViewDeletion(Sender: TObject; Item: TListItem);
    procedure SetColumnImage(List: TListView; ColumnIndex, Image: Integer;
      ShowImage: Boolean);
    procedure ApplicationEvents1Message(var Msg: tagMSG; var Handled: Boolean);
    procedure ProcessesListViewDeletion(Sender: TObject; Item: TListItem);
    procedure ServerTreeViewDblClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    iActiveSessions: Integer;
    iDisconnectedSessions: Integer;
    procedure OnTerminalServerEvent(Sender: TObject);
    procedure UpdateTotals;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  IconRec: TIconRec;
  strServer: String;
  TerminalServer: TJwTerminalServer;

implementation

{$R *.dfm}
function QueryCurrentWinStation(lpWinStationName: PWideChar;
  lpUserName: PWideChar; var dwSessionId: DWORD; var dwWdFlag: DWORD): Boolean;
  stdcall; external 'utildll.dll';

constructor TColumnClicked.Create;
begin
  inherited create;
//  LastSortedColumn := -1;
  Ascending := True;
  Column := nil;
end;

// This event takes care of redrawing the sort glyph in the column
// header on WM_PAINT event
procedure TMainForm.ApplicationEvents1Message(var Msg: tagMSG;
  var Handled: Boolean);
var AListView: TListView;
  ColumnClicked: TColumnClicked;
  i: Integer;
begin
  if (ActiveControl.ClassType = TListView) and (Msg.message = WM_PAINT) then
  begin
    AListView := ActiveControl as TListView;
    ColumnClicked := TColumnClicked(AListView.Tag);

    if ColumnClicked.Column <> nil then
    begin
      for i := 0 to AListView.Columns.Count - 1 do
      begin
        // the Descending icon is no 9 in the Imagelist, the Ascending image
        // is no 10 (9 + Ord(ColumnClicked.Ascending)
        SetColumnImage(AListView, i, 9 + Ord(ColumnClicked.Ascending),
        i = ColumnClicked.Column.Index);
      end;
    end;
    // Last column takes up the remainder space
    AListView.Columns[AListView.Columns.Count-1].Width := -2;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
//TerminalServer: TJwTerminalServer;
  i: Integer;
  HiddenData: THiddenProcessData;
  FormatSettings: TFormatSettings;
begin
  ProcessesListView.Items.Clear;
  SessionsListView.Items.Clear;
  UsersListView.Items.Clear;

  // Get LocaleFormatSettings, we use them later on to format DWORD
  // values with bytes as KByte values with thousand seperators a la taskmgr
  GetLocaleFormatSettings(LOCALE_USER_DEFAULT, FormatSettings);

//  if TerminalServer.EnumerateProcesses then
  if TerminalServer.GetAllProcesses then
  begin
    ProcessesListView.Items.BeginUpdate;
    for i := 0 to TerminalServer.Processes.Count -1 do
    begin
      with TerminalServer.Processes[i] do
      begin
        with ProcessesListView.Items.Add do
        begin
          Caption := TerminalServer.Server;
          ImageIndex := IconRec.Server;

          SubItemImages[SubItems.Add(Username)] := IconRec.User;
          if WinStationName <> 'Console' then
          begin
            SubItemImages[SubItems.Add(WinStationName)] := IconRec.Console;
          end
          else begin
            SubItemImages[SubItems.Add(WinStationName)] := IconRec.Network;
          end;

          SubItems.Add(IntToStr(SessionId));
          SubItems.Add(IntToStr(ProcessId));
          SubItemImages[SubItems.Add(ProcessName)] := IconRec.Process;
          SubItems.Add(ProcessAge);
          SubItems.Add(ProcessCPUTime);
          // Convert to Kilobytes and display with thousands seperator
          // and no decimals
          SubItems.Add(Format('%.0n K', [ProcessMemUsage / 1024],
            FormatSettings));
          // Convert to Kilobytes and display with thousands seperator
          // and no decimals
          SubItems.Add(Format('%.0n K', [ProcessVMSize / 1024],
            FormatSettings));

          Data := THiddenProcessData.Create;
          with THiddenProcessData(Data) do
          begin
            MemUsage := ProcessMemUsage;
            VMSize := ProcessVMSize;
          end;
        end;
      end;
    end;
    ProcessesListView.Items.EndUpdate;
  end;

  if TerminalServer.EnumerateSessions then
  begin
    SessionsListView.Items.BeginUpdate;
    UsersListView.Items.BeginUpdate;

    for i := 0 to TerminalServer.Sessions.Count -1 do
    begin
      with TerminalServer.Sessions[i] do
      begin
        if ConnectState <> WTSListen then
        begin
          if UserName <> '' then
          begin
            with UsersListView.Items.Add do
            begin
              Caption := TerminalServer.Server;
              ImageIndex := IconRec.Server;
              if Username <> '' then
              begin
                SubItemImages[SubItems.Add(Username)] := IconRec.User;
              end
              else begin
                SubItems.Add('');
              end;

              if WdFlag < WD_FLAG_RDP then
              begin
                SubItemImages[SubItems.Add(WinStationName)] := IconRec.Console;
              end
              else begin
                SubItemImages[SubItems.Add(WinStationName)] := IconRec.Network;
              end;
              
              SubItems.Add(IntToStr(SessionId));
              SubItems.Add(ConnectStateStr);
              SubItems.Add(IdleTimeStr);
              SubItems.Add(LogonTimeStr);
            end;
          end;
        end;
        with SessionsListView.Items.Add do
        begin
          Caption := TerminalServer.Server;
          ImageIndex := IconRec.Server;
          if WinStationName = 'Console' then
          begin
            SubItemImages[SubItems.Add(WinStationName)] := IconRec.Console;
          end
          else begin
            SubItemImages[SubItems.Add(WinStationName)] := IconRec.Network;
          end;
          if Username <> '' then
          begin
            SubItemImages[SubItems.Add(Username)] := IconRec.User;
          end
          else begin
            SubItems.Add('');
          end;

          SubItems.Add(IntToStr(SessionId));
          SubItems.Add(ConnectStateStr);
          SubItems.Add(WinStationDriverName);
          SubItems.Add(ClientName);
          SubItems.Add(IdleTimeStr);
          SubItems.Add(LogonTimeStr);
          SubItems.Add(RemoteAddress);

          // Show Counters only for Active and non-console sessions
          if (ConnectState = WTSActive) and (WdFlag > WD_FLAG_CONSOLE) then
          begin
            SubItems.Add(IntToStr(IncomingBytes));
            SubItems.Add(IntToStr(OutgoingBytes));
            SubItems.Add(CompressionRatio);
          end;

          Data := THiddenSessionData.Create;
          THiddenSessionData(Data).ConnectState := ConnectState;
        end;
      end;
    end;
    SessionsListView.Items.EndUpdate;
    UsersListView.Items.EndUpdate;
  end;
//  TerminalServer.Free;
//  MessageBox(0, 'we have enumerated all', 'debug', MB_OK);
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  pWinStationName: PWideChar;
  pUserName: PWideChar;
  SessionId: Cardinal;
  WdFlag: Cardinal;
  Res: Boolean;
begin
  GetMem(pWinStationName, 66);
  GetMem(pUserName, 21);
  Res := QueryCurrentWinStation(pWinStationName, pUserName, SessionId, WdFlag);
  MessageBoxW(0, pWinStationName, pUserName, MB_OK);
  MessageBox(0, PChar(Format('WinStationName: %s, UserName: %s, SessionId: %d, WdFlag: %d', [pWinStationName, pUserName, SessionId, WdFlag])), 'QueryCurrentWinStation', MB_OK);
  FreeMem(pWinStationName);
  FreeMem(pUserName);
end;


procedure TMainForm.UpdateTotals;
begin
  StatusBar1.Panels[0].Text := Format(StatusBar1.Panels[0].Text,
    [iActiveSessions]);
  StatusBar1.Panels[1].Text := Format(StatusBar1.Panels[1].Text,
    [iDisconnectedSessions]);
  StatusBar1.Panels[2].Text := Format(StatusBar1.Panels[1].Text,
    [SessionsListView.Items.Count]);
end;

procedure TMainForm.Button3Click(Sender: TObject);
var WinStationInfoPtr: _WINSTATION_INFORMATIONW;
  dwReturnLength: DWORD;
begin
  ZeroMemory(@WinStationInfoPtr, SizeOf(WinStationInfoPtr));
  if WinstationQueryInformationW(SERVERNAME_CURRENT, 2, 8, @WinStationInfoPtr,
    SizeOf(WinStationInfoPtr), dwReturnLength) then
  begin
{  Memo1.Lines.Add(Format('Incoming Bytes: %d | Outgoing Bytes: %d', [WinStationInfoPtr.IncomingBytes, WinStationInfoPtr.OutgoingBytes]));
  Memo1.Lines.Add(Format('Incoming Frames: %d | Outgoing Frames: %d', [WinStationInfoPtr.IncomingFrames, WinStationInfoPtr.OutgoingFrames]));
  Memo1.Lines.Add(Format('Compression Ratio: %1.2f', [WinStationInfoPtr.OutgoingCompressBytes / WinStationInfoPtr.OutgoingBytes]));}
  {  Memo1.Lines.Add(Format('Value3: %d', [WinStationInfoPtr.Value3]));
  Memo1.Lines.Add(Format('Value4: %d', [WinStationInfoPtr.Value4]));
  Memo1.Lines.Add(Format('Value5: %d', [WinStationInfoPtr.Value5]));}
  end;
end;

procedure TMainForm.Button4Click(Sender: TObject);
var TerminalServer: TJwTerminalServer;
  i: Integer;
begin
  TerminalServer := TJwTerminalServer.Create;
  for i := 0 to TerminalServer.Servers.Count-1 do
  begin
    ServerTreeView.Items.AddChild(nil, TerminalServer.Servers[i]);
  end;
  TerminalServer.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
  c: Integer;
begin
  ReportMemoryLeaksOnShutDown := DebugHook <> 0;
  IconRec.Server := 11;
  IconRec.User := 12;
  IconRec.Network := 13;
  IconRec.Process := 14;
  IconRec.Console := 15;

  for i := 0 to ComponentCount - 1 do
  begin
    if Components[i].ClassType = TListView then
    begin
      with Components[i] as TListView do
      begin
        // Set sort type and sort handler
        SortType := stBoth;
        OnColumnClick := ListViewColumnClick;

        // ListView options
//        RowSelect := True;
//        HotTrack := True;
        RowSelect := True;
//        HotTrackStyles := [];
        SmallImages := ImageList1;

        // Create a ColumnClicked type for each ListView
        // (we store some sort data in there)
        Tag := Integer(Pointer(TColumnClicked.Create));
//        TColumnClicked(Tag).Parent := (Components[i] as TListView);

        // Enumerate Columns
        for c := 0 to Columns.Count - 1 do
        begin
          with Columns[c] do
          begin
            AutoSize := False;
            Width := -1;  // adjust the width to the longest item in the column

            // If column name ends with time set sort type to DateTime
            if RightStr(Caption, 4) = 'Time' then
            begin
              Tag := Integer(stAlpha);
            end
            // If column name ends with ID or Bytes set sort type to Numeric
            else if (RightStr(Caption, 2) = 'ID') or
                    (RightStr(Caption, 5) = 'Bytes') then begin
              Tag := Integer(stNumeric);
              Width := -1;
              Alignment := taRightJustify;
            end
            else if (LeftStr(Caption, 2) = 'VM') or
                    (LeftStr(Caption, 3) = 'Mem') then begin
              Tag := Integer(stData);
              Width := -1;
              Alignment := taRightJustify;
            end;
            // Other columns have tag = 0 (stAlpha) by default
          end;
          Columns[Columns.Count-1].Width := -2;
        end;
      end;
    end;
  end;

  TerminalServer := TJwTerminalServer.Create;
  TerminalServer.Server := strServer;
  TerminalServer.OnSessionEvent := OnTerminalServerEvent;  
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var i: integer;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    if Components[i].ClassType = TListView then
    begin
      with Components[i] as TListView do
      begin
        // Free ColumnClicked
        TColumnClicked(Tag).Free;
      end;
    end;
  end;
  TerminalServer.Free;
end;

procedure TMainForm.OnTerminalServerEvent(Sender: TObject);
var ATerminalServer: TJwTerminalServer;
begin
  with (Sender as TJwTerminalServer) do
  begin
    if LastEventFlag < WTS_EVENT_STATECHANGE then
    begin

    end;
  end;
end;

function SortByColumn(Item1, Item2: TListItem; Data: integer): integer; stdcall;
var ColumnClicked: TColumnClicked;
  n1, n2: Int64;
  s1, s2: String;
  SortType: TColumnSortType;
begin
  ColumnClicked := TColumnClicked(Data);
  SortType := TColumnSortType(ColumnClicked.Column.Tag);

  // 1st column is always alpha sort (servername)
  if ColumnClicked.Column.Index = 0 then
  begin
    Result := AnsiCompareText(Item1.Caption, Item2.Caption);
  end
  else begin
    // need to sort the clicked column (subitems) where
    // subitems = ColumnIndex -1
    if SortType = stNumeric then
    begin
      // Convert string to int64 (if the subitems does not exist, set it to -1)
      if Item1.SubItems.Count >= ColumnClicked.Column.Index then
      begin
        n1 := StrToInt64Def(Item1.SubItems[ColumnClicked.Column.Index-1], -1);
      end
      else begin
        n1 := -1;
      end;
      if Item2.SubItems.Count >= ColumnClicked.Column.Index then
      begin
        n2 := StrToInt64Def(Item2.SubItems[ColumnClicked.Column.Index-1], -1);
      end
      else begin
        n2 := -1;
      end;

      // Now compare
      if n1 > n2 then
      begin
        Result := 1
      end
      else if n1 < n2 then
      begin
        Result := -1
      end
      else begin
        Result := 0;
      end;
    end
    else if SortType = stData then
    begin
      if Item1.Data <> nil then
      begin
        if ColumnClicked.Column.Caption = 'Mem Usage' then
        begin
          n1 := THiddenProcessData(Item1.Data).MemUsage;
        end
        else begin
          n1 := THiddenProcessData(Item1.Data).VMSize;
        end;
      end
      else begin
        n1 := -1;
      end;

      if Item2.Data <> nil then
      begin
        if ColumnClicked.Column.Caption = 'Mem Usage' then
        begin
          n2 := THiddenProcessData(Item2.Data).MemUsage;
        end
        else begin
          n2 := THiddenProcessData(Item2.Data).VMSize;
        end;
      end
      else begin
        n2 := -1;
      end;

      // Now compare
      if n1 > n2 then
      begin
        Result := 1
      end
      else if n1 < n2 then
      begin
        Result := -1
      end
      else begin
        Result := 0;
      end;
    end
    else begin // stAlpha
      // Check if the column (subitem) exists, if not then set to empty string
      if Item1.SubItems.Count >= ColumnClicked.Column.Index then
      begin
        s1 := Item1.SubItems[ColumnClicked.Column.Index-1];
      end
      else begin
        s1 := '';
      end;
      // Check if the column (subitem) exists, if not then set to empty string
      if Item2.SubItems.Count >= ColumnClicked.Column.Index then
      begin
      s2 := Item2.SubItems[ColumnClicked.Column.Index-1];
      end
      else begin
        s2 := '';
      end;
      // Now compare
      Result := AnsiCompareText(s1, s2);
    end;
  end;

  // Reverse result when Ascending
  if not ColumnClicked.Ascending then Result := -Result;
end;

// SetColumnImage procedure was taken from
// http://www.delphi3000.com/articles/article_2063.asp?SK=
procedure TMainForm.SetColumnImage( List: TListView; ColumnIndex, Image: Integer;
                                 ShowImage: Boolean);
var
  Align,hHeader: integer;
  HD: HD_ITEM;

begin
  hHeader := SendMessage(List.Handle, LVM_GETHEADER, 0, 0);
  with HD do
  begin
    case List.Columns[ColumnIndex].Alignment of
      taLeftJustify:  Align := HDF_LEFT;
      taCenter:       Align := HDF_CENTER;
      taRightJustify: Align := HDF_RIGHT;
    else
      Align := HDF_LEFT;
    end;

    mask := HDI_IMAGE or HDI_FORMAT;

    pszText := PChar(List.Columns[ColumnIndex].Caption);

    if ShowImage then
      fmt := HDF_STRING or HDF_IMAGE or HDF_BITMAP_ON_RIGHT
    else
      fmt := HDF_STRING or Align;

    iImage := Image;
  end;
  SendMessage(hHeader, HDM_SETITEM, ColumnIndex, Integer(@HD));
end;

procedure TMainForm.ListViewColumnClick(Sender: TObject;
  Column: TListColumn);
var ColumnClicked: TColumnClicked;
  i: Integer;
  AListView: TListView;
begin
  AListView := Sender as TListView;
  ColumnClicked := TColumnClicked(AListView.Tag);

  if Column = ColumnClicked.Column then
  begin
    ColumnClicked.Ascending := not ColumnClicked.Ascending;
  end
  else begin
    // Remove column image
    ColumnClicked.Column := Column;
//    ColumnClicked.LastSortedColumn := Column.Index;
  end;

  // Customsort Routine
  AListView.CustomSort(@SortByColumn, Integer(ColumnClicked));

  // This loop displays the icon in the selected column.
  for i := 0 to AListView.Columns.Count-1 do
  begin
    // the Descending icon is no 9 in the Imagelist, the Ascending image
    // is no 10 (9 + Ord(ColumnClicked.Ascending)
    SetColumnImage(AListView, i, 9 + Ord(ColumnClicked.Ascending),
      i = Column.Index);
  end;

  AListView := nil;
  AListView.Free;
end;

procedure TMainForm.ProcessesListViewDeletion(Sender: TObject; Item: TListItem);
begin
  if Item.Data <> nil then
  begin
    THiddenProcessData(Item.Data).Free;
    Item.Data := nil;
  end;
end;

procedure TMainForm.ServerTreeViewDblClick(Sender: TObject);
begin
  strServer := ServerTreeView.Selected.Text;
  Button1.Click;
end;

procedure TMainForm.SessionsListViewDeletion(Sender: TObject; Item: TListItem);
begin
  if Item.Data <> nil then
  begin
    THiddenProcessData(Item.Data).Free;
    Item.Data := nil;
  end;
end;

procedure TMainForm.SessionsListViewInsert(Sender: TObject; Item: TListItem);
begin
  if Item.Data <> nil then
  begin
    case THiddenSessionData(Item.Data).ConnectState of
      WTSActive: Inc(iActiveSessions);
      WTSDisconnected: Inc(IDisconnectedSessions);
    end;
    UpdateTotals;
  end;
end;

end.
