{@abstract(This unit provides access to Terminal Server api functions)
@author(Remko Weijnen)
@created(10/26/2007)
@lastmod(10/26/2007)
This unit contains types that are used by the units of the Security Manager Suite


Project JEDI Windows Security Code Library (JWSCL)

The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License. You may obtain a copy of the
License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.

The Original Code is JwsclTerminalServer.pas.

The Initial Developer of the Original Code is Remko Weijnen.
Portions created by Remko Weijnen are Copyright (C) Remko Weijnen. All rights reserved.
}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclTerminalServer;
{$I Jwscl.inc}

interface

uses Classes, Contnrs,
  JwaWindows,
  jwsclTypes, JwsclStrings;

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
type

  { forward declarations }
  TJwTerminalServer = class;
  TJwWtsSession = class;
  TJwWtsSessionList = class;
  TJwWtsProcess = class;
  TJwWtsProcessList = class;

  TJwTerminalServer = class(TPersistent)
  private
    FSessions: TJwWtsSessionList;
    FServer: TJwString;
    hServer: THandle;
    procedure SetServer(const Value: TJwString);
  protected
    function Open: THandle;
    procedure Close;
  published
    property Server: TJwString read FServer write SetServer;
    constructor Create; reintroduce;
    function Enumerate: boolean;
  end;

  TJwWtsSession = class(TPersistent)
  private
  protected
    FOwner: TJwWtsSessionList;
    SessionId: TJwSessionId;
    WinStationName: TjwString;
    State: TJwState;
  published
    constructor Create(AOwner: TjwWTSSessionList; ASessionId: TJwSessionId; AWinStationName: TJwString;
      AState: TjwState); reintroduce;
  end;

  TJwWtsSessionList = class(TObjectList)
  private
  protected
    FOwner: TjwTerminalServer;
    function GetOwner: TJwTerminalServer;

    //    function GetItem(AIndex: integer): TJwTWtsSession;
//    function GetOwner: TJwTerminalServer;
  public
    constructor Create(AOwner: TJwTerminalServer);
//    procedure Delete(Index: integer); reintroduce;
//    procedure Refresh;
//    function FindSession(ASessionId: TJwSessionID): TJwTWtsSession;
//    function FindClientSession(AClientName: TJwString): TJwTWtsSession;
//    function FindUser(AUserName: TJwString) : TJwTWtsSession;
//    property Items[AIndex: integer]: TJwTWtsSession read GetItem; default;
  end;

  TJwWtsProcess = class(TPersistent)
  private
    FOwner: TjwWtsProcessList;
    FSessionId: TJwSessionID;
    FProcessId: TJwProcessID;
    FProcessName: TJwString;
  protected
  public
  end;

  TJwWtsProcessList = class(TPersistent)
  protected
//    function GetItem(AIndex: integer): TJwWtsProcess;
//    function GetOwner: TJwTerminalServer; override;
  public
//    constructor Create(AOwner: TPersistent);
//    procedure Delete(Index: integer);
//    procedure Refresh;
//    function GetProcess(AProcessName: TJwString): TJwWtsProcess; overload;
//    function GetProcess(AProcessId: DWORD): TJwWtsProcess; overload;
//    property Items[AIndex: integer]: TJwWtsProcess read GetItem; default;
  end;

  _WINSTATIONQUERYINFORMATIONW = record
    State: DWORD;
    WinStationNameName: array[0..10] of WideChar;
    Unknown1: array[0..10] of byte;
    Unknown3: array[0..10] of WideChar;
    Unknown2: array[0..8] of byte;
    SessionId: Longint;
    Reserved2: array[0..3] of byte;
    ConnectTime: FILETIME;
    DisconnectTime: FILETIME;
    LastInputTime: FILETIME;
    LoginTime: FILETIME;
    Reserved3: array[0..1011] of byte;
    Domain: array[0..17] of WideChar;
    UserName: array[0..22] of WideChar;
    CurrentTime: FILETIME;
  end;

  PJwWtsSessionInfoAArray = ^TJwWtsSessionInfoAArray;
  TJwWtsSessionInfoAArray = array[0..ANYSIZE_ARRAY-1] of WTS_SESSION_INFOA;

  PJwWtsSessionInfoWArray = ^TJwWtsSessionInfoWArray;
  TJwWtsSessionInfoWArray = array[0..ANYSIZE_ARRAY-1] of WTS_SESSION_INFOW;

{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}

implementation
{$ENDIF SL_OMIT_SECTIONS}

constructor TJwTerminalServer.Create;
begin
  inherited Create;
  FSessions.Create(Self);
end;

procedure TJwTerminalServer.SetServer(const Value: TJwString);
begin
  FServer := Value;
end;

function TJwTerminalServer.Enumerate: boolean;
var SessionInfoPtr: {$IFDEF UNICODE}PJwWtsSessionInfoWArray;
  {$ELSE}PJwWtsSessionInfoAArray;{$ENDIF UNICODE}
  pCount: Cardinal;
  i: integer;
  Res: Longbool;
  ASession: TjwWTSSession;
begin
  Open;
  Res :=
  {$IFDEF UNICODE}
    WtsEnumerateSessionsW(hServer, 0, 1, PWTS_SESSION_INFOW(SessionInfoPtr),
      pCount);
  {$ELSE}
    WtsEnumerateSessions(hServer, 0, 1, PWTS_SESSION_INFOA(SessionInfoPtr),
      pCount);
  {$ENDIF UNICODE}

  // Clear the sessionslist
  FSessions.Clear;

  // Add all sessions
  for i := 0 to pCount - 1 do
  begin
    ASession := TJwWTSSession.Create(FSessions, SessionInfoPtr^[i].SessionId,
      SessionInfoPtr^[i].pWinStationName, TJwState(SessionInfoPtr^[i].State));
  end;

  // Pass the result
  Result := Res;
  Close;
end;

function TJwTerminalServer.Open: THandle;
begin
  if FServer = '' then
  begin
    Result := WTS_CURRENT_SERVER_HANDLE;
  end
  else begin
    {$IFDEF UNICODE}
      Result := WTSOpenServerW(PWideChar(WideString(FServer)));
    {$ELSE}
      Result := WTSOpenServer(PChar(FServer));
    {$ENDIF}
  end;
end;

procedure TJwTerminalServer.Close;
begin
  if hServer <> WTS_CURRENT_SERVER_HANDLE then
  begin
    WTSCloseServer(hServer);
  end;
end;

constructor TJwWtsSessionList.Create(AOwner: TjwTerminalServer);
begin
  FOwner := AOwner;
end;

function TJwWtsSessionList.GetOwner;
begin
  Result := FOwner;
end;

constructor TJwWtsSession.Create(AOwner: TjwWTSSessionList; ASessionId: Cardinal; AWinStationName: string; AState: TJwState);
begin
//  handle
end;
end.
