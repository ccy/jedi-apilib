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
// Last modified: $Date: 2007-26-10 10:00:00 +0100 $

interface

uses JwaWtsApi32, JwsclWinStations;

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
const WinStationInformation = 8;

type
  TJwTerminalServer = class;

  TJwTWtsSession = class(TPersistent)
  private
  protected
    FOwner: TJwTWtsSessionList;
    FSessionId: TJwSessionID;
    FState: DWORD;
    FWinStationName: string;
    procedure SetSessionId(const Value: TJwSessionID);
    procedure SetState(const Value: DWORD);
    procedure SetWinStationName(const Value: string);
  published
    constructor Create; reintroduce;
  end;

  TJwWtsSessionList = class(TPersistent)
  protected
    function    GetItem(AIndex: integer): TJwTWtsSession;
    function    GetOwner: TJwTerminalServer;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure   Delete(Index: integer); override;
    procedure   Refresh; override;
    function    GetSession(ASessionId: TJwSessionID): TJwTWtsSession; overload;
    function    GetSession(AClientName: string): TJwTWtsSession; overload;
    function    GetSession(AUserName: string) : TJwTWtsSession; overload;
    property    Items[AIndex: integer]: TJwTWtsSession read GetItem; default;
  end;

  TJwWtsProcess = class(TPersistent)
  private
    FOwner:       TjwWtsProcessList;
    FSessionId:   TJwSessionID;
    FProcessId:   DWORD;
    FProcessName: string;
  protected
  public
  published
  end;

  TJwWtsProcessList = class(TPersistent)
  protected
    function    GetItem(AIndex: integer): TJwWtsProcess;
    function    GetOwner: TJwTerminalServer;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure   Delete(Index: integer); override;
    procedure   Refresh; override;
    function    GetProcess(AProcessName: string): TJwWtsProcess; overload;
    function    GetProcess(AProcessId: DWORD): TJwWtsProcess; overload;
    property    Items[AIndex: integer]: TJwWtsProcess read GetItem; default;
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
  TJwWtsSessionInfoAArray = array[0..ANYSIZE_ARRAY] of WTS_SESSION_INFOA;
{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}

implementation

{$ENDIF SL_OMIT_SECTIONS}

end.
