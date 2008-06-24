unit SyncObjsEx;

interface
uses Windows, SyncObjs, JwsclUtils;

type
  {TMutexEx extends the standard VCL class TMutex with additional functions
   * Aquire mutex with timeout
   * Try 	acquirement of mutex}
  TMutexEx = class(TMutex)
  protected
    fEvent : SyncObjs.TEvent;
  public
    {Standard call with infinite timeout. Blocks until the mutex
    is freed.
    raises
      EAbort This exception will be raised if the StopEven is fired. The exception
       prevents code to be executed after the return of the method.
    }
    procedure Acquire; overload; override;

    {Releases the mutex}
    procedure Release; override;

    {Like the method Acquire it waits until the mutex is free.
     If the specified timeout is reached the method also returns.
     @return Returns true if the mutex is free. If the timeout has been
     reached the return value is false.
    }
    function Acquire(const TimeOut : DWORD) : Boolean; overload; virtual;

    {TryAcquire checks the mutex signal state and returns immediately.
     @return Returns true if the mutex is free otherwise false.
    }
    function TryAcquire : boolean; virtual;

    {WaitFor is an extended version that supports a stop event that can let
     return any Acquire method early.
    }
    function WaitFor(Timeout: LongWord): TWaitResult; override;

    {The stop event makes it possible to return from a waiting state early.
    All Acquire methods blocks until the mutex is free, a timeout has occured
    or an error. However if you create a TEvent instance and assigns it to
    this property before any Acquire methods, you can return immediately
    any acquirement call.
    Setting the event after a call to Acquire has no effect.
    Closing an event while it is used can have unpredictable results.
    }
    property StopEvent : SyncObjs.TEvent read fEvent write fEvent;
  end;


implementation
uses SysUtils;

{ TMutexEx }

procedure TMutexEx.Acquire;
var WR : TWaitResult;
begin
  WR := WaitFor(INFINITE);
  case WR of
    wrError : RaiseLastOSError;
    wrTimeout : raise EAbort.Create('StopEvent fired');
  end;
end;

function TMutexEx.Acquire(const TimeOut : DWORD) : Boolean;
var WR : TWaitResult;
begin
  WR := WaitFor(TimeOut);
  case WR of
    wrError : RaiseLastOSError;
    wrAbandoned,
    wrSignaled : result := true;
  else
    //wrTimeout
    result := false;
  end;
end;

function TMutexEx.WaitFor(Timeout: LongWord): TWaitResult;
var
  Index: DWORD;
begin
  if FUseCOMWait or
   not Assigned(fEvent) then
  begin
    result := inherited WaitFor(Timeout);
  end else
  begin
    case JwWaitForMultipleObjects([FHandle, fEvent.Handle], false, Timeout) of
      WAIT_ABANDONED: Result := wrAbandoned;
      WAIT_OBJECT_0: Result := wrSignaled;
      WAIT_OBJECT_0+1,
      WAIT_TIMEOUT: Result := wrTimeout;
      WAIT_FAILED:
        begin
          Result := wrError;
          FLastError := GetLastError;
        end;
    else
      Result := wrError;
    end;
  end;
end;

procedure TMutexEx.Release;
begin
  inherited;

end;

function TMutexEx.TryAcquire: boolean;
begin
  result := Acquire(0);
end;

end.
