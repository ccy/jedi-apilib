unit JwsclCoMapping;
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, StdVcl, Classes, JWSCLCom_TLB,
  JwaWindows,JwaVista, TypInfo, ComLib,
  JWSCLSid, JwsclMapping;

type
  TJwGenericMappingImpl = class(TAutoObject, IJwGenericMapping)
  //TJwCoSid = class(TTypedComObject, IJwCoSid, ISupportErrorInfo)
  protected
    fInternalObject : TJwSecurityGenericMapping;

    function GetMapping: JWSCLCom_TLB.TGenericMapping; safecall;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; safecall;
    function GetAccessNames(out Count: LongWord): PChar; safecall;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;


  TJwFileFolderMappingImpl = class(TJwGenericMappingImpl, IJwFileFolderMapping)
  end;

  TJwFileMappingImpl = class(TJwGenericMappingImpl, IJwFileMapping)
  end;

  TJwRegistryMappingImpl = class(TJwGenericMappingImpl, IJwRegistryMapping)
  end;

  TJwWinStationMappingImpl = class(TJwGenericMappingImpl, IJwWinStationMapping)
  end;

  TJwDesktopMappingImpl = class(TJwGenericMappingImpl, IJwDesktopMapping)
  end;

  TJwServiceMappingImpl = class(TJwGenericMappingImpl, IJwServiceMapping)
  end;

  TJwServiceManagerMappingImpl = class(TJwGenericMappingImpl, IJwServiceManagerMapping)
  end;

  TJwPrinterMappingImpl = class(TJwGenericMappingImpl, IJwPrinterMapping)
  end;

  TJwShareMappingImpl = class(TJwGenericMappingImpl, IJwShareMapping)
  end;

  TJwProcessMappingImpl = class(TJwGenericMappingImpl, IJwProcessMapping)
  end;

  TJwThreadMappingImpl = class(TJwGenericMappingImpl, IJwThreadMapping)
  end;

  TJwJobMappingImpl = class(TJwGenericMappingImpl, IJwJobMapping)
  end;

  TJwSemaphoreMappingImpl = class(TJwGenericMappingImpl, IJwSemaphoreMapping)
  end;

  TJwEventMappingImpl = class(TJwGenericMappingImpl, IJwEventMapping)
  end;

  TJwMutexMappingImpl = class(TJwGenericMappingImpl, IJwMutexMapping)
  end;

  TJwFileMapMappingImpl = class(TJwGenericMappingImpl, IJwFileMapMapping)
  end;

  TJwTimerMappingImpl = class(TJwGenericMappingImpl, IJwTimerMapping)
  end;

  TJwTokenMappingImpl = class(TJwGenericMappingImpl, IJwTokenMapping)
  end;

  TJwPipeMappingImpl = class(TJwGenericMappingImpl, IJwPipeMapping)
  end;


implementation
uses ComServ,  SysUtils, Dialogs,
     JwsclUtils, JwsclExceptions, Variants,
     JwsclCOMExports, JwsclTypes,
     JWSCLCoException;

{ TJwGenericMapping }

type
  TJwGenericMappingImplClass = class of TJwGenericMappingImpl;


  TImplMap = record
    Impl : TJwGenericMappingImplClass;
    Jw : TJwSecurityGenericMappingClass;
  end;

const ImplMaps : array[0..19] of TImplMap = (

   (Impl: TJwFileMappingImpl;
    Jw : TJwSecurityFileMapping),
   (Impl: TJwRegistryMappingImpl;
    Jw : TJwSecurityRegistryMapping),
   (Impl: TJwWinStationMappingImpl;
    Jw : TJwSecurityWinStationMapping),
   (Impl: TJwDesktopMappingImpl;
    Jw : TJwSecurityDesktopMapping),
   (Impl: TJwFileFolderMappingImpl;
    Jw : TJwSecurityFileFolderMapping),
   (Impl: TJwServiceMappingImpl;
    Jw : TJwSecurityServiceMapping),
   (Impl: TJwServiceManagerMappingImpl;
    Jw : TJwSecurityServiceManagerMapping),
   (Impl: TJwPrinterMappingImpl;
    Jw : TJwSecurityPrinterMapping),
   (Impl: TJwShareMappingImpl;
    Jw : TJwSecurityShareMapping),
   (Impl: TJwProcessMappingImpl;
    Jw : TJwSecurityProcessMapping),
   (Impl: TJwThreadMappingImpl;
    Jw : TJwSecurityThreadMapping),
   (Impl: TJwJobMappingImpl;
    Jw : TJwSecurityJobMapping),
   (Impl: TJwSemaphoreMappingImpl;
    Jw : TJwSecuritySemaphoreMapping),
   (Impl: TJwEventMappingImpl;
    Jw : TJwSecurityEventMapping),
   (Impl: TJwMutexMappingImpl;
    Jw : TJwSecurityMutexMapping),
   (Impl: TJwFileMapMappingImpl;
    Jw : TJwSecurityFileMapping),
   (Impl: TJwTimerMappingImpl;
    Jw : TJwSecurityTimerMapping),
   (Impl: TJwTokenMappingImpl;
    Jw : TJwSecurityTokenMapping),
   (Impl: TJwPipeMappingImpl;
    Jw : TJwSecurityPipeMapping),
{}
   (Impl: TJwGenericMappingImpl;
    Jw : TJwSecurityGenericMapping)
    );


destructor TJwGenericMappingImpl.Destroy;
begin
  inherited;
end;

function TJwGenericMappingImpl.GetAccessNames(out Count: LongWord): PChar;
begin
  result := PCHAR(fInternalObject.GetAccessNames(Count));
end;

function TJwGenericMappingImpl.GetMapping: JWSCLCom_TLB.TGenericMapping;
var G : TGenericMapping;
begin
  G := fInternalObject.GetMapping;
  result.GenericRead := G.GenericRead;
  result.GenericWrite := G.GenericWrite;
  result.GenericExecute := G.GenericExecute;
  result.GenericAll := G.GenericAll;
end;

procedure TJwGenericMappingImpl.Initialize;
var i : Integer;
begin
  inherited;

  fInternalObject := nil;
   for i := low(ImplMaps) to high(ImplMaps) do
  begin
    if Self is ImplMaps[i].Impl then
    begin
      fInternalObject := ImplMaps[i].Jw.Create;
      break;
    end;
  end;

  if not Assigned(fInternalObject) then
    fInternalObject := TJwSecurityGenericMapping.Create;
  showmessage(fInternalObject.ClassName);
end;

function TJwGenericMappingImpl.MapAccessMaskToString(AccessMask: LongWord): WideString;
begin
  result := fInternalObject.MapAccessMaskToString(AccessMask);
end;

initialization
  TAutoObjectFactory.Create(ComServer, TJwGenericMappingImpl, CLASS_JwGenericMapping,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwFileMappingImpl, CLASS_JwFileMapping,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwRegistryMappingImpl, CLASS_JwRegistryMapping,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwWinStationMappingImpl, CLASS_JwWinStationMapping,
    ciMultiInstance, tmApartment);   
  TAutoObjectFactory.Create(ComServer, TJwDesktopMappingImpl, CLASS_JwDesktopMapping,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwFileFolderMappingImpl, CLASS_JwFileFolderMapping,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwServiceMappingImpl, CLASS_JwServiceMapping,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwServiceManagerMappingImpl, CLASS_JwServiceMappingManager,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwPrinterMappingImpl, CLASS_JwPrinterMapping,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwShareMappingImpl, CLASS_JwShareMapping,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwProcessMappingImpl, CLASS_JwProcessMapping,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwThreadMappingImpl, CLASS_JwThreadMapping,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwJobMappingImpl, CLASS_JwJobMapping,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwSemaphoreMappingImpl, CLASS_JwSemaphoreMapping,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwEventMappingImpl, CLASS_JwEventMapping,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwMutexMappingImpl, CLASS_JwMutexMapping,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwFileMapMappingImpl, CLASS_JwFileMapMapping,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwTimerMappingImpl, CLASS_JwTimerMapping,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwTokenMappingImpl, CLASS_JwTokenMapping,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwPipeMappingImpl, CLASS_JwPipeMapping,
    ciMultiInstance, tmApartment);

end.
