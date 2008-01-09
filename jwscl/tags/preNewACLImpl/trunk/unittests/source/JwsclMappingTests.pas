unit JwsclMappingTests;

interface

uses
  JwsclMapping,
  JwaWindows,
  TestFrameWork;

type
  TSecurityMappingTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

  published

    // Test methods
    procedure TestCheckAndStrip;
    procedure TestMapAccessMaskToString;

    procedure TestAll_GetMapping;

  end;


implementation

{ TSecurityMappingTests }
{$I Compilers.inc}

procedure TSecurityMappingTests.TestAll_GetMapping;
var Maps : String;
begin
  {TJwSecurityGenericMapping}
  Maps := TJwSecurityGenericMapping.MapAccessMaskToString(GENERIC_READ or GENERIC_WRITE or GENERIC_EXECUTE);
  {$IFDEF COMPILER_5}CheckEqualsWideString{$ELSE}CheckEquals{$ENDIF}('Bit 31 [Generic] or Bit 30 [Generic] or Bit 0 [Specific]',Maps);

  Maps := TJwSecurityGenericMapping.MapAccessMaskToString(GENERIC_ALL);
  {$IFDEF COMPILER_5}CheckEqualsWideString{$ELSE}CheckEquals{$ENDIF}('Bit 29 [Generic] or Bit 0 [Specific]',Maps);

(* We cannot test these strings because they depend on the language. 
  {TJwSecurityFileMapping}
  Maps := TJwSecurityFileMapping.MapAccessMaskToString(TJwSecurityFileMapping.Map(GENERIC_READ));
  {$IFDEF COMPILER_5}CheckEqualsWideString{$ELSE}CheckEquals{$ENDIF}('STANDARD_RIGHTS_READ or FILE_READ_DATA or FILE_READ_ATTRIBUTES or FILE_READ_EA or SYNCHRONIZE',Maps);

  Maps := TJwSecurityFileMapping.MapAccessMaskToString(TJwSecurityFileMapping.Map(GENERIC_WRITE));
  {$IFDEF COMPILER_5}CheckEqualsWideString{$ELSE}CheckEquals{$ENDIF}('STANDARD_RIGHTS_READ or SYNCHRONIZE or FILE_WRITE_DATA or FILE_WRITE_ATTRIBUTES or FILE_WRITE_EA or FILE_APPEND_DATA',Maps);

  Maps := TJwSecurityFileMapping.MapAccessMaskToString(TJwSecurityFileMapping.Map(GENERIC_EXECUTE));
  {$IFDEF COMPILER_5}CheckEqualsWideString{$ELSE}CheckEquals{$ENDIF}('STANDARD_RIGHTS_READ or FILE_READ_ATTRIBUTES or SYNCHRONIZE or FILE_EXECUTE',Maps);

  Maps := TJwSecurityFileMapping.MapAccessMaskToString(TJwSecurityFileMapping.Map(GENERIC_ALL));
  {$IFDEF COMPILER_5}CheckEqualsWideString{$ELSE}CheckEquals{$ENDIF}('STANDARD_RIGHTS_REQUIRED or $1FF or SYNCHRONIZE',Maps);


  {TJwSecurityRegistryMapping}
  Maps := TJwSecurityRegistryMapping.MapAccessMaskToString(TJwSecurityRegistryMapping.Map(GENERIC_READ));
  {$IFDEF COMPILER_5}CheckEqualsWideString{$ELSE}CheckEquals{$ENDIF}('KEY_QUERY_VALUE or KEY_ENUMERATE_SUB_KEYS or KEY_NOTIFY or STANDARD_RIGHTS_READ',Maps);

  Maps := TJwSecurityRegistryMapping.MapAccessMaskToString(TJwSecurityRegistryMapping.Map(GENERIC_WRITE));
  {$IFDEF COMPILER_5}CheckEqualsWideString{$ELSE}CheckEquals{$ENDIF}('KEY_SET_VALUE or KEY_CREATE_SUB_KEY or STANDARD_RIGHTS_READ',Maps);

  Maps := TJwSecurityRegistryMapping.MapAccessMaskToString(TJwSecurityRegistryMapping.Map(GENERIC_EXECUTE));
  {$IFDEF COMPILER_5}CheckEqualsWideString{$ELSE}CheckEquals{$ENDIF}('KEY_QUERY_VALUE or KEY_ENUMERATE_SUB_KEYS or KEY_NOTIFY or STANDARD_RIGHTS_READ',Maps);

  Maps := TJwSecurityRegistryMapping.MapAccessMaskToString(TJwSecurityRegistryMapping.Map(GENERIC_ALL));
  {$IFDEF COMPILER_5}CheckEqualsWideString{$ELSE}CheckEquals{$ENDIF}('STANDARD_RIGHTS_ALL or KEY_QUERY_VALUE or KEY_ENUMERATE_SUB_KEYS or KEY_NOTIFY or KEY_SET_VALUE or KEY_CREATE_SUB_KEY or KEY_CREATE_LINK',Maps);
*)
end;

procedure TSecurityMappingTests.TestCheckAndStrip;
begin

end;

procedure TSecurityMappingTests.TestMapAccessMaskToString;
begin

end;

initialization

  TestFramework.RegisterTest('JwsclMappingTests Suite',
    TSecurityMappingTests.Suite);

end.
 