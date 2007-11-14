{
@abstract(Contains structures to support auto free class instances.)
@author(Christian Wimmer)
@created(03/23/2007)
@lastmod(09/10/2007)
This unit contains types that are used by the units of the Security Manager Suite


Project JEDI Windows Security Code Library (JWSCL)

The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License. You may obtain a copy of the
License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.

The Original Code is JwsclComUtils.pas.

The Initial Developer of the Original Code is Robert Giesecke.

To use this code simply to the follwoing:
@longCode(#
var myClass : TMyClass;
begin
  myClass := TMyClass.Create;
  TJwAutoPointer.Wrapt(myClass);

  ... your code here ...
end; //autodestruction of myClass here.
#)


}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclComUtils;
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $

interface

uses
  Classes;
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
const
  IIDIJwAutoPointer = '{08360FA5-F278-45EF-98FE-6C43DF9C778B}';

type
  IJwAutoPointer = interface
  [IIDIJwAutoPointer]
    function GetPointer   : Pointer;
    function GetInstance  : TObject;
    property Pointer   : Pointer read GetPointer;
    property Instance  : TObject read GetInstance;
  end;

  TJwAutoPointer = class
    class function Wrap(const Instance  : TObject) : IJwAutoPointer; overload;
    class function CreateInstance(
      const ClassReference : TClass) : IJwAutoPointer; overload;
    class function CreateInstance(
      const ClassReference : TComponentClass;
      const Owner          : TComponent = nil) : IJwAutoPointer; overload;
  end;

{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation
{$ENDIF SL_OMIT_SECTIONS}


{$IFNDEF SL_INTERFACE_SECTION}
type
  TJwAutoPointerImpl = class(TInterfacedObject, IJwAutoPointer)
  private
    fInstance : TObject;
    function GetInstance: TObject;
    function GetPointer: Pointer;
  public
    constructor Create(const Instance : TObject);
    procedure BeforeDestruction; override;
  end;

{ AutoPointer }

class function TJwAutoPointer.Wrap(const Instance: TObject): IJwAutoPointer;
begin
  Result := TJwAutoPointerImpl.Create(instance);
end;

class function TJwAutoPointer.CreateInstance(
  const ClassReference : TClass): IJwAutoPointer;
begin
  Result := Wrap(classReference.Create());
end;

class function TJwAutoPointer.CreateInstance(
  const ClassReference : TComponentClass;
  const Owner : TComponent): IJwAutoPointer;
begin
  Result := Wrap(classReference.Create(nil));
end;

{ TAutoPinterImpl }

function TJwAutoPointerImpl.GetInstance : TObject;
begin
  Result := fInstance;
end;

function TJwAutoPointerImpl.GetPointer : Pointer;
begin
  Result := fInstance;
end;

procedure TJwAutoPointerImpl.BeforeDestruction;
begin
  fInstance.Free();
  inherited;
end;

constructor TJwAutoPointerImpl.Create(const Instance : TObject);
begin
  fInstance := Instance;
end;

{$ENDIF SL_INTERFACE_SECTION}


{$IFNDEF SL_OMIT_SECTIONS}
initialization
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INITIALIZATION_SECTION}
{$ENDIF SL_INITIALIZATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}