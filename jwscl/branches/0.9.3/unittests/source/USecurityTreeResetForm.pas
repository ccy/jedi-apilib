{@abstract(This unit contains a form to display progress information)
@author(Christian Wimmer)
@created(03/23/2007)
@lastmod(09/10/2007)

Project JEDI Windows Security Code Library (JWSCL)

The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License. You may obtain a copy of the
License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.

The Original Code is JwsvclSecurityTreeResetForm.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.


Description:
This unit contains a form to display progress information for changing a huge
amount of files, regkeys or other objects that support inheritance.
It is displayed to show the user the progress of changine the security information
of a object system (usually files or reg keys).
}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $
unit USecurityTreeResetForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, JvThread, JvThreadDialog, JvComponentBase, JvgFixFont,
  StdCtrls, Buttons, ExtCtrls,
  jwaWindows,
  JwsclTypes,
  JwsclExceptions,
  JwsclStrings;

type
  TSM_SecurityTreeReset_Form = class(TForm)
    Object_Label: TLabel;
    Cancel_BitBtn: TBitBtn;
    Animate_Ctrl: TAnimate;
    Timer_Label: TLabel;
    Timer_Main: TTimer;
    procedure FormShow(Sender: TObject);
    procedure Timer_MainTimer(Sender: TObject);
    procedure Cancel_BitBtnClick(Sender: TObject);
  private
    { Private-Deklarationen }
    iTime : TTime;
    bCancel : Boolean;
  public
     procedure FNProgressMethod(
                                const pObjectName : TJwString;              // Name of object just processed
                                const cStatus : Cardinal;                    // Status of operation on object
                                var pInvokeSetting : TJwProgInvokeSetting; // When to set
                                const E : EJwsclSecurityException;         //Exception type that was raised or nil if none- only supported by TJwSecureFileObject.TreeFileObjectSetNamedSecurityInfo
                                const Args : Pointer;                      // Caller specific data
                                const bSecuritySet : Boolean                 // Whether security was set
                                );
  public
    { Public-Deklarationen }
    class function CreateSecurityTreeResetForm(parent : HWND) : TSM_SecurityTreeReset_Form; overload;
    class function CreateSecurityTreeResetForm(parent : TComponent) : TSM_SecurityTreeReset_Form; overload;
  end;

var
  SM_SecurityTreeReset_Form: TSM_SecurityTreeReset_Form;



implementation

{$R *.dfm}

{ TSM_SecurityTreeReset_Form }

class function TSM_SecurityTreeReset_Form.CreateSecurityTreeResetForm(
  parent: HWND): TSM_SecurityTreeReset_Form;
begin
  result := TSM_SecurityTreeReset_Form.CreateParented(parent);
  result.Show;
end;

class function TSM_SecurityTreeReset_Form.CreateSecurityTreeResetForm(
  parent: TComponent): TSM_SecurityTreeReset_Form;
begin
  result := TSM_SecurityTreeReset_Form.Create(parent);
  result.Show;
end;

procedure TSM_SecurityTreeReset_Form.FNProgressMethod(
  const pObjectName: TJwString; const cStatus: Cardinal;
  var pInvokeSetting: TJwProgInvokeSetting; const E: EJwsclSecurityException;
  const Args: Pointer; const bSecuritySet: Boolean);
var R : Integer;
begin
  Object_Label.Caption := pObjectName;
  Update;
  Application.ProcessMessages;

  if pInvokeSetting = pis_ProgressInvokeEveryObject then      
  begin
    if bCancel then
      pInvokeSetting := pis_ProgressCancelOperation;
  end
  else
  if pInvokeSetting = pis_ProgressInvokeOnError then
  begin
{$IFDEF UNICODE}
    R := MessageBoxW(GetActiveWindow,TJwPChar(EJwsclSecurityException.GetErrorMessage(cStatus) + #13#10+pObjectName),'Error',MB_CANCELTRYCONTINUE);
{$ELSE}
    R := MessageBoxA(GetActiveWindow,TJwPChar(EJwsclSecurityException.GetErrorMessage(cStatus) + #13#10+pObjectName),'Error',MB_CANCELTRYCONTINUE);
{$ENDIF}
    case R of
      IDTRYAGAIN : pInvokeSetting := pis_ProgressRetryOperation;
      IDCONTINUE : pInvokeSetting := pis_ProgressInvokeEveryObject;
    else
      pInvokeSetting := pis_ProgressCancelOperation;
    end;
  end
  else
  if pInvokeSetting = pis_ProgressFinished then
  begin
  end;

end;

procedure TSM_SecurityTreeReset_Form.FormShow(Sender: TObject);
begin
  Animate_Ctrl.Active := true;
  bCancel := false;
  Cancel_BitBtn.Caption := '&Cancel';
  iTime := Now;
  Timer_MainTimer(Sender);
  Timer_Main.Enabled := true;
end;

procedure TSM_SecurityTreeReset_Form.Timer_MainTimer(Sender: TObject);
begin
  Timer_Label.Caption := TimeToStr(Now - iTime);
end;

procedure TSM_SecurityTreeReset_Form.Cancel_BitBtnClick(Sender: TObject);
begin
  bCancel := true;
  Cancel_BitBtn.Caption := 'Canceling';
end;

end.
