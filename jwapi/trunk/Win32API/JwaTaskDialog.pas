{******************************************************************************}
{                                                                              }
{ JwaTaskDialog API interface Unit for Object Pascal                           }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2008 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ Portions created by Christian Wimmer are Copyright (C) 2010                  }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project JEDI         }
{ APILIB home page, located at http://jedi-apilib.sourceforge.net              }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaTaskDialog;
{$ENDIF JWA_OMIT_SECTIONS}

{.$HPPEMIT ''}
{.$HPPEMIT '#include "xxxx.h"'}
{.$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface
{$IFDEF JWA_WINDOWS}
  uses JwaWindows;
{$ELSE}
  uses JwaWinType, JwaWinUser;
{$ENDIF JWA_WINDOWS}

{$ENDIF JWA_OMIT_SECTIONS}

{$IFDEF WIN2008_UP}
  {$DEFINE SUPPORT_TASK_DIALOG}
{$ENDIF WIN2008_UP}

{$IFDEF WINVISTA_UP}
  {$DEFINE SUPPORT_TASK_DIALOG}
{$ENDIF WINVISTA_UP}


{$IFNDEF JWA_IMPLEMENTATIONSECTION}

{$IFDEF SUPPORT_TASK_DIALOG}
type
  PFTASKDIALOGCALLBACK = function (
      {__in} hwnd : HWND;
      {__in} msg : UINT;
      {__in} wParam : WPARAM;
      {__in} lParam : LPARAM;
      {__in} lpRefData : LONG_PTR) : HRESULT; stdcall;
  {$EXTERNALSYM PFTASKDIALOGCALLBACK}

const
  //_TASKDIALOG_FLAGS = (
    TDF_ENABLE_HYPERLINKS               = $0001;
    {$EXTERNALSYM TDF_ENABLE_HYPERLINKS}
    TDF_USE_HICON_MAIN                  = $0002;
    {$EXTERNALSYM TDF_USE_HICON_MAIN}
    TDF_USE_HICON_FOOTER                = $0004;
    {$EXTERNALSYM TDF_USE_HICON_FOOTER}
    TDF_ALLOW_DIALOG_CANCELLATION       = $0008;
    {$EXTERNALSYM TDF_ALLOW_DIALOG_CANCELLATION}
    TDF_USE_COMMAND_LINKS               = $0010;
    {$EXTERNALSYM TDF_USE_COMMAND_LINKS}
    TDF_USE_COMMAND_LINKS_NO_ICON       = $0020;
    {$EXTERNALSYM TDF_USE_COMMAND_LINKS_NO_ICON}
    TDF_EXPAND_FOOTER_AREA              = $0040;
    {$EXTERNALSYM TDF_EXPAND_FOOTER_AREA}
    TDF_EXPANDED_BY_DEFAULT             = $0080;
    {$EXTERNALSYM TDF_EXPANDED_BY_DEFAULT}
    TDF_VERIFICATION_FLAG_CHECKED       = $0100;
    {$EXTERNALSYM TDF_VERIFICATION_FLAG_CHECKED}
    TDF_SHOW_PROGRESS_BAR               = $0200;
    {$EXTERNALSYM TDF_SHOW_PROGRESS_BAR}
    TDF_SHOW_MARQUEE_PROGRESS_BAR       = $0400;
    {$EXTERNALSYM TDF_SHOW_MARQUEE_PROGRESS_BAR}
    TDF_CALLBACK_TIMER                  = $0800;
    {$EXTERNALSYM TDF_CALLBACK_TIMER}
    TDF_POSITION_RELATIVE_TO_WINDOW     = $1000;
    {$EXTERNALSYM TDF_POSITION_RELATIVE_TO_WINDOW}
    TDF_RTL_LAYOUT                      = $2000;
    {$EXTERNALSYM TDF_RTL_LAYOUT}
    TDF_NO_DEFAULT_RADIO_BUTTON         = $4000;
    {$EXTERNALSYM TDF_NO_DEFAULT_RADIO_BUTTON}
    TDF_CAN_BE_MINIMIZED                = $8000;
    {$EXTERNALSYM TDF_CAN_BE_MINIMIZED}
  //);
type
  TASKDIALOG_FLAGS = Integer;                         // Note: _TASKDIALOG_FLAGS is an int
  {$EXTERNALSYM TASKDIALOG_FLAGS}

const
  //_TASKDIALOG_MESSAGES = (
    TDM_NAVIGATE_PAGE                   = WM_USER+101;
    {$EXTERNALSYM TDM_NAVIGATE_PAGE}
    TDM_CLICK_BUTTON                    = WM_USER+102; // wParam = Button ID
    {$EXTERNALSYM TDM_CLICK_BUTTON}
    TDM_SET_MARQUEE_PROGRESS_BAR        = WM_USER+103; // wParam = 0 (nonMarque) wParam != 0 (Marquee)
    {$EXTERNALSYM TDM_SET_MARQUEE_PROGRESS_BAR}
    TDM_SET_PROGRESS_BAR_STATE          = WM_USER+104; // wParam = new progress state
    {$EXTERNALSYM TDM_SET_PROGRESS_BAR_STATE}
    TDM_SET_PROGRESS_BAR_RANGE          = WM_USER+105; // lParam = MAKELPARAM(nMinRange, nMaxRange)
    {$EXTERNALSYM TDM_SET_PROGRESS_BAR_RANGE}
    TDM_SET_PROGRESS_BAR_POS            = WM_USER+106; // wParam = new position
    {$EXTERNALSYM TDM_SET_PROGRESS_BAR_POS}
    TDM_SET_PROGRESS_BAR_MARQUEE        = WM_USER+107; // wParam = 0 (stop marquee), wParam != 0 (start marquee), lparam = speed (milliseconds between repaints)
    {$EXTERNALSYM TDM_SET_PROGRESS_BAR_MARQUEE}
    TDM_SET_ELEMENT_TEXT                = WM_USER+108; // wParam = element (TASKDIALOG_ELEMENTS), lParam = new element text (LPCWSTR)
    {$EXTERNALSYM TDM_SET_ELEMENT_TEXT}
    TDM_CLICK_RADIO_BUTTON              = WM_USER+110; // wParam = Radio Button ID
    {$EXTERNALSYM TDM_CLICK_RADIO_BUTTON}
    TDM_ENABLE_BUTTON                   = WM_USER+111; // lParam = 0 (disable), lParam != 0 (enable), wParam = Button ID
    {$EXTERNALSYM TDM_ENABLE_BUTTON}
    TDM_ENABLE_RADIO_BUTTON             = WM_USER+112; // lParam = 0 (disable), lParam != 0 (enable), wParam = Radio Button ID
    {$EXTERNALSYM TDM_ENABLE_RADIO_BUTTON}
    TDM_CLICK_VERIFICATION              = WM_USER+113; // wParam = 0 (unchecked), 1 (checked), lParam = 1 (set key focus)
    {$EXTERNALSYM TDM_CLICK_VERIFICATION}
    TDM_UPDATE_ELEMENT_TEXT             = WM_USER+114; // wParam = element (TASKDIALOG_ELEMENTS), lParam = new element text (LPCWSTR)
    {$EXTERNALSYM TDM_UPDATE_ELEMENT_TEXT}
    TDM_SET_BUTTON_ELEVATION_REQUIRED_STATE = WM_USER+115; // wParam = Button ID, lParam = 0 (elevation not required), lParam != 0 (elevation required)
    {$EXTERNALSYM TDM_SET_BUTTON_ELEVATION_REQUIRED_STATE}
    TDM_UPDATE_ICON                     = WM_USER+116;  // wParam = icon element (TASKDIALOG_ICON_ELEMENTS), lParam = new icon (hIcon if TDF_USE_HICON_* was set, PCWSTR otherwise)
    {$EXTERNALSYM TDM_UPDATE_ICON}
  //);
type
  TASKDIALOG_MESSAGES = Integer;
  {$EXTERNALSYM TASKDIALOG_MESSAGES}

const
  //_TASKDIALOG_NOTIFICATIONS = (
    TDN_CREATED                         = 0;
    {$EXTERNALSYM TDN_CREATED}
    TDN_NAVIGATED                       = 1;
    {$EXTERNALSYM TDN_NAVIGATED}
    TDN_BUTTON_CLICKED                  = 2;            // wParam = Button ID
    {$EXTERNALSYM TDN_BUTTON_CLICKED}
    TDN_HYPERLINK_CLICKED               = 3;            // lParam = (LPCWSTR)pszHREF
    {$EXTERNALSYM TDN_HYPERLINK_CLICKED}
    TDN_TIMER                           = 4;            // wParam = Milliseconds since dialog created or timer reset
    {$EXTERNALSYM TDN_TIMER}
    TDN_DESTROYED                       = 5;
    {$EXTERNALSYM TDN_DESTROYED}
    TDN_RADIO_BUTTON_CLICKED            = 6;            // wParam = Radio Button ID
    {$EXTERNALSYM TDN_RADIO_BUTTON_CLICKED}
    TDN_DIALOG_CONSTRUCTED              = 7;
    {$EXTERNALSYM TDN_DIALOG_CONSTRUCTED}
    TDN_VERIFICATION_CLICKED            = 8;             // wParam = 1 if checkbox checked, 0 if not, lParam is unused and always 0
    {$EXTERNALSYM TDN_VERIFICATION_CLICKED}
    TDN_HELP                            = 9;
    {$EXTERNALSYM TDN_HELP}
    TDN_EXPANDO_BUTTON_CLICKED          = 10;            // wParam = 0 (dialog is now collapsed), wParam != 0 (dialog is now expanded)
    {$EXTERNALSYM TDN_EXPANDO_BUTTON_CLICKED}
//  );
TYPE
  TASKDIALOG_NOTIFICATIONS = Integer;
  {$EXTERNALSYM TASKDIALOG_NOTIFICATIONS}

type
  _TASKDIALOG_BUTTON = packed record
    nButtonID : Integer;
    pszButtonText : PWideChar;
  end;
  {$EXTERNALSYM _TASKDIALOG_BUTTON}
  TASKDIALOG_BUTTON = _TASKDIALOG_BUTTON;
  {$EXTERNALSYM TASKDIALOG_BUTTON}

  PTaskDialogButton = ^TTaskDialogButton;
  TTaskDialogButton = TASKDIALOG_BUTTON;

const
  //_TASKDIALOG_ELEMENTS = (
    TDE_CONTENT = 0;
    {$EXTERNALSYM TDE_CONTENT}
    TDE_EXPANDED_INFORMATION = 1;
    {$EXTERNALSYM TDE_EXPANDED_INFORMATION}
    TDE_FOOTER = 2;
    {$EXTERNALSYM TDE_FOOTER}
    TDE_MAIN_INSTRUCTION = 4;
    {$EXTERNALSYM TDE_MAIN_INSTRUCTION}
  //);
type
  TASKDIALOG_ELEMENTS = Integer;
  {$EXTERNALSYM TASKDIALOG_ELEMENTS}

const
  //_TASKDIALOG_ICON_ELEMENTS = (
    TDIE_ICON_MAIN = 0;
    {$EXTERNALSYM TDIE_ICON_MAIN}
    TDIE_ICON_FOOTER = 1;
    {$EXTERNALSYM TDIE_ICON_FOOTER}
  //);
type
  TASKDIALOG_ICON_ELEMENTS = Integer;
  {$EXTERNALSYM TASKDIALOG_ICON_ELEMENTS}

const
  TD_WARNING_ICON         = MAKEINTRESOURCEW(Word(-1));
  {$EXTERNALSYM TD_WARNING_ICON}
  TD_ERROR_ICON           = MAKEINTRESOURCEW(Word(-2));
  {$EXTERNALSYM TD_ERROR_ICON}
  TD_INFORMATION_ICON     = MAKEINTRESOURCEW(Word(-3));
  {$EXTERNALSYM TD_INFORMATION_ICON}
  TD_SHIELD_ICON          = MAKEINTRESOURCEW(Word(-4));
  {$EXTERNALSYM TD_SHIELD_ICON}

const
  //_TASKDIALOG_COMMON_BUTTON_FLAGS = (
    TDCBF_OK_BUTTON            = $0001; // selected control return value IDOK
    {$EXTERNALSYM TDCBF_OK_BUTTON}
    TDCBF_YES_BUTTON           = $0002; // selected control return value IDYES
    {$EXTERNALSYM TDCBF_YES_BUTTON}
    TDCBF_NO_BUTTON            = $0004; // selected control return value IDNO
    {$EXTERNALSYM TDCBF_NO_BUTTON}
    TDCBF_CANCEL_BUTTON        = $0008; // selected control return value IDCANCEL
    {$EXTERNALSYM TDCBF_CANCEL_BUTTON}
    TDCBF_RETRY_BUTTON         = $0010; // selected control return value IDRETRY
    {$EXTERNALSYM TDCBF_RETRY_BUTTON}
    TDCBF_CLOSE_BUTTON         = $0020; // selected control return value IDCLOSE
    {$EXTERNALSYM TDCBF_CLOSE_BUTTON}
  //);
type
  TASKDIALOG_COMMON_BUTTON_FLAGS = Integer;           // Note: _TASKDIALOG_COMMON_BUTTON_FLAGS is an int
  {$EXTERNALSYM TASKDIALOG_COMMON_BUTTON_FLAGS}

type
  TASKDIALOGCONFIG = packed record
    cbSize : UINT;
    hwndParent : HWND;
    hInstance : HINST;
    dwFlags : TASKDIALOG_FLAGS;
    dwCommonButtons : TASKDIALOG_COMMON_BUTTON_FLAGS;
    pszWindowTitle : PWideChar;
    case Integer of
      0 : (hMainIcon : HICON);
      1 : (pszMainIcon : PWideChar;
           pszMainInstruction : PWideChar;
           pszContent : PWideChar;
           cButtons : UINT;
           pButtons : PTaskDialogButton;
           nDefaultButton : Integer;
           cRadioButtons : UINT;
           pRadioButtons : PTaskDialogButton;
           iDefaultRadioButton : Integer;
           pszVerificationText,
           pszExpandedInformation,
           pszExpandedControlText,
           pszCollapsedControlText : PWideChar;
           case Integer of
             0 : (hFooterIcon : HICON);
             1 : (pszFooterIcon : PWideChar;
                  pszFooterText : PWideChar;
                  pfCallback : PFTASKDIALOGCALLBACK;
                  lpCallbackData : Pointer;
                  cxWidth : UINT;
                 )
          );
  end;
  {$EXTERNALSYM TASKDIALOGCONFIG}
  PTaskDialogConfig = ^TASKDIALOGCONFIG;
  TTaskDialogConfig = TASKDIALOGCONFIG;



function TaskDialogIndirect(
  var ptc : TTaskDialogConfig;
  pnButton: PInteger;
  pnRadioButton: PInteger;
  pfVerificationFlagChecked: PBool): HRESULT; stdcall;
  {$EXTERNALSYM TaskDialogIndirect}

function TaskDialog(
  hwndParent: HWND;
  hInstance: longword;
  pszWindowTitle: PWideChar;
  pszMainInstruction : PWideChar;
  pszContent: PWideChar;
  dwCommonButtons: TASKDIALOG_COMMON_BUTTON_FLAGS;
  pszIcon : LPWSTR;
  pnButton: Pinteger): HRESULT; stdcall;
  {$EXTERNALSYM TaskDialog}

{$ENDIF SUPPORT_TASK_DIALOG}

{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation
uses JwaWinDllNames;
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}


{$IFDEF DYNAMIC_LINK}

{$IFDEF SUPPORT_TASK_DIALOG}
var
  _TaskDialogIndirect: Pointer;

function TaskDialogIndirect;
begin
  GetProcedureAddress(_TaskDialogIndirect, comctl32, 'TaskDialogIndirect');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TaskDialogIndirect]
  end;
end;

var
  _TaskDialog: Pointer;

function TaskDialog;
begin
  GetProcedureAddress(_TaskDialog, comctl32, 'TaskDialog');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TaskDialog]
  end;
end;
{$ENDIF SUPPORT_TASK_DIALOG}

{$ELSE}

{$IFDEF SUPPORT_TASK_DIALOG}
function TaskDialogIndirect; external comctl32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'TaskDialogIndirect';
function TaskDialog; external comctl32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'TaskDialog';
{$ENDIF SUPPORT_TASK_DIALOG}

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
