{
Description
Project JEDI Windows Security Code Library (JWSCL)

This project is not intended for production but only for analyzing purposes.

Author
Christian Wimmer
License
The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"); you may not use this file except in compliance with the
\License. You may obtain a copy of the License at http://www.mozilla.org/MPL

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
specific language governing rights and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the GNU
Lesser General Public License (the "LGPL License"), in which case the provisions
of the LGPL License are applicable instead of those above. If you wish to allow
use of your version of this file only under the terms of the LGPL License and
not to allow others to use your version of this file under the MPL, indicate
your decision by deleting the provisions above and replace them with the notice
and other provisions required by the LGPL License. If you do not delete the
provisions above, a recipient may use your version of this file under either the
MPL or the LGPL License.

For more information about the LGPL: <i>http://www.gnu.org/copyleft/lesser.html</i>


Note
The Original Code is JwsclMain.dpr.

The Initial Developer of the Original Code is Christian Wimmer. Portions created
by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

Version
The following values are automatically injected by Subversion on commit.
<table>
\Description                                                        Value
------------------------------------------------------------------  ------------
Last known date the file has changed in the repository              \$Date$
Last known revision number the file has changed in the repository   \$Revision$
Last known author who changed the file in the repository.           \$Author$
Full URL to the latest version of the file in the repository.       \$HeadURL$
</table>
}
{$APPTYPE CONSOLE}
Program JwsclMain;
{$INCLUDE ..\includes\Jwscl.inc}

{$IFNDEF DEBUG}
  {$WARNINGS ON}
  {$MESSAGE WARN 'JwsclMain.dpr is not intended for production but only for analyzing purposes.'}
{$ELSE}
  {$MESSAGE HINT 'JwsclMain.dpr was compiled.'}
{$ENDIF}

{$IFNDEF DELPHI7_UP}
Support for Delphi 5 and 6 has ceased.
{$ENDIF DELPHI7_UP}


uses
  JwaWindows,
  JwsclAcl in 'JwsclAcl.pas',
  JwsclAuthCtx in 'JwsclAuthCtx.pas',
  JwsclCertificates in 'JwsclCertificates.pas',
  JwsclComSecurity in 'JwsclComSecurity.pas',
  JwsclComUtils in 'JwsclComUtils.pas',
  JwsclConstants in 'JwsclConstants.pas',
  JwsclCredentials in 'JwsclCredentials.pas',
  JwsclCryptProvider in 'JwsclCryptProvider.pas',
  JwsclDescriptor in 'JwsclDescriptor.pas',
  JwsclDesktops in 'JwsclDesktops.pas',
  JwsclElevation in 'JwsclElevation.pas',
  JwsclEncryption in 'JwsclEncryption.pas',
  JwsclEnumerations in 'JwsclEnumerations.pas',
  JwsclExceptions in 'JwsclExceptions.pas',
  JwsclFirewall in 'JwsclFirewall.pas',
  JwsclImpersonation in 'JwsclImpersonation.pas',
  JwsclKnownSid in 'JwsclKnownSid.pas',
  JwsclLogging in 'JwsclLogging.pas',
  JwsclLsa in 'JwsclLsa.pas',
  JwsclMapping in 'JwsclMapping.pas',
  JwsclPrivileges in 'JwsclPrivileges.PAS',
  JwsclProcess in 'JwsclProcess.pas',
  JwsclRegistry in 'JwsclRegistry.pas',
  JwsclResource in 'JwsclResource.pas',
  JwsclSecureObjects in 'JwsclSecureObjects.pas',
  JwsclSecurityDialogs in 'JwsclSecurityDialogs.pas',
  JwsclSid in 'JwsclSid.pas',
  JwsclSimpleDescriptor in 'JwsclSimpleDescriptor.pas',
  JwsclStreams in 'JwsclStreams.pas',
  JwsclStrings in 'JwsclStrings.pas',
  JwsclTerminalServer in 'JwsclTerminalServer.pas',
  JwsclToken in 'JwsclToken.pas',
  JwsclTypes in 'JwsclTypes.pas',
  JwsclUtils in 'JwsclUtils.pas',
  JwsclVersion in 'JwsclVersion.pas',
  JwsclWinStations in 'JwsclWinStations.pas';

begin
end.
