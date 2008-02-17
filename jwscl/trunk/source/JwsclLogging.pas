{@abstract()
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

Alternatively, the contents of this file may be used under the terms of the  
GNU Lesser General Public License (the  "LGPL License"), in which case the   
provisions of the LGPL License are applicable instead of those above.        
If you wish to allow use of your version of this file only under the terms   
of the LGPL License and not to allow others to use your version of this file 
under the MPL, indicate your decision by deleting  the provisions above and  
replace  them with the notice and other provisions required by the LGPL      
License.  If you do not delete the provisions above, a recipient may use     
your version of this file under either the MPL or the LGPL License.          
                                                                             
For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html 

The Original Code is JwsclLogging.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

Description:

}
{.$IFNDEF SL_OMIT_SECTIONS}
unit JwsclLogging;

{$INCLUDE Jwscl.inc}
interface
uses Classes,
     SyncObjs,
     SysUtils, 
     JwsclStrings;

type TJwLogType = (
        lsMessage,
        lsWarning,
        lsError,
        lsStop);
     TJwEnterType = (
        etNone,
        etFunction,
        etMethod,
        etThread);

     TJwSignalType = (
        stSend,
        stReceived,
        stWait);

     TJwMemoryType = (
        mtAlloc,
        mtDeAlloc);

    TJwXMLTag = (
        xtLogFile,
        xtLogProcess,
        xtEnter,
        xtLeave,
        xtSignal,
        xtMemory,
        xtLog,
        xtException,
        xtType,
        xtGuid,
        xtGetLastError,
        xtWinApiFunction,
        xtLogString,
        xtComSource,
        xtStackTrace,
        xtMessage,
        xtLastErrorString,
        xtSourceProc);

    TJwXMLAttrTag = (
        atStart,
        atEnd,
        atType,
        atMethod,
        atClass,
        atFile,
        atThread,
        atSource,
        atTarget,
        atMemType
        );

     TJwXMLAttribute = record
       Name, Value : TJwString;
     end;
     
     TJwXMLAttributes = array of TJwXMLAttribute;

     TJwLogWriter = class
     protected
       fStartMTags,
       fMultipleTags : Boolean;
       fTagName : TJwString;
       fIndLevel : Integer;
     public
       function WriteSingleTag(IndLevel : Integer; const TagName, Value : TJwString;
         const Attributes : TJwXMLAttributes) : TJwString; virtual;

       function StartWriteMultipleTags(const IndLevel : Integer; const TagName : TJwString;
         const Attributes : TJwXMLAttributes) : TJwString; virtual;
       function EndWriteMultipleTags : TJwString; virtual;

       class procedure AddAttribute(var Attr : TJwXMLAttributes; const Name, Value : TJwString);
       class procedure AddAttributes(var Attr : TJwXMLAttributes; const ClassName, MethodName, FileName : TJwString);

       class function FormatString(const Str : TJwString) : TJwString;
       class function GetThreadName : TJwString;
     end;

     TJwLogWriterClass = class of TJwLogWriter;

     IJwLogClient = interface (IInterface)
       ['{B7202309-4766-4D62-9E16-ECE5953C2AEA}']
        procedure Log(const LogType : TJwLogType; const LogMessage : TJwString); overload; safecall;
        procedure Log(const LogType : TJwLogType; const ClassName, MethodName, FileName, LogMessage : TJwString); overload; safecall;

        procedure Signal(const SignalType : TJwSignalType; const Source, Target, LogMessage : TJwString); overload; safecall;
        procedure Signal(const SignalType : TJwSignalType; const Source, Target, ClassName, MethodName, FileName, LogMessage : TJwString); overload; safecall;

        procedure Memory(const MemoryType : TJwMemoryType; const MemType, LogMessage : TJwString); overload; safecall;
        procedure Memory(const MemoryType : TJwMemoryType; const MemType, ClassName, MethodName, FileName, LogMessage : TJwString); overload; safecall;

        procedure Exception(const E : Exception); overload; safecall;
        procedure Exception(const E : Exception; const ClassName, MethodName, FileName : TJwString); overload; safecall;
     end;

     IJwLogServer = interface (IInterface)
       ['{1B3EC217-2F6D-4FE2-A9DC-BF7E8C025D4F}']
       function Connect(const EnterType : TJwEnterType; const ClassName, MethodName, FileName, MessageText : TJwString) : IJwLogClient; safecall;
       procedure Disconnect(var Client : IJwLogClient); safecall;
     end;


function CreateLogServer(const Elements : TStringList; const WriterClass : TJwLogWriterClass = nil) : IJwLogServer;

var JwStrNewLine : String = #13#10;
    JwStrIdent : String = #9;
    JwTimeOutputString : String = 'dd.mm.yyyy hh:nn:ss:zzz';

    JwLogTypeStrings : array[TJwLogType] of TJwString = (
         'message',
         'warning',
         'error',
         'stop');

    JwEnterTypeString : array[TJwEnterType] of TJwString = (
         '',
         'function',
         'method',
         'thread');

    JwSignalTypeString : array[TJwSignalType] of TJwString = (
         'send',
         'received',
         'wait');

    JwMemoryTypeString : array[TJwMemoryType] of TJwString = (
         'alloc',
         'dealloc');


    JwXMLTagsString : array[TJwXMLTag] of TJwString = (
        'logfile',
        'logprocess',
        'enter',
        'leave',
        'signal',
        'memory',
        'log',
        'exception',
        'type',
        'guid',
        'getlasterror',
        'winapifunction',
        'logstring',
        'comsource',
        'stacktrace',
        'message',
        'getlasterrorstring',
        'sourceproc');

    JwXMLAttributeString : array[TJwXMLAttrTag] of TJwString = (
        'start',
        'end',
        'type',
        'method',
        'class',
        'file',
        'thread',
        'source',
        'target',
        'memtype');
        
implementation
uses JwaWindows, JwsclExceptions, JwsclUtils;

type TJwLogServerImpl = class;

     TJwLogClientImpl = class(TInterfacedObject, IJwLogClient)
     protected
       fOwner : TJwLogServerImpl;
       fInd : Integer;
       fClassName,
       fMethodName, fFileName, fMessageText : TJwString;
       fEnterType : TJwEnterType;
       fWriter : TJwLogWriter;
     public
       constructor Create(Owner : TJwLogServerImpl;
          const EnterType : TJwEnterType; const ClassName, MethodName, FileName, MessageText : TJwString);
       destructor Destroy; override;

       procedure Log(const LogType : TJwLogType; const LogMessage : TJwString); overload; safecall;
       procedure Log(const LogType : TJwLogType; const ClassName, MethodName, FileName, LogMessage : TJwString); overload; safecall;

       procedure Signal(const SignalType : TJwSignalType; const Source, Target, LogMessage : TJwString); overload; safecall;
       procedure Signal(const SignalType : TJwSignalType; const Source, Target, ClassName, MethodName, FileName, LogMessage : TJwString); overload; safecall;

       procedure Memory(const MemoryType : TJwMemoryType; const MemType, LogMessage : TJwString); overload; safecall;
       procedure Memory(const MemoryType : TJwMemoryType; const MemType, ClassName, MethodName, FileName, LogMessage : TJwString); overload; safecall;

       procedure Exception(const E : Exception); overload; safecall;
       procedure Exception(const E : Exception; const ClassName, MethodName, FileName: TJwString); overload; safecall;
     end;

     TJwLogServerImpl = class(TInterfacedObject, IJwLogServer)
     protected
       fElements : TStringList;
       fWriterClass : TJwLogWriterClass;
       fWriter : TJwLogWriter;
       fInd : Integer;
       fProcessLogTag : TJwString;
       fCritical : SyncObjs.TCriticalSection;
       procedure EnterCriticalSection;
       procedure LeaveCriticalSection;
     public
       constructor Create(const Elements : TStringList; const WriterClass : TJwLogWriterClass);
       destructor Destroy; override;

       function Connect(const EnterType : TJwEnterType; const ClassName, MethodName, FileName, MessageText : TJwString) : IJwLogClient;safecall;
       procedure Disconnect(var Client : IJwLogClient); safecall;
     end;

function CreateLogServer(const Elements : TStringList; const WriterClass : TJwLogWriterClass = nil) : IJwLogServer;
begin
  result := TJwLogServerImpl.Create(Elements,WriterClass);
end;

{ TJwLogClientImpl }

constructor TJwLogClientImpl.Create(Owner : TJwLogServerImpl;
  const EnterType : TJwEnterType; 
  const ClassName, MethodName, FileName, MessageText : TJwString);
var Attributes : TJwXMLAttributes;
begin
  fOwner := Owner;
  fClassName := ClassName;
  fMethodName := MethodName;
  fFileName := FileName;
  fMessageText := MessageText;
  fEnterType := EnterType;
  fInd := fOwner.fInd+1;
  fWriter := fOwner.fWriterClass.Create;


  if EnterType <> etNone then
  begin
    Inc(fInd);
    fOwner.EnterCriticalSection;
    try
      fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atType], JwEnterTypeString[EnterType]);
      fOwner.fWriterClass.AddAttributes(Attributes, fClassName, fMethodName, fFileName);
      fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atThread], fOwner.fWriterClass.GetThreadName);

      fOwner.fElements.Add(fWriter.WriteSingleTag(fOwner.fInd+1, JwXMLTagsString[xtEnter], MessageText, Attributes));
    finally
      fOwner.LeaveCriticalSection;
    end;
  end;
end;

destructor TJwLogClientImpl.Destroy;
var Attributes : TJwXMLAttributes;
begin
  if fEnterType <> etNone then
  begin
    fOwner.EnterCriticalSection;
    try
      fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atType], JwEnterTypeString[fEnterType]);
      fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atThread], fOwner.fWriterClass.GetThreadName);
      fOwner.fElements.Add(fWriter.WriteSingleTag(fOwner.fInd+1, JwXMLTagsString[xtLeave], fMessageText, Attributes));
    finally
      fOwner.LeaveCriticalSection;
    end;
  end;

  inherited;
end;

procedure TJwLogClientImpl.Log(const LogType: TJwLogType;
  const LogMessage: TJwString);
begin
  Log(LogType, '', '', '', LogMessage); 
end;

procedure TJwLogClientImpl.Log(const LogType : TJwLogType; const ClassName, MethodName, FileName, LogMessage : TJwString); safecall;
var Attributes : TJwXMLAttributes;
begin
  fOwner.EnterCriticalSection;
  try
    fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atType],JwLogTypeStrings[LogType]);
    fOwner.fWriterClass.AddAttributes(Attributes, ClassName, MethodName, FileName);
    fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atThread], fOwner.fWriterClass.GetThreadName);
    fOwner.fElements.Add(fWriter.WriteSingleTag(fInd, JwXMLTagsString[xtLog], LogMessage, Attributes));
  finally
    fOwner.LeaveCriticalSection;
  end;
end;



procedure TJwLogClientImpl.Memory(const MemoryType: TJwMemoryType;
  const MemType, LogMessage: TJwString);
begin
  Memory(MemoryType, MemType, '', '', '', LogMessage);
end;

procedure TJwLogClientImpl.Memory(const MemoryType: TJwMemoryType;
  const MemType, ClassName, MethodName, FileName, LogMessage: TJwString);
var Attributes : TJwXMLAttributes;
begin
  fOwner.EnterCriticalSection;
  try
    fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atType], JwMemoryTypeString[MemoryType]);
    fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atMemType], MemType);
    fOwner.fWriterClass.AddAttributes(Attributes, ClassName, MethodName, FileName);
    fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atThread], fOwner.fWriterClass.GetThreadName);
    
    fOwner.fElements.Add(fWriter.WriteSingleTag(fInd, JwXMLTagsString[xtMemory], LogMessage, Attributes));
  finally
    fOwner.LeaveCriticalSection;
  end;
end;

procedure TJwLogClientImpl.Signal(const SignalType: TJwSignalType;
  const Source, Target, LogMessage: TJwString);
begin
  Signal(SignalType, Source, Target, '', '', '', LogMessage);
end;

procedure TJwLogClientImpl.Signal(const SignalType: TJwSignalType;
  const Source, Target, ClassName, MethodName, FileName,
  LogMessage: TJwString);
var Attributes : TJwXMLAttributes;
begin
  fOwner.EnterCriticalSection;
  try
    fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atType], JwSignalTypeString[SignalType]);
    fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atSource], Source);
    fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atTarget], Target);
    fOwner.fWriterClass.AddAttributes(Attributes, ClassName, MethodName, FileName);
    fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atThread], fOwner.fWriterClass.GetThreadName);
    fOwner.fElements.Add(fWriter.WriteSingleTag(fInd, JwXMLTagsString[xtSignal], LogMessage, Attributes));
  finally
    fOwner.LeaveCriticalSection;
  end;
end;


procedure TJwLogClientImpl.Exception(const E: Exception);
begin
  Exception(E, '', '', '');
end;

procedure TJwLogClientImpl.Exception(const E: Exception; const ClassName,
  MethodName, FileName: TJwString);
var Attributes : TJwXMLAttributes;
    Writer : TJwLogWriter;
    JE : EJwsclSecurityException;
begin
  fOwner.EnterCriticalSection;

  Writer := fOwner.fWriterClass.Create;
  try

    fOwner.fWriterClass.AddAttributes(Attributes, ClassName, MethodName, FileName);
    fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atThread], fOwner.fWriterClass.GetThreadName);

    fOwner.fElements.Add(Writer.StartWriteMultipleTags(fInd, JwXMLTagsString[xtException], Attributes));

    fOwner.fElements.Add(Writer.WriteSingleTag(fOwner.fInd,
        JwXMLTagsString[xtType], E.ClassName, Attributes));

    fOwner.fElements.Add(Writer.WriteSingleTag(fOwner.fInd,
        JwXMLTagsString[xtGuid], GUIDToString(JwMapException(E.ClassName)) , Attributes));
        
    if E is EJwsclSecurityException then
    begin
      JE := E as EJwsclSecurityException;

      fOwner.fElements.Add(Writer.WriteSingleTag(fOwner.fInd,
        JwXMLTagsString[xtGetLastError], IntToStr(JE.LastError), Attributes));

      fOwner.fElements.Add(Writer.WriteSingleTag(fOwner.fInd,
        JwXMLTagsString[xtLastErrorString], JE.GetLastErrorMessage(JE.LastError), Attributes));

      if Length(Trim(JE.WinCallName)) > 0 then
        fOwner.fElements.Add(Writer.WriteSingleTag(fOwner.fInd,
          JwXMLTagsString[xtWinApiFunction], JE.WinCallName, Attributes));

      if Length(Trim(JE.Log)) > 0 then
        fOwner.fElements.Add(Writer.WriteSingleTag(fOwner.fInd,
          JwXMLTagsString[xtLogString], JE.Log , Attributes));

      if Length(Trim(JE.ComSource)) > 0 then
        fOwner.fElements.Add(Writer.WriteSingleTag(fOwner.fInd,
          JwXMLTagsString[xtComSource], JE.ComSource, Attributes));

      if Length(Trim(JE.StackTrace)) > 0 then
        fOwner.fElements.Add(Writer.WriteSingleTag(fOwner.fInd,
          JwXMLTagsString[xtStackTrace], JE.StackTrace , Attributes));

      if Length(Trim(JE.SourceProc)) > 0 then
        fOwner.fElements.Add(Writer.WriteSingleTag(fOwner.fInd,
          JwXMLTagsString[xtSourceProc], JE.SourceProc , Attributes));

    end
    else
    begin

    end;

    fOwner.fElements.Add(Writer.WriteSingleTag(fOwner.fInd,
        JwXMLTagsString[xtMessage], E.Message, Attributes));
    fOwner.fElements.Add(Writer.EndWriteMultipleTags);


    //fOwner.fElements.Add(fOwner.fWriter.WriteSingleTag(fOwner.fInd,
  finally
    Writer.Free;
    fOwner.LeaveCriticalSection;
  end;
end;


{ TJwLogServer }

function TJwLogServerImpl.Connect(const EnterType : TJwEnterType; const ClassName, MethodName, FileName, MessageText : TJwString): IJwLogClient;
begin
  result := TJwLogClientImpl.Create(Self, EnterType, ClassName, MethodName, FileName, MessageText);
end;

constructor TJwLogServerImpl.Create;
var Attributes : TJwXMLAttributes;
    S : String;
begin
  fCritical := SyncObjs.TCriticalSection.Create;
  fElements := Elements;

  fInd := 1;
  if Assigned(WriterClass) then
    fWriterClass := WriterClass
  else
    fWriterClass := TJwLogWriter;

  fWriter := fWriterClass.Create;


  DateTimeToString(S, JwTimeOutputString, Now);

  fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atStart], S);
  fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atThread], fWriterClass.GetThreadName);
  fElements.Add(fWriter.StartWriteMultipleTags(fInd, JwXMLTagsString[xtLogProcess],Attributes));
end;

destructor TJwLogServerImpl.Destroy;
begin
  fElements.Add(fWriter.EndWriteMultipleTags);

  FreeAndNil(fCritical);
  FreeAndNil(fWriter);
  inherited;
end;

procedure TJwLogServerImpl.Disconnect(var Client: IJwLogClient);
begin
  //TJwLogClientImpl(Client).Free;
  Client._Release;
  Client := nil;
end;

procedure TJwLogServerImpl.EnterCriticalSection;
begin
  fCritical.Enter; 
end;

procedure TJwLogServerImpl.LeaveCriticalSection;
begin
  fCritical.Leave;
end;


{ TJwLogWriter }

class procedure TJwLogWriter.AddAttribute(var Attr: TJwXMLAttributes;
  const Name, Value: TJwString);
begin
  SetLength(Attr, Length(Attr)+1);
  Attr[High(Attr)].Name := Name;
  Attr[High(Attr)].Value := Value;
end;

class procedure TJwLogWriter.AddAttributes(var Attr : TJwXMLAttributes; const ClassName, MethodName, FileName : TJwString);
begin
  if Length(ClassName) > 0 then
  begin
    SetLength(Attr, Length(Attr)+1);
    Attr[High(Attr)].Name := JwXMLAttributeString[atClass];
    Attr[High(Attr)].Value := ClassName;
  end;

  if Length(MethodName) > 0 then
  begin
    SetLength(Attr, Length(Attr)+1);
    Attr[High(Attr)].Name := JwXMLAttributeString[atMethod];
    Attr[High(Attr)].Value := MethodName;
  end;

  if Length(FileName) > 0 then
  begin
    SetLength(Attr, Length(Attr)+1);
    Attr[High(Attr)].Name := JwXMLAttributeString[atFile];
    Attr[High(Attr)].Value := FileName;
  end;
end;

function TJwLogWriter.EndWriteMultipleTags(): TJwString;
var i : Integer;
begin
  result := '';
  for i := 1 to fIndLevel-1 do
    result := JwStrIdent + result;
  result := result + '</'+fTagName+'>';

  fMultipleTags := false;
end;

class function TJwLogWriter.FormatString(const Str: TJwString): TJwString;
var i : Integer;
begin
  result := Str;
  for i := Length(Str) downto 0 do
  begin
    if result[i] = #10 then
      System.Delete(result, i,1)
    else
    if result[i] = #13 then
    begin
      result[i] := '\';
      System.Insert('n',result,i+1);
    end;
  end;
end;

class function TJwLogWriter.GetThreadName: TJwString;
begin
  result := IntToStr(GetCurrentThreadId);
end;

function TJwLogWriter.StartWriteMultipleTags(const IndLevel : Integer;
  const TagName: TJwString; const Attributes: TJwXMLAttributes) : TJwString;
begin  
  fIndLevel := IndLevel+1;
  fTagName := TagName;
   fStartMTags := true;
   result := WriteSingleTag(IndLevel, TagName, '', Attributes);
   fStartMTags := false;
  fMultipleTags := true;
end;

function TJwLogWriter.WriteSingleTag(IndLevel : Integer; const TagName, Value: TJwString;
  const Attributes: TJwXMLAttributes): TJwString;
var S, AttributesLine : TJwString;
    i : Integer;

begin
  AttributesLine := '';
  for i := Low(Attributes) to high(Attributes) do
  begin
    //TODO: check for spaces in Name
    //TODO: check for #13#10 in Name and Value
    if (Length(Attributes[i].Name) > 0) and
       (Length(Attributes[i].Value) > 0) then
    AttributesLine := AttributesLine + Attributes[i].Name+'="'+Attributes[i].Value+'" ';
  end;

  if Length(AttributesLine) > 0 then
  begin
    System.Delete(AttributesLine, Length(AttributesLine), 1);
    AttributesLine := ' '+AttributesLine;
  end;

  if fStartMTags then
    result := JwFormatStringEx('<%0:s%1:s>',[TagName, AttributesLine])
  else
  if (Length(Value) > 0) then
  begin
    S := FormatString(Value);
    result := JwFormatStringEx('<%0:s%2:s>%1:s</%0:s>',[TagName, S, AttributesLine])
  end
  else
    result := JwFormatStringEx('<%0:s%1:s/>',[TagName, AttributesLine]);


  if fMultipleTags then
    IndLevel := fIndLevel;
    //Inc(IndLevel);

  for i := 1 to IndLevel do
    result := JwStrIdent + result;

{  if fMultipleTags then
    result := result + JwStrNewLine;  }
end;



end.
