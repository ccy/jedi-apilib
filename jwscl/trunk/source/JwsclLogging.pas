{@abstract(This unit contains a log mechanisms)
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

type {@Name defines log tag attribute types}
     TJwLogType = (
        lsNone,
        //logs a message. Information about ths.
        lsMessage,
        //logs a warning.
        lsWarning,
        //logs an error but not a show stopper
        lsError,
        //logs an showstopper
        lsStop);
     {@Name defines an enter/leave attribute type}
     TJwEnterType = (
        //unknown or unsupported enter/leave
        etNone,
        //enter or leave a function body
        etFunction,
        //enter or leave a method body
        etMethod,
        //enter or leave a thread main func. body
        etThread);

     {@Name defines a signal attribute type}
     TJwSignalType = (
        stNone,
        //log sent signal 
        stSend,
        //log received signal
        stReceived,
        //log wait for signal
        stWait);

     {@Name defines a memory attribute type}   
     TJwMemoryType = (
        mtNone,
        //log memory allocation
        mtAlloc,
        //log memory deallocation
        mtDeAlloc);

    {@Name defines known XML tag names}    
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

    TJwXMLLogTag = (
        ltEnter, ltLeave, ltSignal, ltMemory, ltLog, ltException);

    {@Name defines known xml tag attributes}
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

     {@Name defines an attribute.
      Attributes with empty name or value will be ignored
     }
     TJwXMLAttribute = record
       //@Name defines the name of the attribute
       Name,
       //@Name contains the content of the attribute
       Value : TJwString;
     end;

     TJwEventType = record
       {@Name defines an event that can be logged.}
       TagName   : TJwXMLLogTag;
       {@Name contains an or combined bit mask of enumeration constants
        from TJwLogType, TJwEnterType, TJwMemoryType or TJwSignalType.
        Set to -1 if all event type are to be logged. 
       }
       TypeValues : Integer;
     end;

     TJwEventTypes = array of TJwEventType;

     //@Name defines an dynamic array of attributes
     TJwXMLAttributes = array of TJwXMLAttribute;

     {@Name defines a default and base class for xml write operations
     and other util functions.
     Overwrite this class and specify its class type to CreateLogServer
     for changed behavior.
     }
     TJwLogWriter = class
     protected
       fStartMTags,
       fMultipleTags : Boolean;
       fTagName : TJwString;
       fIndLevel : Integer;
     public
       {@Name writes a single tag.
       @param(IndLevel defines the indentation level.)
       @param(TagName defines the name of the tag)
       @param(Value defines the value of the tag. If empty the tag has no value)
       @param(Attributes defines an array of attributes to be added to the tag)
       @return(Returns the formatted xml tag.)
       }
       function WriteSingleTag(IndLevel : Integer; const TagName, Value : TJwString;
         const Attributes : TJwXMLAttributes) : TJwString; virtual;

       {@Name starts a tag with sub tags. All subsequent calls to WriteSingleTag
        will create tag under this tag.
        Call EndWriteMultipleTags to end creating sub tags.
        Multiple sub tags is not supported directly. Instead the instance
        is created again.

        @param(IndLevel defines the indentation level.)
        @param(TagName defines the name of the tag)
        @param(Attributes defines an array of attributes to be added to the tag)
        @return(Returns the formatted xml tag.)
       }
       function StartWriteMultipleTags(const IndLevel : Integer; const TagName : TJwString;
         const Attributes : TJwXMLAttributes) : TJwString; virtual;

       {@Name ends creating sub tags which was commenced by StartWriteMultipleTags.
        @return(The return value is the last closing tag started by StartWriteMultipleTags)
       }
       function EndWriteMultipleTags : TJwString; virtual;

       procedure Done; virtual;

       {@Name is a helper function that adds an attribute structure TJwXMLAttribute
        to an array TJwXMLAttributes.
        @param(Attr receives an TJwXMLAttributes where the new attribute is added to the end)
        @param(Name defines a name for the attribute. The function does not check for duplicates)
        @param(Value defines the value of the attribute)
        }
       class procedure AddAttribute(var Attr : TJwXMLAttributes; const Name, Value : TJwString); virtual;

       {@Name is a helper function that adds source code location attribute like
         classname, methodname and filename from where the log is made.
        @param(Attr receives an TJwXMLAttributes where the new attribute is added to the end)
        @param(ClassName defines the classname where the method is located. Can be empty
          if the method is not in a class. In this case the attribute is ignored.)
        @param(Methodname defines the name of the method or function)
        @param(Filename defines the filename where the method is located. The filename can contain
          a source line at the end of string succeeded by a colon.)
        }
       class procedure AddAttributes(var Attr : TJwXMLAttributes; const ClassName, MethodName, FileName : TJwString); virtual;

       {@Name is a helper function that is used to format a tag value.
        It replaces line breaks with c style line breaks (\n).}
       class function FormatString(const Str : TJwString) : TJwString; virtual;

       {@Name returns the value of the attribute thread which contains the name (or ID)
        of the thread which logged a message.
        @return(Name and/or ID of thread) 
         }
       class function GetThreadName : TJwString; virtual;

       {@Name checks whether a log type and its type value should be logged to file.
        @param(LogTag defines a log tag which is checked)
        @param(LogTypeValue defines a or bitmask of types (a type like TJwLogType).)
        @param(AllowedTypes defines a list of event types that can be logged)
       }
       class function CheckLogEventType(const LogTag : TJwXMLLogTag; const LogTypeValue : Integer;
            const AllowedTypes : TJwEventTypes) : Boolean; virtual;

       {@Name adds an event type to an events list.
        @param(Event defines a list of events that can be logged)
        @param(LogTag defines an event to be logged)
        @param(TypeValues defines a or bitmask of types (like TJwLogType).)
        }
       class procedure AddEventType(var Events : TJwEventTypes; const LogTag : TJwXMLLogTag; const TypeValues : Integer); virtual;
     end;

     TJwLogWriterClass = class of TJwLogWriter;

     {@Name defines an interface for a log client.
      Use IJwLogServer.Connect to create an instance of this interface.
      Each log function is multi thread safe. Different log calls in different threads
      will wait for each other.
     }
     IJwLogClient = interface (IInterface)
       ['{B7202309-4766-4D62-9E16-ECE5953C2AEA}']
        {@Name creates an ordinary log entry.
         @param(LogType defines the type of log entry. See TJwLogType for more information)
         @param(LogMessage defines the message to be shown)
         }
        procedure Log(const LogType : TJwLogType; const LogMessage : TJwString); overload; safecall;

        {@Name creates an ordinary log entry with log source information.
         @param(LogType defines the type of log entry. See TJwLogType for more information)
         @param(ClassName defines the classname where the method is located. Can be empty
          if the method is not in a class. In this case the attribute is ignored.)
         @param(Methodname defines the name of the method or function)
         @param(Filename defines the filename where the method is located. The filename can contain
          a source line at the end of string succeeded by a colon.)
         @param(LogMessage defines the message to be shown)
         }
        procedure Log(const LogType : TJwLogType; const ClassName, MethodName, FileName, LogMessage : TJwString); overload; safecall;

        {@Name creates a signal log entry. A signal log entries is used for communcation logging.
         Threads or processes may use events to signal special events.
         @param(SignalType defines the type of signal entry. See TJwSignalType for more information)
         @param(Source defines the source of the signal, like another thread or process ID.)
         @param(Target defines the target of the signal, like another thread or process ID.)
         @param(LogMessage defines the message to be shown)
         }
        procedure Signal(const SignalType : TJwSignalType; const Source, Target, LogMessage : TJwString); overload; safecall;

        {@Name creates a signal log entry. A signal log entries is used for communcation logging.
         Threads or processes may use events to signal special events.
         @param(LogType defines the type of log entry. See TJwLogType for more information)
         @param(Source defines the source of the signal, like another thread or process ID.)
         @param(Target defines the target of the signal, like another thread or process ID.)
         @param(ClassName defines the classname where the method is located. Can be empty
          if the method is not in a class. In this case the attribute is ignored.)
         @param(Methodname defines the name of the method or function)
         @param(Filename defines the filename where the method is located. The filename can contain
          a source line at the end of string succeeded by a colon.)
         @param(LogMessage defines the message to be shown)
         }
        procedure Signal(const SignalType : TJwSignalType; const Source, Target, ClassName, MethodName, FileName, LogMessage : TJwString); overload; safecall;

        {@Name creates a memory information log entry.
         Application can log allocations and deallocations of memory for leak search. 

         @param(MemoryType defines the type of memory allocation. See TJwMemoryType for more information)
         @param(MemType defines which type of (de-)allocation mechanism is used (like GetMem))
         @param(LogMessage defines the message to be shown.)
         }
        procedure Memory(const MemoryType : TJwMemoryType; const MemType, LogMessage : TJwString); overload; safecall;

        {@Name creates a memory information log entry with log source information.
         Application can log allocations and deallocations of memory for leak search. 

         @param(MemoryType defines the type of memory allocation. See TJwMemoryType for more information)
         @param(MemType defines which type of (de-)allocation mechanism is used (like GetMem))
         @param(ClassName defines the classname where the method is located. Can be empty
          if the method is not in a class. In this case the attribute is ignored.)
         @param(Methodname defines the name of the method or function)
         @param(Filename defines the filename where the method is located. The filename can contain
          a source line at the end of string succeeded by a colon.)
         @param(LogMessage defines the message to be shown.)
         }
        procedure Memory(const MemoryType : TJwMemoryType; const MemType, ClassName, MethodName, FileName, LogMessage : TJwString); overload; safecall;

        {@Name creates an exception information log entry.

         @param(E contains an exception object which content is logged.
            The object can be of class EJwsclSecurityException. In this case more information is available in the log.)
         @param(LogMessage defines the message to be shown.)
         }
        procedure Exception(const E : Exception); overload; safecall;

        {@Name creates an exception information log entry with log source information.

         @param(E contains an exception object which content is logged.
            The object can be of class EJwsclSecurityException. In this case more information is available in the log.)
         @param(ClassName defines the classname where the method is located. Can be empty
          if the method is not in a class. In this case the attribute is ignored.)
         @param(Methodname defines the name of the method or function)
         @param(Filename defines the filename where the method is located. The filename can contain
          a source line at the end of string succeeded by a colon.)
         @param(LogMessage defines the message to be shown.)
         }
        procedure Exception(const E : Exception; const ClassName, MethodName, FileName : TJwString); overload; safecall;

        {@Name sets the type of events which are logged of this log client}
        procedure SetEventTypes(const EventTypes : TJwEventTypes); safecall;
     end;

     IJwLogServer = interface (IInterface)
       ['{1B3EC217-2F6D-4FE2-A9DC-BF7E8C025D4F}']
       {@Name creates a new log client. A log client (IJwLogClient) provides access to logging mechanisms.
        Use this function at the beginning of a process, thread or function start. Obtain an instance
        of IJwLogClient and start to log information.
        @param(EnterType define of which kind this log client consists. A log client automatically
         creates an enter tag using this parameter. Specify etNone to do not create an enter log.
         If the instance is destroyed the log client will automatically create
         a leave tag.)
        @param(ClassName defines the classname where the method is located. Can be empty
          if the method is not in a class. In this case the attribute is ignored.)
        @param(Methodname defines the name of the method or function)
        @param(Filename defines the filename where the method is located. The filename can contain
          a source line at the end of string succeeded by a colon.)
        @param(MessageText defines the message to be used for the enter log entry.)
       }
       function Connect(const EnterType : TJwEnterType; const ClassName, MethodName, FileName, MessageText : TJwString) : IJwLogClient; safecall;

       {@Name nils the log instance and frees it if reference count is zero.}
       procedure Disconnect(var Client : IJwLogClient); safecall;

       {@Name stops the logging server and shut it down.}
       procedure Done; SafeCall;
     end;

{@Name creates a new log server that can hold several log clients (IJwLogClient).
A log sever creates a new logprocess tag and closes it if it is destroyed.
It saves each xml tag into a new string list item in parameter Elements. This
behavior can be overwritten by using a non default WriterClass (TJwLogWriterClass)
@param(Elements receives the xml tags. Each item contains a whole line of an xml tag.
  Must not be nil if parameter WriterClass is left nil.
  )
@param(LogTypes receives a list of TJwEventType records that contains tags and its
attributes which ought to be logged.
If the array is empty all types of events are logged. 
)
@param(WriterClass defines a custom class that can be used to change the
default mechanism how xml is stored. By default (if nil) the TJwLogWriterClass
uses a string list implementation to store xml.)
@return(Returns an instance of IJwLogServer for logging information)

}
function CreateLogServer(Elements : TStringList; const LogTypes : TJwEventTypes; const WriterClass : TJwLogWriterClass = nil) : IJwLogServer;


var //JwStrNewLine : String = #13#10;
    //identation string (default #9 = tabulator char)
    JwStrIdent : String = #9;

    //format of start attribute value for processlog tag
    JwTimeOutputString : String = 'dd.mm.yyyy hh:nn:ss:zzz';

    //strings for log tag value
    JwLogTypeStrings : array[TJwLogType] of TJwString = (
         '',
         'message',
         'warning',
         'error',
         'stop');

    //strings for enter/leave tag type value
    JwEnterTypeString : array[TJwEnterType] of TJwString = (
         '',
         'function',
         'method',
         'thread');

    //strings for signal tag type value
    JwSignalTypeString : array[TJwSignalType] of TJwString = (
         '',
         'send',
         'received',
         'wait');

    //strings for memory type value
    JwMemoryTypeString : array[TJwMemoryType] of TJwString = (
         '',
         'alloc',
         'dealloc');

    //strings for xml tags
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

    //strings type attribute names
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

     TThreadMapRec = record
       //thread id
       ThreadID : DWORD;
       //identation 
       Ident : Integer;
     end;
     TThreadMapRecs = array of TThreadMapRec;



     TJwLogClientImpl = class(TInterfacedObject, IJwLogClient)
     protected
       fOwner : TJwLogServerImpl;
       fInd : Integer;
       fClassName,
       fMethodName, fFileName, fMessageText : TJwString;
       fEnterType : TJwEnterType;
       fWriter : TJwLogWriter;

       //ID connects enter and leave tag unambiguously
       fID : Int64;
       fEventTypes : TJwEventTypes;

     public
       constructor Create(Owner : TJwLogServerImpl;
          const EnterType : TJwEnterType;
          const EventTypes : TJwEventTypes;
          const ClassName, MethodName, FileName, MessageText : TJwString);
       destructor Destroy; override;

       procedure Log(const LogType : TJwLogType; const LogMessage : TJwString); overload; safecall;
       procedure Log(const LogType : TJwLogType; const ClassName, MethodName, FileName, LogMessage : TJwString); overload; safecall;

       procedure Signal(const SignalType : TJwSignalType; const Source, Target, LogMessage : TJwString); overload; safecall;
       procedure Signal(const SignalType : TJwSignalType; const Source, Target, ClassName, MethodName, FileName, LogMessage : TJwString); overload; safecall;

       procedure Memory(const MemoryType : TJwMemoryType; const MemType, LogMessage : TJwString); overload; safecall;
       procedure Memory(const MemoryType : TJwMemoryType; const MemType, ClassName, MethodName, FileName, LogMessage : TJwString); overload; safecall;

       procedure Exception(const E : Exception); overload; safecall;
       procedure Exception(const E : Exception; const ClassName, MethodName, FileName: TJwString); overload; safecall;

       procedure SetEventTypes(const EventTypes : TJwEventTypes); safecall;

       function AddToList(const S : TJwString) : Integer;
     end;

     TJwLogServerImpl = class(TInterfacedObject, IJwLogServer)
     protected
       fElements : TStringList;
       fWriterClass : TJwLogWriterClass;
       fWriter : TJwLogWriter;
       fIndents : TThreadMapRecs;

       fEventTypes : TJwEventTypes;

       fCritical : SyncObjs.TCriticalSection;
       //ID connects enter and leave tag unambiguously 
       fID : Int64;

       fIdx : Integer;

       procedure EnterCriticalSection;
       procedure LeaveCriticalSection;

       //gets identation for a specific thread
       function GetIdentByThread : Integer;
       //sets or adds identation for a specific thread
       procedure SetIdent(Ident : Integer);

       //@Name creates a new unambiguous ID threadsafe.
       function GetID : Int64;
     public
       constructor Create(const Elements : TStringList; const EventTypes : TJwEventTypes;  const WriterClass : TJwLogWriterClass);
       destructor Destroy; override;

       function Connect(const EnterType : TJwEnterType; const ClassName, MethodName, FileName, MessageText : TJwString) : IJwLogClient;safecall;
       procedure Disconnect(var Client : IJwLogClient); safecall;

       procedure Done; SafeCall;
     end;

function CreateLogServer(Elements : TStringList; const LogTypes : TJwEventTypes; const WriterClass : TJwLogWriterClass = nil) : IJwLogServer;
begin
  result := TJwLogServerImpl.Create(Elements,LogTypes, WriterClass);
end;

{ TJwLogClientImpl }

constructor TJwLogClientImpl.Create(Owner : TJwLogServerImpl;
  const EnterType : TJwEnterType;
  const EventTypes : TJwEventTypes;
  const ClassName, MethodName, FileName, MessageText : TJwString);
var Attributes : TJwXMLAttributes;
begin
  fOwner := Owner;
  fClassName := ClassName;
  fMethodName := MethodName;
  fFileName := FileName;
  fMessageText := MessageText;
  fEnterType := EnterType;

  fEventTypes := EventTypes;

  fID := fOwner.GetID; //thread safe

  fWriter := fOwner.fWriterClass.Create;

  fOwner.EnterCriticalSection;
  try
    fInd := fOwner.GetIdentByThread; //deep

    if (EnterType <> etNone) and
      fOwner.fWriterClass.CheckLogEventType(ltEnter, Integer(0), fEventTypes) then
    begin
      fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atType], JwEnterTypeString[EnterType]);
      fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atSource], IntToStr(fID));
      fOwner.fWriterClass.AddAttributes(Attributes, fClassName, fMethodName, fFileName);
      fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atThread], fOwner.fWriterClass.GetThreadName);

      AddToList(fWriter.WriteSingleTag(fInd, JwXMLTagsString[xtEnter], MessageText, Attributes));

      Inc(fInd); //deeper identation for sub tags
      fOwner.SetIdent(fInd); //set parent identation for this thread
    end;

  finally
     fOwner.LeaveCriticalSection
  end;
end;

destructor TJwLogClientImpl.Destroy;
var Attributes : TJwXMLAttributes;
begin
  if (fEnterType <> etNone) and
    fOwner.fWriterClass.CheckLogEventType(ltLeave, Integer(0), fEventTypes) then
  begin
    fOwner.EnterCriticalSection;
    try
      //get one step back of identation
      fOwner.SetIdent(fInd-1);

      fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atType], JwEnterTypeString[fEnterType]);
      fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atSource], IntToStr(fID));
      fOwner.fWriterClass.AddAttributes(Attributes, fClassName, fMethodName, fFileName);
      fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atThread], fOwner.fWriterClass.GetThreadName);
      AddToList(fWriter.WriteSingleTag(fInd-1, JwXMLTagsString[xtLeave], fMessageText, Attributes));
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
  if not fOwner.fWriterClass.CheckLogEventType(ltLog, Integer(LogType), fEventTypes) then
    exit;

  fOwner.EnterCriticalSection;
  try
    fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atType],JwLogTypeStrings[LogType]);
    fOwner.fWriterClass.AddAttributes(Attributes, ClassName, MethodName, FileName);
    fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atThread], fOwner.fWriterClass.GetThreadName);
    AddToList(fWriter.WriteSingleTag(fInd, JwXMLTagsString[xtLog], LogMessage, Attributes));
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
  if not fOwner.fWriterClass.CheckLogEventType(ltMemory, Integer(MemoryType), fEventTypes) then
    exit;

  fOwner.EnterCriticalSection;
  try
    fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atType], JwMemoryTypeString[MemoryType]);
    fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atMemType], MemType);
    fOwner.fWriterClass.AddAttributes(Attributes, ClassName, MethodName, FileName);
    fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atThread], fOwner.fWriterClass.GetThreadName);
    
    AddToList(fWriter.WriteSingleTag(fInd, JwXMLTagsString[xtMemory], LogMessage, Attributes));
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
  if not fOwner.fWriterClass.CheckLogEventType(ltSignal, Integer(SignalType), fEventTypes) then
    exit;

  fOwner.EnterCriticalSection;
  try
    fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atType], JwSignalTypeString[SignalType]);
    fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atSource], Source);
    fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atTarget], Target);
    fOwner.fWriterClass.AddAttributes(Attributes, ClassName, MethodName, FileName);
    fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atThread], fOwner.fWriterClass.GetThreadName);
    AddToList(fWriter.WriteSingleTag(fInd, JwXMLTagsString[xtSignal], LogMessage, Attributes));
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
  if not fOwner.fWriterClass.CheckLogEventType(ltException, Integer(0), fEventTypes) then
    exit;

  fOwner.EnterCriticalSection;
  try
    Writer := fOwner.fWriterClass.Create;

    fOwner.fWriterClass.AddAttributes(Attributes, ClassName, MethodName, FileName);
    fOwner.fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atThread], fOwner.fWriterClass.GetThreadName);

    AddToList(Writer.StartWriteMultipleTags(fInd, JwXMLTagsString[xtException], Attributes));

    AddToList(Writer.WriteSingleTag(fInd,
        JwXMLTagsString[xtType], E.ClassName, Attributes));

    AddToList(Writer.WriteSingleTag(fInd,
        JwXMLTagsString[xtGuid], GUIDToString(JwMapException(E.ClassName)) , Attributes));
        
    if E is EJwsclSecurityException then
    begin
      JE := E as EJwsclSecurityException;

      AddToList(Writer.WriteSingleTag(fInd,
        JwXMLTagsString[xtGetLastError], IntToStr(JE.LastError), Attributes));

      AddToList(Writer.WriteSingleTag(fInd,
        JwXMLTagsString[xtLastErrorString], JE.GetLastErrorMessage(JE.LastError), Attributes));

      if Length(Trim(JE.WinCallName)) > 0 then
        AddToList(Writer.WriteSingleTag(fInd,
          JwXMLTagsString[xtWinApiFunction], JE.WinCallName, Attributes));

      if Length(Trim(JE.Log)) > 0 then
        AddToList(Writer.WriteSingleTag(fInd,
          JwXMLTagsString[xtLogString], JE.Log , Attributes));

      if Length(Trim(JE.ComSource)) > 0 then
        AddToList(Writer.WriteSingleTag(fInd,
          JwXMLTagsString[xtComSource], JE.ComSource, Attributes));

      if Length(Trim(JE.StackTrace)) > 0 then
        AddToList(Writer.WriteSingleTag(fInd,
          JwXMLTagsString[xtStackTrace], JE.StackTrace , Attributes));

      if Length(Trim(JE.SourceProc)) > 0 then
        AddToList(Writer.WriteSingleTag(fInd,
          JwXMLTagsString[xtSourceProc], JE.SourceProc , Attributes));

    end
    else
    begin

    end;

    AddToList(Writer.WriteSingleTag(fInd,
        JwXMLTagsString[xtMessage], E.Message, Attributes));
    AddToList(Writer.EndWriteMultipleTags);


    //AddToList(fOwner.fWriter.WriteSingleTag(fOwner.fInd,
  finally
    Writer.Free;
    fOwner.LeaveCriticalSection;
  end;
end;


procedure TJwLogClientImpl.SetEventTypes(const EventTypes: TJwEventTypes);
begin
  fEventTypes := EventTypes;
end;

function TJwLogClientImpl.AddToList(const S: TJwString): Integer;
begin
  result := 0;
  if Assigned(fOwner.fElements) then
    result := fOwner.fElements.Add(S);
end;

{ TJwLogServer }

function TJwLogServerImpl.Connect(const EnterType : TJwEnterType; const ClassName, MethodName, FileName, MessageText : TJwString): IJwLogClient;
begin
  result := TJwLogClientImpl.Create(Self, EnterType, fEventTypes, ClassName, MethodName, FileName, MessageText);
end;



constructor TJwLogServerImpl.Create;
var Attributes : TJwXMLAttributes;
    S : String;
begin
  fCritical := SyncObjs.TCriticalSection.Create;
  fElements := Elements;

  fID := 0;
  SetIdent(2); //logprocess' sub tags ident 2 tabs

  fEventTypes := EventTypes;

  if Assigned(WriterClass) then
    fWriterClass := WriterClass
  else
    fWriterClass := TJwLogWriter;

  fWriter := fWriterClass.Create;
    

  DateTimeToString(S, JwTimeOutputString, Now);

  fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atStart], S);
  fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atThread], fWriterClass.GetThreadName);
  fWriterClass.AddAttribute(Attributes, JwXMLAttributeString[atEnd], '%s'); //save space for end date and time

  S := fWriter.StartWriteMultipleTags(1, JwXMLTagsString[xtLogProcess],Attributes);
  if Assigned(fElements) then
    fIdx := fElements.Add(S);
end;

destructor TJwLogServerImpl.Destroy;
begin
  Done;
  inherited;
end;

procedure TJwLogServerImpl.Done;
var Attributes : TJwXMLAttributes;
    S : String;
    Value : TJwString;
begin
  if not Assigned(fCritical) then
    exit;

  DateTimeToString(S, JwTimeOutputString, Now);

  if (fIdx >= 0) and Assigned(fElements) then
  try
    //write end date and time into formatted string from .Create
    Value := JwFormatStringEx(fElements[fIdx], [S]);
    fElements[fIdx] := Value;
  except
  end;

  S := fWriter.EndWriteMultipleTags;

  if Assigned(fElements) then
    fElements.Add(S);

  fWriter.Done;

  FreeAndNil(fCritical);
  FreeAndNil(fWriter);
end;

procedure TJwLogServerImpl.Disconnect(var Client: IJwLogClient);
begin
  //TJwLogClientImpl(Client).Free;
  //Client._Release;
  Client := nil;
end;

procedure TJwLogServerImpl.EnterCriticalSection;
begin
  Assert(Assigned(fCritical), 'LogServer is no more active.');
  if Assigned(fCritical) then
    fCritical.Enter; 
end;

function TJwLogServerImpl.GetIdentByThread: Integer;
var i : Integer;
    ID : DWORD;
begin
  result := 0;
  ID := GetCurrentThreadId;
  for i := low(fIndents) to high(fIndents) do
   if fIndents[i].ThreadID = ID then
     result := fIndents[i].Ident;
end;

procedure TJwLogServerImpl.SetIdent(Ident: Integer);
var i : Integer;
    ID : DWORD;
begin
  ID := GetCurrentThreadId;
  for i := low(fIndents) to high(fIndents) do
    if fIndents[i].ThreadID = ID then
    begin
      fIndents[i].Ident := Ident;
      exit;
    end;

  SetLength(fIndents, Length(fIndents)+1);
  fIndents[high(fIndents)].ThreadID := ID;
  fIndents[high(fIndents)].Ident := Ident;
end;

function TJwLogServerImpl.GetID : Int64;
begin
  InterlockedIncrement64(fID);
  result := fID;
end;


procedure TJwLogServerImpl.LeaveCriticalSection;
begin
  Assert(Assigned(fCritical), 'LogServer is no more active.');
  if Assigned(fCritical) then
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

class procedure TJwLogWriter.AddEventType(var Events: TJwEventTypes;
  const LogTag: TJwXMLLogTag; const TypeValues: Integer);
begin
  SetLength(Events, Length(Events)+1);
  Events[high(Events)].TagName := LogTag;
  Events[high(Events)].TypeValues := TypeValues;
end;

class function TJwLogWriter.CheckLogEventType(const LogTag: TJwXMLLogTag;
  const LogTypeValue: Integer; const AllowedTypes: TJwEventTypes): Boolean;

var i,i2 : Integer;
begin
  if Length(AllowedTypes) = 0 then
  begin
    result := true;
    exit;
  end;
  
  result := false;
  for i := low(AllowedTypes) to high(AllowedTypes) do
  begin
    if AllowedTypes[i].TagName = LogTag then
    begin
      result := (AllowedTypes[i].TypeValues and LogTypeValue) = LogTypeValue;
      exit;
    end;
  end;
end;

procedure TJwLogWriter.Done;
begin

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
  result := JwGetThreadName + ' ('+IntToStr(GetCurrentThreadId)+')';
  if Length(Trim(JwGetThreadName)) = 0 then
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
