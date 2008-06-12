unit CredentialsForm;

interface

uses
  JwaWindows,
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, Registry, ComCtrls, AppEvnts,
  JvExControls, JvLookOut, JvExExtCtrls, JvBevel,
  JvButton, JvTransparentButton, JvComponent, JvExStdCtrls, JvHtControls,
  JvLinkLabel, JvExtComponent, JvLinkLabelTools, SessionPipe, JwsclUtils,
  JvPanel, jpeg, JvLabel, JvComponentBase, JvComputerInfoEx, JvImage, JvGradient,
  JvGradientHeaderPanel, JvEdit, JvWaitingGradient, JvWaitingProgress,
  JvStaticText, JvExComCtrls, JvProgressBar, JvThread,
  JwsclDesktops,
  JwsclExceptions,
  JwsclSecureObjects,
  JwsclTypes,
  JwsclDescriptor,
  JwsclToken,
  JwsclComUtils,
  JwsclAcl,
  JwsclPrivileges,
  XPMan,
  UserProfileImage,
  XPElevationCommon,
  JwsclStrings;
                                     

const
   {This event is send if a desktop switch event occured.
    On the first desktop switch lParam is 1, on the second switch
    it is 2.
   }
   WM_SWITCH_DESKTOP         = WM_USER + 1999;
   WM_GET_USER_PROFILE_IMAGE = WM_USER + 2000;

var
   SecureDesktopName : WideString = '';

type
  {TJwDesktopSwitchThread creates a thread that waits
  for the winlogon desktop switch event.
  It then sends the message WM_SWITCH_DESKTOPto the credential forms.

  we just assume it is the winlogon Lockstation call
  since this call returns to the last desktop that was in action
  if we just exit after the first switch
  the call will switchback to an empty desktop which is a dead end.
  }
  TJwDesktopSwitchThread = class(TJwThread)
  public
    procedure Execute; override;
    procedure Terminate; reintroduce;

    destructor Destroy; override;
  end;

  {TFormCredentials implements the credentials form
  }
  TFormCredentials = class(TForm)
    BitBtnCancel: TBitBtn;
    BitBtn_Ok: TBitBtn;
    Image1: TImage;
    Label_DefaultUser: TLabel;
    Image2: TImage;
    UsersComboBox: TComboBox;
    CheckBoxSaveLogon: TCheckBox;
    ButtonDefaultUser: TJvTransparentButton;
    JvBevel1: TJvBevel;
    JvBevel2: TJvBevel;
    ButtonUser: TJvTransparentButton;
    JvPanel1: TJvPanel;
    JvLinkLabel1: TJvLinkLabel;
    Image3: TImage;
    JvLinkLabel2: TJvLinkLabel;
    JvComputerInfoEx1: TJvComputerInfoEx;
    JvImage1: TJvImage;
    JvGradient_SignedApp: TJvGradient;
    Image_Windows: TImage;
    JvGradient_UnsignedApp: TJvGradient;
    JvLabel_UnsignedApp: TJvLabel;
    JvLabel_SignedApp: TJvLabel;
    Image_Application: TImage;
    JvStaticText_AppPublisher: TJvStaticText;
    JvEdit_CmdLine: TJvEdit;
    JvProgressBar: TJvProgressBar;
    Timer_ElevationTimeOut: TTimer;
    JvEdit_AppName: TJvEdit;
    EditPassword1: TJvEdit;
    EditPassword2: TJvEdit;
    Button_EndService: TButton;
    Image_LogonError: TImage;
    JvStaticText_LogonError: TJvStaticText;
    XPManifest1: TXPManifest;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure EditPassword1Change(Sender: TObject);
    procedure EditPassword2Change(Sender: TObject);
    procedure ButtonDefaultUserClick(Sender: TObject);
    procedure ButtonUserClick(Sender: TObject);
    procedure UsersComboBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxSaveLogonClick(Sender: TObject);
    procedure BitBtn_OkClick(Sender: TObject);
    procedure BitBtnCancelClick(Sender: TObject);
    procedure JvBevel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure JvLinkLabel1LinkClick(Sender: TObject; LinkNumber: Integer;
      LinkText, LinkParam: String);
    procedure Timer_ElevationTimeOutTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EditPassword1Exit(Sender: TObject);
    procedure EditPassword1Enter(Sender: TObject);
    procedure UsersComboBoxMouseEnter(Sender: TObject);
    procedure Button_EndServiceClick(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure Image_ApplicationDblClick(Sender: TObject);
    procedure UsersComboBoxKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditPassword1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
    fSaveLogon : Boolean;
    fAppName,
    fAppCmdLine,
    fUserName,
    fPassword : TJwString;
    internalMsg : Boolean;
    fTimeout,
    fFlags : Cardinal;
    fPipeSession : TClientSessionPipe;
    fShowWebPage : Boolean;
    fDesktopSwitchThread : TJwDesktopSwitchThread;
    fDesktopSwitchNotification : Boolean;
    fControlFlags : DWORD;
    fUserRegKey : HKEY;
    fUserRegistry : TRegistry;
    fUserImage : TMemoryStream;
    fUserImageType : WideString;
    fSecureDesktop : Boolean;
    fDesktop : TJwSecurityDesktop;

    function IsPasswordCacheAvailable : Boolean;

    procedure ShowLogonError(const Visible : Boolean);
    procedure SetUserKey(key : HKEY);
  public
    destructor Destroy; override;

    procedure SWITCH_DESKTOP(var Message: TMsg); message WM_SWITCH_DESKTOP;


    procedure CenterInMonitor(const i : Integer);
    { Public-Deklarationen }
    property AppName : TJwString read fAppName write fAppName;
    property AppCmdLine : TJwString read fAppCmdLine write fAppCmdLine;


    property SaveLogon : Boolean read fSaveLogon write fSaveLogon;
    property UserName : TJwString read fUserName write fUserName;
    property Password : TJwString read fPassword write fPassword;
    property Flags : Cardinal read fFlags write fFlags;
    property TimeOut : Cardinal read fTimeout write fTimeout;

    property PipeSession : TClientSessionPipe read fPipeSession write fPipeSession;

    property LogonError : Boolean write ShowLogonError;

    property ShowWebPage : Boolean read fShowWebPage;
    property ControlFlags : DWORD read fControlFlags write fControlFlags;
    property UserRegKey : HKEY write SetUserKey;

    property UserImage : TMemoryStream read fUserImage write fUserImage;
    property UserImageType : WideString read fUserImageType write fUserImageType;

    property SecureDesktop : Boolean read fSecureDesktop write fSecureDesktop;

    property Desktop : TJwSecurityDesktop write fDesktop;
  end;

var
  FormCredentials: TFormCredentials;

//creates random data with at least 40 chars
function GetFillPasswd : String;

implementation
uses Math;

{$R *.dfm}




procedure TJwDesktopSwitchThread.Terminate;
begin
  SetEvent(FTerminatedEvent);
  inheriteD;
end;

destructor TJwDesktopSwitchThread.Destroy;
begin
  inherited;
end;

procedure TJwDesktopSwitchThread.Execute;
var
  DE : THandle;
  WR : DWORD;
  Counter : Integer;
begin
  inherited;

  FreeOnTerminate := true;
  FTerminatedEvent := CreateEvent(nil, true, false, nil);


  Name := 'DesktopSwitchThread';

  DE := OpenEvent(JwaWindows.SYNCHRONIZE, false, 'WinSta0_DesktopSwitch');

  //wait for switch to another desktop
  //we just assume it is the winlogon Lockstation call
  //since this call returns to the last desktop that was in action
  //if we just exit after the first switch
  //the call will switchback to an empty desktop which is a dead end.

  Counter := 1;
  repeat
    WR := JwWaitForMultipleObjects([DE, FTerminatedEvent], false, INFINITE);
    if WR = WAIT_OBJECT_0 then
    begin
      SendMessage(FormCredentials.Handle, WM_SWITCH_DESKTOP, 0, Counter);
      Inc(Counter);
    end;
  until WR = WAIT_OBJECT_0 +1;
end;


procedure TFormCredentials.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  EditPassword1.Text := GetFillPasswd;
  EditPassword1.Text := '';
  EditPassword2.Text := GetFillPasswd;
  EditPassword2.Text := '';

  Application.Terminate;
end;





procedure TFormCredentials.Image_ApplicationDblClick(Sender: TObject);
begin
  LockWorkStation;
end;

function TFormCredentials.IsPasswordCacheAvailable: Boolean;
begin
  result := Flags and SERVER_CACHEAVAILABLE = SERVER_CACHEAVAILABLE;
end;

procedure TFormCredentials.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var bButtons,
    bDefaultUserPassword,
    bUser : Boolean;
begin
  CanClose := true;

  if ModalResult = mrOk then
  begin


    try
      bButtons := (ButtonDefaultUser.Down xor ButtonUser.Down);
      bDefaultUserPassword :=
        ButtonDefaultUser.Down and ((Length(Trim(EditPassword1.Text)) > 0) or
                IsPasswordCacheAvailable);
      bUser := ButtonUser.Down and (Length(Trim(UsersComboBox.Text)) > 0) and
              ((Length(Trim(EditPassword2.Text)) > 0) or IsPasswordCacheAvailable);

      CanClose := bButtons and (bDefaultUserPassword or bUser);
    finally
      //if not CanClose then
      //Do info here
    end;

    if CanClose then
    begin
      fSaveLogon := CheckBoxSaveLogon.Checked;
      fUserName := '';
      if ButtonUser.Down then
      begin
        fUserName  := UsersComboBox.Text;
        fPassword  := EditPassword2.Text;
        if (Length(EditPassword2.Text) = 0) and
          (IsPasswordCacheAvailable) then //use cached password
           Flags := Flags or CLIENT_USECACHECREDS;
      end
      else
      begin
        fPassword  := EditPassword1.Text;
        if (Length(EditPassword2.Text) = 0) and
          (IsPasswordCacheAvailable) then //use cached password
          Flags := Flags or CLIENT_USECACHECREDS;
      end;

      if SaveLogon then
        Flags := Flags or CLIENT_CACHECREDS; //save pass to cache if correct

      EditPassword2.Text := GetFillPasswd;
      EditPassword2.Text := '';
      EditPassword1.Text := GetFillPasswd;
      EditPassword1.Text := '';
    end;
  end;
end;

procedure TFormCredentials.EditPassword1Change(Sender: TObject);
begin
  if internalMsg then
    exit;
  internalMsg := true;

  if Sender = EditPassword2 then
  begin
    try
      ButtonUser.Click
    except
    end;
  end
  else
  if Sender = EditPassword1 then
    ButtonDefaultUser.Click;

  if (Length(TJvEdit(Sender).Text) > 0) then
  begin
    TJvEdit(Sender).PasswordChar := #7;
    //EditPassword1.ThemedPassword := true;
    TJvEdit(Sender).ProtectPassword := true;
  end;

  internalMsg := false;

end;

procedure TFormCredentials.EditPassword1Enter(Sender: TObject);
begin
  if (Length(TJvEdit(Sender).Text) = 0) then
  begin
    TJvEdit(Sender).EmptyValue := '';
    TJvEdit(Sender).Text := '';

    TJvEdit(Sender).PasswordChar := #7;
    //EditPassword1.ThemedPassword := false;
    TJvEdit(Sender).ProtectPassword := true;
  end;
end;

procedure TFormCredentials.EditPassword1Exit(Sender: TObject);
begin
  if (Length(TJvEdit(Sender).Text) = 0) and
     IsPasswordCacheAvailable then
  begin
    TJvEdit(Sender).PasswordChar := #0;
    TJvEdit(Sender).EmptyValue := 'Password is chached';
    //EditPassword1.ThemedPassword := false;
    TJvEdit(Sender).ProtectPassword := false;
    TJvEdit(Sender).Text := '';
  end;
end;

procedure TFormCredentials.EditPassword1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DOWN then
    try
      if ButtonUser.Enabled and ButtonUser.Visible then
        ButtonUser.Click
    except
    end;

end;

procedure TFormCredentials.EditPassword2Change(Sender: TObject);
begin
  if internalMsg then
    exit;
  internalMsg := true;
  begin
    try
      ButtonUser.Click
    except
    end;
  end;
  internalMsg := false;
end;

procedure TFormCredentials.Button_EndServiceClick(Sender: TObject);
begin
  ModalResult := mrAbort;
  Close;
end;

procedure TFormCredentials.ButtonDefaultUserClick(Sender: TObject);
begin
  if internalMsg then
    exit;
  internalMsg := true;
  begin
    if EditPassword1.Tag = 1 then //password cached
      BitBtn_Ok.SetFocus
    else
      try
        EditPassword1.SetFocus;
      except
      end;
  end;
  internalMsg := false;
end;

procedure TFormCredentials.ButtonUserClick(Sender: TObject);
begin
  if internalMsg then
    exit;

  internalMsg := true;
  begin
    if Length(Trim(UsersComboBox.Text)) = 0 then
    begin
      try
        UsersComboBox.SetFocus
      except
      end;
    end
    else
      if EditPassword2.Tag = 1 then //password cached
        BitBtn_Ok.SetFocus
      else
        EditPassword2.SetFocus;

  end;
  internalMsg := false;
end;

procedure TFormCredentials.UsersComboBoxChange(Sender: TObject);
begin
  if internalMsg then
    exit;
  internalMsg := true;
  begin
    try
      ButtonUser.Click
    except
    end;
  end;
  internalMsg := false;
end;

procedure TFormCredentials.UsersComboBoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_UP then
    if ButtonDefaultUser.Enabled and ButtonDefaultUser.Visible then
    try
      ButtonDefaultUser.Click;
    except

    end;
end;



procedure TFormCredentials.FormCreate(Sender: TObject);
begin
  fShowWebPage := false;
end;

procedure TFormCredentials.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fUserRegistry);
  RegCloseKey(fUserRegKey);
end;

procedure TFormCredentials.FormShow(Sender: TObject);

function IsSigned(const App : String; out Publisher : String) : Boolean;
begin
  result := true;
  Publisher := 'Cert Publisher';
end;

function GetBinaryPublisher : String;
begin
  result := 'Binary Publisher';
end;

const
  UserKeyPath = 'Software\JEDI-API\XPElevation';

function IsAlternativeLogon : Boolean;
begin
  result := true;
  if Assigned(fUserRegistry) and fUserRegistry.OpenKey(UserKeyPath, true) then
  begin
    try
      result := fUserRegistry.ReadBool('LastAlternativeLogon');
    except
    end;
    fUserRegistry.CloseKey;
  end;
end;

function GetAlternativeLogonUserName : WideString;
begin
  result := '';
  if Assigned(fUserRegistry) and fUserRegistry.OpenKey(UserKeyPath, true) then
  begin
    try
      result := fUserRegistry.ReadString('LastAlternativeLogonName');
    except
    end;
    fUserRegistry.CloseKey;
  end;
end;

procedure FillUserList;
begin

end;

var
  Publisher : String;
  Signed : Boolean;

begin

  //turn off saving credentials?
  CheckBoxSaveLogon.Enabled := ControlFlags and XPCTRL_FORCE_NO_CACHE_CREDENTIALS <> XPCTRL_FORCE_NO_CACHE_CREDENTIALS;
  if not CheckBoxSaveLogon.Enabled and CheckBoxSaveLogon.Checked then
    CheckBoxSaveLogon.Checked := False;

  UsersComboBox.Clear;
  //get last logon name if available from registry
  UsersComboBox.Text := GetAlternativeLogonUserName;
  //if not alternate name available...
  if (Length(UsersComboBox.Text) = 0) then
  begin
    //set the name from service
    UsersComboBox.Text := UserName
  end
  else //otherwise set name and add it 
  begin
    UsersComboBox.Items.Add(UsersComboBox.Text);
    UsersComboBox.Text := UsersComboBox.Text;
  end;

  //add service from username
  UsersComboBox.Items.Add(UserName);
  FillUserList;

  try
    //turn off alternate logon?
    if ControlFlags and XPCTRL_FORCE_NO_ALTERNATE_LOGON = XPCTRL_FORCE_NO_ALTERNATE_LOGON then
    begin
      ButtonUser.Enabled := false;
      UsersComboBox.Enabled := false;
      EditPassword2.Enabled := false;
    end;
  except
  end;

  JvEdit_AppName.Text := AppName;
  JvEdit_CmdLine.Text := '"'+AppName + '" ' +AppCmdLine;
  Label_DefaultUser.Caption := 'Elevate me as Administrator';

  if TimeOut = INFINITE then
  begin
    JvProgressBar.Max := 0;
    Timer_ElevationTimeOut.Enabled := false;
  end
  else
  begin
    Timer_ElevationTimeOut.Enabled := true;
    JvProgressBar.Max := (TimeOut div 1000);//Max(TimeOut div 1000, 60);
  {  if JvProgressBar.Max < 10 then
      JvProgressBar.Max := 60;        }
  end;


  if not IsPasswordCacheAvailable or
    //secondary logon mechanism (name + pass) is disabled
    (not EditPassword2.Enabled) then
  begin
    EditPassword1.Tag := 0;
    EditPassword1.Text := '';
    EditPassword1.EmptyValue := '';
    //EditPassword1.ThemedPassword := true;
    EditPassword1.ProtectPassword := true;

    EditPassword2.Tag := 0;
    EditPassword2.Text := '';
    EditPassword2.EmptyValue := '';
    //EditPassword1.ThemedPassword := true;
    EditPassword2.ProtectPassword := true;

    try
      EditPassword1.SetFocus;
    except

    end;
  end
  else
  begin
    EditPassword1.Tag := 1;
    EditPassword1.EmptyValue := 'Password is cached';
    //EditPassword1.ThemedPassword := false;
    EditPassword1.ProtectPassword := false;
    EditPassword1.PasswordChar := #0;
    EditPassword1.Text := '';

    EditPassword2.Tag := 1;
    EditPassword2.EmptyValue := 'Password is cached';
    //EditPassword1.ThemedPassword := false;
    EditPassword2.ProtectPassword := false;
    EditPassword2.PasswordChar := #0;
    EditPassword2.Text := '';
  end;


  if IsAlternativeLogon and ButtonUser.Enabled then
  begin
    ButtonUser.Click;
  end
  else
  begin
    ButtonDefaultUser.Click;
  end;

  Signed := not IsSigned(AppName ,Publisher);
  if Signed then
    JvStaticText_AppPublisher.Caption := Publisher
  else
    JvStaticText_AppPublisher.Caption := GetBinaryPublisher;

  JvGradient_SignedApp.Visible := Signed;
  JvGradient_UnsignedApp.Visible := not Signed;
  JvLabel_SignedApp.Visible := Signed;
  JvLabel_UnsignedApp.Visible := not Signed;



  Application.OnIdle := ApplicationEvents1Idle;

  Button_EndService.Visible := Flags and SERVER_DEBUGTERMINATE = SERVER_DEBUGTERMINATE;

  fDesktopSwitchNotification := False;

  if SecureDesktop then
  begin
    fDesktopSwitchThread := TJwDesktopSwitchThread.Create(true,'');
    fDesktopSwitchThread.FreeOnTerminate := true;
    fDesktopSwitchThread.Resume;
  end
  else
  begin
    
  end;

  if Assigned(UserImage) then
    try
      JvImage1.Picture.Graphic.LoadFromStream(UserImage);
    except
      JvImage1.Picture.Assign(nil);

    end;
  try
    BringToFront;
    SetFocus;

  except
  end;
end;

procedure TFormCredentials.CenterInMonitor(const i : Integer);
var SX, SY : Integer;
begin
  SX := abs(abs(Screen.Monitors[I].BoundsRect.Right) - abs(Screen.Monitors[I].BoundsRect.Left));
  Left := Screen.Monitors[I].Left + ((SX div 2) - (Width div 2));

  SY := abs(abs(Screen.Monitors[I].BoundsRect.Bottom) - abs(Screen.Monitors[I].BoundsRect.Top));
  Top := Screen.Monitors[I].Top + ((SY div 2) - (Height div 2));

  exit;
  SY := Screen.Monitors[I].BoundsRect.Top - Screen.Monitors[I].BoundsRect.Bottom;
  SetBounds(Screen.Monitors[I].Left + SX, //+ ((SX - Width) div 2),
                    Top,
                    //(((Screen.Monitors[I].BoundsRect.Bottom - Screen.Monitors[I].BoundsRect.Top)- Height) div 2),
                     Width, Height);
//  Left := 0;
end;

procedure TFormCredentials.CheckBoxSaveLogonClick(Sender: TObject);
begin
  {if CheckBoxSaveLogon.Checked then
    MakeFullyVisible(Screen.Monitors[1])
  else
    MakeFullyVisible(Screen.Monitors[0]);
  DefaultMonitor := dmPrimary;
  Position := poScreenCenter;   }

  if CheckBoxSaveLogon.Checked then
    Flags := Flags or CLIENT_CACHECREDS
  else
    Flags := Flags and not CLIENT_CACHECREDS;
  
end;

destructor TFormCredentials.Destroy;
begin
  Application.OnIdle := nil;

  fAppName := GetFillPasswd;
  fAppCmdLine := GetFillPasswd;
  fUserName := GetFillPasswd;
  fPassword := GetFillPasswd;

  if Assigned(fDesktopSwitchThread) then
  begin
    fDesktopSwitchThread.Terminate;
    fDesktopSwitchThread := nil;
  end;

  inherited;
end;

procedure TFormCredentials.BitBtn_OkClick(Sender: TObject);
begin
  ModalResult := mrOk;
  Close;
end;

procedure TFormCredentials.ApplicationEvents1Idle(Sender: TObject;
  var Done: Boolean);
var
  R,
  BytesRead : DWORD;
  Data : Array[0..1] of DWORD;
begin
{$IFDEF FORMONLY}
  exit;
{$ENDIF FORMONLY}
  //ignore timeout if desktop is not our secure desktop
  if not fDesktopSwitchNotification then
  begin
    if PeekNamedPipe(
      PipeSession.Handle,//hNamedPipe: THandle;
      @Data,//lpBuffer: Pointer;
      sizeof(Data),//nBufferSize: DWORD;
      @BytesRead,//lpBytesRead,
      nil,//lpTotalBytesAvail,
      nil//lpBytesLeftThisMessage: Pointer): BOOL; stdcall;
     ) then
    begin
      if BytesRead = sizeof(Data) then
      begin
        if Data[0] = ERROR_TIMEOUT then
        begin
          Application.OnIdle := nil;
          ModalResult := mrIgnore;
          Close;
        end;
      end;
    end
    else
    begin
      OutputDebugStringA(PChar(IntToStr(GetLAstError)));
      case GetLastError() of
        ERROR_BROKEN_PIPE,
        ERROR_BAD_PIPE,
        ERROR_PIPE_NOT_CONNECTED,
        ERROR_NO_DATA,
        ERROR_PIPE_BUSY :
          begin
            Application.OnIdle := nil;

            BitBtnCancel.Click;
          end;
      end;
    end;
  end;
end;

procedure TFormCredentials.BitBtnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
  Close;
end;

procedure TFormCredentials.JvBevel1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var S : String;
begin
  try
{$IFDEF DEBUG}
    S := Format('%s%s(%s)',
        [TControl(Sender).Hint, #13#10, TControl(Sender).Name]);
{$ELSE}
   S := TControl(Sender).Hint;
{$ENDIF DEBUG}
  except
    S := '';
  end;
  if Sender = JvProgressBar then
  begin
    S := Format('%d seconds left until this dialog will be canceled and the eleveation is aborted.',[JvProgressBar.Max - JvProgressBar.Position]);
  end;

  try
    JvLinkLabel2.Caption := S;
  except
    JvLinkLabel2.Caption := '';
  end;
end;

procedure TFormCredentials.UsersComboBoxMouseEnter(Sender: TObject);
begin
  try
{$IFDEF DEBUG}
    JvLinkLabel2.Caption := Format('%s%s(%s)',
        [TControl(Sender).Hint, #13#10, TControl(Sender).Name]);
{$ELSE}
    JvLinkLabel2.Caption := TControl(Sender).Hint;
{$ENDIF DEBUG}
  except
  end;
end;

procedure TFormCredentials.JvLinkLabel1LinkClick(Sender: TObject;
  LinkNumber: Integer; LinkText, LinkParam: String);
begin
  //
  fShowWebPage := true;

end;



procedure TFormCredentials.SetUserKey(key: HKEY);
begin
  fUserRegKey := key;
  fUserRegistry := TRegistry.Create;
  fUserRegistry.RootKey := Key;
  fUserRegistry.Access := KEY_ALL_ACCESS;
end;

procedure TFormCredentials.ShowLogonError(const Visible: Boolean);
begin
  Image_LogonError.Visible := Visible;
  JvStaticText_LogonError.Visible := Visible;
end;

procedure TFormCredentials.SWITCH_DESKTOP(var Message: TMsg);

  function CreateAndGetInputDesktop: TJwSecurityDesktop;
  var
    SD: TJwSecurityDescriptor;
    Token : TJwSecurityToken;
    DACL : TJwDAccessControlList;
    i : Integer;
  begin
    for i := 1 to 2 do
    try
      result := TJwSecurityDesktop.CreateAndGetInputDesktop([],false, DESKTOP_READOBJECTS or DESKTOP_SWITCHDESKTOP);
      break;
    except
      on E : EJwsclOpenDesktopException do
        {if we cannot access the object we have to force it
        }
        if E.LastError = 5 then
        begin
          //these privileges allows to change the owner
          TJwPrivilegeScope.Create([SE_TAKE_OWNERSHIP_NAME, SE_RESTORE_NAME],pst_EnableIfAvail);

          //get handle to set owner
          result := TJwSecurityDesktop.CreateAndGetInputDesktop([],false, WRITE_OWNER);
          try
            TJwSecureGeneralObject.TakeOwnerShip(result.Handle, SE_WINDOW_OBJECT);
          finally
            result.Free;
          end;

          //get handle to write DACL
          result := TJwSecurityDesktop.CreateAndGetInputDesktop([],false, WRITE_DAC);
          try
            Token := TJwSecurityToken.CreateTokenEffective(MAXIMUM_ALLOWED);
            TJwAutoPointer.Wrap(Token);
            DACL := Token.TokenDefaultDacl;
            TJwAutoPointer.Wrap(DACL);

            //we use the default dacl that allows us full access
            TJwSecureGeneralObject.SetSecurityInfo(result.Handle, SE_WINDOW_OBJECT,
              [siDaclSecurityInformation],nil,nil,DACL,nil);
          finally
            Result.Free;
          end;

        end;
    end;
  end;

var
  Desk : TJwSecurityDesktop;
  LastDesk : HDESK;

begin
  try
    fDesktopSwitchNotification := true;

    Desk := CreateAndGetInputDesktop;

    try
      {
      If the desktop is switched to a desktop unequal to the winlogon desktop
      we switch back immediately.
      Winlogon is not changed.
      }
      if (JwCompareString(Desk.Name,'winlogon', true) <> 0) then
      begin
        try
          if Assigned(fDesktop) then
          begin
            LastDesk := fDesktop.LastSwitchDesktop;
            try
              fDesktop.SwitchDesktop;
            finally
             fDesktop.LastSwitchDesktop := LastDesk;
            end;
          end;
        finally
          fDesktopSwitchNotification := false;
        end;
      end;
    finally
      Desk.Free;
    end;
  except
  end;
end;

procedure TFormCredentials.Timer_ElevationTimeOutTimer(Sender: TObject);
Var Element : TObject;
  P : TPoint;
begin
  //sometime these commands may fail if the desktop is the
  //winlogon desktop, so we catch it
  try
    P := Self.ScreenToClient(Mouse.CursorPos);

    Element := ControlAtPos(P,false,true{, true});

    //update the hint window with new text and time progress
    if Assigned(Element) and (Element = JvProgressBar) then
       JvBevel1MouseMove(Element,[],P.X, P.Y);
  except
  end;

  JvProgressBar.StepIt;

  if GetLastError <> 0 then
    OutputDebugStringA(PAnsiChar(SysErrorMessage(GetLastError)));
end;

function GetFillPasswd : String;
var i,c : Integer;
begin

  repeat
    c := random(200);
  until c > 40;

  SetLength(result, c);
  for i := 1 to c  do
    result[i] := Char(random(255));
end;


{ TXPRegistry }


begin
  randomize;
end.
