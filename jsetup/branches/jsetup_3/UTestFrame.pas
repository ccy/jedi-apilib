unit UTestFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, UInstallPage, CheckScrollBox, StdCtrls, JvExStdCtrls, JvRadioButton,
  JvCombobox, ExtCtrls, JvExExtCtrls, JvExtComponent, JvPanel, Mask, JvExMask,
  JvToolEdit, JvExControls, JvLabel;

type
  TFrame2 = class(TFrame)
    jvlbLabel: TJvLabel;
    edtTargetPath: TJvDirectoryEdit;
    chkForAllDelphi: TCheckBox;
    jvpnlJWA: TJvPanel;
    JLabel2: TJvLabel;
    JLabel3: TJvLabel;
    JLabel1: TJvLabel;
    btnJWAReleaseUpdate: TButton;
    cbbJWARelease: TJvComboBox;
    cbbJwaCurrentConfiguration: TJvComboBox;
    rbSingleJWAMode: TJvRadioButton;
    rbPrecompiledJWA: TJvRadioButton;
    chckscrlbxJWAConfiguration: TCheckScrollBox;
    btnHistoryJWA: TButton;
    jvpnl1: TJvPanel;
    JLabel4: TJvLabel;
    JLabel6: TJvLabel;
    btnJWSCLReleaseUpdate: TButton;
    cbbJWSCLRelease: TJvComboBox;
    chckscrlbxJWSCLConfiguration: TCheckScrollBox;
    btnHistoryJWSCL: TButton;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

end.
