unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, ToolWin, Menus,

  JwaVista, JwsclUtils,
  JwsclTypes, JwsclExceptions, JwsclAcl, JwsclMapping, JwsclSid, JwsclKnownSid,
  JwsclSecureObjects, JwsclResource, JwsclSecurePrivateObjects,JwsclEnumerations,
  JwsclVersion, JwsclConstants, JwsclProcess, JwsclDescriptor, JwsclToken,
  JwsclStrings;


type
  TSecurityData = TJwInterfacedPrivateSecurityInformation;
  TChildData = class;
  TTreeItemData = class;

  TFormMain = class(TForm)
    MainMenu: TMainMenu;
    MainTreeView: TTreeView;
    file1: TMenuItem;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    Splitter1: TSplitter;
    Panel1: TPanel;
    MainListView: TListView;
    Splitter2: TSplitter;
    Panel2: TPanel;
    ACLListView: TListView;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    ACLEditButton: TBitBtn;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure MainTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure ToolButton1Click(Sender: TObject);
    procedure MainTreeViewDeletion(Sender: TObject; Node: TTreeNode);
  private
    { Private-Deklarationen }
    function GetSecurityData(const Node : TTreeNode) : TSecurityData; overload;
    function GetSecurityData(const Item : TListItem) : TSecurityData; overload;

    function GetData(const Node : TTreeNode) : TTreeItemData; overload;
    function GetData(const Item : TListItem) : TChildData; overload;

  public
    { Public-Deklarationen }
  end;

  TChildData = class
  public
    Security : IJwPrivateSecurityInformation;
    Name : String;
  end;

  TTreeItemData = class
  public
    Security : IJwPrivateSecurityInformation;
    Childs   : TList;

    destructor Destroy; override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

var TreeItem : TTreeNode;
    ListItem : TListItem;
    I : Integer;

{ TFormMain }

function GetDefaultSecurity : TSecurityData;
begin
  result := TSecurityData.Create();
end;


procedure TFormMain.FormCreate(Sender: TObject);
begin
  JwInitWellKnownSIDs;

  TreeItem := MainTreeView.Items.AddChild(nil,'root');

  TreeItem.Data := TTreeItemData.Create;
  GetData(TreeItem).Childs := TList.Create;
  GetData(TreeItem).Security := GetDefaultSecurity;
end;

procedure TFormMain.MainTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  //
  MainListView.Clear;
  if Assigned(GetData(Node)) and Assigned(GetData(Node).Childs) then
  begin
    for I := 0 to GetData(Node).Childs.Count -1 do
    begin
      ListItem := MainListView.Items.Add;
      ListItem.Data := TChildData(GetData(Node).Childs[i]);
      ListItem.Caption := GetData(ListItem).Name;
    end; 
  end;
end;

function TFormMain.GetSecurityData(const Node: TTreeNode): TSecurityData;
begin
  result := nil;
  if Assigned(Node) and Assigned(Node.Data) then
    result := TSecurityData(TTreeItemData(Node.Data).Security);
end;

function TFormMain.GetSecurityData(const Item: TListItem): TSecurityData;
begin
  result := nil;
  if Assigned(Item) and Assigned(Item.Data) then
    result := TSecurityData(TChildData(Item.Data).Security);
end;

function TFormMain.GetData(const Node: TTreeNode): TTreeItemData;
begin
  result := nil;
  if Assigned(Node) then
    result := TTreeItemData(Node.Data);
end;

function TFormMain.GetData(const Item: TListItem): TChildData;
begin
  result := nil;
  if Assigned(Item) then
    result := TChildData(Item.Data);
end;

procedure TFormMain.ToolButton1Click(Sender: TObject);
begin
  //
end;

procedure TFormMain.MainTreeViewDeletion(Sender: TObject; Node: TTreeNode);
begin
  TTreeItemData(Node.Data).Free;
end;

{ TTreeItemData }

destructor TTreeItemData.Destroy;
begin
  FreeAndNil(Childs);
  Security._Release;
  Security := nil;
  inherited;
end;

end.
