unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, StdCtrls, JwsclTerminalServer;

type
  PNodeData = ^TNodeData;
  TNodeData = record
    Index: Integer;
    List: ^TjwWTSSessionList;
end;

  TForm4 = class(TForm)
    VirtualStringTree1: TVirtualStringTree;
    Button1: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure VirtualStringTree1GetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure Button1Click(Sender: TObject);
    procedure VirtualStringTree1GetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VirtualStringTree1InitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  private
    { Private declarations }
    TerminalServer: TJwTerminalServer;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.Button1Click(Sender: TObject);
var
  pNode:  PVirtualNode;
  pData:  PNodeData;

begin
  // 1e parameter is eventueel parent
//  pNode := VirtualStringTree1.AddChild(nil, nil);
//  pData := VirtualStringTree1.GetNodeData(pNode);
  if TerminalServer.EnumerateSessions then
  begin
    VirtualStringTree1.RootNodeCount := TerminalServer.Sessions.Count;
  end;

end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  TerminalServer := TJwTerminalServer.Create;
end;

procedure TForm4.VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  pData:    PNodeData;
begin
  if Kind in [ikNormal, ikSelected] then begin
    pData      := Sender.GetNodeData(Node);
  end;
end;

procedure TForm4.VirtualStringTree1GetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNodeData);
end;

procedure TForm4.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  pData: PNodeData;
begin
  pData := Sender.GetNodeData(Node);
  case Column of
    0: CellText := pData^.List.Owner.Server;
    1: CellText := pData^.List.Items[pData^.Index].Username;
    2: CellText := pData^.List.Items[pData^.Index].WinStationName;
    3: CellText := IntToStr(pData^.List.Items[pData^.Index].SessionId);
    4: CellText := pData^.List.Items[pData^.Index].ConnectStateStr;
    5: CellText := pData^.List.Items[pData^.Index].IdleTimeStr;
    6: CellText := pData^.List.Items[pData^.Index].LogonTimeStr;
  end;
//  CellText := pData^.Caption;
end;

procedure TForm4.VirtualStringTree1InitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var P : PNodeData;
begin
  Memo1.Lines.Add(Format('Initnode: %d', [Node^.Index]));
  P := (Sender.GetNodeData(Node));
  P^.Index := Node^.Index;
  P^.List := @TerminalServer.Sessions;
end;

end.
