unit UJwaTypeForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UDataModule, UPage, StdCtrls, ComCtrls, VirtualTrees;

type
  TJwaTypeForm = class(TPageForm)
    Label1: TLabel;
    MainVirtualStringTree: TVirtualStringTree;
    procedure FormActivate(Sender: TObject);
    procedure MainVirtualStringTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure MainVirtualStringTreeInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure MainVirtualStringTreeInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    function GetNextPageIndex(const showGui : Boolean) : Integer; override;
    procedure GetNextUpdate(Sender : TObject); override;

    procedure OnBack(Sender : TObject); override;

    procedure OnGetData(const DataModul : TSetupDataModule); override;
    procedure OnSetData(const DataModul : TSetupDataModule); override;
  end;

var
  JwaTypeForm: TJwaTypeForm;

implementation

{$R *.dfm}

type
  PItemData = ^TItemData;
  TItemData = record
    Delphi : String;
    Targets : TStringList;
  end;

{ TJwaTypeForm }

procedure TJwaTypeForm.FormActivate(Sender: TObject);
var
  P : PVirtualNode;
  Data : PItemData;
begin
  MainVirtualStringTree.NodeDataSize := sizeof(TItemData);
  //
  MainVirtualStringTree.
  {P := MainVirtualStringTree.AddChild(nil);
  P.ChildCount := 2;
  Data := MainVirtualStringTree.GetNodeData(P);
  Data^.Delphi := '123';      }
  //MainVirtualStringTree.AddChild(P);


end;

function TJwaTypeForm.GetNextPageIndex(const showGui: Boolean): Integer;
begin
  result := NextArray[JWA_TYPE_FORM];
end;

procedure TJwaTypeForm.GetNextUpdate(Sender: TObject);
begin
  inherited;

end;

procedure TJwaTypeForm.MainVirtualStringTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  Data : PItemData;
begin
  Data := MainVirtualStringTree.GetNodeData(Node);
  case Column of
    0 : CellText := Data^.Delphi;
  end;
end;

procedure TJwaTypeForm.MainVirtualStringTreeInitChildren(
  Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
var
  Data : PItemData;
begin
  Data := MainVirtualStringTree.GetNodeData(Node);
  if Assigned(Data^.Targets) then
    ChildCount := Data^.Targets.Count
  else
    ChildCount := 2;
end;

procedure TJwaTypeForm.MainVirtualStringTreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  //

end;

procedure TJwaTypeForm.OnBack(Sender: TObject);
begin
  inherited;

end;

procedure TJwaTypeForm.OnGetData(const DataModul: TSetupDataModule);
begin
  inherited;

end;

procedure TJwaTypeForm.OnSetData(const DataModul: TSetupDataModule);
begin
  inherited;

end;

end.

