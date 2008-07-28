unit UJwaTypeForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UDataModule, UPage, StdCtrls, ComCtrls, VirtualTrees,
  DelphiVersionTool;

type
  TJwaTypeForm = class(TPageForm)
    Label1: TLabel;
    MainVirtualStringTree: TVirtualStringTree;
    procedure FormActivate(Sender: TObject);
    procedure MainVirtualStringTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure MainVirtualStringTreeMeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
  private
    { Private-Deklarationen }
    IsInit : Boolean;
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
  TItemType = (itParent, itSingle, itDcu);

  PItemData = ^TItemData;
  TItemData = record
    Text : WideString;
    JwsclSelected : Boolean;
    ItemType : TItemType;
    InstalledDelphiIndex : Integer; //index of DelphiVersionTool.KnownDelphiVersions
  end;

{ TJwaTypeForm }

{
'Add the folder of the single source files of JWA to Delphi''s source path. '#13#10+
                'You can create projects using the single JwaXXXX.pas files directly.'#13#10+
                'However this approach cannot be selected when using JWSCL.';

 'Compile the JwaWindows.dcu file and add its folder to the Delphi''s source path'#13#10+
         'This approach is very convenient because only one unit must be included into a project'#13#10+
         'Searching for units is no more necessary.'#13#10+
         'JWSCL needs this file to be available.';
}

procedure TJwaTypeForm.FormActivate(Sender: TObject);

  procedure AddChilds(const DelphiVersionIndex : Integer; const Node : PVirtualNode; NodeData : PItemData;
    CheckSingleUnits : Boolean);
  var
    P1,P2 : PVirtualNode;
    Data : PItemData;
  begin
    P1 := MainVirtualStringTree.AddChild(Node);
    Data := MainVirtualStringTree.GetNodeData(P1);
    Data.ItemType := itSingle;
    Data.JwsclSelected := NodeData.JwsclSelected;
    Data^.InstalledDelphiIndex := DelphiVersionIndex;

    MainVirtualStringTree.MultiLine[P1] := true;
    MainVirtualStringTree.CheckType[P1] := ctRadioButton;
    if CheckSingleUnits then
      MainVirtualStringTree.CheckState[P1] := csCheckedNormal
    else
      MainVirtualStringTree.CheckState[P1] := csUnCheckedNormal;

    Data^.Text := 'Single units'#13#10+
                  'e.g. uses JwaNative, JwaWinTypes, JwaShellApi.';

    P2 := MainVirtualStringTree.AddChild(Node);

    Data := MainVirtualStringTree.GetNodeData(P2);
    Data.ItemType := itDcu;
    Data.JwsclSelected := NodeData.JwsclSelected;
    Data^.InstalledDelphiIndex := DelphiVersionIndex;
    Data^.Text := 'Use JwaWindows.dcu'#13#10+
                'e.g. uses JwaWindows;';


    MainVirtualStringTree.MultiLine[P2] := true;
    MainVirtualStringTree.CheckType[P2] := ctRadioButton;
    if not CheckSingleUnits then
      MainVirtualStringTree.CheckState[P2] := csCheckedNormal
    else
      MainVirtualStringTree.CheckState[P2] := csUnCheckedNormal;
  end;

  procedure AddDelphi(const DelphiVersionIndex : Integer;
                JwsclSelected : Boolean; SingleUnitsSelected : Boolean );
  var
    P : PVirtualNode;
    Data : PItemData;
  begin
    P := MainVirtualStringTree.AddChild(nil);
    Data := MainVirtualStringTree.GetNodeData(P);
    Data.JwsclSelected := JwsclSelected;

    Data.ItemType := itParent;
    Data.Text := KnownDelphiVersions[DelphiVersionIndex].VersionName;

    if (Data.JwsclSelected) then
      Data.Text := Data.Text + ' (JWSCL was selected on previous page) ';
    Data.InstalledDelphiIndex := DelphiVersionIndex;

    AddChilds(DelphiVersionIndex, P, Data, SingleUnitsSelected);
    MainVirtualStringTree.Expanded[P] := true;
  end;




  function IsJwaAndJwsclSelected(const Index : Integer) : Boolean;
  var i1,i2 : Integer;
    I: Integer;
  begin
    result := false;
    for I := 0 to SetupDataModule.TargetDelphiJwscl.Count - 1 do
    begin
      result := (SetupDataModule.TargetDelphiJwscl[i] = SetupDataModule.TargetDelphiJwa[Index]);
      if result then
        Break;
    end;
  end;

var
  i, Index : Integer;
  SingleChecked : Boolean;
begin
  MainVirtualStringTree.NodeDataSize := sizeof(TItemData);
  IsInit := true;

  MainVirtualStringTree.Clear;


  if Assigned(SetupDataModule.TargetDelphiJwa) then
  
  for i := 0 to SetupDataModule.TargetDelphiJwa.Count - 1 do
  begin
    Index := Integer(SetupDataModule.TargetDelphiJwa[i]);

    SingleChecked := SetupDataModule.JwaTypes.IsSingleJwa[Index];

    AddDelphi(SetupDataModule.TargetDelphiVerToKnownDelphiIndex(Index),
                IsJwaAndJwsclSelected(i), SingleChecked);
  end;


  IsInit := false;
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
    0 : CellText := Data^.Text;
  end;
end;

procedure TJwaTypeForm.MainVirtualStringTreeMeasureItem(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  var NodeHeight: Integer);
var
  Data : PItemData;
  S : Widestring;
  p,c : Integer;
begin
  Data := MainVirtualStringTree.GetNodeData(Node);

  S := Data^.Text;
  c := 1;
  repeat
    p := Pos(#13,S);
    if p > 0 then
    begin
      S[p] := '_';
      Inc(c);
    end;
  until p <= 0;
  //
  NodeHeight := TargetCanvas.TextHeight('A')*c+3;
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
var i : Integer;
    Data : PItemData;
  Node, ChildNode : PVirtualNode;
begin
  inherited;

  Node := MainVirtualStringTree.GetFirstChecked;
  while Node <> nil do
  begin
    Data := MainVirtualStringTree.GetNodeData(Node);
    SetupDataModule.JwaTypes.IsSingleJwa[KnownDelphiVersions[Data^.InstalledDelphiIndex].Ver]
      := Data^.ItemType = itSingle;

    Node := MainVirtualStringTree.GetNextChecked(Node);
  end;
end;

end.

