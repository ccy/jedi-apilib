{
 Unit: CheckScrollBox
 Component : TCheckScrollBox
 Author: Dezipaitor (dezipaitor(@t)gmx.de)                                  

 CreationDate: 27.12.2005
 LastChange: 24.1.2007
 Version : 1.3 
 Tested with: Delphi 5,7, 2006
 License: Creative Commons License
  # Attribution. You must attribute the work in the manner specified by the author or licensor.
  # Noncommercial. You may not use this work for commercial purposes.
  # Share Alike. If you alter, transform, or build upon this work, you may distribute the resulting work only under a license identical to this one. 
  See for more information
	http://creativecommons.org/licenses/by-nc-sa/2.0
 Attribution: Show name and contact data of author in your credits. THX
 Commercial use: contact the author
 
 The license does not affect compilers.inc. 

 Bugs, hints, questions and so on.... write to the author.


 TCheckScrollBox provides a list of checkboxes in a scrollbox.
  It automatically adjust checkboxes and labels with more than one line.
  Its features are :
    - (dis-)enable and (un-)check checkboxes
    - labels with more than one line (auto wrap)
    - focus controls
    - using & within label caption
    - auto adjustment of label width if scrollbar width is changed
    - manipulating Items also changes checkboxes and labels in realtime
    - unicode support (through TNT Controls)
            TTNTCustomCheckScrollBox and TCheckScrollBox
}

{Includes some useful compiler version infos.}
{$I Compilers.inc}


{DEFINE USE_UNICODE}   //remove *** to active unicode
{$UNDEF USE_UNICODE}   //remove *** to active unicode

{
 Setting this compiler directive uses the unicode controls of
  http://www.tntware.com/delphicontrols/unicode

 Make sure the following units are in visible range to delphi:
  TntClasses
  TntStdCtrls
  TntForms

 We will use the following components:
  TTntStringList
  TTntStrings;
  TTntLabel;
  TTntCheckBox;
  TTntScrollBox;

 You can also set this directive in Package Options 
 }



{$IFDEF USE_UNICODE}
 {$DEFINE USE_TNT_PREFIX}
{$ENDIF}
(*
 This compiler directive renames
 TCustomCheckScrollBox  to TTNTCustomCheckScrollBox and
 TCheckScrollBox  to TTNTCheckScrollBox
 so you can install and use both
   unicode and ascii code CheckScrollBox.


 If there is no file named TNTCheckScrollBox.pas read on:

 However you must use a renamed copy ( TNTCheckScrollBox.pas)
 and add it to your package.
 Additionally you have to do :
   * remove definition "USE_UNICODE" from your package options!!
   * activate USE_UNICODE in your renamed copy ( TNTCheckScrollBox.pas)
      - find ->> {***$DEFINE USE_UNICODE}   //remove *** to active unicode
     set {$DEFINE USE_TNT_PREFIX} in this unit, too.
   * deactivate USE_UNICODE in TCheckScollBox.pas
 Now you can compile and install everything.

*)



{******************** CODE ****************************************************}

{$IFDEF USE_TNT_PREFIX}
unit TNTCheckScrollBox;
{$ELSE}
unit CheckScrollBox;
{$ENDIF}

interface



uses
  SysUtils, Classes, Controls, Forms,StdCtrls, Dialogs,
{$IFDEF COMPILER_7_UP} //or COMPILER_6_UP ?
  Types,
{$ELSE}
  Windows,
{$ENDIF}


{$IFDEF USE_UNICODE}
  TntClasses,
  TntStdCtrls,
  TntForms,
{$ENDIF}
  Graphics;

type
  {We simply set our type names to the corresponding unicode or ascii controls}
  {$IFDEF USE_UNICODE}
    TUStringList = TTntStringList;
    TUStrings    = TTntStrings;
    TULabel      = TTntLabel;
    TUCheckBox   = TTntCheckBox;
    TUScrollBox  = TTntScrollBox;


  {$ELSE}
    TUStringList = TStringList;
    TUStrings    = TStrings;
    TULabel      = TLabel;
    TUCheckBox   = TCheckBox;
    TUScrollBox  = TScrollBox;

    {$IFDEF USE_TNT_PREFIX}
    {$ERROR You can't use TNT Prefix if you don't use unicode.}
    {$ENDIF}
  {$ENDIF}



  {
  TCustomCheckScrollBox provides a list of checkboxes in a scrollbox.
  It automatically adjust checkboxes and labels with more than one line.
  Its features are :
    - (dis-)enable and (un-)check checkboxes
    - labels with more than one line (auto wrap)
    - focus controls
    - using & within label caption
    - auto adjustment of label width if scrollbar width is changed
    - manipulating Items also changes checkboxes and labels in realtime

  @link(TCheckScrollBox) extends this class to offer published properties.
  You should use this one.

  You can extend this class like TCheckScrollBox does to create new
  possibilites.
  }
  {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF} = class;

  {This event method is called if a checkbox changes its check state.
   @param(Sender is a TCustomCheckScrollBox thats checkbox is changed)
   @param(CheckBoxIndex is the Items Index of checkbox. You can access Items[CheckBoxIndex])
   @param(Checkbox is the checkbox that is changed. You can access directly its properties (like Checked) and methods.)

   Example:

   procedure TMainForm.CheckScrollBox1CheckBoxChecked(Sender: TCustomCheckScrollBox; CheckBoxIndex: Integer;
                                                       CheckBox: TUCheckBox);
   begin
     if CheckBox.Checked then
       Label1.Caption := 'checked: Index: '+ IntToStr(CheckBoxIndex) + ' Label: ' + Sender.Labels[CheckBoxIndex].Caption
     else
      Label1.Caption := 'unchecked, Index: '+ IntToStr(CheckBoxIndex) + ' Label: ' + Sender.Labels[CheckBoxIndex].Caption;
   end;

   Changing Sender.Labels[CheckBoxIndex].Caption does not affect Items[CheckBoxIndex].

   }
  TOnCheckBoxChecked = procedure(Sender : {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}; CheckBoxIndex : Integer; CheckBox : TUCheckBox) of object;

  {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF} = class(TUScrollBox)
  private
    { Private-Deklarationen }
    FItems : TUStrings;
    FMargin : TPoint;

    {old color of label background.
     Its set in OnCheckBoxEnter and
      used OnCheckBoxLeave to
      obtain original label background color.
    }
    tempColor : TColor;

    //focus color
    FFocusLabelColor : TColor;

    //stores count of items
    FOldItemsCount : Integer;

    FOnCheckBoxChecked : TOnCheckBoxChecked;
    FCheckBoxNames : TUStrings;
  protected
    { Protected-Deklarationen }
    procedure SetItems(const Items : TUStrings);
    //procedure SetCheckBoxNames(const Items : TUStrings);

    function GetCount : Integer;


    function GetChecked(Index : Integer) : Boolean;
    procedure SetChecked(Index : Integer; Check : Boolean);

    function GetCheckBoxIndex(Name : WideString) : Integer;

    function GetCheckBoxHint(Index : Integer) : WideString;
    procedure  SetCheckBoxHint(Index : Integer; Hint : WideString);

    function GetObjects(Index : Integer) : Pointer;
    procedure SetObjects(Index : Integer; Data : Pointer);

    function GetCaptions(Index : Integer) : String;
    procedure SetCaption(Index : Integer; const Caption : String);

    function GetItemEnabled(Index : Integer) : Boolean;

    {}
    procedure SetItemEnabled(Index : Integer; Enable : Boolean);

    {It returns the corresponding label of a checkbox.
     The way TULabel.FocusControl -> Checkbox is beeing
     turned around.
     All labels are checked for their FocusControl.
      If it is equal the label is returned.
     In any other case nil is returned.
     }
    function GeTULabel(CheckBox :TUCheckBox) : TULabel;

    {see @link(Labels)}
    function GeTULabelByIndex(Index : Integer) : TULabel;

    procedure OnBoxClick(Sender: TObject);

    {Is beeing called when user clicks on a label
     and checkbox shall change check state.}
    procedure OnLabelClick(Sender: TObject);
    {Is beeing called when checkbox gains focus}
    procedure OnCheckBoxEnter(Sender: TObject);
    {Is beeing called when checkbox loses focus}
    procedure OnCheckBoxLeave(Sender: TObject);
    {checkbox is clicked (changes state)}
    procedure OnCheckBoxClick(Sender: TObject);


    {called when a stringlist entry is change}
    procedure OnStringListChange(Sender: TObject);

    {ReadState is called when published properties are read from stream.
     It simply calls UpdateItems to show all labels and checkboxes
      after FItems has been read from stream.}
    procedure ReadState(Reader: TReader); override;

    {Removes all Checkboxes and Labels}
    procedure Remove_CheckBoxes;

    {Adds (but not removes) new checkboxes and labels according to Items.}
    procedure Update_CheckBoxes;


  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {Adjust labels if scrollbox width is beeing changed.}
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    {Updates all Items after a change has happend.
     This procedure is no more necessary to be called. }
    procedure UpdateItems;

    procedure Clear; virtual;

    function GetCheckBoxNameIndex(Name : WideString) : Integer;

    {Sets or get checkbox check status.
     If index is not in bounds (< Items.Count) an Exception is raised.}
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;

    {Sets or get checkbox enable status.
     If index is not in bounds (< Items.Count) an Exception is raised.}
    property ItemEnabled[Index: Integer]: Boolean read GetItemEnabled write SetItemEnabled;

    {Labels gives you direct access to a label and checkbox control in scrollbox.
     You can simypl access a checkbox by (Labels[1].FocusControl as TUCheckBox).
     Warning:
       You must not change Labels[1].FocusControl or results are unpredictable.
       Following events are used:

        TULabel.OnClick
        TUCheckBox.OnEnter
        TUCheckBox.OnExit
        TUCheckBox.OnClick

     Property Labels is readonly, however you can change properties of given label.
    }
    property Labels[Index : Integer] : TULabel read GeTULabelByIndex;

    property CheckBoxIndex[CheckBoxName : WideString] : Integer read GetCheckBoxIndex;

    {Margin contains values to add to left and top scrollbox borders.
     All checkboxes and labels are affected.}
    property Margin : TPoint read FMargin write FMargin;


    {contains label descriptions.
     Setting Items directly (by Items := myStringList) updates labels and checkboxes automatically.

     All types of direct manipulating Items are supported :
       Items[0] := '123';
       Items.Exchange(1,0);
       items.Add
       items.delete
       and so on...
     Add and Delete will erase all checked settings.



     Be aware that all Objects (TUStringList.Objects) are used for
      internal operations. Changes leads to unpredictable results.

     }
    //property Items : TUStrings read FItems write SetItems;



    function Add(const Caption : String; Data : Pointer) : Integer;
    procedure Remove(const Index : Integer);


    property Captions[Index : Integer] : String read GetCaptions write SetCaption;

    {Renames checkbox names.
     If you enters invalid or duplicate names an EComponentError will be raised
      as soon as the checkboxes are created.
      Checkboxes will be removed and readded. 
    }
    property CheckBoxNames : TUStrings read FCheckBoxNames{ write SetCheckBoxNames};


    {Changes label background color.
     This color is beeing used if a checkbox gets focus.}
    property FocusLabelColor : TColor read FFocusLabelColor write FFocusLabelColor;

    {If a checkbox changes check state this event is called.
     More info see @link(TOnCheckBoxChecked)}
    property OnCheckBoxChecked : TOnCheckBoxChecked read FOnCheckBoxChecked write FOnCheckBoxChecked;

    {Sets or gets a hint of a checkbox given by index.
    }
    property CheckBoxHint[Index : Integer] : WideString read GetCheckBoxHint write SetCheckBoxHint;

    property Objects[Index : Integer] : Pointer read GetObjects write SetObjects;

    property Count : Integer read GetCount;

    {**** If you adds new properties don't forget to add them to the published class underneath,
     otherwise the new property will not be visible within object inspector}
  end;

  {}
  {$IFDEF USE_TNT_PREFIX}TTNTCheckScrollBox{$ELSE}TCheckScrollBox{$ENDIF}
     = class({$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF})
  published
//    property Items;
    property FocusLabelColor;
    property OnCheckBoxChecked;
    property CheckBoxNames;
  end;

{Automatically called by delphi component manager. Do not call directly.}
procedure Register;

{*************** Invisible part ***********************************************}
implementation
                        

type
  PItemData = ^TItemData;
  TItemData = record
    _Label : TULabel;
    UserData : Pointer;
  end;

function TCustomCheckScrollBox.Add(const Caption: String;
  Data: Pointer): Integer;
var
  P : PItemData;
  i, pos : Integer;
begin
  New(P);

  P^.UserData       := Data;


  try
    pos := FItems.AddObject(Caption,Pointer(P));
    FItems.Objects[pos] := Pointer(P);

    result := pos;
    for i := 0 to fItems.Count - 1 do
      if GeTULabelByIndex(i).Tag = pos then
        result := i;
  except
    Dispose(P);
    raise;
  end;
end;

procedure TCustomCheckScrollBox.Clear;
var i : Integer;
begin
  Remove_CheckBoxes;

  //prevents Delete to send an change event
  TUStringList(FItems).OnChange := nil;
  try
    for I := fItems.Count - 1 downto 0 do
    begin
      Dispose(PItemData(FItems.Objects[i]));
      FItems.Delete(i);
    end;
  finally
    TUStringList(FItems).OnChange := OnStringListChange;
  end;
end;

constructor {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TUStringList.Create;
  FCheckBoxNames := TUStringList.Create;

  Self.OnClick := OnBoxClick;

  //we want to be called if changes happen in Items
  TUStringList(FItems).OnChange := OnStringListChange;
  FOldItemsCount := 0;
  FOnCheckBoxChecked := nil;

  Margin := Point(10,5);
  TabStop := false;
  FFocusLabelColor := clLtGray;
end;

destructor {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.Destroy;
begin
  {by calling inherited here
   all controls in this scrollbox are beeing freed
   automatically
   because we set parent.
  }
  inherited;

  {we could also make it by ourselves.
  }
  {try
   Remove_CheckBoxes;
  except
  end;
          }
  Clear;
  try
   FreeAndNil(fItems);
  except
  end;

  try
   FreeAndNil(FCheckBoxNames);
  except
  end;

  //inherited;
end;


procedure {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.OnBoxClick(Sender: TObject);
begin
   if (csDestroying in ComponentState) or (Self.Focused) then
    exit;
  try
    self.SetFocus;

    if Assigned(FItems) and (FItems.Count > 0) then
      Labels[0].FocusControl.SetFocus;
  except
  end;
end;

procedure {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.OnLabelClick(Sender: TObject);
var aLabel : TULabel;
begin
  if not (Sender is TULabel) then
    exit;

  try
    self.SetFocus;
  except
  end;

  try
    aLabel := Sender as TULabel;
  except
    exit;
  end;

  //make focus visible to label
  if Assigned(aLabel.FocusControl) then
  begin
    TUCheckBox(aLabel.FocusControl).Checked := not TUCheckBox(aLabel.FocusControl).Checked;
    TUCheckBox(aLabel.FocusControl).SetFocus;
  end;

end;

procedure {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.OnCheckBoxClick(Sender: TObject);
var CheckBox : TUCheckBox;
    aLabel : TULabel;
    i, Index : Integer;
begin
  if Assigned(FOnCheckBoxChecked) then
  begin
    CheckBox := Sender as TUCheckBox;


    Index := -1;
    for i := 0 to FItems.Count -1 do
    begin
      aLabel := GeTULabelByIndex(Index);
      if Assigned(aLabel.FocusControl) then
        if (aLabel.FocusControl = CheckBox) then
        begin
          Index := i;
          break;
        end;
    end;

    try
      OnCheckBoxChecked(self,Index,CheckBox);
    except
    end;

  end;
end;

function {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.GeTULabelByIndex(Index : Integer) : TULabel;
begin
 try
    result := (PItemData(FItems.Objects[Index])^._Label as TULabel);
  except
    raise Exception.CreateFmt('Index %d out of bounds.',[Index]);
  end;
end;

function {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.GeTULabel(CheckBox :TUCheckBox) : TULabel;

  var i : Integer;
begin
  result := nil;

  //
 try
  for i := 0 to ControlCount-1 do
  begin
    if (Controls[i] is TULabel) then
    begin
      if ((Controls[i] as TULabel).FocusControl) = CheckBox then
      begin
        result := (Controls[i] as TULabel) as TULabel;
        exit;
      end;
    end;
  end;
 except
 end;
end;

procedure {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.OnCheckBoxEnter(Sender: TObject);
var aLabel : TULabel;
    CheckBox : TUCheckBox;
begin
  aLabel := GeTULabel(Sender as TUCheckBox);
  if Assigned(aLabel) then
  begin

    tempColor := aLabel.Font.Color;  //save original color to reset
    //aLabel.Color := FFocusLabelColor;
    //aLabel.Invalidate;

   // CheckBox := Sender as TUCheckBox;
    //CheckBox.Color := FFocusLabelColor;
{    CheckBox.Invalidate;}

    aLabel.Font.Color := FFocusLabelColor;

    {aLabel.Canvas.DrawFocusRect(Rect(CheckBox.Left-2,CheckBox.Top-2,
            CheckBox.Left+CheckBox.Width+Margin.X + aLabel.Width+2,
            aLabel.Top + aLabel.Height + Margin.Y           ));  }
    {Self.Canvas.DrawFocusRect(Rect(CheckBox.Left-2,CheckBox.Top-2,
            CheckBox.Left+CheckBox.Width+Margin.X + aLabel.Width+2,
            aLabel.Top + aLabel.Height + Margin.Y           ));}
    {aLabel.Canvas.DrawFocusRect(Rect(CheckBox.Left-2,CheckBox.Top-2,
            100,100));}
   { aLabel.Canvas.DrawFocusRect(Rect(1,1,10,10));
    aLabel.Canvas.TextOut(0,0,'123');   }
  end;
end;

procedure {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.OnCheckBoxLeave(Sender: TObject);
var aLabel : TULabel;
begin
  aLabel := GeTULabel(Sender as TUCheckBox);
  if Assigned(aLabel) then
  begin
    //aLabel.Color := tempColor;
    aLabel.Font.Color := tempColor;
   // aLabel.Canvas.TextOut(0,0,'123');
  end;
end;

procedure TCustomCheckScrollBox.Remove(const Index: Integer);
begin
  Dispose(PItemData(FItems.Objects[Index]));
  FItems.Delete(Index);
end;

procedure {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.Remove_CheckBoxes;
var i : Integer;
    C : TULabel;
begin
  for i := ControlCount-1 downto 0 do
  begin
    if (Controls[i] is TULabel) then
    begin
      C := Controls[i] as TULabel;
      if Assigned(C.FocusControl) then
      begin
        C.FocusControl.Free;
        C.FocusControl := nil;
      end;

      //this decreases Controlcount by one and removes it from controls property
      C.Parent := nil;

      try
        C.Free;
      except
      end;
      
    end;
  end;
end;

procedure {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.Update_CheckBoxes;
var CheckBox : TUCheckBox;
    aLabel : TULabel;
    i, StartTop : Integer;


begin
  if not Assigned(FItems) then exit;

  { We do not want to call OnStringListChange;
   everytime FItems is accessed.
  }
  TUStringList(FItems).OnChange := nil;

  StartTop := Margin.Y;
  for i := 0 to FItems.Count -1 do
  begin
    CheckBox := TUCheckBox.Create(self);
    //add checkbox to control list of scrollbox (self)
    CheckBox.Parent := self;

    if Assigned(FCheckBoxNames) and (i < FCheckBoxNames.Count) then
    begin
       try
         CheckBox.Name := FCheckBoxNames.Strings[i];
       except
         on E : EComponentError do
           raise EComponentError.CreateFmt('T(TNT)CustomCheckScrollBox.Update_CheckBoxes: CheckBoxNames[%i] = "%s" : %s', [i,FCheckBoxNames.Strings[i],E.Message]);
       end;
    end;


    //set position
    CheckBox.Left := Margin.x;
    CheckBox.Top := StartTop;

    {sets width to height.
     height is automatically set to proper height of checkbox images.
     So we have not to bother about size.}
    CheckBox.Width := CheckBox.Height;

    {of course tab stops}
    CheckBox.TabStop := true;
    CheckBox.TabOrder := i;

    {some events}
    CheckBox.OnEnter := OnCheckBoxEnter;
    CheckBox.OnExit := OnCheckBoxLeave;
    CheckBox.OnClick := OnCheckBoxClick;

    {set visible}
    CheckBox.Visible := true;

    aLabel := TULabel.Create(self);

    aLabel.Parent := self;
    aLabel.Caption := FItems[i];//FItems.Values[FItems.Names[i]];

    {sets position}
    aLabel.Left := CheckBox.Left + CheckBox.Width ;
    aLabel.Top := StartTop+1;

    {}
    //aLabel.Height := 20;

    {this is necessary because we access checkboxes
    through labels FocusControl}
    aLabel.FocusControl := CheckBox;

    {and some events}
    aLabel.OnClick := OnLabelClick;


    {Creates auto height
     It brings all the text into the label.
     The width is fixed but height can be variable.
    }
    aLabel.WordWrap := false;
    aLabel.Width := self.Width - aLabel.Left - 2* Margin.X;
    aLabel.WordWrap := true;

    aLabel.Visible := true;


    PItemData(FItems.Objects[i])^._Label := aLabel;
    aLabel.Tag := i;


    Inc(StartTop,aLabel.Height+Margin.Y);
  end;

  TUStringList(FItems).OnChange := OnStringListChange;
end;




function {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.GetCheckBoxNameIndex(Name : WideString) : Integer;
begin
  result := FCheckBoxNames.IndexOf(Name);
end;

procedure {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.UpdateItems;
var i, StartTop : Integer;
    aLabel : TULabel;
    aCheckBox : TUCheckBox;
begin
  StartTop := Margin.Y;

  if not Assigned(FItems) then exit;

  //no controls? we need them, so we create them
  if ControlCount = 0 then
  begin
    Update_CheckBoxes;
    exit;
  end;

  if FOldItemsCount <> FItems.Count then
  begin
    //showMessage('Größ0er kleiner 2');
    Remove_CheckBoxes;
  end;

  { We do not want to call OnStringListChange;
   everytime FItems is accessed.
  }
  TUStringList(FItems).OnChange := nil;

  for i := 0 to ControlCount-1 do
  begin

    if (Controls[i] is TULabel) then
    begin
      aLabel := (Controls[i] as TULabel);
      aLabel.WordWrap := false;
      aLabel.Width := self.Width - aLabel.Left - 2* Margin.X;
      aLabel.WordWrap := true;

      aLabel.Tag := Controls[i].Tag;
      aLabel.Left := Margin.X;
      aLabel.Top := StartTop;
      try
      aLabel.Caption := FItems[i];
      except
        //showMessage('Err');
      end;

      {if there is a checkbox}
      if Assigned(aLabel.FocusControl) and (aLabel.FocusControl is TUCheckBox) then
      begin
        aCheckBox := (aLabel.FocusControl as TUCheckBox);

        //adjust top position
        aCheckBox.Top := StartTop;
        //and left position
        aCheckBox.Left := Margin.X;

        {also adjust left position}
        aLabel.Left := aCheckBox.Left + aCheckBox.Width ;

        {some events}
        aCheckBox.OnEnter := OnCheckBoxEnter;
        aCheckBox.OnExit := OnCheckBoxLeave;
        aCheckBox.OnClick := OnCheckBoxClick;
      end;

      Inc(StartTop,aLabel.Height+Margin.Y);
    end;
  end;

  TUStringList(FItems).OnChange := OnStringListChange;
end;

procedure {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.ReadState(Reader: TReader);
begin
  //read all published values (including Items)
  inherited;
  //now we can show all checkboxes
  FOldItemsCount := FItems.Count;
  UpdateItems;
end;

(*procedure {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.
  SetCheckBoxNames(const Items : TUStrings);
begin
  if not Assigned(Items) then
  begin
    if Assigned(FCheckBoxNames) then
      FCheckBoxNames.Clear
  end
  else
  begin
    FCheckBoxNames.Assign(Items);
  end;
  SetItems(Self.fItems);
end;
*)

procedure {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.SetItems(const Items : TUStrings);
begin
  //remove all old checkboxes and labels
  Remove_CheckBoxes;

  //now there is no more checkbox 
  FOldItemsCount := 0;

  if not Assigned(Items) then
  begin
    if Assigned(FItems) then
      FItems.Clear
  end
  else
  begin
    {we need no event at this time
     we will update manually

     if we did not, there would be
     a difference between FItems and checkboxes
     and an update would create exceptions on calling FItems[i].
     }
    TUStringList(FItems).OnChange := nil;

    FItems.Assign(Items);
    //reset event
    TUStringList(FItems).OnChange := OnStringListChange;

    //be aware that there is a new items count
    FOldItemsCount := FItems.Count;
  end;

  //add new checkboxes and labels depeding on Items
  Update_CheckBoxes;
end;

procedure TCustomCheckScrollBox.SetObjects(Index: Integer; Data: Pointer);
begin
  PItemData(FItems.Objects[Index])^.UserData := Data;
end;

procedure {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.OnStringListChange(Sender: TObject);
begin
  {we dont want to update scrollbox content
   while we are reading published values from stream.}
  if (csReadingState in ControlState) then
    exit;

  {If new items are added or removed we
   easily remove everything and create it from beginning (in UpdateItems)}
  if FOldItemsCount <> FItems.Count then
  begin
    //showMessage('Größ0er kleiner');
    Remove_CheckBoxes;
  end;

  {user has changed a value directly in Items - so we have to update}
  UpdateItems;

  //save new items count
  FOldItemsCount := FItems.Count;

  //showMessage(Format('change: %d',[FItems.Count]));
end;

function TCustomCheckScrollBox.GetCaptions(Index: Integer): String;
begin
  result := FItems[Index];
end;

function {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.GetCheckBoxHint(Index : Integer) : WideString;
begin
  //error detection could be better, i know
   try
    result := (GeTULabelByIndex(Index).FocusControl as TUCheckBox).Hint;
  except
    raise Exception.CreateFmt('Index %d out of bounds.',[Index]);
  end;
end;

procedure TCustomCheckScrollBox.SetCaption(Index: Integer;
  const Caption: String);
begin
  FItems[Index] := Caption;
  Update_CheckBoxes;
end;

procedure {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.SetCheckBoxHint(Index : Integer; Hint : WideString);
begin
  //error detection could be better, i know
   try
    (GeTULabelByIndex(Index).FocusControl as TUCheckBox).ShowHint := true;
    (GeTULabelByIndex(Index).FocusControl as TUCheckBox).Hint := Hint;

    GeTULabelByIndex(Index).ShowHint := true;
    GeTULabelByIndex(Index).Hint := Hint;
  except
    raise Exception.CreateFmt('Index %d out of bounds.',[Index]);
  end;
end;

function {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.GetChecked(Index : Integer) : Boolean;
begin
  //error detection could be better, i know
  try
    result := (GeTULabelByIndex(Index).FocusControl as TUCheckBox).Checked;
  except
    result := false;
  end;
end;


function TCustomCheckScrollBox.GetCount: Integer;
begin
  result := FItems.Count;
end;

function {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.GetCheckBoxIndex(Name : WideString) : Integer;
begin
  //error detection could be better, i know
   try
     result := FCheckBoxNames.IndexOf(Name);
  except
     result := -1;
  end;
end;

procedure {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.SetChecked(Index : Integer; Check : Boolean);
begin
   //error detection could be better, i know
   try
    (GeTULabelByIndex(Index).FocusControl as TUCheckBox).Checked := Check;
  except
    raise Exception.CreateFmt('Index %d out of bounds.',[Index]);
  end;
end;

function {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.GetItemEnabled(Index : Integer) : Boolean;
begin
  //error detection could be better, i know
  try
    result := (GeTULabelByIndex(Index).FocusControl as TUCheckBox).Enabled;
  except
    result := false;
  end;
end;
function TCustomCheckScrollBox.GetObjects(Index: Integer): Pointer;
begin
  result := PItemData(FItems.Objects[Index])^.UserData;
end;

procedure {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.SetItemEnabled(Index : Integer; Enable : Boolean);
begin
   //error detection could be better, i know
   try
    (GeTULabelByIndex(Index).FocusControl as TUCheckBox).Enabled := Enable;
    GeTULabelByIndex(Index).Enabled := Enable;
  except
    raise Exception.CreateFmt('Index %d out of bounds.',[Index]);
  end;
end;

procedure {$IFDEF USE_TNT_PREFIX}TTNTCustomCheckScrollBox{$ELSE}TCustomCheckScrollBox{$ENDIF}.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var oldwidth : Integer;
begin
  oldWidth := Width;
  inherited;

  {
    we only have to update items bounds
    if width is changed

    We also do not wish to update while we are reading from stream.
  }

  if (oldWidth <> AWidth) and not (csReadingState in ControlState) then
  begin
    //if Assigned(FItems) then
    // showMessage(Format('SetBounds change : %d',[FItems.Count]));
    UpdateItems;
  end;
end;

procedure Register;
begin                  
  RegisterComponents('Standard',
  [{$IFDEF USE_TNT_PREFIX}TTNTCheckScrollBox{$ELSE}TCheckScrollBox{$ENDIF}]) ;
end;

end.
