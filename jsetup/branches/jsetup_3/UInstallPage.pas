unit UInstallPage;

interface
uses ActnList, Forms;

type
  IInstallPage = interface
    procedure UpdateNext(Action : TAction);
    procedure UpdatePrevious(Action : TAction);
    procedure BeforeNext(var Allowed : Boolean);
    procedure AfterNext;
    procedure BeforePrevious(var Allowed : Boolean);
    procedure AfterPrevious;
    procedure InitPage(OnNext : Boolean);
  end;

  TInstallPage = class(TFrame, IInstallPage)
  protected
   // fDataModule : TDelphiInstallation;
  public
    {This method is called every time the next button is updated.
    }
    procedure UpdateNext(Action : TAction); virtual;

    {This method is called when the user clicks on previous button
     but before the action is done.
    }
    procedure Previous(var Allowed : Boolean); virtual;

    {This method is called every time the previous button is updated.
    }
    procedure UpdatePrevious(Action : TAction); virtual;

    {This method is called when the user clicks on next button
     but before the action is done.
    }
    procedure BeforeNext(var Allowed : Boolean); virtual;

    {This method is called after the next button has been clicked and
     the parameter Allowed in BeforeNext returned true.
    }
    procedure AfterNext; virtual;

    {This method is called when the user clicks on previous button
     but before the action is done.
    }
    procedure BeforePrevious(var Allowed : Boolean); virtual;

    {This method is called after the previous button has been clicked and
     the parameter Allowed in BeforePrevious returned true.
    }
    procedure AfterPrevious; virtual;

    {This method is called when the page is shown (again) through
    a click on next or previous button.
    @param OnNext If the page is shown because the user clicked on previous button
    the parameter is false; otherwise true.
    }
    procedure InitPage(OnNext : Boolean);virtual;

   // property DataModule : TDelphiInstallation read fDataModule write fDataModule;
  end;

implementation

{ TInstallPage }


{ TInstallPage }

procedure TInstallPage.AfterNext;
begin

end;

procedure TInstallPage.AfterPrevious;
begin

end;

procedure TInstallPage.BeforeNext(var Allowed: Boolean);
begin

end;

procedure TInstallPage.BeforePrevious(var Allowed: Boolean);
begin

end;



procedure TInstallPage.InitPage(OnNext: Boolean);
begin

end;

procedure TInstallPage.Previous(var Allowed: Boolean);
begin

end;

procedure TInstallPage.UpdateNext(Action: TAction);
begin

end;

procedure TInstallPage.UpdatePrevious(Action: TAction);
begin

end;

end.
