unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    function  ResLoader(RSectionsName : PChar; RName : String) : TBitmap;
    procedure ExecuteBlending;
  public
    { Public declarations }
  protected
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{$R Logo.res}

function TForm1.ResLoader(RSectionsName : PChar; RName : String) : TBitmap;
  var
    Stream: TResourceStream;
  begin
    Stream := TResourceStream.Create(hInstance, RName, PChar(RSectionsName));
    try
      result := TBITMAP.Create;
      Result.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;

procedure TForm1.ExecuteBlending;
var
  BlendFunction: TBlendFunction;
  BitmapPos: TPoint;
  BitmapSize: TSize;
  exStyle: DWORD;
  Bitmap: TBitmap;
  Bit2  : TBitmap;
begin
  // Enable window layering
  exStyle := GetWindowLongA(Handle, GWL_EXSTYLE);
  if (exStyle and WS_EX_LAYERED = 0) then
    SetWindowLong(Handle, GWL_EXSTYLE, exStyle or WS_EX_LAYERED);

  Bitmap := TBitmap.Create;
  Bit2 := ResLoader(PChar('DIB')
  try
    Bitmap.Assign(Bit2, PChar('JEDILOGO')));

    ASSERT(Bitmap.PixelFormat = pf32bit, 'Wrong bitmap format - must be 32 bits/pixel');

    // Resize form to fit bitmap
    ClientWidth := Bitmap.Width;
    ClientHeight := Bitmap.Height;

    // Position bitmap on form
    BitmapPos := Point(0, 0);
    BitmapSize.cx := Bitmap.Width;
    BitmapSize.cy := Bitmap.Height;

    // Setup alpha blending parameters
    BlendFunction.BlendOp := AC_SRC_OVER;
    BlendFunction.BlendFlags := 0;
    BlendFunction.SourceConstantAlpha := 255;
    BlendFunction.AlphaFormat := AC_SRC_ALPHA;

    // ... and action!
    UpdateLayeredWindow(Handle, 0, nil, @BitmapSize, Bitmap.Canvas.Handle,
      @BitmapPos, 0, @BlendFunction, ULW_ALPHA);
    Show;

    // Setzt Fenster an die vorderste Front
    SetWindowPos(Handle, HWND_TOPMOST, Left, Top, Width, Height,
    SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);

    // Setzt Parent auf den Desktop
    SetWindowLong(Handle, GWL_HWNDPARENT, 0);

    // Versteckt das Fenster in der Taskleiste
    SetWindowLong(Handle, GWL_EXSTYLE,
    GetWindowLong(Handle, GWL_EXSTYLE) or
    WS_EX_TOOLWINDOW and not WS_EX_APPWINDOW);      

  finally
    Bitmap.Free;
    Bit2.Free;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  ExecuteBlending;
end;

end.
