unit CredentialUtils;

interface
uses JwaWindows, Forms, Graphics;

{Copies the current desktop in a bitmap and highlights the given Window.
Also returns the used Resolution.
}
function GetScreenBitmap(const HilightWindow : HWND; out Resolution : TRect) : TBitmap;
function GetMaxResolution : TRect;
function AlphaBlendImage(const Image, Mask, Background : TBitmap; const BX,BY : Integer) : TBitmap;

implementation
uses JwsclVersion;

function GetMaxResolution : TRect;
var i : Integer;
begin
  ZeroMemory(@result, sizeof(result));
  for i := 0 to Screen.MonitorCount -1 do
  begin
    if Screen.Monitors[i].BoundsRect.Left < result.Left then
      result.Left := Screen.Monitors[i].BoundsRect.Left;
    if Screen.Monitors[i].BoundsRect.Top < result.Top then
      result.Top := Screen.Monitors[i].BoundsRect.Top;

    if Screen.Monitors[i].BoundsRect.Right > result.Right then
      result.Right := Screen.Monitors[i].BoundsRect.Right;
    if Screen.Monitors[i].BoundsRect.Bottom > result.Bottom then
      result.Bottom := Screen.Monitors[i].BoundsRect.Bottom;
  end;
end;

function GetScreenBitmap(const HilightWindow : HWND; out Resolution : TRect) : TBitmap;

  function BitBltBlack(DestDC: HDC; DestX, DestY : Integer; nWidth, nHeight: integer; SrcDC: HDC; SrcX, SrcY: integer; Black: byte): LongBool;
  var BlendStruct: BLENDFUNCTION;
  begin
    BitBlt(DestDC, DestX, DestY, nWidth, nHeight, 0, SrcX, SrcY, BLACKNESS);
    BlendStruct.BlendFlags:=0;
    BlendStruct.BlendOp:=AC_SRC_OVER;
    BlendStruct.SourceConstantAlpha:=255-Black;
    BlendStruct.AlphaFormat:=0;
    Result:= jwaWindows.AlphaBlend(DestDC, DestX, DestY, nWidth, nHeight, SrcDC, SrcX, SrcY, NWidth, nHeight, BlendStruct);
  end;



var i : Integer;
    SrcR,DestR,
    R,R2 : TRect;
    DeskDC : HDC;
    WndBitB,
    WndBit : TBitmap;
    C : TCanvas;
    Wnd : THandle;
begin
  Resolution := GetMaxResolution;

  Wnd := HilightWindow;
  if Wnd <> 0 then
  begin
    //GetClientRect(Wnd, SrcR);
    GetWindowRect(Wnd,SrcR);
  {  BringWindowToTop(Wnd);
    SetForegroundWindow(Wnd);
    Sleep(100);     }
  end;

  C := TCanvas.Create;
  C.Handle := GetDC(0);

  try
    if Wnd <> 0 then
    begin
      WndBit := Graphics.TBitmap.Create;

      SetRect(DestR,0,0,
        abs(SrcR.Right-SrcR.Left),abs(SrcR.Bottom - SrcR.Top));
                                                          
      WndBit.Width := DestR.Right;
      WndBit.Height := DestR.Bottom;

      //WndBit.Canvas.CopyRect(DestR,C,SrcR);
      //print the windows content into the bitmap
      try
        if not PrintWindow(Wnd, WndBit.Canvas.Handle, 0) then
        begin
          //we ignore the window on errors
          Wnd := 0;
          Wndbit.Free;
        end;
      except
      end;
      // WndBit.SaveToFile('E:\Temp\_Bmp2.bmp');
    end;

    result := Graphics.TBitmap.Create;
    result.Width := abs(Resolution.Left) + abs(Resolution.Right);
    result.Height:= abs(Resolution.Top) + abs(Resolution.Bottom);
    BitBltBlack(result.Canvas.Handle, 0, 0, result.Width, result.Height, C.Handle, Resolution.Left, Resolution.Top, 200);

    if Wnd <> 0 then
    begin
      WndBitB := Graphics.TBitmap.Create;
      WndBitB.Width := WndBit.Width;
      WndBitB.Height := WndBit.Height;

      BitBltBlack(WndBitB.Canvas.Handle, 0, 0,
          WndBit.Width, WndBit.Height, WndBit.Canvas.Handle,
            0,0, 120);

      //WndBitB.SaveToFile('E:\Temp\_Bmp3.bmp');

      result.Canvas.CopyRect(SrcR, WndBitB.Canvas, DestR);
      WndBit.Free;
      WndBitB.Free;
    end;
  finally
    C.Free;
  end;

  Resolution.Right  := abs(Resolution.Left) + abs(Resolution.Right);
  Resolution.Bottom := abs(Resolution.Top) + abs(Resolution.Bottom);
end;


function AlphaBlendImage(const Image, Mask, Background : TBitmap; const BX,BY : Integer) : TBitmap;
Type
  PBGR = ^TBGR;

  TBGR = Record
    Blue: Byte;
    Green: Byte;
    red: Byte;
  End;

Var
  i, j: Integer;
  backP, origP, MaskP: PBGR;
Begin
  Mask.pixelformat := pf24bit;
  Image.pixelformat := pf24bit;
  Background.pixelformat := pf24bit;

  result := TBitmap.Create;
  result.Width := Image.Width;
  result.Height := Image.Height;
  result.pixelformat := pf24bit;
  result.Canvas.Draw(-BX,-BY, Background);
  (*
  Dieser Algorithmus erzeugt fehler wenn size Image <> Size Mask
  *)
    For j := 0 To Image.height - 1 Do Begin
    OrigP := Image.ScanLine[j];
    MaskP := Mask.ScanLine[j];
    For i := 0 To Image.width - 1 Do Begin
      (*
      Diese If Verhindert das wenn Image > als Backorund ist es keine AV's gibt.
      *)
      If (i < result.width) And (j < result.Height) Then Begin
        backP := result.ScanLine[j];
        inc(backp, i);
        (*
        Back = Alpha * Orig + 1-Alpha *Back
        *)
        backp^.Blue := round((1 - MaskP^.Blue / 255) * OrigP^.Blue + (MaskP^.Blue / 255) * backP^.Blue);
        backp^.Green := round((1 - MaskP^.Green / 255) * OrigP^.Green + (MaskP^.Green / 255) * backP^.Green);
        backp^.red := round((1 - MaskP^.red / 255) * OrigP^.red + (MaskP^.red / 255) * backP^.red);
      End;
      inc(OrigP);
      inc(maskP);
    End;
  End;   
 { result.SaveToFile('E:\Temp\jwsclbutton.bmp');
  Image.SaveToFile('E:\Temp\jwsclbutton_image.bmp');
  Mask.SaveToFile('E:\Temp\jwsclbutton_MAsk.bmp');
  Background.SaveToFile('E:\Temp\jwsclbutton_Back.bmp');     }
End;

end.
