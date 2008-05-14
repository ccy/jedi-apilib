unit CredentialUtils;

interface
uses JwaWindows, Forms, Graphics;

function GetScreenBitmap(out Resolution : TRect) : TBitmap;
function GetMaxResolution : TRect;

implementation


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

function GetScreenBitmap(out Resolution : TRect) : TBitmap;

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

  Wnd := GetForegroundWindow;
  //Wnd := Getwindow
  //Wnd := GetActiveWindow;
  if Wnd <> 0 then
  begin
    //GetClientRect(Wnd, SrcR);
    GetWindowRect(Wnd,SrcR);
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

      WndBit.Canvas.CopyRect(DestR,C,SrcR);
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

     // WndBitB.SaveToFile('E:\Temp\_Bmp3.bmp');

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

end.
