(*
This demonstration computes and dumps the md5 hash of a given string.
*)
program SimpleMD5;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  JwsclCryptProvider,
  JwaWindows,
  JwsclTypes;

procedure Dump(Loc: Pointer; Len: Cardinal);
var i: Integer;
begin
  for i := 1 to Len do
  begin
    Write(IntToHex(PByte(Loc)^, 2));
    Inc(PByte(Loc), 1);
  end;
end;

var Hash: TJwHash; Data: String; HashVal: PByte; Size: Cardinal;
begin
  Hash:= TJwHash.Create(haMD5);
  try
    Readln(Data);
    Hash.HashData(PChar(Data)^, Length(Data));
    Size := Hash.GetHashLength;
    GetMem(HashVal, Size);
    try
      Hash.RetrieveHash(HashVal^, Size);
      Dump(HashVal, Size);
    finally
      FreeMem(HashVal);
    end;
  finally
    Hash.Free;
  end;
  Readln;
end.
