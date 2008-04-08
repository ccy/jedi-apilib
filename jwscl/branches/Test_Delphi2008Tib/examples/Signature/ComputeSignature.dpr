{This program computes the signature if a given string. This
signature and the public key necessary for verification are then
stored in two files. These files are used by the example
VerifySignature.}
program ComputeSignature;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  JwsclCryptProvider,
  JwsclTypes;

//The files are put into the current directory. If you do not have
//the necessary rights to write, you have to change the paths.
const
  SignatureFile = 'Signature.sig';
  PublicKeyFile = 'PublicKey.pbk';

var Provider: TJwCryptProvider; Key: TJwCryptKey; Hash: TJwHash;
    Data: string; Len: Cardinal; Blob: Pointer; FS: TFileStream;
begin
  //If the default key container has not yet been created, specify
  //[ccfNewKeyset] as the last parameter
  Provider := TJwCryptProvider.Create('', '', ctRsaFull, []);
  try
    Hash := TJwHash.Create(haSHA, Provider);
    try
      Writeln('Enter a string! Its signature will be stored in '+SignatureFile+'.');
      Readln(Data);
      Hash.HashData(Pointer(Data), Length(Data));
      Blob := Hash.Sign(Len, kptSignature);
      try
        FS := TFileStream.Create(SignatureFile, fmCreate);
        try
          FS.WriteBuffer(Blob^, Len);
        finally
          FS.Free;
        end;
      finally
        Hash.FreeBuffer(Blob);
      end;
      Writeln('Signature successfully stored');
    finally
      Hash.Free;
    end;

    Key := TJwCryptKey.GetUserKey(Provider, kptSignature);
    try
      Blob := Key.ExportKey(nil, kekPublic, [], Len);
      try
        FS := TFileStream.Create(PublicKeyFile, fmCreate);
        try
          FS.WriteBuffer(Blob^, Len);
        finally
          FS.Free;
        end;
        Writeln('Public key for signature decryption stored in '+PublicKeyFile+'.');
      finally
        Key.FreeBuffer(Blob);
      end;
    finally
      Key.Free;
    end;
  finally
    Provider.Free;
    Readln;
  end;
end.
