unit DelphiVersionTool;

(******************************************************************************
  Some useful functions for Delphi-Version-Handling
  (immer diese englische Doku tststs)
  written by Michael Groß (July 6th 2008)
*******************************************************************************)

interface
  uses
    Classes;

(*******************************************************************************
  GetInstalledDelphiVersions
  --------------------------
  fills a Stringlist with Names of Installed Delphi-Versions
*******************************************************************************)
  procedure GetInstalledDelphiVersions(var sl:tStringlist);

(*******************************************************************************
  GetInstalledDelphiVersions
  --------------------------
  fills a Stringlist with the Regkeys of Installed Delphi-Versions
  *****************************************************************************)
  procedure GetInstalledDelphiRegKeys(var sl:tStringlist);

(*******************************************************************************
  AddPathToDelphiPath
  --------------------------
  adds a path to the Regkey specified in parameter "aPathName". Possible
  Values for aPathname: "Search Path"    (in german: Bibliothekspfad)
                        "Browsing Path"  (in german: Suchpfad)

  Examples:
    AddPathToDelphiPath('Search Path','c:\MyFolder');        //Bilbiothekspfad
    AddPathToDelphiPath('Browsing Path','$(DELPHI)\Brause'); //Suchpfad

*******************************************************************************)
  procedure AddPathToDelphiPath(aPathName:String;AdditionalPath:String); //Adds a Path

  const
    MAXKnownDelphiVersions=7;

  type
    tKnownDelphiVersions=Array[0..MAXKnownDelphiVersions-1] of Record
      VersionName:String;
      VersionRegKey:String;
      Installed:Boolean;
      Ver : Integer;
    end;

  var
    KnownDelphiVersions:tKnownDelphiVersions=
      (
      (VersionName:'Delphi 5'    ; VersionRegKey:'\Software\Borland\Delphi\5.0' ; Installed:False; Ver:5),
      (VersionName:'Delphi 6'    ; VersionRegKey:'\Software\Borland\Delphi\6.0' ; Installed:False; Ver:6),
      (VersionName:'Delphi 7'    ; VersionRegKey:'\Software\Borland\Delphi\7.0' ; Installed:False; Ver:7),
      (VersionName:'Delphi 8'    ; VersionRegKey:'\Software\Borland\BDS\2.0'    ; Installed:False; Ver:8),
      (VersionName:'Delphi 2005' ; VersionRegKey:'\Software\Borland\BDS\3.0'    ; Installed:False; Ver:9),
      (VersionName:'Delphi 2006' ; VersionRegKey:'\Software\Borland\BDS\4.0'    ; Installed:False; Ver:10),
      (VersionName:'Delphi 2007' ; VersionRegKey:'\Software\Borland\BDS\5.0'    ; Installed:False; Ver:11)
      );

implementation

  uses
    Windows, SysUtils, Registry;



  function FindEntry(aEntry:String):boolean;
    var
      r:tRegistry;
      Value : String;
    begin
      r:=tRegistry.Create;
      try
        r.RootKey:=HKEY_CURRENT_USER;
        result := r.OpenKey(aEntry,false);
        if result then
        begin
          try
            Value := r.ReadString('App');
            result := FileExists(Value);
          except
            result := false;
          end;

        end;
      finally
        r.Free;
      end;
    end;

  procedure InitInstalledDelphiVersions;
    var
      i:Integer;
    begin
      for i:= 0 to MAXKnownDelphiVersions-1 do begin
        KnownDelphiVersions[i].Installed:=FindEntry(KnownDelphiVersions[i].VersionRegKey);
      end;
    end;

  procedure GetInstalledDelphiRegKeys(var sl:tStringlist);
    var
      i:Integer;
    begin
      sl.Clear;
      InitInstalledDelphiVersions;
      for i:= 0 to MAXKnownDelphiVersions-1 do begin
        if KnownDelphiVersions[i].Installed then sl.AddObject(KnownDelphiVersions[i].VersionRegKey, Pointer(KnownDelphiVersions[i].Ver));
      end;
    end;

  procedure GetInstalledDelphiVersions(var sl:tStringlist);
    var
      i:Integer;
    begin
      sl.Clear;
      InitInstalledDelphiVersions;
      for i:= 0 to MAXKnownDelphiVersions-1 do begin
        if KnownDelphiVersions[i].Installed then sl.AddObject(KnownDelphiVersions[i].VersionName, Pointer(KnownDelphiVersions[i].Ver)); 
      end;
    end;

  procedure AddPathToDelphiPath(aPathName:String;AdditionalPath:String);
    var
      r:tRegistry;
      sl:tStringlist;
      i:Integer;
      st:String;

    procedure AddPath(var st:String;aPath:String);
      var
        p:Integer;
      begin
        p:=pos(aPath,st);
        if p>0 then exit;
        st:=st+';'+aPath;
      end;

    begin
      sl:=tStringlist.Create;
      GetInstalledDelphiRegKeys(sl);

      for i:= 0 to sl.Count-1 do begin
        r:=tRegistry.Create;
        r.RootKey:=HKEY_CURRENT_USER;
        if r.OpenKey(sl[i]+'\Library',false) then begin
          try
            st:=r.ReadString(aPathName);
            AddPath(st,AdditionalPath);
            try
              r.WriteString(aPathname,st);
            except
              on e:Exception do raise exception.Create('AddPathToDelphiPath: Cannot WRITE "'+aPathname+'" in Reg-Path "'+sl[i]+#13#10+#13#10+e.Message+#13#10+#13#10+st);
            end;
          except
            on e:Exception do raise exception.Create('AddPathToDelphiPath: Cannot READ "'+aPathname+'" not found in Reg-Path "'+sl[i]+#13#10+#13#10+e.Message);
          end;
        end;
        r.Free;
      end;
      sl.Free;
    end;

end.
