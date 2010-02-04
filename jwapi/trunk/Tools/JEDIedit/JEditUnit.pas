unit JEditUnit;

{==============================================================================}

interface

{==============================================================================}

function RunProgram: Integer;

{==============================================================================}

implementation

uses
  Classes, IniFiles, SysUtils;

{==============================================================================}

const
  CharSpace = ' ';
  CharTab = #9;

  FileJEDITeditIni = 'JEDIedit.ini';

  IniSectionDefault = 'Default';

  IniKeyRootPathList = 'RootPaths';
  IniKeyExcludesList = 'Excludes';
  IniKeyIncludesList = 'Includes';
  IniKeyExtensonList = 'Extensions';
  IniKeySkipDirsList = 'SkipDirs';
  IniKeyTabReplace = 'TabReplace';
  IniKeyTabSpacing = 'TabSpacing';

  IniKeyRootPathListDefault = '"..\jedi-apilib","..\..\..\..\..\jedi-apilib"';
  IniKeyExcludesListDefault = '"jwapi\trunk\Examples","jwscl\trunk\examples","jwscl\trunk\unittests"';
  IniKeyIncludesListDefault = '"jwapi\trunk","jwscl\trunk"';
  IniKeyExtensonListDefault = '"dpr","inc","pas"';
  IniKeySkipDirsDefault = '"CVS",".svn"';
  IniKeyTabReplaceDefault = True;
  IniKeyTabSpacingDefault = 2;

  MessageInit = 'Greetings Earthling! Take me to your Leader.';
  MessageFini = 'Files: count %d, trim %d, tabs %d.';

{==============================================================================}

function RightTrim( var Trimmed: Boolean; const Arg: string ): string;

  var
    Index, Limit: Integer;

begin
  Result := Arg;

  Index := Length( Arg );
  Limit := Index;
  while ( Index > 0 ) and ( Result[ Index ] <= ' ' ) do Dec( Index );

  Trimmed := Index < Limit;
  if Trimmed then SetLength( Result, Index );
end;

{==============================================================================}

function ReplaceTabs( var Replaced: Boolean;
                      const Arg: string;
                      const TabSpacing: Integer
                      ): string;

  var
    Index: Integer;
    Source: string;

begin
  Replaced := False;
  Result := '';
  Source := Arg;

  Index := Pos( CharTab, Source );
  while Index > 0 do
  begin
    Result := Result + Copy( Source, 1, Index - 1 );
    Delete( Source, 1, Index );

    Result := Result
            +  StringOfChar(  CharSpace,
                              TabSpacing - Length( Result ) mod TabSpacing
                              )
            ;

    Replaced := True;

    Index := Pos( CharTab, Source );
  end;

  Result := Result + Source;
end;

{==============================================================================}

type
  TFileEdit = class( TStringList )
  public
    function EditTabs(  const FileName: string;
                        const TabSpacing: Integer
                        ): Boolean;
    function EditWhiteSpace( const FileName: string ): Boolean;
    function FindTabs( const FileName: string ): string;

  end;

{------------------------------------------------------------------------------}

type
  TFileList = class( TStringList )
  public
    procedure AddFiles( const Path: string;
                        const Recursive: Boolean;
                        const SkipDirsList: TStringList
                        );
  end;

{==============================================================================}

function TFileEdit.EditTabs(  const FileName: string;
                              const TabSpacing: Integer
                              ): Boolean;

  var
    Index: Integer;
    Replaced: Boolean;

begin
  Result := False;
  try
    try
      LoadFromFile( FileName );

      // Edit all lines containing tabs
      Index := 0;
      while Index < Count do
      begin
        Strings[ Index ] := ReplaceTabs( Replaced, Strings[ Index ], TabSpacing );
        Result := Result or Replaced;
        Inc( Index );
      end;

      if Result then
      begin
        SaveToFile( FileName );
      end;

    except
    end;
  finally
    Clear;
  end;
end;

{------------------------------------------------------------------------------}

function TFileEdit.EditWhiteSpace( const FileName: string ): Boolean;

  var
    Index: Integer;
    Trimmed: Boolean;

begin
  Result := False;
  try
    try
      LoadFromFile( FileName );

      // Right Trim all lines
      Index := 0;
      while Index < Count do
      begin
        Strings[ Index ] := RightTrim( Trimmed, Strings[ Index ] );
        Result := Result or Trimmed;
        Inc( Index );
      end;

      // Discard leading blank lines
      Index := 0;
      while ( Index < Count ) and ( Length( Strings[ Index ] ) = 0 ) do
      begin
        Delete( Index );
        Result := True;
        // Delete( Index ) decrements Count; omit Inc( Index );
      end;

      // Discard trailing blank lines
      Index := Count - 1;
      while ( Index >= 0 ) and ( Length( Strings[ Index ] ) = 0 ) do
      begin
        Delete( Index );
        Result := True;
        Dec( Index );
      end;

      if Result then
      begin
        SaveToFile( FileName );
      end;

    except
    end;
  finally
    Clear;
  end;
end;

{------------------------------------------------------------------------------}

function TFileEdit.FindTabs( const FileName: string ): string;

  var
    Index: Integer;

begin
  Result := '';
  try
    try
      LoadFromFile( FileName );

      // Find all lines containing a tab
      Index := 0;
      while Index < Count do
      begin
        if Pos( CharTab, Strings[ Index ] ) > 0 then
        begin
          Result := Result + IntToStr( Index + 1 ) + ',';
        end;

        Inc( Index );
      end;

      // Discard trailing comma (if any)
      Index := Length( Result );
      if Index > 0 then
      begin
        SetLength( Result, Index - 1 );
      end;

    except
    end;
  finally
    Clear;
  end;
end;

{==============================================================================}
{
type
  TSearchRec = record
    Time: Integer;
    Size: Integer;
    Attr: Integer;
    Name: TFileName;
    ExcludeAttr: Integer;
    FindHandle: THandle;
    FindData: TWin32FindData;
  end;
}

{$WARN SYMBOL_PLATFORM OFF}

procedure TFileList.AddFiles( const Path: string;
                              const Recursive: Boolean;
                              const SkipDirsList: TStringList
                              );

  const
    PathSep = '\';
    AnyFile = PathSep + '*.*';

  var
    SearchRec: TSearchRec;

begin
  if FindFirst( Path + AnyFile, faAnyFile, SearchRec ) = 0 then
  begin
    repeat
      if ( ( SearchRec.Attr and faDirectory ) <> 0 ) and Recursive then
      begin
        if ( SkipDirsList.IndexOf( SearchRec.Name ) < 0 ) then
        begin
          AddFiles( Path + PathSep + SearchRec.Name, Recursive, SkipDirsList );
        end;
      end
      else
      if ( SearchRec.Attr and faVolumeID ) <> 0 then
      begin
        // Ignore Volume Labels
      end
      else
      begin
        Add( Path + PathSep + SearchRec.Name );
      end;

    until FindNext( SearchRec ) <> 0;

    FindClose( SearchRec );
  end;
end;

{$WARN SYMBOL_PLATFORM ON}

{==============================================================================}

function RunProgram: Integer;

  var
    FileEdit: TFileEdit;
    FileList: TFileList;
    RootPathList,
    ExcludesList,
    IncludesList,
    ExtensonList,
    SkipDirsList: TStringList;
    FileExtn, FileName, RootPath, TabLines: string;
    Compare, EditedTabs, EditedTrim, Index, Jadex, TabSpacing: Integer;
    IniFile: TMemIniFile;
    SaveIniFile, TabReplace: Boolean;

  procedure InitializeStringList( StringList: TStringList );
  begin
    StringList.Duplicates := dupIgnore;
    StringList.CaseSensitive := False;
    StringList.Sorted := True;
  end;

begin
  Result := 0;

  FileEdit := TFileEdit.Create;
  FileList := TFileList.Create;
  RootPathList := TStringList.Create;
  ExcludesList := TStringList.Create;
  IncludesList := TStringList.Create;
  ExtensonList := TStringList.Create;
  SkipDirsList := TStringList.Create;
  IniFile := TMemIniFile.Create( FileJEDITeditIni );
  try
    WriteLn( MessageInit );

    EditedTabs := 0;
    EditedTrim := 0;

    // Prepare IniFile
    SaveIniFile := False;
    if not IniFile.ValueExists( IniSectionDefault, IniKeyRootPathList ) then
    begin
      IniFile.WriteString(  IniSectionDefault,
                            IniKeyRootPathList,
                            IniKeyRootPathListDefault
                            );
      SaveIniFile := True;
    end;
    if not IniFile.ValueExists( IniSectionDefault, IniKeyExtensonList ) then
    begin
      IniFile.WriteString(  IniSectionDefault,
                            IniKeyExtensonList,
                            IniKeyExtensonListDefault
                            );
      SaveIniFile := True;
    end;
    if not IniFile.ValueExists( IniSectionDefault, IniKeyIncludesList ) then
    begin
      IniFile.WriteString(  IniSectionDefault,
                            IniKeyIncludesList,
                            IniKeyIncludesListDefault
                            );
      SaveIniFile := True;
    end;
    if not IniFile.ValueExists( IniSectionDefault, IniKeyExcludesList ) then
    begin
      IniFile.WriteString(  IniSectionDefault,
                            IniKeyExcludesList,
                            IniKeyExcludesListDefault
                            );
      SaveIniFile := True;
    end;
    if not IniFile.ValueExists( IniSectionDefault, IniKeySkipDirsList ) then
    begin
      IniFile.WriteString(  IniSectionDefault,
                            IniKeySkipDirsList,
                            IniKeySkipDirsDefault
                            );
      SaveIniFile := True;
    end;
    if not IniFile.ValueExists( IniSectionDefault, IniKeyTabReplace ) then
    begin
      IniFile.WriteBool(  IniSectionDefault,
                          IniKeyTabReplace,
                          IniKeyTabReplaceDefault
                          );
      SaveIniFile := True;
    end;
    if not IniFile.ValueExists( IniSectionDefault, IniKeyTabSpacing ) then
    begin
      IniFile.WriteInteger( IniSectionDefault,
                            IniKeyTabSpacing,
                            IniKeyTabSpacingDefault
                            );
      SaveIniFile := True;
    end;
    if SaveIniFile then IniFile.UpdateFile;

    // Prepare FileList
    InitializeStringList( FileList );

    // Prepare RootPathList
    RootPathList.DelimitedText := IniFile.ReadString( IniSectionDefault,
                                                      IniKeyRootPathList,
                                                      IniKeyRootPathListDefault
                                                      );

    // Prepare ExcludesList
    InitializeStringList( ExcludesList );
    ExcludesList.DelimitedText := IniFile.ReadString( IniSectionDefault,
                                                      IniKeyExcludesList,
                                                      IniKeyExcludesListDefault
                                                      );

    // Prepare IncludesList
    InitializeStringList( IncludesList );
    IncludesList.DelimitedText := IniFile.ReadString( IniSectionDefault,
                                                      IniKeyIncludesList,
                                                      IniKeyIncludesListDefault
                                                      );

    // Prepare ExtensonList
    InitializeStringList( ExtensonList );
    ExtensonList.DelimitedText := IniFile.ReadString( IniSectionDefault,
                                                      IniKeyExtensonList,
                                                      IniKeyExtensonListDefault
                                                      );

    // Prepare SkipDirsList
    InitializeStringList( SkipDirsList );
    SkipDirsList.DelimitedText := IniFile.ReadString( IniSectionDefault,
                                                      IniKeySkipDirsList,
                                                      IniKeySkipDirsDefault
                                                      );
    SkipDirsList.Add( '.' );
    SkipDirsList.Add( '..' );

    // Prepare TabReplace and TabSpacing
    TabReplace := IniFile.ReadBool( IniSectionDefault,
                                    IniKeyTabReplace,
                                    IniKeyTabReplaceDefault
                                    );
    TabSpacing := IniFile.ReadInteger(  IniSectionDefault,
                                        IniKeyTabSpacing,
                                        IniKeyTabSpacingDefault
                                        );

    // Locate first valid RootPathList entry
    RootPath := ''; Index := 0;
    while ( Length( RootPath ) <= 0 ) and ( Index < RootPathList.Count ) do
    begin
      FileName := RootPathList.Strings[ Index ];
      if DirectoryExists( FileName ) then
      begin
        RootPath := FileName;
      end;

      Inc( Index );
    end;

    // Enumerate valid RootPathList entry
    if DirectoryExists( RootPath ) then
    begin
      RootPath := IncludeTrailingPathDelimiter( RootPath );

      // Enumerate IncludesList entries
      for Index := 0 to IncludesList.Count - 1 do
      begin
        FileList.AddFiles( RootPath + IncludesList.Strings[ Index ], True, SkipDirsList );
      end;

      // Remove ExcludesList entries
      Index := 0; Jadex := 0;
      while ( Index < ExcludesList.Count ) and ( Jadex < FileList.Count ) do
      begin
        FileName := IncludeTrailingPathDelimiter( RootPath + ExcludesList.Strings[ Index ] );

        Compare := CompareText( FileName, Copy( FileList.Strings[ Jadex ], 1, Length( FileName ) ) );

        if Compare < 0 then
        begin
          // ExcludesList < FileList
          Inc( Index );
        end
        else
        if Compare > 0 then
        begin
          // ExcludesList > FileList
          Inc( Jadex );
        end
        else
        begin
          // ExcludesList = FileList
          FileList.Delete( Jadex );
        end;
      end;

      // Keep ExtensonList entries
      Index := 0;
      while Index < FileList.Count do
      begin
        FileExtn := Copy( ExtractFileExt( FileList.Strings[ Index ] ), 2, MaxInt );

        if ExtensonList.IndexOf( FileExtn ) >= 0 then
        begin
          // FileExtn present, retain
          Inc( Index );
        end
        else
        begin
          // FileExtn absent, discard
          FileList.Delete( Index );
        end;
      end;
    end;

    // Remove extra white space
    for Index := 0 to FileList.Count - 1 do
    begin
      FileName := FileList.Strings[ Index ];

      if FileEdit.EditWhiteSpace( FileName ) then
      begin
        WriteLn( 'Edit: ', FileName );
        Inc( EditedTrim );
      end;
    end;

    if TabReplace then
    begin
      // Replace tabs
      for Index := 0 to FileList.Count - 1 do
      begin
        FileName := FileList.Strings[ Index ];

        if FileEdit.EditTabs( FileName, TabSpacing ) then
        begin
          WriteLn( 'Tabs: ', FileName );
          Inc( EditedTabs );
        end;
      end;
    end
    else
    begin
      // Report where tabs exist
      for Index := 0 to FileList.Count - 1 do
      begin
        FileName := FileList.Strings[ Index ];
        TabLines := FileEdit.FindTabs( FileName );

        if Length( TabLines ) > 0 then
        begin
          WriteLn( 'Tabs: ', FileName, ', lines ', TabLines );
          Inc( EditedTabs );
        end;
      end;
    end;

    WriteLn( Format( MessageFini, [ FileList.Count, EditedTrim, EditedTabs ] ) );

  finally
    FreeAndNil( IniFile );
    FreeAndNil( SkipDirsList );
    FreeAndNil( ExtensonList );
    FreeAndNil( IncludesList );
    FreeAndNil( ExcludesList );
    FreeAndNil( RootPathList );
    FreeAndNil( FileList );
    FreeAndNil( FileEdit );
  end;
end;

{==============================================================================}

end.
