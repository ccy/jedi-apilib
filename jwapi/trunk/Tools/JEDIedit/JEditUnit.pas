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
  IniKeyTabReports = 'TabReports';
  IniKeyTabSpacing = 'TabSpacing';

  IniKeyRootPathListDefault = '"..\jedi-apilib","..\..\..\..\..\jedi-apilib"';
  IniKeyExcludesListDefault = '"jwapi\trunk\Examples","jwscl\trunk\examples","jwscl\trunk\unittests"';
  IniKeyIncludesListDefault = '"jwapi\trunk","jwscl\trunk"';
  IniKeyExtensonListDefault = '"dpr","inc","pas"';
  IniKeySkipDirsDefault = '"CVS",".svn"';
  IniKeyTabReplaceDefault = True;
  IniKeyTabReportsDefault = not IniKeyTabReplaceDefault;
  IniKeyTabSpacingDefault = 2;

  MessageInit = 'Greetings Earthling! Take me to your Leader.';
  MessageFile = 'File not found: %s.';
  MessageFini = 'Files: count %d, trim %d, tabs %d %d.';

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

{------------------------------------------------------------------------------}

type
  TProgramOptionId =
    (
      optDirectories,
      optExtensions,
      optExtensionsError,
      optIniFileName,
      optIniFileNameError,
      optIniFileSection,
      optIniFileSectionError,
      optRootPaths,
      optRootPathsError,
      optTabReplace,
      optTabReports,
      optTabSpacing,
      optTabSpacingError
    );

  TProgramOptionUsed = set of TProgramOptionId;

  TProgramOptions = record
    Recursive,
    TabReplace,
    TabReports: Boolean;

    TabSpacing: Integer;

    RootPaths,
    Extensions,
    IniFileName,
    IniSectionName: string;

    FilesPaths: array of string;

    OptionUsed: TProgramOptionUsed;
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
    Argument, FileExtn, FileName, RootPath, TabLines: string;
    Compare, EditedTabs, EditedTrim, FinderTabs, Index, Jadex: Integer;
    IniFile: TMemIniFile;
    SaveIniFile: Boolean;
    Option: TProgramOptions;

  procedure InitializeStringList( StringList: TStringList );
  begin
    StringList.Duplicates := dupIgnore;
    StringList.CaseSensitive := False;
    StringList.Sorted := True;
  end;

begin
  Result := 0;

  WriteLn( MessageInit );

  {----------------------------------------------------------------------------}

  Option.Recursive := False;
  Option.TabReplace := IniKeyTabReplaceDefault;
  Option.TabReports := IniKeyTabReportsDefault;
  Option.TabSpacing := IniKeyTabSpacingDefault;
  Option.RootPaths := IniKeyRootPathListDefault;
  Option.Extensions := IniKeyExtensonListDefault;
  Option.IniFileName := FileJEDITeditIni;
  Option.IniSectionName := IniSectionDefault;

  // Command line arguments
  Index := 0; Jadex := 0;
  while Index < ParamCount do
  begin
    Inc( Index );
    Argument := ParamStr( Index );

    if ( CompareText( Argument, '--directories' ) = 0 )
    or ( CompareText( Argument, '-d' ) = 0 )
    or ( CompareText( Argument, '/d' ) = 0 ) then
    begin
      Option.OptionUsed := Option.OptionUsed + [ optDirectories ];
      Option.Recursive := True;
    end
    else
    if ( CompareText( Argument, '--extensions' ) = 0 )
    or ( CompareText( Argument, '-e' ) = 0 )
    or ( CompareText( Argument, '/e' ) = 0 ) then
    begin
      Inc( Index );
      if Index < ParamCount then
      begin
        Option.OptionUsed := Option.OptionUsed + [ optExtensions ];
        Option.Extensions := ParamStr( Index );
      end
      else
      begin
        Option.OptionUsed := Option.OptionUsed + [ optExtensionsError ];
      end;
    end
    else
    if ( CompareText( Argument, '--ini-file' ) = 0 )
    or ( CompareText( Argument, '-i' ) = 0 )
    or ( CompareText( Argument, '/i' ) = 0 ) then
    begin
      Inc( Index );
      if Index < ParamCount then
      begin
        Option.OptionUsed := Option.OptionUsed + [ optIniFileName ];
        Option.IniFileName := ParamStr( Index );
      end
      else
      begin
        Option.OptionUsed := Option.OptionUsed + [ optIniFileNameError ];
      end;
    end
    else
    if ( CompareText( Argument, '--ini-section' ) = 0 )
    or ( CompareText( Argument, '-j' ) = 0 )
    or ( CompareText( Argument, '/j' ) = 0 ) then
    begin
      Inc( Index );
      if Index < ParamCount then
      begin
        Option.OptionUsed := Option.OptionUsed + [ optIniFileSection ];
        Option.IniSectionName := ParamStr( Index );
      end
      else
      begin
        Option.OptionUsed := Option.OptionUsed + [ optIniFileSectionError ];
      end;
    end
    else
    if ( CompareText( Argument, '--root-paths' ) = 0 )
    or ( CompareText( Argument, '-r' ) = 0 )
    or ( CompareText( Argument, '/r' ) = 0 ) then
    begin
      Inc( Index );
      if Index < ParamCount then
      begin
        Option.OptionUsed := Option.OptionUsed + [ optRootPaths ];
        Option.RootPaths := ParamStr( Index );
      end
      else
      begin
        Option.OptionUsed := Option.OptionUsed + [ optRootPathsError ];
      end;
    end
    else
    if ( CompareText( Argument, '--tab-replace' ) = 0 )
    or ( CompareText( Argument, '-t' ) = 0 )
    or ( CompareText( Argument, '/t' ) = 0 ) then
    begin
      Option.OptionUsed := Option.OptionUsed + [ optTabReplace ];
      Option.TabReplace := True;
    end
    else
    if ( CompareText( Argument, '--tab-report' ) = 0 )
    or ( CompareText( Argument, '-u' ) = 0 )
    or ( CompareText( Argument, '/u' ) = 0 ) then
    begin
      Option.OptionUsed := Option.OptionUsed + [ optTabReports ];
      Option.TabReports := True;
    end
    else
    if ( CompareText( Argument, '--tab-spacing' ) = 0 )
    or ( CompareText( Argument, '-v' ) = 0 )
    or ( CompareText( Argument, '/v' ) = 0 ) then
    begin
      Inc( Index );
      if Index < ParamCount then
      begin
        Option.TabSpacing := StrToIntDef( ParamStr( Index ), 0 );
        if Option.TabSpacing > 0 then
        begin
          Option.OptionUsed := Option.OptionUsed + [ optTabSpacing ];
        end
        else
        begin
          Option.OptionUsed := Option.OptionUsed + [ optTabSpacingError ];
        end;
      end
      else
      begin
        Option.OptionUsed := Option.OptionUsed + [ optTabSpacingError ];
      end;
    end
    else
    if DirectoryExists( Argument ) or FileExists( Argument ) then
    begin
      SetLength( Option.FilesPaths, Jadex + 1 );
      Option.FilesPaths[ Jadex ] := Argument;
      Inc( Jadex );
    end
    else
    begin
      WriteLn( Format( MessageFile, [ Argument ] ) );
    end;
  end;
  if optExtensionsError     in Option.OptionUsed then WriteLn( 'optExtensionsError' );
  if optIniFileNameError    in Option.OptionUsed then WriteLn( 'optIniFileNameError' );
  if optIniFileSectionError in Option.OptionUsed then WriteLn( 'optIniFileSectionError' );
  if optRootPathsError      in Option.OptionUsed then WriteLn( 'optRootPathsError' );
  if optTabSpacingError     in Option.OptionUsed then WriteLn( 'optTabSpacingError' );
  if  [ optExtensionsError,
        optIniFileNameError,
        optIniFileSectionError,
        optRootPathsError,
        optTabSpacingError
      ] * Option.OptionUsed <> [ ] then Exit;

  {----------------------------------------------------------------------------}

  FileEdit := TFileEdit.Create;
  FileList := TFileList.Create;
  RootPathList := TStringList.Create;
  ExcludesList := TStringList.Create;
  IncludesList := TStringList.Create;
  ExtensonList := TStringList.Create;
  SkipDirsList := TStringList.Create;
  IniFile := TMemIniFile.Create( Option.IniFileName );

  {----------------------------------------------------------------------------}

  try
    {--------------------------------------------------------------------------}

    // Prepare IniFile
    SaveIniFile := False;
    if not IniFile.ValueExists( Option.IniSectionName, IniKeyRootPathList ) then
    begin
      IniFile.WriteString(  Option.IniSectionName,
                            IniKeyRootPathList,
                            Option.RootPaths
                            );
      SaveIniFile := True;
    end;
    if not IniFile.ValueExists( Option.IniSectionName, IniKeyExtensonList ) then
    begin
      IniFile.WriteString(  Option.IniSectionName,
                            IniKeyExtensonList,
                            Option.Extensions
                            );
      SaveIniFile := True;
    end;
    if not IniFile.ValueExists( Option.IniSectionName, IniKeyIncludesList ) then
    begin
      IniFile.WriteString(  Option.IniSectionName,
                            IniKeyIncludesList,
                            IniKeyIncludesListDefault
                            );
      SaveIniFile := True;
    end;
    if not IniFile.ValueExists( Option.IniSectionName, IniKeyExcludesList ) then
    begin
      IniFile.WriteString(  Option.IniSectionName,
                            IniKeyExcludesList,
                            IniKeyExcludesListDefault
                            );
      SaveIniFile := True;
    end;
    if not IniFile.ValueExists( Option.IniSectionName, IniKeySkipDirsList ) then
    begin
      IniFile.WriteString(  Option.IniSectionName,
                            IniKeySkipDirsList,
                            IniKeySkipDirsDefault
                            );
      SaveIniFile := True;
    end;
    if not IniFile.ValueExists( Option.IniSectionName, IniKeyTabReplace ) then
    begin
      IniFile.WriteBool(  Option.IniSectionName,
                          IniKeyTabReplace,
                          Option.TabReplace
                          );
      SaveIniFile := True;
    end;
    if not IniFile.ValueExists( Option.IniSectionName, IniKeyTabReports ) then
    begin
      IniFile.WriteBool(  Option.IniSectionName,
                          IniKeyTabReports,
                          Option.TabReports
                          );
      SaveIniFile := True;
    end;
    if not IniFile.ValueExists( Option.IniSectionName, IniKeyTabSpacing ) then
    begin
      IniFile.WriteInteger( Option.IniSectionName,
                            IniKeyTabSpacing,
                            Option.TabSpacing
                            );
      SaveIniFile := True;
    end;
    if SaveIniFile then IniFile.UpdateFile;

    {--------------------------------------------------------------------------}

    // Prepare FileList
    InitializeStringList( FileList );

    // Prepare RootPathList
    if optRootPaths in Option.OptionUsed then
    begin
      RootPathList.DelimitedText := Option.RootPaths;
    end
    else
    begin
      RootPathList.DelimitedText := IniFile.ReadString( Option.IniSectionName,
                                                        IniKeyRootPathList,
                                                        Option.RootPaths
                                                        );
    end;

    // Prepare ExcludesList
    InitializeStringList( ExcludesList );
    ExcludesList.DelimitedText := IniFile.ReadString( Option.IniSectionName,
                                                      IniKeyExcludesList,
                                                      IniKeyExcludesListDefault
                                                      );

    // Prepare IncludesList
    InitializeStringList( IncludesList );
    IncludesList.DelimitedText := IniFile.ReadString( Option.IniSectionName,
                                                      IniKeyIncludesList,
                                                      IniKeyIncludesListDefault
                                                      );

    // Prepare ExtensonList
    InitializeStringList( ExtensonList );
    if optExtensions in Option.OptionUsed then
    begin
      ExtensonList.DelimitedText := Option.Extensions;
    end
    else
    begin
      ExtensonList.DelimitedText := IniFile.ReadString( Option.IniSectionName,
                                                        IniKeyExtensonList,
                                                        IniKeyExtensonListDefault
                                                        );
    end;

    // Prepare SkipDirsList
    InitializeStringList( SkipDirsList );
    SkipDirsList.DelimitedText := IniFile.ReadString( Option.IniSectionName,
                                                      IniKeySkipDirsList,
                                                      IniKeySkipDirsDefault
                                                      );
    SkipDirsList.Add( '.' );
    SkipDirsList.Add( '..' );

    {--------------------------------------------------------------------------}

    // Prepare TabReplace, TabReports and TabSpacing
    if not ( optTabReplace in Option.OptionUsed ) then
    begin
      Option.TabReplace := IniFile.ReadBool(  Option.IniSectionName,
                                              IniKeyTabReplace,
                                              Option.TabReplace
                                              );
    end;
    if not ( optTabReports in Option.OptionUsed ) then
    begin
      Option.TabReports := IniFile.ReadBool(  Option.IniSectionName,
                                              IniKeyTabReports,
                                              Option.TabReports
                                              );
    end;
    if not ( optTabSpacing in Option.OptionUsed ) then
    begin
      Option.TabSpacing := IniFile.ReadInteger( Option.IniSectionName,
                                                IniKeyTabSpacing,
                                                Option.TabSpacing
                                                );
    end;

    {--------------------------------------------------------------------------}

    Jadex := Length( Option.FilesPaths );
    if Jadex > 0 then
    begin
      Dec( Jadex );
      for Index := 0 to Jadex do
      begin
        FileName := Option.FilesPaths[ Index ];
        if DirectoryExists( FileName ) then
        begin
          FileList.AddFiles( FileName, Option.Recursive, SkipDirsList );
        end
        else
        begin
          FileList.Add( FileName );
        end;
      end;
    end
    else
    begin
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
          // INI File selection is always recursive
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

    {--------------------------------------------------------------------------}

    // Remove extra white space
    EditedTrim := 0;
    for Index := 0 to FileList.Count - 1 do
    begin
      FileName := FileList.Strings[ Index ];

      if FileEdit.EditWhiteSpace( FileName ) then
      begin
        WriteLn( 'Edit: ', FileName );
        Inc( EditedTrim );
      end;
    end;

    {--------------------------------------------------------------------------}

    // Replace tabs
    EditedTabs := 0;
    if Option.TabReplace then
    begin
      for Index := 0 to FileList.Count - 1 do
      begin
        FileName := FileList.Strings[ Index ];

        if FileEdit.EditTabs( FileName, Option.TabSpacing ) then
        begin
          WriteLn( 'Tabs: ', FileName );
          Inc( EditedTabs );
        end;
      end;
    end;

    {--------------------------------------------------------------------------}

    // Report where tabs exist
    FinderTabs := 0;
    if Option.TabReports then
    begin
      for Index := 0 to FileList.Count - 1 do
      begin
        FileName := FileList.Strings[ Index ];
        TabLines := FileEdit.FindTabs( FileName );

        if Length( TabLines ) > 0 then
        begin
          WriteLn( 'Tabs: ', FileName, ', lines ', TabLines );
          Inc( FinderTabs );
        end;
      end;
    end;

    {--------------------------------------------------------------------------}

    WriteLn( Format( MessageFini, [ FileList.Count, EditedTrim, EditedTabs, FinderTabs ] ) );

    {--------------------------------------------------------------------------}

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

  {----------------------------------------------------------------------------}

end;

{==============================================================================}

end.
