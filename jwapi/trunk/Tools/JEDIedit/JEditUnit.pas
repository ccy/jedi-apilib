unit JEditUnit;

{==============================================================================}

interface

{==============================================================================}

function RunProgram: Integer;

{==============================================================================}

implementation

uses
  Classes, SysUtils;

{==============================================================================}

const
  FileRootPathList = 'JEDIeditRoot.txt';
  FileExcludesList = 'JEDIeditSkip.txt';
  FileIncludesList = 'JEDIeditAdds.txt';
  FileExtensonList = 'JEDIeditExts.txt';

{==============================================================================}

function RightTrim( var Trimmed: Boolean; const Arg: string ): string;

  var
    Index, Limit: Integer;

begin
  Trimmed := False;
  Result := Arg;

  Index := Length( Arg );
  Limit := Index;
  while ( Index > 0 ) and ( Result[ Index ] <= ' ' ) do Dec( Index );

  Trimmed := Index < Limit;
  if Trimmed then SetLength( Result, Index );
end;

{==============================================================================}

type
  TFileEdit = class( TStringList )
  public
    function EditWhiteSpace( const FileName: string ): Boolean;
    function FindTabs( const FileName: string ): string;

  end;

{------------------------------------------------------------------------------}

type
  TFileList = class( TStringList )
  public
    procedure AddFiles( const Path: string; const Recursive: Boolean );
  end;

{==============================================================================}

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
        if Pos( #9, Strings[ Index ] ) > 0 then
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
procedure TFileList.AddFiles( const Path: string; const Recursive: Boolean );

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
        if ( SearchRec.Name <> '.' ) and ( SearchRec.Name <> '..' ) then
        begin
          AddFiles( Path + PathSep + SearchRec.Name, Recursive );
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

{==============================================================================}

function RunProgram: Integer;

  var
    FileEdit: TFileEdit;
    FileList: TFileList;
    RootPathList, ExcludesList, IncludesList, ExtensonList: TStringList;
    FileExtn, FileName, RootPath, TabLines: string;
    Compare, Index, Jadex: Integer;

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
  try
    // Prepare FileList
    InitializeStringList( FileList );

    // Prepare RootPathList
    if FileExists( FileRootPathList ) then
    begin
      RootPathList.LoadFromFile( FileRootPathList );
    end
    else
    begin
      RootPathList.Add( '..\jedi-apilib' );
      RootPathList.Add( '..\..\..\..\..\jedi-apilib' );
      RootPathList.SaveToFile( FileRootPathList );
    end;

    // Prepare ExcludesList
    InitializeStringList( ExcludesList );
    if FileExists( FileExcludesList ) then
    begin
      ExcludesList.LoadFromFile( FileExcludesList );
    end
    else
    begin
      ExcludesList.Add( 'jwapi\trunk\Examples' );
      ExcludesList.Add( 'jwscl\trunk\examples' );
      ExcludesList.Add( 'jwscl\trunk\unittests' );
      ExcludesList.SaveToFile( FileExcludesList );
    end;

    // Prepare IncludesList
    InitializeStringList( IncludesList );
    if FileExists( FileIncludesList ) then
    begin
      IncludesList.LoadFromFile( FileIncludesList );
    end
    else
    begin
      IncludesList.Add( 'jwapi\trunk' );
      IncludesList.Add( 'jwscl\trunk' );
      IncludesList.SaveToFile( FileIncludesList );
    end;

    // Prepare ExtensonList
    InitializeStringList( ExtensonList );
    if FileExists( FileExtensonList ) then
    begin
      ExtensonList.LoadFromFile( FileExtensonList );
    end
    else
    begin
      ExtensonList.Add( 'dpr' );
      ExtensonList.Add( 'inc' );
      ExtensonList.Add( 'pas' );
      ExtensonList.SaveToFile( FileExtensonList );
    end;

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
        FileList.AddFiles( RootPath + IncludesList.Strings[ Index ], True );
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
      end;
    end;

    // Report where tabs exist
    for Index := 0 to FileList.Count - 1 do
    begin
      FileName := FileList.Strings[ Index ];
      TabLines := FileEdit.FindTabs( FileName );

      if Length( TabLines ) > 0 then
      begin
        WriteLn( 'Tabs: ', FileName, ', lines ', TabLines );
      end;
    end;

  finally
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
