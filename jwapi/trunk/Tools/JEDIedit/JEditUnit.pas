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
    function EditFile( const FileName: string ): Boolean;

  end;

{------------------------------------------------------------------------------}

type
  TFileList = class( TStringList )
  public
    procedure AddFiles( const Path: string; const Recursive: Boolean );
  end;

{==============================================================================}

function TFileEdit.EditFile( const FileName: string ): Boolean;

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
    FileExtension: string;

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
        FileExtension := ExtractFileExt( SearchRec.Name );

        if SameText( FileExtension, '.pas' )
        or SameText( FileExtension, '.inc' )
        or SameText( FileExtension, '.dpr' ) then
        begin
          Add( Path + PathSep + SearchRec.Name );
        end;
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
    FileName: string;
    Index: Integer;

begin
  Result := 0;

  FileEdit := TFileEdit.Create;
  FileList := TFileList.Create;
  try
    FileList.AddFiles( 'jwapi', True );
    FileList.AddFiles( 'jwscl', True );
    FileList.Sort;

    for Index := 0 to FileList.Count - 1 do
    begin
      FileName := FileList.Strings[ Index ];

      if FileEdit.EditFile( FileName ) then
      begin
        WriteLn( FileName );
      end;
    end;

  finally
    FreeAndNil( FileList );
    FreeAndNil( FileEdit );
  end;
end;

{==============================================================================}

end.
