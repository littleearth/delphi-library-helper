program DelphiLibraryHelper;

uses
  Vcl.Forms,
  Windows,
  Controls,
  Dialogs,
  SysUtils,
  Printers,
  System.UITypes,
  frmDelphiLibraryHelperU in 'frmDelphiLibraryHelperU.pas' {frmDelphiLibraryHelper},
  LibraryPathsU in 'LibraryPathsU.pas',
  LibraryHelperU in 'LibraryHelperU.pas',
  frmAddLibraryPathU in 'frmAddLibraryPathU.pas' {frmAddLibraryPath},
  frmAddEnvironmentVariableU in 'frmAddEnvironmentVariableU.pas' {frmAddEnvironmentVariable},
  frmAboutU in 'frmAboutU.pas' {frmAbout},
  FileVersionInformationU in 'FileVersionInformationU.pas',
  frmFindReplaceU in 'frmFindReplaceU.pas' {frmFindReplace},
  frmSearchU in 'frmSearchU.pas' {frmSearch};

{$R *.res}

function IsFontInstalled(const AFontName: string): boolean;
begin
  Result := Screen.Fonts.IndexOf(AFontName) <> -1;
end;

begin
  Application.Initialize;
  if CheckWin32Version(6) then
  begin

    if IsFontInstalled('Segoe UI') then
    begin
      Application.DefaultFont.Name := 'Segoe UI';
      Screen.MessageFont.Name := 'Segoe UI';
    end;
    //
    if IsFontInstalled('Roboto Lt') then
    begin
      Application.DefaultFont.Name := 'Roboto Lt';
      Screen.MessageFont.Name := 'Roboto Lt';
    end;

    if IsFontInstalled('Roboto Light') then
    begin
      Application.DefaultFont.Name := 'Roboto Light';
      Screen.MessageFont.Name := 'Roboto Light';
    end;
  end;

  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDelphiLibraryHelper, frmDelphiLibraryHelper);
  Application.Run;

end.
