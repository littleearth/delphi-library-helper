program DelphiLibraryHelper;

uses
  Vcl.Forms,
  frmDelphiLibraryHelperU in 'frmDelphiLibraryHelperU.pas' {frmDelphiLibraryHelper},
  LibraryPathsU in 'LibraryPathsU.pas',
  LibraryHelperU in 'LibraryHelperU.pas',
  frmAddLibraryPathU in 'frmAddLibraryPathU.pas' {frmAddLibraryPath},
  frmAddEnvironmentVariableU in 'frmAddEnvironmentVariableU.pas' {frmAddEnvironmentVariable},
  frmAboutU in 'frmAboutU.pas' {frmAbout},
  FileVersionInformationU in 'FileVersionInformationU.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDelphiLibraryHelper, frmDelphiLibraryHelper);
  Application.Run;
end.
