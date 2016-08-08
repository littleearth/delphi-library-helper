unit frmDelphiLibraryHelperU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ExtCtrls, LibraryHelperU, Vcl.ComCtrls, System.Actions,
  Vcl.ActnList, System.ImageList, Vcl.ImgList, Vcl.Menus;

type
  TfrmDelphiLibraryHelper = class(TForm)
    GroupBox1: TGroupBox;
    comboDelphiInstallations: TComboBox;
    lblRootPath: TLabel;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    comboLibraries: TComboBox;
    BitBtn4: TBitBtn;
    BitBtn7: TBitBtn;
    StatusBar: TStatusBar;
    ActionList: TActionList;
    ActionAddEnvironmentVariables: TAction;
    ActionDeleteEnvironmentVariable: TAction;
    ActionAddLibraryPath: TAction;
    ActionDeleteLibraryPath: TAction;
    ActionDeleteAllLibraryPaths: TAction;
    ActionSave: TAction;
    ActionLoad: TAction;
    ListViewLibrary: TListView;
    ActionApplyTemplate: TAction;
    BitBtn8: TBitBtn;
    ImageList: TImageList;
    Panel3: TPanel;
    ListViewSystemEnvironmentVariables: TListView;
    Label2: TLabel;
    Panel2: TPanel;
    ListViewEnvironmentVariables: TListView;
    Panel4: TPanel;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    Label1: TLabel;
    Panel5: TPanel;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Load1: TMenuItem;
    Save1: TMenuItem;
    N1: TMenuItem;
    emplates1: TMenuItem;
    ApplyTemplate2: TMenuItem;
    Help1: TMenuItem;
    ActionExit: TAction;
    ActionAbout: TAction;
    Exit1: TMenuItem;
    About1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure comboDelphiInstallationsChange(Sender: TObject);
    procedure ActionLoadExecute(Sender: TObject);
    procedure ActionSaveUpdate(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionLoadUpdate(Sender: TObject);
    procedure comboLibrariesChange(Sender: TObject);
    procedure ActionApplyTemplateUpdate(Sender: TObject);
    procedure ActionApplyTemplateExecute(Sender: TObject);
    procedure ActionDeleteLibraryPathExecute(Sender: TObject);
    procedure ActionDeleteAllLibraryPathsExecute(Sender: TObject);
    procedure ActionDeleteEnvironmentVariableExecute(Sender: TObject);
    procedure ActionAddLibraryPathExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ActionAddEnvironmentVariablesExecute(Sender: TObject);
    procedure ListViewLibraryDblClick(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionAboutExecute(Sender: TObject);
  private
    FApplicationActive: Boolean;
    FModified: Boolean;
    FLibraryHelper: TLibraryHelper;
    FDelphiInstallation: TDelphiInstallation;
    FActiveDelphiLibrary: TDelphiLibrary;
    procedure LoadDelphiInstallation;
    procedure LoadEnvironmentVariables;
    procedure SaveEnvironmentVariables;
    procedure LoadLibrary;
    procedure SaveLibrary;
    procedure LoadSystemEnvironmentVariables;
  public
    { Public declarations }
  end;

var
  frmDelphiLibraryHelper: TfrmDelphiLibraryHelper;

implementation

{$R *.dfm}

uses
  frmAddLibraryPathU, frmAddEnvironmentVariableU, frmAboutU;

procedure TfrmDelphiLibraryHelper.ActionAboutExecute(Sender: TObject);
begin
  with TfrmAbout.Create(Self) do
  begin
    try
      ShowModal;
    finally
      Free;
    end;
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionAddEnvironmentVariablesExecute
  (Sender: TObject);
var
  LfrmAddEnvironmentVariable: TfrmAddEnvironmentVariable;
begin
  LfrmAddEnvironmentVariable := TfrmAddEnvironmentVariable.Create(Self);
  try
    if LfrmAddEnvironmentVariable.Add(FDelphiInstallation) then
    begin
      FModified := True;
      LoadEnvironmentVariables;
    end;
  finally
    FreeAndNil(LfrmAddEnvironmentVariable);
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionAddLibraryPathExecute(Sender: TObject);
var
  LfrmAddLibraryPath: TfrmAddLibraryPath;
begin
  LfrmAddLibraryPath := TfrmAddLibraryPath.Create(nil);
  try
    if LfrmAddLibraryPath.Add(FActiveDelphiLibrary, FDelphiInstallation) then
    begin
      FModified := True;
      LoadLibrary;
    end;
  finally
    FreeAndNil(LfrmAddLibraryPath);
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionApplyTemplateExecute(Sender: TObject);
var
  LOpenDialog: TOpenDialog;
begin
  LOpenDialog := TOpenDialog.Create(Self);
  try
    LOpenDialog.DefaultExt := '.dlht';
    LOpenDialog.Filter :=
      'Delphi Library Helper Template (*.dlht)|*.dlht|All Files (*.*)|*' + '.*';
    LOpenDialog.Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
    LOpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
    if LOpenDialog.Execute then
    begin
      FDelphiInstallation.Apply(LOpenDialog.FileName);
      comboLibrariesChange(nil);
      FModified := True;
    end;
  finally
    FreeAndNil(LOpenDialog);
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionApplyTemplateUpdate(Sender: TObject);
begin
  ActionApplyTemplate.Enabled := Assigned(FDelphiInstallation);
end;

procedure TfrmDelphiLibraryHelper.ActionDeleteAllLibraryPathsExecute
  (Sender: TObject);
begin
  if (MessageDlg('Delete All?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    ListViewLibrary.Clear;
    SaveLibrary;
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionDeleteEnvironmentVariableExecute
  (Sender: TObject);
begin
  if (MessageDlg('Delete Selected?', mtConfirmation, [mbYes, mbNo], 0) = mrYes)
  then
  begin
    ListViewEnvironmentVariables.DeleteSelected;
    SaveEnvironmentVariables;
    FModified := True;
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionDeleteLibraryPathExecute
  (Sender: TObject);
begin
  if (MessageDlg('Delete Selected?', mtConfirmation, [mbYes, mbNo], 0) = mrYes)
  then
  begin
    ListViewLibrary.DeleteSelected;
    SaveLibrary;
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionExitExecute(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmDelphiLibraryHelper.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  if FModified then
  begin
    StatusBar.Panels[0].Text := 'Modified';
  end
  else
  begin
    StatusBar.Panels[0].Text := '';
  end;
  if Assigned(FLibraryHelper) and (FLibraryHelper.IsDelphiRunning) then
  begin
    StatusBar.Panels[1].Text := 'Delphi running.';
  end
  else
  begin
    StatusBar.Panels[1].Text := 'Delphi is not running.';
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionLoadExecute(Sender: TObject);
begin
  if comboDelphiInstallations.ItemIndex <> -1 then
  begin
    FDelphiInstallation := FLibraryHelper.Installation
      [comboDelphiInstallations.Text];
    FDelphiInstallation.Load;
    lblRootPath.Caption := FDelphiInstallation.RootPath;
    LoadSystemEnvironmentVariables;
    LoadEnvironmentVariables;
    comboLibrariesChange(nil);
    FModified := False;
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionLoadUpdate(Sender: TObject);
begin
  ActionLoad.Enabled := comboDelphiInstallations.ItemIndex <> -1;
end;

procedure TfrmDelphiLibraryHelper.ActionSaveExecute(Sender: TObject);
var
  LAllow: Boolean;
begin
  LAllow := True;
  if FLibraryHelper.IsDelphiRunning then
  begin
    LAllow := False;
    if (MessageDlg('Delphi is still running, continue with save?', mtWarning,
      [mbYes, mbNo], 0) = mrYes) then
    begin
      LAllow := True;
    end;
  end;

  if LAllow then
  begin
    if Assigned(FDelphiInstallation) then
    begin
      FDelphiInstallation.Save;
      FModified := False;
    end;
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionSaveUpdate(Sender: TObject);
begin
  ActionSave.Enabled := (Assigned(FDelphiInstallation)) and (FModified);
end;

procedure TfrmDelphiLibraryHelper.comboDelphiInstallationsChange
  (Sender: TObject);
begin
  ActionLoad.Execute;
end;

procedure TfrmDelphiLibraryHelper.comboLibrariesChange(Sender: TObject);
begin
  if comboLibraries.ItemIndex <> -1 then
  begin
    FActiveDelphiLibrary := TDelphiLibrary(comboLibraries.ItemIndex);
    LoadLibrary;
  end;
end;

procedure TfrmDelphiLibraryHelper.FormActivate(Sender: TObject);
begin
  if not FApplicationActive then
  begin
    FApplicationActive := True;
    LoadDelphiInstallation;
  end;
end;

procedure TfrmDelphiLibraryHelper.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := not FModified;
  if not CanClose then
  begin
    if (MessageDlg('You have unsaved changes, are you sure you want to exit?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      CanClose := True;
    end;
  end;

end;

procedure TfrmDelphiLibraryHelper.FormCreate(Sender: TObject);
begin
  FApplicationActive := False;
  FLibraryHelper := TLibraryHelper.Create;
  comboLibraries.ItemIndex := 0;
end;

procedure TfrmDelphiLibraryHelper.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FLibraryHelper);
end;

procedure TfrmDelphiLibraryHelper.ListViewLibraryDblClick(Sender: TObject);
begin
  if (Assigned(FDelphiInstallation)) and (Assigned(ListViewLibrary.Selected))
  then
  begin
    FDelphiInstallation.OpenFolder(ListViewLibrary.Selected.Caption);
  end;
end;

procedure TfrmDelphiLibraryHelper.LoadDelphiInstallation;
var
  LIdx: integer;
begin
  comboDelphiInstallations.ItemIndex := -1;
  comboDelphiInstallations.Items.Clear;
  try
    FLibraryHelper.Load;
    for LIdx := 0 to Pred(FLibraryHelper.InstallationCount) do
    begin
      if FLibraryHelper.Installations[LIdx].Installed then
      begin
        comboDelphiInstallations.Items.Add(FLibraryHelper.Installations[LIdx]
          .ProductName);
      end;
    end;
  finally
    if comboDelphiInstallations.Items.Count > 0 then
    begin
      comboDelphiInstallations.ItemIndex := 0;
      comboDelphiInstallationsChange(nil);
    end;
  end;
end;

procedure TfrmDelphiLibraryHelper.LoadEnvironmentVariables;
var
  LIdx: integer;
begin
  ListViewEnvironmentVariables.Clear;
  if Assigned(FDelphiInstallation) then
  begin
    ListViewEnvironmentVariables.Items.BeginUpdate;
    try
      for LIdx := 0 to Pred(FDelphiInstallation.EnvironmentVariables.Count) do
      begin
        with ListViewEnvironmentVariables.Items.Add do
        begin
          Caption := FDelphiInstallation.EnvironmentVariables.Variable
            [LIdx].Name;
          SubItems.Add(FDelphiInstallation.EnvironmentVariables.Variable
            [LIdx].Value);
        end;
      end;
    finally
      ListViewEnvironmentVariables.Items.EndUpdate;
    end;
  end;
end;

procedure TfrmDelphiLibraryHelper.LoadSystemEnvironmentVariables;
var
  LIdx: integer;
begin
  ListViewSystemEnvironmentVariables.Clear;
  if Assigned(FDelphiInstallation) then
  begin
    ListViewSystemEnvironmentVariables.Items.BeginUpdate;
    try
      for LIdx :=
        0 to Pred(FDelphiInstallation.SystemEnvironmentVariables.Count) do
      begin
        with ListViewSystemEnvironmentVariables.Items.Add do
        begin
          Caption := FDelphiInstallation.SystemEnvironmentVariables.Variable
            [LIdx].Name;
          SubItems.Add(FDelphiInstallation.SystemEnvironmentVariables.Variable
            [LIdx].Value);
        end;
      end;
    finally
      ListViewSystemEnvironmentVariables.Items.EndUpdate;
    end;
  end;
end;

procedure TfrmDelphiLibraryHelper.SaveEnvironmentVariables;
var
  LIdx: integer;
  LName, LValue: string;
begin
  if Assigned(FDelphiInstallation) then
  begin
    FDelphiInstallation.EnvironmentVariables.Clear;
    for LIdx := 0 to Pred(ListViewEnvironmentVariables.Items.Count) do
    begin
      LName := ListViewEnvironmentVariables.Items[LIdx].Caption;
      LValue := ListViewEnvironmentVariables.Items[LIdx].SubItems[0];
      FDelphiInstallation.EnvironmentVariables.Add(LName, LValue);
    end;
  end;
end;

procedure TfrmDelphiLibraryHelper.SaveLibrary;
var
  LLibrary: TStringList;
  LIdx: integer;
begin
  LLibrary := TStringList.Create;
  try
    FModified := True;
    for LIdx := 0 to Pred(ListViewLibrary.Items.Count) do
    begin
      LLibrary.Add(ListViewLibrary.Items[LIdx].Caption);
    end;
    case FActiveDelphiLibrary of
      dlAndroid32:
        FDelphiInstallation.LibraryAndroid32 := LLibrary.Text;
      dlIOS32:
        FDelphiInstallation.LibraryIOS32 := LLibrary.Text;
      dlIOS64:
        FDelphiInstallation.LibraryIOS64 := LLibrary.Text;
      dlIOSimulator:
        FDelphiInstallation.LibraryIOSSimulator := LLibrary.Text;
      dlOSX32:
        FDelphiInstallation.LibraryOSX32 := LLibrary.Text;
      dlWin32:
        FDelphiInstallation.LibraryWin32 := LLibrary.Text;
      dlWin64:
        FDelphiInstallation.LibraryWin64 := LLibrary.Text;
    end;
    LoadLibrary;
  finally
    FreeAndNil(LLibrary);
  end;
end;

procedure TfrmDelphiLibraryHelper.LoadLibrary;
var
  LLibrary: TStringList;
  LIdx: integer;
begin
  ListViewLibrary.Clear;
  if Assigned(FDelphiInstallation) then
  begin
    LLibrary := TStringList.Create;
    ListViewLibrary.Items.BeginUpdate;
    try
      case FActiveDelphiLibrary of
        dlAndroid32:
          LLibrary.Text := FDelphiInstallation.LibraryAndroid32;
        dlIOS32:
          LLibrary.Text := FDelphiInstallation.LibraryIOS32;
        dlIOS64:
          LLibrary.Text := FDelphiInstallation.LibraryIOS64;
        dlIOSimulator:
          LLibrary.Text := FDelphiInstallation.LibraryIOSSimulator;
        dlOSX32:
          LLibrary.Text := FDelphiInstallation.LibraryOSX32;
        dlWin32:
          LLibrary.Text := FDelphiInstallation.LibraryWin32;
        dlWin64:
          LLibrary.Text := FDelphiInstallation.LibraryWin64;
      end;
      for LIdx := 0 to Pred(LLibrary.Count) do
      begin
        with ListViewLibrary.Items.Add do
        begin
          Caption := LLibrary[LIdx];
        end;
      end;
    finally
      FreeAndNil(LLibrary);
      ListViewLibrary.Items.EndUpdate;
    end;
  end;
end;

end.
