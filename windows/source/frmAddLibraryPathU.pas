unit frmAddLibraryPathU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Buttons, LibraryHelperU, Vcl.Menus,
  System.Actions, Vcl.ActnList, System.ImageList, Vcl.ImgList;

type
  TfrmAddLibraryPath = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    GroupBoxDestination: TGroupBox;
    GridPanel1: TGridPanel;
    cbAndroid32: TCheckBox;
    cbIOS32: TCheckBox;
    cbIOS64: TCheckBox;
    cbIOSSimulator: TCheckBox;
    cbOSX: TCheckBox;
    cbWin32: TCheckBox;
    cbWin64: TCheckBox;
    Label1: TLabel;
    PopupMenuDestinations: TPopupMenu;
    CheckAll1: TMenuItem;
    UncheckAll1: TMenuItem;
    ActionList: TActionList;
    ImageList: TImageList;
    ActionOk: TAction;
    ActionCancel: TAction;
    Panel3: TPanel;
    editPath: TComboBox;
    BitBtn3: TBitBtn;
    ActionSelectFolder: TAction;
    procedure CheckAll1Click(Sender: TObject);
    procedure UncheckAll1Click(Sender: TObject);
    procedure ActionCancelExecute(Sender: TObject);
    procedure ActionOkExecute(Sender: TObject);
    procedure ActionSelectFolderExecute(Sender: TObject);
    procedure editPathDropDown(Sender: TObject);
    procedure ActionOkUpdate(Sender: TObject);
  private
    FDelphiLibrary: TDelphiLibrary;
    FDelphiInstallation: TDelphiInstallation;
    procedure SetDestinationsChecked(AValue: Boolean);
    procedure SetFormValues;
    procedure UpdatePathVariables;
  public
    function Add(ADelphiLibrary: TDelphiLibrary;
      ADelphiInstallation: TDelphiInstallation): Boolean;
  end;

implementation

{$R *.dfm}
{ TfrmAddLibraryPath }

procedure TfrmAddLibraryPath.ActionCancelExecute(Sender: TObject);
begin
  Self.ModalResult := mrCancel;
end;

procedure TfrmAddLibraryPath.ActionOkExecute(Sender: TObject);
var
  LSuccess: Boolean;
begin
  LSuccess := False;
  if Trim(editPath.Text) <> '' then
  begin
    if cbAndroid32.Checked then
      LSuccess := FDelphiInstallation.AddPath(editPath.Text, dlAndroid32);
    if cbIOS32.Checked then
      LSuccess := FDelphiInstallation.AddPath(editPath.Text, dlIOS32);
    if cbIOS64.Checked then
      LSuccess := FDelphiInstallation.AddPath(editPath.Text, dlIOS64);
    if cbIOSSimulator.Checked then
      LSuccess := FDelphiInstallation.AddPath(editPath.Text, dlIOSimulator);
    if cbOSX.Checked then
      LSuccess := FDelphiInstallation.AddPath(editPath.Text, dlOSX32);
    if cbWin32.Checked then
      LSuccess := FDelphiInstallation.AddPath(editPath.Text, dlWin32);
    if cbWin64.Checked then
      LSuccess := FDelphiInstallation.AddPath(editPath.Text, dlWin64);
  end;
  if LSuccess then
  begin
    Self.ModalResult := mrOk;
  end
  else
  begin
    MessageDlg(Format('Failed to add path "%s", please ensure it is valid',
      [editPath.Text]), mtError, [mbOK], 0);
  end;
end;

procedure TfrmAddLibraryPath.ActionOkUpdate(Sender: TObject);
begin
  ActionOk.Enabled := Trim(editPath.Text) <> '';
end;

procedure TfrmAddLibraryPath.ActionSelectFolderExecute(Sender: TObject);
begin
  with TFileOpenDialog.Create(nil) do
    try
      Options := [fdoPickFolders];
      if Execute then
      begin
        editPath.Text := FileName;
      end;
    finally
      Free;
    end;
end;

function TfrmAddLibraryPath.Add(ADelphiLibrary: TDelphiLibrary;
  ADelphiInstallation: TDelphiInstallation): Boolean;
begin
  FDelphiLibrary := ADelphiLibrary;
  FDelphiInstallation := ADelphiInstallation;
  SetFormValues;
  Result := Self.ShowModal = mrOk;
end;

procedure TfrmAddLibraryPath.CheckAll1Click(Sender: TObject);
begin
  SetDestinationsChecked(True);
end;

procedure TfrmAddLibraryPath.editPathDropDown(Sender: TObject);
begin
  UpdatePathVariables;
end;

procedure TfrmAddLibraryPath.SetDestinationsChecked(AValue: Boolean);
begin
  cbAndroid32.Checked := AValue;
  cbIOS32.Checked := AValue;
  cbIOS64.Checked := AValue;
  cbIOSSimulator.Checked := AValue;
  cbOSX.Checked := AValue;
  cbWin32.Checked := AValue;
  cbWin64.Checked := AValue;
end;

procedure TfrmAddLibraryPath.SetFormValues;
begin
  SetDestinationsChecked(False);
  case FDelphiLibrary of
    dlAndroid32:
      cbAndroid32.Checked := True;
    dlIOS32:
      cbIOS32.Checked := True;
    dlIOS64:
      cbIOS64.Checked := True;
    dlIOSimulator:
      cbIOSSimulator.Checked := True;
    dlOSX32:
      cbOSX.Checked := True;
    dlWin32:
      cbWin32.Checked := True;
    dlWin64:
      cbWin64.Checked := True;
  end;
end;

procedure TfrmAddLibraryPath.UncheckAll1Click(Sender: TObject);
begin
  SetDestinationsChecked(False);
end;

procedure TfrmAddLibraryPath.UpdatePathVariables;
var
  LIdx: Integer;
  LName, LPath: string;
begin
  editPath.Items.BeginUpdate;
  try
    for LIdx := 0 to Pred(FDelphiInstallation.EnvironmentVariables.Count) do
    begin
      LPath := FDelphiInstallation.EnvironmentVariables.Variable[LIdx].Value;
      LName := FDelphiInstallation.EnvironmentVariables.Variable[LIdx].Name;
      if DirectoryExists(LPath) then
      begin
        editPath.Items.Add(Format('$(%s)\', [LName]));
      end;
    end;
    for LIdx := 0 to Pred
      (FDelphiInstallation.SystemEnvironmentVariables.Count) do
    begin
      LPath := FDelphiInstallation.SystemEnvironmentVariables.Variable
        [LIdx].Value;
      LName := FDelphiInstallation.SystemEnvironmentVariables.Variable
        [LIdx].Name;
      if DirectoryExists(LPath) then
      begin
        editPath.Items.Add(Format('$(%s)\', [LName]));
      end;
    end;
  finally
    editPath.Items.EndUpdate;
  end;
end;

end.
