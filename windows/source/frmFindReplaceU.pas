unit frmFindReplaceU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Actions,
  Vcl.ActnList, System.ImageList, Vcl.ImgList, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.ComCtrls, LibraryHelperU, Vcl.Menus;

type
  TfrmFindReplace = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    ActionList: TActionList;
    ImageList: TImageList;
    ActionReplace: TAction;
    ActionCancel: TAction;
    Panel3: TPanel;
    Panel4: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    cbIgnoreCase: TCheckBox;
    editFind: TComboBox;
    editReplace: TComboBox;
    GroupBoxDestination: TGroupBox;
    GridPanel1: TGridPanel;
    cbAndroid32: TCheckBox;
    cbIOS32: TCheckBox;
    cbIOS64: TCheckBox;
    cbIOSSimulator: TCheckBox;
    cbOSX: TCheckBox;
    cbWin32: TCheckBox;
    cbWin64: TCheckBox;
    cbLinux64: TCheckBox;
    PopupMenuLibrary: TPopupMenu;
    CheckAll1: TMenuItem;
    UncheckAll1: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure ActionReplaceExecute(Sender: TObject);
    procedure ActionCancelExecute(Sender: TObject);
    procedure CheckAll1Click(Sender: TObject);
    procedure UncheckAll1Click(Sender: TObject);
  private
    FDelphiInstallation: TDelphiInstallation;
    procedure UpdateComboBoxOptions(ADelphiInstallation: TDelphiInstallation);
    procedure FindReplace(AFind, AReplace: string; AIgnoreCase: Boolean);
    procedure SetDestinationsChecked(AValue: Boolean);
  public
    function Execute(ADelphiInstallation: TDelphiInstallation): Boolean;
  end;

implementation

{$R *.dfm}

uses
  frmProgressU;

procedure TfrmFindReplace.ActionCancelExecute(Sender: TObject);
begin
  Self.ModalResult := mrCancel;
end;

procedure TfrmFindReplace.SetDestinationsChecked(AValue: Boolean);
begin
  cbAndroid32.Checked := AValue;
  cbIOS32.Checked := AValue;
  cbIOS64.Checked := AValue;
  cbIOSSimulator.Checked := AValue;
  cbOSX.Checked := AValue;
  cbWin32.Checked := AValue;
  cbWin64.Checked := AValue;
  cbLinux64.Checked := AValue;
end;

procedure TfrmFindReplace.ActionReplaceExecute(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmFindReplace.CheckAll1Click(Sender: TObject);
begin
  SetDestinationsChecked(True);
end;

function TfrmFindReplace.Execute(ADelphiInstallation
  : TDelphiInstallation): Boolean;
begin
  Result := False;
  if Assigned(ADelphiInstallation) then
  begin
    FDelphiInstallation := ADelphiInstallation;
    UpdateComboBoxOptions(FDelphiInstallation);
    SetDestinationsChecked(True);
    if Self.ShowModal = mrOk then
    begin
      FindReplace(editFind.Text, editReplace.Text, cbIgnoreCase.Checked);
      // FDelphiInstallation.Save;
      Result := True;
    end;
  end;
end;

procedure TfrmFindReplace.FindReplace(AFind, AReplace: string;
  AIgnoreCase: Boolean);
var
  LFlags: TReplaceFlags;
begin
  ShowProgress('Please wait...');
  try
    LFlags := [];
    if AIgnoreCase then
    begin
      LFlags := LFlags + [rfIgnoreCase];
    end;
    LFlags := LFlags + [rfReplaceAll];

    if cbAndroid32.Checked then
      FDelphiInstallation.LibraryAndroid32 :=
        StringReplace(FDelphiInstallation.LibraryAndroid32, AFind,
        AReplace, LFlags);

    if cbIOS32.Checked then
      FDelphiInstallation.LibraryIOS32 :=
        StringReplace(FDelphiInstallation.LibraryIOS32, AFind,
        AReplace, LFlags);

    if cbIOS64.Checked then
      FDelphiInstallation.LibraryIOS64 :=
        StringReplace(FDelphiInstallation.LibraryIOS64, AFind,
        AReplace, LFlags);

    if cbIOSSimulator.Checked then
      FDelphiInstallation.LibraryIOSSimulator :=
        StringReplace(FDelphiInstallation.LibraryIOSSimulator, AFind,
        AReplace, LFlags);

    if cbOSX.Checked then
      FDelphiInstallation.LibraryOSX32 :=
        StringReplace(FDelphiInstallation.LibraryOSX32, AFind,
        AReplace, LFlags);

    if cbWin32.Checked then
      FDelphiInstallation.LibraryWin32 :=
        StringReplace(FDelphiInstallation.LibraryWin32, AFind,
        AReplace, LFlags);

    if cbWin64.Checked then
      FDelphiInstallation.LibraryWin64 :=
        StringReplace(FDelphiInstallation.LibraryWin64, AFind,
        AReplace, LFlags);

    if cbLinux64.Checked then
      FDelphiInstallation.LibraryLinux64 :=
        StringReplace(FDelphiInstallation.LibraryLinux64, AFind,
        AReplace, LFlags);
  finally
    HideProgress;
  end;
end;

procedure TfrmFindReplace.FormShow(Sender: TObject);
begin
  editFind.SetFocus;
end;

procedure TfrmFindReplace.UncheckAll1Click(Sender: TObject);
begin
  SetDestinationsChecked(False);
end;

procedure TfrmFindReplace.UpdateComboBoxOptions(ADelphiInstallation
  : TDelphiInstallation);
var
  LItems: TStringList;

  function ValidatePath(APath: string; ADelphiLibrary: TDelphiLibrary): Boolean;
  var
    LPath: string;
  begin
    Result := False;
    LPath := APath;
    if Trim(LPath) <> '' then
    begin
      LPath := FDelphiInstallation.ExpandLibraryPath(LPath, ADelphiLibrary);
      Result := DirectoryExists(LPath);
    end;
  end;

  procedure AddLibraryPaths(ADelphiLibrary: TDelphiLibrary);
  var
    LLibrary: TStringList;
    LIdx: integer;
    LLibraryValue: string;
  begin
    LLibrary := TStringList.Create;
    try
      FDelphiInstallation.LibraryAsStrings(LLibrary, ADelphiLibrary);
      for LIdx := 0 to Pred(LLibrary.Count) do
      begin
        LLibraryValue := LLibrary[LIdx];
        if ValidatePath(LLibraryValue, ADelphiLibrary) then
        begin
          LItems.Add(LLibrary[LIdx]);
        end;
      end;
    finally
      FreeAndNil(LLibrary);
    end;
  end;

  procedure AddEnvironmentVariables(AEnvironmentVariables
    : TEnvironmentVariables);
  var
    LIdx: integer;
    LLibraryValue: string;
  begin
    for LIdx := 0 to Pred(AEnvironmentVariables.Count) do
    begin
      LLibraryValue := AEnvironmentVariables.Variable[LIdx].Value;
      if ValidatePath(LLibraryValue, dlWin32) then
      begin
        LItems.Add(LLibraryValue);
      end;

      LLibraryValue := AEnvironmentVariables.Variable[LIdx].Name;
      if Trim(LLibraryValue) <> '' then
      begin
        LLibraryValue := '${' + LLibraryValue + '}';
        LItems.Add(LLibraryValue);
      end;
    end;
  end;

begin
  editFind.Items.Clear;
  editReplace.Items.Clear;
  LItems := TStringList.Create;
  try
    LItems.Duplicates := dupIgnore;
    LItems.Sorted := True;

    AddLibraryPaths(dlAndroid32);
    AddLibraryPaths(dlIOS32);
    AddLibraryPaths(dlIOS64);
    AddLibraryPaths(dlOSX32);
    AddLibraryPaths(dlIOSimulator);
    AddLibraryPaths(dlIOS32);
    AddLibraryPaths(dlWin32);
    AddLibraryPaths(dlWin64);
    AddLibraryPaths(dlLinux64);
    AddEnvironmentVariables(FDelphiInstallation.EnvironmentVariables);
    AddEnvironmentVariables(FDelphiInstallation.SystemEnvironmentVariables);

    editFind.Items.Text := LItems.Text;
    editReplace.Items.Text := LItems.Text;
  finally
    FreeAndNil(LItems);
  end;
end;

end.
