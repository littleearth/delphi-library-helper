unit frmSearchU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Actions,
  Vcl.ActnList, System.ImageList, Vcl.ImgList, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.ComCtrls, LibraryHelperU, Vcl.Menus;

type
  TfrmSearch = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    ActionList: TActionList;
    ImageList: TImageList;
    ActionFind: TAction;
    ActionClose: TAction;
    Panel3: TPanel;
    Panel4: TPanel;
    Label2: TLabel;
    cbIgnoreCase: TCheckBox;
    editFind: TComboBox;
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
    ListViewSearch: TListView;
    procedure FormShow(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure CheckAll1Click(Sender: TObject);
    procedure UncheckAll1Click(Sender: TObject);
    procedure ActionFindExecute(Sender: TObject);
    procedure editFindKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FDelphiInstallation: TDelphiInstallation;
    procedure UpdateComboBoxOptions(ADelphiInstallation: TDelphiInstallation);
    procedure Find(AFind: string; AIgnoreCase: Boolean);
    procedure SetDestinationsChecked(AValue: Boolean);
  public
    function Execute(ADelphiInstallation: TDelphiInstallation): Boolean;
  end;

implementation

{$R *.dfm}

uses
  System.StrUtils, frmProgressU;

procedure TfrmSearch.ActionCloseExecute(Sender: TObject);
begin
  Self.ModalResult := mrCancel;
end;

procedure TfrmSearch.SetDestinationsChecked(AValue: Boolean);
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

procedure TfrmSearch.ActionFindExecute(Sender: TObject);
begin
  Find(editFind.Text, cbIgnoreCase.Checked);
  if ListViewSearch.Items.Count = 0 then
  begin
    MessageDlg(Format('"%s" was not found.', [editFind.Text]), mtInformation,
      [mbOK], 0);
  end;
end;

procedure TfrmSearch.CheckAll1Click(Sender: TObject);
begin
  SetDestinationsChecked(True);
end;

procedure TfrmSearch.editFindKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    ActionFind.Execute;
end;

function TfrmSearch.Execute(ADelphiInstallation: TDelphiInstallation): Boolean;
begin
  Result := True;
  if Assigned(ADelphiInstallation) then
  begin
    FDelphiInstallation := ADelphiInstallation;
    UpdateComboBoxOptions(FDelphiInstallation);
    SetDestinationsChecked(True);
    Self.ShowModal;
  end;
end;

procedure TfrmSearch.Find(AFind: string; AIgnoreCase: Boolean);

var
  LLibrary: TStringList;

  function SearchText(AText, ASubText: string; AIgnoreCase: Boolean): Boolean;
  begin
    if AIgnoreCase then
    begin
      Result := ContainsStr(AText, ASubText);
    end
    else
    begin
      Result := ContainsText(AText, ASubText);
    end;
  end;

  procedure AddFind(AEntry: string; APath: string; ALibrary: string);
  begin
    with ListViewSearch.Items.Add do
    begin
      Caption := AEntry;
      SubItems.Add(APath);
      SubItems.Add(ALibrary);
    end;
  end;

  procedure SearchLibrary(ALibrary: TDelphiLibrary);
  var
    LLibrary: TStringList;
    LIdx: integer;
    LLibraryPath: string;
    LLibraryEntry: string;
  begin
    LLibrary := TStringList.Create;
    try
      FDelphiInstallation.LibraryAsStrings(LLibrary, ALibrary);
      for LIdx := 0 to Pred(LLibrary.Count) do
      begin
        LLibraryEntry := LLibrary[LIdx];
        LLibraryPath := FDelphiInstallation.ExpandLibraryPath(LLibraryEntry,
          ALibrary);
        if SearchText(LLibraryPath, AFind, AIgnoreCase) then
        begin
          AddFind(LLibraryEntry, LLibraryPath,
            FDelphiInstallation.GetLibraryPlatformName(ALibrary));
        end;
      end;
    finally
      FreeAndNil(LLibrary);
    end;
  end;

begin
  ShowProgress('Searching...');
  LLibrary := TStringList.Create;
  ListViewSearch.Items.Clear;
  ListViewSearch.Items.BeginUpdate;
  try
    if cbAndroid32.Checked then
    begin
      SearchLibrary(dlAndroid32);
    end;

    if cbIOS32.Checked then
    begin
      SearchLibrary(dlIOS32);
    end;

    if cbIOS64.Checked then
    begin
      SearchLibrary(dlIOS64);
    end;

    if cbIOSSimulator.Checked then
    begin
      SearchLibrary(dlIOSimulator);
    end;

    if cbOSX.Checked then
    begin
      SearchLibrary(dlOSX32);
    end;

    if cbWin32.Checked then
    begin
      SearchLibrary(dlWin32);
    end;

    if cbWin64.Checked then
    begin
      SearchLibrary(dlWin64);
    end;

    if cbLinux64.Checked then
    begin
      SearchLibrary(dlLinux64);
    end;

  finally
    ListViewSearch.Items.EndUpdate;
    FreeAndNil(LLibrary);
    HideProgress;
  end;

end;

procedure TfrmSearch.FormShow(Sender: TObject);
begin
  editFind.SetFocus;
end;

procedure TfrmSearch.UncheckAll1Click(Sender: TObject);
begin
  SetDestinationsChecked(False);
end;

procedure TfrmSearch.UpdateComboBoxOptions(ADelphiInstallation
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
  ListViewSearch.Items.Clear;
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
  finally
    FreeAndNil(LItems);
  end;
end;

end.
