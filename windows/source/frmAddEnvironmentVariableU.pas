unit frmAddEnvironmentVariableU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.ImageList, Vcl.ImgList,
  LibraryHelperU, System.UITypes,
  System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.Mask, JvExMask, JvToolEdit;

type
  TfrmAddEnvironmentVariable = class(TForm)
    Panel2: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ActionList: TActionList;
    ActionOk: TAction;
    ActionCancel: TAction;
    Panel1: TPanel;
    Label1: TLabel;
    editName: TEdit;
    Label2: TLabel;
    editValue: TJvDirectoryEdit;
    procedure ActionCancelExecute(Sender: TObject);
    procedure ActionOkExecute(Sender: TObject);
  private
    FDelphiInstallation: TDelphiInstallation;
  public
    function Add(ADelphiInstallation: TDelphiInstallation): Boolean;
  end;

implementation

{$R *.dfm}

uses
  dmDelphiLibraryHelperU;

{ TfrmAddEnvironmentVariable }

procedure TfrmAddEnvironmentVariable.ActionCancelExecute(Sender: TObject);
begin
  Self.ModalResult := mrCancel;
end;

procedure TfrmAddEnvironmentVariable.ActionOkExecute(Sender: TObject);
var
  LAllow: Boolean;
begin
  LAllow := True;
  if Trim(editName.Text) <> '' then
  begin
    MessageDlg('Please specify a valid name.', mtError, [mbOK], 0);
    LAllow := False;
  end;

  if Trim(editValue.Text) <> '' then
  begin
    MessageDlg('Please specify a valid value.', mtError, [mbOK], 0);
    LAllow := False;
  end;

  if LAllow then
  begin
    FDelphiInstallation.EnvironmentVariables.Add(editName.Text, editValue.Text);
    Self.ModalResult := mrOk;
  end;
end;

function TfrmAddEnvironmentVariable.Add(ADelphiInstallation
  : TDelphiInstallation): Boolean;
begin
  FDelphiInstallation := ADelphiInstallation;
  try
    editName.Text := '';
    editValue.Text := '';
    Result := Self.ShowModal = mrOk;
  finally
    FDelphiInstallation := nil;
  end;
end;

end.
