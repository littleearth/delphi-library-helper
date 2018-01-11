{ -----------------------------------------------------------------------------
  Unit Name:  dlgSelectDatabaseU
  Author: Tristan Marlow
  Purpose: Progress Dialog.

  ----------------------------------------------------------------------------
  Copyright (c) 2007 Tristan David Marlow
  Copyright (c) 2007 ABit Consulting
  All Rights Reserved

  This product is protected by copyright and distributed under
  licenses restricting copying, distribution and decompilation

  ----------------------------------------------------------------------------

  History: 19/04/2007 - First Release.
  26/04/2007 - Thread Timer for dialog updates

  ----------------------------------------------------------------------------- }
unit frmProgressU;

interface

uses
  ComObj,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, ImgList, System.ImageList,
  Vcl.Imaging.pngimage;

const
  CLSID_TaskbarList: TGUID = (D1: $56FDF344; D2: $FD6D; D3: $11D0;
    D4: ($95, $8A, $00, $60, $97, $C9, $A0, $90));
  IID_TaskbarList: TGUID = (D1: $56FDF342; D2: $FD6D; D3: $11D0;
    D4: ($95, $8A, $00, $60, $97, $C9, $A0, $90));
  IID_TaskbarList2: TGUID = (D1: $602D4995; D2: $B13A; D3: $429B;
    D4: ($A6, $6E, $19, $35, $E4, $4F, $43, $17));
  IID_TaskbarList3: TGUID = (D1: $EA1AFB91; D2: $9E28; D3: $4B86;
    D4: ($90, $E9, $9E, $9F, $8A, $5E, $EF, $AF));

const
  THBF_ENABLED = $0000;
  THBF_DISABLED = $0001;
  THBF_DISMISSONCLICK = $0002;
  THBF_NOBACKGROUND = $0004;
  THBF_HIDDEN = $0008;

const
  THB_BITMAP = $0001;
  THB_ICON = $0002;
  THB_TOOLTIP = $0004;
  THB_FLAGS = $0008;

const
  THBN_CLICKED = $1800;

const
  TBPF_NOPROGRESS = $00;
  TBPF_INDETERMINATE = $01;
  TBPF_NORMAL = $02;
  TBPF_ERROR = $04;
  TBPF_PAUSED = $08;

const
  TBATF_USEMDITHUMBNAIL: DWORD = $00000001;
  TBATF_USEMDILIVEPREVIEW: DWORD = $00000002;

const
  WM_DWMSENDICONICTHUMBNAIL = $0323;
  WM_DWMSENDICONICLIVEPREVIEWBITMAP = $0326;

type
  TTipString = array [0 .. 259] of widechar;
  PTipString = ^TTipString;

  tagTHUMBBUTTON = packed record
    dwMask: DWORD;
    iId: UINT;
    iBitmap: UINT;
    hIcon: hIcon;
    szTip: TTipString;
    dwFlags: DWORD;
  end;

  THUMBBUTTON = tagTHUMBBUTTON;
  THUMBBUTTONLIST = ^THUMBBUTTON;

type
  ITaskbarList = interface
    ['{56FDF342-FD6D-11D0-958A-006097C9A090}']
    procedure HrInit; safecall;
    procedure AddTab(hwnd: cardinal); safecall;
    procedure DeleteTab(hwnd: cardinal); safecall;
    procedure ActivateTab(hwnd: cardinal); safecall;
    procedure SetActiveAlt(hwnd: cardinal); safecall;
  end;

  ITaskbarList2 = interface(ITaskbarList)
    ['{602D4995-B13A-429B-A66E-1935E44F4317}']
    procedure MarkFullscreenWindow(hwnd: cardinal; fFullscreen: Bool); safecall;
  end;

  ITaskbarList3 = interface(ITaskbarList2)
    ['{EA1AFB91-9E28-4B86-90E9-9E9F8A5EEFAF}']
    function SetProgressValue(hwnd: hwnd; ullCompleted: ULONGLONG;
      ullTotal: ULONGLONG): HRESULT; stdcall;
    function SetProgressState(hwnd: hwnd; tbpFlags: integer): HRESULT; stdcall;
    procedure RegisterTab(hwndTab: cardinal; hwndMDI: cardinal); safecall;
    procedure UnregisterTab(hwndTab: cardinal); safecall;
    procedure SetTabOrder(hwndTab: cardinal;
      hwndInsertBefore: cardinal); safecall;
    procedure SetTabActive(hwndTab: cardinal; hwndMDI: cardinal;
      tbatFlags: DWORD); safecall;
    procedure ThumbBarAddButtons(hwnd: cardinal; cButtons: UINT;
      Button: THUMBBUTTONLIST); safecall;
    procedure ThumbBarUpdateButtons(hwnd: cardinal; cButtons: UINT;
      pButton: THUMBBUTTONLIST); safecall;
    procedure ThumbBarSetImageList(hwnd: cardinal; himl: cardinal); safecall;
    procedure SetOverlayIcon(hwnd: cardinal; hIcon: hIcon;
      pszDescription: LPCWSTR); safecall;
    procedure SetThumbnailTooltip(hwnd: cardinal; pszTip: LPCWSTR); safecall;
    procedure SetThumbnailClip(hwnd: cardinal; prcClip: PRect); safecall;
  end;

type
  TProgressStyle = (psNone, psNormal);

type
  TfrmProgress = class(TForm)
    pnlProgress: TPanel;
    Panel1: TPanel;
    imgProgress: TImage;
    lblCaption: TLabel;
    Panel2: TPanel;
    progressNormal: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTaskBarFormHandle: THandle;
    FTaskbarList: ITaskbarList;
    FTaskbarList3: ITaskbarList3;
    function CalculatePercentage(AValue, ATotal: integer): integer;
  public
    procedure Show(ACaption: string; AProgressStyle: TProgressStyle);
      reintroduce;
    procedure Progress(AProgress: integer; ACaption: string = ''); overload;
    procedure Progress(AValue, AMax: integer; ACaption: string = ''); overload;
    procedure Update; override;
  published
  end;

var
  _ProgressForm: TfrmProgress;

procedure ShowProgress(ACaption: string;
  AProgressStyle: TProgressStyle = psNormal);
procedure UpdateProgress(AProgress: integer; ACaption: string = ''); overload;
procedure UpdateProgress(AValue, AMax: integer; ACaption: string = '');
  overload;
procedure HideProgress;

implementation

uses DateUtils;

{$R *.dfm}

procedure HideProgress;
begin
  if Assigned(_ProgressForm) then
  begin
    _ProgressForm.Close;
    _ProgressForm.Free;
  end;
  _ProgressForm := nil;
end;

procedure ShowProgress(ACaption: string; AProgressStyle: TProgressStyle);
begin
  if not Assigned(_ProgressForm) then
  begin
    _ProgressForm := TfrmProgress.Create(Application.MainForm);
  end;

  if Assigned(_ProgressForm) then
  begin
    _ProgressForm.Show(ACaption, AProgressStyle);
  end;
end;

procedure UpdateProgress(AProgress: integer; ACaption: string);
begin
  if Assigned(_ProgressForm) then
  begin
    _ProgressForm.Progress(AProgress, ACaption);
  end;
end;

procedure UpdateProgress(AValue, AMax: integer; ACaption: string = '');
begin
  if Assigned(_ProgressForm) then
  begin
    _ProgressForm.Progress(AValue, AMax, ACaption);
  end;
end;

procedure TfrmProgress.Show(ACaption: string; AProgressStyle: TProgressStyle);
begin
  lblCaption.Caption := ACaption;
  progressNormal.Visible := False;
  progressNormal.Position := 100;
  case AProgressStyle of
    psNone:
      begin
      end;
    psNormal:
      begin
        progressNormal.Visible := True;
        progressNormal.Position := 0;

      end;
  end;
  inherited Show;
  Application.ProcessMessages;
end;

procedure TfrmProgress.Update;
begin
  inherited Update;
  Application.ProcessMessages;
end;

procedure TfrmProgress.FormCreate(Sender: TObject);
begin
  // Windows 7
  FTaskBarFormHandle := 0;
  if CheckWin32Version(6, 1) then
  begin
    FTaskbarList := CreateComObject(CLSID_TaskbarList) as ITaskbarList;
    FTaskbarList.HrInit;
    Supports(FTaskbarList, IID_TaskbarList3, FTaskbarList3);
    if not Application.MainFormOnTaskBar then
    begin
      FTaskBarFormHandle := Application.Handle;
    end
    else
    begin
      FTaskBarFormHandle := Application.MainForm.Handle;
    end;
  end;
end;

procedure TfrmProgress.FormDestroy(Sender: TObject);
begin
  if Assigned(FTaskbarList3) then
    FTaskbarList3.SetProgressState(FTaskBarFormHandle, TBPF_NOPROGRESS);
end;

procedure TfrmProgress.Progress(AValue, AMax: integer; ACaption: string);
begin
  Progress(CalculatePercentage(AValue, AMax), ACaption);
end;

function TfrmProgress.CalculatePercentage(AValue: integer;
  ATotal: integer): integer;
begin
  Result := 0;
  if ATotal > 0 then
  begin
    Result := Round((AValue / ATotal) * 100);
  end;
end;

procedure TfrmProgress.Progress(AProgress: integer; ACaption: string = '');
begin
  if ACaption <> '' then
    lblCaption.Caption := ACaption;

  // 05/05/2007 - FIX: Vista TProgressBar not completing issue

  if (Win32MajorVersion = 6) then
  begin
    progressNormal.Position := AProgress + 2;
  end;

  progressNormal.Position := AProgress;

  // Windows 7 Taskbar progress

  if (FTaskBarFormHandle <> 0) and (CheckWin32Version(6, 1)) then
  begin
    if Assigned(FTaskbarList3) then
      FTaskbarList3.SetProgressState(FTaskBarFormHandle, TBPF_NORMAL);
    if Assigned(FTaskbarList3) then
      FTaskbarList3.SetProgressValue(FTaskBarFormHandle,
        progressNormal.Position, progressNormal.Max);
  end;
  Application.ProcessMessages;
end;

end.
