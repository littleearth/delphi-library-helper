{ -----------------------------------------------------------------------------
  Unit Name: LibraryPathsU
  Author: Tristan Marlow
  Purpose: Library Paths

  ----------------------------------------------------------------------------
  Copyright (c) 2016 Tristan David Marlow
  Copyright (c) 2016 Little Earth Solutions
  All Rights Reserved

  This product is protected by copyright and distributed under
  licenses restricting copying, distribution and decompilation

  ----------------------------------------------------------------------------

  History:

  ----------------------------------------------------------------------------- }
unit LibraryPathsU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Generics.Collections;

type
  TLibraryPathType = (dlpNone, dlpAll, dlpSearch, dlpBrowse, dlpDebugDCU);

type
  TLibraryPath = class
  private
    FPath: string;
    FPathType: TLibraryPathType;
    procedure SetEnabled(const Value: boolean);
    procedure SetPath(const Value: string);
    function GetEnabled: boolean;
    procedure SetPathType(const Value: TLibraryPathType);
  protected
  public
    class function PathTypeFromString(AText: string): TLibraryPathType;
    class function PathTypeToString(APathType: TLibraryPathType): string;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property Path: string read FPath write SetPath;
    property PathType: TLibraryPathType read FPathType write SetPathType;
  end;

type
  TLibraryPathList = TObjectList<TLibraryPath>;

type
  TLibraryPaths = class
  private
    FLibraryPathList: TLibraryPathList;
    function GetLibraryPath(AIndex: integer): TLibraryPath;
  protected
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Sort;
    procedure AsStrings(AStrings: TStrings;
      APathType: TLibraryPathType = dlpSearch);
    function AsString(APathType: TLibraryPathType = dlpSearch): string;
    function AsDelimitedString(APathType: TLibraryPathType;
      ADelimeter: string = ';'): string;
    procedure FromString(APaths: string; APathType: TLibraryPathType);
    procedure FromStrings(APaths: TStrings; APathType: TLibraryPathType);
    procedure FromDelimitedString(APaths: string; APathType: TLibraryPathType;
      ADelimeter: string = ';');
    procedure Delete(AIndex: integer);
    function Count: integer;
    function Find(APath: string; ALibraryPathType: TLibraryPathType)
      : TLibraryPath;
    function FindByIndex(APath: string;
      ALibraryPathType: TLibraryPathType): integer;
    procedure Clear(APathType: TLibraryPathType = dlpAll);
    procedure Copy(ALibraryPaths: TLibraryPaths);
    function Add(APath: string; ALibraryPathType: TLibraryPathType): integer;
    property Path[AIndex: integer]: TLibraryPath read GetLibraryPath;
  end;

type
  TLibraryPathTemplate = class
  private
    FCommon: TLibraryPaths;
    FCommonFMX: TLibraryPaths;
    FCommonVCL: TLibraryPaths;
    FWin32: TLibraryPaths;
    FWin64: TLibraryPaths;
    FOSX32: TLibraryPaths;
    FIOS32: TLibraryPaths;
    FIOS64: TLibraryPaths;
    FAndroid32: TLibraryPaths;
    FIOSSimulator: TLibraryPaths;
    FLinux64: TLibraryPaths;
  protected
    procedure LoadSection(AFileName: TFileName; ASectionName: string;
      ALibraryPathList: TLibraryPaths; AAppend: boolean = false);
    function ValidateProductVersion(AFileName: TFileName;
      AProductVersion: integer): boolean;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Load(AFileName: TFileName; AProductVersion: integer);
    property Common: TLibraryPaths read FCommon;
    property CommonFMX: TLibraryPaths read FCommonFMX;
    property CommonVCL: TLibraryPaths read FCommonVCL;
    property Win32: TLibraryPaths read FWin32;
    property Win64: TLibraryPaths read FWin64;
    property OSX32: TLibraryPaths read FOSX32;
    property IOS32: TLibraryPaths read FIOS32;
    property IOS64: TLibraryPaths read FIOS64;
    property IOSSimulator: TLibraryPaths read FIOSSimulator;
    property Android32: TLibraryPaths read FAndroid32;
    property Linux64: TLibraryPaths read FLinux64;
  end;

implementation

uses
  System.IniFiles, System.Generics.Defaults, Winapi.ShLwApi, LoggingU;

{ TLibraryPaths }

constructor TLibraryPathTemplate.Create;
begin
  inherited;
  FCommon := TLibraryPaths.Create;
  FCommonFMX := TLibraryPaths.Create;
  FCommonVCL := TLibraryPaths.Create;
  FWin32 := TLibraryPaths.Create;
  FWin64 := TLibraryPaths.Create;
  FOSX32 := TLibraryPaths.Create;
  FIOS32 := TLibraryPaths.Create;
  FIOS64 := TLibraryPaths.Create;
  FAndroid32 := TLibraryPaths.Create;
  FIOSSimulator := TLibraryPaths.Create;
  FLinux64 := TLibraryPaths.Create;
end;

destructor TLibraryPathTemplate.Destroy;
begin
  try
    FreeAndNil(FCommon);
    FreeAndNil(FCommonFMX);
    FreeAndNil(FCommonVCL);
    FreeAndNil(FWin32);
    FreeAndNil(FWin64);
    FreeAndNil(FOSX32);
    FreeAndNil(FIOS32);
    FreeAndNil(FIOS64);
    FreeAndNil(FAndroid32);
    FreeAndNil(FIOSSimulator);
    FreeAndNil(FLinux64);
  finally
    inherited;
  end;

end;

procedure TLibraryPathTemplate.Load(AFileName: TFileName;
  AProductVersion: integer);
begin

  if ValidateProductVersion(AFileName, AProductVersion) then
  begin

    LoadSection(AFileName, 'common', FCommon);
    LoadSection(AFileName, 'common-' + IntToStr(AProductVersion),
      FCommon, True);

    LoadSection(AFileName, 'common-fmx', FCommonFMX);
    LoadSection(AFileName, 'common-fmx-' + IntToStr(AProductVersion),
      FCommonFMX, True);

    LoadSection(AFileName, 'common-vcl', FCommonVCL);
    LoadSection(AFileName, 'common-vcl-' + IntToStr(AProductVersion),
      FCommonVCL, True);

    LoadSection(AFileName, 'win32', FWin32);
    LoadSection(AFileName, 'win32-' + IntToStr(AProductVersion), FWin32, True);

    LoadSection(AFileName, 'win64', FWin64);
    LoadSection(AFileName, 'win64-' + IntToStr(AProductVersion), FWin64, True);

    LoadSection(AFileName, 'osx32', FOSX32);
    LoadSection(AFileName, 'osx32-' + IntToStr(AProductVersion), FOSX32, True);

    LoadSection(AFileName, 'ios32', FIOS32);
    LoadSection(AFileName, 'ios32-' + IntToStr(AProductVersion), FIOS32, True);

    LoadSection(AFileName, 'ios64', FIOS64);
    LoadSection(AFileName, 'ios64-' + IntToStr(AProductVersion), FIOS64, True);

    LoadSection(AFileName, 'iossimulator', FIOSSimulator);
    LoadSection(AFileName, 'iossimulator-' + IntToStr(AProductVersion),
      FIOSSimulator, True);

    LoadSection(AFileName, 'android32', FAndroid32);
    LoadSection(AFileName, 'android32-' + IntToStr(AProductVersion),
      FAndroid32, True);

    LoadSection(AFileName, 'linux64', FLinux64);
    LoadSection(AFileName, 'linux64-' + IntToStr(AProductVersion),
      FLinux64, True);
  end;

end;

procedure TLibraryPathTemplate.LoadSection(AFileName: TFileName;
  ASectionName: string; ALibraryPathList: TLibraryPaths; AAppend: boolean);
var
  LINIFile: TIniFile;
  LSectionList: TStringList;
  LSectionIdx: integer;
  LPath: string;
  LPathType: TLibraryPathType;
begin
  if not AAppend then
    ALibraryPathList.Clear;
  if FileExists(AFileName) then
  begin
    LINIFile := TIniFile.Create(AFileName);
    LSectionList := TStringList.Create;
    try
      LINIFile.ReadSection(ASectionName, LSectionList);
      for LSectionIdx := 0 to Pred(LSectionList.Count) do
      begin
        LPath := LSectionList[LSectionIdx];
        LPathType := TLibraryPath.PathTypeFromString
          (LINIFile.ReadString(ASectionName, LSectionList[LSectionIdx],
          'None'));
        ALibraryPathList.Add(LPath, LPathType);
      end;
      ALibraryPathList.Sort;
    finally
      FreeAndNil(LSectionList);
      FreeAndNil(LINIFile);
    end;
  end;
end;

function TLibraryPathTemplate.ValidateProductVersion(AFileName: TFileName;
  AProductVersion: integer): boolean;
var
  LINIFile: TIniFile;
begin
  Result := false;
  if FileExists(AFileName) then
  begin
    LINIFile := TIniFile.Create(AFileName);
    try
      Result := SameText('True', LINIFile.ReadString('profile',
        IntToStr(AProductVersion), 'True'));
    finally
      FreeAndNil(LINIFile);
    end;
  end;
end;

{ TLibraryPaths }

function TLibraryPaths.Add(APath: string;
  ALibraryPathType: TLibraryPathType): integer;
var
  LLibraryPath: TLibraryPath;
begin
  Result := -1;
  if Trim(APath) <> '' then
  begin
    Result := FindByIndex(APath, ALibraryPathType);
    if Result = -1 then
    begin
      LLibraryPath := TLibraryPath.Create;
      LLibraryPath.Path := APath;
      LLibraryPath.PathType := ALibraryPathType;
      Result := FLibraryPathList.Add(LLibraryPath);
    end;
  end;
end;

function TLibraryPaths.AsDelimitedString(APathType: TLibraryPathType;
  ADelimeter: string): string;
var
  LStringList: TStringList;
  LIdx: integer;
begin
  Result := '';
  LStringList := TStringList.Create;
  try
    AsStrings(LStringList, APathType);
    for LIdx := 0 to Pred(LStringList.Count) do
    begin
      if (Trim(LStringList[LIdx]) <> '') then
      begin
        if Trim(Result) <> '' then
        begin
          Result := Result + ADelimeter;
        end;
        Result := Result + ExcludeTrailingPathDelimiter(LStringList[LIdx]);
      end;
    end;
  finally
    FreeAndNil(LStringList);
  end;
end;

function TLibraryPaths.AsString(APathType: TLibraryPathType): string;
var
  LStringList: TStringList;
begin
  LStringList := TStringList.Create;
  try
    AsStrings(LStringList, APathType);
    Result := LStringList.Text;
  finally
    FreeAndNil(LStringList);
  end;
end;

procedure TLibraryPaths.AsStrings(AStrings: TStrings;
  APathType: TLibraryPathType);
var
  LLibraryPath: TLibraryPath;
begin
  if Assigned(AStrings) then
  begin
    AStrings.Clear;
    for LLibraryPath in FLibraryPathList do
    begin
      if (LLibraryPath.PathType = APathType) or (APathType = dlpAll) then
      begin
        AStrings.Add(LLibraryPath.Path);
      end;
    end;
  end
  else
  begin
    raise Exception.Create('AStrings is not assigned');
  end;
end;

procedure TLibraryPaths.Clear(APathType: TLibraryPathType);
var
  LIdx: integer;
begin
  if APathType = dlpAll then
  begin
    FLibraryPathList.Clear;
  end
  else
  begin
    LIdx := 0;
    while LIdx < Count do
    begin
      if FLibraryPathList[LIdx].PathType = APathType then
      begin
        FLibraryPathList.Delete(LIdx);
      end
      else
      begin
        Inc(LIdx);
      end;
    end;
  end;

end;

procedure TLibraryPaths.Copy(ALibraryPaths: TLibraryPaths);
var
  LLibraryPath: TLibraryPath;
begin
  Clear;
  for LLibraryPath in FLibraryPathList do
  begin
    Add(LLibraryPath.Path, LLibraryPath.PathType);
  end;
end;

function TLibraryPaths.Count: integer;
begin
  Result := FLibraryPathList.Count;
end;

constructor TLibraryPaths.Create;
begin
  FLibraryPathList := TLibraryPathList.Create(True);
end;

procedure TLibraryPaths.Delete(AIndex: integer);
begin
  FLibraryPathList.Delete(AIndex);
end;

destructor TLibraryPaths.Destroy;
begin
  try
    FreeAndNil(FLibraryPathList);
  finally
    inherited;
  end;
end;

function TLibraryPaths.FindByIndex(APath: string;
  ALibraryPathType: TLibraryPathType): integer;
var
  LIdx: integer;
begin
  Result := -1;
  LIdx := 0;
  while (Result = -1) and (LIdx < Count) do
  begin
    if SameText(FLibraryPathList[LIdx].Path, IncludeTrailingPathDelimiter(APath)
      ) and (FLibraryPathList[LIdx].PathType = ALibraryPathType) then
    begin
      Result := LIdx;
    end;
    Inc(LIdx);
  end;
end;

procedure TLibraryPaths.FromDelimitedString(APaths: string;
  APathType: TLibraryPathType; ADelimeter: string);
var
  LPath: string;
  LPaths: TStringList;
begin
  LPaths := TStringList.Create;
  try
    LPath := APaths;
    LPath := StringReplace(LPath, ADelimeter, #13#10, [rfReplaceAll]);
    LPaths.Text := LPath;
    FromStrings(LPaths, APathType);
  finally
    FreeAndNil(LPaths);
  end;
end;

procedure TLibraryPaths.FromString(APaths: string; APathType: TLibraryPathType);
var
  LPaths: TStringList;
begin
  LPaths := TStringList.Create;
  try
    LPaths.Text := APaths;
    FromStrings(LPaths, APathType);
  finally
    FreeAndNil(LPaths);
  end;
end;

procedure TLibraryPaths.FromStrings(APaths: TStrings;
  APathType: TLibraryPathType);
var
  LPath: string;
begin
  Clear(APathType);
  for LPath in APaths do
  begin
    if Trim(LPath) <> '' then
    begin
      Add(LPath, APathType);
    end;
  end;
  Sort;
end;

function TLibraryPaths.Find(APath: string; ALibraryPathType: TLibraryPathType)
  : TLibraryPath;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := FindByIndex(APath, ALibraryPathType);
  if LIdx <> -1 then
  begin
    Result := FLibraryPathList[LIdx];
  end;

end;

function TLibraryPaths.GetLibraryPath(AIndex: integer): TLibraryPath;
begin
  Result := FLibraryPathList.Items[AIndex];
end;

procedure TLibraryPaths.Sort;
begin
  FLibraryPathList.Sort(TComparer<TLibraryPath>.Construct(
    function(const L, R: TLibraryPath): integer
    begin
      Result := StrCmpLogicalW(PChar(L.Path), PChar(R.Path));
    end));
end;

{ TLibraryPath }

function TLibraryPath.GetEnabled: boolean;
begin
  Result := FPathType = dlpNone;
end;

class function TLibraryPath.PathTypeFromString(AText: string): TLibraryPathType;
begin
  Result := dlpNone;
  if SameText(AText, 'None') then
    Result := dlpNone;
  if SameText(AText, 'Browse') then
    Result := dlpBrowse;
  if SameText(AText, 'DebugDCU') then
    Result := dlpDebugDCU;
  if SameText(AText, 'Search') then
    Result := dlpSearch;
  if SameText(AText, 'All') then
    Result := dlpAll;
  // older entries had true or false for enabled.
  if SameText(AText, 'False') then
    Result := dlpNone;
  if SameText(AText, 'True') then
    Result := dlpSearch;
end;

class function TLibraryPath.PathTypeToString
  (APathType: TLibraryPathType): string;
begin
  case APathType of
    dlpAll:
      Result := 'All';
    dlpSearch:
      Result := 'Search';
    dlpBrowse:
      Result := 'Browse';
    dlpDebugDCU:
      Result := 'DebugDCU';
  else
    begin
      Result := 'None';
    end;
  end;
end;

procedure TLibraryPath.SetEnabled(const Value: boolean);
begin
  FPathType := dlpNone;
end;

procedure TLibraryPath.SetPath(const Value: string);
begin
  FPath := IncludeTrailingPathDelimiter(Value);
end;

procedure TLibraryPath.SetPathType(const Value: TLibraryPathType);
begin
  FPathType := Value;
end;

end.
