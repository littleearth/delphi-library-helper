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
  TLibraryPath = class
  private
    FPath: string;
    FEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    procedure SetPath(const Value: string);
    function GetEnabled: boolean;
  protected
  public
    property Enabled: boolean read GetEnabled write SetEnabled;
    property Path: string read FPath write SetPath;
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
    procedure AsStringList(AStrings: TStrings; AEnabledOnly: boolean = True);
    function Count: integer;
    procedure Clear;
    function Add(APath: string; AEnabled: boolean): integer;
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
  System.IniFiles;

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
        ALibraryPathList.Add(LSectionList[LSectionIdx],
          SameText('True', LINIFile.ReadString(ASectionName,
          LSectionList[LSectionIdx], 'True')));
      end;
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

function TLibraryPaths.Add(APath: string; AEnabled: boolean): integer;
var
  LLibraryPath: TLibraryPath;
begin
  LLibraryPath := TLibraryPath.Create;
  LLibraryPath.Path := APath;
  LLibraryPath.Enabled := AEnabled;
  Result := FLibraryPathList.Add(LLibraryPath);
end;

procedure TLibraryPaths.AsStringList(AStrings: TStrings; AEnabledOnly: boolean);
var
  LLibraryPath: TLibraryPath;
begin
  if AssigneD(AStrings) then
  begin
    AStrings.Clear;
    for LLibraryPath in FLibraryPathList do
    begin
      if (LLibraryPath.Enabled) or (not AEnabledOnly) then
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

procedure TLibraryPaths.Clear;
begin
  FLibraryPathList.Clear;
end;

function TLibraryPaths.Count: integer;
begin
  Result := FLibraryPathList.Count;
end;

constructor TLibraryPaths.Create;
begin
  FLibraryPathList := TLibraryPathList.Create(True);
end;

destructor TLibraryPaths.Destroy;
begin
  try
    FreeAndNil(FLibraryPathList);
  finally
    inherited;
  end;
end;

function TLibraryPaths.GetLibraryPath(AIndex: integer): TLibraryPath;
begin
  Result := FLibraryPathList.Items[AIndex];
end;

{ TLibraryPath }

function TLibraryPath.GetEnabled: boolean;
begin
  Result := (FEnabled);
end;

procedure TLibraryPath.SetEnabled(const Value: boolean);
begin
  FEnabled := Value;
end;

procedure TLibraryPath.SetPath(const Value: string);
begin
  FPath := IncludeTrailingPathDelimiter(Value);
end;

end.
