{ -----------------------------------------------------------------------------
  Unit Name: FileVersionInformationU
  Author: Tristan Marlow
  Purpose: File Version Information

  ----------------------------------------------------------------------------
  Copyright (c) 2016 Tristan David Marlow
  Copyright (c) 2016 Little Earth Solutions
  All Rights Reserved

  This product is protected by copyright and distributed under
  licenses restricting copying, distribution and decompilation

  ----------------------------------------------------------------------------

  History: 03/05/2007 - First Release.
  18/06/2014 - Improvements for Locale

  ----------------------------------------------------------------------------- }
unit FileVersionInformationU;

interface

uses Classes, Forms, Windows, Dialogs, SysUtils;

type
  EFileVersionInformationException = class(Exception);

type
  TFileVersionInformation = class(TObject)
  private
    FExceptionOnError: Boolean;
    FFileName: TFileName;
    FLanguageCodepage: string;
    FLanguage: string;
    FCompanyName: string;
    FFileDescription: string;
    FFileVersion: string;
    FInternalName: string;
    FLegalCopyright: string;
    FLegalTrademarks: string;
    FOriginalFileName: string;
    FProductName: string;
    FProductVersion: string;
    FComments: string;
    FMajorVersion: Word;
    FMinorVersion: Word;
    FReleaseVersion: Word;
    FBuild: Word;
    FDebugBuild: Boolean;
    FPrivateBuild: Boolean;
    FSpecialBuild: Boolean;
    FInfoInferred: Boolean;
    FPatched: Boolean;
    FPreRelease: Boolean;
  protected
    procedure SetFilename(const AFileName: TFileName);
    procedure ClearValues;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property ExceptionOnError: Boolean read FExceptionOnError
      write FExceptionOnError;
    property FileName: TFileName read FFileName write SetFilename;
    property LanguageCodepage: string read FLanguageCodepage;
    property Language: string read FLanguage;
    property CompanyName: string read FCompanyName;
    property FileDescription: string read FFileDescription;
    property FileVersion: string read FFileVersion;
    property InternalName: string read FInternalName;
    property LegalCopyright: string read FLegalCopyright;
    property LegalTrademarks: string read FLegalTrademarks;
    property OriginalFileName: string read FOriginalFileName;
    property ProductName: string read FProductName;
    property ProductVersion: string read FProductVersion;
    property Comments: string read FComments;
    property MajorVersion: Word read FMajorVersion;
    property MinorVersion: Word read FMinorVersion;
    property ReleaseVersion: Word read FReleaseVersion;
    property Build: Word read FBuild;
    property DebugBuild: Boolean read FDebugBuild;
    property PreRelease: Boolean read FPreRelease;
    property Patched: Boolean read FPatched;
    property PrivateBuild: Boolean read FPrivateBuild;
    property InfoInferred: Boolean read FInfoInferred;
    property SpecialBuild: Boolean read FSpecialBuild;
  end;

type
  TApplicationVersionInfo = class(TFileVersionInformation)
  public
    constructor Create; override;
  end;

implementation

constructor TFileVersionInformation.Create;
begin
  inherited;
  ClearValues;
  FExceptionOnError := False;
end;

procedure TFileVersionInformation.ClearValues;
begin
  FFileDescription := '';
  FLanguageCodepage := '';
  FLanguage := '';
  FCompanyName := '';
  FFileVersion := '0.0.0.0';
  FInternalName := '';
  FLegalCopyright := '';
  FLegalTrademarks := '';
  FOriginalFileName := '';
  FProductName := '';
  FProductVersion := '0.0.0.0';
  FComments := '';
end;

destructor TFileVersionInformation.Destroy;
begin
  inherited Destroy;
end;

procedure TFileVersionInformation.SetFilename(const AFileName: TFileName);
type
  PLandCodepage = ^TLandCodepage;

  TLandCodepage = record
    wLanguage, wCodePage: Word;
  end;
var
  dummy, len: cardinal;
  buf, pntr: pointer;
  FixedPtr: PVSFixedFileInfo;
begin
  FFileName := AFileName;
  ClearValues;
  if FileExists(FFileName) then
  begin
    try
      len := GetFileVersionInfoSize(PChar(FFileName), dummy);
      if len = 0 then
        RaiseLastOSError;
      GetMem(buf, len);
      try
        if not GetFileVersionInfo(PChar(FileName), 0, len, buf) then
          RaiseLastOSError;

        if not VerQueryValue(buf, '\VarFileInfo\Translation\', pntr, len) then
          RaiseLastOSError;

        FLanguageCodepage := Format('%.4x%.4x', [PLandCodepage(pntr)^.wLanguage,
          PLandCodepage(pntr)^.wCodePage]);

        VerLanguageName(PLandCodepage(pntr)^.wLanguage, pntr, 256);
        FLanguage := PChar(pntr);

        if VerQueryValue(buf, PChar('\StringFileInfo\' + FLanguageCodepage +
          '\CompanyName'), pntr, len) { and (@len <> nil) } then
          FCompanyName := PChar(pntr);
        if VerQueryValue(buf, PChar('\StringFileInfo\' + FLanguageCodepage +
          '\FileDescription'), pntr, len) { and (@len <> nil) } then
          FFileDescription := PChar(pntr);
        if VerQueryValue(buf, PChar('\StringFileInfo\' + FLanguageCodepage +
          '\FileVersion'), pntr, len) { and (@len <> nil) } then
          FFileVersion := PChar(pntr);
        if VerQueryValue(buf, PChar('\StringFileInfo\' + FLanguageCodepage +
          '\InternalName'), pntr, len) { and (@len <> nil) } then
          FInternalName := PChar(pntr);
        if VerQueryValue(buf, PChar('\StringFileInfo\' + FLanguageCodepage +
          '\LegalCopyright'), pntr, len) { and (@len <> nil) } then
          FLegalCopyright := PChar(pntr);
        if VerQueryValue(buf, PChar('\StringFileInfo\' + FLanguageCodepage +
          '\LegalTrademarks'), pntr, len) { and (@len <> nil) } then
          FLegalTrademarks := PChar(pntr);
        if VerQueryValue(buf, PChar('\StringFileInfo\' + FLanguageCodepage +
          '\OriginalFileName'), pntr, len) { and (@len <> nil) } then
          FOriginalFileName := PChar(pntr);
        if VerQueryValue(buf, PChar('\StringFileInfo\' + FLanguageCodepage +
          '\ProductName'), pntr, len) { and (@len <> nil) } then
          FProductName := PChar(pntr);
        if VerQueryValue(buf, PChar('\StringFileInfo\' + FLanguageCodepage +
          '\ProductVersion'), pntr, len) { and (@len <> nil) } then
          FProductVersion := PChar(pntr);
        if VerQueryValue(buf, PChar('\StringFileInfo\' + FLanguageCodepage +
          '\Comments'), pntr, len) { and (@len <> nil) } then
          FComments := PChar(pntr);
        // if VerQueryValue(buf, PChar('\StringFileInfo\' + FLanguageCodepage +
        // '\PrivateBuild'), pntr, len) { and (@len <> nil) } then
        // FPrivateBuild := PChar(pntr);
        // if VerQueryValue(buf, PChar('\StringFileInfo\' + FLanguageCodepage +
        // '\SpecialBuild'), pntr, len) { and (@len <> nil) } then
        // FSpecialBuild := PChar(pntr);
        if VerQueryValue(buf, '\', pointer(FixedPtr), len) then
        begin
          FMajorVersion := LongRec(FixedPtr.dwFileVersionMS).Hi;
          // Major Version
          FMinorVersion := LongRec(FixedPtr.dwFileVersionMS).Lo;
          // Minor Version
          FReleaseVersion := LongRec(FixedPtr.dwFileVersionLS).Hi;
          // Release Version
          FBuild := LongRec(FixedPtr.dwFileVersionLS).Lo; // Build
          FDebugBuild := (FixedPtr^.dwFileFlags and VS_FF_DEBUG) <> 0;

          FPreRelease := (FixedPtr^.dwFileFlags and VS_FF_PRERELEASE) <> 0;
          FPatched := (FixedPtr^.dwFileFlags and VS_FF_PATCHED) <> 0;
          FPrivateBuild := (FixedPtr^.dwFileFlags and VS_FF_PRIVATEBUILD) <> 0;
          FInfoInferred := (FixedPtr^.dwFileFlags and VS_FF_INFOINFERRED) <> 0;
          FSpecialBuild := (FixedPtr^.dwFileFlags and VS_FF_SPECIALBUILD) <> 0;
        end;
      finally
        FreeMem(buf);
      end;
    except
      on E: Exception do
      begin
        if FExceptionOnError then
        begin
          raise EFileVersionInformationException.Create(E.Message);
        end;
      end;
    end;
  end
  else
  begin
    if FExceptionOnError then
    begin
      raise EFileVersionInformationException.CreateFmt('"%s" does not exist.',
        [FFileName]);
    end;
  end;
end;

// TApplicationVersionInfo

constructor TApplicationVersionInfo.Create;
begin
  inherited;
  ExceptionOnError := False;
  FileName := ParamStr(0);
end;

end.
