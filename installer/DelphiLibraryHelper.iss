; ----------------------------------------------------------------------------
; Delphi Library Helper - Installation Script
; Author: Tristan Marlow
; Purpose: Install application
;
; ----------------------------------------------------------------------------
; Copyright (c) 2016 Tristan David Marlow
; Copyright (c) 2016 Little Earth Solutions
; All Rights Reserved
;
; This product is protected by copyright and distributed under
; licenses restricting copying, distribution and decompilation
;
; ----------------------------------------------------------------------------
;
; History: 14/02/2011 - First Release.
;
;-----------------------------------------------------------------------------
#define ConstAppVersion GetFileVersion("..\windows\bin\win32\release\DelphiLibraryHelper.exe") ; define variable
#define ConstAppName "Delphi Library Helper"
#define ConstAppMutex "Delphi Library Helper"
#define ConstAppDescription "Delphi Library Helper"
#define ConstAppPublisher "Little Earth Solutions"
#define ConstAppCopyright "Copyright (C) 2017 Little Earth Solutions"
#define ConstAppURL "http://www.littleearthsolutions.net/"
#define ConstAppExeName "DelphiLibraryHelper.exe"

[Setup]
AppId={{A74907F6-9360-4932-9504-2A7A12437B9F}
AppMutex={#ConstAppMutex}
AppName={#ConstAppName}
AppVersion={#ConstAppName} {#ConstAppVersion}
AppPublisher={#ConstAppPublisher}
AppPublisherURL={#ConstAppURL}
AppSupportURL={#ConstAppURL}
AppUpdatesURL={#ConstAppURL}
AppCopyright={#ConstAppCopyright}
VersionInfoCompany={#ConstAppPublisher}
VersionInfoDescription={#ConstAppName}
VersionInfoCopyright={#ConstAppCopyright}
VersionInfoVersion={#ConstAppVersion}
VersionInfoTextVersion={#ConstAppVersion}
OutputDir=output
OutputBaseFilename=DelphiLibraryHelper-Setup-{#ConstAppVersion}
UninstallDisplayName={#ConstAppName}
DefaultDirName={pf}\{#ConstAppPublisher}\{#ConstAppName}
DefaultGroupName={#ConstAppPublisher}\{#ConstAppName}
AllowNoIcons=true
MinVersion=0,5.0.2195sp3
InfoBeforeFile=..\docs\{#ConstAppName} - Release Notes.rtf
LicenseFile=..\docs\{#ConstAppName} - License.rtf
UninstallDisplayIcon={app}\{#ConstAppExeName}
InternalCompressLevel=ultra
Compression=lzma/ultra

[Types]
Name: typical; Description: Typical Installation
Name: custom; Description: Custom Installation; Flags: iscustom

[Components]
Name: code; Description: Source Code; Types: custom
Name: program; Description: Program Files; Types: typical custom

[Tasks]
Name: "delphitools"; Description: "Add to delphi tools menu"; Components: program

[Files]
Source: "..\windows\bin\win32\release\{#ConstAppExeName}"; DestDir: "{app}"; Flags: promptifolder replacesameversion; Components: program
Source: "..\docs\templates\*.dlht"; DestDir: "{app}\templates";  Components: program
Source: "..\images\DelphiLibraryHelper.ico"; DestDir: "{app}"; DestName: "DelphiLibraryHelper.ico"
; Source Code Components
Source: "..\docs\*.*"; DestDir: "{app}\source\docs\"; Flags: recursesubdirs; Components: code
Source: "..\installer\*.*"; DestDir: "{app}\source\installer\"; Components: code
Source: "..\images\*.*"; DestDir: "{app}\source\images\"; Flags: recursesubdirs; Components: code
Source: "..\windows\*.pas"; DestDir: "{app}\source\windows\"; Flags: recursesubdirs; Components: code
Source: "..\windows\*.dfm"; DestDir: "{app}\source\windows\"; Flags: recursesubdirs; Components: code
Source: "..\windows\*.dpr"; DestDir: "{app}\source\windows\"; Flags: recursesubdirs; Components: code
Source: "..\windows\*.res"; DestDir: "{app}\source\windows\"; Flags: recursesubdirs; Components: code
Source: "..\windows\*.dproj"; DestDir: "{app}\source\windows\"; Flags: recursesubdirs; Components: code

[Icons]
; Program Components
Name: {group}\{#ConstAppName}; Filename: {app}\{#ConstAppExeName}; WorkingDir: {app}; Components: program
; Source Code Components
Name: {group}\{#ConstAppName} Source Code; Filename: {app}\Source; Flags: foldershortcut; Components: code; Tasks: 

[Run]
Filename: "{app}\{#ConstAppExeName}"; WorkingDir: "{app}"; Flags: nowait postinstall runasoriginaluser; Description: "Launch {#ConstAppName}"

[Registry]
; Add Delphi Library Helper
Root: "HKCU"; Subkey: "Software\Embarcadero\BDS\19.0\Transfer\Delphi Library Helper"; ValueType: string; ValueName: "Params"; Tasks: delphitools
Root: "HKCU"; Subkey: "Software\Embarcadero\BDS\19.0\Transfer\Delphi Library Helper"; ValueType: string; ValueName: "Path"; ValueData: "{app}\DelphiLibraryHelper.exe"; Tasks: delphitools
Root: "HKCU"; Subkey: "Software\Embarcadero\BDS\19.0\Transfer\Delphi Library Helper"; ValueType: string; ValueName: "Title"; ValueData: "Delphi Library Helper"; Tasks: delphitools
Root: "HKCU"; Subkey: "Software\Embarcadero\BDS\19.0\Transfer\Delphi Library Helper"; ValueType: string; ValueName: "WorkingDir"; ValueData: "{app}\DelphiLibraryHelper"; Tasks: delphitools
