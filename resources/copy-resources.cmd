@echo off
if [%1]==[] goto usage
if [%2]==[] goto usage
if [%3]==[] goto usage
if [%4]==[] goto usage

set folder=%2
set type=%1
set platform=%3
set config=%4
set source=%~dp0

echo xcopy %source%%type%\all\* %folder% /E /C /Y
xcopy "%source%%type%\all\*" %folder% /E /C /Y
xcopy "%source%%type%\%platform%\all\*" %folder% /E /C /Y
xcopy "%source%%type%\%platform%\%config%\*" %folder% /E /C /Y

xcopy "%source%common\%platform%\all\*" %folder% /E /C /Y
xcopy "%source%common\%platform%\%config%\*" %folder% /E /C /Y
xcopy "%source%common\all\*" %folder% /E /C /Y


goto exit

:usage
echo.
echo Incorrect parameters
echo post-resources.cmd type destination platform config
echo eg Client "C:\My Folder" Win32 Debug
goto error

:error
EXIT /B 1

:exit