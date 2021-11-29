const
WM_CLOSE = 16;

procedure TaskKill(AProcessName : string);
var
	ResultCode : integer;
begin
  Exec('TASKKILL.EXE', '/IM:' + AProcessName + ' /F', '', SW_HIDE,
     ewWaitUntilTerminated, ResultCode);
end;

function TaskClose(AClassName : string; AFriendlyName : string) : Boolean;
var winHwnd: longint;
    retVal : boolean;
    strProg: string;
begin
 Result := true;
 strProg := AClassName;
 Repeat
    try
    winHwnd := FindWindowByClassName(strProg);
    Log('winHwnd: ' + strProg + inttostr(winHwnd));
    if winHwnd <> 0 then
    begin
      if MsgBox(AFriendlyName + ' is running, close and continue with installation?', mbConfirmation, MB_YESNO or MB_DEFBUTTON2) = IDYES then
      begin
        retVal:=postmessage(winHwnd,WM_CLOSE,0,0);
        Sleep(5000);
      end else
      begin
        Result := False;
      end;
    end;
  except
    ShowExceptionMessage;
    Result := False;
  end;
  Until (Result = False) or (winHwnd = 0);
end;