unit LoggingU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Generics.Collections;

type
  TLogLevel = (logDebug, logInformation);

type
  TLoggerBase = class
  private
    FLogLevel: TLogLevel;
    procedure DoLog(AMessage: string); overload;
    procedure DoLog(const AFormat: string; const Args: array of const);
      overload;
    procedure SetLogLevel(const Value: TLogLevel);
  protected
    procedure InternalLog(AMessage: string); virtual; abstract;
    procedure InitializeLog; virtual; abstract;
    procedure FinalizeLog; virtual; abstract;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Log(AMessage: string);
    procedure Error(AMessage: string); overload;
    procedure Error(AException: Exception; AMessage: string = ''); overload;
    procedure Debug(AProcedure, AMessage: string);
    property LogLevel: TLogLevel read FLogLevel write SetLogLevel;
  end;

  TSimpleLogger = class(TLoggerBase)
  private
    FLogCache: TStringList;
    FMaxCacheSize: integer;
    function GetCache: string;
    procedure SetMaxCacheSize(const Value: integer);
  protected
    procedure InternalLog(AMessage: string); override;
    procedure InitializeLog; override;
    procedure FinalizeLog; override;
  public
    property Cache: string read GetCache;
    property MaxCacheSize: integer read FMaxCacheSize write SetMaxCacheSize;
  end;

var
  _Logging: TSimpleLogger;

procedure Log(const AFormat: string; const Args: array of const); overload;
procedure Log(AMessage: string); overload;

procedure Debug(AProcedure: string; const AFormat: string;
  const Args: array of const); overload;
procedure Debug(AProcedure, AMessage: string); overload;

procedure Error(const AFormat: string; const Args: array of const); overload;
procedure Error(AMessage: string); overload;
procedure Error(AException: Exception; AMessage: string = ''); overload;
procedure Error(AException: Exception; const AFormat: string;
  const Args: array of const); overload;

implementation

{ TLoggerBase }

constructor TLoggerBase.Create;
begin
{$IFDEF DEBUG}
  FLogLevel := logDebug;
{$ELSE}
  FLogLevel := logInformation;
{$ENDIF}
  InitializeLog;
end;

procedure TLoggerBase.Debug(AProcedure, AMessage: string);
begin
  if (FLogLevel = logDebug) then
  begin
    DoLog('[DEBUG]:(%s):%s', [AProcedure, AMessage]);
  end;
end;

destructor TLoggerBase.Destroy;
begin
  try
    FinalizeLog;
  finally
    inherited;
  end;
end;

procedure TLoggerBase.DoLog(const AFormat: string; const Args: array of const);
begin
  DoLog(Format(AFormat, Args));
end;

procedure TLoggerBase.DoLog(AMessage: string);
begin
  InternalLog(AMessage);
end;

procedure TLoggerBase.Error(AMessage: string);
begin
  DoLog('[ERROR]: %s', [AMessage]);
end;

procedure TLoggerBase.Error(AException: Exception; AMessage: string);
begin
  DoLog('[ERROR]: %s %s', [AException.Message, AMessage]);
end;

procedure TLoggerBase.Log(AMessage: string);
begin
  DoLog('[LOG]: %s', [AMessage]);
end;

procedure TLoggerBase.SetLogLevel(const Value: TLogLevel);
begin
  FLogLevel := Value;
end;

procedure Log(const AFormat: string; const Args: array of const);
begin
  _Logging.Log(Format(AFormat, Args));
end;

procedure Log(AMessage: string);
begin
  _Logging.Log(AMessage);
end;

procedure Debug(AProcedure: string; const AFormat: string;
  const Args: array of const);
begin
  _Logging.Debug(AProcedure, Format(AFormat, Args));
end;

procedure Debug(AProcedure, AMessage: string);
begin
  _Logging.Debug(AProcedure, AMessage);
end;

procedure Error(const AFormat: string; const Args: array of const);
begin
  _Logging.Error(Format(AFormat, Args));
end;

procedure Error(AMessage: string);
begin
  _Logging.Error(AMessage);
end;

procedure Error(AException: Exception; AMessage: string = '');
begin
  _Logging.Error(AException, AMessage);
end;

procedure Error(AException: Exception; const AFormat: string;
  const Args: array of const);
begin
  _Logging.Error(AException, Format(AFormat, Args));
end;

{ TSimpleLogger }

procedure TSimpleLogger.FinalizeLog;
begin
  FreeAndNil(FLogCache);
end;

function TSimpleLogger.GetCache: string;
begin
  Result := '';
  if Assigned(FLogCache) then
  begin
    Result := FLogCache.Text;
  end;
end;

procedure TSimpleLogger.InitializeLog;
begin
  FLogCache := TStringList.Create;
  FMaxCacheSize := 1000;
end;

procedure TSimpleLogger.InternalLog(AMessage: string);
begin
  FLogCache.Insert(0, AMessage);
  while FLogCache.Count > FMaxCacheSize do
  begin
    FLogCache.Delete(FLogCache.Count - 1);
  end;

end;

procedure TSimpleLogger.SetMaxCacheSize(const Value: integer);
begin
  FMaxCacheSize := Value;
end;

initialization

_Logging := TSimpleLogger.Create;

finalization

FreeAndNil(_Logging);

end.
