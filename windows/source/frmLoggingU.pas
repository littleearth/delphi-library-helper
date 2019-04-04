unit frmLoggingU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TfrmLogging = class(TForm)
    memoLog: TMemo;
    TimerLog: TTimer;
    procedure TimerLogTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmLogging: TfrmLogging;

implementation

uses
  LoggingU;

{$R *.dfm}

procedure TfrmLogging.TimerLogTimer(Sender: TObject);
begin
  memoLog.Lines.Text := _Logging.Cache;
end;

end.
