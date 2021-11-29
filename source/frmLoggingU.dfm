object frmLogging: TfrmLogging
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Log'
  ClientHeight = 561
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  PixelsPerInch = 96
  TextHeight = 13
  object memoLog: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 778
    Height = 555
    Align = alClient
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object TimerLog: TTimer
    Interval = 5000
    OnTimer = TimerLogTimer
    Left = 60
    Top = 48
  end
end
