object frmProgress: TfrmProgress
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'Please Wait...'
  ClientHeight = 125
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlProgress: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 394
    Height = 119
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 5
    ParentBackground = False
    TabOrder = 0
    ExplicitWidth = 357
    object Panel1: TPanel
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 378
      Height = 59
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 10
      ExplicitTop = 10
      ExplicitWidth = 337
      object imgProgress: TImage
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 45
        Height = 53
        Align = alLeft
        Center = True
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000400000
          00400806000000AA6971DE00000006624B474400FF00FF00FFA0BDA793000002
          B54944415478DAED9ACF6B134114C7BB4B36D6250A22A227E9419A930AE6D2B3
          1EF51AC9263186A5841AA51EFC07F22F5835D2A5CAB224BB62AE7AD4B397148A
          A78820F556B014A12C31091BBFA339C8D2CC6E3686C98FF7856167C8DB79EF7D
          66E6CD422B2D2DB824D101881601101D80681100D10188160110E9BC582C2EB3
          A7699AED850290CBE5AE4A92F4A8DFEF6B7F82902407FDE7F57AFDF3DC022895
          4A8AEBBA1A12DDC43035C4AC09185BAAAA3A866174E702403A9DBEA0288A8EC4
          36305C09F9DA3780DA467BE538CE8F9904806DBE86471949DC45F2A7A2CC8177
          7FE1DDB7E856713C3E4D3D006C7315DB3C8BC01F607823C09C25F402B6C8512A
          A3BF1660BF0BDB6A229160C7C39D2A00F97CFE3A827B826E1A6D99637A84640D
          59965F5B96F5E5DF1F0A85C2AAE7793AE62961788E33C731E6B0F1ACD66AB53D
          610070B64FE36CDF43302CE05480F91E5BBD6EB76B371A8D639EA1AEEB67DAED
          B606480FF1CEB580799BB031E2F1B815F52A1D194026935989C5621B705CC4F0
          22C79455F137B07B6ADB76334A70D96C3605C08F995B3485637A003B133BE825
          7CED4F0280A469DA1DAC0ABBC26EA2C91CDBEF6CB5D14C54F0832889FB850FA6
          4B9D4EE7FEA0565CE6987A681F01620BBEDFA1DF1F0B00CEE5F95EAFB70EC7EB
          185EE19832471F98E36432F9BE52A978FF2371BF30AFDC6AB56E0F16E25640FC
          5FB1083BD8AD3BA837872303C0F67B86C40BE89EE53871D957DCD2DF82B43B89
          A487098597DD32E5C1D7A4CA31FD8966E11ADD1C0900EE71DEF6095DD426ADB0
          4513004ECC352A809913012000046034007EF981F8279CF6DF87890010000240
          00080001200004800010000240000800012000048000100002400008C04913CE
          BAE8EF020480008C07206C519916858D9F00849D60D6450008000118EFFF03E6
          5504407400A245004407205A04407400A2B5F0007E031919446E3EDC48050000
          000049454E44AE426082}
        Proportional = True
        Stretch = True
        ExplicitLeft = 24
        ExplicitTop = 17
        ExplicitHeight = 42
      end
      object lblCaption: TLabel
        AlignWithMargins = True
        Left = 54
        Top = 3
        Width = 321
        Height = 56
        Align = alClient
        AutoSize = False
        Caption = 'Please Wait...'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
        ExplicitTop = 6
        ExplicitWidth = 280
      end
    end
    object Panel2: TPanel
      AlignWithMargins = True
      Left = 8
      Top = 73
      Width = 378
      Height = 38
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 10
      ExplicitTop = 75
      ExplicitWidth = 337
      ExplicitHeight = 34
      object progressNormal: TProgressBar
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 372
        Height = 17
        Align = alTop
        Position = 50
        Smooth = True
        TabOrder = 0
        ExplicitWidth = 331
      end
    end
  end
end
