object frmAbout: TfrmAbout
  Left = 200
  Top = 108
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 272
  ClientWidth = 298
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 292
    Height = 219
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    ParentColor = True
    TabOrder = 0
    object Comments: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 111
      Width = 282
      Height = 13
      Align = alTop
      Alignment = taCenter
      Caption = 'Comments'
      Layout = tlCenter
      WordWrap = True
      IsControl = True
      ExplicitWidth = 50
    end
    object Bevel1: TBevel
      AlignWithMargins = True
      Left = 5
      Top = 96
      Width = 282
      Height = 9
      Align = alTop
      Shape = bsTopLine
    end
    object Panel3: TPanel
      Left = 2
      Top = 2
      Width = 288
      Height = 91
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Panel4: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 90
        Height = 85
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object ProgramIcon: TImage
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 84
          Height = 79
          Align = alClient
          Picture.Data = {
            0954506E67496D61676589504E470D0A1A0A0000000D49484452000000600000
            00600806000000E298773800000006624B474400FF00FF00FFA0BDA793000003
            704944415478DAED9B4F48146118C6DF292DD44AA2430611041D2AE8D8A95B74
            8AEE41B788D4408820293AC4D2A108A30804B5E81A748F4E41274F1D83E81005
            115487084DFBA7EEF4A85934B6CEBBBBF3CEB3E6F3838F9D611FE7FBF363BF99
            5798C40495843D80B58E049091003212404602C84800190920B36A05A4237618
            A3BFF86B16D7923E7BC21E5323AC2A0169C5DAACC74EE0F03CDA81CCD7CFD06E
            D87BBB9F546C963D562FAB42407AD33AACD37A7178166D774EFC35DA6DDB6863
            C949FBC61E7B1E2D2D201DB36EABDA398CB21FA7DBEBFCF30F96DAA8ADB35BD8
            9E26D873A9454B0A48876D1B369B411C9E41DBD2E4E526D146B0290D2503F691
            3DB72C2D2520BD6B3BB1508318D5299C76157CF969FC22EE41EC5072DADEB2E7
            BA444B08C056B3171F97B040C7F1B921B8BB1F98F5037C5EC5D6F4823D77AA80
            F48E1DC41E7F198747D1D695DC7D15ED117ABD92F4DA53D61A500460E18F60FA
            151C1E624D3CC338445420E271D91D9726204DD1D7981DC3E1056B9D85CF328E
            76DDFAEC619260432C81700139C553435467CCBEBE339BFDB278DED669D6D183
            3DACB8BB4769455D98803A8B2737F38BFFF925AE3F9799C87AB3CD7B20A1BDD0
            69841775850B68B278CA65FA8DD9CCE4BFBF6B47C5D0B5ABF0359A27ACA82B4C
            40C1C5534D269EA3AF6A8DC9E057D0BD2FAAE7050A2FEA9A16105C3C2D634501
            7890EDDE1F3D82050A2BEA1A165072F1F467E69C2DA8164D1775750B20174F36
            F7DD6CEA556937612F0D17756E01AD543CFD7E0C9D5A3C6FDB84C7D01DB4C5CF
            525751E717305A4E61F2BF90F4FBD656028290003212404602C8480099C20564
            F934B0B290ADC37F5F7BADE5BD484050DE8B0404E5BD484050DE4BE3FF8CCBB9
            29676F426B2DEF450282F25E242028EF450282F25E242028EF450282F25E2420
            28EF450282F25E242028EF450282F25E242028EF450282F25E242028EF450282
            F25E242028EF450282F25E242028EF450282F25E242028EF450282F25E242028
            EF450282F25E242028EF450282F25E242028EF450282F25E242028EF450282F2
            5E242028EF450282F25E242028EF450282F25EF4925E107A4B928C0490910032
            4D0BC87B294DD447AD97F824A02424808C0490C915A09B6CB92CDDA425808404
            90910032CB04080E12404602C8480019092023016424808C0490910032124046
            02C8480019092023016424808C0490F9095015618E2CDBE9A10000000049454E
            44AE426082}
          Stretch = True
          IsControl = True
          ExplicitLeft = 8
          ExplicitTop = 8
          ExplicitWidth = 65
          ExplicitHeight = 57
        end
      end
      object Panel5: TPanel
        AlignWithMargins = True
        Left = 99
        Top = 3
        Width = 186
        Height = 85
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object ProductName: TLabel
          Left = 0
          Top = 0
          Width = 67
          Height = 13
          Align = alClient
          Alignment = taCenter
          Caption = 'Product Name'
          WordWrap = True
          IsControl = True
        end
        object CompanyName: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 72
          Width = 75
          Height = 13
          Align = alBottom
          Alignment = taCenter
          Caption = 'Company Name'
          IsControl = True
        end
        object Version: TLabel
          Left = 0
          Top = 56
          Width = 36
          Height = 13
          Align = alBottom
          Alignment = taCenter
          Caption = '0.0.0.0'
          IsControl = True
        end
      end
    end
    object memoCredits: TMemo
      AlignWithMargins = True
      Left = 5
      Top = 127
      Width = 282
      Height = 87
      Align = alClient
      Alignment = taCenter
      Lines.Strings = (
        'Development'
        'Tristan Marlow'
        ''
        'Icons from Icons8')
      ReadOnly = True
      TabOrder = 1
      ExplicitTop = 150
      ExplicitHeight = 64
    end
  end
  object Panel2: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 228
    Width = 292
    Height = 41
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object OKButton: TButton
      AlignWithMargins = True
      Left = 102
      Top = 5
      Width = 88
      Height = 31
      Margins.Left = 100
      Margins.Right = 100
      Align = alClient
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
end
