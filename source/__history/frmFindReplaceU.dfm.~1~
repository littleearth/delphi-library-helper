object frmFindReplace: TfrmFindReplace
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Find Replace'
  ClientHeight = 310
  ClientWidth = 451
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 445
    Height = 110
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Panel3: TPanel
      Left = 336
      Top = 0
      Width = 109
      Height = 110
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object cbIgnoreCase: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 103
        Height = 17
        Align = alTop
        Caption = 'Ignore case'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
    end
    object Panel4: TPanel
      Left = 0
      Top = 0
      Width = 336
      Height = 110
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 46
        Width = 330
        Height = 13
        Align = alTop
        Caption = 'Replace'
        ExplicitTop = 49
        ExplicitWidth = 38
      end
      object Label2: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 330
        Height = 13
        Align = alTop
        Caption = 'Find'
        ExplicitWidth = 20
      end
      object editFind: TComboBox
        AlignWithMargins = True
        Left = 3
        Top = 19
        Width = 330
        Height = 21
        Align = alTop
        TabOrder = 0
      end
      object editReplace: TComboBox
        AlignWithMargins = True
        Left = 3
        Top = 62
        Width = 330
        Height = 21
        Align = alTop
        TabOrder = 1
      end
    end
  end
  object Panel2: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 266
    Width = 445
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btnOk: TBitBtn
      AlignWithMargins = True
      Left = 236
      Top = 3
      Width = 100
      Height = 35
      Action = ActionReplace
      Align = alRight
      Caption = 'Replace'
      TabOrder = 0
      ExplicitLeft = 286
    end
    object btnCancel: TBitBtn
      AlignWithMargins = True
      Left = 342
      Top = 3
      Width = 100
      Height = 35
      Action = ActionCancel
      Align = alRight
      Caption = 'Cancel'
      TabOrder = 1
      ExplicitLeft = 367
    end
  end
  object GroupBoxDestination: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 119
    Width = 445
    Height = 141
    Align = alClient
    Caption = 'Library'
    TabOrder = 1
    object GridPanel1: TGridPanel
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 435
      Height = 118
      Align = alClient
      BevelOuter = bvNone
      ColumnCollection = <
        item
          Value = 50.000000000000000000
        end
        item
          Value = 50.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = cbAndroid32
          Row = 0
        end
        item
          Column = 1
          Control = cbIOS32
          Row = 0
        end
        item
          Column = 0
          Control = cbIOS64
          Row = 1
        end
        item
          Column = 1
          Control = cbIOSSimulator
          Row = 1
        end
        item
          Column = 0
          Control = cbOSX
          Row = 2
        end
        item
          Column = 1
          Control = cbWin32
          Row = 2
        end
        item
          Column = 0
          Control = cbWin64
          Row = 3
        end
        item
          Column = 1
          Control = cbLinux64
          Row = 3
        end>
      ExpandStyle = emFixedSize
      PopupMenu = PopupMenuLibrary
      RowCollection = <
        item
          SizeStyle = ssAbsolute
          Value = 25.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 25.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 25.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 25.000000000000000000
        end>
      TabOrder = 0
      object cbAndroid32: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 211
        Height = 19
        Align = alClient
        Caption = 'Android32'
        PopupMenu = PopupMenuLibrary
        TabOrder = 0
      end
      object cbIOS32: TCheckBox
        AlignWithMargins = True
        Left = 220
        Top = 3
        Width = 212
        Height = 19
        Align = alClient
        Caption = 'IOS32'
        PopupMenu = PopupMenuLibrary
        TabOrder = 1
      end
      object cbIOS64: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 28
        Width = 211
        Height = 19
        Align = alClient
        Caption = 'IOS64'
        PopupMenu = PopupMenuLibrary
        TabOrder = 2
      end
      object cbIOSSimulator: TCheckBox
        AlignWithMargins = True
        Left = 220
        Top = 28
        Width = 212
        Height = 19
        Align = alClient
        Caption = 'IOSSimulator'
        PopupMenu = PopupMenuLibrary
        TabOrder = 3
      end
      object cbOSX: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 53
        Width = 211
        Height = 19
        Align = alClient
        Caption = 'OSX'
        PopupMenu = PopupMenuLibrary
        TabOrder = 4
      end
      object cbWin32: TCheckBox
        AlignWithMargins = True
        Left = 220
        Top = 53
        Width = 212
        Height = 19
        Align = alClient
        Caption = 'Win32'
        PopupMenu = PopupMenuLibrary
        TabOrder = 5
      end
      object cbWin64: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 78
        Width = 211
        Height = 19
        Align = alClient
        Caption = 'Win64'
        PopupMenu = PopupMenuLibrary
        TabOrder = 6
      end
      object cbLinux64: TCheckBox
        AlignWithMargins = True
        Left = 220
        Top = 78
        Width = 212
        Height = 19
        Align = alClient
        Caption = 'Linux64'
        PopupMenu = PopupMenuLibrary
        TabOrder = 7
      end
    end
  end
  object ActionList: TActionList
    Images = dmDelphiLibraryHelper.ImageListCommon
    Left = 56
    Top = 112
    object ActionReplace: TAction
      Caption = 'Replace'
      ImageIndex = 13
      OnExecute = ActionReplaceExecute
    end
    object ActionCancel: TAction
      Caption = 'Cancel'
      ImageIndex = 3
      OnExecute = ActionCancelExecute
    end
  end
  object PopupMenuLibrary: TPopupMenu
    Images = dmDelphiLibraryHelper.ImageListCommon
    Left = 152
    Top = 111
    object CheckAll1: TMenuItem
      Caption = 'Check All'
      OnClick = CheckAll1Click
    end
    object UncheckAll1: TMenuItem
      Caption = 'Uncheck All'
      OnClick = UncheckAll1Click
    end
  end
end
