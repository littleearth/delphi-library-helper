object frmSearch: TfrmSearch
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  BorderStyle = bsSingle
  Caption = 'Search'
  ClientHeight = 459
  ClientWidth = 834
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 828
    Height = 62
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Panel3: TPanel
      Left = 719
      Top = 0
      Width = 109
      Height = 62
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
      Width = 719
      Height = 62
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object Label2: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 713
        Height = 13
        Align = alTop
        Caption = 'Find'
        ExplicitWidth = 20
      end
      object editFind: TComboBox
        AlignWithMargins = True
        Left = 3
        Top = 22
        Width = 713
        Height = 21
        Align = alTop
        TabOrder = 0
        OnKeyDown = editFindKeyDown
      end
    end
  end
  object Panel2: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 415
    Width = 828
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object btnOk: TBitBtn
      AlignWithMargins = True
      Left = 619
      Top = 3
      Width = 100
      Height = 35
      Action = ActionFind
      Align = alRight
      Caption = 'Search'
      TabOrder = 0
    end
    object btnCancel: TBitBtn
      AlignWithMargins = True
      Left = 725
      Top = 3
      Width = 100
      Height = 35
      Action = ActionClose
      Align = alRight
      Caption = 'Close'
      TabOrder = 1
    end
  end
  object GroupBoxDestination: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 71
    Width = 828
    Height = 143
    Align = alTop
    Caption = 'Library'
    TabOrder = 1
    object GridPanel1: TGridPanel
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 818
      Height = 120
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
          Value = 25.000000000000000000
        end
        item
          Value = 25.000000000000000000
        end
        item
          Value = 25.000000000000000000
        end
        item
          Value = 25.000000000000000000
        end>
      TabOrder = 0
      object cbAndroid32: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 403
        Height = 24
        Align = alClient
        Caption = 'Android32'
        PopupMenu = PopupMenuLibrary
        TabOrder = 0
        ExplicitHeight = 19
      end
      object cbIOS32: TCheckBox
        AlignWithMargins = True
        Left = 412
        Top = 3
        Width = 403
        Height = 24
        Align = alClient
        Caption = 'IOS32'
        PopupMenu = PopupMenuLibrary
        TabOrder = 1
        ExplicitHeight = 19
      end
      object cbIOS64: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 33
        Width = 403
        Height = 24
        Align = alClient
        Caption = 'IOS64'
        PopupMenu = PopupMenuLibrary
        TabOrder = 2
        ExplicitTop = 28
        ExplicitHeight = 19
      end
      object cbIOSSimulator: TCheckBox
        AlignWithMargins = True
        Left = 412
        Top = 33
        Width = 403
        Height = 24
        Align = alClient
        Caption = 'IOSSimulator'
        PopupMenu = PopupMenuLibrary
        TabOrder = 3
        ExplicitTop = 28
        ExplicitHeight = 19
      end
      object cbOSX: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 63
        Width = 403
        Height = 24
        Align = alClient
        Caption = 'OSX'
        PopupMenu = PopupMenuLibrary
        TabOrder = 4
        ExplicitTop = 53
        ExplicitHeight = 19
      end
      object cbWin32: TCheckBox
        AlignWithMargins = True
        Left = 412
        Top = 63
        Width = 403
        Height = 24
        Align = alClient
        Caption = 'Win32'
        PopupMenu = PopupMenuLibrary
        TabOrder = 5
        ExplicitTop = 53
        ExplicitHeight = 19
      end
      object cbWin64: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 93
        Width = 403
        Height = 24
        Align = alClient
        Caption = 'Win64'
        PopupMenu = PopupMenuLibrary
        TabOrder = 6
        ExplicitTop = 78
        ExplicitHeight = 19
      end
      object cbLinux64: TCheckBox
        AlignWithMargins = True
        Left = 412
        Top = 93
        Width = 403
        Height = 24
        Align = alClient
        Caption = 'Linux64'
        PopupMenu = PopupMenuLibrary
        TabOrder = 7
        ExplicitTop = 78
        ExplicitHeight = 19
      end
    end
  end
  object ListViewSearch: TListView
    AlignWithMargins = True
    Left = 3
    Top = 220
    Width = 828
    Height = 189
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'Entry'
      end
      item
        AutoSize = True
        Caption = 'Path'
      end
      item
        Caption = 'Library'
        Width = 120
      end>
    GridLines = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 2
    ViewStyle = vsReport
  end
  object ActionList: TActionList
    Images = dmDelphiLibraryHelper.ImageListCommon
    Left = 56
    Top = 112
    object ActionFind: TAction
      Caption = 'Search'
      ImageIndex = 15
      OnExecute = ActionFindExecute
    end
    object ActionClose: TAction
      Caption = 'Close'
      ImageIndex = 3
      OnExecute = ActionCloseExecute
    end
  end
  object PopupMenuLibrary: TPopupMenu
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
