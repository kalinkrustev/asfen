object frmExport: TfrmExport
  Left = 0
  Top = 0
  Caption = #1045#1082#1089#1087#1086#1088#1090
  ClientHeight = 383
  ClientWidth = 323
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  DesignSize = (
    323
    383)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 8
    Top = 166
    Width = 307
    Height = 113
    Anchors = [akLeft, akTop, akRight]
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    object RingGroupName: TCheckBox
      Left = 8
      Top = 16
      Width = 200
      Height = 17
      Caption = #1048#1084#1077#1085#1072' '#1085#1072' '#1075#1088#1091#1087#1080' '#1079#1072' '#1086#1087#1086#1074#1077#1089#1090#1103#1074#1072#1085#1077
      Enabled = False
      TabOrder = 0
    end
    object MessageName: TCheckBox
      Left = 8
      Top = 39
      Width = 200
      Height = 17
      Caption = #1048#1084#1077#1085#1072' '#1085#1072' '#1089#1098#1086#1073#1097#1077#1085#1080#1103
      Enabled = False
      TabOrder = 1
    end
    object ScriptName: TCheckBox
      Left = 8
      Top = 62
      Width = 200
      Height = 17
      Caption = #1048#1084#1077#1085#1072' '#1085#1072' '#1089#1094#1077#1085#1072#1088#1080#1080
      Enabled = False
      TabOrder = 2
    end
    object EventName: TCheckBox
      Left = 8
      Top = 85
      Width = 200
      Height = 17
      Caption = #1048#1084#1077#1085#1072' '#1085#1072' '#1089#1098#1073#1080#1090#1080#1103
      Enabled = False
      TabOrder = 3
    end
  end
  object Panel1: TPanel
    Left = 8
    Top = 16
    Width = 307
    Height = 137
    Anchors = [akLeft, akTop, akRight]
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object OrganisationName: TCheckBox
      Left = 8
      Top = 16
      Width = 201
      Height = 17
      Caption = #1048#1084#1077#1085#1072' '#1085#1072' '#1086#1090#1076#1077#1083#1080' '#1080' '#1076#1083#1098#1078#1085#1086#1089#1090#1080
      Enabled = False
      TabOrder = 0
    end
    object Address1: TCheckBox
      Left = 8
      Top = 39
      Width = 201
      Height = 17
      Caption = #1057#1083#1091#1078#1077#1073#1077#1085' '#1072#1076#1088#1077#1089
      Enabled = False
      TabOrder = 1
    end
    object PersonName: TCheckBox
      Left = 8
      Top = 62
      Width = 201
      Height = 17
      Caption = #1051#1080#1095#1085#1080' '#1076#1072#1085#1085#1080' - '#1080#1084#1077
      Enabled = False
      TabOrder = 2
    end
    object Address2: TCheckBox
      Left = 8
      Top = 85
      Width = 201
      Height = 17
      Caption = #1051#1080#1095#1085#1080' '#1076#1072#1085#1085#1080' - '#1072#1076#1088#1077#1089
      Enabled = False
      TabOrder = 3
    end
    object Site: TCheckBox
      Left = 8
      Top = 108
      Width = 201
      Height = 17
      Caption = #1055#1091#1085#1082#1090' '#1079#1072' '#1091#1087#1088#1072#1074#1083#1077#1085#1080#1077
      Enabled = False
      TabOrder = 4
    end
  end
  object Organisation: TCheckBox
    Left = 16
    Top = 9
    Width = 161
    Height = 17
    Caption = #1054#1088#1075#1072#1085#1080#1079#1072#1094#1080#1086#1085#1085#1072' '#1089#1090#1088#1091#1082#1090#1091#1088#1072
    TabOrder = 0
    OnClick = OrganisationClick
  end
  object Settings: TCheckBox
    Left = 16
    Top = 159
    Width = 161
    Height = 17
    Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1079#1072' '#1086#1087#1086#1074#1077#1089#1090#1103#1074#1072#1085#1077
    TabOrder = 2
    OnClick = SettingsClick
  end
  object Users: TCheckBox
    Left = 16
    Top = 285
    Width = 161
    Height = 17
    Caption = #1055#1086#1090#1088#1077#1073#1080#1090#1077#1083#1080' '#1080' '#1087#1088#1072#1074#1072
    TabOrder = 4
  end
  object Stats: TCheckBox
    Left = 16
    Top = 308
    Width = 161
    Height = 17
    Caption = #1057#1090#1072#1090#1080#1089#1090#1080#1082#1072
    TabOrder = 5
  end
  object Panel: TPanel
    Left = 0
    Top = 342
    Width = 323
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 6
    object btnOk: TBitBtn
      Left = 64
      Top = 9
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = btnOkClick
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
    end
    object btnCancel: TBitBtn
      Left = 184
      Top = 9
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
end
