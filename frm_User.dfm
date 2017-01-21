object frmUser: TfrmUser
  Left = 0
  Top = 0
  ActiveControl = UserName
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #1044#1077#1092#1080#1085#1080#1088#1072#1085#1077' '#1085#1072' '#1087#1072#1088#1086#1083#1072' '#1079#1072' '#1076#1086#1089#1090#1098#1087' '
  ClientHeight = 147
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 65
    Top = 8
    Width = 297
    Height = 97
    BevelOuter = bvNone
    TabOrder = 0
    object Panel3: TPanel
      AlignWithMargins = True
      Left = 8
      Top = 62
      Width = 273
      Height = 24
      BevelOuter = bvNone
      TabOrder = 2
      object Label3: TLabel
        Left = 0
        Top = 2
        Width = 101
        Height = 21
        AutoSize = False
        Caption = #1055#1086#1090#1088'. '#1075#1088#1091#1087#1072
        FocusControl = WorkGroup
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object SpeedButton1: TSpeedButton
        Left = 246
        Top = 0
        Width = 23
        Height = 23
        Flat = True
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFA0726EA0726EA0726FA0726EA0726EA0726EA0726EA0
          726EA0726EA0726EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFA0726EEDDFD1
          E9DACBE9D8C8E8D7C5E8D7C4E8D6C5E8D7C6EDDFCCA0726EFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFBF8A85FAF2E5F5ECE0F3EADDF3E8D9F1E6D6F1E5D4F1
          E6D4F8EDD9A0726EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC48F87F9F3EA
          F9DFC3FAD7B5F9D7B6F8D7B4F8D6B1F6DABCF6ECD9A0726EFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFCA9588F9F6EFF8E9D8F8E4D0F7E3CEF6E2CBF6E0C8F2
          DFC9F4EAD7A0726EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFD19D89FBF8F2
          F8DFC5F6D5B3F8D7B6FAD7B5F2D0ADDEC7ACDCD5C7A0726EFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFD6A38BFCF9F5E8DED3D3C6BADED0C2E8D9CACFC0B1A7
          9C91A7A298967A73FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFDCA98CFCFCFB
          9FA6B7AF967FB09780BEA3895572995B7CA460617677625DFF00FFFF00FFFF00
          FF5B5757464242464242E0AE8DFEFEFE5572995572999A98975572997CC4E755
          7299425583514C62FF00FF8F8F905454545B57573B3A3C5B5555E3B18EFFFFFF
          557299CBEFF65572997CC4E75572997CC4E75572993E598C374D848F8F909195
          9B5B5757524D4E5B5555EDBD92FFFFFFFCFCFD557299CBEFF65572997CC4E755
          72997CC4E75EADEA4D99E48F8F90F1F1F15B5757686463635E5EEDBD92FAF2ED
          FAF2EDF8F0EC557299A5E9F45572997CC4E76EBEEF5EACE94D99E48F8F90F1F1
          F15B5757757272767474EDBD92E0B398E1B599E1B599DCB19855729997C2D9C4
          EBF66EC0F15EAEEB3B5D9B8F8F90F1F1F15B57578C8C8C8C8C8CFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FF6A96C05A8DC7556DA4FF00FF8F8F908F8F
          905B57578C8C8C7A7979FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FF5B57575B57575B5757}
        OnClick = SpeedButton1Click
      end
      object WorkGroup: TDBLookupComboBox
        Left = 107
        Top = 0
        Width = 133
        Height = 21
        BevelInner = bvSpace
        BevelOuter = bvNone
        BevelKind = bkTile
        KeyField = 'ID'
        ListField = 'GroupName'
        ListSource = srcWorkgroupList
        TabOrder = 0
      end
    end
    object Panel4: TPanel
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 273
      Height = 24
      BevelOuter = bvNone
      TabOrder = 0
      object Label4: TLabel
        Left = 0
        Top = 0
        Width = 101
        Height = 21
        AutoSize = False
        Caption = #1055#1086#1090#1088#1077#1073'. '#1080#1084#1077
        FocusControl = UserName
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object UserName: TEdit
        Left = 107
        Top = 0
        Width = 133
        Height = 21
        BevelInner = bvSpace
        BevelKind = bkTile
        BorderStyle = bsNone
        TabOrder = 0
      end
    end
    object Panel5: TPanel
      AlignWithMargins = True
      Left = 8
      Top = 35
      Width = 273
      Height = 24
      BevelOuter = bvNone
      TabOrder = 1
      object Label5: TLabel
        Left = 0
        Top = 0
        Width = 101
        Height = 21
        AutoSize = False
        Caption = #1055#1072#1088#1086#1083#1072
        FocusControl = Password
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Password: TEdit
        Left = 107
        Top = 0
        Width = 133
        Height = 21
        BevelInner = bvSpace
        BevelKind = bkTile
        BorderStyle = bsNone
        PasswordChar = '#'
        TabOrder = 0
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 121
    Width = 380
    Height = 26
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnOk: TBitBtn
      Left = 65
      Top = 0
      Width = 75
      Height = 25
      Caption = #1047#1072#1087#1080#1089
      TabOrder = 0
      OnClick = btnOkClick
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 154
      Top = 0
      Width = 87
      Height = 25
      Caption = #1054#1090#1082#1072#1079
      TabOrder = 1
      Kind = bkCancel
    end
    object btnDelete: TBitBtn
      Left = 255
      Top = 0
      Width = 91
      Height = 25
      Caption = #1041#1077#1079' '#1076#1086#1089#1098#1087
      TabOrder = 2
      OnClick = btnDeleteClick
      Kind = bkNo
    end
  end
  object srcWorkgroupList: TDataSource
    DataSet = dmGUI.WorkGroupList
    Left = 320
    Top = 32
  end
end
