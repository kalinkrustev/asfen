object frmOrganisation: TfrmOrganisation
  Left = 0
  Top = 0
  Caption = #1054#1073#1077#1082#1090#1080
  ClientHeight = 675
  ClientWidth = 774
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 435
    Top = 88
    Height = 587
    Beveled = True
    Color = clMedGray
    ParentColor = False
    ResizeStyle = rsUpdate
    ExplicitLeft = 460
    ExplicitTop = 49
    ExplicitHeight = 600
  end
  object TreePanel: TPanel
    Left = 0
    Top = 88
    Width = 435
    Height = 587
    Align = alLeft
    BevelOuter = bvNone
    Constraints.MinWidth = 200
    TabOrder = 0
  end
  object ActionToolBar1: TActionToolBar
    Left = 0
    Top = 0
    Width = 774
    Height = 88
    ActionManager = ActionManager1
    Anchors = []
    Caption = 'ActionToolBar1'
    ColorMap.HighlightColor = 14410210
    ColorMap.BtnSelectedColor = clBtnFace
    ColorMap.UnusedColor = 14410210
    Spacing = 0
  end
  object Panel3: TPanel
    Left = 438
    Top = 88
    Width = 336
    Height = 587
    Align = alClient
    BevelOuter = bvNone
    Constraints.MinWidth = 200
    TabOrder = 2
    object Splitter2: TSplitter
      Left = 0
      Top = 156
      Width = 336
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      Beveled = True
      Color = clMedGray
      ParentColor = False
      ResizeStyle = rsUpdate
      ExplicitLeft = 1
      ExplicitTop = 43
      ExplicitWidth = 316
    end
    object ScrollBox2: TScrollBox
      Left = 0
      Top = 159
      Width = 336
      Height = 428
      Align = alBottom
      BorderStyle = bsNone
      Constraints.MinHeight = 100
      TabOrder = 0
      DesignSize = (
        320
        428)
      object GroupPrivate: TGroupBox
        Left = 0
        Top = 0
        Width = 317
        Height = 484
        Anchors = [akLeft, akTop, akRight]
        Caption = #1057#1083#1091#1078#1077#1073#1085#1080' '#1076#1072#1085#1085#1080
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        DesignSize = (
          317
          484)
        object Label3: TLabel
          Left = 6
          Top = 143
          Width = 87
          Height = 21
          AutoSize = False
          Caption = #1048#1084#1077
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label6: TLabel
          Left = 6
          Top = 251
          Width = 87
          Height = 21
          AutoSize = False
          Caption = #1052#1086#1073'. '#1090#1077#1083#1077#1092#1086#1085
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label1: TLabel
          Left = 6
          Top = 296
          Width = 37
          Height = 13
          Caption = #1040#1076#1088#1077#1089
        end
        object Label5: TLabel
          Left = 6
          Top = 315
          Width = 87
          Height = 21
          AutoSize = False
          Caption = #1053#1072#1089'. '#1084#1103#1089#1090#1086
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object btnCity2: TSpeedButton
          Left = 281
          Top = 314
          Width = 23
          Height = 23
          Anchors = [akTop, akRight]
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
          OnClick = btnCity2Click
          ExplicitLeft = 260
        end
        object Label8: TLabel
          Left = 6
          Top = 396
          Width = 87
          Height = 21
          AutoSize = False
          Caption = #1040#1076#1088#1077#1089
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label2: TLabel
          Left = 6
          Top = 16
          Width = 87
          Height = 21
          AutoSize = False
          Caption = #1057#1083#1091#1078#1077#1073#1077#1085' '#1090#1077#1083'.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label7: TLabel
          Left = 6
          Top = 224
          Width = 87
          Height = 21
          AutoSize = False
          Caption = #1044#1086#1084#1072#1096#1077#1085' '#1090#1077#1083'.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label4: TLabel
          Left = 6
          Top = 70
          Width = 65
          Height = 21
          AutoSize = False
          Caption = #1044#1083#1098#1078#1085#1086#1089#1090
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object btnPosition: TSpeedButton
          Left = 281
          Top = 70
          Width = 23
          Height = 23
          Anchors = [akTop, akRight]
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
          OnClick = btnPositionClick
        end
        object Label9: TLabel
          Left = 6
          Top = 124
          Width = 73
          Height = 13
          Caption = #1051#1080#1095#1085#1080' '#1076#1072#1085#1085#1080
        end
        object Label12: TLabel
          Left = 6
          Top = 43
          Width = 87
          Height = 21
          AutoSize = False
          Caption = 'EMail'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label13: TLabel
          Left = 6
          Top = 170
          Width = 87
          Height = 21
          AutoSize = False
          Caption = #1055#1088#1077#1079#1080#1084#1077
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label14: TLabel
          Left = 6
          Top = 197
          Width = 87
          Height = 21
          AutoSize = False
          Caption = #1060#1072#1084#1080#1083#1080#1103
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label15: TLabel
          Left = 6
          Top = 97
          Width = 87
          Height = 21
          AutoSize = False
          Caption = #1055#1091#1085#1082#1090' '#1079#1072' '#1091#1087#1088'.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label16: TLabel
          Left = 6
          Top = 278
          Width = 87
          Height = 21
          AutoSize = False
          Caption = #1055#1072#1088#1086#1083#1072
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label18: TLabel
          Left = 6
          Top = 342
          Width = 87
          Height = 21
          AutoSize = False
          Caption = #1054#1073#1097'./ '#1086#1073#1083#1072#1089#1090
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label20: TLabel
          Left = 6
          Top = 369
          Width = 87
          Height = 21
          AutoSize = False
          Caption = #1056#1072#1081#1086#1085
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object btnRaion2: TSpeedButton
          Left = 281
          Top = 369
          Width = 23
          Height = 23
          Anchors = [akTop, akRight]
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
          OnClick = btnRaion1Click
          ExplicitLeft = 284
        end
        object btnSetName: TSpeedButton
          Left = 77
          Top = 70
          Width = 21
          Height = 21
          Hint = #1055#1088#1077#1093#1074#1098#1088#1083#1080' '#1074' '#1080#1084#1077#1090#1086
          Flat = True
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            1800000000000003000000000000000000000000000000000000FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF527BC6FF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FF317BEF527BC6296BC6FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF397BE7007BFF0073F7527BC6FF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FF009CFF008CFF008CFF527BC6FF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00B5FF008CFF00
            94FF527BC6527BC6FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FF00B5FF08C6FF009CFF009CFF527BC6FF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00
            B5FF08BDFF00ADFF009CFF527BC6FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FF527BC6527BC6527BC600C6FF08FFFF31F7FF10BDFF00ADFF527B
            C6527BC6FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF29ADFF00C6FF00EFFF00
            F7FF00F7FF00FFFF4AEFFF18CEFF00A5FF527BC6FF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FF39A5FF00C6FF00EFFF00F7FF00EFFF00DEFF00FFFF00FFFF39EF
            FF08C6FF527BC6FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF08C6FF39E7FF4A
            EFFF42F7FF18FFFF00FFFF00FFFF08FFFF21FFFF527BC6FF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FF31D6FF08F7FF00FFFF00F7FF00D6FF00B5
            FF527BC6FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FF31D6FF00F7FF00EFFF00ADFF00A5FF527BC6FF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF31D6FF42DE
            FF10D6FF5AA5FF527BC6FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FF31D6FF52A5FF527BC6}
          Margin = 0
          ParentShowHint = False
          ShowHint = True
          OnClick = btnSetNameClick
        end
        object Name1: TDBEdit
          Left = 99
          Top = 143
          Width = 205
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          BevelInner = bvSpace
          BevelKind = bkTile
          BorderStyle = bsNone
          DataField = 'Name1'
          DataSource = srcTree
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
        end
        object Phone2: TDBEdit
          Left = 99
          Top = 251
          Width = 205
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          BevelInner = bvSpace
          BevelKind = bkTile
          BorderStyle = bsNone
          DataField = 'Phone2'
          DataSource = srcTree
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 8
        end
        object City2: TDBLookupComboBox
          Left = 99
          Top = 315
          Width = 176
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          BevelInner = bvNone
          BevelOuter = bvNone
          BevelKind = bkTile
          Ctl3D = True
          DataField = 'City2'
          DataSource = srcTree
          DropDownRows = 20
          DropDownWidth = 250
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          KeyField = 'EKATTE'
          ListField = 'Name;EKATTE;Kmetstvo'
          ListSource = srcCityList
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 10
        end
        object Address2: TDBMemo
          Left = 99
          Top = 396
          Width = 205
          Height = 80
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelInner = bvSpace
          BevelKind = bkTile
          BorderStyle = bsNone
          Constraints.MinHeight = 21
          DataField = 'Address2'
          DataSource = srcTree
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 14
        end
        object Phone: TDBEdit
          Left = 99
          Top = 16
          Width = 205
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          BevelInner = bvSpace
          BevelKind = bkTile
          BorderStyle = bsNone
          DataField = 'Phone'
          DataSource = srcTree
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
        object Phone3: TDBEdit
          Left = 99
          Top = 224
          Width = 205
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          BevelInner = bvSpace
          BevelKind = bkTile
          BorderStyle = bsNone
          DataField = 'Phone3'
          DataSource = srcTree
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 7
        end
        object Position: TDBLookupComboBox
          Left = 99
          Top = 70
          Width = 176
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          BevelInner = bvNone
          BevelOuter = bvNone
          BevelKind = bkTile
          Ctl3D = True
          DataField = 'Position'
          DataSource = srcTree
          DropDownRows = 20
          DropDownWidth = 450
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          KeyField = 'Code'
          ListField = 'PositionName;Code'
          ListSource = srcPosition
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 2
        end
        object EMail: TDBEdit
          Left = 99
          Top = 43
          Width = 205
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          BevelInner = bvSpace
          BevelKind = bkTile
          BorderStyle = bsNone
          DataField = 'EMail'
          DataSource = srcTree
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
        end
        object Name2: TDBEdit
          Left = 99
          Top = 170
          Width = 205
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          BevelInner = bvSpace
          BevelKind = bkTile
          BorderStyle = bsNone
          DataField = 'Name2'
          DataSource = srcTree
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 5
        end
        object Name3: TDBEdit
          Left = 99
          Top = 197
          Width = 205
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          BevelInner = bvSpace
          BevelKind = bkTile
          BorderStyle = bsNone
          DataField = 'Name3'
          DataSource = srcTree
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 6
        end
        object Site: TDBEdit
          Left = 99
          Top = 97
          Width = 205
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          BevelInner = bvSpace
          BevelKind = bkTile
          BorderStyle = bsNone
          DataField = 'Site'
          DataSource = srcTree
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
        end
        object DTMF: TDBEdit
          Left = 99
          Top = 278
          Width = 205
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          BevelInner = bvSpace
          BevelKind = bkTile
          BorderStyle = bsNone
          DataField = 'DTMF'
          DataSource = srcTree
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 9
        end
        object Obstina2: TDBEdit
          Left = 99
          Top = 342
          Width = 102
          Height = 21
          BevelInner = bvSpace
          BevelKind = bkTile
          BorderStyle = bsNone
          DataField = 'Obstina2'
          DataSource = srcTree
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 11
        end
        object Oblast2: TDBEdit
          Left = 207
          Top = 342
          Width = 97
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          BevelInner = bvSpace
          BevelKind = bkTile
          BorderStyle = bsNone
          DataField = 'Oblast2'
          DataSource = srcTree
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 12
        end
        object Raion2: TDBLookupComboBox
          Left = 99
          Top = 369
          Width = 176
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          BevelInner = bvNone
          BevelOuter = bvNone
          BevelKind = bkTile
          Ctl3D = True
          DataField = 'Raion2'
          DataSource = srcTree
          DropDownRows = 20
          DropDownWidth = 250
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          KeyField = 'Raion'
          ListField = 'Name;Raion'
          ListSource = srcRaion
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 13
          OnEnter = Raion2Enter
          OnExit = Raion2Exit
        end
      end
    end
    object GroupWork: TGroupBox
      Left = 0
      Top = 0
      Width = 336
      Height = 156
      Align = alClient
      Caption = #1057#1083#1091#1078#1077#1073#1077#1085' '#1072#1076#1088#1077#1089
      Constraints.MinHeight = 129
      Ctl3D = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 1
      DesignSize = (
        336
        156)
      object Label10: TLabel
        Left = 6
        Top = 17
        Width = 87
        Height = 21
        AutoSize = False
        Caption = #1053#1072#1089'. '#1084#1103#1089#1090#1086
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object btnCity1: TSpeedButton
        Left = 300
        Top = 17
        Width = 23
        Height = 23
        Anchors = [akTop, akRight]
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
        OnClick = btnCity1Click
        ExplicitLeft = 252
      end
      object Label11: TLabel
        Left = 6
        Top = 98
        Width = 87
        Height = 21
        AutoSize = False
        Caption = #1040#1076#1088#1077#1089
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label17: TLabel
        Left = 6
        Top = 44
        Width = 87
        Height = 21
        AutoSize = False
        Caption = #1054#1073#1097'./ '#1086#1073#1083#1072#1089#1090
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label19: TLabel
        Left = 6
        Top = 71
        Width = 87
        Height = 21
        AutoSize = False
        Caption = #1056#1072#1081#1086#1085
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object btnRaion1: TSpeedButton
        Left = 300
        Top = 71
        Width = 23
        Height = 23
        Anchors = [akTop, akRight]
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
        OnClick = btnRaion1Click
        ExplicitLeft = 284
      end
      object City1: TDBLookupComboBox
        Left = 99
        Top = 17
        Width = 195
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        BevelInner = bvNone
        BevelOuter = bvNone
        BevelKind = bkTile
        Ctl3D = True
        DataField = 'City1'
        DataSource = srcTree
        DropDownRows = 20
        DropDownWidth = 250
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        KeyField = 'EKATTE'
        ListField = 'Name;EKATTE;Kmetstvo'
        ListSource = srcCityList
        ParentCtl3D = False
        ParentFont = False
        TabOrder = 0
      end
      object Address1: TDBMemo
        Left = 99
        Top = 98
        Width = 224
        Height = 42
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelInner = bvSpace
        BevelKind = bkTile
        BorderStyle = bsNone
        Constraints.MinHeight = 21
        DataField = 'Address1'
        DataSource = srcTree
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
      end
      object Obstina1: TDBEdit
        Left = 99
        Top = 44
        Width = 102
        Height = 21
        BevelInner = bvSpace
        BevelKind = bkTile
        BorderStyle = bsNone
        DataField = 'Obstina1'
        DataSource = srcTree
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
      object Oblast1: TDBEdit
        Left = 207
        Top = 44
        Width = 116
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        BevelInner = bvSpace
        BevelKind = bkTile
        BorderStyle = bsNone
        DataField = 'Oblast1'
        DataSource = srcTree
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
      object Raion1: TDBLookupComboBox
        Left = 99
        Top = 71
        Width = 195
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        BevelInner = bvNone
        BevelOuter = bvNone
        BevelKind = bkTile
        Ctl3D = True
        DataField = 'Raion1'
        DataSource = srcTree
        DropDownRows = 20
        DropDownWidth = 250
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        KeyField = 'Raion'
        ListField = 'Name;Raion'
        ListSource = srcRaion
        ParentCtl3D = False
        ParentFont = False
        TabOrder = 3
        OnEnter = Raion1Enter
        OnExit = Raion1Exit
      end
    end
  end
  object srcTree: TDataSource
    OnStateChange = srcTreeStateChange
    OnDataChange = srcTreeDataChange
    Left = 312
    Top = 240
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items.AutoHotKeys = False
        Items = <
          item
            Action = acNewOrganisation
            ImageIndex = 0
          end
          item
            Action = acNewDivision
            ImageIndex = 0
          end
          item
            Action = acNewPosition
            ImageIndex = 0
          end
          item
            Action = acEdit
            ImageIndex = 42
          end
          item
            Action = acDelete
            ImageIndex = 19
          end
          item
            Visible = False
            Action = acRecode
            ImageIndex = 1
          end
          item
            Items = <
              item
                Action = acImportAdd
                ImageIndex = 28
              end
              item
                Action = acImportReplace
                ImageIndex = 28
              end>
            Caption = #1048#1084#1087#1086#1088#1090
            ImageIndex = 28
          end
          item
            Action = acExportXML
            ImageIndex = 12
          end
          item
            Action = acClose
            ImageIndex = 24
          end>
        ActionBar = ActionToolBar1
        GlyphLayout = blGlyphTop
      end>
    Images = dmImages.Images
    Left = 400
    Top = 208
    StyleName = 'XP Style'
    object acNewOrganisation: TAction
      Caption = #1044#1086#1073#1072#1074#1080' '#1086#1088#1075#1072#1085#1080#1079#1072#1094#1080#1103
      ImageIndex = 0
      OnExecute = acNewOrganisationExecute
    end
    object acNewDivision: TAction
      Caption = #1044#1086#1073#1072#1074#1080' '#1086#1090#1076#1077#1083
      ImageIndex = 0
      OnExecute = acNewDivisionExecute
    end
    object acNewPosition: TAction
      Caption = #1044#1086#1073#1072#1074#1080' '#1076#1083#1098#1078#1085#1086#1089#1090
      ImageIndex = 0
      OnExecute = acNewPositionExecute
    end
    object acEdit: TAction
      Caption = #1055#1088#1077#1080#1084#1077#1085#1091#1074#1072#1085#1077
      ImageIndex = 42
      OnExecute = acEditExecute
    end
    object acDelete: TAction
      Caption = #1048#1079#1090#1088#1080#1074#1072#1085#1077
      ImageIndex = 19
      OnExecute = acDeleteExecute
    end
    object acRecode: TAction
      Caption = #1055#1088#1077#1082#1086#1076#1080#1088#1072#1085#1077
      ImageIndex = 1
      Visible = False
      OnExecute = acRecodeExecute
    end
    object acImportAdd: TAction
      Caption = #1048#1084#1087#1086#1088#1090'-'#1076#1086#1073#1072#1074#1103#1085#1077
      ImageIndex = 28
      OnExecute = acImportAddExecute
    end
    object acClose: TAction
      Caption = #1048#1079#1093#1086#1076
      ImageIndex = 24
      OnExecute = acCloseExecute
    end
    object acExportXML: TAction
      Caption = #1045#1082#1089#1087#1086#1088#1090
      ImageIndex = 12
      OnExecute = acExportXMLExecute
    end
    object acImportReplace: TAction
      Caption = #1048#1084#1087#1086#1088#1090'-'#1087#1086#1076#1084#1103#1085#1072
      ImageIndex = 28
      OnExecute = acImportReplaceExecute
    end
  end
  object srcCityList: TDataSource
    Left = 312
    Top = 272
  end
  object srcPosition: TDataSource
    Left = 312
    Top = 304
  end
  object srcRaion: TDataSource
    Left = 312
    Top = 336
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.xls'
    FileName = 'organisation'
    Filter = 'XML|*.xml|Excel|*.xls'
    FilterIndex = 2
    InitialDir = '.'
    Options = [ofNoChangeDir, ofEnableSizing, ofDontAddToRecent]
    Left = 400
    Top = 248
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.xlw'
    FileName = 'organisation'
    Filter = 'XML|*.xml|Excel|*.xls|Excel worksheet|*.xlw'
    FilterIndex = 3
    InitialDir = '.'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofNoChangeDir, ofEnableSizing, ofDontAddToRecent]
    Left = 400
    Top = 280
  end
end
