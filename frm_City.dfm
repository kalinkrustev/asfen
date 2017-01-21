object frmCity: TfrmCity
  Left = 0
  Top = 0
  Caption = #1053#1072#1089#1077#1083#1077#1085#1080' '#1084#1077#1089#1090#1072
  ClientHeight = 419
  ClientWidth = 663
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
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object TreePanel: TPanel
    Left = 0
    Top = 43
    Width = 381
    Height = 376
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object ActionToolBar1: TActionToolBar
    Left = 0
    Top = 0
    Width = 663
    Height = 43
    ActionManager = ActionManager1
    Anchors = []
    Caption = 'ActionToolBar1'
    Color = 13948116
    ColorMap = dmImages.ColorMap
    Spacing = 0
  end
  object EditPanel: TPanel
    Left = 381
    Top = 43
    Width = 282
    Height = 376
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    OnExit = EditPanelExit
    DesignSize = (
      282
      376)
    object Label1: TLabel
      Left = 55
      Top = 6
      Width = 80
      Height = 13
      Caption = #1053#1072#1089#1077#1083#1077#1085#1086' '#1084#1103#1089#1090#1086
    end
    object Label2: TLabel
      Left = 136
      Top = 179
      Width = 70
      Height = 13
      Caption = #1050#1086#1076' '#1082#1084#1077#1090#1089#1090#1074#1086
    end
    object Label3: TLabel
      Left = 6
      Top = 179
      Width = 38
      Height = 13
      Caption = #1045#1050#1040#1058#1058#1045
    end
    object Label5: TLabel
      Left = 136
      Top = 225
      Width = 50
      Height = 13
      Caption = #1044#1086#1082#1091#1084#1077#1085#1090
    end
    object Label6: TLabel
      Left = 6
      Top = 52
      Width = 54
      Height = 13
      Caption = #1050#1072#1090#1077#1075#1086#1088#1080#1103
    end
    object Label9: TLabel
      Left = 68
      Top = 52
      Width = 56
      Height = 13
      Caption = #1043'.'#1076#1098#1083#1078#1080#1085#1072
    end
    object Label10: TLabel
      Left = 131
      Top = 52
      Width = 48
      Height = 13
      Caption = #1043'.'#1096#1080#1088#1080#1085#1072
    end
    object Label11: TLabel
      Left = 216
      Top = 225
      Width = 50
      Height = 13
      Caption = #1042#1098#1074#1077#1076#1077#1085#1086
    end
    object Label12: TLabel
      Left = 216
      Top = 179
      Width = 48
      Height = 13
      Caption = #1055#1086#1097'. '#1082#1086#1076
    end
    object Label13: TLabel
      Left = 6
      Top = 125
      Width = 37
      Height = 13
      Caption = #1054#1073#1083#1072#1089#1090
    end
    object Label14: TLabel
      Left = 6
      Top = 152
      Width = 41
      Height = 13
      Caption = #1054#1073#1097#1080#1085#1072
    end
    object btnCity1: TSpeedButton
      Left = 29
      Top = 2
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
      OnClick = Label15Click
    end
    object SpeedButton1: TSpeedButton
      Left = 253
      Top = 48
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
      OnClick = Label7Click
    end
    object SpeedButton2: TSpeedButton
      Left = 107
      Top = 220
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
      OnClick = Label8Click
    end
    object SpeedButton3: TSpeedButton
      Left = 253
      Top = 98
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
      OnClick = Label4Click
    end
    object Label15: TLabel
      Left = 8
      Top = 6
      Width = 19
      Height = 13
      Caption = #1042#1080#1076
    end
    object Label4: TLabel
      Left = 8
      Top = 100
      Width = 35
      Height = 13
      Caption = #1056#1077#1075#1080#1086#1085
    end
    object Label8: TLabel
      Left = 8
      Top = 225
      Width = 76
      Height = 13
      Caption = #1057#1090#1072#1090#1080#1089#1090'. '#1073#1102#1088#1086
    end
    object Label7: TLabel
      Left = 192
      Top = 52
      Width = 54
      Height = 13
      Caption = #1053#1072#1076#1084'. '#1074#1080#1089'.'
    end
    object EditName: TDBEdit
      Left = 55
      Top = 25
      Width = 221
      Height = 21
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      DataField = 'name'
      DataSource = srcTree
      TabOrder = 0
    end
    object Kmetstvo: TDBEdit
      Left = 136
      Top = 198
      Width = 74
      Height = 21
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      DataField = 'kmetstvo'
      DataSource = srcTree
      TabOrder = 7
    end
    object Ekatte: TDBEdit
      Left = 6
      Top = 198
      Width = 124
      Height = 21
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      DataField = 'Ekatte'
      DataSource = srcTree
      TabOrder = 6
    end
    object document: TDBEdit
      Left = 136
      Top = 244
      Width = 74
      Height = 21
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      DataField = 'document'
      DataSource = srcTree
      TabOrder = 9
    end
    object category: TDBEdit
      Left = 6
      Top = 71
      Width = 56
      Height = 21
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      DataField = 'category'
      DataSource = srcTree
      TabOrder = 1
    end
    object longitude: TDBEdit
      Left = 68
      Top = 71
      Width = 57
      Height = 21
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      DataField = 'longitude'
      DataSource = srcTree
      TabOrder = 2
    end
    object latitude: TDBEdit
      Left = 131
      Top = 71
      Width = 53
      Height = 21
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      DataField = 'latitude'
      DataSource = srcTree
      TabOrder = 3
    end
    object lastchange: TDBEdit
      Left = 216
      Top = 244
      Width = 60
      Height = 21
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      DataField = 'lastchange'
      DataSource = srcTree
      TabOrder = 10
    end
    object postcode: TDBEdit
      Left = 216
      Top = 198
      Width = 60
      Height = 21
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      DataField = 'postcode'
      DataSource = srcTree
      TabOrder = 8
    end
    object oblast: TDBEdit
      Left = 49
      Top = 125
      Width = 227
      Height = 21
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      DataField = 'oblast'
      DataSource = srcTree
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 4
    end
    object obstina: TDBEdit
      Left = 49
      Top = 152
      Width = 227
      Height = 21
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      DataField = 'obstina'
      DataSource = srcTree
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 5
    end
    object kind: TDBLookupComboBox
      Left = 6
      Top = 25
      Width = 43
      Height = 21
      BevelInner = bvSpace
      DataField = 'kind'
      DataSource = srcTree
      DropDownWidth = 100
      KeyField = 'kind'
      ListField = 'name;fullname'
      ListSource = srcKind
      TabOrder = 11
    end
    object altitude: TDBLookupComboBox
      Left = 190
      Top = 71
      Width = 86
      Height = 21
      BevelInner = bvSpace
      DataField = 'altitude'
      DataSource = srcTree
      DropDownRows = 8
      KeyField = 'altitude'
      ListField = 'name'
      ListSource = srcAltitude
      TabOrder = 12
    end
    object region: TDBLookupComboBox
      Left = 49
      Top = 98
      Width = 202
      Height = 21
      BevelInner = bvSpace
      DataField = 'region'
      DataSource = srcTree
      DropDownWidth = 300
      KeyField = 'region'
      ListField = 'name;ekatte'
      ListSource = srcRegion
      TabOrder = 13
    end
    object tsb: TDBLookupComboBox
      Left = 6
      Top = 244
      Width = 124
      Height = 21
      BevelInner = bvSpace
      DataField = 'tsb'
      DataSource = srcTree
      DropDownRows = 28
      KeyField = 'tsb'
      ListField = 'name'
      ListSource = srcTSB
      TabOrder = 14
    end
  end
  object ProgressPanel: TPanel
    Left = 181
    Top = 184
    Width = 295
    Height = 35
    BevelInner = bvRaised
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 3
    Visible = False
    object ProgressText: TLabel
      Left = 1
      Top = 1
      Width = 291
      Height = 14
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitWidth = 4
    end
    object ProgressBar: TProgressBar
      Left = 1
      Top = 15
      Width = 291
      Height = 17
      Align = alBottom
      Max = 10
      Smooth = True
      TabOrder = 0
    end
  end
  object srcTree: TDataSource
    OnStateChange = srcTreeStateChange
    Left = 48
    Top = 64
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items.AutoHotKeys = False
        Items = <
          item
            Visible = False
            Action = acSave
            ImageIndex = 18
          end
          item
            Visible = False
            Action = acCopy
            ImageIndex = 7
          end
          item
            Visible = False
            Action = acCancel
            ImageIndex = 10
          end
          item
            Visible = False
            Action = acInsert
            ImageIndex = 0
          end
          item
            Visible = False
            Action = acDelete
            ImageIndex = 19
          end
          item
            Action = acImport
            ImageIndex = 28
          end
          item
            Action = acClose
            ImageIndex = 24
          end>
        ActionBar = ActionToolBar1
        GlyphLayout = blGlyphTop
      end>
    Images = dmImages.Images
    Left = 624
    Top = 8
    StyleName = 'XP Style'
    object acPaste: TAction
      Caption = 'Paste '#1086#1090' Excel'
      ImageIndex = 9
      Visible = False
      OnExecute = acPasteExecute
    end
    object acDelete: TAction
      Caption = #1048#1079#1090#1088#1080#1074#1072#1085#1077
      ImageIndex = 19
      Visible = False
      OnExecute = acDeleteExecute
    end
    object acInsert: TAction
      Caption = #1044#1086#1073#1072#1074#1103#1085#1077
      ImageIndex = 0
      Visible = False
      OnExecute = acInsertExecute
    end
    object acEdit: TAction
      Caption = #1056#1077#1076#1072#1082#1094#1080#1103
      ImageIndex = 42
      OnExecute = acEditExecute
    end
    object acSave: TAction
      Caption = #1047#1072#1087#1080#1089
      ImageIndex = 18
      Visible = False
      OnExecute = acSaveExecute
    end
    object acCancel: TAction
      Caption = #1054#1090#1082#1072#1079
      ImageIndex = 10
      Visible = False
      OnExecute = acCancelExecute
    end
    object acClose: TAction
      Caption = #1048#1079#1093#1086#1076
      ImageIndex = 24
      OnExecute = acCloseExecute
    end
    object acImport: TAction
      Caption = #1048#1084#1087#1086#1088#1090
      ImageIndex = 28
      OnExecute = acImportExecute
    end
    object acCopy: TAction
      Caption = 'Copy'
      ImageIndex = 7
      Visible = False
      OnExecute = acCopyExecute
    end
    object Action1: TAction
      Caption = #1045#1082#1089#1087#1086#1088#1090
      ImageIndex = 12
      Visible = False
      OnExecute = Action1Execute
    end
  end
  object srcKind: TDataSource
    AutoEdit = False
    OnStateChange = srcTreeStateChange
    Left = 48
    Top = 96
  end
  object srcAltitude: TDataSource
    AutoEdit = False
    OnStateChange = srcTreeStateChange
    Left = 48
    Top = 128
  end
  object srcTSB: TDataSource
    AutoEdit = False
    OnStateChange = srcTreeStateChange
    Left = 48
    Top = 160
  end
  object srcRegion: TDataSource
    AutoEdit = False
    OnStateChange = srcTreeStateChange
    Left = 48
    Top = 192
  end
  object srcRaion: TDataSource
    AutoEdit = False
    OnStateChange = srcTreeStateChange
    Left = 48
    Top = 224
  end
end
