object frmEvent: TfrmEvent
  Left = 0
  Top = 0
  Caption = #1056#1077#1076#1072#1082#1090#1080#1088#1072#1085#1077' '#1085#1072' '#1089#1098#1073#1080#1090#1080#1077
  ClientHeight = 445
  ClientWidth = 665
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 249
    Top = 272
    Height = 173
    Color = clMedGray
    ParentColor = False
    ResizeStyle = rsUpdate
    ExplicitLeft = 272
    ExplicitTop = 184
    ExplicitHeight = 100
  end
  object ActionToolBar1: TActionToolBar
    Left = 0
    Top = 0
    Width = 665
    Height = 23
    ActionManager = ActionManager1
    Anchors = []
    Caption = 'ActionToolBar1'
    ColorMap.HighlightColor = 14410210
    ColorMap.BtnSelectedColor = clBtnFace
    ColorMap.UnusedColor = 14410210
    Spacing = 0
    ExplicitHeight = 26
  end
  object Scripts: TListView
    Left = 0
    Top = 272
    Width = 249
    Height = 173
    Align = alLeft
    BorderStyle = bsNone
    Columns = <
      item
        AutoSize = True
        Caption = #1057#1094#1077#1085#1072#1088#1080#1081
      end
      item
        Caption = 'ID'
        Width = 70
      end>
    HideSelection = False
    IconOptions.Arrangement = iaLeft
    IconOptions.WrapText = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnSelectItem = ScriptsSelectItem
    ExplicitTop = 275
    ExplicitHeight = 170
  end
  object Panel1: TPanel
    Left = 0
    Top = 23
    Width = 665
    Height = 29
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 2
    ExplicitTop = 26
    object Label11: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 26
      Height = 16
      Align = alLeft
      Caption = #1048#1084#1077
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label15: TLabel
      AlignWithMargins = True
      Left = 548
      Top = 4
      Width = 14
      Height = 16
      Align = alRight
      Caption = 'ID'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object EventName: TEdit
      AlignWithMargins = True
      Left = 36
      Top = 4
      Width = 506
      Height = 21
      Align = alClient
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
      Text = 'EventName'
    end
    object EventID: TEdit
      AlignWithMargins = True
      Left = 568
      Top = 4
      Width = 93
      Height = 21
      Align = alRight
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ReadOnly = True
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 252
    Top = 272
    Width = 413
    Height = 173
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitTop = 275
    ExplicitHeight = 170
    DesignSize = (
      413
      173)
    object Label1: TLabel
      Left = 6
      Top = 6
      Width = 49
      Height = 13
      Caption = #1057#1094#1077#1085#1072#1088#1080#1081
      FocusControl = ScriptSelect
    end
    object Label2: TLabel
      Left = 6
      Top = 44
      Width = 72
      Height = 26
      Caption = #1043#1088#1091#1087#1072' '#1079#1072' '#1087#1088#1086#1079#1074#1098#1085#1103#1074#1072#1085#1077
      FocusControl = RingGroupSelect
      WordWrap = True
    end
    object Label3: TLabel
      Left = 6
      Top = 82
      Width = 66
      Height = 26
      Caption = #1041#1088#1086#1081' '#1082#1072#1085#1072#1083#1080' '#1087#1088#1080' '#1089#1090#1072#1088#1090
      FocusControl = RingGroupSelect
      WordWrap = True
    end
    object ScriptSelect: TDBLookupComboBox
      AlignWithMargins = True
      Left = 88
      Top = 6
      Width = 318
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BevelInner = bvSpace
      BevelOuter = bvNone
      DropDownRows = 30
      KeyField = 'FileID'
      ListField = 'FileName;FileID'
      ListSource = srcScripts
      TabOrder = 0
      OnClick = ScriptSelectClick
    end
    object RingGroupSelect: TDBLookupComboBox
      AlignWithMargins = True
      Left = 88
      Top = 44
      Width = 318
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BevelInner = bvSpace
      BevelOuter = bvNone
      DropDownRows = 30
      KeyField = 'ID'
      ListField = 'RingGroupName;ID'
      ListSource = srcRingGroups
      TabOrder = 1
      OnClick = ScriptSelectClick
      OnKeyDown = RingGroupSelectKeyDown
    end
    object Count: TEdit
      Left = 88
      Top = 82
      Width = 121
      Height = 21
      TabOrder = 2
      OnChange = CountChange
      OnExit = CountExit
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 52
    Width = 665
    Height = 220
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 4
    ExplicitTop = 55
    object StaticText1: TStaticText
      Left = 0
      Top = 200
      Width = 665
      Height = 20
      Align = alBottom
      Alignment = taCenter
      BevelInner = bvNone
      BevelKind = bkTile
      BevelOuter = bvRaised
      Caption = #1057#1094#1077#1085#1072#1088#1080#1080' '#1079#1072' '#1089#1090#1072#1088#1090#1080#1088#1072#1085#1077
      Color = clSkyBlue
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      TabOrder = 0
      ExplicitWidth = 166
    end
    object Panel4: TPanel
      Left = 265
      Top = 0
      Width = 400
      Height = 200
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object StaticText2: TStaticText
        Left = 0
        Top = 0
        Width = 193
        Height = 20
        Align = alTop
        Alignment = taCenter
        BevelInner = bvNone
        BevelKind = bkTile
        BevelOuter = bvRaised
        Caption = #1055#1072#1088#1072#1084#1077#1090#1088#1080' '#1085#1072' '#1089#1090#1072#1088#1090#1080#1088#1072#1085#1077#1090#1086
        Color = clSkyBlue
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        TabOrder = 0
      end
      object Params: TValueListEditor
        Left = 0
        Top = 20
        Width = 400
        Height = 180
        Align = alClient
        BorderStyle = bsNone
        FixedColor = clSkyBlue
        KeyOptions = [keyUnique]
        TabOrder = 1
        TitleCaptions.Strings = (
          #1055#1072#1088#1072#1084#1077#1090#1098#1088
          #1057#1090#1086#1081#1085#1086#1089#1090)
        OnStringsChange = ParamsStringsChange
        ColWidths = (
          124
          274)
      end
    end
    object Panel5: TPanel
      Left = 0
      Top = 0
      Width = 265
      Height = 200
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 2
      object StaticText3: TStaticText
        Left = 0
        Top = 0
        Width = 265
        Height = 20
        Align = alTop
        Alignment = taCenter
        BevelInner = bvNone
        BevelKind = bkTile
        BevelOuter = bvRaised
        Caption = #1044#1077#1092#1080#1085#1080#1094#1080#1103' '#1085#1072' '#1089#1098#1073#1080#1090#1080#1077#1090#1086
        Color = clSkyBlue
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        TabOrder = 0
        ExplicitWidth = 172
      end
      object EventKind: TRadioGroup
        Left = 0
        Top = 20
        Width = 265
        Height = 156
        Align = alClient
        Caption = #1057#1090#1072#1088#1090#1080#1088#1072#1085#1077' '#1086#1090':'
        TabOrder = 1
        OnClick = EventKindClick
      end
      object Panel6: TPanel
        Left = 0
        Top = 176
        Width = 265
        Height = 24
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 2
        DesignSize = (
          265
          24)
        object btnCity1: TSpeedButton
          Left = 240
          Top = 0
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
          OnClick = SpeedButton1Click
        end
        object Label4: TLabel
          Left = 6
          Top = 5
          Width = 18
          Height = 13
          Caption = #1058#1080#1087
        end
        object CrisisKind: TDBLookupComboBox
          Left = 32
          Top = 1
          Width = 207
          Height = 21
          BevelInner = bvSpace
          DropDownWidth = 600
          KeyField = 'id'
          ListField = 'id;name'
          ListFieldIndex = 1
          ListSource = srcEventType
          TabOrder = 0
          OnClick = CrisisKindClick
        end
      end
    end
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = acSave
          end
          item
            Action = acAdd
          end
          item
            Action = acDelete
          end>
      end
      item
        Items = <
          item
            Action = acSave
            ImageIndex = 19
          end
          item
            Action = acAdd
            ImageIndex = 40
          end
          item
            Action = acDelete
            ImageIndex = 27
          end
          item
            Action = acClose
            ImageIndex = 34
          end>
      end
      item
        Items = <
          item
            Action = acSave
            ImageIndex = 18
          end
          item
            Action = acAdd
            ImageIndex = 40
          end
          item
            Action = acDelete
            ImageIndex = 41
          end
          item
            Action = acClose
            ImageIndex = 24
          end>
        ActionBar = ActionToolBar1
      end>
    Images = dmImages.Images
    Left = 624
    Top = 8
    StyleName = 'XP Style'
    object acSave: TAction
      Caption = #1047#1072#1087#1080#1089
      ImageIndex = 18
      OnExecute = acSaveExecute
    end
    object acAdd: TAction
      Caption = #1044#1086#1073#1072#1074#1103#1085#1077' '#1085#1072' '#1089#1094#1077#1085#1072#1088#1080#1081
      ImageIndex = 40
      OnExecute = acAddExecute
    end
    object acDelete: TAction
      Caption = #1048#1079#1090#1088#1080#1074#1072#1085#1077' '#1085#1072' '#1089#1094#1077#1085#1072#1088#1080#1081
      ImageIndex = 41
      OnExecute = acDeleteExecute
    end
    object acClose: TAction
      Caption = #1048#1079#1093#1086#1076
      ImageIndex = 24
      OnExecute = acCloseExecute
    end
  end
  object srcScripts: TDataSource
    AutoEdit = False
    DataSet = dmGUI.ScriptList
    Left = 480
    Top = 400
  end
  object srcRingGroups: TDataSource
    DataSet = dmGUI.RingGroupsList
    Left = 440
    Top = 400
  end
  object srcEventType: TDataSource
    AutoEdit = False
    DataSet = dmGUI.Crisis
    Left = 520
    Top = 400
  end
end
