object frmRingGroupTree: TfrmRingGroupTree
  Left = 0
  Top = 0
  Caption = #1043#1088#1091#1087#1080' '#1079#1072' '#1086#1087#1086#1074#1077#1089#1090#1103#1074#1072#1085#1077
  ClientHeight = 674
  ClientWidth = 752
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 279
    Top = 88
    Height = 586
    Color = clMedGray
    ParentColor = False
    ResizeStyle = rsUpdate
    ExplicitLeft = 264
    ExplicitTop = 168
    ExplicitHeight = 100
  end
  object Splitter2: TSplitter
    Left = 564
    Top = 88
    Height = 586
    Align = alRight
    Color = clMedGray
    ParentColor = False
    ResizeStyle = rsUpdate
    ExplicitLeft = 412
    ExplicitTop = 32
    ExplicitHeight = 467
  end
  object ActionToolBar1: TActionToolBar
    Left = 0
    Top = 0
    Width = 752
    Height = 88
    ActionManager = ActionManager1
    Anchors = []
    Caption = 'ActionToolBar1'
    ColorMap.HighlightColor = 14410210
    ColorMap.BtnSelectedColor = clBtnFace
    ColorMap.UnusedColor = 14410210
    Spacing = 0
  end
  object GroupGrid: TDBGrid
    Left = 0
    Top = 88
    Width = 279
    Height = 586
    Align = alLeft
    BorderStyle = bsNone
    Ctl3D = False
    DataSource = srcRingGroup
    FixedColor = clSkyBlue
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnExit = GroupGridExit
    Columns = <
      item
        Expanded = False
        FieldName = 'RingGroupName'
        Title.Caption = #1048#1084#1077' '#1085#1072' '#1075#1088#1091#1087#1072#1090#1072
        Width = 183
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ID'
        Visible = True
      end>
  end
  object PanelTree: TPanel
    Left = 282
    Top = 88
    Width = 282
    Height = 586
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object Splitter3: TSplitter
      Left = 0
      Top = 356
      Width = 282
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      Color = clMedGray
      ParentColor = False
      ResizeStyle = rsUpdate
      ExplicitTop = 0
      ExplicitWidth = 320
    end
    object Info: TMemo
      Left = 0
      Top = 359
      Width = 282
      Height = 227
      Align = alBottom
      BorderStyle = bsNone
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
    end
  end
  object Panel1: TPanel
    Left = 567
    Top = 88
    Width = 185
    Height = 586
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 3
    object ListGroup: TListBox
      Left = 0
      Top = 19
      Width = 185
      Height = 567
      Style = lbOwnerDrawFixed
      Align = alClient
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 0
      OnClick = ListGroupClick
      OnDrawItem = ListGroupDrawItem
      OnKeyDown = ListGroupKeyDown
      OnKeyPress = ListGroupKeyPress
      OnMouseDown = ListGroupMouseDown
      OnMouseMove = ListGroupMouseMove
      OnMouseUp = ListGroupMouseUp
    end
    object LabelGroup: TStaticText
      Left = 0
      Top = 0
      Width = 185
      Height = 19
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      BevelInner = bvSpace
      BevelKind = bkTile
      BevelOuter = bvRaised
      Caption = #1055#1088#1080#1086#1088#1080#1090#1077#1090
      Color = clSkyBlue
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      TabOrder = 1
    end
  end
  object ActionManager1: TActionManager
    ActionBars.Customizable = False
    ActionBars = <
      item
        Items = <
          item
            Action = acSave
            ImageIndex = 18
          end
          item
            Action = acAdd
            ImageIndex = 0
          end
          item
            Action = acRename
            ImageIndex = 42
          end
          item
            Action = acDelete
            ImageIndex = 19
          end
          item
            Action = acUp
            ImageIndex = 5
          end
          item
            Action = acDown
            ImageIndex = 3
          end
          item
            Action = acUncheck
            ImageIndex = 10
          end
          item
            Action = acAutoCheck
            ImageIndex = 26
          end
          item
            Action = acClose
            ImageIndex = 24
          end>
        ActionBar = ActionToolBar1
        GlyphLayout = blGlyphTop
      end>
    Images = dmImages.Images
    Left = 480
    StyleName = 'XP Style'
    object acSave: TAction
      Caption = #1047#1072#1087#1080#1089
      ImageIndex = 18
      OnExecute = acSaveExecute
    end
    object acAdd: TAction
      Caption = #1044#1086#1073#1072#1074#1103#1085#1077' '#1085#1072' '#1075#1088#1091#1087#1072
      ImageIndex = 0
      OnExecute = acAddExecute
    end
    object acDelete: TAction
      Caption = #1048#1079#1090#1088#1080#1074#1072#1085#1077' '#1085#1072' '#1075#1088#1091#1087#1072
      ImageIndex = 19
      OnExecute = acDeleteExecute
    end
    object acClose: TAction
      Caption = #1048#1079#1093#1086#1076
      ImageIndex = 24
      OnExecute = acCloseExecute
    end
    object acCheck: TAction
      Caption = #1042#1082#1083#1102#1095#1074#1072#1085#1077
      Enabled = False
      Hint = 
        #1042#1082#1083#1102#1095#1074#1072#1085#1077' '#1085#1072' '#1080#1079#1073#1088#1072#1085#1080#1090#1077' '#1087#1086#1090#1088#1077#1073#1080#1090#1077#1083#1080' '#1074' '#1075#1088#1091#1087#1072#1090#1072' ( Enter / '#1076#1074#1086#1081#1085#1086' '#1097#1088 +
        #1072#1082#1074#1072#1085#1077' )'
      ImageIndex = 29
    end
    object acUncheck: TAction
      Caption = #1048#1079#1082#1083#1102#1095#1074#1072#1085#1077
      Hint = 
        #1048#1079#1082#1083#1102#1095#1074#1072#1085#1077' '#1085#1072' '#1080#1079#1073#1088#1072#1085#1080#1090#1077' '#1087#1086#1090#1088#1077#1073#1080#1090#1077#1083#1080' '#1086#1090' '#1075#1088#1091#1087#1072#1090#1072' ( Enter / '#1076#1074#1086#1081#1085#1086' ' +
        #1097#1088#1072#1082#1074#1072#1085#1077' )'
      ImageIndex = 10
      OnExecute = acUncheckExecute
    end
    object acUp: TAction
      Caption = #1059#1074#1077#1083#1080#1095#1072#1074#1072#1085#1077' '#1085#1072' '#1087#1088#1080#1086#1088#1080#1090#1077#1090#1072
      Hint = 
        #1059#1074#1077#1083#1080#1095#1072#1074#1072#1085#1077' '#1085#1072' '#1087#1088#1080#1086#1088#1080#1090#1077#1090#1072' - '#1087#1086#1079#1074#1098#1085#1103#1074#1072#1085#1077' '#1087#1086'-'#1088#1072#1085#1086' ( + / '#1084#1077#1089#1090#1077#1085#1077' '#1089' ' +
        #1076#1077#1089#1077#1085' '#1073#1091#1090#1086#1085' )'
      ImageIndex = 5
      OnExecute = acUpExecute
    end
    object acDown: TAction
      Caption = #1053#1072#1084#1072#1083#1103#1074#1072#1085#1077' '#1085#1072' '#1087#1088#1080#1086#1088#1080#1090#1077#1090#1072
      Hint = 
        #1053#1072#1084#1072#1083#1103#1074#1072#1085#1077' '#1085#1072' '#1087#1088#1080#1086#1088#1080#1090#1077#1090#1072' - '#1087#1086#1079#1074#1098#1085#1103#1074#1072#1085#1077' '#1087#1086'-'#1082#1098#1089#1085#1086' ( - /  '#1084#1077#1089#1090#1077#1085#1077' '#1089 +
        ' '#1076#1077#1089#1077#1085' '#1073#1091#1090#1086#1085' )'
      ImageIndex = 3
      OnExecute = acDownExecute
    end
    object acRename: TAction
      Caption = #1055#1088#1077#1080#1084#1077#1085#1091#1074#1072#1085#1077' '#1085#1072' '#1075#1088#1091#1087#1072
      ImageIndex = 42
      OnExecute = acRenameExecute
    end
    object acAutoCheck: TAction
      AutoCheck = True
      Caption = #1040#1074#1090#1086#1084#1072#1090#1080#1095#1085#1086' '#1084#1072#1088#1082#1080#1088#1072#1085#1077
      ImageIndex = 26
      OnExecute = acAutoCheckExecute
    end
  end
  object srcRingGroup: TDataSource
    AutoEdit = False
    OnDataChange = srcRingGroupDataChange
    Left = 16
    Top = 72
  end
  object srcTree: TDataSource
    AutoEdit = False
    Left = 232
    Top = 72
  end
  object ScrollList: TTimer
    Enabled = False
    Interval = 500
    OnTimer = ScrollListTimer
    Left = 264
    Top = 208
  end
end
