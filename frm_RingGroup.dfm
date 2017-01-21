object frmRingGroup: TfrmRingGroup
  Left = 353
  Top = 326
  Caption = #1043#1088#1091#1087#1080' '#1079#1072' '#1087#1088#1086#1079#1074#1098#1085#1103#1074#1072#1085#1077
  ClientHeight = 411
  ClientWidth = 541
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 225
    Top = 26
    Height = 385
    Color = clMedGray
    ParentColor = False
    ResizeStyle = rsUpdate
    ExplicitLeft = 264
    ExplicitTop = 168
    ExplicitHeight = 100
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 26
    Width = 225
    Height = 385
    Align = alLeft
    BorderStyle = bsNone
    Ctl3D = False
    DataSource = srcRingGroup
    FixedColor = clSkyBlue
    ParentCtl3D = False
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnExit = DBGrid1Exit
    Columns = <
      item
        Expanded = False
        FieldName = 'RingGroupName'
        Title.Caption = #1048#1084#1077' '#1085#1072' '#1075#1088#1091#1087#1072#1090#1072
        Width = 183
        Visible = True
      end>
  end
  object ActionToolBar1: TActionToolBar
    Left = 0
    Top = 0
    Width = 541
    Height = 26
    ActionManager = ActionManager1
    Anchors = []
    Caption = 'ActionToolBar1'
    Color = 13948116
    ColorMap = dmImages.ColorMap
    Spacing = 0
  end
  object Panel1: TPanel
    Left = 228
    Top = 26
    Width = 313
    Height = 385
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    OnResize = Panel1Resize
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 313
      Height = 19
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object LabelPerson: TStaticText
        Left = 0
        Top = 0
        Width = 135
        Height = 19
        Alignment = taCenter
        AutoSize = False
        BevelInner = bvSpace
        BevelKind = bkTile
        BevelOuter = bvRaised
        Caption = #1048#1079#1074#1098#1085' '#1075#1088#1091#1087#1072#1090#1072
        Color = clSkyBlue
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        TabOrder = 0
      end
      object LabelGroup: TStaticText
        Left = 162
        Top = 0
        Width = 152
        Height = 19
        Alignment = taCenter
        AutoSize = False
        BevelInner = bvSpace
        BevelKind = bkTile
        BevelOuter = bvRaised
        Caption = #1042' '#1075#1088#1091#1087#1072#1090#1072
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
    object ListPerson: TListBox
      Left = 0
      Top = 19
      Width = 136
      Height = 366
      Style = lbOwnerDrawFixed
      Align = alLeft
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      ItemHeight = 13
      MultiSelect = True
      Sorted = True
      TabOrder = 1
      OnClick = ListPersonClick
      OnDblClick = ListPersonDblClick
      OnKeyPress = ListPersonKeyPress
    end
    object ListGroup: TListBox
      Left = 162
      Top = 19
      Width = 151
      Height = 366
      Style = lbOwnerDrawFixed
      Align = alClient
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 2
      OnClick = ListPersonClick
      OnDblClick = ListGroupDblClick
      OnDrawItem = ListGroupDrawItem
      OnKeyPress = ListGroupKeyPress
      OnMouseDown = ListGroupMouseDown
      OnMouseMove = ListGroupMouseMove
      OnMouseUp = ListGroupMouseUp
    end
    object Panel3: TPanel
      Left = 136
      Top = 19
      Width = 26
      Height = 366
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 3
      OnResize = Panel3Resize
      object Toolbar2: TActionToolBar
        Left = 0
        Top = 104
        Width = 26
        Height = 107
        ActionManager = ActionManager1
        Align = alNone
        AllowHiding = False
        ColorMap.HighlightColor = 14410210
        ColorMap.BtnSelectedColor = clBtnFace
        ColorMap.UnusedColor = 14410210
        HorzSeparator = False
        Orientation = boTopToBottom
        ParentBackground = True
        ParentColor = True
        ParentShowHint = False
        ShowHint = True
        Spacing = 0
      end
    end
  end
  object srcRingGroup: TDataSource
    AutoEdit = False
    OnDataChange = srcRingGroupDataChange
    Left = 16
    Top = 72
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
            Action = acDelete
            ImageIndex = 19
          end
          item
            Action = acClose
            ImageIndex = 24
          end>
        ActionBar = ActionToolBar1
      end
      item
        ChangesAllowed = []
        Items.AutoHotKeys = False
        Items.Customizable = False
        Items.HideUnused = False
        Items.CaptionOptions = coNone
        Items = <
          item
            ChangesAllowed = []
            Items.HideUnused = False
            Items = <>
            Action = acCheck
            ImageIndex = 29
          end
          item
            ChangesAllowed = []
            Items.HideUnused = False
            Items = <>
            Action = acUncheck
            ImageIndex = 10
          end
          item
            Action = acUp
            ImageIndex = 5
          end
          item
            Action = acDown
            ImageIndex = 3
          end>
        ActionBar = Toolbar2
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
      OnExecute = acCheckExecute
    end
    object acUncheck: TAction
      Caption = #1048#1079#1082#1083#1102#1095#1074#1072#1085#1077
      Enabled = False
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
  end
  object ScrollList: TTimer
    Enabled = False
    Interval = 500
    OnTimer = ScrollListTimer
    Left = 264
    Top = 208
  end
end
