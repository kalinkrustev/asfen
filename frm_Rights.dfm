object frmRights: TfrmRights
  Left = 353
  Top = 326
  Caption = #1055#1088#1072#1074#1072
  ClientHeight = 515
  ClientWidth = 761
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
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 225
    Top = 26
    Height = 489
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
    Height = 489
    Align = alLeft
    BorderStyle = bsNone
    Ctl3D = False
    DataSource = srcWorkGroup
    FixedColor = clSkyBlue
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'GroupName'
        Title.Caption = #1048#1084#1077' '#1085#1072' '#1075#1088#1091#1087#1072#1090#1072
        Width = 183
        Visible = True
      end>
  end
  object ActionToolBar1: TActionToolBar
    Left = 0
    Top = 0
    Width = 761
    Height = 26
    ActionManager = ActionManager1
    Anchors = []
    Caption = 'ActionToolBar1'
    Color = 13948116
    ColorMap = dmImages.ColorMap
    Spacing = 0
    ExplicitWidth = 541
  end
  object Panel1: TPanel
    Left = 228
    Top = 26
    Width = 533
    Height = 489
    Align = alClient
    Caption = 'Panel1'
    TabOrder = 2
    ExplicitLeft = 296
    ExplicitTop = 248
    ExplicitWidth = 185
    ExplicitHeight = 41
    object Rights: TListView
      Left = 1
      Top = 1
      Width = 531
      Height = 466
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Checkboxes = True
      Columns = <
        item
          AutoSize = True
          Caption = #1055#1088#1072#1074#1072
        end>
      ColumnClick = False
      HideSelection = False
      IconOptions.Arrangement = iaLeft
      IconOptions.AutoArrange = True
      IconOptions.WrapText = False
      MultiSelect = True
      SortType = stText
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = RightsChange
      OnChanging = RightsChanging
      OnEditing = RightsEditing
      OnSelectItem = RightsSelectItem
      ExplicitLeft = 3
      ExplicitTop = 6
    end
    object Groups: TTabSet
      Left = 1
      Top = 467
      Width = 531
      Height = 21
      Align = alBottom
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      OnClick = GroupsClick
      ExplicitLeft = 64
      ExplicitTop = 184
      ExplicitWidth = 185
    end
  end
  object srcWorkGroup: TDataSource
    AutoEdit = False
    OnDataChange = srcWorkGroupDataChange
    Left = 16
    Top = 72
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
      end
      item
        Items = <
          item
            Action = acClose
            ImageIndex = 34
          end>
      end
      item
        Items = <
          item
            Action = acCheck
            ImageIndex = 29
          end
          item
            Action = acUncheck
            ImageIndex = 10
          end
          item
            Action = acClose
            ImageIndex = 24
          end>
        ActionBar = ActionToolBar1
      end>
    Images = dmImages.Images
    Left = 472
    StyleName = 'XP Style'
    object acClose: TAction
      Caption = #1048#1079#1093#1086#1076
      ImageIndex = 24
      OnExecute = acCloseExecute
    end
    object acCheck: TAction
      Caption = #1042#1082#1083#1102#1095#1074#1072#1085#1077
      Enabled = False
      ImageIndex = 29
      OnExecute = acCheckExecute
    end
    object acUncheck: TAction
      Caption = #1048#1079#1082#1083#1102#1095#1074#1072#1085#1077
      Enabled = False
      ImageIndex = 10
      OnExecute = acUncheckExecute
    end
  end
  object Q: TADOQuery
    Parameters = <>
    Left = 72
    Top = 200
  end
end
