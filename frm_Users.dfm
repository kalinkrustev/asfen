object frmUsers: TfrmUsers
  Left = 0
  Top = 0
  Caption = #1055#1086#1090#1088#1077#1073#1080#1090#1077#1083#1080
  ClientHeight = 366
  ClientWidth = 580
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
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 257
    Top = 26
    Height = 340
    Color = clMedGray
    ParentColor = False
    ResizeStyle = rsUpdate
    ExplicitLeft = 296
    ExplicitTop = 144
    ExplicitHeight = 100
  end
  object ActionToolBar1: TActionToolBar
    Left = 0
    Top = 0
    Width = 580
    Height = 26
    ActionManager = ActionManager1
    Anchors = []
    Caption = 'ActionToolBar1'
    Color = 13948116
    ColorMap = dmImages.ColorMap
    Spacing = 0
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 26
    Width = 257
    Height = 340
    Align = alLeft
    BorderStyle = bsNone
    DataSource = srcUser
    FixedColor = clSkyBlue
    ReadOnly = True
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'UserName'
        Title.Caption = #1055#1086#1090#1088#1077#1073#1080#1090#1077#1083
        Width = 221
        Visible = True
      end>
  end
  object Panel1: TPanel
    Left = 260
    Top = 26
    Width = 320
    Height = 340
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      320
      340)
    object Label1: TLabel
      Left = 15
      Top = 11
      Width = 99
      Height = 13
      Alignment = taRightJustify
      Caption = #1055#1086#1090#1088#1077#1073#1080#1090#1077#1083#1089#1082#1086' '#1080#1084#1077
    end
    object Label2: TLabel
      Left = 77
      Top = 38
      Width = 37
      Height = 13
      Alignment = taRightJustify
      Caption = #1055#1072#1088#1086#1083#1072
    end
    object Label3: TLabel
      Left = 4
      Top = 65
      Width = 110
      Height = 13
      Alignment = taRightJustify
      Caption = #1055#1086#1090#1088#1077#1073#1080#1090#1077#1083#1089#1082#1072' '#1075#1088#1091#1087#1072
    end
    object UserName: TDBEdit
      Left = 120
      Top = 8
      Width = 193
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      DataField = 'UserName'
      DataSource = srcUser
      TabOrder = 0
    end
    object Password: TDBEdit
      Left = 120
      Top = 35
      Width = 193
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      DataField = 'Password'
      DataSource = srcUser
      PasswordChar = '#'
      TabOrder = 1
    end
    object WorkGroup: TDBLookupComboBox
      Left = 120
      Top = 62
      Width = 193
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BevelInner = bvSpace
      BevelOuter = bvNone
      BevelKind = bkTile
      DataField = 'WorkGroup'
      DataSource = srcUser
      KeyField = 'ID'
      ListField = 'GroupName'
      ListSource = srcWorkgroupList
      TabOrder = 2
    end
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
            Action = acAdd
            ImageIndex = 0
          end
          item
            Action = acDelete
            ImageIndex = 19
          end
          item
            Action = acGroups
            ImageIndex = 27
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
    object acAdd: TAction
      Caption = #1044#1086#1073#1072#1074#1103#1085#1077
      ImageIndex = 0
      OnExecute = acAddExecute
    end
    object acDelete: TAction
      Caption = #1048#1079#1090#1088#1080#1074#1072#1085#1077
      ImageIndex = 19
      OnExecute = acDeleteExecute
    end
    object acGroups: TAction
      Caption = #1055#1086#1090#1088#1077#1073#1080#1090#1077#1083#1089#1082#1080' '#1075#1088#1091#1087#1080
      ImageIndex = 27
      OnExecute = acGroupsExecute
    end
  end
  object srcUser: TDataSource
    DataSet = dmGUI.UserEdit
    OnStateChange = srcUserStateChange
    OnDataChange = srcUserDataChange
    Left = 16
    Top = 72
  end
  object srcWorkgroupList: TDataSource
    DataSet = dmGUI.WorkGroupList
    Left = 544
    Top = 112
  end
end
