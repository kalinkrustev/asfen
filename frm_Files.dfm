object frmFiles: TfrmFiles
  Left = 0
  Top = 0
  Caption = #1060#1072#1081#1083#1086#1074#1077
  ClientHeight = 484
  ClientWidth = 701
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ActionToolBar1: TActionToolBar
    Left = 0
    Top = 0
    Width = 701
    Height = 26
    ActionManager = ActionManager1
    Anchors = []
    Caption = 'ActionToolBar1'
    Color = 13948116
    ColorMap = dmImages.ColorMap
    Spacing = 0
  end
  object Files: TListView
    Left = 0
    Top = 26
    Width = 701
    Height = 458
    Align = alClient
    BevelInner = bvSpace
    BevelKind = bkTile
    BevelWidth = 2
    BorderStyle = bsNone
    Columns = <
      item
        Caption = #1048#1084#1077
        Width = 200
      end
      item
        Alignment = taRightJustify
        Caption = 'ID'
        Width = 80
      end
      item
        Caption = #1055#1088#1086#1084#1077#1085#1080#1083
        Width = 100
      end
      item
        Caption = #1042#1088#1077#1084#1077' '#1085#1072' '#1087#1088#1086#1084#1103#1085#1072
        Width = 120
      end>
    RowSelect = True
    SortType = stText
    TabOrder = 1
    ViewStyle = vsList
    OnColumnClick = FilesColumnClick
    OnCompare = FilesCompare
    OnDblClick = FilesDblClick
    OnEdited = FilesEdited
    OnEditing = FilesEditing
    OnKeyDown = FilesKeyDown
    OnKeyPress = FilesKeyPress
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
      end
      item
      end
      item
        Items = <
          item
            Action = acNew
            ImageIndex = 0
          end
          item
            Action = acRename
            ImageIndex = 42
            ShortCut = 113
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
            Action = acDetails
            ImageIndex = 44
          end
          item
            Action = acClose
            ImageIndex = 24
          end>
        ActionBar = ActionToolBar1
      end>
    Images = dmImages.Images
    Left = 552
    Top = 32
    StyleName = 'XP Style'
    object acScript: TAction
      AutoCheck = True
      Caption = #1057#1094#1077#1085#1072#1088#1080#1080
      GroupIndex = 1
      ImageIndex = 37
      OnExecute = acScriptExecute
    end
    object acRecording: TAction
      AutoCheck = True
      Caption = #1057#1098#1086#1073#1097#1077#1085#1080#1103
      GroupIndex = 1
      ImageIndex = 33
      OnExecute = acRecordingExecute
    end
    object acEvent: TAction
      AutoCheck = True
      Caption = #1057#1098#1073#1080#1090#1080#1103
      GroupIndex = 1
      ImageIndex = 39
      OnExecute = acEventExecute
    end
    object acEventReport: TAction
      AutoCheck = True
      Caption = #1057#1087#1088#1072#1074#1082#1080' '#1079#1072' '#1089#1098#1073#1080#1090#1080#1103
      GroupIndex = 1
      ImageIndex = 43
      OnExecute = acEventReportExecute
    end
    object acNew: TAction
      Caption = #1057#1098#1079#1076#1072#1074#1072#1085#1077
      ImageIndex = 0
      OnExecute = acNewExecute
    end
    object acDelete: TAction
      Caption = #1048#1079#1090#1088#1080#1074#1072#1085#1077
      ImageIndex = 19
      OnExecute = acDeleteExecute
    end
    object acClose: TAction
      Caption = #1048#1079#1093#1086#1076
      ImageIndex = 24
      OnExecute = acCloseExecute
    end
    object acEdit: TAction
      Caption = #1056#1077#1076#1072#1082#1090#1080#1088#1072#1085#1077
      ImageIndex = 42
      OnExecute = acEditExecute
    end
    object acDetails: TAction
      AutoCheck = True
      Caption = #1044#1077#1090#1072#1081#1083#1080
      ImageIndex = 44
      OnExecute = acDetailsExecute
    end
    object acRename: TAction
      Caption = #1055#1088#1077#1080#1084#1077#1085#1091#1074#1072#1085#1077
      ImageIndex = 42
      ShortCut = 113
      OnExecute = acRenameExecute
      OnUpdate = acRenameUpdate
    end
  end
  object FileList: TADODataSet
    Connection = dmGUI.Connection
    CursorType = ctStatic
    CommandText = 'select FileName from tblFiles where FilePath='#39'%s'#39
    Parameters = <>
    Left = 24
    Top = 48
  end
end
