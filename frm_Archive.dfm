object frmArchive: TfrmArchive
  Left = 0
  Top = 0
  Caption = #1040#1088#1093#1080#1074#1080
  ClientHeight = 511
  ClientWidth = 615
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ActionToolBar1: TActionToolBar
    Left = 0
    Top = 0
    Width = 615
    Height = 26
    ActionManager = ActionManager1
    Anchors = []
    Caption = 'ActionToolBar1'
    Color = 13948116
    ColorMap = dmImages.ColorMap
    Spacing = 0
  end
  object FileList: TListView
    Left = 0
    Top = 26
    Width = 615
    Height = 485
    Align = alClient
    BorderStyle = bsNone
    Columns = <
      item
        AutoSize = True
        Caption = #1048#1084#1077
      end
      item
        Caption = #1044#1072#1090#1072' '#1080' '#1095#1072#1089
        MinWidth = 80
        Width = 120
      end
      item
        Caption = #1056#1072#1079#1084#1077#1088
        MinWidth = 40
        Width = 60
      end>
    SortType = stText
    TabOrder = 1
    ViewStyle = vsReport
    OnColumnClick = FileListColumnClick
    OnCompare = FileListCompare
    OnSelectItem = FileListSelectItem
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
            Action = acBackup
            ImageIndex = 18
          end
          item
            Action = acRestore
            ImageIndex = 13
          end
          item
            Action = acClose
            ImageIndex = 24
          end>
        ActionBar = ActionToolBar1
      end>
    Images = dmImages.Images
    Left = 472
    Top = 56
    StyleName = 'XP Style'
    object acClose: TAction
      Caption = #1048#1079#1093#1086#1076
      ImageIndex = 24
      OnExecute = acCloseExecute
    end
    object acBackup: TAction
      Caption = #1040#1088#1093#1080#1074#1080#1088#1072#1085#1077
      ImageIndex = 18
      OnExecute = acBackupExecute
    end
    object acRestore: TAction
      Caption = #1056#1072#1079#1072#1088#1093#1080#1074#1080#1088#1072#1085#1077
      Enabled = False
      ImageIndex = 13
      OnExecute = acRestoreExecute
    end
  end
end
