object frmWorkgroup: TfrmWorkgroup
  Left = 353
  Top = 326
  Caption = #1055#1086#1090#1088#1077#1073#1080#1090#1077#1083#1089#1082#1080' '#1075#1088#1091#1087#1080
  ClientHeight = 411
  ClientWidth = 426
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
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object grdWorkgroup: TDBGrid
    Left = 0
    Top = 26
    Width = 426
    Height = 385
    Align = alClient
    BorderStyle = bsNone
    Ctl3D = False
    DataSource = srcWorkgroup
    FixedColor = clSkyBlue
    ParentCtl3D = False
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
        Width = 359
        Visible = True
      end>
  end
  object ActionToolBar1: TActionToolBar
    Left = 0
    Top = 0
    Width = 426
    Height = 26
    ActionManager = ActionManager1
    Anchors = []
    Caption = 'ActionToolBar1'
    Color = 13948116
    ColorMap = dmImages.ColorMap
    Spacing = 0
  end
  object srcWorkgroup: TDataSource
    DataSet = dmGUI.WorkgroupEdit
    Left = 16
    Top = 72
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
            ImageIndex = 0
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
      end>
    Images = dmImages.Images
    Left = 352
    StyleName = 'XP Style'
    object acSave: TAction
      Caption = #1047#1072#1087#1080#1089
      ImageIndex = 18
      OnExecute = acSaveExecute
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
    object acClose: TAction
      Caption = #1048#1079#1093#1086#1076
      ImageIndex = 24
      OnExecute = acCloseExecute
    end
  end
end
