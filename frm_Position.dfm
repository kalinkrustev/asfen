object frmPosition: TfrmPosition
  Left = 353
  Top = 326
  Caption = #1044#1083#1098#1078#1085#1086#1089#1090#1080
  ClientHeight = 411
  ClientWidth = 502
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
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 0
    Top = 43
    Width = 502
    Height = 368
    Align = alClient
    BorderStyle = bsNone
    Ctl3D = False
    DataSource = srcPositionEdit
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
        FieldName = 'PositionName'
        Title.Caption = #1048#1084#1077' '#1085#1072' '#1076#1083#1098#1078#1085#1086#1089#1090#1090#1072
        Width = 359
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Code'
        Title.Caption = #1050#1086#1076
        Width = 102
        Visible = True
      end>
  end
  object ActionToolBar1: TActionToolBar
    Left = 0
    Top = 0
    Width = 502
    Height = 43
    ActionManager = ActionManager1
    Anchors = []
    Caption = 'ActionToolBar1'
    Color = 13948116
    ColorMap = dmImages.ColorMap
    Spacing = 0
  end
  object srcPositionEdit: TDataSource
    Left = 16
    Top = 72
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items.AutoHotKeys = False
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
            Visible = False
            Action = acPaste
            ImageIndex = 9
          end
          item
            Action = acClose
            ImageIndex = 24
          end>
        ActionBar = ActionToolBar1
        GlyphLayout = blGlyphTop
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
    object acPaste: TAction
      Caption = 'Paste '#1086#1090' Excel'
      ImageIndex = 9
      Visible = False
      OnExecute = acPasteExecute
    end
  end
  object srcPositionList: TDataSource
    Left = 16
    Top = 104
  end
end
