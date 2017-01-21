object frmGenericGrid: TfrmGenericGrid
  Left = 0
  Top = 0
  Caption = #1056#1077#1076#1072#1082#1090#1080#1088#1072#1085#1077' '#1085#1072' '#1085#1086#1084#1077#1085#1082#1083#1072#1090#1091#1088#1072
  ClientHeight = 334
  ClientWidth = 522
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
  object ActionToolBar1: TActionToolBar
    Left = 0
    Top = 0
    Width = 522
    Height = 43
    ActionManager = ActionManager1
    Anchors = []
    Caption = 'ActionToolBar1'
    Color = 13948116
    ColorMap = dmImages.ColorMap
    Spacing = 0
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 43
    Width = 522
    Height = 291
    Align = alClient
    BorderStyle = bsNone
    Ctl3D = True
    DataSource = DataSource
    FixedColor = clSkyBlue
    ParentCtl3D = False
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items.AutoHotKeys = False
        Items = <
          item
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
    Left = 272
    Top = 40
    StyleName = 'XP Style'
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
    object acCopy: TAction
      Caption = 'Copy'
      ImageIndex = 7
      OnExecute = acCopyExecute
    end
    object acPaste: TAction
      Caption = 'Paste'
      ImageIndex = 9
      OnExecute = acPasteExecute
    end
    object acImport: TAction
      Caption = #1048#1084#1087#1086#1088#1090
      ImageIndex = 28
      OnExecute = acImportExecute
    end
  end
  object DataSource: TDataSource
    Left = 232
    Top = 40
  end
end
