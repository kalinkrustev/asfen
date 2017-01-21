object frmEventMonitor: TfrmEventMonitor
  Left = 0
  Top = 0
  Caption = #1050#1086#1085#1090#1088#1086#1083' '#1085#1072' '#1089#1098#1073#1080#1090#1080#1103
  ClientHeight = 274
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Events: TDBLookupListBox
    Left = 0
    Top = 26
    Width = 426
    Height = 247
    Align = alClient
    BorderStyle = bsNone
    KeyField = 'FileID'
    ListField = 'FileName'
    ListSource = srcEvents
    TabOrder = 0
    OnClick = EventsClick
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
            Action = acFireEvent
            ImageIndex = 39
          end
          item
            Action = acClose
            ImageIndex = 24
          end>
        ActionBar = ActionToolBar1
      end>
    Images = dmImages.Images
    Left = 392
    Top = 40
    StyleName = 'XP Style'
    object acFireEvent: TAction
      Caption = #1057#1090#1072#1088#1090#1080#1088#1072#1085#1077
      Enabled = False
      ImageIndex = 39
      OnExecute = acFireEventExecute
    end
    object acClose: TAction
      Caption = #1048#1079#1093#1086#1076
      ImageIndex = 24
      OnExecute = acCloseExecute
    end
  end
  object srcEvents: TDataSource
    DataSet = dmGUI.EventList
    Left = 8
    Top = 32
  end
end
