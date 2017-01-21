object frmReport: TfrmReport
  Left = 0
  Top = 0
  Caption = #1057#1087#1088#1072#1074#1082#1072
  ClientHeight = 293
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
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 26
    Width = 426
    Height = 29
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 0
    object Label11: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 26
      Height = 21
      Align = alLeft
      Caption = #1048#1084#1077
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitHeight = 16
    end
    object Label15: TLabel
      AlignWithMargins = True
      Left = 348
      Top = 4
      Width = 14
      Height = 21
      Align = alRight
      Caption = 'ID'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitLeft = 360
      ExplicitTop = 2
    end
    object ReportName: TEdit
      AlignWithMargins = True
      Left = 36
      Top = 4
      Width = 306
      Height = 21
      Align = alClient
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
      Text = 'ReportName'
      ExplicitWidth = 326
    end
    object ReportID: TEdit
      AlignWithMargins = True
      Left = 368
      Top = 4
      Width = 54
      Height = 21
      Align = alRight
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ReadOnly = True
      TabOrder = 1
    end
  end
  object browser: TWebBrowser
    Left = 0
    Top = 55
    Width = 426
    Height = 238
    Align = alClient
    TabOrder = 1
    OnNavigateComplete2 = browserNavigateComplete2
    ExplicitHeight = 241
    ControlData = {
      4C000000072C0000991800000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
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
            Action = acClose
            ImageIndex = 24
          end>
        ActionBar = ActionToolBar1
      end>
    Images = dmImages.Images
    Left = 400
    StyleName = 'XP Style'
    object acClose: TAction
      Caption = #1048#1079#1093#1086#1076
      ImageIndex = 24
      OnExecute = acCloseExecute
    end
  end
end
