object frmWave: TfrmWave
  Left = 0
  Top = 0
  Caption = #1056#1077#1076#1072#1082#1090#1080#1088#1072#1085#1077' '#1085#1072' '#1089#1098#1086#1073#1097#1077#1085#1080#1077
  ClientHeight = 293
  ClientWidth = 544
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Scaled = False
  Visible = True
  WindowState = wsMaximized
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 87
    Width = 141
    Height = 150
    Align = alLeft
    AutoSize = False
    Caption = #1058#1077#1082#1089#1090#1086#1074#1086' '#1089#1098#1086#1073#1097#1077#1085#1080#1077
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ExplicitHeight = 16
  end
  object ActionToolBar1: TActionToolBar
    Left = 0
    Top = 0
    Width = 544
    Height = 26
    ActionManager = ActionManager1
    Anchors = []
    Caption = 'ActionToolBar1'
    Color = 13948116
    ColorMap = dmImages.ColorMap
    Spacing = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 26
    Width = 544
    Height = 29
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 1
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
      Left = 442
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
      ExplicitHeight = 16
    end
    object WaveName: TEdit
      AlignWithMargins = True
      Left = 36
      Top = 4
      Width = 400
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
      Text = 'WaveName'
    end
    object WaveID: TEdit
      AlignWithMargins = True
      Left = 462
      Top = 4
      Width = 78
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
  object Panel2: TPanel
    Left = 0
    Top = 55
    Width = 544
    Height = 29
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 2
    object Label1: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 140
      Height = 21
      Align = alLeft
      AutoSize = False
      Caption = #1055#1088#1086#1076#1098#1083#1078#1080#1090#1077#1083#1085#1086#1089#1090
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      AlignWithMargins = True
      Left = 419
      Top = 4
      Width = 37
      Height = 21
      Align = alRight
      Caption = #1054#1073#1097#1086
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitLeft = 427
      ExplicitHeight = 16
    end
    object WaveLength: TEdit
      AlignWithMargins = True
      Left = 150
      Top = 4
      Width = 263
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
      Text = 'WaveName'
      ExplicitWidth = 390
    end
    object TotalLength: TEdit
      AlignWithMargins = True
      Left = 462
      Top = 4
      Width = 78
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
  object Digit: TStaticText
    Left = 0
    Top = 240
    Width = 544
    Height = 53
    Align = alBottom
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
  end
  object MessageText: TMemo
    AlignWithMargins = True
    Left = 150
    Top = 87
    Width = 391
    Height = 150
    Align = alClient
    BorderStyle = bsNone
    TabOrder = 4
    OnChange = MessageTextChange
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = acSave
          end
          item
            Action = acRecord
          end
          item
            Action = acPlayback
          end>
      end
      item
        Items = <
          item
            Action = acSave
            ImageIndex = 19
          end
          item
            Action = acRecord
            ImageIndex = 1
          end
          item
            Action = acPlayback
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
            Action = acSave
            ImageIndex = 18
          end
          item
            Action = acRecord
            ImageIndex = 1
          end
          item
            Action = acStop
            ImageIndex = 22
          end
          item
            Action = acPlayback
            ImageIndex = 21
          end
          item
            Action = acImport
            ImageIndex = 13
          end
          item
            Action = acClose
            ImageIndex = 24
          end>
        ActionBar = ActionToolBar1
      end>
    Images = dmImages.Images
    Left = 392
    Top = 32
    StyleName = 'XP Style'
    object acSave: TAction
      Caption = #1047#1072#1087#1080#1089
      ImageIndex = 18
      OnExecute = acSaveExecute
    end
    object acRecord: TAction
      Caption = #1047#1072#1087#1080#1089' '#1086#1090' '#1084#1080#1082#1088#1086#1092#1086#1085
      ImageIndex = 1
      OnExecute = acRecordExecute
    end
    object acPlayback: TAction
      Caption = #1055#1088#1086#1089#1083#1091#1096#1074#1072#1085#1077
      ImageIndex = 21
      OnExecute = acPlaybackExecute
    end
    object acClose: TAction
      Caption = #1048#1079#1093#1086#1076
      ImageIndex = 24
      OnExecute = acCloseExecute
    end
    object acStop: TAction
      Caption = #1057#1090#1086#1087
      Enabled = False
      ImageIndex = 22
      OnExecute = acStopExecute
    end
    object acImport: TAction
      Caption = #1048#1084#1087#1086#1088#1090
      ImageIndex = 13
      OnExecute = acImportExecute
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'Wave files|*.wav'
    InitialDir = '.'
    Options = [ofHideReadOnly, ofNoChangeDir, ofFileMustExist, ofEnableSizing]
    Title = #1048#1084#1087#1086#1088#1090' '#1085#1072' '#1089#1098#1086#1073#1097#1077#1085#1080#1077' '#1086#1090' wave '#1092#1072#1081#1083
    Left = 256
    Top = 128
  end
end
