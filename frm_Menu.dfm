object frmMenu: TfrmMenu
  Left = 0
  Top = 0
  HelpType = htKeyword
  HelpKeyword = 'TfrmMenu'
  Caption = #1045#1089#1090#1077#1083' - '#1040#1057#1054
  ClientHeight = 785
  ClientWidth = 768
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIForm
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  WindowState = wsMaximized
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LoginPanel: TPanel
    Left = 141
    Top = 0
    Width = 627
    Height = 764
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = LoginPanelResize
    object Center: TPanel
      Left = 144
      Top = 48
      Width = 241
      Height = 121
      HelpType = htKeyword
      HelpKeyword = 'login'
      BevelOuter = bvNone
      TabOrder = 0
      object btnLogin: TButton
        Left = 48
        Top = 90
        Width = 75
        Height = 25
        Caption = #1042#1093#1086#1076
        TabOrder = 3
        OnClick = btnLoginClick
      end
      object UserName: TLabeledEdit
        Left = 48
        Top = 36
        Width = 121
        Height = 21
        EditLabel.Width = 19
        EditLabel.Height = 13
        EditLabel.Caption = #1048#1084#1077
        LabelPosition = lpLeft
        TabOrder = 1
        OnKeyDown = UserNameKeyDown
      end
      object Password: TLabeledEdit
        Left = 48
        Top = 63
        Width = 121
        Height = 21
        EditLabel.Width = 37
        EditLabel.Height = 13
        EditLabel.Caption = #1055#1072#1088#1086#1083#1072
        LabelPosition = lpLeft
        PasswordChar = '#'
        TabOrder = 2
        OnKeyDown = UserNameKeyDown
      end
      object DBSelect: TComboBox
        Left = 48
        Top = 9
        Width = 177
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnKeyDown = UserNameKeyDown
      end
      object StaticText1: TStaticText
        Left = 13
        Top = 13
        Width = 34
        Height = 17
        AutoSize = False
        Caption = #1056#1077#1078#1080#1084
        TabOrder = 4
      end
    end
  end
  object ToolbarPanel: TPanel
    Left = 0
    Top = 0
    Width = 141
    Height = 764
    Align = alLeft
    BevelOuter = bvNone
    Color = 13948116
    TabOrder = 1
    Visible = False
    object Toolbar: TActionToolBar
      Left = 0
      Top = 0
      Width = 141
      Height = 764
      ActionManager = ActionManager1
      Align = alClient
      AllowHiding = False
      Caption = 'Toolbar'
      Color = 13948116
      ColorMap = dmImages.ColorMap
      EdgeOuter = esRaised
      Orientation = boTopToBottom
      ParentShowHint = False
      ShowHint = True
      Spacing = 0
    end
  end
  object Tabs: TTabSet
    Left = 0
    Top = 764
    Width = 768
    Height = 21
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Images = dmImages.Images
    OnChange = TabsChange
    OnGetImageIndex = TabsGetImageIndex
  end
  object ActionManager1: TActionManager
    ActionBars.Customizable = False
    ActionBars = <
      item
        ChangesAllowed = []
        Color = clNone
        ContextItems.AutoHotKeys = False
        ContextItems.Customizable = False
        ContextItems = <>
        Items.AutoHotKeys = False
        Items.Customizable = False
        Items.HideUnused = False
        Items.CaptionOptions = coAll
        Items.SmallIcons = False
        Items = <
          item
            Action = acLogin
            ImageIndex = 4
          end
          item
            ChangesAllowed = []
            Items.HideUnused = False
            Items = <>
            Action = acPerson
            ImageIndex = 6
          end
          item
            ChangesAllowed = []
            Items.HideUnused = False
            Items = <>
            Action = acRingGroup
            ImageIndex = 0
          end
          item
            Action = acFilesEvents
            ImageIndex = 11
          end
          item
            Action = acFilesScripts
            ImageIndex = 9
          end
          item
            Action = acFilesMessages
            ImageIndex = 10
          end
          item
            Action = acExport
            ImageIndex = 5
          end
          item
            Action = acImport
            ImageIndex = 5
          end
          item
            Action = acArchive
            ImageIndex = 8
          end
          item
            Action = acRights
            ImageIndex = 2
          end
          item
            Action = Action1
            ImageIndex = 1
          end
          item
            Visible = False
            Action = acFilesReports
            ImageIndex = 3
          end
          item
            Action = acSettings
            ImageIndex = 7
          end>
        ActionBar = Toolbar
        AutoSize = False
        GlyphLayout = blGlyphTop
      end>
    Images = dmImages.LargeImages
    Left = 8
    Top = 8
    StyleName = 'XP Style'
    object acPerson: TAction
      Caption = #1054#1073#1077#1082#1090#1080
      ImageIndex = 6
      OnExecute = acPersonExecute
    end
    object acRingGroup: TAction
      Caption = #1043#1088#1091#1087#1080
      ImageIndex = 0
      OnExecute = acRingGroupExecute
    end
    object acRights: TAction
      Caption = #1055#1088#1072#1074#1072
      ImageIndex = 2
      OnExecute = acRightsExecute
    end
    object acFilesScripts: TAction
      Caption = #1057#1094#1077#1085#1072#1088#1080#1080
      ImageIndex = 9
      OnExecute = acFilesScriptsExecute
    end
    object acFilesMessages: TAction
      Caption = #1057#1098#1086#1073#1097#1077#1085#1080#1103
      ImageIndex = 10
      OnExecute = acFilesMessagesExecute
    end
    object acFilesEvents: TAction
      Caption = #1057#1098#1073#1080#1090#1080#1103
      ImageIndex = 11
      OnExecute = acFilesEventsExecute
    end
    object acFilesReports: TAction
      Caption = #1057#1087#1088#1072#1074#1082#1080
      ImageIndex = 3
      Visible = False
      OnExecute = acFilesReportsExecute
    end
    object acLogin: TAction
      Caption = #1055#1088#1086#1084#1103#1085#1072' '#1085#1072' '#1088#1077#1078#1080#1084#1072
      ImageIndex = 4
      OnExecute = acLoginExecute
    end
    object acMode: TAction
      Caption = #1048#1079#1073#1086#1088' '#1085#1072' '#1088#1077#1078#1080#1084
      ImageIndex = 5
      OnExecute = acModeExecute
    end
    object acEventMonitor: TAction
      Caption = #1050#1086#1085#1090#1088#1086#1083' '#1085#1072' '#1089#1098#1073#1080#1090#1080#1103
      ImageIndex = 6
      OnExecute = acEventMonitorExecute
    end
    object acSettings: TAction
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
      ImageIndex = 7
      OnExecute = acSettingsExecute
    end
    object acExport: TAction
      Caption = #1045#1082#1089#1087#1086#1088#1090
      ImageIndex = 5
      OnExecute = acExportExecute
    end
    object acImport: TAction
      Caption = #1048#1084#1087#1086#1088#1090
      ImageIndex = 5
      OnExecute = acImportExecute
    end
    object Action1: TAction
      Caption = #1055#1086#1090#1088#1077#1073#1080#1090#1077#1083#1080
      ImageIndex = 1
      OnExecute = Action1Execute
    end
    object acArchive: TAction
      Caption = #1040#1088#1093#1080#1074#1080
      ImageIndex = 8
      OnExecute = acArchiveExecute
    end
  end
  object Events: TApplicationEvents
    OnHelp = EventsHelp
    OnMessage = EventsMessage
    Left = 376
    Top = 312
  end
  object IdleTimer: TTimer
    Enabled = False
    Interval = 10000
    OnTimer = IdleTimerTimer
    Left = 408
    Top = 312
  end
end
