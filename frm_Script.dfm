object frmScript: TfrmScript
  Left = 0
  Top = 0
  Caption = #1056#1077#1076#1072#1082#1090#1080#1088#1072#1085#1077' '#1085#1072' '#1089#1094#1077#1085#1072#1088#1080#1081
  ClientHeight = 473
  ClientWidth = 657
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 201
    Top = 83
    Height = 390
    Color = clMedGray
    ParentColor = False
    ResizeStyle = rsUpdate
    ExplicitLeft = 272
    ExplicitTop = 184
    ExplicitHeight = 100
  end
  object ActionToolBar1: TActionToolBar
    Left = 0
    Top = 0
    Width = 657
    Height = 54
    ActionManager = ActionManager1
    Anchors = []
    Caption = 'ActionToolBar1'
    Color = 13948116
    ColorMap = dmImages.ColorMap
    Spacing = 0
  end
  object Actions: TListView
    Left = 0
    Top = 83
    Width = 201
    Height = 390
    Align = alLeft
    BorderStyle = bsNone
    Columns = <>
    HideSelection = False
    IconOptions.Arrangement = iaLeft
    IconOptions.WrapText = False
    ReadOnly = True
    TabOrder = 1
    ViewStyle = vsList
    OnSelectItem = ActionsSelectItem
  end
  object Action: TPageControl
    Left = 204
    Top = 83
    Width = 453
    Height = 390
    ActivePage = Ring
    Align = alClient
    Images = dmImages.Images
    MultiLine = True
    Style = tsButtons
    TabOrder = 2
    OnChange = ActionChanged
    object None: TTabSheet
      Caption = #1041#1077#1079' '#1086#1087#1077#1088#1072#1094#1080#1103
      ImageIndex = 35
      object Label7: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 439
        Height = 19
        Align = alTop
        Caption = #1055#1088#1077#1093#1086#1076' '#1082#1098#1084' '#1089#1083#1077#1076#1074#1072#1097#1072#1090#1072' '#1089#1090#1098#1087#1082#1072
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
        ExplicitWidth = 273
      end
    end
    object Ring: TTabSheet
      Caption = #1055#1086#1079#1074#1098#1085#1103#1074#1072#1085#1077
      ImageIndex = 30
      object Label2: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 439
        Height = 19
        Align = alTop
        Caption = #1055#1086#1079#1074#1098#1085#1103#1074#1072#1085#1077' '#1085#1072' '#1087#1086#1090#1088#1077#1073#1080#1090#1077#1083' '#1080#1083#1080' '#1075#1088#1091#1087#1072
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
        ExplicitWidth = 326
      end
      object Label18: TLabel
        Left = 3
        Top = 129
        Width = 260
        Height = 13
        Caption = #1055#1086#1089#1083#1077#1076#1086#1074#1072#1090#1077#1083#1085#1086#1089#1090' '#1085#1072' '#1087#1086#1079#1074#1098#1085#1103#1074#1072#1085#1077' '#1085#1072' '#1090#1077#1083#1077#1092#1086#1085#1080#1090#1077
        FocusControl = RingSequence
      end
      object Label19: TLabel
        Left = 3
        Top = 156
        Width = 147
        Height = 13
        Caption = #1041#1088#1086#1081' '#1086#1087#1080#1090#1080' '#1087#1088#1080' '#1089#1080#1075#1085#1072#1083' '#1079#1072#1077#1090#1086
        FocusControl = RetryBusy
      end
      object Label20: TLabel
        Left = 3
        Top = 183
        Width = 172
        Height = 13
        Caption = #1048#1079#1095#1072#1082#1074#1072#1085#1077' '#1087#1088#1080' '#1089#1080#1075#1085#1072#1083' '#1079#1072#1077#1090#1086' ('#1089#1077#1082')'
        FocusControl = DelayBusy
      end
      object Label21: TLabel
        Left = 3
        Top = 210
        Width = 153
        Height = 13
        Caption = #1041#1088#1086#1081' '#1086#1087#1080#1090#1080' '#1087#1088#1080' '#1079#1072#1077#1090#1080' '#1087#1098#1090#1080#1097#1072
        FocusControl = RetryBusyRoute
      end
      object Label22: TLabel
        Left = 3
        Top = 237
        Width = 178
        Height = 13
        Caption = #1048#1079#1095#1072#1082#1074#1072#1085#1077' '#1087#1088#1080' '#1079#1072#1077#1090#1080' '#1087#1098#1090#1080#1097#1072' ('#1089#1077#1082')'
        FocusControl = DelayBusyRoute
      end
      object Label23: TLabel
        Left = 3
        Top = 264
        Width = 198
        Height = 13
        Caption = #1041#1088#1086#1081' '#1086#1087#1080#1090#1080' '#1087#1088#1080' '#1089#1075#1088#1077#1096#1077#1085#1072' DTMF '#1087#1072#1088#1086#1083#1072
        FocusControl = RetryFailDTMF
      end
      object Label24: TLabel
        Left = 3
        Top = 291
        Width = 223
        Height = 13
        Caption = #1048#1079#1095#1072#1082#1074#1072#1085#1077' '#1087#1088#1080' '#1089#1075#1088#1077#1096#1077#1085#1072' DTMF '#1087#1072#1088#1086#1083#1072' ('#1089#1077#1082')'
        FocusControl = DelayFailDTMF
      end
      object RingKind: TRadioGroup
        AlignWithMargins = True
        Left = 3
        Top = 28
        Width = 439
        Height = 65
        Align = alTop
        ItemIndex = 0
        Items.Strings = (
          #1055#1086#1079#1074#1098#1085#1103#1074#1072#1085#1077' '#1085#1072' '#1075#1088#1091#1087#1072' '#1079#1072' '#1086#1087#1086#1074#1077#1089#1090#1103#1074#1072#1085#1077
          #1055#1086#1079#1074#1098#1085#1103#1074#1072#1085#1077' '#1085#1072' '#1087#1086#1090#1088#1077#1073#1080#1090#1077#1083)
        TabOrder = 0
        OnClick = RingKindClick
      end
      object RingPersonSelect: TDBLookupComboBox
        AlignWithMargins = True
        Left = 3
        Top = 99
        Width = 439
        Height = 21
        Align = alTop
        BevelInner = bvSpace
        BevelOuter = bvNone
        Ctl3D = True
        DropDownRows = 30
        KeyField = 'Code'
        ListField = 'Code;Name'
        ListFieldIndex = 1
        ListSource = Persons
        ParentCtl3D = False
        TabOrder = 1
        Visible = False
        OnClick = RingPersonSelectClick
      end
      object RingSequence: TEdit
        Left = 321
        Top = 126
        Width = 121
        Height = 21
        Hint = '1 - '#1089#1083#1091#1078#1077#1073#1077#1085', 2 - '#1076#1086#1084#1072#1096#1077#1085', 3 - '#1084#1086#1073#1080#1083#1077#1085', s - SMS, m - EMail'
        BevelInner = bvSpace
        BevelKind = bkTile
        BorderStyle = bsNone
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnChange = RingSequenceChange
        OnKeyPress = RingSequenceKeyPress
      end
      object RetryBusy: TEdit
        Left = 321
        Top = 153
        Width = 121
        Height = 21
        BevelInner = bvSpace
        BevelKind = bkTile
        BorderStyle = bsNone
        TabOrder = 3
        OnChange = RingSequenceChange
        OnKeyPress = RingSequenceKeyPress
      end
      object DelayBusy: TEdit
        Left = 321
        Top = 180
        Width = 121
        Height = 21
        BevelInner = bvSpace
        BevelKind = bkTile
        BorderStyle = bsNone
        TabOrder = 4
        OnChange = RingSequenceChange
        OnKeyPress = RingSequenceKeyPress
      end
      object RetryBusyRoute: TEdit
        Left = 321
        Top = 207
        Width = 121
        Height = 21
        BevelInner = bvSpace
        BevelKind = bkTile
        BorderStyle = bsNone
        TabOrder = 5
        OnChange = RingSequenceChange
        OnKeyPress = RingSequenceKeyPress
      end
      object DelayBusyRoute: TEdit
        Left = 321
        Top = 234
        Width = 121
        Height = 21
        BevelInner = bvSpace
        BevelKind = bkTile
        BorderStyle = bsNone
        TabOrder = 6
        OnChange = RingSequenceChange
        OnKeyPress = RingSequenceKeyPress
      end
      object RetryFailDTMF: TEdit
        Left = 321
        Top = 261
        Width = 121
        Height = 21
        BevelInner = bvSpace
        BevelKind = bkTile
        BorderStyle = bsNone
        TabOrder = 7
        OnChange = RingSequenceChange
        OnKeyPress = RingSequenceKeyPress
      end
      object DelayFailDTMF: TEdit
        Left = 321
        Top = 288
        Width = 121
        Height = 21
        BevelInner = bvSpace
        BevelKind = bkTile
        BorderStyle = bsNone
        TabOrder = 8
        OnChange = RingSequenceChange
        OnKeyPress = RingSequenceKeyPress
      end
    end
    object Delay: TTabSheet
      Caption = #1048#1079#1095#1072#1082#1074#1072#1085#1077
      ImageIndex = 34
      object Label3: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 439
        Height = 38
        Align = alTop
        Caption = #1048#1079#1095#1072#1082#1074#1072#1085#1077' '#1087#1088#1077#1076#1080' '#1087#1088#1077#1084#1080#1085#1072#1074#1072#1085#1077' '#1082#1098#1084' '#1089#1083#1077#1076#1074#1072#1097#1072#1090#1072' '#1089#1090#1098#1087#1082#1072
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
        ExplicitWidth = 415
      end
      object DelayMillisecond: TLabeledEdit
        Left = 192
        Top = 44
        Width = 105
        Height = 21
        EditLabel.Width = 134
        EditLabel.Height = 13
        EditLabel.Caption = #1048#1079#1095#1072#1082#1074#1072#1085#1077' - '#1084#1080#1083#1080#1089#1077#1082#1091#1085#1076#1080':'
        LabelPosition = lpLeft
        TabOrder = 0
        OnChange = DelayMillisecondChange
      end
    end
    object Play: TTabSheet
      Caption = #1057#1098#1086#1073#1097#1077#1085#1080#1077
      ImageIndex = 33
      object Label4: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 439
        Height = 19
        Align = alTop
        Caption = #1055#1088#1077#1076#1072#1074#1072#1085#1077' '#1085#1072' '#1075#1083#1072#1089#1086#1074#1086' '#1089#1098#1086#1073#1097#1077#1085#1080#1077
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
        ExplicitWidth = 289
      end
      object Label10: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 28
        Width = 439
        Height = 13
        Align = alTop
        Caption = #1048#1084#1077' '#1085#1072' '#1075#1083#1072#1089#1086#1074#1086#1090#1086' '#1089#1098#1086#1073#1097#1077#1085#1080#1077
        ExplicitWidth = 149
      end
      object PlayFileName: TDBLookupComboBox
        AlignWithMargins = True
        Left = 3
        Top = 47
        Width = 439
        Height = 21
        Align = alTop
        BevelInner = bvSpace
        BevelOuter = bvNone
        Ctl3D = True
        DropDownRows = 30
        KeyField = 'FileID'
        ListField = 'FileName'
        ListSource = Recordings
        ParentCtl3D = False
        TabOrder = 0
        OnClick = PlayFileNameClick
      end
    end
    object DTMF: TTabSheet
      Caption = #1058#1086#1085#1086#1074#1077
      ImageIndex = 36
      object Label5: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 439
        Height = 19
        Align = alTop
        Caption = #1056#1072#1079#1087#1086#1079#1085#1072#1074#1072#1085#1077' '#1085#1072' '#1090#1086#1085#1072#1083#1085#1086' '#1085#1072#1073#1088#1072#1085#1080' '#1094#1080#1092#1088#1080
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
        ExplicitWidth = 350
      end
      object DTMFTimeOut: TLabeledEdit
        Left = 168
        Top = 28
        Width = 97
        Height = 21
        EditLabel.Width = 86
        EditLabel.Height = 13
        EditLabel.Caption = #1048#1079#1095#1072#1082#1074#1072#1085#1077' ('#1089#1077#1082'):'
        LabelPosition = lpLeft
        TabOrder = 0
        OnChange = DelayMillisecondChange
      end
      object DTMFFail: TCheckBox
        Left = 3
        Top = 55
        Width = 262
        Height = 26
        Alignment = taLeftJustify
        Caption = #1055#1088#1077#1082#1098#1089#1074#1072#1085#1077' '#1085#1072' '#1089#1094#1077#1085#1072#1088#1080#1103' '#1087#1088#1080' '#1085#1077#1088#1072#1079#1087#1086#1079#1085#1072#1074#1072#1085#1077
        TabOrder = 1
        OnClick = DTMFFailClick
      end
    end
    object Jump: TTabSheet
      Caption = #1055#1088#1077#1093#1086#1076
      ImageIndex = 32
      object Label1: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 439
        Height = 38
        Align = alTop
        Caption = #1055#1088#1077#1093#1086#1076' '#1082#1098#1084' '#1076#1088#1091#1075' '#1089#1094#1077#1085#1072#1088#1080#1081', '#1073#1077#1079' '#1074#1088#1098#1097#1072#1085#1077' '#1082#1098#1084' '#1090#1077#1082#1091#1097#1080#1103
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
        ExplicitWidth = 391
      end
      object Label8: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 47
        Width = 439
        Height = 13
        Align = alTop
        Caption = #1048#1084#1077' '#1085#1072' '#1089#1094#1077#1085#1072#1088#1080#1081' '#1079#1072' '#1087#1088#1077#1093#1086#1076':'
        ExplicitWidth = 142
      end
      object JumpScriptSelect: TDBLookupComboBox
        AlignWithMargins = True
        Left = 3
        Top = 66
        Width = 439
        Height = 21
        Align = alTop
        BevelInner = bvSpace
        BevelOuter = bvNone
        DropDownRows = 30
        KeyField = 'FileID'
        ListField = 'FileName'
        ListSource = Scripts
        TabOrder = 0
        OnClick = JumpScriptSelectClick
      end
    end
    object Call: TTabSheet
      Caption = #1048#1079#1074#1080#1082#1074#1072#1085#1077
      ImageIndex = 31
      object Label6: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 439
        Height = 38
        Align = alTop
        Caption = #1048#1079#1087#1098#1083#1085#1077#1085#1080#1077' '#1085#1072' '#1076#1088#1091#1075' '#1089#1094#1077#1085#1072#1088#1080#1081' '#1080' '#1074#1088#1098#1097#1072#1085#1077' '#1082#1098#1084' '#1090#1077#1082#1091#1097#1080#1103
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
        ExplicitWidth = 393
      end
      object Label9: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 47
        Width = 439
        Height = 13
        Align = alTop
        Caption = #1048#1084#1077' '#1085#1072' '#1089#1094#1077#1085#1072#1088#1080#1081' '#1079#1072' '#1087#1088#1077#1093#1086#1076':'
        ExplicitWidth = 142
      end
      object CallScriptSelect: TDBLookupComboBox
        AlignWithMargins = True
        Left = 3
        Top = 66
        Width = 439
        Height = 21
        Align = alTop
        BevelInner = bvSpace
        BevelOuter = bvNone
        DropDownRows = 30
        KeyField = 'FileID'
        ListField = 'FileName'
        ListSource = Scripts
        TabOrder = 0
        OnClick = CallScriptSelectClick
      end
    end
    object Hangup: TTabSheet
      Caption = #1055#1088#1077#1082#1098#1089#1074#1072#1085#1077
      ImageIndex = 22
      object Label12: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 439
        Height = 19
        Align = alTop
        Caption = #1055#1088#1077#1082#1098#1089#1074#1072#1085#1077' '#1085#1072' '#1086#1073#1072#1078#1076#1072#1085#1077#1090#1086
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
        ExplicitWidth = 236
      end
    end
    object SMS: TTabSheet
      Caption = 'SMS'
      ImageIndex = 33
      object Label13: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 439
        Height = 19
        Align = alTop
        Caption = #1048#1079#1087#1088#1072#1097#1072#1085#1077' '#1085#1072' SMS '#1085#1072' '#1087#1086#1090#1088#1077#1073#1080#1090#1077#1083' '#1080#1083#1080' '#1075#1088#1091#1087#1072
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
        ExplicitWidth = 375
      end
      object Label14: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 28
        Width = 439
        Height = 13
        Align = alTop
        Caption = #1048#1084#1077' '#1085#1072' '#1089#1098#1086#1073#1097#1077#1085#1080#1077#1090#1086
        ExplicitWidth = 106
      end
      object SMSKind: TRadioGroup
        AlignWithMargins = True
        Left = 3
        Top = 74
        Width = 439
        Height = 65
        Align = alTop
        ItemIndex = 0
        Items.Strings = (
          #1048#1079#1087#1088#1072#1097#1072#1085#1077' '#1085#1072' SMS '#1085#1072' '#1075#1088#1091#1087#1072' '#1079#1072' '#1086#1087#1086#1074#1077#1089#1090#1103#1074#1072#1085#1077
          #1048#1079#1087#1088#1072#1097#1072#1085#1077' '#1085#1072' SMS '#1085#1072' '#1087#1086#1090#1088#1077#1073#1080#1090#1077#1083)
        TabOrder = 0
        OnClick = SMSKindClick
      end
      object SMSPersonSelect: TDBLookupComboBox
        AlignWithMargins = True
        Left = 3
        Top = 145
        Width = 439
        Height = 21
        Align = alTop
        BevelInner = bvSpace
        BevelOuter = bvNone
        Ctl3D = True
        DropDownRows = 30
        KeyField = 'Code'
        ListField = 'Code;Name'
        ListFieldIndex = 1
        ListSource = Persons
        ParentCtl3D = False
        TabOrder = 1
        Visible = False
        OnClick = SMSPersonSelectClick
      end
      object SMSName: TDBLookupComboBox
        AlignWithMargins = True
        Left = 3
        Top = 47
        Width = 439
        Height = 21
        Align = alTop
        BevelInner = bvSpace
        BevelOuter = bvNone
        Ctl3D = True
        DropDownRows = 30
        KeyField = 'FileID'
        ListField = 'FileName'
        ListSource = Recordings
        ParentCtl3D = False
        TabOrder = 2
        OnClick = SMSNameClick
      end
    end
    object Mail: TTabSheet
      Caption = 'EMail'
      ImageIndex = 15
      object Label16: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 439
        Height = 19
        Align = alTop
        Caption = #1048#1079#1087#1088#1072#1097#1072#1085#1077' '#1085#1072' EMail '#1085#1072' '#1087#1086#1090#1088#1077#1073#1080#1090#1077#1083' '#1080#1083#1080' '#1075#1088#1091#1087#1072
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
        ExplicitWidth = 385
      end
      object Label17: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 28
        Width = 439
        Height = 13
        Align = alTop
        Caption = #1048#1084#1077' '#1085#1072' '#1089#1098#1086#1073#1097#1077#1085#1080#1077#1090#1086
        ExplicitWidth = 106
      end
      object EMailKind: TRadioGroup
        AlignWithMargins = True
        Left = 3
        Top = 74
        Width = 439
        Height = 65
        Align = alTop
        ItemIndex = 0
        Items.Strings = (
          #1048#1079#1087#1088#1072#1097#1072#1085#1077' '#1085#1072' EMail '#1085#1072' '#1075#1088#1091#1087#1072' '#1079#1072' '#1086#1087#1086#1074#1077#1089#1090#1103#1074#1072#1085#1077
          #1048#1079#1087#1088#1072#1097#1072#1085#1077' '#1085#1072' EMail '#1085#1072' '#1087#1086#1090#1088#1077#1073#1080#1090#1077#1083)
        TabOrder = 0
        OnClick = EMailKindClick
      end
      object EMailPersonSelect: TDBLookupComboBox
        AlignWithMargins = True
        Left = 3
        Top = 145
        Width = 439
        Height = 21
        Align = alTop
        BevelInner = bvSpace
        BevelOuter = bvNone
        Ctl3D = True
        DropDownRows = 30
        KeyField = 'Code'
        ListField = 'Code;Name'
        ListFieldIndex = 1
        ListSource = Persons
        ParentCtl3D = False
        TabOrder = 1
        Visible = False
        OnClick = EMailPersonSelectClick
      end
      object EMailName: TDBLookupComboBox
        AlignWithMargins = True
        Left = 3
        Top = 47
        Width = 439
        Height = 21
        Align = alTop
        BevelInner = bvSpace
        BevelOuter = bvNone
        Ctl3D = True
        DropDownRows = 30
        KeyField = 'FileID'
        ListField = 'FileName'
        ListSource = Recordings
        ParentCtl3D = False
        TabOrder = 2
        OnClick = EMailNameClick
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 54
    Width = 657
    Height = 29
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 3
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
      Left = 556
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
    object ScriptName: TEdit
      AlignWithMargins = True
      Left = 36
      Top = 4
      Width = 514
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
      Text = 'ScriptName'
    end
    object ScriptID: TEdit
      AlignWithMargins = True
      Left = 576
      Top = 4
      Width = 77
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
            Action = acSave
            ImageIndex = 18
          end
          item
            Action = acAdd
            ImageIndex = 0
          end
          item
            Action = acUp
            ImageIndex = 5
          end
          item
            Action = acDown
            ImageIndex = 3
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
    Left = 400
    StyleName = 'XP Style'
    object acSave: TAction
      Caption = #1047#1072#1087#1080#1089
      ImageIndex = 18
      OnExecute = acSaveExecute
    end
    object acAdd: TAction
      Caption = #1044#1086#1073#1072#1074#1103#1085#1077' '#1085#1072' '#1089#1090#1098#1087#1082#1072
      ImageIndex = 0
      OnExecute = acAddExecute
    end
    object acDelete: TAction
      Caption = #1048#1079#1090#1088#1080#1074#1072#1085#1077' '#1085#1072' '#1089#1090#1098#1087#1082#1072
      ImageIndex = 19
      OnExecute = acDeleteExecute
    end
    object acClose: TAction
      Caption = #1048#1079#1093#1086#1076
      ImageIndex = 24
      OnExecute = acCloseExecute
    end
    object acUp: TAction
      Caption = #1055#1088#1077#1084#1077#1089#1090#1074#1072#1085#1077' '#1085#1072#1075#1086#1088#1077
      Enabled = False
      ImageIndex = 5
      OnExecute = acUpExecute
    end
    object acDown: TAction
      Caption = #1055#1088#1077#1084#1077#1089#1090#1074#1072#1085#1077' '#1085#1072#1076#1086#1083#1091
      Enabled = False
      ImageIndex = 3
      OnExecute = acDownExecute
    end
  end
  object Recordings: TDataSource
    AutoEdit = False
    DataSet = dmGUI.RecordingsList
    Left = 616
    Top = 168
  end
  object Scripts: TDataSource
    AutoEdit = False
    DataSet = dmGUI.ScriptList
    Left = 576
    Top = 168
  end
  object Persons: TDataSource
    DataSet = dmGUI.PersonList
    Left = 536
    Top = 168
  end
  object RingGroups: TDataSource
    DataSet = dmGUI.RingGroupsList
    Left = 496
    Top = 168
  end
end
