object frmMonitor: TfrmMonitor
  Left = 0
  Top = 0
  Caption = #1052#1086#1085#1080#1090#1086#1088#1080#1085#1075
  ClientHeight = 805
  ClientWidth = 675
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Visible = True
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    675
    805)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 0
    Width = 49
    Height = 13
    Caption = #1057#1094#1077#1085#1072#1088#1080#1080
  end
  object Label2: TLabel
    Left = 340
    Top = 0
    Width = 59
    Height = 13
    Caption = #1057#1098#1086#1073#1097#1077#1085#1080#1103
  end
  object Label3: TLabel
    Left = 174
    Top = 0
    Width = 119
    Height = 13
    Caption = #1043#1088#1091#1087#1080' '#1079#1072' '#1087#1088#1086#1079#1074#1098#1085#1103#1074#1072#1085#1077
  end
  object Label4: TLabel
    Left = 8
    Top = 276
    Width = 37
    Height = 13
    Caption = #1050#1072#1085#1072#1083#1080
  end
  object Label5: TLabel
    Left = 506
    Top = 0
    Width = 44
    Height = 13
    Caption = #1057#1098#1073#1080#1090#1080#1103
  end
  object Button1: TButton
    Left = 425
    Top = 260
    Width = 75
    Height = 24
    Caption = #1048#1079#1093#1086#1076
    TabOrder = 0
    OnClick = Button1Click
  end
  object Scripts: TListBox
    Left = 8
    Top = 19
    Width = 160
    Height = 235
    ItemHeight = 13
    TabOrder = 1
  end
  object Sounds: TListBox
    Left = 340
    Top = 19
    Width = 160
    Height = 235
    ItemHeight = 13
    TabOrder = 2
  end
  object Button2: TButton
    Left = 88
    Top = 260
    Width = 193
    Height = 24
    Caption = #1048#1079#1087#1098#1083#1085#1077#1085#1080#1077' '#1085#1072' '#1089#1094#1077#1085#1072#1088#1080#1081' '#1079#1072' '#1075#1088#1091#1087#1072
    TabOrder = 3
    OnClick = Button2Click
  end
  object RingGroups: TListBox
    Left = 174
    Top = 19
    Width = 160
    Height = 235
    ItemHeight = 13
    TabOrder = 4
  end
  object Status: TListBox
    Left = 8
    Top = 295
    Width = 658
    Height = 501
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 5
  end
  object Events: TListBox
    Left = 506
    Top = 19
    Width = 160
    Height = 235
    ItemHeight = 13
    TabOrder = 6
  end
  object Button4: TButton
    Left = 506
    Top = 260
    Width = 160
    Height = 24
    Caption = #1057#1080#1084#1091#1083#1080#1088#1072#1085#1077' '#1085#1072' '#1089#1098#1073#1080#1090#1080#1077
    TabOrder = 7
    OnClick = Button4Click
  end
  object Button3: TButton
    Left = 296
    Top = 260
    Width = 113
    Height = 25
    Caption = #1055#1088#1077#1079#1072#1088#1077#1078#1076#1072#1085#1077
    TabOrder = 8
    OnClick = Button3Click
  end
end
