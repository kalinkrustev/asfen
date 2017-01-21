object frmPassword: TfrmPassword
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = #1042#1093#1086#1076
  ClientHeight = 110
  ClientWidth = 288
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 56
    Top = 13
    Width = 184
    Height = 13
    Caption = #1052#1086#1083#1103' '#1074#1098#1074#1077#1076#1077#1090#1077' '#1087#1072#1088#1086#1083#1072#1090#1072' '#1079#1072' '#1076#1086#1089#1090#1098#1087
  end
  object Password: TEdit
    Left = 56
    Top = 32
    Width = 184
    Height = 21
    PasswordChar = '#'
    TabOrder = 0
  end
  object btnOk: TButton
    Left = 40
    Top = 67
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnClose: TButton
    Left = 121
    Top = 67
    Width = 132
    Height = 25
    Caption = #1047#1072#1090#1074#1086#1088#1080' '#1087#1088#1080#1083#1086#1078#1077#1085#1080#1077#1090#1086
    TabOrder = 2
    OnClick = btnCloseClick
  end
end
