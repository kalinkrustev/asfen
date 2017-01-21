object frmMode: TfrmMode
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #1048#1079#1073#1086#1088' '#1085#1072' '#1088#1077#1078#1080#1084
  ClientHeight = 162
  ClientWidth = 428
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ModeSelect: TRadioGroup
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 422
    Height = 116
    Align = alClient
    Caption = #1056#1077#1078#1080#1084
    TabOrder = 0
    ExplicitHeight = 123
  end
  object Panel1: TPanel
    Left = 0
    Top = 122
    Width = 428
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 130
    DesignSize = (
      428
      40)
    object btnOk: TBitBtn
      Left = 112
      Top = 7
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = #1048#1079#1073#1086#1088
      TabOrder = 0
      OnClick = btnOkClick
      Kind = bkOK
      ExplicitTop = 8
    end
    object btnCancel: TBitBtn
      Left = 241
      Top = 7
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = #1054#1090#1082#1072#1079
      TabOrder = 1
      Kind = bkCancel
      ExplicitTop = 8
    end
  end
end
