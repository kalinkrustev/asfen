object frmPerson: TfrmPerson
  Left = 265
  Top = 186
  Caption = #1055#1086#1090#1088#1077#1073#1080#1090#1077#1083#1080
  ClientHeight = 526
  ClientWidth = 702
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  Visible = True
  WindowState = wsMaximized
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GridPanel1: TGridPanel
    Left = 0
    Top = 0
    Width = 702
    Height = 526
    Align = alClient
    BevelOuter = bvNone
    Caption = 'GridPanel1'
    ColumnCollection = <
      item
        Value = 100.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = ActionToolBar1
        Row = 0
      end
      item
        Column = 0
        Control = Panel1
        Row = 1
      end>
    RowCollection = <
      item
        SizeStyle = ssAbsolute
        Value = 26.000000000000000000
      end
      item
        Value = 100.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 26.000000000000000000
      end>
    TabOrder = 0
    object ActionToolBar1: TActionToolBar
      Left = 0
      Top = 0
      Width = 702
      Height = 26
      ActionManager = ActionManager1
      Anchors = []
      Caption = 'ActionToolBar1'
      Color = 13948116
      ColorMap = dmGUI.ColorMap
      Spacing = 0
    end
    object Panel1: TPanel
      Left = 0
      Top = 26
      Width = 702
      Height = 474
      Align = alClient
      BevelOuter = bvNone
      Caption = 'Panel1'
      TabOrder = 1
      object Splitter1: TSplitter
        Left = 192
        Top = 0
        Height = 474
        Color = clMedGray
        ParentColor = False
        ResizeStyle = rsUpdate
        ExplicitLeft = 328
        ExplicitTop = 192
        ExplicitHeight = 100
      end
      object DBGrid1: TDBGrid
        Left = 0
        Top = 0
        Width = 192
        Height = 474
        Align = alLeft
        BorderStyle = bsNone
        Ctl3D = False
        DataSource = srcPerson
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
            FieldName = 'FullName'
            Title.Caption = #1048#1084#1077
            Width = 223
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Phone'
            Title.Caption = #1058#1077#1083#1077#1092#1086#1085
            Width = 80
            Visible = True
          end>
      end
      object FlowPanel1: TFlowPanel
        Left = 195
        Top = 0
        Width = 507
        Height = 474
        Align = alClient
        BevelOuter = bvNone
        Padding.Left = 3
        Padding.Top = 3
        Padding.Right = 3
        Padding.Bottom = 3
        TabOrder = 1
        object Label1: TLabel
          AlignWithMargins = True
          Left = 6
          Top = 6
          Width = 101
          Height = 21
          AutoSize = False
          Caption = #1048#1084#1077
          FocusControl = DBEdit1
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object DBEdit1: TDBEdit
          AlignWithMargins = True
          Left = 113
          Top = 6
          Width = 382
          Height = 21
          BevelInner = bvSpace
          BevelKind = bkTile
          BorderStyle = bsNone
          DataField = 'FullName'
          DataSource = srcPerson
          TabOrder = 0
        end
        object Panel2: TPanel
          AlignWithMargins = True
          Left = 6
          Top = 33
          Width = 243
          Height = 24
          BevelOuter = bvNone
          TabOrder = 1
          object Label2: TLabel
            Left = 0
            Top = 0
            Width = 101
            Height = 21
            AutoSize = False
            Caption = #1058#1077#1083#1077#1092#1086#1085' 1'
            FocusControl = DBEdit2
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object DBEdit2: TDBEdit
            Left = 107
            Top = 0
            Width = 133
            Height = 21
            BevelInner = bvSpace
            BevelKind = bkTile
            BorderStyle = bsNone
            DataField = 'Phone'
            DataSource = srcPerson
            TabOrder = 0
          end
        end
        object Panel6: TPanel
          AlignWithMargins = True
          Left = 255
          Top = 33
          Width = 243
          Height = 24
          BevelOuter = bvNone
          TabOrder = 2
          object Label6: TLabel
            Left = 0
            Top = 0
            Width = 101
            Height = 21
            AutoSize = False
            Caption = #1058#1077#1083#1077#1092#1086#1085' 2'
            FocusControl = DBEdit6
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object DBEdit6: TDBEdit
            Left = 107
            Top = 0
            Width = 133
            Height = 21
            BevelInner = bvSpace
            BevelKind = bkTile
            BorderStyle = bsNone
            DataField = 'Phone2'
            DataSource = srcPerson
            TabOrder = 0
          end
        end
        object Panel7: TPanel
          AlignWithMargins = True
          Left = 6
          Top = 63
          Width = 243
          Height = 24
          BevelOuter = bvNone
          TabOrder = 3
          object Label7: TLabel
            Left = 0
            Top = 0
            Width = 101
            Height = 21
            AutoSize = False
            Caption = #1052#1086#1073#1080#1083#1077#1085' '#1090#1077#1083'.'
            FocusControl = DBEdit7
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object DBEdit7: TDBEdit
            Left = 107
            Top = 0
            Width = 133
            Height = 21
            BevelInner = bvSpace
            BevelKind = bkTile
            BorderStyle = bsNone
            DataField = 'Phone3'
            DataSource = srcPerson
            TabOrder = 0
          end
        end
        object Panel3: TPanel
          AlignWithMargins = True
          Left = 255
          Top = 63
          Width = 243
          Height = 24
          BevelOuter = bvNone
          TabOrder = 4
          object Label3: TLabel
            Left = 0
            Top = 2
            Width = 101
            Height = 21
            AutoSize = False
            Caption = #1054#1090#1076#1077#1083
            FocusControl = Division
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object btnDivision: TSpeedButton
            Left = 220
            Top = 0
            Width = 23
            Height = 23
            Flat = True
            Glyph.Data = {
              36030000424D3603000000000000360000002800000010000000100000000100
              1800000000000003000000000000000000000000000000000000FF00FFFF00FF
              FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
              FFFF00FFFF00FFFF00FFA0726EA0726EA0726FA0726EA0726EA0726EA0726EA0
              726EA0726EA0726EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFA0726EEDDFD1
              E9DACBE9D8C8E8D7C5E8D7C4E8D6C5E8D7C6EDDFCCA0726EFF00FFFF00FFFF00
              FFFF00FFFF00FFFF00FFBF8A85FAF2E5F5ECE0F3EADDF3E8D9F1E6D6F1E5D4F1
              E6D4F8EDD9A0726EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC48F87F9F3EA
              F9DFC3FAD7B5F9D7B6F8D7B4F8D6B1F6DABCF6ECD9A0726EFF00FFFF00FFFF00
              FFFF00FFFF00FFFF00FFCA9588F9F6EFF8E9D8F8E4D0F7E3CEF6E2CBF6E0C8F2
              DFC9F4EAD7A0726EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFD19D89FBF8F2
              F8DFC5F6D5B3F8D7B6FAD7B5F2D0ADDEC7ACDCD5C7A0726EFF00FFFF00FFFF00
              FFFF00FFFF00FFFF00FFD6A38BFCF9F5E8DED3D3C6BADED0C2E8D9CACFC0B1A7
              9C91A7A298967A73FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFDCA98CFCFCFB
              9FA6B7AF967FB09780BEA3895572995B7CA460617677625DFF00FFFF00FFFF00
              FF5B5757464242464242E0AE8DFEFEFE5572995572999A98975572997CC4E755
              7299425583514C62FF00FF8F8F905454545B57573B3A3C5B5555E3B18EFFFFFF
              557299CBEFF65572997CC4E75572997CC4E75572993E598C374D848F8F909195
              9B5B5757524D4E5B5555EDBD92FFFFFFFCFCFD557299CBEFF65572997CC4E755
              72997CC4E75EADEA4D99E48F8F90F1F1F15B5757686463635E5EEDBD92FAF2ED
              FAF2EDF8F0EC557299A5E9F45572997CC4E76EBEEF5EACE94D99E48F8F90F1F1
              F15B5757757272767474EDBD92E0B398E1B599E1B599DCB19855729997C2D9C4
              EBF66EC0F15EAEEB3B5D9B8F8F90F1F1F15B57578C8C8C8C8C8CFF00FFFF00FF
              FF00FFFF00FFFF00FFFF00FFFF00FF6A96C05A8DC7556DA4FF00FF8F8F908F8F
              905B57578C8C8C7A7979FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
              00FFFF00FFFF00FFFF00FFFF00FFFF00FF5B57575B57575B5757}
            OnClick = btnDivisionClick
          end
          object Division: TDBLookupComboBox
            Left = 107
            Top = 0
            Width = 113
            Height = 21
            BevelInner = bvSpace
            BevelOuter = bvNone
            BevelKind = bkTile
            DataField = 'Division'
            DataSource = srcPerson
            KeyField = 'ID'
            ListField = 'DivisionName'
            ListSource = srcDivision
            TabOrder = 0
          end
        end
        object Panel4: TPanel
          AlignWithMargins = True
          Left = 6
          Top = 93
          Width = 243
          Height = 24
          BevelOuter = bvNone
          TabOrder = 5
          object Label4: TLabel
            Left = 0
            Top = 2
            Width = 101
            Height = 21
            AutoSize = False
            Caption = #1044#1083#1098#1078#1085#1086#1089#1090
            FocusControl = Position
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object btnPosition: TSpeedButton
            Left = 220
            Top = 0
            Width = 23
            Height = 23
            Flat = True
            Glyph.Data = {
              36030000424D3603000000000000360000002800000010000000100000000100
              1800000000000003000000000000000000000000000000000000FF00FFFF00FF
              FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
              FFFF00FFFF00FFFF00FFA0726EA0726EA0726FA0726EA0726EA0726EA0726EA0
              726EA0726EA0726EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFA0726EEDDFD1
              E9DACBE9D8C8E8D7C5E8D7C4E8D6C5E8D7C6EDDFCCA0726EFF00FFFF00FFFF00
              FFFF00FFFF00FFFF00FFBF8A85FAF2E5F5ECE0F3EADDF3E8D9F1E6D6F1E5D4F1
              E6D4F8EDD9A0726EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC48F87F9F3EA
              F9DFC3FAD7B5F9D7B6F8D7B4F8D6B1F6DABCF6ECD9A0726EFF00FFFF00FFFF00
              FFFF00FFFF00FFFF00FFCA9588F9F6EFF8E9D8F8E4D0F7E3CEF6E2CBF6E0C8F2
              DFC9F4EAD7A0726EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFD19D89FBF8F2
              F8DFC5F6D5B3F8D7B6FAD7B5F2D0ADDEC7ACDCD5C7A0726EFF00FFFF00FFFF00
              FFFF00FFFF00FFFF00FFD6A38BFCF9F5E8DED3D3C6BADED0C2E8D9CACFC0B1A7
              9C91A7A298967A73FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFDCA98CFCFCFB
              9FA6B7AF967FB09780BEA3895572995B7CA460617677625DFF00FFFF00FFFF00
              FF5B5757464242464242E0AE8DFEFEFE5572995572999A98975572997CC4E755
              7299425583514C62FF00FF8F8F905454545B57573B3A3C5B5555E3B18EFFFFFF
              557299CBEFF65572997CC4E75572997CC4E75572993E598C374D848F8F909195
              9B5B5757524D4E5B5555EDBD92FFFFFFFCFCFD557299CBEFF65572997CC4E755
              72997CC4E75EADEA4D99E48F8F90F1F1F15B5757686463635E5EEDBD92FAF2ED
              FAF2EDF8F0EC557299A5E9F45572997CC4E76EBEEF5EACE94D99E48F8F90F1F1
              F15B5757757272767474EDBD92E0B398E1B599E1B599DCB19855729997C2D9C4
              EBF66EC0F15EAEEB3B5D9B8F8F90F1F1F15B57578C8C8C8C8C8CFF00FFFF00FF
              FF00FFFF00FFFF00FFFF00FFFF00FF6A96C05A8DC7556DA4FF00FF8F8F908F8F
              905B57578C8C8C7A7979FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
              00FFFF00FFFF00FFFF00FFFF00FFFF00FF5B57575B57575B5757}
            OnClick = btnPositionClick
          end
          object Position: TDBLookupComboBox
            Left = 107
            Top = 0
            Width = 113
            Height = 21
            BevelInner = bvSpace
            BevelOuter = bvNone
            BevelKind = bkTile
            DataField = 'Position'
            DataSource = srcPerson
            KeyField = 'ID'
            ListField = 'PositionName'
            ListSource = srcPosition
            TabOrder = 0
          end
        end
      end
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
            Action = acAdd
            ImageIndex = 0
          end
          item
            Action = acDelete
            ImageIndex = 19
          end
          item
            Action = acUser
            ImageIndex = 38
          end
          item
            Action = acClose
            ImageIndex = 24
          end>
        ActionBar = ActionToolBar1
      end>
    Images = dmGUI.Images
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
    object acUser: TAction
      Caption = #1044#1086#1089#1090#1098#1087
      ImageIndex = 38
      OnExecute = acUserExecute
    end
  end
  object srcPerson: TDataSource
    DataSet = dmGUI.PersonEdit
    Left = 80
    Top = 96
  end
  object srcWorkgroup: TDataSource
    DataSet = dmGUI.WorkGroupList
    Left = 488
    Top = 184
  end
  object srcDivision: TDataSource
    DataSet = dmGUI.DivisionList
    Left = 520
    Top = 184
  end
  object srcPosition: TDataSource
    DataSet = dmGUI.PositionList
    Left = 552
    Top = 184
  end
end
