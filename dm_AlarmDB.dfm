object dmAlarmDB: TdmAlarmDB
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 374
  Width = 497
  object RSettings: TADOQuery
    Parameters = <
      item
        Name = 'key'
        Size = -1
        Value = Null
      end
      item
        Name = 'id'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'select * from tblSettings where SettingName=:key and SettingID=:' +
        'id')
    Left = 40
    Top = 40
  end
  object WSettings: TADOCommand
    CommandText = 
      'if exists (select * from tblSettings where SettingName=:name1 an' +
      'd SettingID=:id1)'#13#10'update tblSettings'#13#10'set SettingValue=:value2'#13 +
      #10'where SettingName=:name2 and SettingID=:id2'#13#10'else'#13#10'insert into ' +
      'tblSettings(SettingName,SettingID,SettingValue)'#13#10'values (:name3,' +
      ':id3,:value3)'
    ExecuteOptions = [eoExecuteNoRecords]
    Parameters = <
      item
        Name = 'name1'
        DataType = ftString
        Size = -1
        Value = Null
      end
      item
        Name = 'id1'
        DataType = ftInteger
        Value = Null
      end
      item
        Name = 'value2'
        DataType = ftString
        Size = -1
        Value = Null
      end
      item
        Name = 'name2'
        DataType = ftString
        Size = -1
        Value = Null
      end
      item
        Name = 'id2'
        DataType = ftInteger
        Value = Null
      end
      item
        Name = 'name3'
        DataType = ftString
        Size = -1
        Value = Null
      end
      item
        Name = 'id3'
        DataType = ftInteger
        Value = Null
      end
      item
        Name = 'value3'
        DataType = ftString
        Size = -1
        Value = Null
      end>
    ParamCheck = False
    Left = 112
    Top = 40
  end
  object WSettings1: TADOQuery
    ExecuteOptions = [eoExecuteNoRecords]
    ParamCheck = False
    Parameters = <
      item
        Name = 'name1'
        DataType = ftString
        Size = -1
        Value = Null
      end
      item
        Name = 'id1'
        DataType = ftInteger
        Size = -1
        Value = Null
      end
      item
        Name = 'value2'
        DataType = ftString
        Size = -1
        Value = Null
      end
      item
        Name = 'name2'
        DataType = ftString
        Size = -1
        Value = Null
      end
      item
        Name = 'id2'
        DataType = ftInteger
        Size = -1
        Value = Null
      end
      item
        Name = 'name3'
        DataType = ftString
        Size = -1
        Value = Null
      end
      item
        Name = 'id3'
        DataType = ftInteger
        Size = -1
        Value = Null
      end
      item
        Name = 'value3'
        DataType = ftString
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'if exists (select * from tblSettings where SettingName=:name1 an' +
        'd SettingID=:id1)'
      'update tblSettings'
      'set SettingValue=:value2'
      'where SettingName=:name2 and SettingID=:id2'
      'else'
      'insert into tblSettings(SettingName,SettingID,SettingValue)'
      'values (:name3,:id3,:value3)')
    Left = 112
    Top = 96
  end
  object TransferExport: TADOStoredProc
    ProcedureName = 'TransferExport'
    Parameters = <
      item
        Name = '@RETURN_VALUE'
        DataType = ftInteger
        Direction = pdReturnValue
        Precision = 10
        Value = Null
      end
      item
        Name = '@ExportOrganisation'
        Attributes = [paNullable]
        DataType = ftBoolean
        Value = Null
      end
      item
        Name = '@ExportSettings'
        Attributes = [paNullable]
        DataType = ftBoolean
        Value = Null
      end
      item
        Name = '@ExportUsers'
        Attributes = [paNullable]
        DataType = ftBoolean
        Value = Null
      end
      item
        Name = '@ExportStats'
        Attributes = [paNullable]
        DataType = ftBoolean
        Value = Null
      end
      item
        Name = '@FileName'
        Attributes = [paNullable]
        DataType = ftString
        Size = 1000
        Value = Null
      end
      item
        Name = '@Options'
        Attributes = [paNullable]
        DataType = ftString
        Size = 1000
        Value = Null
      end>
    Left = 192
    Top = 40
  end
  object Databases: TADOQuery
    Parameters = <>
    SQL.Strings = (
      'select name from master.dbo.sysdatabases where name like '#39'asos%'#39)
    Left = 112
    Top = 160
  end
  object TransferImport: TADOStoredProc
    ProcedureName = 'Alarm.TransferImport'
    Parameters = <
      item
        Name = '@RETURN_VALUE'
        DataType = ftInteger
        Direction = pdReturnValue
        Precision = 10
        Value = Null
      end
      item
        Name = '@FileName'
        Attributes = [paNullable]
        DataType = ftString
        Size = 1000
        Value = Null
      end>
    Left = 192
    Top = 96
  end
  object ADOQuery: TADOQuery
    Connection = ADOConnection
    CursorType = ctStatic
    Parameters = <>
    SQL.Strings = (
      'select * from [ek_obl$]')
    Left = 336
    Top = 240
  end
  object ADOConnection: TADOConnection
    CursorLocation = clUseServer
    LoginPrompt = False
    Mode = cmShareDenyNone
    Left = 248
    Top = 240
  end
  object SaveXLS: TADOQuery
    Parameters = <>
    SQL.Strings = (
      'select name from master.dbo.sysdatabases where name like '#39'asos%'#39)
    Left = 112
    Top = 240
  end
end
