object dmFixedConnPool: TdmFixedConnPool
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 260
  Width = 371
  object dbConnection: TADOConnection
    ConnectionString = 'FILE NAME=database.udl'
    LoginPrompt = False
    Provider = 'database.udl'
    Left = 48
    Top = 24
  end
end
