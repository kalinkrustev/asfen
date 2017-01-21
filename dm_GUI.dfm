object dmGUI: TdmGUI
  OldCreateOrder = False
  OnCreate = max
  Height = 849
  Width = 598
  object Connection: TADOConnection
    ConnectionString = 'FILE NAME=database.udl'
    LoginPrompt = False
    Provider = 'database.udl'
    Left = 48
    Top = 24
  end
  object WorkgroupEdit: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    BeforeInsert = UsersEdit
    BeforeEdit = UsersEdit
    BeforePost = SetLast
    BeforeDelete = UsersChange
    CommandText = 'select * from tblWorkgroup'
    Parameters = <>
    Left = 48
    Top = 80
  end
  object RingGroupsEdit: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    BeforeInsert = SettingsEdit
    BeforeEdit = SettingsEdit
    BeforePost = SetLast
    AfterPost = RingGroupsEditAfterPost
    BeforeDelete = SettingsChange
    AfterDelete = RingGroupsEditAfterDelete
    CommandText = 'select * from tblRingGroups'
    Parameters = <>
    Left = 48
    Top = 320
  end
  object RingGroupMembersEdit: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    BeforeInsert = SettingsEdit
    BeforeEdit = SettingsEdit
    BeforePost = SetLast
    BeforeDelete = SettingsChange
    CommandText = 'select * from tblRingGroupMembers'
    Parameters = <>
    Left = 48
    Top = 368
  end
  object WorkGroupList: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    BeforeInsert = AbortChanges
    BeforeEdit = AbortChanges
    BeforeDelete = AbortChanges
    CommandText = 'select * from tblWorkgroup'
    Parameters = <>
    Left = 128
    Top = 80
  end
  object RightsList: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    BeforeInsert = AbortChanges
    BeforeEdit = AbortChanges
    BeforeDelete = AbortChanges
    CommandText = 'select * from tblRights'
    Parameters = <>
    Left = 128
    Top = 128
  end
  object NomRightsList: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    BeforeInsert = AbortChanges
    BeforeEdit = AbortChanges
    BeforeDelete = AbortChanges
    CommandText = 'select * from tblNomRights'
    Parameters = <>
    Left = 128
    Top = 224
  end
  object RightsEdit: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    BeforeInsert = UsersEdit
    BeforeEdit = UsersEdit
    BeforePost = SetLast
    BeforeDelete = UsersChange
    CommandText = 'select * from tblRights'
    Parameters = <>
    Left = 48
    Top = 128
  end
  object RecordingsList: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    BeforeInsert = AbortChanges
    BeforeEdit = AbortChanges
    BeforeDelete = AbortChanges
    CommandText = 'select FileID,FileName from tblFiles where FilePath='#39#1089#1098#1086#1073#1097#1077#1085#1080#1103#39
    Parameters = <>
    Left = 128
    Top = 416
  end
  object ScriptList: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    BeforeInsert = AbortChanges
    BeforeEdit = AbortChanges
    BeforeDelete = AbortChanges
    CommandText = 'select FileID,FileName from tblFiles where FilePath='#39#1089#1094#1077#1085#1072#1088#1080#1080#39
    Parameters = <>
    Left = 200
    Top = 416
  end
  object RingGroupsList: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    BeforeInsert = AbortChanges
    BeforeEdit = AbortChanges
    BeforeDelete = AbortChanges
    CommandText = 'select * from tblRingGroups'
    Parameters = <>
    Left = 128
    Top = 320
  end
  object FilesEdit: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    BeforePost = SetLast
    CommandText = 'select * from tblFiles'
    Parameters = <>
    Left = 48
    Top = 416
  end
  object UserList: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    BeforeInsert = AbortChanges
    BeforeEdit = AbortChanges
    BeforeDelete = AbortChanges
    CommandText = 'select * from tblUser'
    Parameters = <>
    Left = 128
    Top = 176
  end
  object UserEdit: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    BeforeInsert = UsersEdit
    BeforeEdit = UsersEdit
    BeforePost = SetLast
    BeforeDelete = UsersChange
    CommandText = 'select * from tblUser'
    Parameters = <>
    Left = 48
    Top = 176
  end
  object EventList: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    BeforeInsert = AbortChanges
    BeforeEdit = AbortChanges
    BeforeDelete = AbortChanges
    CommandText = 'select FileID,FileName from tblFiles where FilePath='#39#1089#1098#1073#1080#1090#1080#1103#39
    Parameters = <>
    Left = 272
    Top = 416
  end
  object OrganisationsList: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    BeforeInsert = AbortChanges
    BeforeEdit = AbortChanges
    BeforeDelete = AbortChanges
    OnCalcFields = OrganisationsListCalcFields
    CommandText = 'select * from tblOrganisation'
    Parameters = <>
    Left = 200
    Top = 544
  end
  object OrganisationsEdit: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    AfterOpen = OrganisationsEditAfterOpen
    BeforeInsert = OrganisationsBeforeEdit
    BeforeEdit = OrganisationsBeforeEdit
    BeforePost = SetLast
    AfterPost = OrganisationsEditAfterPost
    BeforeDelete = OrganisationsChange
    AfterDelete = OrganisationsEditAfterDelete
    OnCalcFields = OrganisationsEditCalcFields
    CommandText = 'select * from tblOrganisation'
    Parameters = <>
    Left = 48
    Top = 544
  end
  object City: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    BeforeInsert = OrganisationsBeforeEdit
    BeforeEdit = OrganisationsBeforeEdit
    BeforePost = SetLast
    BeforeDelete = OrganisationsChange
    CommandText = 'select * from tblCity order by Name'
    Parameters = <>
    Left = 48
    Top = 592
  end
  object Position: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    AfterOpen = PositionAfterOpen
    BeforeInsert = OrganisationsBeforeEdit
    BeforeEdit = OrganisationsBeforeEdit
    BeforePost = SetLast
    BeforeDelete = OrganisationsChange
    CommandText = 'select * from tblPosition order by PositionName'
    Parameters = <>
    Left = 200
    Top = 640
  end
  object Raion: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    AfterOpen = RaionAfterOpen
    BeforeInsert = OrganisationsBeforeEdit
    BeforeEdit = OrganisationsBeforeEdit
    BeforePost = SetLast
    BeforeDelete = OrganisationsChange
    CommandText = 'select * from tblRaion order by Name'
    Parameters = <>
    Left = 200
    Top = 688
  end
  object CityListQ: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    BeforeInsert = AbortChanges
    BeforeEdit = AbortChanges
    BeforeDelete = AbortChanges
    CommandText = 'select * from tblCity where Len(kmetstvo)>5 order by Name'
    Parameters = <>
    Left = 128
    Top = 592
  end
  object PersonList: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    AfterOpen = PersonListAfterOpen
    BeforeInsert = AbortChanges
    BeforeEdit = AbortChanges
    BeforeDelete = AbortChanges
    CommandText = 'select * from tblOrganisation where Code>=1e12 order by Code'
    Parameters = <>
    Left = 128
    Top = 544
  end
  object Kind: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    AfterOpen = NomAfterOpen
    BeforeInsert = OrganisationsBeforeEdit
    BeforeEdit = OrganisationsBeforeEdit
    BeforePost = SetLast
    BeforeDelete = OrganisationsChange
    CommandText = 'select * from tblKind order by kind'
    Parameters = <>
    Left = 48
    Top = 640
  end
  object TSB: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    AfterOpen = NomAfterOpen
    BeforeInsert = OrganisationsBeforeEdit
    BeforeEdit = OrganisationsBeforeEdit
    BeforePost = SetLast
    BeforeDelete = OrganisationsChange
    CommandText = 'select * from tblTSB order by name'
    Parameters = <>
    Left = 128
    Top = 640
  end
  object Altitude: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    AfterOpen = NomAfterOpen
    BeforeInsert = OrganisationsBeforeEdit
    BeforeEdit = OrganisationsBeforeEdit
    BeforePost = SetLast
    BeforeDelete = OrganisationsChange
    CommandText = 'select * from tblAltitude order by altitude'
    Parameters = <>
    Left = 48
    Top = 688
  end
  object Region: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    AfterOpen = NomAfterOpen
    BeforeInsert = OrganisationsBeforeEdit
    BeforeEdit = OrganisationsBeforeEdit
    BeforePost = SetLast
    BeforeDelete = OrganisationsChange
    CommandText = 'select * from tblRegion order by region'
    Parameters = <>
    Left = 128
    Top = 688
  end
  object CityList: TClientDataSet
    Aggregates = <>
    Params = <>
    ProviderName = 'prvCityList'
    AfterOpen = CityListAfterOpen
    BeforeInsert = AbortChanges
    BeforeEdit = AbortChanges
    BeforeDelete = AbortChanges
    Left = 272
    Top = 592
  end
  object prvCityList: TDataSetProvider
    DataSet = CityListQ
    Left = 200
    Top = 592
  end
  object FileList: TClientDataSet
    Aggregates = <>
    IndexFieldNames = 'FileID'
    Params = <>
    ProviderName = 'prvFileList'
    AfterOpen = FileListAfterOpen
    Left = 456
    Top = 416
  end
  object prvFileList: TDataSetProvider
    DataSet = FileListQ
    Left = 400
    Top = 416
  end
  object FileListQ: TADODataSet
    Connection = Connection
    CommandText = 'select * from tblFiles'
    Parameters = <>
    Left = 336
    Top = 416
  end
  object OrgExport: TClientDataSet
    Aggregates = <>
    Params = <>
    ProviderName = 'prvOrg'
    AfterOpen = CityListAfterOpen
    BeforeInsert = AbortChanges
    BeforeEdit = AbortChanges
    BeforeDelete = AbortChanges
    Left = 336
    Top = 544
  end
  object prvOrg: TDataSetProvider
    DataSet = OrganisationsList
    Left = 272
    Top = 544
  end
  object MaxID: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    BeforeInsert = OrganisationsBeforeEdit
    BeforeEdit = OrganisationsBeforeEdit
    BeforePost = SetLast
    BeforeDelete = OrganisationsChange
    OnCalcFields = OrganisationsEditCalcFields
    CommandText = 
      'select max(case when code<100 then code else 0 end) code ,sum(ca' +
      'se when code>1E12 then 1 else 0 end) cnt from tblOrganisation'
    Parameters = <>
    Left = 400
    Top = 544
  end
  object Crisis: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    AfterOpen = NomAfterOpen
    BeforePost = SetLast
    CommandText = 'select * from tblCrisis order by id'
    Parameters = <>
    Left = 48
    Top = 464
  end
  object QPathSize: TADOQuery
    Connection = Connection
    Parameters = <>
    Left = 520
    Top = 416
  end
  object OrganisationExp: TADODataSet
    Connection = Connection
    CursorType = ctStatic
    BeforeInsert = AbortChanges
    BeforeEdit = AbortChanges
    BeforeDelete = AbortChanges
    CommandText = 
      'select replicate('#39'0'#39',len(cast(code as numeric)) % 2)+cast(cast(c' +
      'ode as numeric) as varchar(20))+replicate('#39'0'#39',14-len(cast(code a' +
      's numeric))-len(cast(code as numeric)) % 2) Sort,Code,Name,Level' +
      ',Name1,Name2,Name3,Phone,Phone2,Phone3,Position,EMail,DTMF,Site,' +
      'City1,Raion1,Address1,City2,Raion2,Address2 from tblOrganisation' +
      ' order by 1'
    Parameters = <>
    Left = 464
    Top = 544
  end
  object OrganisationImp: TADOQuery
    CursorType = ctStatic
    Parameters = <>
    SQL.Strings = (
      'select * from [organisation$]')
    Left = 464
    Top = 592
  end
  object OrgDelete: TADOQuery
    Connection = Connection
    Parameters = <>
    SQL.Strings = (
      'delete from  tblOrganisation')
    Left = 336
    Top = 592
  end
end
