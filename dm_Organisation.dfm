object dmOrganisation: TdmOrganisation
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 529
  Width = 389
  object Cities: TClientDataSet
    Aggregates = <>
    FileName = 'city.xml'
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'CitiesIndex1'
        Fields = 'ID'
        Options = [ixPrimary, ixUnique]
      end>
    IndexName = 'CitiesIndex1'
    Params = <>
    StoreDefs = True
    AfterOpen = CitiesAfterOpen
    BeforePost = CitiesBeforePost
    Left = 80
    Top = 56
    object CitiesID: TFloatField
      FieldName = 'ID'
    end
    object CitiesEKATTE: TStringField
      FieldName = 'EKATTE'
      Size = 5
    end
    object CitiesName: TStringField
      FieldName = 'Name'
      Size = 50
    end
    object CitiesKmetstvo: TStringField
      FieldName = 'Kmetstvo'
      Size = 10
    end
    object CitiesOblast: TStringField
      FieldKind = fkInternalCalc
      FieldName = 'Oblast'
      OnGetText = CitiesOblastGetText
      OnSetText = CitiesOblastSetText
      Size = 10
    end
    object CitiesObstina: TStringField
      FieldKind = fkInternalCalc
      FieldName = 'Obstina'
      OnGetText = CitiesObstinaGetText
      OnSetText = CitiesObstinaSetText
    end
    object CitiesKind: TIntegerField
      FieldName = 'Kind'
    end
    object CitiesCategory: TIntegerField
      FieldName = 'Category'
    end
    object CitiesAltitude: TIntegerField
      FieldName = 'Altitude'
    end
    object CitiesDocument: TStringField
      FieldName = 'Document'
      Size = 10
    end
    object CitiesTsb: TStringField
      FieldName = 'Tsb'
      Size = 10
    end
    object CitiesRegion: TStringField
      FieldName = 'Region'
      Size = 5
    end
    object CitiesPostCode: TStringField
      FieldName = 'PostCode'
      Size = 4
    end
    object CitiesPhoneCode: TStringField
      FieldName = 'PhoneCode'
      Size = 10
    end
    object CitiesLongitude: TStringField
      FieldName = 'Longitude'
      Size = 15
    end
    object CitiesLatitude: TStringField
      FieldName = 'Latitude'
      Size = 15
    end
    object CitiesLastChange: TDateTimeField
      FieldName = 'LastChange'
    end
    object CitiesNameHex: TStringField
      FieldName = 'NameHex'
      Size = 100
    end
  end
  object CitiesClone: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    OnFilterRecord = CitiesCloneFilterRecord
    Left = 80
    Top = 112
  end
  object Organisations: TClientDataSet
    Aggregates = <>
    FileName = 'organisation.xml'
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'OrganisationsIndex1'
        Fields = 'Code'
        Options = [ixPrimary, ixUnique]
      end>
    IndexName = 'OrganisationsIndex1'
    Params = <>
    StoreDefs = True
    OnCalcFields = OrganisationsCalcFields
    Left = 152
    Top = 56
    object OrganisationsCode: TFloatField
      FieldName = 'Code'
    end
    object OrganisationsName: TStringField
      FieldName = 'Name'
      Size = 50
    end
    object OrganisationsLevel: TIntegerField
      FieldName = 'Level'
    end
    object OrganisationsParent: TFloatField
      FieldKind = fkCalculated
      FieldName = 'Parent'
      Calculated = True
    end
    object OrganisationsFullName: TStringField
      FieldName = 'Name1'
      Size = 50
    end
    object OrganisationsName2: TStringField
      FieldName = 'Name2'
      Size = 50
    end
    object OrganisationsName3: TStringField
      FieldName = 'Name3'
      Size = 50
    end
    object OrganisationsPhone: TStringField
      FieldName = 'Phone'
      Size = 50
    end
    object OrganisationsPhone2: TStringField
      FieldName = 'Phone2'
      Size = 50
    end
    object OrganisationsPhone3: TStringField
      FieldName = 'Phone3'
      Size = 50
    end
    object OrganisationsPosition: TStringField
      FieldName = 'Position'
      OnChange = OrganisationsPositionChange
      Size = 10
    end
    object OrganisationsEMail: TStringField
      FieldName = 'EMail'
      Size = 50
    end
    object OrganisationsDTMF: TStringField
      FieldName = 'DTMF'
      Size = 10
    end
    object OrganisationsSite: TStringField
      FieldName = 'Site'
      Size = 100
    end
    object OrganisationsCity1: TStringField
      FieldName = 'City1'
      OnChange = OrganisationsCity1Change
    end
    object OrganisationsObstina1: TStringField
      FieldKind = fkCalculated
      FieldName = 'Obstina1'
      OnGetText = OrganisationsObstina1GetText
      Size = 50
      Calculated = True
    end
    object OrganisationsOblast1: TStringField
      FieldKind = fkCalculated
      FieldName = 'Oblast1'
      OnGetText = OrganisationsOblast1GetText
      Size = 50
      Calculated = True
    end
    object OrganisationsRaion1: TStringField
      FieldName = 'Raion1'
      Size = 8
    end
    object OrganisationsAddress1: TStringField
      FieldName = 'Address1'
      Size = 1000
    end
    object OrganisationsCity2: TStringField
      FieldName = 'City2'
      OnChange = OrganisationsCity2Change
    end
    object OrganisationsObstina2: TStringField
      FieldKind = fkCalculated
      FieldName = 'Obstina2'
      OnGetText = OrganisationsObstina2GetText
      Size = 50
      Calculated = True
    end
    object OrganisationsOblast2: TStringField
      FieldKind = fkCalculated
      FieldName = 'Oblast2'
      OnGetText = OrganisationsOblast2GetText
      Size = 50
      Calculated = True
    end
    object OrganisationsRaion2: TStringField
      FieldName = 'Raion2'
      Size = 8
    end
    object OrganisationsAddress2: TStringField
      FieldName = 'Address2'
      Size = 1000
    end
    object OrganisationsNewCode: TFloatField
      FieldKind = fkInternalCalc
      FieldName = 'NewCode'
    end
  end
  object Positions: TClientDataSet
    Aggregates = <>
    FileName = 'position.xml'
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'PositionsIndex1'
        Fields = 'code'
        Options = [ixPrimary, ixUnique]
      end>
    IndexName = 'PositionsIndex1'
    Params = <>
    StoreDefs = True
    AfterOpen = PositionsAfterOpen
    Left = 232
    Top = 56
    object PositionsCode: TStringField
      FieldName = 'Code'
      Size = 8
    end
    object PositionsCode1: TStringField
      FieldKind = fkInternalCalc
      FieldName = 'Code1'
      OnSetText = PositionsCode1SetText
      Size = 4
    end
    object PositionsCode2: TStringField
      FieldKind = fkInternalCalc
      FieldName = 'Code2'
      OnSetText = PositionsCode2SetText
      Size = 4
    end
    object PositionsPositionName: TStringField
      FieldName = 'PositionName'
      Size = 50
    end
  end
  object PositionsClone: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 232
    Top = 112
  end
  object Altitude: TClientDataSet
    Aggregates = <>
    FileName = 'altitude.xml'
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'AltitudeIndex1'
        Fields = 'altitude'
        Options = [ixPrimary, ixUnique]
      end>
    IndexName = 'AltitudeIndex1'
    Params = <>
    StoreDefs = True
    Left = 80
    Top = 272
    object Altitudealtitude: TStringField
      FieldName = 'altitude'
      Size = 2
    end
    object Altitudename: TStringField
      FieldName = 'name'
    end
  end
  object Kind: TClientDataSet
    Aggregates = <>
    FileName = 'kind.xml'
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'KindIndex1'
        Fields = 'kind'
        Options = [ixPrimary, ixUnique]
      end>
    IndexName = 'KindIndex1'
    Params = <>
    StoreDefs = True
    Left = 128
    Top = 272
    object Kindkind: TStringField
      FieldName = 'kind'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Size = 1
    end
    object Kindname: TStringField
      FieldName = 'name'
      Size = 4
    end
    object Kindfullname: TStringField
      FieldName = 'fullname'
      Size = 10
    end
  end
  object Region: TClientDataSet
    Aggregates = <>
    FileName = 'region.xml'
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'RegionIndex1'
        Fields = 'region'
        Options = [ixPrimary, ixUnique]
      end>
    IndexName = 'RegionIndex1'
    Params = <>
    StoreDefs = True
    Left = 176
    Top = 272
    object Regionregion: TStringField
      FieldName = 'region'
      Size = 2
    end
    object Regionekatte: TStringField
      FieldName = 'ekatte'
      Size = 5
    end
    object Regionname: TStringField
      FieldName = 'name'
      Size = 50
    end
    object Regiondocument: TStringField
      FieldName = 'document'
      Size = 10
    end
  end
  object TSB: TClientDataSet
    Aggregates = <>
    FileName = 'tsb.xml'
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'TSBIndex1'
        Fields = 'tsb'
        Options = [ixPrimary, ixUnique]
      end>
    IndexName = 'TSBIndex1'
    Params = <>
    StoreDefs = True
    Left = 224
    Top = 272
    object TSBtsb: TStringField
      FieldName = 'tsb'
      Size = 2
    end
    object TSBname: TStringField
      FieldName = 'name'
      Size = 50
    end
  end
  object Raion: TClientDataSet
    Aggregates = <>
    FileName = 'raion.xml'
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'RaionIndex1'
        Fields = 'raion'
        Options = [ixPrimary, ixUnique]
      end>
    IndexName = 'RaionIndex1'
    Params = <>
    StoreDefs = True
    AfterOpen = RaionAfterOpen
    Left = 272
    Top = 272
    object Raionraion: TStringField
      FieldName = 'raion'
      Size = 8
    end
    object Raionname: TStringField
      FieldName = 'name'
      Size = 30
    end
    object Raionnamehex: TStringField
      FieldName = 'namehex'
      Visible = False
      Size = 60
    end
  end
  object RaionClone: TClientDataSet
    Aggregates = <>
    Params = <>
    BeforePost = RaionCloneBeforePost
    Left = 272
    Top = 328
  end
end
