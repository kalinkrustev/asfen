object dmAlarm: TdmAlarm
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 285
  Width = 417
  object TrayIcon1: TTrayIcon
    Icon.Data = {
      0000010001002020100000000000E80200001600000028000000200000004000
      0000010004000000000080020000000000000000000000000000000000000000
      000000008000008000000080800080000000800080008080000080808000C0C0
      C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
      000000000000330077000000000000000000000000003B077070000000000000
      000000000000BB807007000000000000000000000300B0007000700000000000
      00000000330070070700070000000000000000003B0700700070007000000000
      00000000BB800700000700070000000000000300B00070000000700070000000
      0000330070070000000007000700000000003B07007000000000007007000000
      0000BB800700000000000007070000000300B000700000000070000077000000
      330070070000000007000000803300003B070070000000000000000800330000
      BB8007000000000000000080BBBB0300B000700000000070000008000BB03300
      70070000000707000000803300003B070070000000707000000800330000BB80
      07000000070700000080BBBB0000B000700000000070000008000BB000007007
      0000000007000000803300000000707000007770000000080033000000008700
      0007070700000080BBBB00000000080000077777000008000BB0000000000080
      0007070700008033000000000000000800007770000800330000000000000000
      800000000080BBBB00000000000000000800000008000BB00000000000000000
      0080000080330000000000000000000000080008003300000000000000000000
      00008080BBBB00000000000000000000000008000BB00000000000000000FFFF
      33FFFFFF21FFFFFF00FFFFFB007FFFF3003FFFF2001FFFF0000FFFB00007FF30
      0003FF200003FF000003FB000003F3000000F2000000F0000010B00000393000
      000F2000000F0000010F0000039F000000FF000000FF000010FF800039FFC000
      0FFFE0000FFFF0010FFFF8039FFFFC00FFFFFE00FFFFFF10FFFFFFB9FFFF}
    OnMouseDown = TrayIcon1MouseDown
    Left = 192
    Top = 128
  end
  object PopupMenu1: TPopupMenu
    Left = 112
    Top = 128
    object mnuExit: TMenuItem
      Caption = 'Exit'
      OnClick = mnuExitClick
    end
  end
  object QRingGroup: TADOQuery
    Parameters = <>
    SQL.Strings = (
      'select'
      
        '  min(priority) priority,code,ringgroupname,organisationname,div' +
        'isionname,positionname,'
      
        '  personname,phone,dtmf,phone2 mobilephone,phone3 homephone,emai' +
        'l,site,'
      '  case'
      '    when address1<>'#39#39' then address1+'#39', '#39
      '    when divaddress1<>'#39#39' then divaddress1+'#39', '#39
      '    when orgaddress1<>'#39#39' then orgaddress1+'#39', '#39
      '    else '#39#39
      '  end+city1 address1,'
      '  case'
      '    when address2<>'#39#39' then address2+'#39', '#39
      '    when divaddress2<>'#39#39' then divaddress2+'#39', '#39
      '    when orgaddress2<>'#39#39' then orgaddress2+'#39', '#39
      '    else '#39#39
      '  end+city2 address2'
      'from('
      'select'
      '  m.priority,'
      '  convert(bigint,p.code) code,'
      '  g.ringgroupname,'
      '  o.name organisationname,'
      '  div.name divisionname,'
      '  p.name positionname,'
      
        '  rtrim(isnull(p.name1,'#39#39')+'#39' '#39'+ltrim(isnull(p.name2,'#39#39')+'#39' '#39'+isnu' +
        'll(p.name3,'#39#39'))) personname,'
      '  p.phone,'
      '  p.dtmf,'
      '  p.phone2,'
      '  p.phone3,'
      '  p.email,'
      '  p.site,'
      '  p.address1,'
      '  p.address2,'
      '  div.address1 divaddress1,'
      '  o.address1 orgaddress1,'
      '  div.address2 divaddress2,'
      '  o.address2 orgaddress2,'
      '  coalesce(c1.name,divc1.name,oc1.name,'#39#39') city1,'
      '  coalesce(c2.name,divc2.name,oc2.name,'#39#39') city2'
      'from'
      '  tblOrganisation p'
      'left join'
      '  vCity c1 on c1.ekatte=p.city1'
      'left join'
      '  vCity c2 on c2.ekatte=p.city2'
      'left join'
      '  tblOrganisation div on'
      
        '  left(case when len(convert(bigint,div.code))%%2=1 then '#39'0'#39' els' +
        'e '#39#39' end+convert(varchar(20),convert(bigint,div.code))+'#39'00000000' +
        #39',12)='
      
        '  case when len(convert(bigint,p.code))%%2=1 then '#39'0'#39' else '#39#39' en' +
        'd+convert(varchar(20),convert(bigint,floor(p.code/100)))'
      '  and (div.code>99) and (div.code<1e12)'
      'left join'
      '  vCity divc1 on divc1.ekatte=div.city1'
      'left join'
      '  vCity divc2 on divc2.ekatte=div.city2'
      'left join'
      '  tblOrganisation o on o.code=floor(p.code/1000000000000)'
      'left join'
      '  vCity oc1 on oc1.ekatte=o.city1'
      'left join'
      '  vCity oc2 on oc2.ekatte=o.city2'
      'join'
      
        '  tblRingGroupMembers m on convert(bigint,p.code) like m.person+' +
        #39'%%'#39
      'join'
      '  tblRingGroups g on g.id=%0:1d'
      'where'
      '  p.code>=1e12 and'
      '  m.ringgroup=%0:1d'
      ') a'
      'group by'
      
        '  code,ringgroupname,organisationname,divisionname,positionname,' +
        'personname,phone,dtmf,phone2,phone3,email,site,'
      
        '  address1,address2,divaddress1,orgaddress1,divaddress2,orgaddre' +
        'ss2,'
      '  city1,city2'
      'order by'
      '  1,2')
    Left = 296
    Top = 128
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 192
    Top = 80
  end
  object StartupTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = StartupTimerTimer
    Left = 192
    Top = 184
  end
end
