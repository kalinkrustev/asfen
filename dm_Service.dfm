object dmService: TdmService
  OldCreateOrder = False
  Dependencies = <
    item
      Name = 'MSSQLSERVER'
      IsGroup = False
    end>
  DisplayName = 'Service1'
  Interactive = True
  OnContinue = ServiceContinue
  OnPause = ServicePause
  OnShutdown = ServiceShutdown
  OnStop = ServiceStop
  Height = 319
  Width = 491
end
