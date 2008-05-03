object XPService: TXPService
  OldCreateOrder = False
  DisplayName = 'XP Elevation Service'
  StartType = stManual
  OnExecute = ServiceExecute
  OnShutdown = ServiceShutdown
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
  object EurekaLog1: TEurekaLog
    OnCustomWebFieldsRequest = EurekaLog1CustomWebFieldsRequest
    Left = 48
    Top = 40
  end
end
