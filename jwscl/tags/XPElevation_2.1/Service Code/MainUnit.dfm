object XPService: TXPService
  OldCreateOrder = False
  OnCreate = ServiceCreate
  DisplayName = 'XP Elevation Service'
  StartType = stManual
  OnExecute = ServiceExecute
  OnShutdown = ServiceShutdown
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
  object EurekaLog1: TEurekaLog
    Left = 88
    Top = 56
  end
end
