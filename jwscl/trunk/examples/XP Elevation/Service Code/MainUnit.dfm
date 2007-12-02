object Service1: TService1
  OldCreateOrder = False
  DisplayName = 'XP Elevation Service'
  StartType = stManual
  OnExecute = ServiceExecute
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
