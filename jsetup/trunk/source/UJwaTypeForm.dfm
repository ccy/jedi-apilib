object JwaTypeForm: TJwaTypeForm
  Left = 0
  Top = 0
  Caption = 'JwaTypeForm'
  ClientHeight = 490
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 537
    Height = 193
    AutoSize = False
    Caption = 
      'This page shows you a list of selected Delphi versions that had ' +
      'selected to integrate the JEDI API.'#13#10'However there are two ways ' +
      'to integrate the JEDI API units.'#13#10#13#10'1. Use single units. It mean' +
      's that we will add the folder of the JwaXXX.pas units to your De' +
      'lphi'#39's'#13#10'source path so you can include these files directly. '#13#10'H' +
      'owever this approach is problematic when you also want to use JW' +
      'SCL units in your project. You cannot'#13#10'use both JwaXXXX units an' +
      'd JwaWindows in your uses clause. '#13#10'Also select this option if y' +
      'ou want to include JwaWindows.pas directly. This approach will t' +
      'ake its time every time'#13#10'you rebuild your project.'#13#10#13#10'2. Use Jwa' +
      'Windows.dcu. It means that we will compile all the single JwaXXX' +
      ' units into '#13#10'one file called JwaWindows.dcu that can be include' +
      'd into your projects instead of all the single'#13#10'JEDI API units. ' +
      'This is the prefered way when using JWSCL.'#13#10#13#10
  end
  object MainVirtualStringTree: TVirtualStringTree
    Left = 8
    Top = 207
    Width = 537
    Height = 258
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoVisible]
    TabOrder = 0
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    OnGetText = MainVirtualStringTreeGetText
    OnMeasureItem = MainVirtualStringTreeMeasureItem
    Columns = <
      item
        Position = 0
        Width = 533
      end>
  end
end
