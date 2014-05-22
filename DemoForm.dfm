object Form1: TForm1
  Left = 303
  Top = 282
  AutoScroll = False
  Caption = 'ATTabs demo'
  ClientHeight = 381
  ClientWidth = 716
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object bAdd: TButton
    Left = 200
    Top = 192
    Width = 75
    Height = 25
    Caption = 'add'
    TabOrder = 0
    OnClick = bAddClick
  end
  object bDel: TButton
    Left = 280
    Top = 192
    Width = 75
    Height = 25
    Caption = 'del'
    TabOrder = 1
    OnClick = bDelClick
  end
  object bCl: TButton
    Left = 360
    Top = 192
    Width = 75
    Height = 25
    Caption = 'color'
    TabOrder = 2
    OnClick = bClClick
  end
  object Button4: TButton
    Left = 336
    Top = 224
    Width = 75
    Height = 25
    Caption = '<'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 416
    Top = 224
    Width = 75
    Height = 25
    Caption = '>'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Edit1: TEdit
    Left = 336
    Top = 256
    Width = 145
    Height = 21
    TabOrder = 5
    OnChange = Edit1Change
  end
end
