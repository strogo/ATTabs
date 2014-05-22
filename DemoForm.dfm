object Form1: TForm1
  Left = 303
  Top = 282
  Width = 732
  Height = 419
  Caption = 'FormDemo'
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
  object Button1: TButton
    Left = 200
    Top = 192
    Width = 75
    Height = 25
    Caption = 'add'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 280
    Top = 192
    Width = 75
    Height = 25
    Caption = 'del'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 360
    Top = 192
    Width = 75
    Height = 25
    Caption = 'color'
    TabOrder = 2
    OnClick = Button3Click
  end
end
