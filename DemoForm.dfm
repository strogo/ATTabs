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
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object bAdd: TButton
    Left = 160
    Top = 192
    Width = 115
    Height = 25
    Caption = 'add after current'
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
  object bColor: TButton
    Left = 360
    Top = 192
    Width = 75
    Height = 25
    Caption = 'color'
    TabOrder = 2
    OnClick = bColorClick
  end
  object bLeft: TButton
    Left = 288
    Top = 224
    Width = 75
    Height = 25
    Caption = '<'
    TabOrder = 3
    OnClick = bLeftClick
  end
  object bRt: TButton
    Left = 368
    Top = 224
    Width = 75
    Height = 25
    Caption = '>'
    TabOrder = 4
    OnClick = bRtClick
  end
  object Edit1: TEdit
    Left = 288
    Top = 256
    Width = 225
    Height = 21
    TabOrder = 5
    OnChange = Edit1Change
  end
  object chkX: TCheckBox
    Left = 200
    Top = 152
    Width = 97
    Height = 17
    Caption = '"x" buttons'
    TabOrder = 6
    OnClick = chkXClick
  end
  object chkPlus: TCheckBox
    Left = 200
    Top = 168
    Width = 97
    Height = 17
    Caption = '"+" tab'
    TabOrder = 7
    OnClick = chkPlusClick
  end
end
