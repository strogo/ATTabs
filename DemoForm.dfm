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
  object Label1: TLabel
    Left = 144
    Top = 240
    Width = 261
    Height = 13
    Caption = 'Note: enabled drag-drop from top tabs to bottom tabs'
  end
  object labStatus: TLabel
    Left = 144
    Top = 256
    Width = 35
    Height = 13
    Caption = 'Status:'
  end
  object bAdd: TButton
    Left = 144
    Top = 144
    Width = 115
    Height = 25
    Caption = 'add after current'
    TabOrder = 0
    OnClick = bAddClick
  end
  object bDel: TButton
    Left = 264
    Top = 144
    Width = 75
    Height = 25
    Caption = 'del'
    TabOrder = 1
    OnClick = bDelClick
  end
  object bColor: TButton
    Left = 344
    Top = 144
    Width = 75
    Height = 25
    Caption = 'color'
    TabOrder = 2
    OnClick = bColorClick
  end
  object bLeft: TButton
    Left = 264
    Top = 176
    Width = 75
    Height = 25
    Caption = '<'
    TabOrder = 3
    OnClick = bLeftClick
  end
  object bRt: TButton
    Left = 344
    Top = 176
    Width = 75
    Height = 25
    Caption = '>'
    TabOrder = 4
    OnClick = bRtClick
  end
  object Edit1: TEdit
    Left = 264
    Top = 208
    Width = 225
    Height = 21
    TabOrder = 5
    OnChange = Edit1Change
  end
  object chkX: TCheckBox
    Left = 144
    Top = 88
    Width = 161
    Height = 17
    Caption = '"x" buttons'
    TabOrder = 6
    OnClick = chkXClick
  end
  object chkPlus: TCheckBox
    Left = 144
    Top = 104
    Width = 169
    Height = 17
    Caption = '"+" tab'
    TabOrder = 7
    OnClick = chkPlusClick
  end
  object chkNums: TCheckBox
    Left = 144
    Top = 120
    Width = 185
    Height = 17
    Caption = 'numbers on bottom'
    TabOrder = 8
    OnClick = chkNumsClick
  end
end
