object Form2: TForm2
  Left = 311
  Top = 293
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'EVOPLANT'
  ClientHeight = 159
  ClientWidth = 410
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 117
    Height = 16
    Caption = 'N'#250'mero de plantes:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 141
    Height = 16
    Caption = 'Longitud de les plantes:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 80
    Width = 115
    Height = 16
    Caption = 'N'#250'mero d'#39'ins'#232'ctes:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Image2: TImage
    Left = 232
    Top = 16
    Width = 2
    Height = 50
    Picture.Data = {
      07544269746D6170FE000000424DFE0000000000000036000000280000000200
      0000190000000100180000000000C80000000000000000000000000000000000
      0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF
      0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF
      0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF
      0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF
      0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF
      0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF
      0000808080FFFFFF0000}
    Stretch = True
  end
  object Image1: TImage
    Left = 232
    Top = 64
    Width = 2
    Height = 42
    Picture.Data = {
      07544269746D6170FE000000424DFE0000000000000036000000280000000200
      0000190000000100180000000000C80000000000000000000000000000000000
      0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF
      0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF
      0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF
      0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF
      0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF
      0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF
      0000808080FFFFFF0000}
    Stretch = True
  end
  object Image3: TImage
    Left = 232
    Top = 104
    Width = 2
    Height = 49
    Picture.Data = {
      07544269746D6170FE000000424DFE0000000000000036000000280000000200
      0000190000000100180000000000C80000000000000000000000000000000000
      0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF
      0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF
      0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF
      0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF
      0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF
      0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF0000808080FFFFFF
      0000808080FFFFFF0000}
    Stretch = True
  end
  object LNumeroPlantes: TEdit
    Left = 152
    Top = 16
    Width = 57
    Height = 21
    TabOrder = 0
    Text = '5'
  end
  object LLongitud: TEdit
    Left = 152
    Top = 48
    Width = 57
    Height = 21
    TabOrder = 1
    Text = '200'
  end
  object LNumMosques: TEdit
    Left = 152
    Top = 80
    Width = 57
    Height = 21
    TabOrder = 2
    Text = '600'
  end
  object Button1: TButton
    Left = 16
    Top = 112
    Width = 193
    Height = 41
    Caption = 'Iniciar evoluci'#243
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = Button1Click
  end
  object GroupBox1: TGroupBox
    Left = 248
    Top = 8
    Width = 153
    Height = 145
    Caption = 'Configuracions est'#224'ndars'
    TabOrder = 4
    object opcio1: TRadioButton
      Left = 16
      Top = 24
      Width = 110
      Height = 17
      Caption = 'Poblats molt grans'
      TabOrder = 0
      OnClick = opcio1Click
    end
    object opcio2: TRadioButton
      Left = 16
      Top = 56
      Width = 110
      Height = 17
      Caption = 'Poblats grans'
      TabOrder = 1
      OnClick = opcio2Click
    end
    object opcio3: TRadioButton
      Left = 16
      Top = 88
      Width = 110
      Height = 17
      Caption = 'Poblats normals'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = opcio3Click
    end
    object opcio4: TRadioButton
      Left = 16
      Top = 120
      Width = 110
      Height = 17
      Caption = 'Poblats petits'
      TabOrder = 3
      OnClick = opcio4Click
    end
  end
end
