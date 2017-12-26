object Form3: TForm3
  Left = 339
  Top = 245
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'EVOPLANT'
  ClientHeight = 320
  ClientWidth = 327
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Visible = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Imatge: TImage
    Left = 0
    Top = 0
    Width = 327
    Height = 299
    Align = alTop
    AutoSize = True
    Center = True
    OnClick = ImatgeClick
  end
  object BarraProgres: TProgressBar
    Left = 0
    Top = 299
    Width = 327
    Height = 21
    Align = alBottom
    Min = 1
    Max = 24
    Position = 1
    Smooth = True
    Step = 1
    TabOrder = 0
  end
  object Timer1: TTimer
    Interval = 333
    OnTimer = Timer1Timer
    Left = 216
    Top = 120
  end
end
