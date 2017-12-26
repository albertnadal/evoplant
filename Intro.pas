unit Intro;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, jpeg;

type
  TForm3 = class(TForm)
    Imatge: TImage;
    Timer1: TTimer;
    BarraProgres: TProgressBar;
    procedure ImatgeClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Continuar();
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    jp: TJPEGImage;
  end;

var
  Form3: TForm3;

implementation

uses EntradaDades;

{$R *.dfm}

procedure TForm3.Continuar();
begin
  Timer1.Enabled:=false;
  BarraProgres.Free;
  Imatge.Picture.Bitmap.FreeImage;
  Imatge.Free;
  jp.free;
  form3.Visible:=false;
  form2.visible:=true;
end;

procedure TForm3.ImatgeClick(Sender: TObject);
begin
  Continuar;
end;

procedure TForm3.Timer1Timer(Sender: TObject);
begin
if BarraProgres.Position>=24 then Continuar
else BarraProgres.StepIt;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  jp := TJPEGImage.Create;
  jp.LoadFromFile('diapo1.jpg');
  Imatge.Picture.Bitmap.Assign(jp);
end;

end.
