unit Tutorial;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, jpeg, StdCtrls;

type
  TForm4 = class(TForm)
    Panel1: TPanel;
    Imatge: TImage;
    BSeguent: TSpeedButton;
    BAnterior: TSpeedButton;
    SpeedButton3: TSpeedButton;
    LContador: TLabel;
    Label1: TLabel;
    procedure SpeedButton3Click(Sender: TObject);
    procedure BSeguentClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BAnteriorClick(Sender: TObject);
    procedure CarregarDiapositiva;
    procedure CarregarImatge(fitxer: String);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    jp: TJPEGImage;
    c: Integer;
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.SpeedButton3Click(Sender: TObject);
begin
Form4.close;
end;

procedure TForm4.BSeguentClick(Sender: TObject);
begin
  if c<6 then
  begin
    inc(c,1);
    LContador.Caption:=IntToStr(c);
    CarregarDiapositiva;
    BAnterior.Enabled:=true;
    if c=6 then BSeguent.Enabled:=false;
  end
  else BSeguent.Enabled:=false;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
c:=0;
jp := TJPEGImage.Create;
CarregarDiapositiva;
end;

procedure TForm4.CarregarImatge(fitxer: String);
begin
  jp.LoadFromFile(fitxer);
  Imatge.Picture.Bitmap.Assign(jp);
end;

procedure TForm4.CarregarDiapositiva;
var
  fitxer: String;
begin
    case c of
      1: fitxer:='diapo2.jpg';
      2: fitxer:='diapo3.jpg';
      3: fitxer:='diapo4.jpg';
      4: fitxer:='diapo5.jpg';
      5: fitxer:='diapo6.jpg';
      6: fitxer:='diapo7.jpg';            
    else
      fitxer:='diapo1.jpg';
    end;
    CarregarImatge(fitxer);
end;

procedure TForm4.BAnteriorClick(Sender: TObject);
begin
  if c>0 then
  begin
    dec(c,1);
    LContador.Caption:=IntToStr(c);
    CarregarDiapositiva;
    BSeguent.enabled:=true;
    if c=0 then BAnterior.enabled:=false;
  end
  else BAnterior.enabled:=false;
end;

procedure TForm4.FormClose(Sender: TObject; var Action: TCloseAction);
begin
jp.free;
Imatge.Picture.Bitmap.FreeImage;
Imatge.Free;
end;

end.
