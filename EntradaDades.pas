unit EntradaDades;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TForm2 = class(TForm)
    LNumeroPlantes: TEdit;
    LLongitud: TEdit;
    LNumMosques: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    Image2: TImage;
    Image1: TImage;
    Image3: TImage;
    GroupBox1: TGroupBox;
    opcio1: TRadioButton;
    opcio2: TRadioButton;
    opcio3: TRadioButton;
    opcio4: TRadioButton;
    procedure Button1Click(Sender: TObject);
    procedure opcio3Click(Sender: TObject);
    procedure opcio2Click(Sender: TObject);
    procedure opcio1Click(Sender: TObject);
    procedure opcio4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses PlantesCarnivores;

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
Form2.Visible:=false;
Form1.Visible:=true;
Form1.numero_mosques:=StrToInt(LNumMosques.text);
Form1.num_generacio.Caption:='0';
Form1.punts_superior.Caption:='0';
Form1.punts_inferior.Caption:='0';
Form1.ColorejarMillorPuntuacio; //En aquest punt les posa totes dos a blanc, ja que les dos valen zero
Form1.IniciarEvolucio(StrToInt(LNumeroPlantes.text),StrToInt(LLongitud.text),StrToInt(LNumMosques.text));
Form1.PintarCromosomes;
end;

procedure TForm2.opcio3Click(Sender: TObject);
begin
  LNumeroPlantes.Text:='5';
  LLongitud.Text:='200';
  LNumMosques.Text:='600';
end;

procedure TForm2.opcio2Click(Sender: TObject);
begin
  LNumeroPlantes.Text:='6';
  LLongitud.Text:='160';
  LNumMosques.Text:='600';
end;

procedure TForm2.opcio1Click(Sender: TObject);
begin
  LNumeroPlantes.Text:='10';
  LLongitud.Text:='100';
  LNumMosques.Text:='650';
end;

procedure TForm2.opcio4Click(Sender: TObject);
begin
  LNumeroPlantes.Text:='3';
  LLongitud.Text:='200';
  LNumMosques.Text:='500';
end;

end.
