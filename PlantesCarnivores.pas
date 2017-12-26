unit PlantesCarnivores;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, AtrapaMosques,
  StdCtrls, ExtCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    Panel2: TPanel;
    Panel3: TPanel;
    Imatge: TImage;
    Buida: TImage;
    BNovaProva: TButton;
    Label1: TLabel;
    num_generacio: TLabel;
    Shape1: TShape;
    Label2: TLabel;
    Label3: TLabel;
    punts_superior: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Image1: TImage;
    avis_evolucio: TLabel;
    Timer2: TTimer;
    punts_inferior: TLabel;
    Shape3: TShape;
    Shape4: TShape;
    Label6: TLabel;
    Label9: TLabel;
    cromosoma_superior: TImage;
    cromosoma_inferior: TImage;
    Image2: TImage;
    Shape5: TShape;
    Shape6: TShape;
    BTutorial: TButton;
    Velocitat: TTrackBar;
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure VelocitatChange(Sender: TObject);
    procedure IniciarEvolucio(num_plantes: Integer; longitud: Integer; num_mosques: Integer);
    procedure PintarCromosomes();
    procedure PintarCromosomaSuperior(c:TCromosoma);
    procedure PintarCromosomaInferior(c:TCromosoma);
    procedure ColorejarMillorPuntuacio();    
    procedure BNovaProvaClick(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure BTutorialClick(Sender: TObject);
  private
    { Private declarations }
  public
    g:TGrupMosques;
    p:TMapa;
    e:TPixel;
    l,l2:TPoblacio;
    numero_mosques, temps: Integer;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses EntradaDades, Tutorial;

{$R *.DFM}

procedure TForm1.IniciarEvolucio(num_plantes: Integer; longitud: Integer; num_mosques: Integer);
var
     separacio:Integer;
begin
     Randomize;
     temps:=0;
     separacio:=Imatge.Width div (num_plantes+1);
     l:=TPoblacio.create(Imatge.Width div (num_plantes+1),100,num_plantes,separacio,longitud);
     l2:=TPoblacio.create(Imatge.Width div (num_plantes+1),300,num_plantes,separacio,longitud);
     e:=l.ObtenirLlistaPixels();
     p:=TMapa.Create(Imatge.Width,Imatge.Height,imatge);
     p.CarregarPixels(e);
     e:=l2.ObtenirLlistaPixels();
     p.CarregarPixels(e);
     imatge.Picture:=buida.Picture;
     p.PintarMapa();
     g:=TGrupMosques.create(num_mosques,imatge.Width,imatge.Height,imatge,p);
     if Velocitat.Position=0 then Timer1.interval:=1
     else Timer1.Interval:=Velocitat.Position * 5;
     Timer1.Enabled:=true;
end;

procedure TForm1.PintarCromosomaSuperior(c:TCromosoma);
var
  e, x, y, a,i, color:Integer;
begin
  i:=255 div 15;
  a:=0;
  for e:=0 to 14 do
  begin
    color:= (c[e]*255) div 360;
    for x:=a to (a+i) do for y:=0 to 13 do cromosoma_superior.Picture.Bitmap.Canvas.Pixels[x,y]:=rgb(color,0,color);
    a:=a + i;
  end;
end;

procedure TForm1.PintarCromosomaInferior(c:TCromosoma);
var
  e, x, y, a,i, color:Integer;
begin
  i:=255 div 15;
  a:=0;
  for e:=0 to 14 do
  begin
    color:= (c[e]*255) div 360;
    for x:=a to (a+i) do for y:=0 to 13 do cromosoma_inferior.Picture.Bitmap.Canvas.Pixels[x,y]:=rgb(color,0,color);
    a:=a + i;
  end;
end;

procedure TForm1.ColorejarMillorPuntuacio();
var
  superior,inferior: Integer;
begin
  superior:=StrToInt(punts_superior.caption);
  inferior:=StrToInt(punts_inferior.caption);
  punts_superior.color:=clWhite;
  punts_inferior.color:=clWhite;

  if superior>inferior then punts_superior.color:=$00FFFFDC
  else if superior<inferior then punts_inferior.color:=$00FFFFDC;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  c:TCromosoma;
begin
  if temps>=4000 then
  begin
    avis_evolucio.Visible:=true;
    p.Free; //Es destrueix el mapa
    g.Free; //Es destrueix el conjunt de mosques
    p:=TMapa.Create(Imatge.Width,Imatge.Height,imatge); //Es crea un nou mapa buit
    punts_superior.caption:= IntToStr(l.Evolucionar); //Es fa EVOLUCIONAR el primer poblat
    c:=l.ObtenirMillorCromosoma; //S'obté el millor cromosoma per pintar-lo per pantalla
    PintarCromosomaSuperior(c);
    e:=l.ObtenirLlistaPixels(); //S'obté els píxels del poblat
    p.CarregarPixels(e); //Es carreguen al mapa
    punts_inferior.caption:= IntToStr(l2.Evolucionar); //Es fa EVOLUCIONAR el segon poblat
    c:=l2.ObtenirMillorCromosoma;
    PintarCromosomaInferior(c);
    e:=l2.ObtenirLlistaPixels();
    p.CarregarPixels(e);
    imatge.Picture:=buida.Picture; //S'esborra el canvas
    p.PintarMapa(); //Es pinta el mapa sobre el canvas
    g:=TGrupMosques.create(numero_mosques,imatge.Width,imatge.Height,imatge,p);
    g.IniciarVol; //S'inicia el vol del nou grup de mosques
    num_generacio.Caption:= IntToStr(StrToInt(num_generacio.Caption) + 1);
    temps:=0;
    ColorejarMillorPuntuacio;
    BNovaProva.enabled:=false;
    BTutorial.Enabled:=false;
    Timer1.Enabled:=false;
    Timer2.Enabled:=true;
  end
  else g.Volar;
  temps:=temps + 1;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin

if MessageDlg('Vols sortir realment de l''aplicació?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    Timer2.enabled:=false;
    Timer1.enabled:=false;
    p.Free;
    e.Free;
    l.free;
    g.free;
    Form2.Close;
    Application.terminate;
  end
  else Action := caNone;
end;

procedure TForm1.VelocitatChange(Sender: TObject);
begin
  if Velocitat.Position=0 then Timer1.interval:=1
  else Timer1.Interval:=Velocitat.Position * 5;
end;

procedure TForm1.BNovaProvaClick(Sender: TObject);
begin
  with Application do
  begin
    NormalizeTopMosts;

     if MessageBox('Finalitzarà el cicle en curs. Vols continuar?','Avís', MB_YESNO) = IDYES then
        begin
             Timer2.enabled:=false;
             Timer1.enabled:=false;
             p.Free; //Es destrueix el mapa
             g.Free; //Es destrueix el conjunt de mosques
             Form1.Visible:=false;
             Form2.Visible:=true;
        end;
    RestoreTopMosts;
  end;
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
    avis_evolucio.Visible:=false;
    Timer2.Enabled:=false;
    Timer1.Enabled:=true;
    BNovaProva.enabled:=true;
    BTutorial.Enabled:=true;        
end;

procedure TForm1.PintarCromosomes();
var
    c:TCromosoma;
begin
    c:=l.ObtenirMillorCromosoma;
    PintarCromosomaSuperior(c);
    c:=l2.ObtenirMillorCromosoma;
    PintarCromosomaInferior(c);
end;

procedure TForm1.BTutorialClick(Sender: TObject);
begin
Application.CreateForm(TForm4, Form4);
form4.showmodal;
end;

end.
