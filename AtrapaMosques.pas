unit AtrapaMosques;

interface

uses Windows, ExtCtrls, Classes, SysUtils, Math;

type TCromosoma = array[0..14] of Integer;
type
          TPlanta = class;
          TMapa = class;
          TPixel = class;
          TMosca = class;
          TPoblacio = class;
          TGrupMosques = class;

{ ********************************************************************* }
{ ****************************** TPixel ******************************* }
          TPixel = class(TObject)
          public
                coord_x: Integer;   //Coordenada X del píxel
                coord_y: Integer;   //Coordenada Y del píxel
                ultim: Boolean;     //Indica si és l'últim de la llista
                seguent: TPixel;    //Punter al següent píxel pintat en la mateixa coordenada X
                angle: Integer;     //Per saber la pendent del píxel
                planta: TPlanta;    //Planta a la que correspon
                constructor create(x, y: integer; u: Boolean; s: TPixel; a: Integer; p:TPlanta);
          end;

{ ********************************************************************* }
{ ******************************** TMapa ****************************** }

          TMapa = class(TObject)
          private
                  ample: Integer;
                  llarg: Integer;
                  vector: array of TPixel;
                  imatge: TImage;
                  procedure MapejarPixel(p: TPixel);
          public
                  constructor create(a, l: Integer; i: TImage);
                  procedure RestaurarMapa();
                  destructor destroy;
                  procedure CarregarPixels(p: TPixel);
                  procedure ObtenirDadesPixel(x,y: Integer; out angle: Integer; out pl:TPlanta; out trobat: Boolean);
                  procedure PintarMapa();
          end;

{ ********************************************************************* }
{ ******************************* TPlanta ***************************** }

          TPlanta = class(TObject)
          private
                  pos_x: Integer; //És la coordenada X base de la planta
                  pos_y: Integer; //És la coordenada Y base de la planta
                  cromosoma: TCromosoma; //Conté els graus de cada vèrtex
                  pixels: TPixel;
                  longitud: Integer; //Nombre de píxels entre vèrtexs
                  fitness: Integer;  //Puntuació de la planta
                  //procedure EliminarLlistaPixels();
          public
                  constructor create(c: TCromosoma; x,y,l:Integer);
                  function ObtenirCreuament(p:TPlanta): TCromosoma;
                  function ObtenirCromosoma(): TCromosoma;
                  procedure Mutar();
                  procedure AssignarNovaPosicio(x,y:Integer);                  
                  procedure NotificarMoscaAtrapada();
                  procedure NotificarMoscaQuasiAtrapada();
                  function ObtenirFitness(f: Integer): Integer;
                  destructor destroy;
                  function ObtenirLlistaPixels(): TPixel;
          end;

{ ********************************************************************* }
{ ****************************** TPoblacio **************************** }

          TPoblacio = class(TObject)
          private
                  pos_x: Integer; //És la coordenada X base de la població
                  pos_y: Integer; //És la coordenada Y base de la població
                  longitud: Integer; //Longitud de les plantes
                  num_plantes: Integer; //Número de plantes del poblat
                  separacio: Integer; //Separació entre les plantes
                  plantes: Array of TPlanta;
                  procedure OrdenarPoblacio();
                  procedure EliminarExemplarsMesFebles();
                  procedure CrearNousExemplarsFentCreuaments();
                  procedure MutarElsNousExemplars();
                  procedure ResetejarElsFitness();                  
          public
                  function Evolucionar(): Integer;
                  function ObtenirMillorCromosoma(): TCromosoma;
                  constructor create(x,y,n,s,l:Integer);
                  function ObtenirLlistaPixels(): TPixel;
                  destructor destroy;
          end;

{ ********************************************************************* }
{ ******************************** TMosca ***************************** }

          TMosca = class(TObject)
          private
                  ample: Integer;
                  llarg: Integer;
                  base_x: Integer;
                  base_y: Integer;
                  pos_x: Integer;
                  pos_y: Integer;
                  x_inc: Integer;
                  y_inc: Integer;
                  i: Integer;
                  j: Integer;
                  angle: Integer; //Direcció del vol
                  tangent: Extended;
                  imatge: TImage;
                  mapa: TMapa;
                  planta: TPlanta;
                  rebots: Integer;
                  PresicioEixY: Boolean;
                  procedure ConfigurarMosca(x,y,a: Integer);
                  procedure IncrementarPosicio();
                  procedure ComprovarColisioEnDiagonal(inc_hor, inc_ver: Integer; out angle_pixel: Integer; out hi_ha_colisio: Boolean);
                  procedure EnregistrarColisio(p: TPlanta);
                  function ObtenirAngleDesviacio(a: Integer): Integer;
          public
                  constructor create(x,y,a,amp,lla:Integer; i: TImage; m: TMapa);
                  procedure Centralitzar();
                  procedure Volar();
          end;

{ ********************************************************************* }
{ ******************************** TGrupMosques ***************************** }

          TGrupMosques = class(TObject)
          private
                  quantitat: Integer;
                  index_inici_vol: Integer;
                  estat_inici_vol: Boolean;
                  mosques: Array of TMosca;
                  procedure CentralitzarMosques();
          public
                  constructor create(n,amp,lla:Integer; i: TImage; m: TMapa);
                  procedure IniciarVol();
                  destructor destroy;
                  procedure Volar();
          end;

{ ******************************************************* }
{ ******************** IMPLEMENTACIÓ ******************** }
{ ******************************************************* }
implementation

{ ********************* TGrupMosques ********************************** }
constructor TGrupMosques.create(n,amp,lla:Integer; i: TImage; m: TMapa);
var
  t: Integer;
begin
  index_inici_vol:=0;
  estat_inici_vol:=false;
  quantitat:=n;
  SetLength(mosques,quantitat);
  for t:=0 to quantitat-1 do mosques[t]:=TMosca.create(random(amp), random(lla),random(360),amp,lla,i,m);
  IniciarVol;
end;

destructor TGrupMosques.destroy();
var
  t: Integer;
begin
  for t:=0 to quantitat-1 do mosques[t].free;
end;

procedure TGrupMosques.IniciarVol;
begin
  CentralitzarMosques();
  estat_inici_vol:=true;
  index_inici_vol:=0;
end;

procedure TGrupMosques.CentralitzarMosques();
var
  t: Integer;
begin
  for t:=0 to quantitat-1 do mosques[t].Centralitzar;
end;

procedure TGrupMosques.Volar();
var
  e: Integer;
begin
  if not estat_inici_vol then for e:=0 to quantitat-1 do mosques[e].Volar()
  else
  begin
    for e:=0 to index_inici_vol do mosques[e].Volar();
    index_inici_vol:=index_inici_vol + 1;
    if index_inici_vol = quantitat then
    begin
      index_inici_vol:=0;
      estat_inici_vol:=false;
    end;
  end;
end;

{ ************************** TMosca ***************************** }
constructor TMosca.create(x,y,a,amp,lla:Integer; i: TImage; m: TMapa);
begin
  ample:=amp;
  llarg:=lla;
  imatge:=i;
  mapa:=m;
  planta:=nil;
  rebots:=0;
  ConfigurarMosca(x,y,a);
end;

procedure TMosca.Centralitzar;
begin
  imatge.Canvas.Pixels[pos_x,pos_y]:=rgb(255,255,255); //Borra el píxel
  ConfigurarMosca((ample-2) div 2,(llarg-2) div 2,random(360)); //Es situa al centre del mapa
end;

function TMosca.ObtenirAngleDesviacio(a: Integer): Integer;
var
  aux: Integer;
begin
  aux:=a + (a - angle);
  if aux <0 then aux:= 360 + aux;
  ObtenirAngleDesviacio:= (aux mod 360);
end;

procedure TMosca.ConfigurarMosca(x,y,a: Integer);
begin
  base_x:=x;
  base_y:=y;
  pos_x:=x;
  pos_y:=y;
  i:=0;
  j:=0;
  angle:=a;
  tangent:= Tan(angle*Pi/180);
  if((angle>90) and (angle<270)) then x_inc:=-1 else x_inc:=1;
  if angle<180 then y_inc:=-1 else y_inc:=1;
  PresicioEixY := (((angle>45)and(angle<135)) or ((angle>225)and(angle<315)));
end;

procedure TMosca.IncrementarPosicio();
begin
  if PresicioEixY then
  begin
    j:=j + y_inc;
    if tangent<>0 then i:=-(variant(SimpleRoundTo(j/tangent,0)));
  end
  else
  begin
    i:=i + x_inc;
    j:= -(Variant(SimpleRoundTo(tangent*i,0)));
  end;
end;

procedure TMosca.EnregistrarColisio(p: TPlanta);
begin
  if planta=p then
  begin
    rebots:=rebots+1;
    if rebots>5 then
    begin
      p.NotificarMoscaAtrapada(); //Es notifica a la planta que ha caçat la mosca
      rebots:=0;
      imatge.Canvas.Pixels[pos_x,pos_y]:=rgb(255,255,255); //Borra el píxel
      ConfigurarMosca(random(ample-2)+1,1,random(360)); //La mosca mor i apareix per un altre lloc
    end;
  end
  else
  begin
    if rebots>3 then planta.NotificarMoscaQuasiAtrapada(); //Es notifica a la planta que quasi caça la mosca
    planta:=p;
    rebots:=1;
  end;
end;

procedure TMosca.ComprovarColisioEnDiagonal(inc_hor, inc_ver: Integer; out angle_pixel: Integer; out hi_ha_colisio: Boolean);
var
    colisio1, colisio2: Boolean;
    pl: TPlanta;
begin
    colisio2:=false;
    mapa.ObtenirDadesPixel(pos_x+inc_hor,pos_y, angle_pixel,pl,colisio1);
    if colisio1 then mapa.ObtenirDadesPixel(pos_x,pos_y+inc_ver, angle_pixel,pl,colisio2);
    hi_ha_colisio:=colisio1 and colisio2;
    if hi_ha_colisio then
    begin
      EnregistrarColisio(pl);
      angle:=ObtenirAngleDesviacio(angle_pixel);     //Calcula la desviació de la mosca deguda a l'impacte
      ConfigurarMosca(pos_x,pos_y,angle);  //Configura la mosca amb la nova trajectòria
    end;
end;

procedure TMosca.Volar();
var
  contador, inc_ver, inc_hor, angle_pixel: Integer;
  hi_ha_colisio: Boolean;
  pl: TPlanta;
begin
  hi_ha_colisio:=false;
  contador:=0;
  IncrementarPosicio();
  imatge.Canvas.Pixels[pos_x,pos_y]:=rgb(255,255,255); //Borra el píxel
  inc_hor:= base_x + i - pos_x;
  inc_ver:= base_y + j - pos_y;
  if ((inc_hor<>0)and(inc_ver<>0)) then ComprovarColisioEnDiagonal(inc_hor, inc_ver, angle_pixel, hi_ha_colisio);
  if not hi_ha_colisio then
  begin
    mapa.ObtenirDadesPixel(pos_x+inc_hor,pos_y+inc_ver, angle_pixel,pl,hi_ha_colisio); //Comprova si hi ha colisió amb alguna planta
    while hi_ha_colisio do
    begin
      EnregistrarColisio(pl);
      if contador>2 then angle:=random(360)
      else angle:=ObtenirAngleDesviacio(angle_pixel);     //Calcula la desviació de la mosca deguda a l'impacte
      ConfigurarMosca(pos_x,pos_y,angle);  //Configura la mosca amb la nova trajectòria
      IncrementarPosicio();
      inc_hor:= base_x + i - pos_x;
      inc_ver:= base_y + j - pos_y;
      mapa.ObtenirDadesPixel(pos_x+inc_hor,pos_y+inc_ver, angle_pixel,pl,hi_ha_colisio); //Comprova si hi ha colisió amb alguna planta
      if ((inc_hor<>0)and(inc_ver<>0)and(not hi_ha_colisio)) then ComprovarColisioEnDiagonal(inc_hor, inc_ver, angle_pixel, hi_ha_colisio);
      contador:=contador + 1;
    end;
    pos_x:=base_x + i;
    pos_y:=base_y + j;
  end;
  if pos_x>=ample-1 then ConfigurarMosca(1,pos_y,angle)
  else if pos_x<=0 then ConfigurarMosca(ample-2,pos_y,angle)
  else if pos_y>=llarg-1 then ConfigurarMosca(pos_x,1,angle)
  else if pos_y<=0 then ConfigurarMosca(pos_x,llarg-2,angle);
  imatge.Canvas.Pixels[pos_x,pos_y]:=rgb(0,0,0); //Pinta el píxel
end;

{ ************************* TPlanta ****************************** }
destructor TPlanta.destroy;
begin
  inherited Destroy;
end;

procedure TPlanta.NotificarMoscaAtrapada();
begin
  fitness:=fitness+3;
end;

procedure TPlanta.NotificarMoscaQuasiAtrapada();
begin
  fitness:=fitness+1;
end;

function TPlanta.ObtenirFitness(f: Integer): Integer;
var
  aux: Integer;
begin
  aux:=fitness;
  if f>=0 then fitness:=f;
  ObtenirFitness:=aux;
end;

procedure TPlanta.Mutar();
var
  i,j,v,z:Integer;
begin
  v:=random(3);
  for z:=0 to v do
  begin
    i:=random(15);
    j:=cromosoma[i];
    if random(2)=0 then cromosoma[i]:=(j + 40) mod 360
    else
    begin
      if j-40 < 0 then cromosoma[i]:=360 - j - 40
      else cromosoma[i]:= j - 40;
    end;
  end;    
end;

{procedure TPlanta.EliminarLlistaPixels();
var
  aux, t:TPixel;
begin
    if pixels <> nil then
    begin
      t:=pixels;
      while not t.ultim do
      begin
        aux:=t;
        t:=t.seguent;
        aux.Free;
      end;
      t.free;
      pixels:=nil;
    end;
end;}

function TPlanta.ObtenirLlistaPixels(): TPixel;
var
  c,i,j,x_inc, y_inc, angle, e, x, y: Integer;
  radiants, tangent: Extended;
  p, ant: TPixel;
begin
  x:=pos_x;
  y:=pos_y;
  pixels:=nil;
  
  for e:=0 to 14 do
  begin
    angle:=cromosoma[e] mod 360; //S'Ajusta l'angle a un quadrant de 360º
    radiants:=(angle*Pi)/180;    //Conversió a radiants

    if((angle>90) and (angle<270)) then x_inc:=-1
    else x_inc:=1;

    if angle<180 then y_inc:=-1
    else y_inc:=1;

    tangent:= Tan(radiants);
    p:= TPixel.create(x,y,false,nil,angle,Self);
    if pixels=nil then pixels:=p
    else ant.seguent:=p;
    ant:=p;
    i:=0;
    j:=0;
    if(((angle>45)and(angle<135)) or ((angle>225)and(angle<315))) then
    begin
      for c:=0 to longitud-1 do
      begin
        j:=j + y_inc;
        if tangent<>0 then i:=-(variant(SimpleRoundTo(j/tangent,0)));
        p:= TPixel.create(x+i,y+j,false,nil,angle,Self);
        ant.seguent:=p;
        ant:=p;
      end;
        j:=j + y_inc;
        y:=y + j;
        if tangent<>0 then i:=-(variant(SimpleRoundTo(j/tangent,0)));
        x:=x + i;
    end
    else
    begin
      for c:=0 to longitud-1 do
      begin
        i:=i + x_inc;
        j:= -(Variant(SimpleRoundTo(tangent*i,0)));
        p:= TPixel.create(x+i,y+j,false,nil,angle,Self);
        ant.seguent:=p;
        ant:=p;
      end;
        i:=i + x_inc;
        x:=x + i;
        j:= -(Variant(SimpleRoundTo(tangent*i,0)));
        y:=y + j;
    end;
  end;
  ant.ultim:=true;
  ObtenirLlistaPixels:=pixels;  
end;

function TPlanta.ObtenirCromosoma(): TCromosoma;
begin
  ObtenirCromosoma:=cromosoma;
end;

function TPlanta.ObtenirCreuament(p:TPlanta): TCromosoma;
var
  c,cromosoma2:TCromosoma;
  i:Integer;
begin
  cromosoma2:=p.ObtenirCromosoma();
  for i:=0 to 14 do c[i]:=(cromosoma[i] + cromosoma2[i]) div 2; //Creua els cromosomes(Fa la mitja dels corresponents angles)
  ObtenirCreuament:=c;  
end;

procedure TPlanta.AssignarNovaPosicio(x,y:Integer);
begin
  pos_x:=x;
  pos_y:=y;
end;

constructor TPlanta.create(c: TCromosoma; x,y,l:Integer);
begin
  pos_x:=x;
  pos_y:=y;
  cromosoma:=c;
  pixels:=nil;
  fitness:=0;
  longitud:=l div 15;
end;

{ ************************ TPoblacio ******************************* }
function TPoblacio.ObtenirMillorCromosoma(): TCromosoma;
var
  c:TCromosoma;
begin
  c:= plantes[0].ObtenirCromosoma; //Retorna el cromosoma la primera planta, es a dir, la millor de totes de les del poblat
  ObtenirMillorCromosoma:=c;
end;

destructor TPoblacio.destroy();
var
  j:Integer;
begin
  for j := 0 to num_plantes-1 do plantes[j].free;
end;

function TPoblacio.ObtenirLlistaPixels(): TPixel;
var
  j:Integer;
  primer,p,aux:TPixel;
begin
  primer:=nil;
  for j := 0 to num_plantes-1 do
  begin
    p:=plantes[j].ObtenirLlistaPixels();
    if primer=nil then primer:=p
    else begin aux.ultim:=false; aux.seguent:=p; end;
    while not p.ultim do
    begin
      p:=p.seguent;
    end;
    aux:=p;
  end;
  p.ultim:=true;
  ObtenirLlistaPixels:=primer;
end;

procedure TPoblacio.OrdenarPoblacio();
var
  valor,v,j,i:Integer;
  aux:TPlanta;
begin
  valor:=0;
  for j := 0 to num_plantes-1 do
  begin
    for i := j to num_plantes-1 do
    begin
      v:=plantes[i].ObtenirFitness(-1);
      if v > valor then
      begin
        aux:=plantes[j];
        plantes[j]:=plantes[i];
        plantes[i]:=aux;
        valor:=v;
      end;
    end;
  end;

  for j := 0 to num_plantes-1 do plantes[j].AssignarNovaPosicio(pos_x + separacio*j, pos_y);
end;

procedure TPoblacio.EliminarExemplarsMesFebles();
var
  i:Integer;
begin
  //Abans d'aplicar aquest mètode s'ha d'ordenar la població!!!
  //La ordenació es fa de millor a pitjor (com mes alt és el fitness millor és l'exemplar)
  //Un cop ordenada la població s'elimina la meitat d'aquesta
  for i := (num_plantes-1)div 2 to num_plantes-1 do plantes[i].destroy;
  //Ara s'han de fer els creuaments i omplir la població eliminada amb aquests
end;

procedure TPoblacio.CrearNousExemplarsFentCreuaments();
var
  i,j:Integer;
  c:TCromosoma;
begin
  j:=0;
  for i := (num_plantes-1)div 2 to num_plantes-1 do
  begin
    c:=plantes[j].ObtenirCreuament(plantes[j+1]);
    plantes[i]:=TPlanta.Create(c,pos_x + separacio*i, pos_y,longitud);
    j:=j+1;
  end;
end;

procedure TPoblacio.MutarElsNousExemplars();
var
  i:Integer;
begin
  for i := (num_plantes-1)div 2 to num_plantes-1 do if random(3)=0 then plantes[i].Mutar;
end;

procedure TPoblacio.ResetejarElsFitness();
var
  i:Integer;
begin
  for i := 0 to num_plantes-1 do plantes[i].ObtenirFitness(0); //Posa el fitness de l'exemplar a zero
end;

function TPoblacio.Evolucionar(): integer;
var
  puntuacio,i:Integer;
begin
  puntuacio:=0;

  // A continuació és mostra l'esquema bàsic d'un algorisme genètic
  OrdenarPoblacio();  //S'ordena la població a partir del fitness de cada exemplar
  EliminarExemplarsMesFebles(); //S'eliminen els més febles(dolents)
  CrearNousExemplarsFentCreuaments(); //Es creuen els millors per a obtenir nous exemplars hipotèticament encara millors
  MutarElsNousExemplars(); //S'aplica, amb una certa probabilitat, una mutació als nous exemplars creats
  for i := 0 to num_plantes-1 do puntuacio:=puntuacio + plantes[i].ObtenirFitness(-1); //Calcula la puntuació total de la població per a actualitzar els indicadors de puntuació
  ResetejarElsFitness(); //Es tornen a posar els fitness a zero per tornar a evaluar els exemplars en la nova tanda (generació)

  Evolucionar:=puntuacio;  
end;

constructor TPoblacio.create(x,y,n,s,l:Integer);
var
  j,i:Integer;
  c:TCromosoma;
begin
  pos_x:=x;
  pos_y:=y;
  num_plantes:=n;
  separacio:=s;
  longitud:=l;
  SetLength(plantes,n);
  for j := 0 to num_plantes-1 do
  begin
    for i:=0 to 14 do c[i]:=random(360); //Crea un cromosoma a l'atzar
    plantes[j]:= TPlanta.Create(c,pos_x + separacio*j, pos_y,longitud);
  end;
end;

{ ************************* TPixel ****************************** }
constructor TPixel.create(x, y: integer; u: Boolean; s: TPixel; a: Integer; p: TPlanta);
begin
  ultim:=u;
  coord_x:=x;
  coord_y:=y;
  angle:=a;
  planta:=p;
  if not ultim then seguent:=s;
end;

{ ***************************** TMapa ************************** }
constructor TMapa.create(a, l: Integer; i: TImage);
var
   j: Integer;
begin
     ample:=a;
     llarg:=l;
     imatge:=i;
     SetLength(vector, a);
     for j := 0 to (ample-1) do vector[j]:= TPixel.Create(-1, -1,true,nil,0,nil);
end;

procedure TMapa.ObtenirDadesPixel(x,y: Integer; out angle: Integer; out pl:TPlanta; out trobat: Boolean);
var
  p: TPixel;
  final: Boolean;
begin
  trobat:=false;
  final:=false;

  if ((x<ample)and(x>=0)) then
  begin
    p:=vector[x];
    while ((not trobat) and (not final)) do
    begin
      if p.coord_y=y then
      begin
        trobat:=true;
        angle:=p.angle;
        pl:=p.planta;
      end
      else if ((p.ultim) or (p.coord_y>y))then final:=true
      else p:=p.seguent;
    end;
  end;

end;

procedure TMapa.RestaurarMapa();
var
  e:Integer;
  aux,t:TPixel;
begin
  for e:=0 to (ample-1) do
  begin
    t:=vector[e];
    if not t.ultim then
    begin
      t.ultim:=true;
      t.coord_y:=-1;
      t.coord_x:=e;
      t:=t.seguent;
      while not t.ultim do
      begin
        aux:=t;
        t:=t.seguent;
        aux.Free;
      end;
      t.free;
    end;
  end;
  for e := 0 to (ample-1) do
  begin
    vector[e].Free;
    vector[e]:= TPixel.Create(-1, -1,true,nil,0,nil);
  end;
end;

destructor TMapa.destroy;
var
  e:Integer;
  aux,t:TPixel;
begin
  for e:=0 to (ample-1) do
  begin
    t:=vector[e];
    while not t.ultim do
    begin
      aux:=t;
      t:=t.seguent;
      aux.Free;
    end;
    t.free;
  end;

  inherited Destroy;
end;

procedure TMapa.CarregarPixels(p: TPixel);
var
  e: TPixel;
begin
  e:=p;
  while not e.ultim do
  begin
    MapejarPixel(e);
    e:=e.seguent;
  end;
  MapejarPixel(e);
end;

procedure TMapa.MapejarPixel(p: TPixel);
var
  inserit: Boolean;
  ant,e,t: TPixel;
begin
  if ((p.coord_x < ample)and(p.coord_x >=0)and(p.coord_y < llarg)and(p.coord_y >=0)) then
  begin
    inserit:=false;
    e:=vector[p.coord_x];
    while (not inserit) do
    begin
      if ((e.ultim)and(e.coord_y<=p.coord_y)) then
      begin
        t:=TPixel.Create(p.coord_x, p.coord_y,true,nil,p.angle,p.planta);
        e.seguent:=t;
        e.ultim:=false;
        inserit:=true;
      end
      else if (e.coord_y>=p.coord_y) then
      begin
        t:=TPixel.Create(p.coord_x, p.coord_y,false,e,p.angle,p.planta);
        ant.seguent:=t;
        inserit:=true;
      end;
      ant:=e;
      e:=e.seguent;
    end;
  end;
end;

procedure TMapa.PintarMapa();
var
  e:Integer;
  t:TPixel;
begin
  e:=0;
  while e<ample do
  begin
    t:=vector[e];
    while not t.ultim do
    begin
      t:=t.seguent;
      imatge.Canvas.Pixels[t.coord_x,t.coord_y]:=rgb(0,0,0);
    end;
    e:=e+1;
  end;
end;
end.
