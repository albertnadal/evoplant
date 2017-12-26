program Plantes;

uses
  Forms,
  PlantesCarnivores in 'PlantesCarnivores.pas' {Form1},
  EntradaDades in 'EntradaDades.pas' {Form2},
  Intro in 'Intro.pas' {Form3},
  Tutorial in 'Tutorial.pas' {Form4};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'EVOPLANT';
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
