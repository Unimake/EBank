unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComObj,
  AutenticarAPI, RegistrarBoleto, ConsultarBoleto;

type

  { TfrmPrincipal }

  TfrmPrincipal = class(TForm)
    BtnAutenticarAPI: TButton;
    BtnRegistrarBoleto: TButton;
    BtnConsultarBoleto: TButton;
    GroupBox1: TGroupBox;
    procedure BtnAutenticarAPIClick(Sender: TObject);
    procedure BtnConsultarBoletoClick(Sender: TObject);
    procedure BtnRegistrarBoletoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private

  public

  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

{$R *.lfm}

{ TfrmPrincipal }


procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin

end;

procedure TfrmPrincipal.BtnAutenticarAPIClick(Sender: TObject);
var
  oServico: TAutenticarAPI;
begin
  oServico := TAutenticarAPI.Create;
  try
    oServico.Executar();
    ShowMessage('Token: ' + oServico.Token);
    ShowMessage('Expiration: ' + FloatToStr(oServico.Expiration));
  finally
  end;
end;

procedure TfrmPrincipal.BtnConsultarBoletoClick(Sender: TObject);
var
  oServico: TConsultarBoleto;
begin
  oServico := TConsultarBoleto.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.BtnRegistrarBoletoClick(Sender: TObject);
var
  oServico: TRegistrarBoleto;
begin
  oServico := TRegistrarBoleto.Create;
  try
    oServico.Executar();
  finally
  end;
end;

end.
