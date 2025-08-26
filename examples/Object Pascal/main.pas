unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComObj,
  AutenticarAPI;

type

  { TfrmPrincipal }

  TfrmPrincipal = class(TForm)
    BtnAutenticarAPI: TButton;
    GroupBox1: TGroupBox;
    procedure BtnAutenticarAPIClick(Sender: TObject);
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




end.
