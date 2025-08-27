// ------------------------------------------------------------------
// Consultar Boleto
// Padrão: Synapse (HTTPSend + ssl_openssl), usa TAutenticarAPI para Token
// ------------------------------------------------------------------
unit ConsultarBoleto;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  httpsend, ssl_openssl, synautil,
  fpjson, jsonparser, AutenticarAPI;

type
  TConsultarBoleto = class
  private
  class var FResponseJSON: string;
    class function BuildConsultaJson: string; static;
  public
    class procedure Executar; static;
    class property ResponseJSON: string read FResponseJSON; // JSON bruto retornado pela API
  end;

implementation

class function TConsultarBoleto.BuildConsultaJson: string;
begin
  Result := '{' + '  "testing": true,' +
    '  "numeroNoBanco": [ "00000001","00000002","00000003" ]' +
    //Números dos boletos a serem consultados
    '}';
end;

class procedure TConsultarBoleto.Executar;
const
  URL_Prod_Base = 'https://unimake.app/ebank/api/v1';
  // URL_Sbx_Base = 'https://ebank.sandbox.unimake.software/api/v1';
  CONFIGURATION_ID = 'ZCKWGQ55LTDXKYYC';
var
  Http: THTTPSend;
  Url: string;
  RequestBody: string;
  ResponseText: string;
  Json: TJSONData = nil;
  ResponseStream: TStringStream;
  Token: string;
  OutDir, OutFile: string;
  SL: TStringList;
begin
  Http := nil;
  ResponseStream := nil;
  FResponseJSON := '';

  try
    // 1) Autentica e obtém Bearer Token
    TAutenticarAPI.Executar;
    Token := TAutenticarAPI.Token;
    if Token = '' then
    begin
      MessageDlg('Autenticação', 'Token não obtido na autenticação.',
        mtError, [mbOK], 0);
      Exit;
    end;

    // 2) Monta JSON da consulta
    RequestBody := BuildConsultaJson;

    // 3) POST /boleto/consultar?configurationId=...
    Http := THTTPSend.Create;
    Http.Timeout := 20000;
    Http.KeepAlive := False;

    Url := URL_Prod_Base + '/boleto/consultar?configurationId=' + CONFIGURATION_ID;

    Http.Headers.Clear;
    Http.MimeType := 'application/json';
    Http.Headers.Add('Accept: application/json');
    Http.Headers.Add('Authorization: Bearer ' + Token);

    WriteStrToStream(Http.Document, RequestBody);
    Http.Document.Position := 0;

    if not Http.HTTPMethod('POST', Url) then
    begin
      MessageDlg('Erro na Requisição',
        'Falha na requisição: erro de transporte/conexão.', mtError, [mbOK], 0);
      Exit;
    end;

    ResponseStream := TStringStream.Create('');
    ResponseStream.CopyFrom(Http.Document, 0);
    ResponseText := ResponseStream.DataString;

    if (Http.ResultCode = 200) then
    begin
      // valida que seja um array, mas não exibe sucesso
      try
        Json := GetJSON(ResponseText);
      except
        on E: Exception do
        begin
          MessageDlg('Erro', 'Erro ao interpretar JSON.', mtError, [mbOK], 0);
          Exit;
        end;
      end;

      if Assigned(Json) then
      begin
        if (Json.JSONType <> jtArray) then
        begin
          MessageDlg('Formato inválido',
            'Formato do JSON inválido. Esperado um array de boletos.', mtError, [mbOK], 0);
          Exit;
        end;
        // guarda o JSON bruto para consumo externo
        FResponseJSON := ResponseText;

        // salvar o JSON em .\teste_ebank\json\GerarPixRetorno.json
        OutDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
          'teste_ebank' + DirectorySeparator + 'json';
        OutFile := IncludeTrailingPathDelimiter(OutDir) + 'ConsultarBoletoRetorno.json';

        ForceDirectories(ExtractFileDir(OutFile));

        SL := TStringList.Create;
        SL.Text := FResponseJSON;
        SL.SaveToFile(OutFile);

      end
      else
        MessageDlg('Erro', 'Erro ao interpretar JSON.', mtError, [mbOK], 0);
    end
    else
    begin
      MessageDlg('Erro na Requisição',
        Format('Falha na requisição: %d%s%s', [Http.ResultCode,
        LineEnding, ResponseText]),
        mtError, [mbOK], 0);
    end;

  finally
    if Assigned(Json) then Json.Free;
    if Assigned(ResponseStream) then ResponseStream.Free;
    if Assigned(Http) then Http.Free;
  end;
end;

end.
