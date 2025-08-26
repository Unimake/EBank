// ------------------------------------------------------------------
// Autenticar API Unimake
// Stack: Synapse (HTTPSend) + OpenSSL (ssl_openssl)
// ------------------------------------------------------------------
unit AutenticarAPI;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, IniFiles,
  httpsend, ssl_openssl, synautil,   // Synapse HTTP + OpenSSL + utilitários
  fpjson, jsonparser;                // Parsing JSON

type
  TAutenticarAPI = class
  private
    class var FToken: string;
    class var FExpiration: Double; // use Int64 se preferir timestamp inteiro
  public
    class procedure Executar; static;
    class property Token: string read FToken;
    class property Expiration: Double read FExpiration;
  end;

implementation

class procedure TAutenticarAPI.Executar;
const
  URL_Producao = 'https://unimake.app/auth/api/auth';
  // URL_Sandbox = 'https://auth.sandbox.unimake.software/api/auth';
var
  Http: THTTPSend;
  Url: string;
  RequestBody: string;
  ResponseText: string;
  Json: TJSONData = nil;
  Node: TJSONData;
  ResponseStream: TStringStream;
  Ini: TIniFile;
  AppId, Secret: string;
begin
  Http := nil;
  ResponseStream := nil;
  Ini := nil;

  // limpa valores anteriores
  FToken := '';
  FExpiration := 0;

  try
    //Ler do .INI o appId e secret do eBank
    Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'auth.ini');
    AppId  := Ini.ReadString('Auth', 'AppId', '');
    Secret := Ini.ReadString('Auth', 'Secret', '');

    if (AppId = '') or (Secret = '') then
    begin
      MessageDlg('Configuração inválida',
        'AppId ou Secret não encontrados no arquivo auth.ini',
        mtError, [mbOK], 0);
      Exit;
    end;

    Http := THTTPSend.Create;

    // Timeout total (ms) para a operação HTTP
    Http.Timeout := 20000; // 20s
    Http.KeepAlive := False;

    // URL alvo
    Url := URL_Producao;
    // Url := URL_Sandbox;

    // Cabeçalhos
    Http.Headers.Clear;
    Http.MimeType := 'application/json';
    Http.Headers.Add('Accept: application/json');

    // Corpo JSON (em produção, ler de configuração/segredo seguro)
    RequestBody :=
      Format('{"appId": "%s","secret": "%s"}', [AppId, Secret]);

    // Escreve o corpo na stream do request
    WriteStrToStream(Http.Document, RequestBody);
    Http.Document.Position := 0;

    // POST síncrono
    if not Http.HTTPMethod('POST', Url) then
    begin
      MessageDlg('Erro na Requisição',
        'Falha na requisição: erro de transporte/conexão.',
        mtError, [mbOK], 0);
      Exit;
    end;

    // Copia corpo de resposta para string
    ResponseStream := TStringStream.Create('');
    ResponseStream.CopyFrom(Http.Document, 0);
    ResponseText := ResponseStream.DataString;

    // Checagem explícita do status
    if (Http.ResultCode = 200) then
    begin
      // Decodifica JSON
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
        // expiration
        Node := Json.FindPath('expiration');
        if Assigned(Node) then
          FExpiration := Node.AsFloat
        else
          MessageDlg('Erro de JSON',
            'Chave ''expiration'' não encontrada no JSON.',
            mtError, [mbOK], 0);

        // token
        Node := Json.FindPath('token');
        if Assigned(Node) then
          FToken := Node.AsString
        else
          MessageDlg('Erro de JSON',
            'Chave ''token'' não encontrada no JSON.',
            mtError, [mbOK], 0);
      end
      else
        MessageDlg('Erro', 'Erro ao interpretar JSON.', mtError, [mbOK], 0);
    end
    else
    begin
      MessageDlg('Erro na Requisição',
        Format('Falha na requisição: %d%s%s',
               [Http.ResultCode, LineEnding, ResponseText]),
        mtError, [mbOK], 0);
    end;

  finally
    if Assigned(Json) then Json.Free;
    if Assigned(ResponseStream) then ResponseStream.Free;
    if Assigned(Http) then Http.Free;
  end;
end;

end.
