// ------------------------------------------------------------------
// Registrar Boleto
// Padrão: Synapse (HTTPSend + ssl_openssl), usa TAutenticarAPI para Token
// ------------------------------------------------------------------
unit RegistrarBoleto;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  httpsend, ssl_openssl, synautil, synacode,
  fpjson, jsonparser;

type
  TRegistrarBoleto = class
  private
    class var FCodigoBarraNumerico: string;
    class var FLinhaDigitavel: string;
    class var FNumeroNoBanco: string;
    class var FPDFPath: string;
    class function BuildBoletoJson: string; static;
    class function SavePDFBase64(const ABase64, AFilePath: string): Boolean; static;
  public
    class procedure Executar; static;
    class property CodigoBarraNumerico: string read FCodigoBarraNumerico;
    class property LinhaDigitavel: string read FLinhaDigitavel;
    class property NumeroNoBanco: string read FNumeroNoBanco;
    class property PDFPath: string read FPDFPath;
  end;

implementation

uses AutenticarAPI; // usa TAutenticarAPI para obter Token/Expiration

class function TRegistrarBoleto.BuildBoletoJson: string;
var
  DataEmissao, DataVencimento, DataJuros: string;
begin
  DataEmissao    := FormatDateTime('yyyy-mm-dd', Now);
  DataVencimento := FormatDateTime('yyyy-mm-dd', Now + 30);
  DataJuros      := FormatDateTime('yyyy-mm-dd', Now + 31);

  // Monta o JSON
  Result :=
    '{' +
    '  "especie": 2,' +
    '  "numeroParcela": 1,' +
    '  "numeroNoBanco": "00000008548",' +
    '  "numeroNaEmpresa": "000001-01",' +
    '  "vencimento": "' + DataVencimento + '",' +
    '  "emissao": "' + DataEmissao + '",' +
    '  "diasParaBaixaOuDevolucao": 0,' +
    '  "tipoBaixaDevolucao": 1,' +
    '  "valorIof": 0,' +
    '  "valorNominal": 10,' +
    '  "valorAbatimento": 0,' +
    '  "testing": true,' +
    '  "mensagens": ["JUROS DIARIOS SOBRE ATRASO PAG.R$ 0,02"],' +
    '  "juros": {' +
    '    "tipo": 1,' +
    '    "data": "' + DataJuros + '",' +
    '    "valor": 0.02' +
    '  },' +
    '  "pagador": {' +
    '    "endereco": {' +
    '      "rua": "RUA Lorem Ipsum",' +
    '      "numero": "001",' +
    '      "bairro": "Centro",' +
    '      "cep": "87711340",' +
    '      "cidade": "PARANAVAI",' +
    '      "uf": "PR"' +
    '    },' +
    '    "nome": "Marcelo de Souza",' +
    '    "tipoInscricao": 1,' +
    '    "inscricao": "25806756807"' +
    '  },' +
    '  "pdfConfig": {' +
    '    "tryGeneratePDF": true' +
    '  }' +
    '}';
end;

class function TRegistrarBoleto.SavePDFBase64(const ABase64, AFilePath: string): Boolean;
var
  Bin: AnsiString;
  FS: TFileStream;
  Dir: string;
begin
  Result := False;
  if ABase64 = '' then Exit;

  // Decodifica base64 (Synapse)
  Bin := DecodeBase64(ABase64);
  if Length(Bin) = 0 then Exit;

  Dir := ExtractFileDir(AFilePath);
  if (Dir <> '') and (not DirectoryExists(Dir)) then
    if not ForceDirectories(Dir) then Exit;

  FS := TFileStream.Create(AFilePath, fmCreate);
  try
    if Length(Bin) > 0 then
      FS.WriteBuffer(Bin[1], Length(Bin));
    Result := True;
  finally
    FS.Free;
  end;
end;

class procedure TRegistrarBoleto.Executar;
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
  Node, PdfNode: TJSONData;
  ResponseStream: TStringStream;
  Token: string;
  PdfContentB64: string;
  OutDir, OutFile: string;
begin
  Http := nil;
  ResponseStream := nil;

  // limpa valores anteriores
  FCodigoBarraNumerico := '';
  FLinhaDigitavel := '';
  FNumeroNoBanco := '';
  FPDFPath := '';

  try
    // 1) Autentica (reutiliza a sua classe já padrão)
    TAutenticarAPI.Executar;
    Token := TAutenticarAPI.Token;
    if Token = '' then
    begin
      MessageDlg('Autenticação', 'Token não obtido na autenticação.', mtError, [mbOK], 0);
      Exit;
    end;

    // 2) Monta JSON do boleto
    RequestBody := BuildBoletoJson;

    // 3) POST /boleto/registrar?configurationId=...
    Http := THTTPSend.Create;
    Http.Timeout := 20000;
    Http.KeepAlive := False;

    Url := URL_Prod_Base + '/boleto/registrar?configurationId=' + CONFIGURATION_ID;

    Http.Headers.Clear;
    Http.MimeType := 'application/json';
    Http.Headers.Add('Accept: application/json');
    Http.Headers.Add('Authorization: Bearer ' + Token);

    WriteStrToStream(Http.Document, RequestBody);
    Http.Document.Position := 0;

    if not Http.HTTPMethod('POST', Url) then
    begin
      MessageDlg('Erro na Requisição', 'Falha na requisição: erro de transporte/conexão.', mtError, [mbOK], 0);
      Exit;
    end;

    ResponseStream := TStringStream.Create('');
    ResponseStream.CopyFrom(Http.Document, 0);
    ResponseText := ResponseStream.DataString;

    if (Http.ResultCode = 200) then
    begin
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
        // CodigoBarraNumerico
        Node := Json.FindPath('CodigoBarraNumerico');
        if Assigned(Node) then
          FCodigoBarraNumerico := Node.AsString;

        // LinhaDigitavel
        Node := Json.FindPath('LinhaDigitavel');
        if Assigned(Node) then
          FLinhaDigitavel := Node.AsString;

        // NumeroNoBanco
        Node := Json.FindPath('NumeroNoBanco');
        if Assigned(Node) then
          FNumeroNoBanco := Node.AsString;

        // PDFContent.Content (base64)
        Node := Json.FindPath('PDFContent');
        if Assigned(Node) and (Node.JSONType = jtObject) then
        begin
          PdfNode := TJSONObject(Node).Find('Content');
          if Assigned(PdfNode) then
          begin
            PdfContentB64 := PdfNode.AsString;

            OutDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'teste_ebank' + DirectorySeparator + 'pdf';
            OutFile := IncludeTrailingPathDelimiter(OutDir) + 'arquivo.pdf';

            if SavePDFBase64(PdfContentB64, OutFile) then
              FPDFPath := OutFile;
          end;
        end;
      end
      else
        MessageDlg('Erro', 'Erro ao interpretar JSON.', mtError, [mbOK], 0);
    end
    else
    begin
      MessageDlg('Erro na Requisição',
        Format('Falha na requisição: %d%s%s', [Http.ResultCode, LineEnding, ResponseText]),
        mtError, [mbOK], 0);
    end;

  finally
    if Assigned(Json) then Json.Free;
    if Assigned(ResponseStream) then ResponseStream.Free;
    if Assigned(Http) then Http.Free;
  end;
end;

end.
