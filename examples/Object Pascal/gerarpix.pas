// ------------------------------------------------------------------
// Gerar PIX
// Padrão: Synapse (HTTPSend + ssl_openssl), usa TAutenticarAPI para Token
// - Salva QRCodeImage (base64) como PNG em .\teste_ebank\image\qrcode.png
// ------------------------------------------------------------------
unit GerarPIX;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  httpsend, ssl_openssl, synautil, synacode,
  fpjson, jsonparser, AutenticarAPI,
  FPImage, FPReadJPEG, FPReadPNG, FPWritePNG; // conversão JPEG -> PNG

type
  TGerarPIX = class
  private
    class var FQRCodeImage: string;      // base64 (ou data-url)
    class var FPixCopiaECola: string;   // EMV
    class var FResponseJSON: string;     // JSON bruto
    class var FQRCodeImagePath: string;  // caminho do PNG salvo
    class function BuildCobrancaJson: string; static;
    class function SaveQRCodeImageToPNG(const ABase64: string; const AFilePath: string): Boolean; static;
  public
    class procedure Executar; static;
    class property QRCodeImage: string read FQRCodeImage;
    class property PixCopiaECola: string read FPixCopiaECola;
    class property ResponseJSON: string read FResponseJSON;
    class property QRCodeImagePath: string read FQRCodeImagePath;
  end;

implementation

class function TGerarPIX.BuildCobrancaJson: string;
var
  DataCriacao: string;
begin
  DataCriacao := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now);

  Result := '{' +
            '  "chave": "unimake@unimake.com.br",' +
            '  "gerarQRCode": true,' +
            '  "testing": true,' +
            '  "txId": "txid2025013117171234567894567894564",' +
            '  "solicitacaoPagador": "Recebimento PIX",' +
            '  "qrCodeConfig": {' +
            '    "height": 256,' +
            '    "imageFormat": "Jpeg",' +
            '    "quality": 100,' +
            '    "width": 256' +
            '  },' +
            '  "calendario": {' +
            '    "criacao": "' + DataCriacao + '",' +
            '    "expiracao": 1800,' +
            '    "validadeAposVencimento": 0' +
            '  },' +
            '  "devedor": {' +
            '    "inscricao": "00000000000",' +
            '    "nome": "' + Copy(Trim('Fulano teste'), 1, 30) + '",' +
            '    "logradouro": "' + Copy(Trim('Rua de teste'), 1, 30) + '",' +
            '    "cidade": "' + Copy(Trim('Paranavaí'), 1, 20) + '",' +
            '    "cep": "87706070",' +
            '    "uf": "PR"' +
            '  },' +
            '  "valor": {' +
            '    "original": 10' +
            '  }' +
            '}';
end;

class function TGerarPIX.SaveQRCodeImageToPNG(const ABase64: string; const AFilePath: string): Boolean;
var
  B64, Dir: string;
  Bin: AnsiString;
  MS: TMemoryStream;
  FS: TFileStream;
  IsPNG, IsJPG: Boolean;
  Sig: array[0..7] of Byte = (0,0,0,0,0,0,0,0);
  Img: TFPMemoryImage;
  ReaderJPEG: TFPReaderJPEG;
  ReaderPNG: TFPReaderPNG;
  WriterPNG: TFPWriterPNG;
begin
  Result := False;
  FQRCodeImagePath := '';

  if ABase64 = '' then Exit;

  // remove prefixo data:*;base64,
  B64 := ABase64;
  if Pos('base64,', B64) > 0 then
    B64 := Copy(B64, Pos('base64,', B64) + Length('base64,'), MaxInt);

  // decodifica
  Bin := DecodeBase64(B64);
  if Length(Bin) = 0 then Exit;

  // garante diretório
  Dir := ExtractFileDir(AFilePath);
  if (Dir <> '') and (not DirectoryExists(Dir)) then
    if not ForceDirectories(Dir) then Exit;

  MS := TMemoryStream.Create;
  try
    MS.WriteBuffer(Bin[1], Length(Bin));
    MS.Position := 0;

    // detectar assinatura
    FillChar(Sig, SizeOf(Sig), 0);
    if MS.Size >= 8 then MS.ReadBuffer(Sig, 8);
    MS.Position := 0;

    IsPNG := (Sig[0]=$89) and (Sig[1]=$50) and (Sig[2]=$4E) and (Sig[3]=$47) and
             (Sig[4]=$0D) and (Sig[5]=$0A) and (Sig[6]=$1A) and (Sig[7]=$0A);
    IsJPG := (Sig[0]=$FF) and (Sig[1]=$D8) and (Sig[2]=$FF);

    if IsPNG then
    begin
      FS := TFileStream.Create(AFilePath, fmCreate);
      try
        MS.SaveToStream(FS);
      finally
        FS.Free;
      end;
      Result := True;
      FQRCodeImagePath := AFilePath;
      Exit;
    end
    else if IsJPG then
    begin
      // converter JPEG -> PNG
      Img := TFPMemoryImage.Create(0,0);
      ReaderJPEG := TFPReaderJPEG.Create;
      WriterPNG := TFPWriterPNG.Create;
      try
        Img.LoadFromStream(MS, ReaderJPEG);
        FS := TFileStream.Create(AFilePath, fmCreate);
        try
          Img.SaveToStream(FS, WriterPNG);
        finally
          FS.Free;
        end;
        Result := True;
        FQRCodeImagePath := AFilePath;
        Exit;
      finally
        WriterPNG.Free;
        ReaderJPEG.Free;
        Img.Free;
      end;
    end
    else
    begin
      // formato desconhecido: tenta PNG direto (ler como PNG)
      Img := TFPMemoryImage.Create(0,0);
      ReaderPNG := TFPReaderPNG.Create;
      WriterPNG := TFPWriterPNG.Create;
      try
          Img.LoadFromStream(MS, ReaderPNG); // se não for PNG válido, exception
        FS := TFileStream.Create(AFilePath, fmCreate);
          try
          Img.SaveToStream(FS, WriterPNG);
          finally
          FS.Free;
          end;
          Result := True;
          FQRCodeImagePath := AFilePath;
        except
            Result := False;
          end;
        WriterPNG.Free;
        ReaderPNG.Free;
        Img.Free;
    end;
  finally
    MS.Free;
  end;
end;

class procedure TGerarPIX.Executar;
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
  Node: TJSONData;
  ResponseStream: TStringStream;
  Token: string;
  OutDir, OutFile: string;
  SL: TStringList;
begin
  Http := nil;
  ResponseStream := nil;
  FQRCodeImage := '';
  FPixCopiaECola := '';
  FResponseJSON := '';
  FQRCodeImagePath := '';

  try
    // 1) Autenticação (usa sua classe padrão)
    TAutenticarAPI.Executar;
    Token := TAutenticarAPI.Token;
    if Token = '' then
    begin
      MessageDlg('Autenticação', 'Token não obtido na autenticação.', mtError, [mbOK], 0);
      Exit;
    end;

    // 2) Monta JSON da cobrança PIX
    RequestBody := BuildCobrancaJson;

    // 3) POST /pix/Cobranca?configurationId=...
    Http := THTTPSend.Create;
    Http.Timeout := 20000;
    Http.KeepAlive := False;

    Url := URL_Prod_Base + '/pix/Cobranca?configurationId=' + CONFIGURATION_ID;

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
      FResponseJSON := ResponseText; // guarda JSON bruto

      // salvar o JSON em .\teste_ebank\json\GerarPixRetorno.json
      OutDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
                'teste_ebank' + DirectorySeparator + 'json';
      OutFile := IncludeTrailingPathDelimiter(OutDir) + 'GerarPixRetorno.json';

      ForceDirectories(ExtractFileDir(OutFile));

      SL := TStringList.Create;
      SL.Text := TGerarPIX.ResponseJSON;
      SL.SaveToFile(OutFile);  // caminho onde salvar

      // Interpreta apenas as chaves esperadas no VB6 (sem mensagens de sucesso)
      try
        Json := GetJSON(ResponseText);
      except
        on E: Exception do
        begin
          MessageDlg('Erro', 'Erro ao interpretar JSON.', mtError, [mbOK], 0);
          Exit;
        end;
      end;

      if Assigned(Json) and (Json.JSONType = jtObject) then
      begin
        Node := TJSONObject(Json).Find('QRCodeImage');
        if Assigned(Node) then
        begin
          //Base64 que contem a imagem QRCode do PIX
          FQRCodeImage := Node.AsString;

          // salvar PNG em .\teste_ebank\image\qrcode.png
          OutDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
                    'teste_ebank' + DirectorySeparator + 'image';
          OutFile := IncludeTrailingPathDelimiter(OutDir) + 'qrcode.png';

          if not SaveQRCodeImageToPNG(FQRCodeImage, OutFile) then
            MessageDlg('Erro', 'Falha ao salvar o QRCode em PNG.', mtError, [mbOK], 0);
        end
        else
          MessageDlg('Erro de JSON', 'Chave ''QRCodeImage'' não encontrada no JSON.', mtError, [mbOK], 0);

        Node := TJSONObject(Json).Find('pixCopiaECola');
        if Assigned(Node) then
          //Variável que contém o PIX CopiaECola
          FPixCopiaECola := Node.AsString
        else
          MessageDlg('Erro de JSON', 'Chave ''pixCopiaECola'' não encontrada no JSON.', mtError, [mbOK], 0);
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
    if Assigned(SL) then SL.Free;

  end;
end;

end.
