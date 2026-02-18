#include "tip.ch"

// Summary: Gera uma cobranca PIX e retorna o hash da resposta.
// Return value: Hash com os dados do JSON retornado ou objeto Error.
FUNCTION GerarPIX()
   LOCAL hAuth := AutenticarAPI()
   LOCAL cToken := ""

   // Autentica e extrai o token; qualquer erro aborta aqui.
   IF ValType( hAuth ) == "H"
      IF HHasKey( hAuth, "token" )
         cToken := hAuth[ "token" ]
      ELSE
         RETURN ErrorNew( "", 1, 2001, "Campo 'token' nao encontrado na autenticacao" )
      ENDIF
   ELSEIF ValType( hAuth ) == "O"
      RETURN hAuth
   ELSE
      RETURN ErrorNew( "", 1, 2002, "Retorno inesperado na autenticacao" )
   ENDIF
RETURN ConsumirAPICobranca( cToken )

// Summary: Envia a cobranca PIX e retorna o hash de resposta.
// Params:
//   cToken -> Token Bearer da autenticacao.
// Return: Hash com os dados da cobranca ou objeto Error.
FUNCTION ConsumirAPICobranca( cToken )
   LOCAL cConfigPath := "config.json"
   LOCAL cConfigRaw := ""
   LOCAL hConfig := {=>}
   LOCAL nConfigDecoded := 0
   LOCAL xAmbiente := NIL
   LOCAL nAmbiente := NIL
   LOCAL cConfigurationID := NIL
   LOCAL cUrl := ""
   // Monta o JSON da cobranca antes da chamada HTTP.
   LOCAL cJson := CriarJSONCobrancaPIX()
   LOCAL oHttp := NIL
   LOCAL cResp := ""
   LOCAL hJson := {=>}
   LOCAL nStatus := 0
   LOCAL nDecoded := 0
   LOCAL cQRCodeImage := NIL
   LOCAL cPixCopiaECola := NIL
   LOCAL cSaveErr := ""
   LOCAL oErr := NIL
   LOCAL bOldError
   LOCAL CRLF := CHR(13) + CHR(10)
   LOCAL cHttpErr := ""
   LOCAL cStatusText := ""

   // Exibe o payload para facilitar suporte e validacao do exemplo.
   ? cJson
   ?
   Wait

   BEGIN SEQUENCE
      // ==============================
      // Leitura config.json
      // ==============================

      cConfigRaw := MemoRead( cConfigPath )
      IF Empty( cConfigRaw )
         BREAK
      ENDIF

      nConfigDecoded := hb_jsonDecode( cConfigRaw, @hConfig )
      IF nConfigDecoded == 0 .OR. ValType( hConfig ) <> "H"
         hConfig := hb_jsonDecode( cConfigRaw )
         IF ValType( hConfig ) <> "H"
            hConfig := JsonToHash( hConfig )
         ENDIF
      ENDIF

      IF ValType( hConfig ) <> "H"
         BREAK
      ENDIF

      xAmbiente := JsonGet( hConfig, "Ambiente" )
      cConfigurationID := JsonGet( hConfig, "ConfigurationID" )

      IF ValType( xAmbiente ) == "C"
         nAmbiente := Val( xAmbiente )
      ELSEIF ValType( xAmbiente ) == "N"
         nAmbiente := xAmbiente
      ENDIF

      IF nAmbiente == NIL .OR. Empty( cConfigurationID )
         BREAK
      ENDIF

      // URL muda conforme ambiente (producao/sandbox).
      cUrl := IF( nAmbiente == 1, ;
         "https://unimake.app/ebank/api/v1/pix/Cobranca?configurationId=", ;
         "https://ebank.sandbox.unimake.software/api/v1/pix/Cobranca?configurationId=" ) + cConfigurationID

      // ==============================
      // WinHTTP
      // ==============================

      // Captura excecoes COM/Harbour para detalhar falhas.
      oErr := NIL
      bOldError := ErrorBlock( { |e| oErr := e, Break( e ) } )

      BEGIN SEQUENCE
         oHttp := CreateObject( "WinHttp.WinHttpRequest.5.1" )
         IF oHttp == NIL
            BREAK
         ENDIF

         // Abre conexao
         // Chamada sincrona para simplificar o fluxo do exemplo.
         oHttp:Open( "POST", cUrl, .F. )

         // Headers
         oHttp:SetRequestHeader( "Authorization", "Bearer " + cToken )
         oHttp:SetRequestHeader( "Content-Type", "application/json" )
         oHttp:SetRequestHeader( "Accept", "application/json" )

         // Envia
         oHttp:Send( cJson )

         nStatus := oHttp:Status
         cResp := oHttp:ResponseText

         BEGIN SEQUENCE
            cStatusText := oHttp:StatusText
         RECOVER
         END SEQUENCE
      RECOVER
      END SEQUENCE

      ErrorBlock( bOldError )

      IF oErr <> NIL
         cHttpErr := CRLF + "Erro WinHTTP: " + ErrText( oErr )
         BREAK
      ENDIF

      IF oHttp == NIL
         BREAK
      ENDIF

      // Se o status for diferente de 200, interrompe o processamento para tratar o erro abaixo. 200 = sucesso.
      IF nStatus <> 200
         BREAK
      ENDIF
   RECOVER
      // Mantem fluxo para o tratamento de erro abaixo.
   END SEQUENCE

   // ==============================
   // Decodifica JSON resposta
   // ==============================

   // Tenta decodificar o JSON retornado para hash.
   hJson := {=>}
   nDecoded := hb_jsonDecode( cResp, @hJson )

   IF nDecoded == 0 .OR. ValType( hJson ) <> "H"
      hJson := hb_jsonDecode( cResp )
      IF ValType( hJson ) <> "H"
         hJson := JsonToHash( hJson )
      ENDIF
   ENDIF

   // Extrai campos principais para validacao e exibicao.
   IF ValType( hJson ) == "H"
      cQRCodeImage := JsonGet( hJson, "QRCodeImage" )
      cPixCopiaECola := JsonGet( hJson, "pixCopiaECola" )
   ENDIF

   // ==============================
   // Tratamento de erros
   // ==============================

   IF Empty( cConfigRaw )
      RETURN ErrorNew( "", 1, 2009, "Falha ao ler " + cConfigPath )
   ENDIF

   IF ValType( hConfig ) <> "H"
      RETURN ErrorNew( "", 1, 2010, "Erro ao decodificar JSON de configuracao" )
   ENDIF

   IF nAmbiente == NIL
      RETURN ErrorNew( "", 1, 2011, "Campo 'Ambiente' invalido" )
   ENDIF

   IF Empty( cConfigurationID )
      RETURN ErrorNew( "", 1, 2012, "Campo 'ConfigurationID' nao encontrado" )
   ENDIF

   IF oHttp == NIL
      RETURN ErrorNew( "", 1, 2003, "Falha ao criar WinHTTP" + cHttpErr )
   ENDIF

   IF nStatus <> 200
      IF Empty( cHttpErr ) .AND. nStatus == 0
         cHttpErr := CRLF + "Erro WinHTTP: sem resposta HTTP; verifique URL/DNS/SSL/proxy"
      ENDIF
      RETURN ErrorNew( "", 1, 2005, ;
         "Falha HTTP: " + LTrim( Str( nStatus ) ) + ;
         IIF( Empty( cStatusText ), "", " " + cStatusText ) + ;
         CRLF + "URL: " + cUrl + ;
         IIF( Empty( cResp ), "", CRLF + cResp ) + cHttpErr )
   ENDIF

   IF ValType( hJson ) <> "H"
      RETURN ErrorNew( "", 1, 2006, "Erro ao decodificar o JSON de resposta" )
   ENDIF

   IF cQRCodeImage == NIL
      RETURN ErrorNew( "", 1, 2007, "Campo 'QRCodeImage' nao encontrado no JSON de resposta" )
   ENDIF

   IF cPixCopiaECola == NIL
      RETURN ErrorNew( "", 1, 2008, "Campo 'pixCopiaECola' nao encontrado no JSON de resposta" )
   ENDIF

   // Salva o QRCode somente quando vier preenchido.
   IF ValType( cQRCodeImage ) == "C" .AND. ! Empty( AllTrim( cQRCodeImage ) )
      cSaveErr := SaveQRCodeImage( cQRCodeImage )
      IF ValType( cSaveErr ) == "C" .AND. ! Empty( cSaveErr )
         RETURN ErrorNew( "", 1, 2013, cSaveErr )
      ENDIF
   ENDIF
RETURN hJson

// Summary: Monta o JSON de cobranca PIX.
FUNCTION CriarJSONCobrancaPIX()
   LOCAL hJson := {=>}
   LOCAL hQrCode := {=>}
   LOCAL hCalendario := {=>}
   LOCAL hDevedor := {=>}
   LOCAL hValor := {=>}

   hJson[ "testing" ] := .T. // Campo para indicar ambiente de teste; pode ser removido em producao
   hJson[ "chave" ] := "unimake@unimake.com.br"
   hJson[ "gerarQRCode" ] := .T.
   hJson[ "testing" ] := .T.
   hJson[ "txId" ] := "txid2025013117171234567894567894564"
   hJson[ "solicitacaoPagador" ] := "Recebimento PIX"

   hQrCode[ "height" ] := 256
   hQrCode[ "imageFormat" ] := "Jpeg"
   hQrCode[ "quality" ] := 100
   hQrCode[ "width" ] := 256
   hJson[ "qrCodeConfig" ] := hQrCode

   hCalendario[ "criacao" ] := DateToIso( Date() )
   hCalendario[ "expiracao" ] := 1800
   hCalendario[ "validadeAposVencimento" ] := 0
   hJson[ "calendario" ] := hCalendario

   hDevedor[ "inscricao" ] := "00000000000"
   hDevedor[ "nome" ] := Left( AllTrim( "Fulano teste" ), 30 )
   hDevedor[ "logradouro" ] := Left( AllTrim( "Rua de teste" ), 30 )
   hDevedor[ "cidade" ] := Left( AllTrim( "Paranavai" ), 20 )
   hDevedor[ "cep" ] := "87706070"
   hDevedor[ "uf" ] := "PR"
   hJson[ "devedor" ] := hDevedor

   hValor[ "original" ] := 10
   hJson[ "valor" ] := hValor
RETURN hb_jsonEncode( hJson, .F. )

// Summary: Extrai texto amigavel de um erro COM/Harbour.
// Params:
//   oErr -> objeto de erro
// Return: texto descritivo
STATIC FUNCTION ErrText( oErr )
   LOCAL cText := ""
   LOCAL xVal
   LOCAL bOldError

   IF oErr == NIL
      RETURN ""
   ENDIF

   bOldError := ErrorBlock( { |e| Break( e ) } )
   BEGIN SEQUENCE
      xVal := __objSendMsg( oErr, "Description" )
      IF ValType( xVal ) == "C" .AND. ! Empty( xVal )
         cText := xVal
      ENDIF

      xVal := __objSendMsg( oErr, "Operation" )
      IF ValType( xVal ) == "C" .AND. ! Empty( xVal )
         cText += IIF( Empty( cText ), "", " (" ) + xVal + IIF( Empty( cText ), "", ")" )
      ENDIF
   RECOVER
   END SEQUENCE
   ErrorBlock( bOldError )

   IF Empty( cText )
      cText := "falha de comunicacao; verifique DNS/SSL/proxy"
   ENDIF

RETURN cText

// Summary: Salva o QRCode em Base64 como imagem em disco.
// Params:
//   cBase64 -> imagem em Base64
// Return: string de erro ou vazio em caso de sucesso
STATIC FUNCTION SaveQRCodeImage( cBase64 )
   LOCAL cDir := "QRCodePIX"
   LOCAL cFile := ""
   LOCAL cBin := ""
   LOCAL nHandle := -1
   LOCAL nWritten := 0

   IF Empty( cBase64 )
      RETURN ""
   ENDIF

   // Cria a pasta se ainda nao existir.
   IF ! IsDirectory( cDir )
      IF MakeDir( cDir ) <> 0
         RETURN "Falha ao criar pasta " + cDir
      ENDIF
   ENDIF

   // Decodifica o Base64 para binario (imagem).
   cBin := hb_base64Decode( cBase64 )
   IF Empty( cBin )
      RETURN "Falha ao decodificar Base64 do QRCode"
   ENDIF

   // Nome de arquivo unico baseado em data/hora.
   cFile := cDir + "\\QRCode_" + DToS( Date() ) + "_" + StrTran( Time(), ":", "" ) + ".jpg"

   nHandle := FCreate( cFile )
   IF nHandle < 0
      RETURN "Falha ao criar arquivo " + cFile
   ENDIF

   nWritten := FWrite( nHandle, cBin )
   FClose( nHandle )
   IF nWritten <> Len( cBin )
      RETURN "Falha ao gravar arquivo " + cFile
   ENDIF
RETURN ""

// Summary: Converte data para ISO (YYYY-MM-DD).
STATIC FUNCTION DateToIso( dDate )
   LOCAL cDate := DToS( dDate )

   IF Empty( cDate ) .OR. Len( cDate ) < 8
      RETURN ""
   ENDIF
RETURN SubStr( cDate, 1, 4 ) + "-" + SubStr( cDate, 5, 2 ) + "-" + SubStr( cDate, 7, 2 )
