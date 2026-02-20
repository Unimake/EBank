#include "tip.ch"

// ============================================================================
// RegistrarBoleto.prg
// Padrao baseado no GerarPIX.prg:
// - Autentica via AutenticarAPI() e usa o mesmo padrao de retorno (Hash ou Error)
// - JSON: hb_jsonEncode / hb_jsonDecode + JsonToHash/JsonGet
// - HTTP: WinHttp.WinHttpRequest.5.1 (sincrono)
// - Base64: hb_base64Decode (mesmo padrao do SaveQRCodeImage do GerarPIX)
// - Tratamento de erro: ErrorNew + ErrText
//
// Requisitos:
// - existir config.json com:
//     {
//       "Ambiente": 1,              // 1=Producao, 2=Sandbox (mesma logica do GerarPIX)
//       "ConfigurationID": "...."   // configurationId fornecido pela Unimake
//     }
// - existir funcao AutenticarAPI() no projeto (a mesma usada no GerarPIX.prg)
// ============================================================================

// Summary: Registra um boleto e retorna o hash da resposta.
// Return value: Hash com os dados do JSON retornado ou objeto Error.
FUNCTION RegistrarBoleto()
   LOCAL hAuth := AutenticarAPI()
   LOCAL cToken := ""

   // Autentica e extrai o token; qualquer erro aborta aqui.
   IF ValType( hAuth ) == "H"
      IF HHasKey( hAuth, "token" )
         cToken := hAuth[ "token" ]
      ELSE
         RETURN ErrorNew( "", 1, 3001, "Campo 'token' nao encontrado na autenticacao" )
      ENDIF
   ELSEIF ValType( hAuth ) == "O"
      RETURN hAuth
   ELSE
      RETURN ErrorNew( "", 1, 3002, "Retorno inesperado na autenticacao" )
   ENDIF
RETURN ConsumirAPIRegistraBoleto( cToken )

// Summary: Envia o registro de boleto e retorna o hash de resposta.
// Params:
//   cToken -> Token Bearer da autenticacao.
// Return: Hash com os dados do registro ou objeto Error.
FUNCTION ConsumirAPIRegistraBoleto( cToken )
   LOCAL cConfigPath := "config.json"
   LOCAL cConfigRaw := ""
   LOCAL hConfig := {=>}
   LOCAL nConfigDecoded := 0
   LOCAL xAmbiente := NIL
   LOCAL nAmbiente := NIL
   LOCAL cConfigurationID := NIL
   LOCAL cUrl := ""
   LOCAL cJson := CriarJSONBoleto()
   LOCAL oHttp := NIL
   LOCAL cResp := ""
   LOCAL hJson := {=>}
   LOCAL nStatus := 0
   LOCAL nDecoded := 0
   LOCAL oErr := NIL
   LOCAL bOldError
   LOCAL CRLF := CHR(13) + CHR(10)
   LOCAL cHttpErr := ""
   LOCAL cStatusText := ""
   LOCAL cProcErr := ""

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
      // Producao (Ambiente=1): https://unimake.app/...
      // Sandbox (outros):      https://ebank.sandbox.unimake.software/...
      cUrl := IF( nAmbiente == 1, ;
         "https://unimake.app/ebank/api/v1/boleto/registrar?configurationId=", ;
         "https://ebank.sandbox.unimake.software/api/v1/boleto/registrar?configurationId=" ) + cConfigurationID

      // ==============================
      // WinHTTP
      // ==============================
      oErr := NIL
      bOldError := ErrorBlock( { |e| oErr := e, Break( e ) } )

      BEGIN SEQUENCE
         oHttp := CreateObject( "WinHttp.WinHttpRequest.5.1" )
         IF oHttp == NIL
            BREAK
         ENDIF

         // Abre conexao (sincrona)
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

      // 200 = sucesso
      IF nStatus <> 200
         BREAK
      ENDIF
   RECOVER
      // Mantem fluxo para o tratamento de erro abaixo.
   END SEQUENCE

   // ==============================
   // Decodifica JSON resposta
   // ==============================
   hJson := {=>}
   nDecoded := hb_jsonDecode( cResp, @hJson )

   IF nDecoded == 0 .OR. ValType( hJson ) <> "H"
      hJson := hb_jsonDecode( cResp )
      IF ValType( hJson ) <> "H"
         hJson := JsonToHash( hJson )
      ENDIF
   ENDIF

   // ==============================
   // Tratamento de erros (padrao GerarPIX)
   // ==============================
   IF Empty( cConfigRaw )
      RETURN ErrorNew( "", 1, 3009, "Falha ao ler " + cConfigPath )
   ENDIF

   IF ValType( hConfig ) <> "H"
      RETURN ErrorNew( "", 1, 3010, "Erro ao decodificar JSON de configuracao" )
   ENDIF

   IF nAmbiente == NIL
      RETURN ErrorNew( "", 1, 3011, "Campo 'Ambiente' invalido" )
   ENDIF

   IF Empty( cConfigurationID )
      RETURN ErrorNew( "", 1, 3012, "Campo 'ConfigurationID' nao encontrado" )
   ENDIF

   IF oHttp == NIL
      RETURN ErrorNew( "", 1, 3003, "Falha ao criar WinHTTP" + cHttpErr )
   ENDIF

   IF nStatus <> 200
      IF Empty( cHttpErr ) .AND. nStatus == 0
         cHttpErr := CRLF + "Erro WinHTTP: sem resposta HTTP; verifique URL/DNS/SSL/proxy"
      ENDIF
      RETURN ErrorNew( "", 1, 3005, ;
         "Falha HTTP: " + LTrim( Str( nStatus ) ) + ;
         IIF( Empty( cStatusText ), "", " " + cStatusText ) + ;
         CRLF + "URL: " + cUrl + ;
         IIF( Empty( cResp ), "", CRLF + cResp ) + cHttpErr )
   ENDIF

   IF ValType( hJson ) <> "H"
      RETURN ErrorNew( "", 1, 3006, "Erro ao decodificar o JSON de resposta" )
   ENDIF

   // ==============================
   // Pos-processamento (PDF, linha digitavel, etc.)
   // ==============================
   cProcErr := ProcessarRetornoBoleto( hJson )
   IF ValType( cProcErr ) == "C" .AND. ! Empty( cProcErr )
      RETURN ErrorNew( "", 1, 3013, cProcErr )
   ENDIF
RETURN hJson

// Summary: Monta o JSON do registro de boleto (padrao hash + hb_jsonEncode).
FUNCTION CriarJSONBoleto()
   LOCAL hRoot := {=>}
   LOCAL hJuros := {=>}
   LOCAL hPagador := {=>}
   LOCAL hEndereco := {=>}
   LOCAL hPdfConfig := {=>}
   LOCAL hPixConfig := {=>}
   LOCAL aMensagens := {}
   LOCAL dEmissao := Date()
   LOCAL dVencimento := Date() + 30
   LOCAL dDataJuros := Date() + 40

   hRoot[ "Aceite" ] := .T.
   hRoot[ "emissao" ] := DateToIso( dEmissao )
   hRoot[ "especie" ] := 2
   hRoot[ "vencimento" ] := DateToIso( dVencimento )
   hRoot[ "valorIof" ] := 0
   hRoot[ "valorNominal" ] := 10
   hRoot[ "valorAbatimento" ] := 0
   hRoot[ "numeroParcela" ] := 1
   hRoot[ "numeroNoBanco" ] := "00000008548"
   hRoot[ "numeroNaEmpresa" ] := "000001-01"
   hRoot[ "diasParaBaixaOuDevolucao" ] := 0
   hRoot[ "tipoBaixaDevolucao" ] := 1
   hRoot[ "testing" ] := .T.

   AAdd( aMensagens, "JUROS DIARIOS SOBRE ATRASO PAG.R$ 0,02" )
   hRoot[ "mensagens" ] := aMensagens

   hJuros[ "tipo" ] := 1
   hJuros[ "data" ] := DateToIso( dDataJuros )
   hJuros[ "valor" ] := 0.02
   hRoot[ "juros" ] := hJuros

   hEndereco[ "rua" ] := "RUA Lorem Ipsum"
   hEndereco[ "numero" ] := "001"
   hEndereco[ "bairro" ] := "Centro"
   hEndereco[ "cep" ] := "87711340"
   hEndereco[ "cidade" ] := "PARANAVAI"
   hEndereco[ "uf" ] := "PR"

   hPagador[ "endereco" ] := hEndereco
   hPagador[ "nome" ] := "Marcelo de Souza"
   hPagador[ "tipoInscricao" ] := 1
   hPagador[ "inscricao" ] := "25806756807"
   hRoot[ "pagador" ] := hPagador

   hPdfConfig[ "tryGeneratePDF" ] := .T.
   hRoot[ "pdfConfig" ] := hPdfConfig

   hPixConfig[ "chave" ] := "06117473000150"
   hPixConfig[ "registrarPIX" ] := .T.
   hRoot[ "pixConfig" ] := hPixConfig
RETURN hb_jsonEncode( hRoot, .F. )

// Summary: Trata retorno do registro e salva o PDF (se vier).
// Obs: Mantem resiliente a campos ausentes (alguns podem vir null).
STATIC FUNCTION ProcessarRetornoBoleto( hJson )
   LOCAL cLinhaDigitavel := JsonGet( hJson, "LinhaDigitavel" )
   LOCAL cCodigoBarraNum := JsonGet( hJson, "CodigoBarraNumerico" )
   LOCAL cNumeroNoBanco := JsonGet( hJson, "NumeroNoBanco" )
   LOCAL xPDF := JsonGet( hJson, "PDFContent" )
   LOCAL hPDF := {=>}
   LOCAL lSuccess := .F.
   LOCAL cContent := ""
   LOCAL cMsg := ""
   LOCAL cErr := ""
   LOCAL cFile := ""
   LOCAL cDir := "BoletoPDF"

   IF cLinhaDigitavel == NIL
      RETURN "Campo 'LinhaDigitavel' nao encontrado no JSON de resposta"
   ENDIF

   IF cCodigoBarraNum == NIL
      RETURN "Campo 'CodigoBarraNumerico' nao encontrado no JSON de resposta"
   ENDIF

   IF cNumeroNoBanco == NIL
      RETURN "Campo 'NumeroNoBanco' nao encontrado no JSON de resposta"
   ENDIF

   IF ValType( cLinhaDigitavel ) == "C" .AND. ! Empty( AllTrim( cLinhaDigitavel ) )
      ? "LinhaDigitavel: " + cLinhaDigitavel
   ENDIF

   IF ValType( cCodigoBarraNum ) == "C" .AND. ! Empty( AllTrim( cCodigoBarraNum ) )
      ? "CodigoBarraNumerico: " + cCodigoBarraNum
   ENDIF

   IF ValType( cNumeroNoBanco ) == "C" .AND. ! Empty( AllTrim( cNumeroNoBanco ) )
      ? "NumeroNoBanco: " + cNumeroNoBanco
   ENDIF

   // PDFContent pode vir como Hash ou Array de pares (depende do decoder/retorno)
   IF ValType( xPDF ) == "H"
      hPDF := xPDF
   ELSEIF ValType( xPDF ) == "A"
      hPDF := JsonToHash( xPDF )
   ENDIF

   IF ValType( hPDF ) == "H"
      cContent := JsonGet( hPDF, "Content" )
      cMsg := JsonGet( hPDF, "Message" )
      lSuccess := JsonGet( hPDF, "Success" )

      IF ValType( lSuccess ) <> "L"
         lSuccess := .F.
      ENDIF

      IF lSuccess .AND. ValType( cContent ) == "C" .AND. ! Empty( AllTrim( cContent ) )
         IF ! IsDirectory( cDir )
            IF MakeDir( cDir ) <> 0
               RETURN "Falha ao criar pasta " + cDir
            ENDIF
         ENDIF
         
         cFile := cDir + "\Boleto_" + DToS( Date() ) + "_" + StrTran( Time(), ":", "" ) + ".pdf"
         cErr := SaveBase64ToFileEx( cContent, cFile, NIL, NIL, NIL, .F. )
         IF ! Empty( cErr )
            RETURN "Falha ao salvar PDF: " + cErr
         ELSE
            ? "PDF salvo em: " + cFile
         ENDIF
      ELSEIF ValType( cMsg ) == "C" .AND. ! Empty( AllTrim( cMsg ) )
         ? "PDFContent.Message: " + cMsg
      ENDIF
   ENDIF
RETURN ""

