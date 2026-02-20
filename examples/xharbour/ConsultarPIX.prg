#include "tip.ch"

// ============================================================================
// ConsultarPIX.prg
// Padrao baseado no ConsultarBoleto.prg e RegistrarBoleto.prg:
// - Autentica via AutenticarAPI() e usa o mesmo padrao de retorno (Hash ou Error)
// - JSON: hb_jsonEncode / hb_jsonDecode + JsonToHash/JsonGet
// - HTTP: WinHttp.WinHttpRequest.5.1 (sincrono)
// - Tratamento de erro: ErrorNew + ErrText
// ============================================================================

// Summary: Consulta PIX e retorna o hash/array da resposta.
// Return value: Array/Hash com os dados do JSON retornado ou objeto Error.
FUNCTION ConsultarPIX()
   LOCAL hAuth := AutenticarAPI()
   LOCAL cToken := ""

   // Autentica e extrai o token; qualquer erro aborta aqui.
   IF ValType( hAuth ) == "H"
      IF HHasKey( hAuth, "token" )
         cToken := hAuth[ "token" ]
      ELSE
         RETURN ErrorNew( "", 1, 5001, "Campo 'token' nao encontrado na autenticacao" )
      ENDIF
   ELSEIF ValType( hAuth ) == "O"
      RETURN hAuth
   ELSE
      RETURN ErrorNew( "", 1, 5002, "Retorno inesperado na autenticacao" )
   ENDIF
RETURN ConsumirAPIConsultarPIX( cToken )

// Summary: Envia a consulta PIX e retorna o JSON decodificado.
// Params:
//   cToken -> Token Bearer da autenticacao.
// Return: Hash/Array com os dados da consulta ou objeto Error.
FUNCTION ConsumirAPIConsultarPIX( cToken )
   LOCAL cConfigPath := "config.json"
   LOCAL cConfigRaw := ""
   LOCAL hConfig := {=>}
   LOCAL nConfigDecoded := 0
   LOCAL xAmbiente := NIL
   LOCAL nAmbiente := NIL
   LOCAL cConfigurationID := NIL
   LOCAL cUrl := ""
   LOCAL cJson := CriarJSONConsultarPIX()
   LOCAL oHttp := NIL
   LOCAL cResp := ""
   LOCAL hJson := NIL
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
         "https://unimake.app/ebank/api/v1/pix/Consultar?configurationId=", ;
         "https://ebank.sandbox.unimake.software/api/v1/pix/Consultar?configurationId=" ) + cConfigurationID

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
   hJson := NIL
   nDecoded := hb_jsonDecode( cResp, @hJson )

   IF nDecoded == 0 .OR. ( ValType( hJson ) <> "H" .AND. ValType( hJson ) <> "A" )
      hJson := hb_jsonDecode( cResp )
      IF ValType( hJson ) == "A"
         // ok
      ELSEIF ValType( hJson ) == "H"
         // ok
      ELSE
         hJson := JsonToHash( hJson )
      ENDIF
   ENDIF

   // ==============================
   // Tratamento de erros (padrao ConsultarBoleto)
   // ==============================
   IF Empty( cConfigRaw )
      RETURN ErrorNew( "", 1, 5009, "Falha ao ler " + cConfigPath )
   ENDIF

   IF ValType( hConfig ) <> "H"
      RETURN ErrorNew( "", 1, 5010, "Erro ao decodificar JSON de configuracao" )
   ENDIF

   IF nAmbiente == NIL
      RETURN ErrorNew( "", 1, 5011, "Campo 'Ambiente' invalido" )
   ENDIF

   IF Empty( cConfigurationID )
      RETURN ErrorNew( "", 1, 5012, "Campo 'ConfigurationID' nao encontrado" )
   ENDIF

   IF oHttp == NIL
      RETURN ErrorNew( "", 1, 5003, "Falha ao criar WinHTTP" + cHttpErr )
   ENDIF

   IF nStatus <> 200
      IF Empty( cHttpErr ) .AND. nStatus == 0
         cHttpErr := CRLF + "Erro WinHTTP: sem resposta HTTP; verifique URL/DNS/SSL/proxy"
      ENDIF
      RETURN ErrorNew( "", 1, 5005, ;
         "Falha HTTP: " + LTrim( Str( nStatus ) ) + ;
         IIF( Empty( cStatusText ), "", " " + cStatusText ) + ;
         CRLF + "URL: " + cUrl + ;
         IIF( Empty( cResp ), "", CRLF + cResp ) + cHttpErr )
   ENDIF

   IF ValType( hJson ) <> "H" .AND. ValType( hJson ) <> "A"
      RETURN ErrorNew( "", 1, 5006, "Erro ao decodificar o JSON de resposta" )
   ENDIF

   // ==============================
   // Pos-processamento
   // ==============================
   cProcErr := ProcessarRetornoConsultaPIX( hJson )
   IF ValType( cProcErr ) == "C" .AND. ! Empty( cProcErr )
      RETURN ErrorNew( "", 1, 5013, cProcErr )
   ENDIF
RETURN hJson

// Summary: Monta o JSON da consulta PIX.
FUNCTION CriarJSONConsultarPIX()
   LOCAL hRoot := {=>}

   hRoot[ "testing" ] := .T.
   hRoot[ "txId" ] := "txid2025013117171234567894567894564"
   hRoot[ "startDate" ] := "2025-01-20T08:00:00.275Z"
   hRoot[ "endDate" ] := "2025-01-23T23:51:01.275Z"
RETURN hb_jsonEncode( hRoot, .F. )

// Summary: Trata o retorno da consulta PIX e exibe dados basicos.
STATIC FUNCTION ProcessarRetornoConsultaPIX( xJson )
   LOCAL aItems := {}
   LOCAL xItems := NIL
   LOCAL nI := 0
   LOCAL hItem := {=>}
   LOCAL cEndToEndId := ""
   LOCAL cValor := ""
   LOCAL CRLF := CHR(13) + CHR(10)
   LOCAL cInfo := ""

   IF ValType( xJson ) == "A"
      aItems := xJson
   ELSEIF ValType( xJson ) == "H"
      xItems := JsonGet( xJson, "Items" )
      IF ValType( xItems ) <> "A"
         xItems := JsonGet( xJson, "items" )
      ENDIF
      IF ValType( xItems ) == "A"
         aItems := xItems
      ENDIF
   ELSE
      RETURN "Resposta JSON invalida"
   ENDIF

   IF Len( aItems ) == 0
      RETURN "Nenhum item encontrado no JSON. PIX ainda nao foi pago"
   ENDIF

   FOR nI := 1 TO Len( aItems )
      hItem := aItems[ nI ]
      IF ValType( hItem ) == "A"
         hItem := JsonToHash( hItem )
      ENDIF
      IF ValType( hItem ) <> "H"
         LOOP
      ENDIF

      cEndToEndId := ValorToString( JsonGet( hItem, "endToEndId" ) )
      cValor := ValorToString( JsonGet( hItem, "valor" ) )

      cInfo := "Item #" + LTrim( Str( nI ) ) + CRLF + ;
         "endToEndId: " + cEndToEndId + CRLF + ;
         "Valor do PIX: " + cValor

      ? cInfo
   NEXT
RETURN ""

// Summary: Converte numero para string de exibicao.
STATIC FUNCTION ValorToString( xVal )
   IF ValType( xVal ) == "C"
      RETURN xVal
   ELSEIF ValType( xVal ) == "N"
      RETURN LTrim( Str( xVal, 15, 2 ) )
   ENDIF
RETURN ""

