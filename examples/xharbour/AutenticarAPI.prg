// Summary: Autentica na API Unimake via WinHTTP e retorna o token de acesso
// Return: Hash com JSON retornado ou objeto de erro

#include "tip.ch"

FUNCTION AutenticarAPI()
   LOCAL cConfigPath := "config.json"
   LOCAL cConfigRaw := ""
   LOCAL hConfig := {=>}
   LOCAL nConfigDecoded := 0
   LOCAL xAmbiente := NIL
   LOCAL nAmbiente := NIL
   LOCAL cAppId := NIL
   LOCAL cSecret := NIL
   LOCAL cUrl := ""
   LOCAL cJson := ""
   LOCAL oHttp := NIL
   LOCAL cResp := ""
   LOCAL hJson := {=>}
   LOCAL cToken := NIL
   LOCAL xExpiration := NIL
   LOCAL nStatus := 0
   LOCAL nDecoded := 0
   LOCAL oErr := NIL
   LOCAL bOldError
	LOCAL CRLF := CHR(13) + CHR(10)
	LOCAL cHttpErr := ""
	LOCAL cStatusText := ""

   // Fluxo principal com BREAK para cair no tratamento de erros.
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
      cAppId    := JsonGet( hConfig, "AppID" )
      cSecret   := JsonGet( hConfig, "Secret" )

      IF ValType( xAmbiente ) == "C"
         nAmbiente := Val( xAmbiente )
      ELSEIF ValType( xAmbiente ) == "N"
         nAmbiente := xAmbiente
      ENDIF

      IF nAmbiente == NIL .OR. Empty( cAppId ) .OR. Empty( cSecret )
         BREAK
      ENDIF

      // Define a URL com base no ambiente escolhido.
      cUrl := IF( nAmbiente == 1, ;
         "https://unimake.app/auth/api/auth", ;
         "https://auth.sandbox.unimake.software/api/auth" )

      // Monta o JSON esperado pela API de autenticacao.
      cJson := '{"appId":"' + cAppId + '","secret":"' + cSecret + '"}'

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
         oHttp:SetRequestHeader( "Content-Type", "application/json" )
         oHttp:SetRequestHeader( "Accept", "application/json" )

         // Envia
         oHttp:Send( cJson )

         nStatus := oHttp:Status
         cResp   := oHttp:ResponseText

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

      // 200 indica sucesso na autenticacao.
      IF nStatus <> 200
         BREAK
      ENDIF

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

      IF ValType( hJson ) <> "H"
         BREAK
      ENDIF

      // Campos obrigatorios esperados no retorno.
      cToken      := JsonGet( hJson, "token" )
      xExpiration := JsonGet( hJson, "expiration" )

      IF cToken == NIL .OR. xExpiration == NIL
         BREAK
      ENDIF

   RECOVER
   END SEQUENCE

   // ==============================
   // Tratamento de erros
   // ==============================

   IF Empty( cConfigRaw )
      RETURN ErrorNew( "", 1, 1000, "Falha ao ler " + cConfigPath )
   ENDIF

   IF ValType( hConfig ) <> "H"
      RETURN ErrorNew( "", 1, 1007, "Erro ao decodificar JSON de configuracao" )
   ENDIF

   IF nAmbiente == NIL
      RETURN ErrorNew( "", 1, 1008, "Campo 'Ambiente' invalido" )
   ENDIF

   IF Empty( cAppId )
      RETURN ErrorNew( "", 1, 1009, "Campo 'AppID' nao encontrado" )
   ENDIF

   IF Empty( cSecret )
      RETURN ErrorNew( "", 1, 1010, "Campo 'Secret' nao encontrado" )
   ENDIF

   // Erros de infraestrutura HTTP/WinHTTP.
   IF oHttp == NIL
      RETURN ErrorNew( "", 1, 1001, "Falha ao criar WinHTTP" + cHttpErr )
   ENDIF

   IF nStatus <> 200
      IF Empty( cHttpErr ) .AND. nStatus == 0
         cHttpErr := CRLF + "Erro WinHTTP: sem resposta HTTP; verifique URL/DNS/SSL/proxy"
      ENDIF
      RETURN ErrorNew( "", 1, 1003, ;
         "Falha HTTP: " + LTrim( Str( nStatus ) ) + ;
         IIF( Empty( cStatusText ), "", " " + cStatusText ) + ;
         CRLF + "URL: " + cUrl + ;
         IIF( Empty( cResp ), "", CRLF + cResp ) + cHttpErr )
   ENDIF

   IF ValType( hJson ) <> "H"
      RETURN ErrorNew( "", 1, 1004, "Erro ao decodificar JSON de resposta" )
   ENDIF

   IF cToken == NIL
      RETURN ErrorNew( "", 1, 1005, "Campo 'token' nao encontrado" )
   ENDIF

   IF xExpiration == NIL
      RETURN ErrorNew( "", 1, 1006, "Campo 'expiration' nao encontrado" )
   ENDIF
RETURN hJson

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