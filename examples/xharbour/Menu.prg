* ------------------------------------------------------------------------------
* Unimake Software
* Exemplo de consumo das APIs Unimake
*
* Data: 10/02/2026 
* ------------------------------------------------------------------------------
FUNCTION Main()
   LOCAL aOpcoes, nOpcao
   LOCAL hAuth
   LOCAL hPix
   LOCAL cToken := ""
   LOCAL nExpiration := 0
   LOCAL cRefreshToken := ""
   LOCAL cType := ""
   PUBLIC nAmbiente := 1 // 1 - Produção, 2 - Sandbox

   aOpcoes := {}
   
   AAdd(aOpcoes, "Autenticar API")
   AAdd(aOpcoes, "Gerar PIX")
  
   DO WHILE .T.
      Cls
      
      @ 1,2 Say "API Unimake for " + Version()
	  
      nOpcao := Achoice( 3, 2, 30, 80, aOpcoes)

      Cls

      DO CASE
         CASE LastKey() = 27
              EXIT

         CASE nOpcao = 1
              hAuth := AutenticarAPI()

              IF ValType( hAuth ) == "H"
                 cToken := hAuth[ "token" ]
                 nExpiration := hAuth[ "expiration" ]

                 IF HHasKey( hAuth, "refreshToken" )
                    cRefreshToken := hAuth[ "refreshToken" ]
                 ELSE
                    cRefreshToken := ""
                 ENDIF

                 IF HHasKey( hAuth, "type" )
                    cType := hAuth[ "type" ]
                 ELSE
                    cType := ""
                 ENDIF
                 
                 ? "Token:"
                 ? cToken
                 ?
                 ? "Expiration:"
                 ? nExpiration
                 ?
                 ? "RefreshToken:"
                 ? cRefreshToken
                 ?
                 ? "Type:"
                 ? cType
                 ?
              ELSEIF ValType( hAuth ) == "O"
                 ? FormatError( hAuth )
              ELSE
                 ? "Erro: retorno inesperado"
              ENDIF

              Wait

         CASE nOpcao = 2
              hPix := GerarPIX()

              IF ValType( hPix ) == "H"
                 IF HHasKey( hPix, "QRCodeImage" )
                    ? "QRCodeImage:"
                    ? hPix[ "QRCodeImage" ]
                    ?
                 ENDIF

                 IF HHasKey( hPix, "pixCopiaECola" )
                    ? "pixCopiaECola:"
                    ? hPix[ "pixCopiaECola" ]
                    ?
                 ENDIF

                 IF ! HHasKey( hPix, "QRCodeImage" ) .AND. ! HHasKey( hPix, "pixCopiaECola" )
                    ? "Retorno OK, mas sem campos esperados."
                 ENDIF
              ELSEIF ValType( hPix ) == "O"
                 ? FormatError( hPix )
              ELSE
                 ? "Erro: retorno inesperado"
              ENDIF

              Wait
      ENDCASE        
   ENDDO
RETURN

// Summary: Formata o erro retornado pela API para exibir detalhes.
// Params:
//   oErr -> objeto de erro
// Return: string amigavel
STATIC FUNCTION FormatError( oErr )
   LOCAL cMsg := "Erro"

   IF oErr == NIL
      RETURN cMsg
   ENDIF

   IF __objHasMsg( oErr, "Description" )
      cMsg += ": " + oErr:Description
   ENDIF

   IF __objHasMsg( oErr, "SubSystem" )
      cMsg += " [" + oErr:SubSystem + "]"
   ENDIF

   IF __objHasMsg( oErr, "GenCode" )
      cMsg += " Code=" + LTrim( Str( oErr:GenCode ) )
   ENDIF

   IF __objHasMsg( oErr, "SubCode" )
      cMsg += " SubCode=" + LTrim( Str( oErr:SubCode ) )
   ENDIF

   IF __objHasMsg( oErr, "Operation" )
      cMsg += " Op=" + oErr:Operation
   ENDIF

RETURN cMsg