* ------------------------------------------------------------------------------
* Unimake Software
* Exemplo de consumo das APIs Unimake
*
* Data: 10/02/2026 
* ------------------------------------------------------------------------------
FUNCTION Main()
   LOCAL aOpcoes, nOpcao

   aOpcoes := {}
   
   AAdd(aOpcoes, "Autenticar API")
   AAdd(aOpcoes, "Gerar PIX")
   AAdd(aOpcoes, "Consultar PIX")
   AAdd(aOpcoes, "Registrar Boleto")
   AAdd(aOpcoes, "Consultar Boleto")
  
   // Loop principal do menu.
   DO WHILE .T.
      Cls
      
      @ 1,2 Say "API Unimake for " + Version()
	  
      nOpcao := Achoice( 3, 2, 30, 80, aOpcoes)

      Cls

      DO CASE
         CASE LastKey() == 27
            EXIT

         CASE nOpcao == 1
            Auth()            

         CASE nOpcao == 2
            GeraPIX()

         CASE nOpcao == 3

         CASE nOpcao == 4
            RegistraBoleto()

         CASE nOpcao == 5
            ConsultaBoleto()
      ENDCASE        
   ENDDO
RETURN

// Autenticacao: exibe os campos retornados ou erro.
STATIC FUNCTION Auth()
   LOCAL hAuth := AutenticarAPI()

   IF ValType( hAuth ) == "H"
      ? "Token:"
      ? hAuth[ "token" ]
      ?
      ? "Expiration:"
      ? hAuth[ "expiration" ]
      ?
      ? "RefreshToken:"
      IF HHasKey( hAuth, "refreshToken" )
         ? hAuth[ "refreshToken" ]
      ELSE
         ?
      ENDIF
      ?
      ? "Type:"
      IF HHasKey( hAuth, "type" )
         ? hAuth[ "type" ]
      ELSE
         ?
      ENDIF
      ?
   ELSEIF ValType( hAuth ) == "O"
      ? FormatError( hAuth )
   ELSE
      ? "Erro: retorno inesperado"
   ENDIF
   Wait
RETURN

// Gera cobranca PIX: exibe QRCode/pixCopiaECola ou erro.
STATIC FUNCTION GeraPIX()
   LOCAL hPix := GerarPIX()

   IF ValType( hPix ) == "H"
      IF HHasKey( hPix, "QRCodeImage" )
         ? "Base64 QRCodeImage:"
         ? hPix[ "QRCodeImage" ]
         ?
         ? "QRCodePIX retornado em Base64 pela API foi salvo como arquivo/imagem na subpasta QRCodePIX."
         ?
         ?
      ENDIF

      IF HHasKey( hPix, "pixCopiaECola" )
         ? "pixCopiaECola: " + hPix[ "pixCopiaECola" ]
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
RETURN

STATIC FUNCTION RegistraBoleto()
   LOCAL hBoleto := RegistrarBoleto()

   IF ValType( hBoleto ) == "H"
      ? "Boleto registrado com sucesso:"
      ? hb_jsonEncode( hBoleto )
   ELSEIF ValType( hBoleto ) == "O"
      ? FormatError( hBoleto )
   ELSE
      ? "Erro: retorno inesperado"
   ENDIF
   Wait
RETURN

STATIC FUNCTION ConsultaBoleto()
   LOCAL xBoleto := ConsultarBoleto()

   IF ValType( xBoleto ) == "H" .OR. ValType( xBoleto ) == "A"
      ? "Consulta de boleto concluida:" 
      ? hb_jsonEncode( xBoleto )
   ELSEIF ValType( xBoleto ) == "O"
      ? FormatError( xBoleto )
   ELSE
      ? "Erro: retorno inesperado"
   ENDIF
   Wait
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