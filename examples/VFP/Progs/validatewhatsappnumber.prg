* -----------------------------------------------------------------------------------
* Validar o número do Whatsapp
* -----------------------------------------------------------------------------------
FUNCTION ValidateWhatsappNumber()
	LOCAL cEndPointAPI, cInstanceName	
	LOCAL cJson, loError, cNumber, lcResponse, cRetorno
	
    DO LOCFILE("PROGS\JSON.PRG")
    DO LOCFILE("PROGS\autenticarapi.PRG")    
	
  *	Define o Whatsapp que será usado para disparar a mensagem
    cInstanceName = "??????????????????????????"
    
  * Endpoint de produção do uMessenger
  * cEndPointAPI = "https://unimake.app/umessenger/"

  * Endpoint de homologação do uMessenger (SANDBOX)
    cEndPointAPI = "https://umessenger.sandbox.unimake.software/"

  * Complementar o EndPoint da API
	cEndPointAPI = cEndPointAPI+ "api/v1/Chat/WhatsappNumbers/" + cInstanceName
	
    TRY      
     * Numero do whatsapp que será validado
       cNumber = "5544111111111" && Número sem separadores: sem traços, parenteses ou qualquer outro caracter que não seja numérico

     * Montar o JSON da mensagem 
       cJson = BuildJsonWhatsappNumbers(cNumber)

     * Enviar a mensagem
       cRetorno = HttpPostWhatsappNumbers(cEndPointAPI, cJson)
       IF cRetorno = "true"
          MESSAGEBOX("O número " + cNumber + " é um WhatsApp")
       ELSE
          IF cRetorno = "false"
             MESSAGEBOX("O número " + cNumber + " não é um WhatsApp")
          Endif
       ENDIF

	CATCH TO loError
        MESSAGEBOX("Erro ao fazer a requisição: " + loError.Message)
	ENDTRY
RETURN

* -----------------------------------------------------------------
* Nome........: BuildJsonWhatsappNumbers
* Descrição...: Monta o JSON para validação do número do WhatsApp
* Parâmetros..: cNumber - Número do WhatsApp a ser validado
* Retorno.....: String com o JSON formatado
* -----------------------------------------------------------------
FUNCTION BuildJsonWhatsappNumbers(cNumber)
   LOCAL cJson 
   LOCAL nMessagingService
   
   nMessagingService = 0 && Whatsapp

   cJson = '{'
   cJson = cJson + '"messagingService": "' + AllTrim(Str(nMessagingService,1)) + '",'
   cJson = cJson + '"testing": false,'
   cJson = cJson + '"numbers": ['
   cJson = cJson + '"' + AllTrim(cNumber) + '"'
   cJson = cJson +']'
   cJson = cJson + '}'
RETURN cJson

* ----------------------------------------------------------------
* Nome........: HttpPostWhatsappNumbers
* Descrição...: Executa o POST para validar numero Whatsapp
* Parâmetros..: cURL - URL da API
*               cJson - JSON com os dados da mensagem
* Retorno.....: .T. se o envio for bem-sucedido, .F. caso contrário
* ----------------------------------------------------------------
FUNCTION HttpPostWhatsappNumbers(cURL, cJson)
   LOCAL cRetorno, cToken, loHttp, loRoot, loItem, lExists, cID

   SET PROCEDURE TO "Progs\json.prg" ADDITIVE

   cToken = AutenticarAPI()
   If EMPTY(cToken)
      MESSAGEBOX("Token de autenticação inválido.")
      
      cRetorno = "erro"

      RETURN cRetorno
   ENDIF   
  
 * Configura a requisição POST
   loHttp = CREATEOBJECT("MSXML2.XMLHTTP.6.0") 
   loHttp.Open("POST", cURL, .F.) && False para requisição síncrona
   loHttp.setRequestHeader("Authorization", "Bearer " + cToken)
   loHttp.setRequestHeader("Content-Type", "application/json")

 * Envia a requisição com o JSON
   loHttp.Send(cJson)
   
 * Salvar o retorno do JSON para que analisemos
   DELETE FILE 'd:\testenfe\validawhatsapp.json'
   StrToFile(loHttp.ResponseText, 'd:\testenfe\validawhatsapp.json', 0)

 * Verifica o status da resposta
   IF loHttp.Status = 200 .or. loHttp.Status = 204 .or. loHttp.Status == 207 &&Mensagem enviada
      cRetorno = "false"
      
      loRoot = json_decode(loHttp.ResponseText)
      
      * Verifica erros na decodificação
      IF ISNULL(loRoot)
         MESSAGEBOX("Erro ao decodificar JSON: " + json_getErrorMsg(), 16, "Erro")
         RETURN .F.
      ENDIF
      
      IF ALEN(loRoot.Array) > 0
         loItem = loRoot.Array[1]
         cId = loItem._id
         lExists = loItem._exists
      
         IF lExists 
            cRetorno = "true"
         ENDIF
      ENDIF
      
   ELSE && Ocorreu falha no consumo da API
      cRetorno = "erro"
      MESSAGEBOX("Falha na requisição: " + TRANSFORM(loHttp.Status) + " " + loHttp.ResponseText)
   ENDIF   
RETURN cRetorno