* -----------------------------------------------------------------------------------
* Enviar mensagens via uMessenger
* -----------------------------------------------------------------------------------
FUNCTION EnviarMensagemUMessenger()
	LOCAL cEndPointAPI, cInstanceName	
	LOCAL cJson, loError
	
    DO LOCFILE("PROGS\JSON.PRG")
    DO LOCFILE("PROGS\autenticarapi.PRG")    
	
  *	Define o Whatsapp que será usado para disparar a mensagem
    cInstanceName = "??????????????????????????" && Este item tem que pegar com o DEV do uMessenger, é gerado por ele.
    
  * Endpoint de produção do uMessenger
  * cEndPointAPI = "https://unimake.app/umessenger/"

  * Endpoint de homologação do uMessenger (SANDBOX)
    cEndPointAPI = "https://umessenger.sandbox.unimake.software/"

  * Complementar o EndPoint da API
	cEndPointAPI = cEndPointAPI+ "api/v1/Messages/Publish/" + cInstanceName
	
    TRY      
     * Montar o JSON da mensagem 
       cJson = BuildJsonMessagesPublish()

     * Enviar a mensagem
       HttpPostMessagePublish(cEndPointAPI, cJson)

	CATCH TO loError
        MESSAGEBOX("Erro ao fazer a requisição: " + loError.Message)
	ENDTRY
RETURN

* --------------------------------------------------
* Montar o JSON para envio da mensagem
* --------------------------------------------------
FUNCTION BuildJsonMessagesPublish()
   LOCAL cJson, cDestination, cText, existFile

   cDestination = "5544111111111"
   cText = "Teste de envio de mensagens via umessenger"
   
   cJson = '{'
   
   existFile = .T.
   IF existFile
      cJson = cJson + BuildJsonFilesMessagensPublish()
   ENDIF
   
   cJson = cJson + '"to": {"destination": "' + cDestination + '"},'
   cJson = cJson + '"text": "' + cText + '",'
   cJson = cJson + '"testing": false'
   cJson = cJson + '}'
RETURN cJson

* --------------------------------------------------
* Montar a parte do JSON que tem os arquivos 
* a serem enviados na mensagem
* --------------------------------------------------
FUNCTION BuildJsonFilesMessagensPublish()
   LOCAL cFileJson, cFileBase64, cFileName, cCaption, cMediaType, lcData
   LOCAL lEnviarDoisArquivos
      
 * Abrir o Json dos arquivos
   cFileJson = '"files": ['

 * Primeiro arquivo que vou enviar
   cFileName = "boleto.pdf" && Nome do arquivo mas sem caminho de pasta para não gerar erro no Json
   lcData   = FILETOSTR("D:\testenfe\pdf\boleto.pdf")  && binário
   cFileBase64 = STRCONV(lcData, 13)
   cMediaType  = "2"
   cCaption = "Boleto para pagamento"
   cFileJson = cFileJson + MontaFileJson(cFileName, cMediaType, cFileBase64, cCaption) 
   
   lEnviarDoisArquivos = .T.
   IF lEnviarDoisArquivos
      cFileJson = cFileJson + ',' &&Tem que somar a virgula para separar os arquivos dentro da Array do Json
      
    * Segundo arquivo que vou enviar (Pode enviar vários em uma mesma mensagem)
      cFileName = "nfsenacional_retornoconsultapdf.pdf" && Nome do arquivo mas sem caminho de pasta para não gerar erro no Json
      lcData   = FILETOSTR("D:\testenfe\pdf\nfsenacional_retornoconsultapdf.pdf")  && binário
      cFileBase64 = STRCONV(lcData, 13)
      cMediaType  = "2"
      cCaption = "NFSe modelo nacional"
      cFileJson = cFileJson + MontaFileJson(cFileName, cMediaType, cFileBase64, cCaption)
   ENDIF
   
 * Fechar o Json dos arquivos
   cFileJson = cFileJson + '],'
RETURN cFileJson

* --------------------------------------------------
* Auxiliar na montagem do JSON dos arquivos
* --------------------------------------------------
* cFileName = Nome do arquivo que está sendo enviado (Pode ser só o nome, sem pasta nem nada)
* cMediaType = Image    - 1
*              Document - 2
*              Audio    - 3
*              Video    - 4
* cFileBase64 = Aquivo que será eniado para o destinatário no formato Base64
* cCaption = Texto que acompanha como uma legenda no arquivo.
FUNCTION MontaFileJson(cFileName, cMediaType, cFileBase64, cCaption)
   LOCAL cFileJson
   
   cFileJson = '{'
   cFileJson = cFileJson + '"fileName": "' + cFileName + '",'
   cFileJson = cFileJson + '"mediaType": "' + cMediaType + '",'
   cFileJson = cFileJson + '"base64Content": "' + cFileBase64 + '",'
   cFileJson = cFileJson + '"caption": "' + cCaption + '"'
   cFileJson = cFileJson + '}'
RETURN cFileJson

* --------------------------------------------------
* Consumir a API - Enviar a mensagem
* --------------------------------------------------
FUNCTION HttpPostMessagePublish(cURL, cJson)
   LOCAL lOk, cToken, loHttp
   
   cToken = AutenticarAPI()
   If EMPTY(cToken)
      MESSAGEBOX("Token de autenticação inválido.")

      RETURN .F.
   ENDIF
   
 * Salvar o arquivo JSON em uma pasta qualquer só para analisar ele, se for necessário 
   DELETE FILE 'd:\testenfe\umessenger.json'
   StrToFile(cJson, 'd:\testenfe\umessenger.json', 0)
   
 * Exemplo de como fica o JSON
 * {
 *	 "files": [
 *		 {
 *			 "fileName": "boleto.pdf",
 *			 "mediaType": "2",
 *			 "base64Content": ".....",
 *			 "caption": "Boleto para pagamento"
 *		 },
 *		 {
 *			 "fileName": "nfsenacional_retornoconsultapdf.pdf",
 *			 "mediaType": "2",
 *			 "base64Content": "........",
 *			 "caption": "NFSe modelo nacional"
 *		 }
 *	 ],
 *	 "to": {
 *		 "destination": "5544999999999"
 *	 },
 *	 "text": "Teste de envio de mensagens via umessenger",
 *	 "testing": false
 * }
   
 * Configura a requisição POST
   loHttp = CREATEOBJECT("MSXML2.XMLHTTP.6.0") 
   loHttp.Open("POST", cURL, .F.) && False para requisição síncrona
   loHttp.setRequestHeader("Authorization", "Bearer " + cToken)
   loHttp.setRequestHeader("Content-Type", "application/json")

 * Envia a requisição com o JSON
   loHttp.Send(cJson)

 * Verifica o status da resposta
   IF loHttp.Status = 200 &&Mensagem enviada
      lcResponse = loHttp.ResponseText
      lOk = .T.
	     
      MESSAGEBOX("Resposta da API:" + CHR(13) + lcResponse)
   ELSE && Ocorreu falha no envio da mensagem
      lOk = .F.
      MESSAGEBOX("Falha na requisição: " + TRANSFORM(loHttp.Status) + " " + loHttp.ResponseText)
   ENDIF
RETURN lOk
