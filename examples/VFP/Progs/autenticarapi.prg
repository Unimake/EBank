* -----------------------------------------------------------------------------------
* Autenticar API Unimake
* -----------------------------------------------------------------------------------
FUNCTION AutenticarAPI()
	LOCAL loHttp, cEndPoint, lcJsonContent, lcResponse, lcHeader
	LOCAL loDecodedJSON, lcToken, lcExpiration
	
	DO LOCFILE("PROGS\JSON.PRG")
	
	* Inicializa o objeto de Internet
	loHttp = CREATEOBJECT("MSXML2.XMLHTTP.6.0")

	* Define a URL da API de Produção
	*cEndPoint = "https://unimake.app/auth/api/auth"
	
  * Definir o EndPoint de homologação (SANDBOX)
	cEndPoint = "https://auth.sandbox.unimake.software/api/auth"

	* Cria o conteúdo da requisição no formato JSON
	lcJsonContent = '{"appId": "????????????????????????????????","secret": "????????????????????????????????"}' &&AppID e Secret tem que ser adquirido junto ao DEV do eBank ou uMessenger

    lcToken = ""
	TRY
	    * Configura a requisição
	    loHttp.Open("POST", cEndPoint, .F.) && False para requisição síncrona
	    loHttp.setRequestHeader("Content-Type", "application/json")

	    * Envia a requisição com o JSON
	    loHttp.Send(lcJsonContent)

	    * Verifica o status da resposta
	    IF loHttp.Status = 200
	        lcResponse = loHttp.ResponseText
		
		  * Decodifica o JSON
		    loDecodedJSON = json_decode(lcResponse)
			
		  * Verifica se houve erros ao decodificar
		    IF EMPTY(json_getErrorMsg())
  			 * Extrai o valor do token
  			   lcExpiration = loDecodedJSON.get("expiration")
  			   lcToken = loDecodedJSON.get("token")
            ELSE
             * Exibe o erro
               MESSAGEBOX("Erro ao decodificar JSON:", json_getErrorMsg())
            ENDIF			
	    ELSE
   	        MESSAGEBOX("Falha na requisição: " + TRANSFORM(loHttp.Status) + CHR(13) + loHttp.ResponseText)
	    ENDIF

	CATCH TO loError
        MESSAGEBOX("Erro ao fazer a requisição: " + loError.Message)
	ENDTRY

	* Libera o objeto
	RELEASE loHttp
RETURN lcToken