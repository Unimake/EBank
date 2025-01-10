* -----------------------------------------------------------------------------------
* Autenticar API Unimake
* -----------------------------------------------------------------------------------
FUNCTION AutenticarAPI()
	LOCAL loHttp, lcURL, lcJsonContent, lcResponse, lcHeader
	LOCAL loDecodedJSON, lcToken, lcExpiration
	
	DO LOCFILE("PROGS\JSON.PRG")
	
	* Inicializa o objeto de Internet
	loHttp = CREATEOBJECT("MSXML2.XMLHTTP.6.0")

	* Define a URL da API
	lcURL = "https://unimake.app/auth/api/auth"

	* Cria o conte�do da requisi��o no formato JSON
	lcJsonContent = '{"appId": "64ec6abf1ae54bceb58862b79d002c55","secret": "03be0c8b3a4a42c094a38005e20b1576"}'

	TRY
	    * Configura a requisi��o
	    loHttp.Open("POST", lcURL, .F.) && False para requisi��o s�ncrona
	    loHttp.setRequestHeader("Content-Type", "application/json")

	    * Envia a requisi��o com o JSON
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
  			   MESSAGEBOX(lcExpiration)
  			    			   
  			   lcToken = loDecodedJSON.get("token")
               MESSAGEBOX(lcToken)  			   
            ELSE
             * Exibe o erro
               MESSAGEBOX("Erro ao decodificar JSON:", json_getErrorMsg())
            ENDIF			
	    ELSE
   	        MESSAGEBOX("Falha na requisi��o: " + TRANSFORM(loHttp.Status) + CHR(13) + loHttp.ResponseText)
	    ENDIF

	CATCH TO loError
        MESSAGEBOX("Erro ao fazer a requisi��o: " + loError.Message)
	ENDTRY

	* Libera o objeto
	RELEASE loHttp
RETURN