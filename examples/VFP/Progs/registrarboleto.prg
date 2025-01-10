* -----------------------------------------------------------------------------------
* Registrar boleto
* -----------------------------------------------------------------------------------
FUNCTION RegistrarBoleto()
	LOCAL loHttp, lcURL, lcJsonContent, lcResponse, lcHeader
    LOCAL loDecodedJSON, lcToken, lcExpiration
	
    DO LOCFILE("PROGS\JSON.PRG")	
	
	* Inicializa o objeto de Internet
	loHttp = CREATEOBJECT("MSXML2.XMLHTTP.6.0")

	* Define a URL da API em Produção
    *lcURL = "https://unimake.app/auth/api/auth"

	* Define a URL da API em Homologação (Sandbox)
    lcURL = "https://auth.sandbox.unimake.software/api/auth"

	* Cria o conteúdo da requisição no formato JSON
	lcJsonContent = '{"appId": "64ec6abf1ae54bceb58862b79d002c55","secret": "03be0c8b3a4a42c094a38005e20b1576"}'

	TRY
	    * Configura a requisição
	    loHttp.Open("POST", lcURL, .F.) && False para requisição síncrona
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
  			  * Extrai o valor do token e o tempo para expirar o token
  			  lcExpiration = loDecodedJSON.get("expiration")
  			  lcToken = loDecodedJSON.get("token")
			  
			  ConsumirAPIRegistraBoleto(lcToken)
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
RETURN

FUNCTION ConsumirAPIRegistraBoleto(pToken)
   LOCAL lcURL, loHttp, lcBearerToken
   LOCAL lcConfigurationId
   LOCAL lcJsonContent
   
   loHttp = CREATEOBJECT("MSXML2.XMLHTTP.6.0")
   
   lcBearerToken = pToken
   
   * Definir configurationId (Este código é fornecido pela Unimake)
   lcConfigurationId = "ZCKWGQ55LTDXKYYC"
   
   * Definir URL em produção
   *lcURL = "https://unimake.app/ebank/api/v1/boleto/registrar?configurationId=" + lcConfigurationId
   
   * Definir URL em homologação (SandoBox)
   lcURL = "https://ebank.sandbox.unimake.software/api/v1/boleto/registrar?configurationId=" + lcConfigurationId
   
   * Cria o conteúdo JSON para envio
   lcJsonContent = CreateJsonBoleto()
   
   TRY
	  * Configura a requisição POST
	  loHttp.Open("POST", lcURL, .F.) && False para requisição síncrona
	  loHttp.setRequestHeader("Authorization", "Bearer " + lcBearerToken)
	  loHttp.setRequestHeader("Content-Type", "application/json")

	  * Envia a requisição com o JSON
	  loHttp.Send(lcJsonContent)

	  * Verifica o status da resposta
	  IF loHttp.Status = 200
	     lcResponse = loHttp.ResponseText
	     
	     ConvertJsonToObject(lcResponse)
	     	      
	     MESSAGEBOX("Resposta da API:" + CHR(13) + lcResponse)
	  ELSE
	     MESSAGEBOX("Falha na requisição: " + TRANSFORM(loHttp.Status) + " " + loHttp.ResponseText)
	  ENDIF
   CATCH TO loError2
	    MESSAGEBOX("Erro ao fazer a requisição: " + loError2.Message)
   ENDTRY
RETURN

FUNCTION CreateJsonBoleto()
    SET PROCEDURE TO "Progs\json.prg" ADDITIVE

	* Criação do objeto principal para o JSON
	LOCAL loJson, loRoot, loJuros, loPagador, loEnderecoPagador

	loJson = CREATEOBJECT("json")
	loRoot = CREATEOBJECT("myObj")

	* Propriedades principais do JSON
	loRoot.set("especie", 2)
	loRoot.set("numeroParcela", 1)
	loRoot.set("numeroNoBanco", "00000008548")
	loRoot.set("numeroNaEmpresa", "000001-01")
	loRoot.set("vencimento", "2025-02-10")
	loRoot.set("emissao", "2025-01-10")
	loRoot.set("diasParaBaixaOuDevolucao", 0)
	loRoot.set("tipoBaixaDevolucao", 1)
	loRoot.set("valorIof", 0)
	loRoot.set("valorNominal", 10)
	loRoot.set("valorAbatimento", 0)
	loRoot.set("testing", .T.)
	
    * Configuração do array de mensagens
    DIMENSION laMensagens[1]
    laMensagens[1] = "JUROS DIARIOS SOBRE ATRASO PAG.R$ 0,02"
    loRoot.set("mensagens", @laMensagens)

	* Juros
	loJuros = CREATEOBJECT("myObj")
	loJuros.set("tipo", 1)
	loJuros.set("data", "2025-03-10")
	loJuros.set("valor", 0.02)
	loRoot.set("juros", loJuros)

	* Pagador
	loPagador = CREATEOBJECT("myObj")
	loEnderecoPagador = CREATEOBJECT("myObj")
	loEnderecoPagador.set("rua", "RUA Lorem Ipsum")
	loEnderecoPagador.set("numero", "001")
	loEnderecoPagador.set("bairro", "Centro")
	loEnderecoPagador.set("cep", "87711340")
	loEnderecoPagador.set("cidade", "PARANAVAI")
	loEnderecoPagador.set("uf", "PR")
	loPagador.set("endereco", loEnderecoPagador)
	loPagador.set("nome", "Marcelo de Souza")
	loPagador.set("tipoInscricao", 1)
	loPagador.set("inscricao", "25806756807")
	loRoot.set("pagador", loPagador)

	* Converte o objeto para JSON
	LOCAL lcJson
	lcJson = json_encode(loRoot)	
	
    _CLIPTEXT = lcJson
RETURN lcJson

* esta função não está conseguindo ler o retorno corretamente. Propriedades do json que estão sendo retornados como null ele não lê. Tem que testar em produção para ver se vai passar bem.
FUNCTION ConvertJsonToObject(pJson)
    SET PROCEDURE TO "Progs\json.prg" ADDITIVE

	* Decodifica o JSON em um objeto
	LOCAL loRoot
	loRoot = json_decode(pJson)

	* Verifica erros na decodificação
	IF ISNULL(loRoot)
	    MESSAGEBOX("Erro ao decodificar JSON: " + json_getErrorMsg(), 16, "Erro")
	    RETURN
	ENDIF

	* Variáveis para armazenar os valores
	LOCAL lcCodigoBarraNumerico, lcLinhaDigitavel, lcNumeroNoBanco
	LOCAL lcPDFContent, lcPDFMessage, llPDFSuccess
	LOCAL lcQRCodeImage, llQRCodeSuccess, lcQRCodeText
	LOCAL lcFichaAceiteContent, lcFichaAceiteMessage, llFichaAceiteSuccess

	* Extraindo as propriedades
	lcCodigoBarraNumerico = loRoot.get("CodigoBarraNumerico")
	lcLinhaDigitavel = loRoot.get("LinhaDigitavel")
	lcNumeroNoBanco = loRoot.get("NumeroNoBanco")

	* PDFContent
	LOCAL loPDFContent
	loPDFContent = loRoot.get("PDFContent")
	IF VARTYPE(loPDFContent) = "O"
	    lcPDFContent = loPDFContent.get("Content")
	    lcPDFMessage = loPDFContent.get("Message")
	    llPDFSuccess = loPDFContent.get("Success")

	    IF llPDFSuccess
	       * Caminho para salvar o arquivo PDF
	       lcOutputFile = "d:\testenfe\boleto.pdf"

	       * Decodifica o Base64 e salva como PDF
	       IF DecodeBase64(lcPDFContent, lcOutputFile)
	          MESSAGEBOX("PDF salvo com sucesso em: " + lcOutputFile, 64, "Sucesso")
	       ELSE
	          MESSAGEBOX("Falha ao salvar o PDF.", 16, "Erro")
	       ENDIF	       
		ENDIF    
	ENDIF

	* QrCodeContent
*LOCAL loQRCodeContent
*loQRCodeContent = loRoot.get("QrCodeContent")
*IF VARTYPE(loQRCodeContent) = "O"
*    lcQRCodeImage = loQRCodeContent.get("Image")
*    llQRCodeSuccess = loQRCodeContent.get("Success")
*    lcQRCodeText = loQRCodeContent.get("Text")
*ENDIF

	* FichaAceiteContent
*LOCAL loFichaAceiteContent
*loFichaAceiteContent = loRoot.get("FichaAceiteContent")
*IF VARTYPE(loFichaAceiteContent) = "O"
*    lcFichaAceiteContent = loFichaAceiteContent.get("Content")
*    lcFichaAceiteMessage = loFichaAceiteContent.get("Message")
*    llFichaAceiteSuccess = loFichaAceiteContent.get("Success")
*ENDIF
RETURN

* Função para decodificar Base64
* Esta função não funcionou para criar o PDF, está gerando com falha, vamos ter que pesquisar algo melhor
FUNCTION DecodeBase64(tcBase64, tcOutputFile)
    LOCAL lcDecoded, lnBinaryFile
    LOCAL lReturn
    
    TRY
        * Decodifica o conteúdo Base64
        lcDecoded = STRCONV(tcBase64, 14) && Decodificar de Base64 para binário

        * Salva no arquivo especificado
        lnBinaryFile = FCREATE(tcOutputFile)
        IF lnBinaryFile > 0
            FWRITE(lnBinaryFile, lcDecoded)
            FCLOSE(lnBinaryFile)
            lReturn = .T. && Sucesso
        ELSE
            lReturn = .F. && Falha ao criar o arquivo
        ENDIF
    CATCH TO loException
        MESSAGEBOX("Erro ao decodificar Base64: " + loException.Message, 16, "Erro")
        lReturn = .F.
    ENDTRY
RETURN lReturn