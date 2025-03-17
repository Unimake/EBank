Attribute VB_Name = "RegistraBoleto"
Option Explicit

' -----------------------------------------------------------------------------------
' Função Principal: Registrar Boleto
' -----------------------------------------------------------------------------------
Public Sub RegistrarBoleto()
    Dim loHttp As Object
    Dim lcURL As String
    Dim lcJsonContent As String
    Dim lcResponse As String
    Dim jsonParser As Object
    Dim lcToken As String
    Dim lcExpiration As String

    ' Inicializa o objeto de Internet
    Set loHttp = CreateObject("MSXML2.XMLHTTP.6.0")

    ' Define a URL da API Produção
    lcURL = "https://unimake.app/auth/api/auth"
    ' Define a URL do SandBOX
    'lcURL = "https://auth.sandbox.unimake.software/api/auth"

    ' Cria o conteúdo da requisição no formato JSON
    lcJsonContent = "{""appId"": ""f1344af8039c41b4b5137c74fb4b4aca"", ""secret"": ""04fd5c84a4fe4ff7bb00458dc6fb0806""}"

    On Error GoTo ErrorHandler

    ' Configura a requisição
    loHttp.Open "POST", lcURL, False ' False para requisição síncrona
    loHttp.setRequestHeader "Content-Type", "application/json"

    ' Envia a requisição com o JSON
    loHttp.send lcJsonContent

    ' Verifica o status da resposta
    If loHttp.Status = 200 Then
        lcResponse = loHttp.responseText

        ' Decodifica o JSON
        Set jsonParser = JsonConverter.ParseJson(lcResponse)

        ' Verifica se houve erro na decodificação
        If Not jsonParser Is Nothing Then
            lcExpiration = jsonParser("expiration")
            lcToken = jsonParser("token")

            ' Chama a API para registrar o boleto
            ConsumirAPIRegistraBoleto lcToken
        Else
            MsgBox "Erro ao decodificar JSON.", vbCritical, "Erro de JSON"
        End If
    Else
        MsgBox "Falha na requisição: " & loHttp.Status & vbCrLf & loHttp.responseText, vbCritical, "Erro na Requisição"
    End If

Cleanup:
    Set loHttp = Nothing
    Exit Sub

ErrorHandler:
    MsgBox "Erro ao fazer a requisição: " & Err.Description, vbCritical, "Erro"
    Resume Cleanup
End Sub

' -----------------------------------------------------------------------------------
' Consome a API para registrar o boleto
' -----------------------------------------------------------------------------------
Public Sub ConsumirAPIRegistraBoleto(pToken As String)
    Dim loHttp As Object
    Dim lcURL As String
    Dim lcBearerToken As String
    Dim lcJsonContent As String
    Dim lcResponse As String
    Dim jsonParser As Object

    Set loHttp = CreateObject("MSXML2.XMLHTTP.6.0")

    lcBearerToken = pToken
    
    ' Define a URL da API Produção
    lcURL = "https://unimake.app/ebank/api/v1/boleto/registrar?configurationId=ZCKWGQ55LTDXKYYC"
    ' Define a URL do SandBOX
    'lcURL = "https://ebank.sandbox.unimake.software/api/v1/boleto/registrar?configurationId=ZCKWGQ55LTDXKYYC"

    ' Cria o conteúdo JSON para envio
    lcJsonContent = CreateJsonBoleto()

    On Error GoTo ErrorHandler

    ' Configura a requisição POST
    loHttp.Open "POST", lcURL, False
    loHttp.setRequestHeader "Authorization", "Bearer " & lcBearerToken
    loHttp.setRequestHeader "Content-Type", "application/json"

    ' Envia a requisição com o JSON
    loHttp.send lcJsonContent

    ' Verifica o status da resposta
    If loHttp.Status = 200 Then
        lcResponse = loHttp.responseText
        ConvertJsonToObject lcResponse
        MsgBox "Resposta da API: " & vbCrLf & lcResponse, vbInformation, "Sucesso"
    Else
        MsgBox "Falha na requisição: " & loHttp.Status & " " & loHttp.responseText, vbCritical, "Erro na Requisição"
    End If

Cleanup:
    Set loHttp = Nothing
    Exit Sub

ErrorHandler:
    MsgBox "Erro ao fazer a requisição: " & Err.Description, vbCritical, "Erro"
    Resume Cleanup
End Sub

' -----------------------------------------------------------------------------------
' Cria o JSON do Boleto
' -----------------------------------------------------------------------------------
Function CreateJsonBoleto() As String
    Dim json As Object
    Dim dataEmissao As Date: dataEmissao = Now
    Dim dataVencimento As Date: dataVencimento = DateAdd("d", 30, Now)
    Set json = JsonConverter.ParseJson("{}")
    
    ' Adicionando propriedades do JSON
    json.Add "especie", 2
    json.Add "numeroParcela", 1
    json.Add "numeroNoBanco", "00000008548"
    json.Add "numeroNaEmpresa", "000001-01"
    json.Add "vencimento", Format(dataVencimento, "yyyy-MM-dd")
    json.Add "emissao", Format(dataEmissao, "yyyy-MM-dd")
    json.Add "diasParaBaixaOuDevolucao", 0
    json.Add "tipoBaixaDevolucao", 1
    json.Add "valorIof", 0
    json.Add "valorNominal", 10
    json.Add "valorAbatimento", 0
    json.Add "testing", True

    ' Mensagens
    Dim mensagens As Object
    Set mensagens = JsonConverter.ParseJson("[]")
    mensagens.Add "JUROS DIARIOS SOBRE ATRASO PAG.R$ 0,02"
    json.Add "mensagens", mensagens

    ' Juros
    Dim jsonJuros As Object
    Set jsonJuros = JsonConverter.ParseJson("{}")
    jsonJuros.Add "tipo", 1
    jsonJuros.Add "data", Format(DateAdd("d", 1, dataVencimento), "yyyy-MM-dd")
    jsonJuros.Add "valor", 0.02
    json.Add "juros", jsonJuros

    ' Pagador
    Dim jsonEndereco As Object
    Set jsonEndereco = JsonConverter.ParseJson("{}")

    jsonEndereco.Add "rua", "RUA Lorem Ipsum"
    jsonEndereco.Add "numero", "001"
    jsonEndereco.Add "bairro", "Centro"
    jsonEndereco.Add "cep", "87711340"
    jsonEndereco.Add "cidade", "PARANAVAI"
    jsonEndereco.Add "uf", "PR"

    Dim jsonPagador As Object
    Set jsonPagador = JsonConverter.ParseJson("{}")
    jsonPagador.Add "endereco", jsonEndereco
    jsonPagador.Add "nome", "Marcelo de Souza"
    jsonPagador.Add "tipoInscricao", 1
    jsonPagador.Add "inscricao", "25806756807"

    json.Add "pagador", jsonPagador
    
    ' Configuração PDF
    Dim jsonPDFConfig As Object
    Set jsonPDFConfig = JsonConverter.ParseJson("{}")
    jsonPDFConfig.Add "tryGeneratePDF", True
    json.Add "pdfConfig", jsonPDFConfig

    ' Retorna o JSON formatado
    CreateJsonBoleto = JsonConverter.ConvertToJson(json, False)
End Function

' -----------------------------------------------------------------------------------
' Converte o JSON de Resposta para Objeto
' -----------------------------------------------------------------------------------
Sub ConvertJsonToObject(pJson As String)
    Dim jsonResponse As Object
    Dim outputPath As String
    Dim sucesso As Boolean
     
    Set jsonResponse = JsonConverter.ParseJson(pJson)
    

    If jsonResponse Is Nothing Then
        MsgBox "Erro ao decodificar JSON.", vbCritical, "Erro"
        Exit Sub
    End If

    ' Variáveis para armazenar os valores
    Dim lcCodigoBarraNumerico As String
    Dim lcLinhaDigitavel As String
    Dim lcNumeroNoBanco As String
    Dim lcPDFContent As String

    ' Extraindo valores básicos
    If jsonResponse.Exists("CodigoBarraNumerico") Then
        lcCodigoBarraNumerico = jsonResponse("CodigoBarraNumerico")
    End If
    If jsonResponse.Exists("LinhaDigitavel") Then
        lcLinhaDigitavel = jsonResponse("LinhaDigitavel")
    End If
    If jsonResponse.Exists("NumeroNoBanco") Then
        lcNumeroNoBanco = jsonResponse("NumeroNoBanco")
    End If

    ' Extraindo PDFContent
    If jsonResponse.Exists("PDFContent") Then
        Dim jsonPDF As Object
        Set jsonPDF = jsonResponse("PDFContent")
        
        If jsonPDF.Exists("Content") Then
            lcPDFContent = jsonPDF("Content")
            
            ' Salvar o PDF do boleto em uma pasta
            outputPath = "d:\testenfe\pdf\arquivo.pdf" ' Caminho onde o arquivo será salvo
            sucesso = DecodeBase64(lcPDFContent, outputPath)
            If sucesso Then
               MsgBox "Arquivo salvo com sucesso em: " & outputPath, vbInformation, "Sucesso"
            Else
               MsgBox "Falha ao salvar o arquivo.", vbCritical, "Erro"
            End If
        Else
            lcPDFContent = ""
        End If
    Else
        lcPDFContent = ""
    End If

    ' Exibir informações capturadas
    MsgBox "Código de Barras: " & lcCodigoBarraNumerico & vbCrLf & _
           "Linha Digitável: " & lcLinhaDigitavel & vbCrLf & _
           "Número no Banco: " & lcNumeroNoBanco, vbInformation, "Dados do Boleto"
End Sub


