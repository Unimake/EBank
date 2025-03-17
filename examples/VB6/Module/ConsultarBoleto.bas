Attribute VB_Name = "ConsultaBoleto"
Option Explicit

' -----------------------------------------------------------------------------------
' Função Principal: Consultar Boleto
' -----------------------------------------------------------------------------------
Public Sub ConsultarBoleto()
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
            ConsumirAPIConsultarBoleto lcToken
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
' Consome a API para consultar o boleto
' -----------------------------------------------------------------------------------
Public Sub ConsumirAPIConsultarBoleto(pToken As String)
    Dim loHttp As Object
    Dim lcURL As String
    Dim lcBearerToken As String
    Dim lcJsonContent As String
    Dim lcResponse As String
    Dim jsonParser As Object

    Set loHttp = CreateObject("MSXML2.XMLHTTP.6.0")

    lcBearerToken = pToken
    
    ' Define a URL da API Produção
    lcURL = "https://unimake.app/ebank/api/v1/boleto/consultar?configurationId=ZCKWGQ55LTDXKYYC"
    ' Define a URL do SandBOX
    'lcURL = "https://ebank.sandbox.unimake.software/api/v1/boleto/consultar?configurationId=ZCKWGQ55LTDXKYYC"

    ' Cria o conteúdo JSON para envio
    lcJsonContent = CreateJsonConsultaBoleto()

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
Function CreateJsonConsultaBoleto() As String
    Dim json As Object
    Set json = JsonConverter.ParseJson("{}") ' Criando objeto JSON principal

    ' Propriedades principais
    json.Add "testing", True
    json.Add "dataEmissaoInicial", "2025-02-01"
    json.Add "dataEmissaoFinal", "2025-02-03"

    ' Retorna o JSON formatado corretamente
    CreateJsonConsultaBoleto = JsonConverter.ConvertToJson(json, False)
End Function


' -----------------------------------------------------------------------------------
' Converte o JSON de Resposta para Objeto
' -----------------------------------------------------------------------------------
Sub ConvertJsonToObject(pJson As String)
    Dim jsonResponse As Object
    Set jsonResponse = JsonConverter.ParseJson(pJson)

    If jsonResponse Is Nothing Then
        MsgBox "Erro ao decodificar JSON.", vbCritical, "Erro"
        Exit Sub
    End If

    ' Verifica se jsonResponse é uma coleção (array de boletos)
    If Not TypeName(jsonResponse) = "Collection" Then
        MsgBox "Formato do JSON inválido. Esperado um array de boletos.", vbCritical, "Erro"
        Exit Sub
    End If

    ' Percorre todos os boletos do JSON
    Dim boleto As Object
    Dim i As Integer
    i = 1 ' Contador para exibição

    For Each boleto In jsonResponse
        ' Variáveis para armazenar os valores do boleto atual
        Dim lcCodigoBarras As String
        Dim lcSituacao As String
        Dim lcTipoLiquidacao As String
        Dim lcLinhaDigitavel As String
        Dim lcPDFContent As String
        Dim lcValor As String
        Dim lcValorDesconto As String
        Dim lcValorJuros As String
        Dim lcValorLiquidado As String

        ' Função para verificar se o valor é Null e retornar uma string vazia
        lcCodigoBarras = SafeGetJsonValue(boleto, "CodigoBarras")
        lcSituacao = SafeGetJsonValue(boleto, "Situacao")
        lcTipoLiquidacao = SafeGetJsonValue(boleto, "TipoLiquidacao")
        lcLinhaDigitavel = SafeGetJsonValue(boleto, "LinhaDigitavel")
        lcValor = SafeGetJsonValue(boleto, "Valor")
        lcValorDesconto = SafeGetJsonValue(boleto, "ValorDesconto")
        lcValorJuros = SafeGetJsonValue(boleto, "ValorJuros")
        lcValorLiquidado = SafeGetJsonValue(boleto, "ValorLiquidado")

        ' Extraindo PDFContent -> Content
        If boleto.Exists("PdfContent") Then
            Dim jsonPDF As Object
            Set jsonPDF = boleto("PdfContent")
            lcPDFContent = SafeGetJsonValue(jsonPDF, "Content")
        Else
            lcPDFContent = "N/A"
        End If

        ' Exibir informações capturadas de cada boleto
        MsgBox "Boleto #" & i & vbCrLf & _
               "Código de Barras: " & lcCodigoBarras & vbCrLf & _
               "Situação: " & lcSituacao & vbCrLf & _
               "Tipo de Liquidação: " & lcTipoLiquidacao & vbCrLf & _
               "Linha Digitável: " & lcLinhaDigitavel & vbCrLf & _
               "Valor: R$ " & lcValor & vbCrLf & _
               "Desconto: R$ " & lcValorDesconto & vbCrLf & _
               "Juros: R$ " & lcValorJuros & vbCrLf & _
               "Valor Liquidado: R$ " & lcValorLiquidado, _
               vbInformation, "Dados do Boleto"

        i = i + 1 ' Incrementa o contador
    Next boleto
End Sub


Function SafeGetJsonValue(jsonObj As Object, key As String) As String
    On Error Resume Next ' Se houver erro, evita falha
    Dim value As Variant
    If jsonObj.Exists(key) Then
        value = jsonObj(key)
        If IsNull(value) Or IsEmpty(value) Then
            SafeGetJsonValue = "null" ' Substitui valores Null/Empty por "0.00"
        Else
            SafeGetJsonValue = CStr(value)
        End If
    Else
        SafeGetJsonValue = "0.00"
    End If
End Function



