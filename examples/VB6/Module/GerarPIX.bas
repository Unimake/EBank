Attribute VB_Name = "GeraPIX"
Option Explicit

' -----------------------------------------------------------------------------------
' Gerar PIX
' -----------------------------------------------------------------------------------
Public Sub GerarPIX()
    Dim loHttp As Object
    Dim lcURL As String
    Dim lcJsonContent As String
    Dim lcResponse As String
    Dim lcToken As String
    Dim lcExpiration As String
    Dim jsonParser As Object

    ' Inicializa o objeto XMLHTTP
    Set loHttp = CreateObject("MSXML2.XMLHTTP.6.0")

    ' Define a URL da API Produção
    lcURL = "https://unimake.app/auth/api/auth"
    ' Define a URL do SandBOX
    'lcURL = "https://auth.sandbox.unimake.software/api/auth"

    ' Cria o conteúdo da requisição no formato JSON
    lcJsonContent = "{""appId"": ""f1344af8039c41b4b5137c74fb4b4aca"",""secret"": ""04fd5c84a4fe4ff7bb00458dc6fb0806""}"

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
        
        If Not jsonParser Is Nothing Then
           If jsonParser.Exists("expiration") Then
              lcExpiration = jsonParser("expiration")
           Else
              MsgBox "Chave 'expiration' não encontrada no JSON.", vbCritical, "Erro de JSON"
           End If
               
           If jsonParser.Exists("token") Then
              lcToken = jsonParser("token")
              
              ConsumirAPICobranca (lcToken)
           Else
              MsgBox "Chave 'token' não encontrada no JSON.", vbCritical, "Erro de JSON"
           End If
        Else
           MsgBox "Erro ao interpretar JSON.", vbCritical, "Erro"
        End If
    Else
        MsgBox "Falha na requisição: " & loHttp.Status & vbCrLf & loHttp.responseText, vbCritical, "Erro na Requisição"
    End If

Cleanup:
    ' Libera os objetos
    Set loHttp = Nothing
    Set jsonParser = Nothing
    Exit Sub

ErrorHandler:
    MsgBox "Erro ao fazer a requisição: " & Err.Description, vbCritical, "Erro"
    Resume Cleanup
End Sub

Public Sub ConsumirAPICobranca(lcToken As String)
    Dim lok As Boolean
    Dim lcJsonContent As String
    Dim lcResponse As String
    Dim jsonParser As Object
    Dim pixCopiaECola As String
    Dim QRCodeImage As String
    Dim cMensagem As String
    Dim cUrl As String
    Dim qrCodePIX As String
    Dim loHttp As Object

    ' Define a URL da API Produção
    cUrl = "https://unimake.app/ebank/api/v1/pix/Cobranca?configurationId=ZCKWGQ55LTDXKYYC"
    ' Define a URL do SandBOX
    'lcURL = "https://ebank.sandbox.unimake.software/api/v1/pix/Cobranca?configurationId=ZCKWGQ55LTDXKYYC"

    ' Gerar JSON para a requisição
    lcJsonContent = CriarJSONCobrancaPIX()

    ' Enviar requisição para API
    Set loHttp = CreateObject("MSXML2.XMLHTTP.6.0")
    loHttp.Open "POST", cUrl, False ' False para requisição síncrona
    loHttp.setRequestHeader "Authorization", "Bearer " + lcToken
    loHttp.setRequestHeader "Content-Type", "application/json"
    
    loHttp.send (lcJsonContent)
    
    ' Verifica o status da resposta
    If loHttp.Status = 200 Then
       lcResponse = loHttp.responseText
       
       ' Interpretar o JSON de resposta
       Set jsonParser = JsonConverter.ParseJson(lcResponse)
       
       If Not jsonParser Is Nothing Then
          If jsonParser.Exists("QRCodeImage") Then
             QRCodeImage = jsonParser("QRCodeImage")
             MsgBox "QrCodeImage: " & QRCodeImage
          Else
             MsgBox "Chave 'QRCodeImage' não encontrada no JSON.", vbCritical, "Erro de JSON"
          End If
               
          If jsonParser.Exists("pixCopiaECola") Then
             pixCopiaECola = jsonParser("pixCopiaECola")
             MsgBox "QrCodeImage: " & pixCopiaECola
          Else
             MsgBox "Chave 'pixCopiaECola' não encontrada no JSON.", vbCritical, "Erro de JSON"
          End If
       Else
          MsgBox "Erro ao interpretar JSON.", vbCritical, "Erro"
       End If

    Else
       MsgBox "Falha na requisição: " & loHttp.Status & vbCrLf & loHttp.responseText, vbCritical, "Erro na Requisição"
    End If
End Sub

Function CriarJSONCobrancaPIX() As String
    Dim json As Object
    Set json = JsonConverter.ParseJson("{}") ' Criando objeto JSON

    ' Cabeçalho
    json.Add "chave", "unimake@unimake.com.br"
    json.Add "gerarQRCode", True
    json.Add "testing", True
    json.Add "txId", "txid2025013117171234567894567894564"
                      
    json.Add "solicitacaoPagador", "Recebimento PIX"

    ' QR Code
    Dim jsonQRCode As Object
    Set jsonQRCode = JsonConverter.ParseJson("{}")
    jsonQRCode.Add "height", 256
    jsonQRCode.Add "imageFormat", "Jpeg"
    jsonQRCode.Add "quality", 100
    jsonQRCode.Add "width", 256
    json.Add "qrCodeConfig", jsonQRCode

    ' Calendário
    Dim jsonCalendario As Object
    Set jsonCalendario = JsonConverter.ParseJson("{}")
    jsonCalendario.Add "criacao", Date
    jsonCalendario.Add "expiracao", 1800 ' 1800 segundos = 30 minutos
    jsonCalendario.Add "validadeAposVencimento", 0 ' Trata-se da quantidade de dias corridos após , em que a cobrança poderá ser paga.
    json.Add "calendario", jsonCalendario

    ' Devedor
    Dim jsonDevedor As Object
    Set jsonDevedor = JsonConverter.ParseJson("{}")
    jsonDevedor.Add "inscricao", "00000000000" ' CPF ou CPJ do pagador
    jsonDevedor.Add "nome", Left(Trim("Fulano teste"), 30)
    jsonDevedor.Add "logradouro", Left(Trim("Rua de teste"), 30)
    jsonDevedor.Add "cidade", Left(Trim("Paranavaí"), 20)
    jsonDevedor.Add "cep", "87706070"
    jsonDevedor.Add "uf", "PR"
    json.Add "devedor", jsonDevedor

    ' Valor
    Dim jsonValor As Object
    Set jsonValor = JsonConverter.ParseJson("{}")
    jsonValor.Add "original", 10# ' Simulando valor
    json.Add "valor", jsonValor

    If json Is Nothing Then
       MsgBox "Erro: O objeto JSON não foi criado corretamente.", vbCritical, "Erro de JSON"
       Exit Function
    End If
    
    ' Retorna o JSON formatado
    CriarJSONCobrancaPIX = JsonConverter.ConvertToJson(json, False)
End Function

