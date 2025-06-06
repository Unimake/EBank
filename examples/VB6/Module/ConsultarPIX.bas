Attribute VB_Name = "ConsultaPIX"
Option Explicit

' -----------------------------------------------------------------------------------
' Consultar PIX
' -----------------------------------------------------------------------------------
Public Sub ConsultarPIX()
    Dim loHttp As Object
    Dim lcURL As String
    Dim lcJsonContent As String
    Dim lcResponse As String
    Dim lcToken As String
    Dim lcExpiration As String
    Dim jsonParser As Object

    ' Inicializa o objeto XMLHTTP
    Set loHttp = CreateObject("MSXML2.XMLHTTP.6.0")

    ' Define a URL da API Produ��o
    lcURL = "https://unimake.app/auth/api/auth"
    ' Define a URL do SandBOX
    'lcURL = "https://auth.sandbox.unimake.software/api/auth"

    ' Cria o conte�do da requisi��o no formato JSON
    lcJsonContent = "{""appId"": ""f1344af8039c41b4b5137c74fb4b4aca"",""secret"": ""04fd5c84a4fe4ff7bb00458dc6fb0806""}"

    On Error GoTo ErrorHandler

    ' Configura a requisi��o
    loHttp.Open "POST", lcURL, False ' False para requisi��o s�ncrona
    loHttp.setRequestHeader "Content-Type", "application/json"

    ' Envia a requisi��o com o JSON
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
              MsgBox "Chave 'expiration' n�o encontrada no JSON.", vbCritical, "Erro de JSON"
           End If
               
           If jsonParser.Exists("token") Then
              lcToken = jsonParser("token")
              
              ConsumirAPIConsultar (lcToken)
           Else
              MsgBox "Chave 'token' n�o encontrada no JSON.", vbCritical, "Erro de JSON"
           End If
        Else
           MsgBox "Erro ao interpretar JSON.", vbCritical, "Erro"
        End If
    Else
        MsgBox "Falha na requisi��o: " & loHttp.Status & vbCrLf & loHttp.responseText, vbCritical, "Erro na Requisi��o"
    End If

Cleanup:
    ' Libera os objetos
    Set loHttp = Nothing
    Set jsonParser = Nothing
    Exit Sub

ErrorHandler:
    MsgBox "Erro ao fazer a requisi��o: " & Err.Description, vbCritical, "Erro"
    Resume Cleanup
End Sub

Public Sub ConsumirAPIConsultar(lcToken As String)
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
    Dim items As Object
    Dim endToEndId As String
    Dim primeiroItem As Object
    Dim valor As Double

    ' Define a URL da API Produ��o
    cUrl = "https://unimake.app/ebank/api/v1/boleto/Consultar?configurationId=ZCKWGQ55LTDXKYYC"
    ' Define a URL do SandBOX
    'lcURL = "https://ebank.sandbox.unimake.software/api/v1/boleto/Consultar?configurationId=ZCKWGQ55LTDXKYYC"

    ' Gerar JSON para a requisi��o
    lcJsonContent = CriarJSONConsultarPIX()

    ' Enviar requisi��o para API
    Set loHttp = CreateObject("MSXML2.XMLHTTP.6.0")
    loHttp.Open "POST", cUrl, False ' False para requisi��o s�ncrona
    loHttp.setRequestHeader "Authorization", "Bearer " + lcToken
    loHttp.setRequestHeader "Content-Type", "application/json"
    
    loHttp.send (lcJsonContent)
    
    ' Verifica o status da resposta
    If loHttp.Status = 200 Then
       lcResponse = loHttp.responseText
       
       ' Interpretar o JSON de resposta
       Set jsonParser = JsonConverter.ParseJson(lcResponse)
       
       If Not jsonParser Is Nothing Then
          If jsonParser.Exists("Items") Then
             Set items = jsonParser("Items")
             
             If items.Count > 0 Then
                Set primeiroItem = items(1)
                
                If primeiroItem.Exists("endToEndId") Then
                   endToEndId = primeiroItem("endToEndId")
                Else
                   endToEndId = "N/A"
                End If
                MsgBox "endToEndId: " & endToEndId
                
                If primeiroItem.Exists("valor") Then
                   valor = primeiroItem("valor")
                Else
                   valor = 0
                End If
                MsgBox "Valor do PIX: " & valor
             Else
                MsgBox "Nenhum item encontrado no JSON."
                MsgBox "PIX ainda n�o foi pago"
             End If
          Else
             MsgBox "Chave 'Items' n�o encontrada no JSON."
          End If
       Else
          MsgBox "Erro ao interpretar JSON.", vbCritical, "Erro"
       End If

    Else
       MsgBox "Falha na requisi��o: " & loHttp.Status & vbCrLf & loHttp.responseText, vbCritical, "Erro na Requisi��o"
    End If
End Sub

Function CriarJSONConsultarPIX() As String
    Dim json As Object
    Set json = JsonConverter.ParseJson("{}") ' Criando objeto JSON

    json.Add "testing", True
    json.Add "txId", "txid2025013117171234567894567894564"
    json.Add "startDate", "2025-01-20T08:00:00.275Z"
    json.Add "endDate", "2025-01-23T23:51:01.275Z"

    If json Is Nothing Then
       MsgBox "Erro: O objeto JSON n�o foi criado corretamente.", vbCritical, "Erro de JSON"
       Exit Function
    End If
    
    ' Retorna o JSON formatado
    CriarJSONConsultarPIX = JsonConverter.ConvertToJson(json, False)
End Function

