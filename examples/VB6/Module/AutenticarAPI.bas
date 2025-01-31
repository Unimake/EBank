Attribute VB_Name = "AutenticaAPI"
Option Explicit

' -----------------------------------------------------------------------------------
' Autenticar API Unimake
' -----------------------------------------------------------------------------------
Public Sub AutenticarAPI()
    Dim loHttp As Object
    Dim lcURL As String
    Dim lcJsonContent As String
    Dim lcResponse As String
    Dim lcToken As String
    Dim lcExpiration As String
    Dim jsonParser As Object

    ' Inicializa o objeto XMLHTTP
    Set loHttp = CreateObject("MSXML2.XMLHTTP.6.0")

    ' Define a URL da API
    lcURL = "https://unimake.app/auth/api/auth"

    ' Cria o conteúdo da requisição no formato JSON
    lcJsonContent = "{""appId"": ""124494fcf65441c2abd36d1e08ab4f45"",""secret"": ""a9ebaee34da7473c9f5126214514a804""}"

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
           If jsonParser.exists("expiration") Then
              lcExpiration = jsonParser("expiration")
              MsgBox lcExpiration, vbInformation, "Expiration"
           Else
              MsgBox "Chave 'expiration' não encontrada no JSON.", vbCritical, "Erro de JSON"
           End If
               
           If jsonParser.exists("token") Then
              lcToken = jsonParser("token")
              MsgBox lcToken, vbInformation, "Token"
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
