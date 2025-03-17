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
              MsgBox lcExpiration, vbInformation, "Expiration"
           Else
              MsgBox "Chave 'expiration' n�o encontrada no JSON.", vbCritical, "Erro de JSON"
           End If
               
           If jsonParser.Exists("token") Then
              lcToken = jsonParser("token")
              MsgBox lcToken, vbInformation, "Token"
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
