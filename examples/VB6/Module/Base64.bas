Attribute VB_Name = "Base64"
Option Explicit

' -----------------------------------------------------------------------------------
' Função para Decodificar Base64 e Salvar um Arquivo Binário (Ex: PDF, Imagem, etc.)
' -----------------------------------------------------------------------------------
Public Function DecodeBase64(ByVal tcBase64 As String, ByVal tcOutputFile As String) As Boolean
    Dim binaryData() As Byte
    Dim fileNum As Integer
    On Error GoTo ErrorHandler

    ' Converte Base64 para binário
    binaryData = DecodeBase64ToBytes(tcBase64)

    ' Abre o arquivo para escrita binária
    fileNum = FreeFile
    Open tcOutputFile For Binary As #fileNum
    Put #fileNum, , binaryData
    Close #fileNum

    DecodeBase64 = True
    Exit Function

ErrorHandler:
    MsgBox "Erro ao decodificar Base64: " & Err.Description, vbCritical, "Erro"
    DecodeBase64 = False
End Function

' -----------------------------------------------------------------------------------
' Função para Converter Base64 em um Array de Bytes (Binário)
' -----------------------------------------------------------------------------------
Private Function DecodeBase64ToBytes(ByVal base64String As String) As Byte()
    Dim xml As Object
    Dim binaryData() As Byte
    Dim missingPadding As Integer

    On Error GoTo ErrorHandler

    ' Remover espaços e quebras de linha
    base64String = Replace(base64String, vbCr, "")
    base64String = Replace(base64String, vbLf, "")
    base64String = Trim(base64String)

    ' Certifica que a string tem tamanho múltiplo de 4 (Base64 exige isso)
    missingPadding = (4 - (Len(base64String) Mod 4)) Mod 4
    If missingPadding > 0 Then
        base64String = base64String & String(missingPadding, "=")
    End If

    ' Criar objeto XML para conversão Base64 -> Binário
    Set xml = CreateObject("MSXML2.DOMDocument.6.0")
    Set xml = xml.createElement("base64")
    xml.dataType = "bin.base64"
    xml.Text = base64String
    binaryData = xml.nodeTypedValue

    ' Retorna os bytes convertidos
    DecodeBase64ToBytes = binaryData

Cleanup:
    Set xml = Nothing
    Exit Function

ErrorHandler:
    MsgBox "Erro ao decodificar Base64: " & Err.Description, vbCritical, "Erro"
    Resume Cleanup
End Function
