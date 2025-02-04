Attribute VB_Name = "Base64"
Option Explicit

' -----------------------------------------------------------------------------------
' Fun��o para Decodificar Base64 e Salvar um Arquivo Bin�rio (Ex: PDF, Imagem, etc.)
' -----------------------------------------------------------------------------------
Public Function DecodeBase64(ByVal tcBase64 As String, ByVal tcOutputFile As String) As Boolean
    Dim binaryData() As Byte
    Dim fileNum As Integer
    On Error GoTo ErrorHandler

    ' Converte Base64 para bin�rio
    binaryData = DecodeBase64ToBytes(tcBase64)

    ' Abre o arquivo para escrita bin�ria
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
' Fun��o para Converter Base64 em um Array de Bytes (Bin�rio)
' -----------------------------------------------------------------------------------
Private Function DecodeBase64ToBytes(ByVal base64String As String) As Byte()
    Dim xml As Object
    Dim binaryData() As Byte
    Dim missingPadding As Integer

    On Error GoTo ErrorHandler

    ' Remover espa�os e quebras de linha
    base64String = Replace(base64String, vbCr, "")
    base64String = Replace(base64String, vbLf, "")
    base64String = Trim(base64String)

    ' Certifica que a string tem tamanho m�ltiplo de 4 (Base64 exige isso)
    missingPadding = (4 - (Len(base64String) Mod 4)) Mod 4
    If missingPadding > 0 Then
        base64String = base64String & String(missingPadding, "=")
    End If

    ' Criar objeto XML para convers�o Base64 -> Bin�rio
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
