Attribute VB_Name = "Utils"
Option Explicit

Private Declare Function MakeSureDirectoryPathExists Lib "imagehlp.dll" (ByVal lpPath As String) As Long
Private Declare Function ShellExecuteA Lib "shell32.dll" (ByVal hWnd As Long, ByVal lpOperation As String, ByVal lpFile As String, ByVal lpParameters As String, ByVal lpDirectory As String, ByVal nShowCmd As Long) As Long

Public hWnd As Long

Public Sub CreateFolder(ByVal pstrFolder As String)
    If Right$(pstrFolder, 1) <> "\" Then pstrFolder = pstrFolder & "\"
    MakeSureDirectoryPathExists pstrFolder
End Sub

Public Sub ShellExecute(ByVal pPath As String)
    ShellExecuteA hWnd, "open", pPath, vbNullString, "C:/", 0
End Sub

