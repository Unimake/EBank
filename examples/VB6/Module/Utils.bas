Attribute VB_Name = "Utils"
Option Explicit

Private Declare Function MakeSureDirectoryPathExists Lib "imagehlp.dll" (ByVal lpPath As String) As Long

Public Sub CreateFolder(ByVal pstrFolder As String)
    If Right$(pstrFolder, 1) <> "\" Then pstrFolder = pstrFolder & "\"
    MakeSureDirectoryPathExists pstrFolder
End Sub
