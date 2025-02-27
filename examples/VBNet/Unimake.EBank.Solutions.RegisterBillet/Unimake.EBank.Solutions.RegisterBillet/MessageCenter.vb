Imports System.Console

Public Class MessageCenter

#Region "Private Constructors"

    Private Sub New()
    End Sub

#End Region

#Region "Private Methods"

    Private Shared Sub WriteLine(ByVal line As String)
        Dim size = 81 - line.Length
        Console.WriteLine($"| {line}{"".PadLeft(size)}|")
    End Sub

    Private Shared Sub WriteTitle(ByVal title As String)
        WriteLine($"        {title}")
    End Sub

#End Region

#Region "Internal Methods"

    Friend Shared Sub Log(ByVal pLog As String)
        Dim oldColor = ForegroundColor
        ForegroundColor = ConsoleColor.Blue
        Console.WriteLine(pLog)
        ForegroundColor = oldColor
    End Sub

#End Region

#Region "Public Methods"

    Public Shared Sub [Error](ByVal ex As Exception)
        Dim oldColor = ForegroundColor
        ForegroundColor = ConsoleColor.DarkRed
        Call DrawLine()
        WriteTitle("ERROR!!!")
        ForegroundColor = ConsoleColor.Red
        Dim message = ex.GetAllMessages()

        For Each item In Split(message, 80)
            WriteLine(item)
        Next

        Call DrawLine()
        Call NewLine()

        ForegroundColor = oldColor
    End Sub

    Public Shared Sub Alert(ByVal message As String, ByVal Optional title As String = "Aviso!")
        Dim oldColor = ForegroundColor
        ForegroundColor = ConsoleColor.DarkYellow

        Call DrawLine()
        WriteTitle(title)

        ForegroundColor = ConsoleColor.Yellow

        For Each item In Split(message, 80)
            WriteLine(item)
        Next

        Call DrawLine()
        Call NewLine()

        ForegroundColor = oldColor
    End Sub
    Public Shared Sub DrawLine()
        Console.WriteLine("_".PadLeft(84, "_"c))
    End Sub
    Public Shared Sub Message(ByVal message As String)
        Call DrawLine()

        For Each item In Split(message, 80)
            WriteLine(item)
        Next

        Call DrawLine()
        Call NewLine()
    End Sub

    Public Shared Sub Message(ByVal messages As IEnumerable(Of String))
        Call DrawLine()

        For Each msg In messages
            For Each item In Split(msg, 80)
                WriteLine(item)
            Next
        Next

        Call DrawLine()
        Call NewLine()
    End Sub
    Public Shared Sub NewLine()
        Console.WriteLine()
    End Sub

    Public Shared Function Split(str As String, chunkSize As Integer) As IEnumerable(Of String)
        str = str.Replace(Environment.NewLine, " ")
        Return If(str.Length <= chunkSize, (New String() {str}), Enumerable.Range(0, str.Length / chunkSize).Select(Function(i) str.Substring(i * chunkSize, chunkSize)))
    End Function

#End Region

End Class