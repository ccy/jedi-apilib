Module Module1

    Private Declare Sub JwOleRaise Lib "..\..\..\..\..\JWSCLCom.dll" _
     Alias "JwOleRaise" (ByVal Res As Integer)

    Private Declare Function JwHasException Lib "..\..\..\..\..\JWSCLCom.dll" _
     Alias "JwHasException" (ByVal Res As Integer) As Boolean


    Sub Main()
        JwHasException(0)
        JwOleRaise(0)
        Dim fso
        fso = CreateObject("JWSCLCom.JwCoSid")
        Try
            JwOleRaise(fso.InitByName("", "Christian"))
            Console.Write(fso.StringSid)
        Catch ex As Exception
            Console.Write(ex.Message)
        End Try

        Console.ReadKey()

        fso = Nothing
    End Sub

End Module
