Module Module1

    Sub Main()
        Dim fso
        fso = CreateObject("JWSCLCom.JwCoSid")
        If fso.InitByName("", "Christian") Then
            'blaat
        End If
        fso = Nothing
    End Sub

End Module
