

Namespace ErrorMessages
    Public Class clsErrorWindow

        ''' <summary>
        ''' Invalid Game Cartridge
        ''' </summary>
        ''' <param name="eMessage">The error message.</param>
        ''' <param name="Critical">If we want to crash the Application all together.</param>
        Public Sub New(ByVal eMessage As String, ByVal Optional Critical As Boolean = True)
            Dim intReturnValue As Integer
            intReturnValue = MessageBox.Show(eMessage, "Error Box", MessageBoxButtons.OKCancel, MessageBoxIcon.Error, MessageBoxDefaultButton.Button1, MessageBoxOptions.DefaultDesktopOnly)
            Select Case intReturnValue
                'Case DialogResult.OK
                'Case DialogResult.Cancel
                Case Else
                    'All we need is the user to click through
            End Select

            If Not Critical Then
                'We selected non critical error, eg: game file was bad try another one. dont kill the app
            Else
                'Kill the application
                Application.Exit() 'Should work just not in the main runtime start
                'Environment.Exit(-1)
                'End
            End If
        End Sub
    End Class
End Namespace
