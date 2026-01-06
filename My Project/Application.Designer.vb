Option Strict On
Option Explicit On

Imports Microsoft.VisualBasic.ApplicationServices

Namespace My
    <Global.System.Diagnostics.DebuggerNonUserCodeAttribute()>
    Partial Friend Class MyApplication
        Inherits WindowsFormsApplicationBase

        Public Sub New()
            MyBase.New(AuthenticationMode.Windows)
            Me.IsSingleInstance = False
            Me.EnableVisualStyles = True
            Me.SaveMySettingsOnExit = True
            Me.ShutdownStyle = ShutdownMode.AfterMainFormCloses
        End Sub

        ' Ensure a startup form is provided to avoid NoStartupFormException.
        Protected Overrides Sub OnCreateMainForm()
            ' Use the VB default form instance. Change Form1 to your actual startup form if different.
            Me.MainForm = Global.Nintendo.Form1
        End Sub

    End Class
End Namespace