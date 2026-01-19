Imports System.Windows.Forms
Imports System.Windows.Forms.VisualStyles

Friend Module Program
    <STAThread()>
    Public Sub Main()
        ' Set these before any forms are created
        Application.SetCompatibleTextRenderingDefault(False)
        Application.VisualStyleState = VisualStyleState.NoneEnabled

        ' Run your main form
        Application.Run(New Form1())
    End Sub
End Module