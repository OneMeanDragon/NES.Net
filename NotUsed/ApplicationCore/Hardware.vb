
Namespace CoreApplicationLayer

    Public Module Hardware

        Public Const nMouseButtons As Byte = 5
        Public Const nKeyboardButtons As Byte = 256

        Public Structure HWButton
            Public bPressed As Boolean
            Public bReleased As Boolean
            Public bHeld As Boolean
            Public Sub New(Optional pressed As Boolean = False, Optional released As Boolean = False, Optional held As Boolean = False)
                bPressed = pressed
                bReleased = released
                bHeld = held
            End Sub
        End Structure

        Public Sub ScanHardware(pKeys As HWButton(), pStateOld As Boolean(), pStateNew As Boolean(), nKeyCount As UInt32)
            For i As UInt32 = 0 To nKeyCount - 1
                pKeys(i).bPressed = False
                pKeys(i).bReleased = False

                If pStateNew(i) <> pStateOld(i) Then
                    If pStateNew(i) Then
                        pKeys(i).bPressed = Not pKeys(i).bHeld
                        pKeys(i).bHeld = True
                    Else
                        pKeys(i).bReleased = True
                        pKeys(i).bHeld = False
                    End If
                End If

                pStateOld(i) = pStateNew(i)
            Next
        End Sub

    End Module

End Namespace
