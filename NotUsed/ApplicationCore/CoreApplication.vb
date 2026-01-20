
Namespace CoreApplicationLayer

    Public Enum ReturnCode
        FAIL
        OK
        NO_FILE = -1
    End Enum


    Class Core
        Private m_timepoint1 As DateTime
        Private m_timepoint2 As DateTime
        Private fLastElapsed As Single

        Private bConsoleSuspendTime As Boolean

        ' State of keyboard
        Private pKeyNewState(nKeyboardButtons) As Boolean
        Private pKeyOldState(nKeyboardButtons) As Boolean
        Private pKeyboardState(nKeyboardButtons) As HWButton

        ' State of mouse
        Private pMouseNewState(nMouseButtons) As Boolean
        Private pMouseOldState(nMouseButtons) As Boolean
        Private pMouseState(nMouseButtons) As HWButton


        Public Sub Update()
            m_timepoint2 = DateTime.Now
            Dim elapsedTime As TimeSpan = m_timepoint2 - m_timepoint1
            m_timepoint1 = m_timepoint2

            Dim fElapsedTime As Single = CSng(elapsedTime.TotalSeconds)
            fLastElapsed = fElapsedTime

            If bConsoleSuspendTime Then
                fElapsedTime = 0.0
            End If


        End Sub


    End Class

End Namespace