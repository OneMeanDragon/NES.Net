Namespace NintendoEntertainmentSystem
    Public Class OscPulse
        Public frequency As Double = 0
        Public dutycycle As Double = 0
        Public amplitude As Double = 1
        Public harmonics As Double = 20
        Private ReadOnly PI As Double = Math.PI

        ' Custom approx sin from the C++ code
        Private Function ApproxSin(t As Double) As Double
            Dim j As Double = t * 0.15915
            j = j - Math.Floor(j)
            Return 20.785 * j * (j - 0.5) * (j - 1.0)
        End Function

        Public Function Sample(t As Double) As Double
            Dim a As Double = 0
            Dim b As Double = 0
            Dim p As Double = dutycycle * 2.0 * PI

            For n As Double = 1 To harmonics - 1
                Dim c As Double = n * frequency * 2.0 * PI * t
                a += -ApproxSin(c) / n
                b += -ApproxSin(c - p * n) / n
            Next

            Return (2.0 * amplitude / PI) * (a - b)
        End Function
    End Class

    Public Structure Sequencer
        Public sequence As UInt32
        Public new_sequence As UInt32
        Public timer As UInt16
        Public reload As UInt16
        Public output As Byte

        Public Delegate Sub Manipulator(ByRef s As UInt32)

        Public Function Clock(bEnable As Boolean, funcManip As Manipulator) As Byte
            If bEnable Then
                ' Underflow check for 0xFFFF equivalent
                If timer = 0 Then
                    timer = reload
                    funcManip(sequence)
                    output = CByte(sequence And &H1)
                Else
                    timer -= 1
                End If
            End If
            Return output
        End Function
    End Structure

    Public Class Envelope
        Public start As Boolean = False
        Public disable As Boolean = False
        Public divider_count As UInt16 = 0
        Public volume As UInt16 = 0
        Public output As UInt16 = 0
        Public decay_count As UInt16 = 0

        Public Sub Clock(bLoop As Boolean)
            If Not start Then
                If divider_count = 0 Then
                    divider_count = volume
                    If decay_count = 0 Then
                        If bLoop Then decay_count = 15
                    Else
                        decay_count -= 1
                    End If
                Else
                    divider_count -= 1
                End If
            Else
                start = False
                decay_count = 15
                divider_count = volume
            End If

            If disable Then
                output = volume
            Else
                output = decay_count
            End If
        End Sub
    End Class

    Public Structure LengthCounter
        Public Counter As Byte

        ''' <summary>
        ''' Clocks the length counter. 
        ''' Decrements if enabled and not halted, or resets to 0 if disabled.
        ''' </summary>
        Public Function Clock(bEnable As Boolean, bHalt As Boolean) As Byte
            If Not bEnable Then
                Counter = 0
            Else
                If Counter > 0 AndAlso Not bHalt Then
                    Counter -= 1
                End If
            End If
            Return Counter
        End Function
    End Structure

    Public Class Sweeper
        Public Enabled As Boolean = False
        Public Down As Boolean = False
        Public Reload As Boolean = False
        Public Shift As Byte = 0
        Public Timer As Byte = 0
        Public Period As Byte = 0
        Public Change As UInt16 = 0
        Public Mute As Boolean = False

        ''' <summary>
        ''' Calculates if the channel should be muted based on the target frequency.
        ''' </summary>
        Public Sub Track(ByRef target As UInt16)
            If Enabled Then
                Change = CUShort(target >> Shift)
                Mute = (target < 8) OrElse (target > &H7FF)
            End If
        End Sub

        ''' <summary>
        ''' Clocks the sweeper. Returns true if the target frequency was modified.
        ''' </summary>
        Public Function Clock(ByRef target As UInt16, channel As Boolean) As Boolean
            Dim changed As Boolean = False

            ' The sweeper logic for modifying the target frequency
            If Timer = 0 AndAlso Enabled AndAlso Shift > 0 AndAlso Not Mute Then
                If target >= 8 AndAlso Change < &H7FF Then
                    If Down Then
                        ' Pulse 1 and Pulse 2 handle "down" shifts slightly differently 
                        ' in hardware; the "channel" parameter (0 or 1) accounts for this.
                        target -= CUShort(Change - (If(channel, 1, 0)))
                    Else
                        target += Change
                    End If
                    changed = True
                End If
            End If

            ' Update the internal timer
            If Timer = 0 OrElse Reload Then
                Timer = Period
                Reload = False
            Else
                Timer -= 1
            End If

            ' Re-evaluate muting
            Mute = (target < 8) OrElse (target > &H7FF)

            Return changed
        End Function
    End Class

    Public Class em2A03
        Private Shared ReadOnly length_table As Byte() = {
            10, 254, 20, 2, 40, 4, 80, 6,
            160, 8, 60, 10, 14, 12, 26, 14,
            12, 16, 24, 18, 48, 20, 96, 22,
            192, 24, 72, 26, 16, 28, 32, 30
        }

        Private frame_clock_counter As UInteger = 0
        Private clock_counter As UInteger = 0
        Private bUseRawMode As Boolean = False

        Private dGlobalTime As Double = 0.0

        '// Square Wave Pulse Channel 1
        Private pulse1_enable As Boolean = False
        Private pulse1_halt As Boolean = False
        Private pulse1_sample As Double = 0.0
        Private pulse1_output As Double = 0.0
        Private pulse1_seq As Sequencer
        Private pulse1_osc As New OscPulse
        Private pulse1_env As New Envelope
        Private pulse1_lc As LengthCounter
        Private pulse1_sweep As New Sweeper

        '// Square Wave Pulse Channel 2
        Private pulse2_enable As Boolean = False
        Private pulse2_halt As Boolean = False
        Private pulse2_sample As Double = 0.0
        Private pulse2_output As Double = 0.0
        Private pulse2_seq As Sequencer
        Private pulse2_osc As New OscPulse
        Private pulse2_env As New Envelope
        Private pulse2_lc As LengthCounter
        Private pulse2_sweep As New Sweeper

        '// Noise Channel
        Private noise_enable As Boolean = False
        Private noise_halt As Boolean = False
        Private noise_env As New Envelope
        Private noise_lc As lengthcounter
        Private noise_seq As Sequencer
        Private noise_sample As Double = 0
        Private noise_output As Double = 0

        Public pulse1_visual As UShort = 0
        Public pulse2_visual As UShort = 0
        Public noise_visual As UShort = 0
        Public triangle_visual As UShort = 0

        Public Sub New()
            noise_seq.sequence = &HDBDBUI
        End Sub

        Protected Overrides Sub Finalize()
            MyBase.Finalize()
        End Sub

        Public Sub CpuWrite(addr As UInt16, data As Byte)
            Select Case addr
        ' --- Pulse 1 ---
                Case &H4000
                    Select Case (data And &HC0) >> 6
                        Case &H0 : pulse1_seq.new_sequence = &B1000000 : pulse1_osc.dutycycle = 0.125
                        Case &H1 : pulse1_seq.new_sequence = &B1100000 : pulse1_osc.dutycycle = 0.25
                        Case &H2 : pulse1_seq.new_sequence = &B1111000 : pulse1_osc.dutycycle = 0.5
                        Case &H3 : pulse1_seq.new_sequence = &B10011111 : pulse1_osc.dutycycle = 0.75
                    End Select
                    pulse1_seq.sequence = pulse1_seq.new_sequence
                    pulse1_halt = (data And &H20) <> 0
                    pulse1_env.volume = (data And &HF)
                    pulse1_env.disable = (data And &H10) <> 0

                Case &H4001
                    pulse1_sweep.Enabled = (data And &H80) <> 0
                    pulse1_sweep.Period = (data And &H70) >> 4
                    pulse1_sweep.Down = (data And &H8) <> 0
                    pulse1_sweep.Shift = (data And &H7)
                    pulse1_sweep.Reload = True

                Case &H4002
                    pulse1_seq.reload = (pulse1_seq.reload And &HFF00) Or data

                Case &H4003
                    pulse1_seq.reload = CUShort((data And &H7) << 8) Or (pulse1_seq.reload And &HFF)
                    pulse1_seq.timer = pulse1_seq.reload
                    pulse1_seq.sequence = pulse1_seq.new_sequence
                    pulse1_lc.Counter = length_table((data And &HF8) >> 3)
                    pulse1_env.start = True

        ' --- Pulse 2 ---
                Case &H4004
                    Select Case (data And &HC0) >> 6
                        Case &H0 : pulse2_seq.new_sequence = &B1000000 : pulse2_osc.dutycycle = 0.125
                        Case &H1 : pulse2_seq.new_sequence = &B1100000 : pulse2_osc.dutycycle = 0.25
                        Case &H2 : pulse2_seq.new_sequence = &B1111000 : pulse2_osc.dutycycle = 0.5
                        Case &H3 : pulse2_seq.new_sequence = &B10011111 : pulse2_osc.dutycycle = 0.75
                    End Select
                    pulse2_seq.sequence = pulse2_seq.new_sequence
                    pulse2_halt = (data And &H20) <> 0
                    pulse2_env.volume = (data And &HF)
                    pulse2_env.disable = (data And &H10) <> 0

                Case &H4005
                    pulse2_sweep.Enabled = (data And &H80) <> 0
                    pulse2_sweep.Period = (data And &H70) >> 4
                    pulse2_sweep.Down = (data And &H8) <> 0
                    pulse2_sweep.Shift = (data And &H7)
                    pulse2_sweep.Reload = True

                Case &H4006
                    pulse2_seq.reload = (pulse2_seq.reload And &HFF00) Or data

                Case &H4007
                    pulse2_seq.reload = CUShort((data And &H7) << 8) Or (pulse2_seq.reload And &HFF)
                    pulse2_seq.timer = pulse2_seq.reload
                    pulse2_seq.sequence = pulse2_seq.new_sequence
                    pulse2_lc.Counter = length_table((data And &HF8) >> 3)
                    pulse2_env.start = True

        ' --- Noise ---
                Case &H400C
                    noise_env.volume = (data And &HF)
                    noise_env.disable = (data And &H10) <> 0
                    noise_halt = (data And &H20) <> 0

                Case &H400E
                    Select Case (data And &HF)
                        Case &H0 : noise_seq.reload = 0
                        Case &H1 : noise_seq.reload = 4
                        Case &H2 : noise_seq.reload = 8
                        Case &H3 : noise_seq.reload = 16
                        Case &H4 : noise_seq.reload = 32
                        Case &H5 : noise_seq.reload = 64
                        Case &H6 : noise_seq.reload = 96
                        Case &H7 : noise_seq.reload = 128
                        Case &H8 : noise_seq.reload = 160
                        Case &H9 : noise_seq.reload = 202
                        Case &HA : noise_seq.reload = 254
                        Case &HB : noise_seq.reload = 380
                        Case &HC : noise_seq.reload = 508
                        Case &HD : noise_seq.reload = 1016
                        Case &HE : noise_seq.reload = 2034
                        Case &HF : noise_seq.reload = 4068
                    End Select

                Case &H400F
                    pulse1_env.start = True
                    pulse2_env.start = True
                    noise_env.start = True
                    noise_lc.Counter = length_table((data And &HF8) >> 3)

        ' --- Status Control ---
                Case &H4015
                    pulse1_enable = (data And &H1) <> 0
                    pulse2_enable = (data And &H2) <> 0
                    noise_enable = (data And &H4) <> 0
            End Select
        End Sub

        Public Function CpuRead(addr As UInt16) As Byte
            Dim data As Byte = &H0

            If addr = &H4015 Then
                ' Read status of length counters
                'data = data Or If(pulse1_lc.Counter > 0, CByte(&H1), CByte(&H0))
                'data = data Or If(pulse2_lc.Counter > 0, CByte(&H2), CByte(&H0))
                'data = data Or If(noise_lc.Counter > 0, CByte(&H4), CByte(&H0))
            End If

            Return data
        End Function

        Public Sub Clock()
            Dim bQuarterFrameClock As Boolean = False
            Dim bHalfFrameClock As Boolean = False

            ' Increment global time based on NES CPU clock frequency
            dGlobalTime += (0.3333333333 / 1789773.0)

            If clock_counter Mod 6 = 0 Then
                frame_clock_counter += 1

                ' 4-Step Sequence Mode
                If frame_clock_counter = 3729 Then
                    bQuarterFrameClock = True
                End If

                If frame_clock_counter = 7457 Then
                    bQuarterFrameClock = True
                    bHalfFrameClock = True
                End If

                If frame_clock_counter = 11186 Then
                    bQuarterFrameClock = True
                End If

                If frame_clock_counter = 14916 Then
                    bQuarterFrameClock = True
                    bHalfFrameClock = True
                    frame_clock_counter = 0
                End If

                ' Update functional units
                If bQuarterFrameClock Then
                    pulse1_env.Clock(pulse1_halt)
                    pulse2_env.Clock(pulse2_halt)
                    noise_env.Clock(noise_halt)
                End If

                If bHalfFrameClock Then
                    pulse1_lc.Clock(pulse1_enable, pulse1_halt)
                    pulse2_lc.Clock(pulse2_enable, pulse2_halt)
                    noise_lc.Clock(noise_enable, noise_halt)
                    pulse1_sweep.Clock(pulse1_seq.reload, False) ' Channel 0
                    pulse2_sweep.Clock(pulse2_seq.reload, True)  ' Channel 1
                End If

                ' --- Update Pulse 1 ---
                pulse1_seq.Clock(pulse1_enable, Sub(ByRef s As UInt32)
                                                    ' Shift right by 1 bit, wrapping around
                                                    s = ((s And &H1) << 7) Or ((s And &HFE) >> 1)
                                                End Sub)

                pulse1_osc.frequency = 1789773.0 / (16.0 * (CDbl(pulse1_seq.reload) + 1.0))
                pulse1_osc.amplitude = (CDbl(pulse1_env.output) - 1.0) / 16.0
                pulse1_sample = pulse1_osc.Sample(dGlobalTime)

                ' Simple Low Pass Filter approximation used in the original C++
                If pulse1_lc.Counter > 0 AndAlso pulse1_seq.timer >= 8 AndAlso Not pulse1_sweep.Mute AndAlso pulse1_env.output > 2 Then
                    pulse1_output += (pulse1_sample - pulse1_output) * 0.5
                Else
                    pulse1_output = 0
                End If

                ' --- Update Pulse 2 ---
                pulse2_seq.Clock(pulse2_enable, Sub(ByRef s As UInt32)
                                                    ' Shift right by 1 bit, wrapping around
                                                    s = ((s And &H1) << 7) Or ((s And &HFE) >> 1)
                                                End Sub)

                pulse2_osc.frequency = 1789773.0 / (16.0 * (CDbl(pulse2_seq.reload) + 1.0))
                pulse2_osc.amplitude = (CDbl(pulse2_env.output) - 1.0) / 16.0
                pulse2_sample = pulse2_osc.Sample(dGlobalTime)

                If pulse2_lc.Counter > 0 AndAlso pulse2_seq.timer >= 8 AndAlso Not pulse2_sweep.Mute AndAlso pulse2_env.output > 2 Then
                    pulse2_output += (pulse2_sample - pulse2_output) * 0.5
                Else
                    pulse2_output = 0
                End If

                ' --- Update Noise ---
                noise_seq.Clock(noise_enable, Sub(ByRef s As UInt32)
                                                  ' Pseudo-random shift logic
                                                  s = (((s And &H1) Xor ((s And &H2) >> 1)) << 14) Or ((s And &H7FFF) >> 1)
                                              End Sub)

                If noise_lc.Counter > 0 AndAlso noise_seq.timer >= 8 Then
                    noise_output = CDbl(noise_seq.output) * ((CDbl(noise_env.output) - 1.0) / 16.0)
                Else
                    noise_output = 0
                End If

                ' Force silence if channels are disabled
                If Not pulse1_enable Then pulse1_output = 0
                If Not pulse2_enable Then pulse2_output = 0
                If Not noise_enable Then noise_output = 0
            End If

            ' Frequency sweepers track targets every clock cycle
            pulse1_sweep.Track(pulse1_seq.reload)
            pulse2_sweep.Track(pulse2_seq.reload)

            ' Visualizer helper values
            pulse1_visual = If(pulse1_enable AndAlso pulse1_env.output > 1 AndAlso Not pulse1_sweep.Mute, pulse1_seq.reload, CUShort(2047))
            pulse2_visual = If(pulse2_enable AndAlso pulse2_env.output > 1 AndAlso Not pulse2_sweep.Mute, pulse2_seq.reload, CUShort(2047))
            noise_visual = If(noise_enable AndAlso noise_env.output > 1, pulse1_seq.reload, CUShort(2047))

            clock_counter += 1
        End Sub

        Public Function GetOutputSample() As Double
            If bUseRawMode Then
                ' Simple raw mixing
                Return (pulse1_sample - 0.5) * 0.5 + (pulse2_sample - 0.5) * 0.5
            Else
                ' Standard NES Mixing (Adjusted for oscillators)
                ' This prevents the "DC Offset" silence issue by keeping the signal centered
                Return ((1.0 * pulse1_output) - 0.8) * 0.1 +
                       ((1.0 * pulse2_output) - 0.8) * 0.1 +
                       ((2.0 * (noise_output - 0.5))) * 0.1
            End If
        End Function

    End Class

End Namespace