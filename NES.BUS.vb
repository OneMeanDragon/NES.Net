#Const APU_IMPLEMENTED = 0


Namespace NintendoEntertainmentSystem

    ' Master Class Assumed
    Public Class clsBus
        Private mReadonly As Boolean
        Private MAX_RAM As UInt32
        'Private m_Ram() As Byte

        'Connected Devices
        ' The 6502 derived processor
        Public CPU As New em6502()
        ' The 2C02 Picture Processing Unit
        Public PPU As New em2C02()
        ' The 2A03 Audio Processing Unit
        Public APU As New em2A03()
        ' The Game Cartridge "ROM"
        'Public Cart As clsCartridge '[Search TODO (it's in one of the mappers)]
        'Public Function GameInserted() As Boolean
        '    Return Not IsNothing(Cart)
        'End Function

        ' 2KB of RAM
        Public cpuRam(2047) As Byte 'VB
        ' Controllers
        Public Controller(1) As Byte 'VB

        ' A count of how many clocks have passed
        Private nSystemClockCounter As UInt32 = 0
        ' Internal cache of the controller state
        Private controller_state(1) As Byte 'VB

        ' DMA
        Public dma_page As Byte = &H0
        Public dma_addr As Byte = &H0
        Public dma_data As Byte = &H0
        Public dma_dummy As Boolean = True
        Public dma_transfer As Boolean = False

        ' Audio
        Public dAudioSample As Double = 0.0
        Private dAudioTime As Double = 0.0
        Private dAudioGlobalTime As Double = 0.0
        Private dAudioTimePerNESClock As Double = 0.0
        Private dAudioTimePerSystemSample As Double = 0.0F

        Public Sub New()
            'Connect to the CPU
            CPU.ConnectBus(Me)
        End Sub

        Protected Overrides Sub Finalize()
#If DEBUG Then
            Debug.WriteLine("NES.BUS Destructor.")
#End If
            MyBase.Finalize()
        End Sub

        Public Sub SetSampleFrequency(ByVal sample_rate As UInt32)
            dAudioTimePerSystemSample = 1.0 / sample_rate
            dAudioTimePerNESClock = 1.0 / 5369318.0
        End Sub

        'ReadWrite to the BUS
        Public Sub cpuWrite(ByVal addr As UInt16, ByVal data As Byte)
            If Cart.cpuWrite(addr, data) Then
                '// The cartridge "sees all" And has the facility to veto
                '// the propagation of the bus transaction if it requires.
                '// This allows the cartridge to map any address to some
                '// other data, including the facility to divert transactions
                '// with other physical devices. The NES does Not do this
                '// but I figured it might be quite a flexible way of adding
                '// "custom" hardware to the NES in the future!
            ElseIf addr >= &H0US AndAlso addr <= &H1FFFUS Then
                '// System RAM Address Range. The range covers 8KB, though
                '// there Is only 2KB available. That 2KB Is "mirrored"
                '// through this address range. Using bitwise And to mask
                '// the bottom 11 bits Is the same as addr % 2048.
                'GOBACK
                cpuRam(addr And &H7FFUS) = data ' this should probably be (& 2047) since thats the max buffer size
            ElseIf addr >= &H2000US AndAlso addr <= &H3FFFUS Then
                '// PPU Address range. The PPU only has 8 primary registers
                '// And these are repeated throughout this range. We can
                '// use bitwise And operation to mask the bottom 3 bits, 
                '// which Is the equivalent of addr % 8.
                ppu.cpuWrite(addr And &H7US, data)
            ElseIf (addr >= &H4000US AndAlso addr <= &H4013US) OrElse addr = &H4015US OrElse addr = &H4017US Then
#If APU_IMPLEMENTED >= 1 Then
                APU.cpuWrite(addr, data)
#End If
            ElseIf addr = &H4014US Then
                dma_page = data
                dma_addr = &H0
                dma_transfer = True
            ElseIf addr >= &H4016US AndAlso addr <= &H4017US Then
                '// "Lock In" controller state at this time
                controller_state(addr And &H1US) = Controller(addr And &H1US)
            End If
        End Sub
        Public Function cpuRead(ByVal addr As UInt16, Optional bReadOnly As Boolean = False) As Byte
            Dim data As Byte = &H0
            If Cart.cpuRead(addr, data) Then
                ' Cartridge address space
            ElseIf addr >= &H0US AndAlso addr <= &H1FFFUS Then
                data = cpuRam(addr And &H7FFUS)
            ElseIf addr >= &H2000US AndAlso addr <= &H3FFF Then
                ' PPU Address Space
                data = PPU.cpuRead(addr And &H7US, bReadOnly)
            ElseIf addr = &H4015US Then
                ' APU Read Status
#If APU_IMPLEMENTED >= 1 Then
                data = APU.cpuRead(addr)
#End If
            ElseIf addr >= &H4016US AndAlso addr <= &H4017 Then
                '// Read out the MSB of the controller status word
                data = (controller_state(addr And &H1US) And &H80) > 0
                controller_state(addr And &H1US) <<= 1
            End If
            Return data
        End Function

        '' Insert The Game into the System
        'Public Sub InsertCartridge(ByRef cartridge As clsCartridge)
        '    Cart = cartridge
        '    PPU.ConnectCartridge(cartridge)
        'End Sub
        'Public Sub RemoveCartridge()
        '    Cart = Nothing
        '    PPU.RemoveCartridge()
        'End Sub

        Public Sub Reset()
            Cart.Reset()
            CPU.Reset()
            PPU.Reset()
            nSystemClockCounter = 0
            dma_page = &H0
            dma_addr = &H0
            dma_data = &H0
            dma_dummy = True
            dma_transfer = False
        End Sub

        Public Function Clock() As Boolean
            '// Clocking. The heart And soul of an emulator. The running
            '// frequency Is controlled by whatever calls this function.
            '// So here we "divide" the clock as necessary And call
            '// the peripheral devices clock() function at the correct
            '// times.
            '// The fastest clock frequency the digital system cares
            '// about Is equivalent to the PPU clock. So the PPU Is clocked
            '// each time this function Is called...
            ppu.clock()
#If APU_IMPLEMENTED >= 1 Then
            '// ...also clock the APU
            APU.clock()
#End If
            '// The CPU runs 3 times slower than the PPU so we only call its
            '// clock() function every 3 times this function Is called. We
            '// have a global counter to keep track of this.
            If (nSystemClockCounter Mod 3) = 0 Then
                '// Is the system performing a DMA transfer form CPU memory to 
                '// OAM memory on PPU?...
                If dma_transfer Then
                    '// ...Yes! We need to wait until the next even CPU clock cycle
                    '// before it starts...
                    If dma_dummy Then
                        '// ...So hang around in here each clock until 1 Or 2 cycles
                        '// have elapsed...
                        If (nSystemClockCounter Mod 2) = 1 Then
                            '// ...and finally allow DMA to start
                            dma_dummy = False
                        End If
                    Else
                        '// DMA can take place!
                        If (nSystemClockCounter Mod 2) = 0 Then
                            '// On even clock cycles, read from CPU bus
                            dma_data = cpuRead(dma_page << 8 Or dma_addr) 'par
                        Else
                            '// On odd clock cycles, write to PPU OAM
                            PPU.OAM(dma_addr \ 4).SetByteAt(dma_addr, dma_data)
                            '// Increment the lo byte of the address
                            dma_addr = MathHelpers.SafeIncrementByte(dma_addr)
                            '// If this wraps around, we know that 256
                            '// bytes have been written, so end the DMA
                            '// transfer, And proceed as normal
                            If dma_addr = 0 Then
                                dma_transfer = False
                                dma_dummy = True
                            End If
                        End If
                    End If
                Else
                    '// No DMA happening, the CPU Is in control of its
                    '// own destiny. Go forth my friend And calculate
                    '// awesomeness for many generations to come...
                    CPU.Clock() 'Looping on one self fun fun
                End If
            End If
            '// Synchronising with Audio
            Dim bAudioSampleReady As Boolean = False
            dAudioTime += dAudioTimePerNESClock
            If dAudioTime >= dAudioTimePerSystemSample Then
                dAudioTime -= dAudioTimePerSystemSample
#If APU_IMPLEMENTED >= 1 Then
                dAudioSample = apu.getoutputsample()
#Else
                dAudioSample = 0
#End If
                bAudioSampleReady = True
            End If
            '// The PPU Is capable of emitting an interrupt to indicate the
            '// vertical blanking period has been entered. If it has, we need
            '// to send that irq to the CPU.
            If ppu.nmi Then
                ppu.nmi = False
                CPU.NMI()
            End If
            '// Check if cartridge is requesting IRQ
            If Cart.GetMapper.irqState() Then
                Cart.GetMapper.irqClear()
                CPU.IRQ()
            End If
            nSystemClockCounter += 1

            Return bAudioSampleReady
        End Function

    End Class

End Namespace