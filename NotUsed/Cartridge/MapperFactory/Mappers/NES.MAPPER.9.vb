Imports System.Net
Imports System.Runtime.CompilerServices

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' Mapper 009: MMC2 (used exclusively by Punch-Out!!)
    ''' // - 8KB switchable PRG bank at $8000-$9FFF
    ''' // - 24KB fixed PRG at $A000-$FFFF (last 3 banks)
    ''' // - Two CHR bank registers per 4KB area, latched by reading tiles $FD/$FE
    ''' // - Mirroring control
    ''' </summary>
    Public Class Mapper009
        Inherits MapperBase

        ' PRG Reg: $A000-$AFFF (8KB switchable bank at $8000)
        Private _prgBankSelect As Integer = 0
        Private _prgFixedOffset As UInteger

        ' CHR Regs: $B000-$EFFF
        Private _chrBank0FD As Integer = 0 ' $B000
        Private _chrBank0FE As Integer = 0 ' $C000
        Private _chrBank1FD As Integer = 0 ' $D000
        Private _chrBank1FE As Integer = 0 ' $E000

        ' Latches: FD = 0, FE = 1
        Private _latch0 As Integer = 0
        Private _latch1 As Integer = 0

        Public Overrides ReadOnly Property MapperName As String = "MMC2 (PxROM)"
        Public Overrides ReadOnly Property MapperNumber As Byte = 9

        Public Sub New(prgBanks As Byte, chrBanks As Byte)
            'PRG Banks:       8 x 16KB = 128 KB
            'CHR Banks:      16 x  8KB = 128 KB
            MyBase.New(CByte(prgBanks * 2), CByte(chrBanks * 2))

            ' MMC2 (Mapper 9) uses 8KB banks. 
            ' If prgBanks is in 16KB units, total 8KB banks = prgBanks * 2
            Dim total8kbBanks As Integer = CInt(prgBanks) * 2

            ' The fixed 24KB block starts 3 banks (8KB each) from the end
            _prgFixedOffset = CUInt((total8kbBanks - 3) * &H2000UI)

            _cartRam = New Memory(Of Byte)(New Byte(8191) {})
            Reset()
        End Sub

        Public Overrides Sub Reset()
            _prgBankSelect = 0
            _chrBank0FD = 0
            _chrBank0FE = 0
            _chrBank1FD = 0
            _chrBank1FE = 0
            _latch0 = 0
            _latch1 = 0
            _mirrorMode = MirrorMode.Horizontal
        End Sub

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function CpuMapRead(addr As UShort, ByRef mappedAddr As UInteger, ByRef data As Byte) As Boolean
            ' PRG RAM: $6000-$7FFF
            If addr >= &H6000US AndAlso addr <= &H7FFFUS Then
                mappedAddr = &HFFFFFFFFUI
                data = _cartRam.Span(addr And &H1FFFUS)
                Return True
            ElseIf addr >= &H8000US AndAlso addr <= &H9FFFUS Then
                mappedAddr = CUInt(_prgBankSelect * &H2000I) + (addr And &H1FFFUS)
                Return True
            ElseIf addr >= &HA000US Then '$A000->$FFFF
                mappedAddr = _prgFixedOffset + (addr - &HA000US)
                Return True
            End If
            Return False
        End Function

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function CpuMapWrite(addr As UShort, ByRef mappedAddr As UInteger, data As Byte) As Boolean
            ' Cart RAM write
            If addr >= &H6000US AndAlso addr <= &H7FFFUS Then
                mappedAddr = &HFFFFFFFFUI
                Dim span = _cartRam.Span
                span(addr And &H1FFFUS) = data
                Return True
            End If
            If addr >= &HA000US AndAlso addr <= &HAFFFUS Then
                _prgBankSelect = data And &HFUI
            ElseIf addr >= &HB000US AndAlso addr <= &HBFFFUS Then
                _chrBank0FD = data And &H1FUI
            ElseIf addr >= &HC000US AndAlso addr <= &HCFFFUS Then
                _chrBank0FE = data And &H1FUI
            ElseIf addr >= &HD000US AndAlso addr <= &HDFFFUS Then
                _chrBank1FD = data And &H1FUI
            ElseIf addr >= &HE000US AndAlso addr <= &HEFFFUS Then
                _chrBank1FE = data And &H1FUI
            ElseIf addr >= &HF000US AndAlso addr <= &HFFFFUS Then
                _mirrorMode = If((data And &H1UI) = 0, MirrorMode.Vertical, MirrorMode.Horizontal)
            End If
            Return False ' CPU writes to mapper registers don't write to PRG RAM/ROM
        End Function

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function PpuMapRead(addr As UShort, ByRef mappedAddr As UInteger) As Boolean
            If addr <= &HFFFUS Then
                Dim bank As Integer = If(_latch0 = 0, _chrBank0FD, _chrBank0FE)
                mappedAddr = CUInt(bank * &H1000I) + (addr And &HFFFUS)

                If addr = &HFD8US Then _latch0 = 0
                If addr = &HFE8US Then _latch0 = 1

                Return True
            ElseIf addr >= &H1000US AndAlso addr <= &H1FFFUS Then
                Dim bank As Integer = If(_latch1 = 0, _chrBank1FD, _chrBank1FE)
                mappedAddr = CUInt(bank * &H1000I) + (addr And &HFFFUS)

                If addr >= &H1FD8US AndAlso addr <= &H1FDFUS Then _latch1 = 0
                If addr >= &H1FE8US AndAlso addr <= &H1FEFUS Then _latch1 = 1

                Return True
            End If
            Return False
        End Function

        Public Overrides Function PpuMapWrite(addr As UShort, ByRef mappedAddr As UInteger) As Boolean
            ' CHR is typically ROM in MMC2
            ' Handle CHR RAM case if present
            If addr <= &H1FFFUS Then
                ' If _chrBanks is 0, the cartridge is using 8KB of CHR-RAM
                If _chrBanks = 0 Then
                    ' Calculate the mapped address based on current latches
                    ' Even with RAM, Mapper 9 logic still dictates which 4KB "window" is active
                    If addr <= &HFFFUS Then
                        Dim bank As Integer = If(_latch0 = 0, _chrBank0FD, _chrBank0FE)
                        mappedAddr = CUInt(bank * &H1000I) + (addr And &HFFFUS)
                    Else
                        Dim bank As Integer = If(_latch1 = 0, _chrBank1FD, _chrBank1FE)
                        mappedAddr = CUInt(bank * &H1000I) + (addr And &HFFFUS)
                    End If
                    Return True ' Signals to the Bus that this write is valid
                End If
            End If

            ' Default: MMC2 is CHR-ROM; PPU cannot write to it
            Return False
        End Function

    End Class
End Namespace
