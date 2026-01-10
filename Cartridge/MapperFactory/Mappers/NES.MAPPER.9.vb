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
            MyBase.New(prgBanks, chrBanks)
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
            If addr >= &H6000 AndAlso addr <= &H7FFF Then
                mappedAddr = (addr And &H1FFF)
                Return True
            End If

            If addr >= &H8000 AndAlso addr <= &H9FFF Then
                ' $8000-$9FFF: 8KB Switchable PRG Bank
                mappedAddr = CUInt(_prgBankSelect * &H2000) + (addr And &H1FFF)
                Return True
            ElseIf addr >= &HA000 Then
                ' $A000-$FFFF: Three fixed 8KB PRG banks (last three banks of the ROM)
                ' $A000 = Penultimate - 2
                ' $C000 = Penultimate - 1
                ' $E000 = Last Bank
                Dim revAddr As Integer = (CInt(_prgBanks) * 2) - (addr >> 13) ' Logic to map top 24KB
                mappedAddr = CUInt(((_prgBanks * 4) - (8 - (addr >> 12))) * &H2000) ' Simpler: relative to end

                ' Fixed calculation:
                Dim bankOffset As Integer = (addr - &HA000) >> 13
                mappedAddr = CUInt(((_prgBanks - 3) + bankOffset) * &H2000) + (addr And &H1FFF)
                Return True
            End If
            Return False
        End Function

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function CpuMapWrite(addr As UShort, ByRef mappedAddr As UInteger, data As Byte) As Boolean
            If addr >= &HA000 AndAlso addr <= &HAFFF Then
                _prgBankSelect = data And &HF
            ElseIf addr >= &HB000 AndAlso addr <= &HBFFF Then
                _chrBank0FD = data And &H1F
            ElseIf addr >= &HC000 AndAlso addr <= &HCFFF Then
                _chrBank0FE = data And &H1F
            ElseIf addr >= &HD000 AndAlso addr <= &HDFFF Then
                _chrBank1FD = data And &H1F
            ElseIf addr >= &HE000 AndAlso addr <= &HEFFF Then
                _chrBank1FE = data And &H1F
            ElseIf addr >= &HF000 AndAlso addr <= &HFFFF Then
                _mirrorMode = If((data And &H1) = 0, MirrorMode.Vertical, MirrorMode.Horizontal)
            End If
            Return False ' CPU writes to mapper registers don't write to PRG RAM/ROM
        End Function

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function PpuMapRead(addr As UShort, ByRef mappedAddr As UInteger) As Boolean
            If addr >= &H0 AndAlso addr <= &HFFF Then
                ' Pattern Table 0
                Dim bank As Integer = If(_latch0 = 0, _chrBank0FD, _chrBank0FE)
                mappedAddr = CUInt(bank * &H1000) + (addr And &HFFF)

                ' Update Latch AFTER read logic (The MMC2 latches on specific tile addresses)
                If addr = &HFD8 Then _latch0 = 0
                If addr = &HFE8 Then _latch0 = 1
                Return True

            Else 'If addr >= &H1000 AndAlso addr <= &H1FFF Then
                ' Pattern Table 1
                Dim bank As Integer = If(_latch1 = 0, _chrBank1FD, _chrBank1FE)
                mappedAddr = CUInt(bank * &H1000) + (addr And &HFFF)

                ' Update Latch
                If addr >= &H1FD8 AndAlso addr <= &H1FDF Then _latch1 = 0
                If addr >= &H1FE8 AndAlso addr <= &H1FEF Then _latch1 = 1
                Return True
            End If
            Return False
        End Function

        Public Overrides Function PpuMapWrite(addr As UShort, ByRef mappedAddr As UInteger) As Boolean
            ' CHR is typically ROM in MMC2
            Return False
        End Function

    End Class
End Namespace
