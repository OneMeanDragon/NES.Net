Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' Mapper 004: MMC3 - Advanced mapper with IRQ counter
    ''' Games: Super Mario Bros 3, Mega Man 3-6, etc.
    ''' </summary>
    Public NotInheritable Class Mapper004
        Inherits MapperBase

        Public Overrides ReadOnly Property MapperName As String = "MMC3 (TxROM)"
        Public Overrides ReadOnly Property MapperNumber As Byte = 4

        ' Bank registers
        Private _registers(7) As Byte
        Private _prgBanksReg(3) As UInt32
        Private _chrBanksReg(7) As UInt32

        ' Control
        Private _targetRegister As Byte
        Private _prgBankMode As Boolean
        Private _chrInversion As Boolean

        ' IRQ
        Private _irqCounter As UInt16
        Private _irqReload As UInt16
        Private _irqEnable As Boolean
        Private _irqActive As Boolean

        Public Sub New(prgBanks As Byte, chrBanks As Byte)
            MyBase.New(prgBanks, chrBanks)
            _cartRam = New Memory(Of Byte)(New Byte(8191) {})
        End Sub

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function CpuMapRead(addr As UInt16, ByRef mappedAddr As UInt32, ByRef data As Byte) As Boolean
            If addr >= &H6000US AndAlso addr < &H8000US Then
                mappedAddr = &HFFFFFFFFUI
                data = _cartRam.Span(addr And &H1FFFUS)
                Return True
            End If

            If addr >= &H8000US AndAlso addr < &HA000US Then
                mappedAddr = _prgBanksReg(0) + (addr And &H1FFFUI)
                Return True
            ElseIf addr >= &HA000US AndAlso addr < &HC000US Then
                mappedAddr = _prgBanksReg(1) + (addr And &H1FFFUI)
                Return True
            ElseIf addr >= &HC000US AndAlso addr < &HE000US Then
                mappedAddr = _prgBanksReg(2) + (addr And &H1FFFUI)
                Return True
            ElseIf addr >= &HE000US Then
                mappedAddr = _prgBanksReg(3) + (addr And &H1FFFUI)
                Return True
            End If

            Return False
        End Function

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function CpuMapWrite(addr As UInt16, ByRef mappedAddr As UInt32, data As Byte) As Boolean
            If addr >= &H6000US AndAlso addr < &H8000US Then
                mappedAddr = &HFFFFFFFFUI
                Dim span = _cartRam.Span
                span(addr And &H1FFFUS) = data
                '_cartRam.Span(addr And &H1FFFUS) = data
                'MemoryMarshal.Write(Of Byte)(_cartRam.Span.Slice(CInt(addr And &H1FFFUS), 1), data)
                Return True
            End If

            If addr >= &H8000US AndAlso addr < &HA000US Then
                If (addr And 1) = 0 Then
                    _targetRegister = data And &H7
                    _prgBankMode = (data And &H40) <> 0
                    _chrInversion = (data And &H80) <> 0
                Else
                    _registers(_targetRegister) = data
                    UpdateBanks()
                End If
            ElseIf addr >= &HA000US AndAlso addr < &HC000US Then
                If (addr And 1) = 0 Then
                    _mirrorMode = If((data And 1) <> 0, MirrorMode.Horizontal, MirrorMode.Vertical)
                End If
            ElseIf addr >= &HC000US AndAlso addr < &HE000US Then
                If (addr And 1) = 0 Then
                    _irqReload = data
                Else
                    _irqCounter = 0
                End If
            ElseIf addr >= &HE000US Then
                If (addr And 1) = 0 Then
                    _irqEnable = False
                    _irqActive = False
                Else
                    _irqEnable = True
                End If
            End If

            Return False
        End Function

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function PpuMapRead(addr As UInt16, ByRef mappedAddr As UInt32) As Boolean
            If addr < &H2000US Then
                Dim bank = addr >> 10 ' Divide by 1024 to get bank (0-7)
                mappedAddr = _chrBanksReg(bank) + (addr And &H3FFUI)
                Return True
            End If
            Return False
        End Function

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function PpuMapWrite(addr As UInt16, ByRef mappedAddr As UInt32) As Boolean
            Return False
        End Function

        Private Sub UpdateBanks()
            ' Update CHR banks
            If _chrInversion Then
                _chrBanksReg(0) = CUInt(_registers(2)) * &H400UI
                _chrBanksReg(1) = CUInt(_registers(3)) * &H400UI
                _chrBanksReg(2) = CUInt(_registers(4)) * &H400UI
                _chrBanksReg(3) = CUInt(_registers(5)) * &H400UI
                _chrBanksReg(4) = CUInt(_registers(0) And &HFE) * &H400UI
                _chrBanksReg(5) = (CUInt(_registers(0)) Or 1) * &H400UI
                _chrBanksReg(6) = CUInt(_registers(1) And &HFE) * &H400UI
                _chrBanksReg(7) = (CUInt(_registers(1)) Or 1) * &H400UI
            Else
                _chrBanksReg(0) = CUInt(_registers(0) And &HFE) * &H400UI
                _chrBanksReg(1) = (CUInt(_registers(0)) Or 1) * &H400UI
                _chrBanksReg(2) = CUInt(_registers(1) And &HFE) * &H400UI
                _chrBanksReg(3) = (CUInt(_registers(1)) Or 1) * &H400UI
                _chrBanksReg(4) = CUInt(_registers(2)) * &H400UI
                _chrBanksReg(5) = CUInt(_registers(3)) * &H400UI
                _chrBanksReg(6) = CUInt(_registers(4)) * &H400UI
                _chrBanksReg(7) = CUInt(_registers(5)) * &H400UI
            End If

            ' Update PRG banks
            If _prgBankMode Then
                _prgBanksReg(0) = CUInt(_prgBanks * 2 - 2) * &H2000UI
                _prgBanksReg(1) = CUInt(_registers(7) And &H3F) * &H2000UI
                _prgBanksReg(2) = CUInt(_registers(6) And &H3F) * &H2000UI
                _prgBanksReg(3) = CUInt(_prgBanks * 2 - 1) * &H2000UI
            Else
                _prgBanksReg(0) = CUInt(_registers(6) And &H3F) * &H2000UI
                _prgBanksReg(1) = CUInt(_registers(7) And &H3F) * &H2000UI
                _prgBanksReg(2) = CUInt(_prgBanks * 2 - 2) * &H2000UI
                _prgBanksReg(3) = CUInt(_prgBanks * 2 - 1) * &H2000UI
            End If
        End Sub

        Public Overrides Sub Reset()
            _targetRegister = 0
            _prgBankMode = False
            _chrInversion = False
            _mirrorMode = MirrorMode.Horizontal

            _irqCounter = 0
            _irqReload = 0
            _irqEnable = False
            _irqActive = False

            Array.Clear(_registers, 0, 8)
            Array.Clear(_prgBanksReg, 0, 4)
            Array.Clear(_chrBanksReg, 0, 8)

            _prgBanksReg(0) = 0
            _prgBanksReg(1) = &H2000UI
            _prgBanksReg(2) = CUInt(_prgBanks * 2 - 2) * &H2000UI
            _prgBanksReg(3) = CUInt(_prgBanks * 2 - 1) * &H2000UI
        End Sub

        Public Overrides Function IsIrqActive() As Boolean
            Return _irqActive
        End Function

        Public Overrides Sub ClearIrq()
            _irqActive = False
        End Sub

        Public Overrides Sub ScanlineCounter()
            If _irqCounter = 0 Then
                _irqCounter = _irqReload
            Else
                _irqCounter -= 1US
            End If

            If _irqCounter = 0 AndAlso _irqEnable Then
                _irqActive = True
            End If
        End Sub

        Public Overrides Function GetMirrorMode() As MirrorMode
            Return _mirrorMode
        End Function
    End Class

End Namespace