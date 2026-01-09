Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' Mapper 002: UxROM - Switchable PRG, fixed CHR
    ''' Games: Mega Man, Castlevania, Contra
    ''' </summary>
    Public NotInheritable Class Mapper002
        Inherits MapperBase

        Public Overrides ReadOnly Property MapperName As String = "UxROM"
        Public Overrides ReadOnly Property MapperNumber As Byte = 2

        Private _prgBankLo As Byte
        Private _prgBankHi As Byte

        Public Sub New(prgBanks As Byte, chrBanks As Byte)
            MyBase.New(prgBanks, chrBanks)
        End Sub

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function CpuMapRead(addr As UInt16, ByRef mappedAddr As UInt32, ByRef data As Byte) As Boolean
            If addr >= &H8000US AndAlso addr < &HC000US Then
                ' Switchable bank
                mappedAddr = CUInt(_prgBankLo) * &H4000UI + (addr And &H3FFFUI)
                Return True
            ElseIf addr >= &HC000US Then
                ' Fixed to last bank
                mappedAddr = CUInt(_prgBankHi) * &H4000UI + (addr And &H3FFFUI)
                Return True
            End If
            Return False
        End Function

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function CpuMapWrite(addr As UInt16, ByRef mappedAddr As UInt32, data As Byte) As Boolean
            If addr >= &H8000US Then
                _prgBankLo = data And &HF
            End If
            Return False
        End Function

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function PpuMapRead(addr As UInt16, ByRef mappedAddr As UInt32) As Boolean
            If addr < &H2000US Then
                mappedAddr = addr
                Return True
            End If
            Return False
        End Function

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function PpuMapWrite(addr As UInt16, ByRef mappedAddr As UInt32) As Boolean
            If addr < &H2000US AndAlso _chrBanks = 0 Then
                mappedAddr = addr
                Return True
            End If
            Return False
        End Function

        Public Overrides Sub Reset()
            _prgBankLo = 0
            _prgBankHi = _prgBanks - 1
        End Sub
    End Class

End Namespace