Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' Mapper 003: CNROM - Simple CHR banking
    ''' Games: Solomon's Key, Arkanoid, Paperboy
    ''' </summary>
    Public NotInheritable Class Mapper003
        Inherits MapperBase

        Public Overrides ReadOnly Property MapperName As String = "CNROM"
        Public Overrides ReadOnly Property MapperNumber As Byte = 3

        Private _chrBank As Byte

        Public Sub New(prgBanks As Byte, chrBanks As Byte)
            MyBase.New(prgBanks, chrBanks)
        End Sub

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function CpuMapRead(addr As UInt16, ByRef mappedAddr As UInt32, ByRef data As Byte) As Boolean
            If addr >= &H8000US Then
                mappedAddr = If(_prgBanks = 1, addr And &H3FFFUS, addr And &H7FFFUS)
                Return True
            End If
            Return False
        End Function

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function CpuMapWrite(addr As UInt16, ByRef mappedAddr As UInt32, data As Byte) As Boolean
            If addr >= &H8000US Then
                _chrBank = data And &H3UI
            End If
            Return False
        End Function

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function PpuMapRead(addr As UInt16, ByRef mappedAddr As UInt32) As Boolean
            If addr < &H2000US Then
                mappedAddr = CUInt(_chrBank) * &H2000UI + addr
                Return True
            End If
            Return False
        End Function

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function PpuMapWrite(addr As UInt16, ByRef mappedAddr As UInt32) As Boolean
            Return False
        End Function

        Public Overrides Sub Reset()
            _chrBank = 0
        End Sub
    End Class

End Namespace