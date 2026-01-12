Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' Mapper 066: GxROM - Simple PRG+CHR banking
    ''' Games: SMB + Duck Hunt, various educational titles
    ''' </summary>
    Public NotInheritable Class Mapper066
        Inherits MapperBase

        Public Overrides ReadOnly Property MapperName As String = "GxROM"
        Public Overrides ReadOnly Property MapperNumber As Byte = 66

        Private _prgBank As Byte
        Private _chrBank As Byte

        Public Sub New(prgBanks As Byte, chrBanks As Byte)
            MyBase.New(prgBanks, chrBanks)
        End Sub

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function CpuMapRead(addr As UInt16, ByRef mappedAddr As UInt32, ByRef data As Byte) As Boolean
            If addr >= &H8000US Then
                ' 32KB bank
                mappedAddr = CUInt(_prgBank) * &H8000UI + (addr And &H7FFFUS)

                ' Bounds check
                Dim maxAddr = CUInt(_prgBanks) * 16384UI
                If mappedAddr >= maxAddr Then
                    mappedAddr = mappedAddr Mod maxAddr
                End If

                Return True
            End If
            Return False
        End Function

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function CpuMapWrite(addr As UInt16, ByRef mappedAddr As UInt32, data As Byte) As Boolean
            If addr >= &H8000US Then
                _chrBank = data And &H3UI
                _prgBank = (data >> 4) And &H3UI
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
            ' Fix to last bank
            _prgBank = If(_prgBanks >= 2, CByte((_prgBanks \ 2) - 1), CByte(0))
        End Sub
    End Class

End Namespace