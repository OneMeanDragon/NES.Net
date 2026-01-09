Imports System.Runtime.CompilerServices

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' Mapper 000: NROM - No mapper hardware, simple direct mapping
    ''' Games: Donkey Kong, Mario Bros, etc.
    ''' </summary>
    Public NotInheritable Class Mapper000
        Inherits MapperBase

        Public Overrides ReadOnly Property MapperName As String = "NROM"
        Public Overrides ReadOnly Property MapperNumber As Byte = 0

        Public Sub New(prgBanks As Byte, chrBanks As Byte)
            MyBase.New(prgBanks, chrBanks)
        End Sub

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function CpuMapRead(addr As UInt16, ByRef mappedAddr As UInt32, ByRef data As Byte) As Boolean
            If addr >= &H8000US Then
                ' 16KB: Mirror, 32KB: Direct map
                mappedAddr = If(_prgBanks > 1, addr And &H7FFFUI, addr And &H3FFFUI)
                Return True
            End If
            Return False
        End Function

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function CpuMapWrite(addr As UInt16, ByRef mappedAddr As UInt32, data As Byte) As Boolean
            ' NROM has no writable PRG
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
                ' CHR-RAM is writable
                mappedAddr = addr
                Return True
            End If
            Return False
        End Function

        Public Overrides Sub Reset()
            ' Nothing to reset
        End Sub
    End Class

End Namespace