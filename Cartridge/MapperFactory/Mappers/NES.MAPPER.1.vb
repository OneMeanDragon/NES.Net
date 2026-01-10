Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' Mapper 001: MMC1 - Nintendo's first mapper chip with serial loading
    ''' Games: Legend of Zelda, Metroid, Kid Icarus
    ''' </summary>
    Public NotInheritable Class Mapper001
        Inherits MapperBase

        Public Overrides ReadOnly Property MapperName As String = "MMC1 (SxROM)"
        Public Overrides ReadOnly Property MapperNumber As Byte = 1

        ' Internal registers
        Private _loadRegister As Byte
        Private _loadCounter As Byte
        Private _controlReg As Byte

        ' Bank selection
        Private _chrBank4Lo As Byte
        Private _chrBank4Hi As Byte
        Private _chrBank8 As Byte
        Private _prgBank16Lo As Byte
        Private _prgBank16Hi As Byte
        Private _prgBank32 As Byte

        Public Sub New(prgBanks As Byte, chrBanks As Byte)
            MyBase.New(prgBanks, chrBanks)
            ' Allocate 8KB cart RAM
            _cartRam = New Memory(Of Byte)(New Byte(8191) {})
        End Sub

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function CpuMapRead(addr As UInt16, ByRef mappedAddr As UInt32, ByRef data As Byte) As Boolean
            ' Cart RAM ($6000-$7FFF)
            If addr >= &H6000US AndAlso addr <= &H7FFFUS Then
                mappedAddr = &HFFFFFFFFUI
                data = _cartRam.Span(addr And &H1FFFUS)
                Return True
            End If

            ' PRG ROM ($8000-$FFFF)
            If addr >= &H8000US Then
                If (_controlReg And &H8) <> 0 Then
                    ' 16KB mode
                    If addr < &HC000US Then
                        mappedAddr = CUInt(_prgBank16Lo) * &H4000UI + (addr And &H3FFFUI)
                    Else
                        mappedAddr = CUInt(_prgBank16Hi) * &H4000UI + (addr And &H3FFFUI)
                    End If
                Else
                    ' 32KB mode
                    mappedAddr = CUInt(_prgBank32) * &H8000UI + (addr And &H7FFFUI)
                End If
                Return True
            End If

            Return False
        End Function

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function CpuMapWrite(addr As UInt16, ByRef mappedAddr As UInt32, data As Byte) As Boolean
            ' Cart RAM write
            If addr >= &H6000US AndAlso addr <= &H7FFFUS Then
                mappedAddr = &HFFFFFFFFUI
                Dim span = _cartRam.Span
                span(addr And &H1FFFUS) = data
                '_cartRam.Span(addr And &H1FFFUS) = data
                'MemoryMarshal.Write(Of Byte)(_cartRam.Span.Slice(CInt(addr And &H1FFFUS), 1), data)
                Return True
            End If

            ' Control register writes
            If addr >= &H8000US Then
                If (data And &H80) <> 0 Then
                    ' Reset shift register
                    _loadRegister = 0
                    _loadCounter = 0
                    _controlReg = _controlReg Or &HC
                Else
                    ' Load bit serially (LSB first)
                    _loadRegister = (_loadRegister >> 1) Or ((data And &H1) << 4)
                    _loadCounter += 1

                    If _loadCounter = 5 Then
                        Dim target = (addr >> 13) And &H3

                        Select Case target
                            Case 0 ' Control ($8000-$9FFF)
                                _controlReg = _loadRegister And &H1F
                                _mirrorMode = CType(_controlReg And &H3, MirrorMode)

                            Case 1 ' CHR bank 0 ($A000-$BFFF)
                                If (_controlReg And &H10) <> 0 Then
                                    _chrBank4Lo = _loadRegister And &H1F
                                Else
                                    _chrBank8 = _loadRegister And &H1E
                                End If

                            Case 2 ' CHR bank 1 ($C000-$DFFF)
                                If (_controlReg And &H10) <> 0 Then
                                    _chrBank4Hi = _loadRegister And &H1F
                                End If

                            Case 3 ' PRG bank ($E000-$FFFF)
                                Dim prgMode = (_controlReg >> 2) And &H3
                                Select Case prgMode
                                    Case 0, 1 ' 32KB
                                        _prgBank32 = (_loadRegister And &HE) >> 1
                                    Case 2 ' Fix first, swap second
                                        _prgBank16Lo = 0
                                        _prgBank16Hi = _loadRegister And &HF
                                    Case 3 ' Swap first, fix last
                                        _prgBank16Lo = _loadRegister And &HF
                                        _prgBank16Hi = _prgBanks - 1
                                End Select
                        End Select

                        _loadRegister = 0
                        _loadCounter = 0
                    End If
                End If
            End If

            Return False
        End Function

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Overrides Function PpuMapRead(addr As UInt16, ByRef mappedAddr As UInt32) As Boolean
            If addr < &H2000US Then
                If _chrBanks = 0 Then
                    mappedAddr = addr
                    Return True
                End If

                If (_controlReg And &H10) <> 0 Then
                    ' 4KB mode
                    If addr < &H1000US Then
                        mappedAddr = CUInt(_chrBank4Lo) * &H1000UI + (addr And &HFFFUI)
                    Else
                        mappedAddr = CUInt(_chrBank4Hi) * &H1000UI + (addr And &HFFFUI)
                    End If
                Else
                    ' 8KB mode
                    mappedAddr = CUInt(_chrBank8) * &H2000UI + (addr And &H1FFFUI)
                End If
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
            _loadRegister = 0
            _loadCounter = 0
            _controlReg = &H1C

            _chrBank4Lo = 0
            _chrBank4Hi = 0
            _chrBank8 = 0

            _prgBank32 = 0
            _prgBank16Lo = 0
            _prgBank16Hi = _prgBanks - 1
        End Sub

        Public Overrides Function GetMirrorMode() As MirrorMode
            Return _mirrorMode
        End Function
    End Class

End Namespace