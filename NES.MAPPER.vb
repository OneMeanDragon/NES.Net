'deal with mapped_addr math where applicable

Namespace NintendoEntertainmentSystem

#Region "Base MAPPER Class"
    ''' <summary>
    ''' Base of the Mapper.
    ''' <para>Check the constructors:</para>
    ''' <para>-------------------------</para>
    ''' <para>Mapper Name</para>
    ''' <para>Mapper Info</para>
    ''' </summary>
    Public MustInherit Class clsMapper
        Protected m_PRGBanks As Byte = 0
        Protected m_CHRBanks As Byte = 0

        '066
        Protected nCHRBankSelect As Byte = &H0 '066, 003
        Protected nPRGBankSelect As Byte = &H0

        '004
        Protected nTargetRegister As Byte = &H0
        Protected bPRGBankMode As Boolean = False
        Protected bCHRInversion As Boolean = False
        Protected mirrormode As enMIRROR = enMIRROR.HORIZONTAL '001, 004
        Protected pRegister(8) As UInt32
        Protected pCHRBank(8) As UInt32
        Protected pPRGBank(4) As UInt32
        Protected bIRQActive As Boolean = False
        Protected bIRQEnable As Boolean = False
        Protected bIRQUpdate As Boolean = False
        Protected nIRQCounter As UInt16 = &H0
        Protected nIRQReload As UInt16 = &H0
        Protected vRAMStatic() As Byte                          '001, 004

        Protected nPRGBankSelectLo As Byte = &H0  '002
        Protected nPRGBankSelectHi As Byte = &H0  '002

        '001
        Protected nCHRBankSelect4Lo As Byte = &H0
        Protected nCHRBankSelect4Hi As Byte = &H0
        Protected nCHRBankSelect8 As Byte = &H0
        Protected nPRGBankSelect16Lo As Byte = &H0
        Protected nPRGBankSelect16Hi As Byte = &H0
        Protected nPRGBankSelect32 As Byte = &H0
        Protected nLoadRegister As Byte = &H0
        Protected nLoadRegisterCount As Byte = &H0
        Protected nControlRegister As Byte = &H0


        Protected m_name As String = ""
        Protected m_info As String = ""
        Public MustOverride Property Name() As String
        Public MustOverride Property Info() As String

        Public Enum enMIRROR
            HARDWARE
            HORIZONTAL
            VERTICAL
            ONESCREEN_LO
            ONESCREEN_HI
        End Enum

        Public Sub New(ByVal prgBanks As Byte, ByVal chrBanks As Byte)
            m_PRGBanks = prgBanks
            m_CHRBanks = chrBanks

            Reset()
        End Sub

        Protected Overrides Sub Finalize()
            MyBase.Finalize()
        End Sub

        ' Transform CPU bus address into PRG ROM offset
        Public MustOverride Function cpuMapRead(ByVal addr As UInt16, ByRef mapped_addr As UInt32, ByRef data As Byte) As Boolean
        Public MustOverride Function cpuMapWrite(ByVal addr As UInt16, ByRef mapped_addr As UInt32, ByVal Optional data As Byte = 0) As Boolean

        ' Transform PPU bus address into CHR ROM offset
        Public MustOverride Function ppuMapRead(ByVal addr As UInt16, ByRef mapped_addr As UInt32) As Boolean
        Public MustOverride Function ppuMapWrite(ByVal addr As UInt16, ByRef mapped_addr As UInt32) As Boolean

        ' Reset mapper to knowen state
        Public MustOverride Sub Reset()

        ' Get MIRROR mode if mapper is in control
        Public Overridable Function Mirror() As enMIRROR
            Return enMIRROR.HARDWARE
        End Function

        ' IRQ Interface
        Public Overridable Function irqState() As Boolean
            Return False
        End Function
        Public Overridable Sub irqClear()
        End Sub

        ' Scanline Counting
        Public Overridable Sub Scanline()
        End Sub

    End Class
#End Region

#Region "MAPPER_066"

    Public Class clsMapper_066 : Inherits clsMapper

        Public Overrides Property Name() As String
            Get
                Return m_name
            End Get
            Set(value As String)
                m_name = value
            End Set
        End Property

        Public Overrides Property Info As String
            Get
                Return m_info
            End Get
            Set(value As String)
                m_info = value
            End Set
        End Property

        Public Sub New(ByVal prgBanks As Byte, ByVal chrBanks As Byte)
            MyBase.New(prgBanks, chrBanks)
            Name = "GNROM switch"
            Info = ""
        End Sub

        Protected Overrides Sub Finalize()
            MyBase.Finalize()
        End Sub

        Public Overrides Sub Reset()
            nCHRBankSelect = &H0UI
            nPRGBankSelect = &H0UI
        End Sub

        Public Overrides Function cpuMapRead(addr As UShort, ByRef mapped_addr As UInteger, ByRef data As Byte) As Boolean
            If addr >= &H8000US AndAlso addr <= &HFFFFUS Then
                mapped_addr = MathHelpers.SafeAddition32(MathHelpers.SafeMul32(nPRGBankSelect, &H8000US), (addr And &H7FFFUS))
                Return True
            Else
                Return False
            End If
        End Function

        Public Overrides Function cpuMapWrite(addr As UShort, ByRef mapped_addr As UInteger, Optional data As Byte = 0) As Boolean
            If addr >= &H8000US AndAlso addr <= &HFFFFUS Then
                nCHRBankSelect = data And &H3UI
                nPRGBankSelect = MathHelpers.SafeShiftRight16((data And &H30UI), 4)
            End If
            Return False
        End Function

        Public Overrides Function ppuMapRead(addr As UShort, ByRef mapped_addr As UInteger) As Boolean
            If addr < &H2000US Then
                mapped_addr = MathHelpers.SafeAddition32(MathHelpers.SafeMul16(nCHRBankSelect, &H2000US), addr)
                Return True
            Else
                Return False
            End If
        End Function

        Public Overrides Function ppuMapWrite(addr As UShort, ByRef mapped_addr As UInteger) As Boolean
            Return False
        End Function
    End Class


#End Region

#Region "MAPPER_004"

    Public Class clsMapper_004 : Inherits clsMapper

        Public Overrides Property Name() As String
            Get
                Return m_name
            End Get
            Set(value As String)
                m_name = value
            End Set
        End Property

        Public Overrides Property Info As String
            Get
                Return m_info
            End Get
            Set(value As String)
                m_info = value
            End Set
        End Property

        Public Sub New(ByVal prgBanks As Byte, ByVal chrBanks As Byte)
            MyBase.New(prgBanks, chrBanks)
            Name = "Nintendo MMC3"
            Info = ""
            ReDim vRAMStatic((32 * KB_SIZE) - ZeroBasedArrays)
            For i As Integer = 0 To (32 * KB_SIZE) - ZeroBasedArrays
                vRAMStatic(i) = &H0UI
            Next
        End Sub

        Protected Overrides Sub Finalize()
            MyBase.Finalize()
        End Sub

        Public Overrides Function cpuMapRead(addr As UShort, ByRef mapped_addr As UInteger, ByRef data As Byte) As Boolean
            If addr >= &H6000US AndAlso addr <= &H7FFFUS Then
                mapped_addr = &HFFFFFFFFUI
                data = vRAMStatic(addr And &H1FFFUS)
                Return True
            End If
            If addr >= &H8000US AndAlso addr <= &H9FFFUS Then
                mapped_addr = MathHelpers.SafeAddition32(pPRGBank(0), (addr And &H1FFFUS))
                Return True
            End If
            If addr >= &HA000US AndAlso addr <= &HBFFFUS Then
                mapped_addr = MathHelpers.SafeAddition32(pPRGBank(1), (addr And &H1FFFUS))
                Return True
            End If
            If addr >= &HC000US AndAlso addr <= &HDFFFUS Then
                mapped_addr = MathHelpers.SafeAddition32(pPRGBank(2), (addr And &H1FFFUS))
                Return True
            End If
            If addr >= &HE000US AndAlso addr <= &HFFFFUS Then
                mapped_addr = MathHelpers.SafeAddition32(pPRGBank(3), (addr And &H1FFFUS))
                Return True
            End If
            Return False
        End Function

        Public Overrides Function cpuMapWrite(addr As UShort, ByRef mapped_addr As UInteger, Optional data As Byte = 0) As Boolean
            If addr >= &H6000US AndAlso addr <= &H7FFFUS Then
                mapped_addr = &HFFFFFFFFUI
                vRAMStatic(addr And &H1FFFUS) = data
                Return True
            End If
            If addr >= &H8000US AndAlso addr <= &H9FFFUS Then
                ' bank select
                If Not (addr And &H1US) Then
                    nTargetRegister = data And &H7UI
                    bPRGBankMode = data And &H40UI
                    bCHRInversion = data And &H80UI
                Else
                    ' update
                    pRegister(nTargetRegister) = data
                    ' update pointer table
                    If bCHRInversion Then
                        pCHRBank(0) = MathHelpers.SafeMul32(pRegister(2), &H400US)
                        pCHRBank(1) = MathHelpers.SafeMul32(pRegister(3), &H400US)
                        pCHRBank(2) = MathHelpers.SafeMul32(pRegister(4), &H400US)
                        pCHRBank(3) = MathHelpers.SafeMul32(pRegister(5), &H400US)
                        pCHRBank(4) = MathHelpers.SafeMul32((pRegister(0) And &HFEUI), &H400US)
                        pCHRBank(5) = MathHelpers.SafeAddition32(MathHelpers.SafeMul32(pRegister(0), &H400US), &H400US)
                        pCHRBank(6) = MathHelpers.SafeMul32((pRegister(1) And &HFEUI), &H400US)
                        pCHRBank(7) = MathHelpers.SafeAddition32(MathHelpers.SafeMul32(pRegister(1), &H400US), &H400US)
                    Else
                        pCHRBank(0) = MathHelpers.SafeMul32((pRegister(0) And &HFEUI), &H400US)
                        pCHRBank(1) = MathHelpers.SafeAddition32(MathHelpers.SafeMul32(pRegister(0), &H400US), &H400US)
                        pCHRBank(2) = MathHelpers.SafeMul32((pRegister(1) And &HFEUI), &H400US)
                        pCHRBank(3) = MathHelpers.SafeAddition32(MathHelpers.SafeMul32(pRegister(1), &H400US), &H400US)
                        pCHRBank(4) = MathHelpers.SafeMul32(pRegister(2), &H400US)
                        pCHRBank(5) = MathHelpers.SafeMul32(pRegister(3), &H400US)
                        pCHRBank(6) = MathHelpers.SafeMul32(pRegister(4), &H400US)
                        pCHRBank(7) = MathHelpers.SafeMul32(pRegister(5), &H400US)
                    End If
                    If bPRGBankMode Then
                        pPRGBank(2) = MathHelpers.SafeMul32((pRegister(6) And &H3FUI), &H2000US)
                        pPRGBank(0) = MathHelpers.SafeMul32(MathHelpers.SafeSubtract32(MathHelpers.SafeMul32(m_PRGBanks, 2), 2), &H2000US)
                    Else
                        pPRGBank(0) = MathHelpers.SafeMul32((pRegister(6) And &H3FUI), &H2000US)
                        pPRGBank(2) = MathHelpers.SafeMul32(MathHelpers.SafeSubtract32(MathHelpers.SafeMul32(m_PRGBanks, 2), 2), &H2000US)
                    End If
                    pPRGBank(1) = MathHelpers.SafeMul32((pRegister(7) And &H3FUI), &H2000US)
                    pPRGBank(3) = MathHelpers.SafeMul32(MathHelpers.SafeSubtract32(MathHelpers.SafeMul32(m_PRGBanks, 2), 1), &H2000US)
                End If
                Return False
            End If
            If addr >= &HA000US AndAlso addr <= &HBFFFUS Then
                If Not (addr And &H1US) Then
                    ' mirroring
                    If data And &H1UI Then
                        mirrormode = enMIRROR.HORIZONTAL
                    Else
                        mirrormode = enMIRROR.VERTICAL
                    End If
                Else
                    ' PRG RAM Protect
                    ' TODO
                End If
                Return False
            End If
            If addr >= &HC000US AndAlso addr <= &HDFFFUS Then
                If Not (addr And &H1US) Then
                    nIRQReload = data
                Else
                    nIRQCounter = &H0US
                End If
                Return False
            End If
            If addr >= &HE000US AndAlso addr <= &HFFFFUS Then
                If Not (addr And &H1US) Then
                    bIRQEnable = False
                    bIRQActive = False
                Else
                    bIRQEnable = True
                End If
                Return False
            End If
            Return False
        End Function

        Public Overrides Function ppuMapRead(addr As UInt16, ByRef mapped_addr As UInt32) As Boolean
            If addr >= &H0US AndAlso addr <= &H3FFUS Then
                mapped_addr = MathHelpers.SafeAddition32(pCHRBank(0), (addr And &H3FFUS))
                Return True
            End If
            If addr >= &H400US AndAlso addr <= &H7FFUS Then
                mapped_addr = MathHelpers.SafeAddition32(pCHRBank(1), (addr And &H3FFUS))
                Return True
            End If
            If addr >= &H800US AndAlso addr <= &HBFFUS Then
                mapped_addr = MathHelpers.SafeAddition32(pCHRBank(2), (addr And &H3FFUS))
                Return True
            End If
            If addr >= &HC00US AndAlso addr <= &HFFFUS Then
                mapped_addr = MathHelpers.SafeAddition32(pCHRBank(3), (addr And &H3FFUS))
                Return True
            End If
            If addr >= &H1000US AndAlso addr <= &H13FFUS Then
                mapped_addr = MathHelpers.SafeAddition32(pCHRBank(4), (addr And &H3FFUS))
                Return True
            End If
            If addr >= &H1400US AndAlso addr <= &H17FFUS Then
                mapped_addr = MathHelpers.SafeAddition32(pCHRBank(5), (addr And &H3FFUS))
                Return True
            End If
            If addr >= &H1800US AndAlso addr <= &H1BFFUS Then
                mapped_addr = MathHelpers.SafeAddition32(pCHRBank(6), (addr And &H3FFUS))
                Return True
            End If
            If addr >= &H1C00US AndAlso addr <= &H1FFFUS Then
                mapped_addr = MathHelpers.SafeAddition32(pCHRBank(7), (addr And &H3FFUS))
                Return True
            End If
            Return False
        End Function

        Public Overrides Function ppuMapWrite(addr As UShort, ByRef mapped_addr As UInteger) As Boolean
            Return False
        End Function

        Public Overrides Sub Reset()
            nTargetRegister = &H0
            bPRGBankMode = False
            bCHRInversion = False
            mirrormode = enMIRROR.HORIZONTAL

            bIRQActive = False
            bIRQEnable = False
            bIRQUpdate = False
            nIRQCounter = &H0US
            nIRQReload = &H0US

            For Offset As UInt32 = 0 To 3 : pPRGBank(Offset) = &H0 : Next
            For Offset As UInt32 = 0 To 7 : pCHRBank(Offset) = &H0 : pRegister(Offset) = &H0 : Next

            pPRGBank(0) = MathHelpers.SafeMul32(0, &H2000US)
            pPRGBank(1) = MathHelpers.SafeMul32(1, &H2000US)
            pPRGBank(2) = MathHelpers.SafeMul32(MathHelpers.SafeSubtract32(MathHelpers.SafeMul32(m_PRGBanks, 2), 2), &H2000US)
            pPRGBank(3) = MathHelpers.SafeMul32(MathHelpers.SafeSubtract32(MathHelpers.SafeMul32(m_PRGBanks, 2), 1), &H2000US)
        End Sub

        Public Overrides Function irqState() As Boolean
            Return bIRQActive
        End Function

        Public Overrides Sub irqClear()
            bIRQActive = False
        End Sub

        Public Overrides Sub Scanline()
            If nIRQCounter = 0 Then
                nIRQCounter = nIRQReload
            Else
                nIRQCounter = MathHelpers.SafeDecrement16(nIRQCounter) '-= 1
            End If
            If nIRQCounter = 0 AndAlso bIRQEnable Then
                bIRQActive = True
            End If
        End Sub

        Public Overrides Function Mirror() As enMIRROR
            Return mirrormode
        End Function

    End Class


#End Region

#Region "MAPPER_003"

    Public Class clsMapper_003 : Inherits clsMapper

        Public Overrides Property Name() As String
            Get
                Return m_name
            End Get
            Set(value As String)
                m_name = value
            End Set
        End Property

        Public Overrides Property Info As String
            Get
                Return m_info
            End Get
            Set(value As String)
                m_info = value
            End Set
        End Property

        Public Sub New(ByVal prgBanks As Byte, ByVal chrBanks As Byte)
            MyBase.New(prgBanks, chrBanks)
            Name = "UNROM switch"
            Info = ""
        End Sub

        Protected Overrides Sub Finalize()
            MyBase.Finalize()
        End Sub

        Public Overrides Sub Reset()
            nCHRBankSelect = &H0UI
        End Sub

        Public Overrides Function cpuMapRead(addr As UShort, ByRef mapped_addr As UInteger, ByRef data As Byte) As Boolean
            If addr >= &H8000US AndAlso addr <= &HFFFFUS Then
                If m_PRGBanks = 1 Then ' 16k
                    mapped_addr = addr And &H3FFFUS
                End If
                If m_PRGBanks = 2 Then ' 32k
                    mapped_addr = addr And &H7FFFUS
                End If
                Return True
            End If
            Return False
        End Function

        Public Overrides Function cpuMapWrite(addr As UShort, ByRef mapped_addr As UInteger, Optional data As Byte = 0) As Boolean
            If addr >= &H8000US AndAlso addr <= &HFFFFUS Then
                nCHRBankSelect = data And &H3
                mapped_addr = addr
            End If
            ' write do not update rom
            Return False
        End Function

        Public Overrides Function ppuMapRead(addr As UShort, ByRef mapped_addr As UInteger) As Boolean
            If addr < &H2000US Then
                mapped_addr = MathHelpers.SafeAddition32(MathHelpers.SafeMul32(nCHRBankSelect, &H2000US), addr)
                Return True
            End If
            Return False
        End Function

        Public Overrides Function ppuMapWrite(addr As UShort, ByRef mapped_addr As UInteger) As Boolean
            Return False
        End Function
    End Class


#End Region

#Region "MAPPER_002"

    Public Class clsMapper_002 : Inherits clsMapper

        Public Overrides Property Name() As String
            Get
                Return m_name
            End Get
            Set(value As String)
                m_name = value
            End Set
        End Property

        Public Overrides Property Info As String
            Get
                Return m_info
            End Get
            Set(value As String)
                m_info = value
            End Set
        End Property

        Public Sub New(ByVal prgBanks As Byte, ByVal chrBanks As Byte)
            MyBase.New(prgBanks, chrBanks)
            Name = "CNROM switch"
            Info = ""
        End Sub

        Protected Overrides Sub Finalize()
            MyBase.Finalize()
        End Sub

        Public Overrides Sub Reset()
            nPRGBankSelectLo = &H0UI
            nPRGBankSelectHi = m_PRGBanks - 1
        End Sub

        Public Overrides Function cpuMapRead(addr As UShort, ByRef mapped_addr As UInteger, ByRef data As Byte) As Boolean
            If addr >= &H8000US AndAlso addr <= &HBFFFUS Then
                mapped_addr = MathHelpers.SafeAddition32(MathHelpers.SafeMul32(nPRGBankSelectLo, &H4000US), (addr And &H3FFFUS))
                Return True
            End If
            If addr >= &HC000US AndAlso addr <= &HFFFFUS Then
                mapped_addr = MathHelpers.SafeAddition32(MathHelpers.SafeMul32(nPRGBankSelectHi, &H4000US), (addr And &H3FFFUS))
                Return True
            End If
            Return False
        End Function

        Public Overrides Function cpuMapWrite(addr As UShort, ByRef mapped_addr As UInteger, Optional data As Byte = 0) As Boolean
            If addr >= &H8000US AndAlso addr <= &HFFFFUS Then
                nPRGBankSelectLo = data And &HFUI
            End If
            Return False
        End Function

        Public Overrides Function ppuMapRead(addr As UShort, ByRef mapped_addr As UInteger) As Boolean
            If addr < &H2000US Then
                mapped_addr = addr
                Return True
            Else
                Return False
            End If
        End Function

        Public Overrides Function ppuMapWrite(addr As UShort, ByRef mapped_addr As UInteger) As Boolean
            If addr < &H2000US Then
                If m_CHRBanks = 0 Then
                    mapped_addr = addr
                    Return True
                End If
            End If
            Return False
        End Function
    End Class

#End Region

#Region "MAPPER_001"

    Public Class clsMapper_001 : Inherits clsMapper

        Public Overrides Property Name() As String
            Get
                Return m_name
            End Get
            Set(value As String)
                m_name = value
            End Set
        End Property

        Public Overrides Property Info As String
            Get
                Return m_info
            End Get
            Set(value As String)
                m_info = value
            End Set
        End Property

        Public Sub New(ByVal prgBanks As Byte, ByVal chrBanks As Byte)
            MyBase.New(prgBanks, chrBanks)
            Name = "Nintendo MMC1"
            Info = ""
            ReDim vRAMStatic((32 * KB_SIZE) - ZeroBasedArrays)
            For i As UInt32 = 0 To (32 * KB_SIZE) - ZeroBasedArrays
                vRAMStatic(i) = &H0
            Next
        End Sub

        Protected Overrides Sub Finalize()
            MyBase.Finalize()
        End Sub

        Public Overrides Function cpuMapRead(addr As UShort, ByRef mapped_addr As UInt32, ByRef data As Byte) As Boolean
            If addr >= &H6000US AndAlso addr <= &H7FFFUS Then
                ' read is from static ram on the cartridge
                mapped_addr = &HFFFFFFFFUI
                ' read data from ram
                data = vRAMStatic(addr And &H1FFFUS)
                ' handled
                Return True
            End If
            If addr >= &H8000US Then
                If nControlRegister And &H8 Then 'b1000
                    ' 16K Mode
                    If addr >= &H8000US AndAlso addr <= &H8FFFUS Then
                        mapped_addr = MathHelpers.SafeAddition32(MathHelpers.SafeMul32(nPRGBankSelect16Lo, &H4000US), (addr And &H3FFFUS))
                        Return True
                    End If
                    If addr >= &HC000US AndAlso addr <= &HFFFFUS Then
                        mapped_addr = MathHelpers.SafeAddition32(MathHelpers.SafeMul32(nPRGBankSelect16Hi, &H4000US), (addr And &H3FFFUS))
                        Return True
                    End If
                Else
                    ' 32K Mode
                    mapped_addr = MathHelpers.SafeAddition32(MathHelpers.SafeMul32(nPRGBankSelect32, &H8000US), (addr And &H7FFFUS))
                    Return True
                End If
            End If
            Return False
        End Function

        Public Overrides Function cpuMapWrite(addr As UShort, ByRef mapped_addr As UInteger, Optional data As Byte = 0) As Boolean
            If addr >= &H6000US AndAlso addr <= &H7FFFUS Then
                ' write is to static ram on the cartridge
                mapped_addr = &HFFFFFFFFUI
                ' write data to ram
                vRAMStatic(addr And &H1FFFUS) = data
                ' handled
                Return True
            End If
            If addr >= &H8000US Then
                If data And &H80 Then
                    ' MSB is set, so reset serial loading
                    nLoadRegister = &H0
                    nLoadRegisterCount = &H0
                    nControlRegister = nControlRegister Or &HC
                Else
                    '// Load data in serially into load register
                    '// It arrives LSB first, so implant this at
                    '// bit 5. After 5 writes, the register Is ready
                    nLoadRegister >>= 1
                    nLoadRegister = nLoadRegister Or MathHelpers.SafeShiftLeft8((data And &H1), 4)
                    nLoadRegisterCount += 1

                    If nLoadRegisterCount = 5 Then
                        '// Get Mapper Target Register, by examining
                        '// bits 13 & 14 of the address
                        Dim nTargetRegister As Byte = MathHelpers.SafeShiftRight16(addr, 13) And &H3

                        If nTargetRegister = 0 Then '// 0x8000 - 0x9FFF

                            nControlRegister = nLoadRegister And &H1F
                            Select Case nControlRegister And &H3
                                Case 0 : mirrormode = enMIRROR.ONESCREEN_LO : Exit Select
                                Case 1 : mirrormode = enMIRROR.ONESCREEN_HI : Exit Select
                                Case 2 : mirrormode = enMIRROR.VERTICAL : Exit Select
                                Case 3 : mirrormode = enMIRROR.HORIZONTAL : Exit Select
                            End Select

                        ElseIf nTargetRegister = 1 Then '// 0xA000 - 0xBFFF

                            '// Set CHR Bank Lo
                            If nControlRegister And &H10 Then 'b10000
                                '// 4K CHR Bank at PPU 0x0000
                                nCHRBankSelect4Lo = nLoadRegister And &H1F
                            Else
                                '// 8K CHR Bank at PPU 0x0000
                                nCHRBankSelect8 = nLoadRegister And &H1E
                            End If

                        ElseIf nTargetRegister = 2 Then '// 0xC000 - 0xDFFF

                            '// Set CHR Bank Hi
                            If nControlRegister And &H10 Then 'b10000
                                '// 4K CHR Bank at PPU 0x1000
                                nCHRBankSelect4Hi = nLoadRegister And &H1F
                            End If

                        ElseIf nTargetRegister = 3 Then '// 0xE000 - 0xFFFF

                            '// Configure PRG Banks
                            Dim nPRGMode As Byte = MathHelpers.SafeShiftRight8(nControlRegister, 2) And &H3
                            If nPRGMode = 0 OrElse nPRGMode = 1 Then
                                '// Set 32K PRG Bank at CPU 0x8000
                                nPRGBankSelect32 = MathHelpers.SafeShiftRight8((nLoadRegister And &HE), 1)
                            ElseIf nPRGMode = 2 Then
                                '// Fix 16KB PRG Bank at CPU 0x8000 to First Bank
                                nPRGBankSelect16Lo = 0
                                '// Set 16KB PRG Bank at CPU 0xC000
                                nPRGBankSelect16Hi = nLoadRegister And &HF
                            ElseIf nPRGMode = 3 Then
                                '// Set 16KB PRG Bank at CPU 0x8000
                                nPRGBankSelect16Lo = nLoadRegister And &HF
                                '// Fix 16KB PRG Bank at CPU 0xC000 to Last Bank
                                nPRGBankSelect16Hi = MathHelpers.SafeDecrementByte(m_PRGBanks) ' - 1 'should check to see if m_PRGBanks = 0 but whatever
                            End If

                        End If

                        '// 5 bits were written, And decoded, so
                        '// reset load register
                        nLoadRegister = &H0
                        nLoadRegisterCount = 0

                    End If
                End If
            End If
            '// Mapper has handled write, but do Not update ROMs
            Return False
        End Function

        Public Overrides Function ppuMapRead(addr As UShort, ByRef mapped_addr As UInteger) As Boolean
            If addr < &H2000US Then
                If m_CHRBanks = 0 Then
                    mapped_addr = addr
                    Return True
                Else
                    If (nControlRegister And &H10) = &H10 Then 'b10000
                        ' 4K CHR Bank Mode
                        If addr >= &H0 AndAlso addr <= &HFFFUS Then
                            mapped_addr = MathHelpers.SafeAddition32(MathHelpers.SafeMul32(nCHRBankSelect4Lo, &H1000US), (addr And &HFFFUS))
                            Return True
                        End If
                        If addr >= &H1000US AndAlso addr <= &H1FFFUS Then
                            mapped_addr = MathHelpers.SafeAddition32(MathHelpers.SafeMul32(nCHRBankSelect4Lo, &H1000US), (addr And &HFFFUS))
                            Return True
                        End If
                    Else
                        ' 8K CHR Bank Mode
                        mapped_addr = MathHelpers.SafeAddition32(MathHelpers.SafeMul32(nCHRBankSelect8, &H2000US), (addr And &H1FFFUS))
                        Return True
                    End If
                End If
            End If
            Return False
        End Function

        Public Overrides Function ppuMapWrite(addr As UShort, ByRef mapped_addr As UInteger) As Boolean
            If addr < &H2000US Then
                If m_CHRBanks = 0 Then
                    mapped_addr = addr
                    Return True
                End If
                Return True
            Else
                Return False
            End If
        End Function

        Public Overrides Sub Reset()
            nControlRegister = &H1CUI
            nLoadRegister = &H0UI
            nLoadRegisterCount = &H0UI

            nCHRBankSelect4Lo = 0
            nCHRBankSelect4Hi = 0
            nCHRBankSelect8 = 0

            nPRGBankSelect32 = 0
            nPRGBankSelect16Lo = 0
            nPRGBankSelect16Hi = m_PRGBanks - 1 'if 0
        End Sub

        Public Overrides Function Mirror() As enMIRROR
            Return mirrormode
        End Function

    End Class

#End Region

#Region "MAPPER_000"
    Public Class clsMapper_000 : Inherits clsMapper

        Public Overrides Property Name() As String
            Get
                Return m_name
            End Get
            Set(value As String)
                m_name = value
            End Set
        End Property

        Public Overrides Property Info As String
            Get
                Return m_info
            End Get
            Set(value As String)
                m_info = value
            End Set
        End Property

        Public Sub New(ByVal prgBanks As Byte, ByVal chrBanks As Byte)
            MyBase.New(prgBanks, chrBanks)
            Name = "No mapper, Mapper 000"
            Info = ""
        End Sub

        Protected Overrides Sub Finalize()
            MyBase.Finalize()
        End Sub

        Public Overrides Function cpuMapRead(ByVal addr As UInt16, ByRef mapped_addr As UInt32, ByRef data As Byte) As Boolean
            '// if PRGROM Is 16KB
            '//     CPU Address Bus          PRG ROM
            '//     0x8000 -> 0xBFFF: Map    0x0000 -> 0x3FFF
            '//     0xC000 -> 0xFFFF: Mirror 0x0000 -> 0x3FFF
            '// if PRGROM Is 32KB
            '//     CPU Address Bus          PRG ROM
            '//     0x8000 -> 0xFFFF: Map    0x0000 -> 0x7FFF	

            If addr >= &H8000US AndAlso addr <= &HFFFFUS Then
                If m_PRGBanks > 1 Then
                    mapped_addr = addr And &H7FFFUS
                Else
                    mapped_addr = addr And &H3FFFUS
                End If
                Return True
            End If
            Return False
        End Function

        Public Overrides Function cpuMapWrite(ByVal addr As UInt16, ByRef mapped_addr As UInt32, ByVal Optional data As Byte = 0) As Boolean
            If addr >= &H8000US AndAlso addr <= &HFFFFUS Then
                If m_PRGBanks > 1 Then
                    mapped_addr = addr And &H7FFFUS
                Else
                    mapped_addr = addr And &H3FFFUS
                End If
                Return True
            End If
            Return False
        End Function

        Public Overrides Function ppuMapRead(ByVal addr As UInt16, ByRef mapped_addr As UInt32) As Boolean
            '// There Is no mapping required for PPU
            '// PPU Address Bus          CHR ROM
            '// 0x0000 -> 0x1FFF: Map    0x0000 -> 0x1FFF
            If addr >= &H0US AndAlso addr <= &H1FFFUS Then
                mapped_addr = addr
                Return True
            End If
            Return False
        End Function

        Public Overrides Function ppuMapWrite(ByVal addr As UInt16, ByRef mapped_addr As UInt32) As Boolean
            If addr >= &H0US AndAlso addr <= &H1FFFUS Then
                If m_CHRBanks = 0 Then
                    mapped_addr = addr
                    Return True
                End If
            End If
            Return False
        End Function

        Public Overrides Sub Reset()
            'Does nothing
        End Sub

    End Class
#End Region

End Namespace