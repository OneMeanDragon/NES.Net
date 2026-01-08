

Imports System.IO
Imports System.Runtime.InteropServices 'For Structure Import / Export


Namespace NintendoEntertainmentSystem
    Public Module CartridgeGlobal
        Public Cart As clsCartridge
    End Module

#Region "Cartridge Class #2 [will be implemented soon as the draws and the audio is working]"
    <StructLayout(LayoutKind.Sequential, Pack:=1, Size:=16)>
    Public Structure CartridgeHeadderDataStruct
        <MarshalAs(UnmanagedType.ByValArray, SizeConst:=4)> Public name() As Byte
        Public prg_rom_chunks As Byte
        Public chr_rom_chunks As Byte
        Public mapper1 As Byte
        Public mapper2 As Byte
        Public prg_ram_size As Byte
        Public tv_system1 As Byte
        Public tv_system2 As Byte
        <MarshalAs(UnmanagedType.ByValArray, SizeConst:=5)> Public unused() As Byte
    End Structure
    Public Structure CartridgeData
        Public header As CartridgeHeadderDataStruct
        'the data
    End Structure
    Public Class CartridgeClass

        ' Second Take 

    End Class
#End Region

    Public Class clsCartridge
        Private Const iNES_HEADER_SIZE As UInt32 = (16)
        Private Const PRGBANK_SIZE As UInt32 = ((16 * KB_SIZE))    '16kb bank size
        Private Const CHRBANK_SIZE As UInt32 = ((8 * KB_SIZE))     ' 8kb bank size
        Private Const TRAINER_SIZE As UInt32 = (512)

        Private vTrainerMemory() As Byte
        Private vPRGMemory() As Byte
        Private vCHRMemory() As Byte

        Private FileContents() As Byte 'Keep in memory for reseting data?
        Private Property CurrentFilePosition() As UInt32

        Private m_ntsc As Boolean = True
        Public ReadOnly Property NTSC() As Boolean
            Get
                Return m_ntsc
            End Get
        End Property

        Private m_ValidImage As Boolean = False
        Public ReadOnly Property ValidImage() As Boolean
            Get
                Return m_ValidImage
            End Get
        End Property

        Private pMAPPER As clsMapper

        Public ReadOnly Property GetMapper() As clsMapper
            Get
                Return pMAPPER
            End Get
        End Property


        Private Structure iNESHeaderData
            <VBFixedArray(4)> Public name() As Byte
            Public prg_rom_chunks As Byte
            Public chr_rom_chunks As Byte
            Public mapper1 As Byte
            Public mapper2 As Byte
            Public prg_ram_size As Byte
            Public tv_system1 As Byte
            Public tv_system2 As Byte
            <VBFixedArray(5)> Public unused() As Byte
        End Structure
        Private GameFileHeader As New iNESHeaderData()

        Private hw_mirror As clsMapper.enMIRROR = clsMapper.enMIRROR.HORIZONTAL

        Public ReadOnly Property Mirror() As clsMapper.enMIRROR
            Get
                Dim m As clsMapper.enMIRROR = pMAPPER.Mirror()
                If m = clsMapper.enMIRROR.HARDWARE Then
                    Return hw_mirror
                Else
                    Return m
                End If
            End Get
        End Property

        Private Enum enVersion
            iNES1 = 1
            iNES2 = 2
        End Enum
        Private iNESVersion As enVersion = enVersion.iNES1
        Private nMapperID As Byte = 0
        Public ReadOnly Property MapperID() As UInt32
            Get
                Return nMapperID
            End Get
        End Property

        Private Property GameCart() As String
        Private Property PRGBanks() As Byte
        Private Property CHRBanks() As Byte

        Private Function InitHeader() As Boolean
            Try
                m_ValidImage = False
                ReDim GameFileHeader.name(4)
                ReDim GameFileHeader.unused(5)
                Return True
            Catch ex As Exception
                Return False
            End Try
        End Function
        Public Sub New()
            'Initialize the header structure
            If Not InitHeader() Then
                'This is a critical error
                Dim erOut As New ErrorMessages.clsErrorWindow("Failed to Initialize the iNES header structure, report this error to the devs.")
            End If
            'we will load the game from Load after validation
        End Sub

        Public Sub New(ByVal FileName As String)
            If Not InitHeader() Then
                'This is a critical error
                Dim erOut As New ErrorMessages.clsErrorWindow("Failed to Initialize the iNES header structure, report this error to the devs.")
            End If

            ' Set our game file
            GameCart = FileName

            ' Load up the data into memory
            Dim eMessageOut As String = Load()
            If Not (eMessageOut = "") Then
                Dim erOut As New ErrorMessages.clsErrorWindow(eMessageOut, False)
            End If
        End Sub

        Public Sub LoadCartridge(ByVal FileName As String)
            ' Set our game file
            GameCart = FileName

            ' Load up the data into memory
            Dim eMessageOut As String = Load()
            If Not (eMessageOut = "") Then
                Dim erOut As New ErrorMessages.clsErrorWindow(eMessageOut, False)
            End If
        End Sub

        Private Sub PopulateHeader()
            With GameFileHeader
                .name = {FileContents(0), FileContents(1), FileContents(2), FileContents(3)}
                .prg_rom_chunks = FileContents(4)
                .chr_rom_chunks = FileContents(5)
                .mapper1 = FileContents(6)
                .mapper2 = FileContents(7)
                .prg_ram_size = FileContents(8)
                .tv_system1 = FileContents(9)
                .tv_system2 = FileContents(10)
                .unused = {FileContents(11), FileContents(12), FileContents(13), FileContents(14), FileContents(15)}
            End With
        End Sub

        Private Function FileValidation() As Boolean
            If (GameFileHeader.name(0) = 78UI) AndAlso    'N
                (GameFileHeader.name(1) = 69UI) AndAlso   'E
                (GameFileHeader.name(2) = 83UI) AndAlso   'S
                (GameFileHeader.name(3) = 26UI) Then      '<EOF>
                ' Set if the cart is NTSC or PAL
                If (GameFileHeader.tv_system1 And &H1) = &H1 Then 'NTSC bit 0 is set as 1
                    m_ntsc = True
                Else
                    m_ntsc = False
                End If
                Return True
            Else
                Return False 'Invalid Head Name
            End If
        End Function

        Private Sub SetFileVersion()
            If (GameFileHeader.mapper2 And &HC) = &H8 Then
                iNESVersion = enVersion.iNES2
            Else
                iNESVersion = enVersion.iNES1
            End If
        End Sub

        Private Sub SetMapperID()
            nMapperID = ((GameFileHeader.mapper2 >> 4) << 4) Or (GameFileHeader.mapper1 >> 4)
        End Sub

        Private Sub SetMirrorMode()
            If GameFileHeader.mapper1 And &H1 Then
                hw_mirror = clsMapper.enMIRROR.VERTICAL
            Else
                hw_mirror = clsMapper.enMIRROR.HORIZONTAL
            End If
        End Sub

        Private Function SetBankSizes() As Boolean
            Select Case iNESVersion
                Case enVersion.iNES1
                    PRGBanks = GameFileHeader.prg_rom_chunks
                    CHRBanks = GameFileHeader.chr_rom_chunks
                Case enVersion.iNES2
                    PRGBanks = ((GameFileHeader.prg_ram_size And &H7) << 8) Or GameFileHeader.prg_rom_chunks
                    CHRBanks = ((GameFileHeader.prg_ram_size And &H38) << 8) Or GameFileHeader.chr_rom_chunks
                Case Else
                    Return False 'Satisfy that compiler
            End Select
            Return True
        End Function

        Public Function ContainsTrainer() As Boolean
            If GameFileHeader.mapper1 And &H4 Then
                Return True
            Else
                Return False
            End If
        End Function

        Public Function PopulateTrainerData() As Boolean
            Try
                ' Check size vs. file length
                If ((iNES_HEADER_SIZE) + (TRAINER_SIZE)) < FileContents.Length() Then Return False

                ReDim vTrainerMemory(TRAINER_SIZE - 1)

                For Offset As UInt32 = 0 To TRAINER_SIZE - 1  ' ← CHANGED: Added - 1 to loop end
                    vTrainerMemory(Offset) = FileContents(Offset + CurrentFilePosition())
                Next

                CurrentFilePosition = CurrentFilePosition + (TRAINER_SIZE)

                Return True
            Catch ex As Exception
                Return False
            End Try
        End Function

        Private Function PopulateDataBanks() As Boolean
            Try
                'Test length of both banks at the same time
                If ContainsTrainer() Then 'account for trainer size
                    If ((PRGBanks * (PRGBANK_SIZE)) + (CHRBanks * (CHRBANK_SIZE)) + (iNES_HEADER_SIZE) + (TRAINER_SIZE)) < FileContents.Length() Then Return False
                Else
                    If ((PRGBanks * (PRGBANK_SIZE)) + (CHRBanks * (CHRBANK_SIZE)) + (iNES_HEADER_SIZE)) < FileContents.Length() Then Return False
                End If

                Dim CurrentIndex As UInt32
                Dim EndIndex As UInt32
                If iNESVersion = enVersion.iNES1 Then
                    ' FIX #1: Subtract 1 from array size
                    ReDim vPRGMemory((PRGBanks * 16384) - 1)

                    CurrentIndex = CurrentFilePosition()
                    EndIndex = CurrentFilePosition() + (PRGBanks * 16384) - 1
                    For Offset As UInt32 = CurrentIndex To EndIndex
                        vPRGMemory(Offset - CurrentIndex) = FileContents(Offset) : CurrentFilePosition() += 1
                    Next

                    If CHRBanks = 0 Then
                        ' FIX #2: Subtract 1 from array size
                        ReDim vCHRMemory(8192 - 1)  ' ← CHANGED: Added - 1
                    Else
                        ' FIX #3: Subtract 1 from array size
                        ReDim vCHRMemory((CHRBanks * 8192) - 1)  ' ← CHANGED: Added - 1
                    End If

                    CurrentIndex = CurrentFilePosition()
                    EndIndex = CurrentFilePosition() + (IIf(CHRBanks = 0, 1, CHRBanks) * 8192) - 1
                    If CurrentIndex = FileContents.Length() Then
                        For Offset As UInt32 = CurrentIndex To EndIndex
                            vCHRMemory(Offset - CurrentIndex) = 0
                        Next
                    Else
                        For Offset As UInt32 = CurrentIndex To EndIndex
                            vCHRMemory(Offset - CurrentIndex) = FileContents(Offset) : CurrentFilePosition() += 1
                        Next
                    End If
                Else
                    ' FIX #4: iNES 2.0 - Subtract 1 from array size
                    ReDim vPRGMemory((PRGBanks * 16384) - 1)

                    CurrentIndex = CurrentFilePosition()
                    EndIndex = CurrentFilePosition() + (PRGBanks * 16384) - 1
                    For Offset As UInt32 = CurrentIndex To EndIndex
                        vPRGMemory(Offset - CurrentIndex) = FileContents(Offset) : CurrentFilePosition() += 1
                    Next

                    ' FIX #5: iNES 2.0 CHR - Subtract 1 from array size
                    ReDim vCHRMemory((CHRBanks * 8192) - 1)

                    CurrentIndex = CurrentFilePosition()
                    EndIndex = CurrentFilePosition() + (IIf(PRGBanks = 0, 1, PRGBanks) * 8192) - 1
                    For Offset As UInt32 = CurrentIndex To EndIndex
                        vCHRMemory(Offset - CurrentIndex) = FileContents(Offset) : CurrentFilePosition() += 1
                    Next
                End If
                'Hex Dump The Memory Must See... 'vPRGMemory, vCHRMemory

                Return True
            Catch ex As Exception
                Return False
            End Try
        End Function

        Private Function Load() As String
            Const ErInvalidHeader As String = "Invalid File Type."
            Const ErGameFileVerification As String = "Game Failed the Verification, try a different game."
            Const ErUnknowenFileVersion As String = "Unknowen iNES Version."
            Const ErFailedToRetrieveTrainerData As String = "Game contains an invalid trainer data set."
            Const ErPossibleFileCorrupted As String = "Game file could not be loaded, Length mismatch."
            Const ErUnsupportedMapperType As String = "MapperType not currently Supported"

            ' Fill the game file data array
            FileContents = File.ReadAllBytes(GameCart) 'My.Computer.FileSystem.ReadAllBytes(GameCart)
            If FileContents.Length() < (iNES_HEADER_SIZE) Then
                Return ErInvalidHeader & " EID=S({" & FileContents.Length().ToString() & ", 16})"
            End If

            ' populate the header
            PopulateHeader()
            CurrentFilePosition = (iNES_HEADER_SIZE)

            ' Validate game
            If Not FileValidation() Then
                Return ErGameFileVerification
            End If

            ' Check and Set Cartridge File Version
            SetFileVersion()

            ' SetMapperID
            SetMapperID()

            ' SetMirrorMode
            SetMirrorMode()

            ' Contains Trainer Data? [Must be loaded before the DataBanks are populated (class designed: currentfileposition index)]
            If ContainsTrainer() Then
                If Not PopulateTrainerData() Then
                    'Either the function triggered an exception, or filesize did not match
                    Return ErFailedToRetrieveTrainerData
                End If
            End If

            ' Load the Sizeing based on Version
            If Not SetBankSizes() Then
                Return ErUnknowenFileVersion
            Else
                ' Populate the DataBanks and Test vs. File.Length()
                If Not PopulateDataBanks() Then
                    Return ErPossibleFileCorrupted
                End If
            End If

            ' Destroy previous mapper if already existing
            ' TODO:
            If Not IsNothing(pMAPPER) Then
                'destroy old mapper
                pMAPPER.Reset()
                pMAPPER = Nothing
            End If

            ' Load The Mapper
            Select Case MapperID
                Case 0 : pMAPPER = New clsMapper_000(PRGBanks, CHRBanks) : Exit Select
                Case 1 : pMAPPER = New clsMapper_001(PRGBanks, CHRBanks) : Exit Select
                Case 2 : pMAPPER = New clsMapper_002(PRGBanks, CHRBanks) : Exit Select
                Case 3 : pMAPPER = New clsMapper_003(PRGBanks, CHRBanks) : Exit Select
                Case 4 : pMAPPER = New clsMapper_004(PRGBanks, CHRBanks) : Exit Select
                Case 66 : pMAPPER = New clsMapper_066(PRGBanks, CHRBanks) : Exit Select
                Case Else : m_ValidImage = False : Return ErUnsupportedMapperType & " {" & MapperID().ToString() & "}"
            End Select

            ' Valid iNES ROM Cartridge
            m_ValidImage = True

            '---------- debug shit
            ' ========== ADD THIS DIAGNOSTIC CODE HERE ==========
            Debug.WriteLine("")
            Debug.WriteLine("=".PadRight(80, "="))
            Debug.WriteLine("CARTRIDGE LOAD DIAGNOSTIC")
            Debug.WriteLine("=".PadRight(80, "="))

            If vPRGMemory.Length >= 16 Then
                Debug.WriteLine("Last 16 bytes of PRG (contains vectors):")
                For i As Integer = vPRGMemory.Length - 16 To vPRGMemory.Length - 1
                    Debug.Write(String.Format("{0:X2} ", vPRGMemory(i)))
                Next
                Debug.WriteLine("")

                ' OLD (WRONG) - These indices are backwards!
                ' Dim nmiLo As Byte = vPRGMemory(vPRGMemory.Length - 6)  ' $FFFA
                ' Dim nmiHi As Byte = vPRGMemory(vPRGMemory.Length - 5)  ' $FFFB

                ' NEW (CORRECT) - Read from the actual vector locations
                ' The vectors are at the LAST 6 bytes of PRG ROM:
                ' Length-6, Length-5 = $FFFA/$FFFB (NMI)
                ' Length-4, Length-3 = $FFFC/$FFFD (RST)
                ' Length-2, Length-1 = $FFFE/$FFFF (IRQ)

                Dim nmiLo As Byte = vPRGMemory(vPRGMemory.Length - 6)  ' Byte at $FFFA
                Dim nmiHi As Byte = vPRGMemory(vPRGMemory.Length - 5)  ' Byte at $FFFB
                Dim rstLo As Byte = vPRGMemory(vPRGMemory.Length - 4)  ' Byte at $FFFC
                Dim rstHi As Byte = vPRGMemory(vPRGMemory.Length - 3)  ' Byte at $FFFD
                Dim irqLo As Byte = vPRGMemory(vPRGMemory.Length - 2)  ' Byte at $FFFE
                Dim irqHi As Byte = vPRGMemory(vPRGMemory.Length - 1)  ' Byte at $FFFF

                ' Build 16-bit addresses (little-endian: low byte first!)
                Dim nmiVec As UInt16 = CUShort((nmiHi << 8) Or nmiLo)
                Dim rstVec As UInt16 = CUShort((rstHi << 8) Or rstLo)
                Dim irqVec As UInt16 = CUShort((irqHi << 8) Or irqLo)

                Debug.WriteLine(String.Format("Vectors in PRG ROM:"))
                Debug.WriteLine(String.Format("  NMI: ${0:X4} (bytes: ${1:X2} ${2:X2})", nmiVec, nmiLo, nmiHi))
                Debug.WriteLine(String.Format("  RST: ${0:X4} (bytes: ${1:X2} ${2:X2})", rstVec, rstLo, rstHi))
                Debug.WriteLine(String.Format("  IRQ: ${0:X4} (bytes: ${1:X2} ${2:X2})", irqVec, irqLo, irqHi))

                ' Verify they're in ROM range
                If rstVec >= &H8000US Then
                    Debug.WriteLine("✓ Reset vector is in ROM range!")
                Else
                    Debug.WriteLine("✗ Reset vector is NOT in ROM range!")
                End If
            End If
            ' Header info
            Debug.WriteLine(String.Format("Mapper ID: {0}", nMapperID))
            Debug.WriteLine(String.Format("PRG Banks: {0} ({1} KB)", PRGBanks, PRGBanks * 16))
            Debug.WriteLine(String.Format("CHR Banks: {0} ({1} KB)", CHRBanks, CHRBanks * 8))
            Debug.WriteLine(String.Format("Mirror Mode: {0}", hw_mirror.ToString()))

            ' Check PRG Memory
            If IsNothing(vPRGMemory) Then
                Debug.WriteLine("✗ CRITICAL: vPRGMemory is Nothing!")
            ElseIf vPRGMemory.Length = 0 Then
                Debug.WriteLine("✗ CRITICAL: vPRGMemory length is 0!")
            Else
                Debug.WriteLine(String.Format("✓ vPRGMemory allocated: {0} bytes", vPRGMemory.Length))

                ' Show first 16 bytes
                Debug.WriteLine("First 16 bytes of PRG:")
                For i As Integer = 0 To Math.Min(15, vPRGMemory.Length - 1)
                    Debug.Write(String.Format("{0:X2} ", vPRGMemory(i)))
                Next
                Debug.WriteLine("")

                ' Show last 16 bytes (where reset vector is)
                If vPRGMemory.Length >= 16 Then
                    Debug.WriteLine("Last 16 bytes of PRG (contains vectors):")
                    For i As Integer = vPRGMemory.Length - 16 To vPRGMemory.Length - 1
                        Debug.Write(String.Format("{0:X2} ", vPRGMemory(i)))
                    Next
                    Debug.WriteLine("")

                    ' Decode the vectors from PRG ROM
                    Dim nmiLo As Byte = vPRGMemory(vPRGMemory.Length - 6)  ' $FFFA
                    Dim nmiHi As Byte = vPRGMemory(vPRGMemory.Length - 5)  ' $FFFB
                    Dim rstLo As Byte = vPRGMemory(vPRGMemory.Length - 4)  ' $FFFC
                    Dim rstHi As Byte = vPRGMemory(vPRGMemory.Length - 3)  ' $FFFD
                    Dim irqLo As Byte = vPRGMemory(vPRGMemory.Length - 2)  ' $FFFE
                    Dim irqHi As Byte = vPRGMemory(vPRGMemory.Length - 1)  ' $FFFF

                    Dim nmiVec As UInt16 = CUShort((nmiHi << 8) Or nmiLo)
                    Dim rstVec As UInt16 = CUShort((rstHi << 8) Or rstLo)
                    Dim irqVec As UInt16 = CUShort((irqHi << 8) Or irqLo)

                    Debug.WriteLine(String.Format("Vectors in PRG ROM:"))
                    Debug.WriteLine(String.Format("  NMI: ${0:X4}", nmiVec))
                    Debug.WriteLine(String.Format("  RST: ${0:X4}", rstVec))
                    Debug.WriteLine(String.Format("  IRQ: ${0:X4}", irqVec))
                End If
            End If

            ' Check CHR Memory
            If IsNothing(vCHRMemory) Then
                Debug.WriteLine("✗ CRITICAL: vCHRMemory is Nothing!")
            ElseIf vCHRMemory.Length = 0 Then
                Debug.WriteLine("✗ CRITICAL: vCHRMemory length is 0!")
            Else
                Debug.WriteLine(String.Format("✓ vCHRMemory allocated: {0} bytes", vCHRMemory.Length))
            End If

            ' Test the mapper
            Debug.WriteLine("")
            Debug.WriteLine("Testing Mapper:")

            Dim testData As Byte = 0
            Dim mapped As UInt32 = 0

            ' Test read at $FFFC (reset vector low byte)
            Dim handled As Boolean = pMAPPER.cpuMapRead(&HFFFCUS, mapped, testData)
            Debug.WriteLine(String.Format("  Mapper.cpuMapRead($FFFC) → handled={0}, mapped=${1:X}, data from mapper param=${2:X2}",
                                  handled, mapped, testData))

            If handled AndAlso mapped <> &HFFFFFFFFUI Then
                ' Mapper gave us an address, read from PRG
                If mapped < vPRGMemory.Length Then
                    Dim actualData As Byte = vPRGMemory(mapped)
                    Debug.WriteLine(String.Format("  vPRGMemory[${0:X}] = ${1:X2}", mapped, actualData))
                Else
                    Debug.WriteLine(String.Format("  ✗ Mapped address ${0:X} is OUT OF BOUNDS! (PRG size = {1})",
                                          mapped, vPRGMemory.Length))
                End If
            ElseIf Not handled Then
                Debug.WriteLine("  ✗ Mapper did NOT handle $FFFC!")
            End If

            ' Also test $8000 (start of PRG ROM)
            handled = pMAPPER.cpuMapRead(&H8000US, mapped, testData)
            Debug.WriteLine(String.Format("  Mapper.cpuMapRead($8000) → handled={0}, mapped=${1:X}", handled, mapped))
            If handled AndAlso mapped <> &HFFFFFFFFUI AndAlso mapped < vPRGMemory.Length Then
                Debug.WriteLine(String.Format("  vPRGMemory[${0:X}] = ${1:X2}", mapped, vPRGMemory(mapped)))
            End If

            Debug.WriteLine("=".PadRight(80, "="))
            Debug.WriteLine("")
            ' ========== END DIAGNOSTIC CODE ==========

            Return ""
        End Function

        ' Communications
        Public Function cpuRead(ByVal addr As UInt16, ByRef data As Byte) As Boolean
            Dim mapped_addr As UInt32 = 0
            If pMAPPER.cpuMapRead(addr, mapped_addr, data) Then
                If mapped_addr = &HFFFFFFFFUI Then
                    Return True
                Else
                    data = vPRGMemory(mapped_addr)
                End If
                Return True
            End If
            Return False
        End Function

        Public Function cpuWrite(ByVal addr As UInt16, ByVal data As Byte) As Boolean
            Dim mapped_addr As UInt32 = 0
            If pMAPPER.cpuMapWrite(addr, mapped_addr, data) Then
                If mapped_addr = &HFFFFFFFFUI Then
                    Return True
                Else
                    vPRGMemory(mapped_addr) = data
                End If
                Return True
            End If
            Return False
        End Function

        Public Function ppuRead(ByVal addr As UInt16, ByRef data As Byte) As Boolean
            Dim mapped_addr As UInt32 = 0
            If pMAPPER.ppuMapRead(addr, mapped_addr) Then
                data = vCHRMemory(mapped_addr)
                Return True
            End If
            Return False
        End Function

        Public Function ppuWrite(ByVal addr As UInt16, ByVal data As Byte) As Boolean
            Dim mapped_addr As UInt32 = 0
            If pMAPPER.ppuMapWrite(addr, mapped_addr) Then
                vCHRMemory(mapped_addr) = data
                Return True
            End If
            Return False
        End Function

        Public Sub Reset()
            '// Note: This does Not reset the ROM contents,
            '// but does reset the mapper.
            If Not IsNothing(pMAPPER) Then
                pMAPPER.Reset()
            End If
        End Sub

    End Class

End Namespace
