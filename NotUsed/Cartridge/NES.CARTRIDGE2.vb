Imports System
Imports System.IO
Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices

Namespace NintendoEntertainmentSystem
    Public Module CartridgeGlobal
        Public Cart As CartridgeClass
    End Module

    ''' <summary>
    ''' Modern, high-performance NES cartridge implementation using Memory(Of Byte) for zero-copy operations
    ''' </summary>
    Public Class CartridgeClass
        Implements IDisposable

#Region "Constants"
        'KB_SIZE
        Private Const INES_HEADER_SIZE As UInteger = 16
        Private Const PRG_BANK_SIZE As UInteger = KB_SIZE * 16  ' 16KB
        Private Const CHR_BANK_SIZE As UInteger = KB_SIZE * 8   ' 8KB
        Private Const TRAINER_SIZE As UInteger = 512
#End Region

#Region "Header Structure"
        <StructLayout(LayoutKind.Sequential, Pack:=1, Size:=16)>
        Private Structure INESHeader
            <MarshalAs(UnmanagedType.ByValArray, SizeConst:=4)>
            Public Magic As Byte()          ' "NES" + $1A

            Public PrgRomSize As Byte       ' PRG ROM size in 16KB units
            Public ChrRomSize As Byte       ' CHR ROM size in 8KB units
            Public Flags6 As Byte           ' Mapper, mirroring, battery, trainer
            Public Flags7 As Byte           ' Mapper, VS/Playchoice, NES 2.0
            Public Flags8 As Byte           ' PRG-RAM size (rarely used)
            Public Flags9 As Byte           ' TV system (rarely used)
            Public Flags10 As Byte          ' TV system, PRG-RAM presence (unofficial)

            <MarshalAs(UnmanagedType.ByValArray, SizeConst:=5)>
            Public Unused As Byte()         ' Unused padding

            ' Property helpers for flag parsing
            Public ReadOnly Property MapperNumber As Byte
                Get
                    Return CByte(((Flags7 And &HF0) Or (Flags6 >> 4)))
                End Get
            End Property

            Public ReadOnly Property IsVerticalMirroring As Boolean
                Get
                    Return (Flags6 And &H1) <> 0
                End Get
            End Property

            Public ReadOnly Property HasBatteryBackedRam As Boolean
                Get
                    Return (Flags6 And &H2) <> 0
                End Get
            End Property

            Public ReadOnly Property HasTrainer As Boolean
                Get
                    Return (Flags6 And &H4) <> 0
                End Get
            End Property

            Public ReadOnly Property IsFourScreenMode As Boolean
                Get
                    Return (Flags6 And &H8) <> 0
                End Get
            End Property

            Public ReadOnly Property IsNES2Format As Boolean
                Get
                    Return (Flags7 And &HC) = &H8
                End Get
            End Property

            Public ReadOnly Property IsValid As Boolean
                Get
                    Return Magic IsNot Nothing AndAlso
                           Magic.Length = 4 AndAlso
                           Magic(0) = &H4E AndAlso  ' 'N'
                           Magic(1) = &H45 AndAlso  ' 'E'
                           Magic(2) = &H53 AndAlso  ' 'S'
                           Magic(3) = &H1A          ' EOF
                End Get
            End Property
        End Structure
#End Region

#Region "Fields"
        ' Modern memory-efficient storage using Memory<T>
        Private _romData As Memory(Of Byte)          ' Entire ROM file in memory
        Private _prgRom As ReadOnlyMemory(Of Byte)   ' PRG ROM view (zero-copy)
        Private _chrRom As Memory(Of Byte)           ' CHR ROM/RAM (may be writable)
        Private _trainer As ReadOnlyMemory(Of Byte)  ' Trainer data if present

        Private _header As INESHeader
        Private _mapper As MapperBase 'clsMapper
        Private _isLoaded As Boolean = False
        Private _isDisposed As Boolean = False
#End Region

#Region "Properties"
        Public ReadOnly Property IsLoaded As Boolean
            Get
                Return _isLoaded
            End Get
        End Property

        Public ReadOnly Property MapperID As Byte
            Get
                Return _header.MapperNumber
            End Get
        End Property

        Public ReadOnly Property PrgBanks As Integer
            Get
                Return _header.PrgRomSize
            End Get
        End Property

        Public ReadOnly Property ChrBanks As Integer
            Get
                Return If(_header.ChrRomSize = 0, 1, _header.ChrRomSize) ' 0 means CHR-RAM
            End Get
        End Property

        Public ReadOnly Property MirrorMode As MirrorMode
            Get
                If _mapper IsNot Nothing Then
                    Dim mapperMirror = _mapper.GetMirrorMode() 'Mirror()
                    If mapperMirror <> MirrorMode.Hardware Then
                        Return mapperMirror
                    End If
                End If

                ' Return hardware mirror mode
                If _header.IsFourScreenMode Then
                    Return MirrorMode.Hardware ' Or define FOURSCREEN
                ElseIf _header.IsVerticalMirroring Then
                    Return MirrorMode.Vertical
                Else
                    Return MirrorMode.Horizontal
                End If
            End Get
        End Property

        Public ReadOnly Property HasBattery As Boolean
            Get
                Return _header.HasBatteryBackedRam
            End Get
        End Property

        Public ReadOnly Property Mapper As MapperBase 'clsMapper
            Get
                Return _mapper
            End Get
        End Property
#End Region

#Region "Constructor / Destructor"
        Public Sub New()
            ' Default constructor - call LoadFromFile to load a ROM
        End Sub

        Public Sub New(filePath As String)
            LoadFromFile(filePath)
        End Sub

        Public Sub Dispose() Implements IDisposable.Dispose
            If Not _isDisposed Then
                _mapper?.Reset()
                _mapper = Nothing
                _romData = Nothing
                _isDisposed = True
            End If
        End Sub
#End Region

#Region "Loading"
        ''' <summary>
        ''' Load a ROM file with modern, high-performance parsing
        ''' </summary>
        Public Function LoadFromFile(filePath As String) As Boolean
            Try
                ' Reset state
                _isLoaded = False
                _mapper?.Reset()
                _mapper = Nothing

                ' Read entire file into memory (modern approach - let OS handle buffering)
                Dim fileBytes = File.ReadAllBytes(filePath)
                If fileBytes.Length < INES_HEADER_SIZE Then
                    Debug.WriteLine($"File too small: {fileBytes.Length} bytes")
                    Return False
                End If

                ' Store in Memory<T> for efficient slicing
                _romData = New Memory(Of Byte)(fileBytes)

                ' Parse header using fast struct marshaling

                _header = ParseHeader(_romData.Slice(0, INES_HEADER_SIZE))

                If Not _header.IsValid Then
                    Debug.WriteLine("Invalid iNES header magic bytes")
                    Return False
                End If

                ' Calculate offsets
                Dim offset = INES_HEADER_SIZE

                ' Handle trainer if present
                If _header.HasTrainer Then
                    _trainer = _romData.Slice(offset, TRAINER_SIZE)
                    offset += TRAINER_SIZE
                End If

                ' Extract PRG ROM (read-only view)
                Dim prgSize = _header.PrgRomSize * PRG_BANK_SIZE
                If offset + prgSize > _romData.Length Then
                    Debug.WriteLine("PRG ROM extends beyond file size")
                    Return False
                End If
                _prgRom = _romData.Slice(offset, prgSize)
                offset += prgSize

                ' Extract CHR ROM/RAM
                If _header.ChrRomSize = 0 Then
                    ' CHR-RAM: Allocate 8KB of writable RAM
                    _chrRom = New Memory(Of Byte)(New Byte(CHR_BANK_SIZE - 1) {})
                Else
                    ' CHR-ROM: Map from file (but keep writable for some mappers)
                    Dim chrSize = _header.ChrRomSize * CHR_BANK_SIZE
                    If offset + chrSize > _romData.Length Then
                        Debug.WriteLine("CHR ROM extends beyond file size")
                        Return False
                    End If
                    _chrRom = _romData.Slice(offset, chrSize)
                End If

                ' Initialize mapper
                If Not InitializeMapper() Then
                    Debug.WriteLine($"Unsupported mapper: {_header.MapperNumber}")
                    Return False
                End If

                _isLoaded = True
                LogDiagnostics()
                Return True

            Catch ex As Exception
                Debug.WriteLine($"Error loading ROM: {ex.Message}")
                Return False
            End Try
        End Function

        ' Fast header parsing using Span<T> and MemoryMarshal
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Private Function ParseHeader(headerSpan As ReadOnlyMemory(Of Byte)) As INESHeader
            Dim s = headerSpan.Span

            ' Note: You cannot use s(i) directly in VB.NET. 
            ' Use Slice().ToArray() for arrays and MemoryMarshal.Read for single bytes.
            Return New INESHeader With {
                .Magic = s.Slice(0, 4).ToArray(),
                .PrgRomSize = MemoryMarshal.Read(Of Byte)(s.Slice(4, 1)),
                .ChrRomSize = MemoryMarshal.Read(Of Byte)(s.Slice(5, 1)),
                .Flags6 = MemoryMarshal.Read(Of Byte)(s.Slice(6, 1)),
                .Flags7 = MemoryMarshal.Read(Of Byte)(s.Slice(7, 1)),
                .Flags8 = MemoryMarshal.Read(Of Byte)(s.Slice(8, 1)),
                .Flags9 = MemoryMarshal.Read(Of Byte)(s.Slice(9, 1)),
                .Flags10 = MemoryMarshal.Read(Of Byte)(s.Slice(10, 1)),
                .Unused = s.Slice(11, 5).ToArray()
            }
        End Function

        ''' <summary>
        ''' Initialize the appropriate mapper
        ''' </summary>
        Private Function InitializeMapper() As Boolean
            Dim mapperID = _header.MapperNumber
            If MapperFactory.IsSupported(mapperID) Then
                _mapper = MapperFactory.CreateMapper(mapperID, PrgBanks, ChrBanks)
                Debug.WriteLine($"Loaded: {MapperFactory.GetMapperName(mapperID)}")
                Return True
            Else
                Debug.WriteLine($"Unsupported mapper: {mapperID}")
                Return False
            End If
            Return False
        End Function

        Public ReadOnly Property GetMapper() As MapperBase 'clsMapper
            Get
                Return _mapper
            End Get
        End Property

#End Region

#Region "CPU Bus Interface"
        ''' <summary>
        ''' CPU read with aggressive inlining for performance
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Function CpuRead(addr As UInt16, ByRef data As Byte) As Boolean
            If Not _isLoaded OrElse _mapper Is Nothing Then Return False

            Dim mappedAddr As UInt32 = 0
            If _mapper.cpuMapRead(addr, mappedAddr, data) Then
                ' Check if mapper handled it internally
                If mappedAddr = &HFFFFFFFFUI Then
                    Return True
                End If

                ' Bounds check and read from PRG ROM
                If mappedAddr < _prgRom.Length Then
                    'data = _prgRom.Span(CInt(mappedAddr))
                    data = MemoryMarshal.Read(Of Byte)(_prgRom.Span.Slice(CInt(mappedAddr), 1))
                    Return True
                End If
            End If

            Return False
        End Function

        ''' <summary>
        ''' CPU write with aggressive inlining for performance
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Function CpuWrite(addr As UInt16, data As Byte) As Boolean
            If Not _isLoaded OrElse _mapper Is Nothing Then Return False

            Dim mappedAddr As UInt32 = 0
            If _mapper.cpuMapWrite(addr, mappedAddr, data) Then
                ' Mapper handled it (usually for mapper register writes)
                If mappedAddr = &HFFFFFFFFUI Then
                    Return True
                End If

                ' Some mappers allow PRG-RAM writes
                ' (We'd need writable PRG-RAM for this, not implemented here)
                Return True
            End If

            Return False
        End Function
#End Region

#Region "PPU Bus Interface"
        ''' <summary>
        ''' PPU read with aggressive inlining for performance
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Function PpuRead(addr As UInt16, ByRef data As Byte) As Boolean
            If Not _isLoaded OrElse _mapper Is Nothing Then Return False

            Dim mappedAddr As UInt32 = 0
            If _mapper.ppuMapRead(addr, mappedAddr) Then
                If mappedAddr < _chrRom.Length Then
                    data = _chrRom.Span(CInt(mappedAddr))
                    Return True
                End If
            End If

            Return False
        End Function

        ''' <summary>
        ''' PPU write with aggressive inlining for performance
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Function PpuWrite(addr As UInt16, data As Byte) As Boolean
            If Not _isLoaded OrElse _mapper Is Nothing Then Return False

            Dim mappedAddr As UInt32 = 0
            If _mapper.ppuMapWrite(addr, mappedAddr) Then
                If mappedAddr < _chrRom.Length Then
                    Dim span = _chrRom.Span
                    span(mappedAddr) = data
                    '_chrRom.Span(CInt(mappedAddr)) = data
                    'MemoryMarshal.Write(Of Byte)(_chrRom.Span.Slice(CInt(mappedAddr), 1), data)
                    Return True
                End If
            End If

            Return False
        End Function
#End Region

#Region "Reset"
        Public Sub Reset()
            _mapper?.Reset()
        End Sub
#End Region

#Region "Diagnostics"
        Private Sub LogDiagnostics()
            Debug.WriteLine("")
            Debug.WriteLine("═══════════════════════════════════════════════════════════════")
            Debug.WriteLine("  MODERN CARTRIDGE LOADER - DIAGNOSTIC REPORT")
            Debug.WriteLine("═══════════════════════════════════════════════════════════════")
            Debug.WriteLine($"File Size:       {_romData.Length:N0} bytes")
            Debug.WriteLine($"Mapper:          {_header.MapperNumber} (0x{_header.MapperNumber:X2})")
            Debug.WriteLine($"PRG Banks:       {_header.PrgRomSize} x 16KB = {_header.PrgRomSize * 16} KB")
            Debug.WriteLine($"CHR Banks:       {_header.ChrRomSize} x 8KB = {_header.ChrRomSize * 8} KB")
            Debug.WriteLine($"CHR Type:        {If(_header.ChrRomSize = 0, "RAM", "ROM")}")
            Debug.WriteLine($"Mirroring:       {If(_header.IsVerticalMirroring, "Vertical", "Horizontal")}")
            Debug.WriteLine($"Battery:         {_header.HasBatteryBackedRam}")
            Debug.WriteLine($"Trainer:         {_header.HasTrainer}")
            Debug.WriteLine($"Format:          {If(_header.IsNES2Format, "NES 2.0", "iNES 1.0")}")

            If _prgRom.Length >= 16 Then
                Debug.WriteLine("")
                Debug.WriteLine("PRG ROM - First 16 bytes:")
                Dim span = _prgRom.Span
                For i = 0 To 15
                    Dim b = MemoryMarshal.Read(Of Byte)(span.Slice(i, 1))
                    Debug.Write($"{b:X2} ")
                Next
                Debug.WriteLine("")

                Debug.WriteLine("PRG ROM - Last 16 bytes (vectors):")
                For i = _prgRom.Length - 16 To _prgRom.Length - 1
                    Dim b = MemoryMarshal.Read(Of Byte)(span.Slice(i, 1))
                    Debug.Write($"{b:X2} ")
                Next
                Debug.WriteLine("")

                ' Decode reset vector
                Dim rstLo = MemoryMarshal.Read(Of Byte)(span.Slice(_prgRom.Length - 4, 1))
                Dim rstHi = MemoryMarshal.Read(Of Byte)(span.Slice(_prgRom.Length - 3, 1))
                Dim rstVec = CUShort((rstHi << 8) Or rstLo)
                Debug.WriteLine($"Reset Vector:    ${rstVec:X4}")
            End If

            Debug.WriteLine("═══════════════════════════════════════════════════════════════")
            Debug.WriteLine("")
        End Sub
#End Region

    End Class

End Namespace