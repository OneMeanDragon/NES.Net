Imports System.Runtime.CompilerServices
'Imports Nintendo.NintendoEntertainmentSystem.em6502

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' MOS Technology 6502 CPU - Core (Partial Class)
    ''' 8-bit Microprocessor used in the NES
    ''' </summary>
    Partial Public NotInheritable Class CPU6502

#Region "CPU Flags"
        <Flags>
        Public Enum StatusFlags As Byte
            C = 1 << 0  ' Carry
            Z = 1 << 1  ' Zero
            I = 1 << 2  ' Interrupt Disable
            D = 1 << 3  ' Decimal Mode (unused on NES)
            B = 1 << 4  ' Break
            U = 1 << 5  ' Unused (always 1)
            V = 1 << 6  ' Overflow
            N = 1 << 7  ' Negative
        End Enum
#End Region

#Region "Registers"
        Public A As Byte            ' Accumulator
        Public X As Byte            ' X Register
        Public Y As Byte            ' Y Register
        Public SP As Byte           ' Stack Pointer
        Public PC As UShort         ' Program Counter
        Public Status As Byte       ' Status Register
#End Region

#Region "Internal State"
        Friend _fetched As Byte
        Friend _addrAbs As UShort
        Friend _addrRel As UShort
        Friend _opcode As Byte
        Friend _cycles As Byte
        Friend _temp As UShort

        Public ClockCount As UInteger
        Public InstructionCount As UInteger
#End Region

#Region "Bus Connection"
        Private Shared _bus As NESBus

        Public Sub ConnectBus(ByRef bus As NESBus)
            _bus = bus
        End Sub

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function Read(addr As UShort) As Byte
            Return _bus.CpuRead(addr, False)
        End Function

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Sub Write(addr As UShort, data As Byte)
            _bus.CpuWrite(addr, data)
        End Sub
#End Region

#Region "Instruction Table"
        Public Enum AddrMode
            NONE
            IMP
            IMM
            ZP0
            ZPX
            ZPY
            ABS
            ABX
            ABY
            IND
            IZX
            IZY
            REL
        End Enum
        Friend Structure Instruction
            Public Name As String
            Public Operate As Func(Of Byte)
            Public AddressingMode As Func(Of Byte)
            Public Cycles As Byte
            Public ModeType As AddrMode

            Public Sub New(name As String, op As Func(Of Byte), mode As Func(Of Byte), modetype As AddrMode, cycles As Byte)
                Me.Name = name
                Me.Operate = op
                Me.AddressingMode = mode
                Me.Cycles = cycles
                Me.ModeType = modetype
            End Sub
        End Structure

        Friend ReadOnly _instructions(255) As Instruction
#End Region

#Region "Properties"
        Public ReadOnly Property IsComplete As Boolean
            Get
                Return _cycles = 0
            End Get
        End Property

        Public Function Complete() As Boolean
            Return _cycles = 0
        End Function

        Public ReadOnly Property Debug_PC As UShort
            Get
                Return PC
            End Get
        End Property

        Public ReadOnly Property Debug_SP As Byte
            Get
                Return SP
            End Get
        End Property

        Public ReadOnly Property Debug_A As Byte
            Get
                Return A
            End Get
        End Property

        Public ReadOnly Property Debug_X As Byte
            Get
                Return X
            End Get
        End Property

        Public ReadOnly Property Debug_Y As Byte
            Get
                Return Y
            End Get
        End Property

        Public ReadOnly Property Debug_Status As Byte
            Get
                Return Status
            End Get
        End Property

        Public ReadOnly Property Debug_ClockCount As UInteger
            Get
                Return ClockCount
            End Get
        End Property
#End Region

#Region "Flag Operations"
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function GetFlag(flag As StatusFlags) As Byte
            Return If((Status And flag) <> 0, 1, 0)
        End Function

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Sub SetFlag(flag As StatusFlags, value As Boolean)
            If value Then
                Status = Status Or flag
            Else
                Status = Status And (Not flag)
            End If
        End Sub
#End Region

#Region "Stack Operations"
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Sub Push(data As Byte)
            Write(&H100US + SP, data)
            SP = CByte((SP - 1) And &HFF)
        End Sub

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function Pop() As Byte
            SP = CByte((SP + 1) And &HFF)
            Return Read(&H100US + SP)
        End Function

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Sub PushWord(data As UShort)
            Push((data >> 8) And &HFF)
            Push(data And &HFF)
        End Sub

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function PopWord() As UShort
            Dim lo = Pop()
            Dim hi = Pop()
            Return (CUShort(hi) << 8) Or lo
        End Function
#End Region

#Region "Fetch Helper"
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function Fetch() As Byte
            If _instructions(_opcode).ModeType = AddrMode.IMP Then
                _fetched = Read(_addrAbs)
            End If
            Return _fetched
        End Function
#End Region

    End Class

End Namespace