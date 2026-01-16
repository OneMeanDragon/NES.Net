Imports System.Runtime.CompilerServices

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' Factory for creating mapper instances with proper disposal support
    ''' </summary>
    Public NotInheritable Class MapperFactory

        ' Private constructor - static class
        Private Sub New()
        End Sub

        ''' <summary>
        ''' Create a mapper instance based on mapper number
        ''' </summary>
        Public Shared Function CreateMapper(mapperNumber As Byte, prgBanks As Byte, chrBanks As Byte) As MapperBase
            Select Case mapperNumber
                Case 0
                    Return New Mapper000(prgBanks, chrBanks)
                Case 1
                    Return New Mapper001(prgBanks, chrBanks)
                Case 2
                    Return New Mapper002(prgBanks, chrBanks)
                Case 3
                    Return New Mapper003(prgBanks, chrBanks)
                Case 4
                    Return New Mapper004(prgBanks, chrBanks)
                Case 9
                    Return New Mapper009(prgBanks, chrBanks)
                Case 66
                    Return New Mapper066(prgBanks, chrBanks)
                Case Else
                    Debug.WriteLine($"Unsupported mapper: {mapperNumber}")
                    Return Nothing
            End Select
        End Function

        ''' <summary>
        ''' Check if a mapper is supported
        ''' </summary>
        Public Shared Function IsSupported(mapperNumber As Byte) As Boolean
            Select Case mapperNumber
                Case 0, 1, 2, 3, 4, 9, 66
                    Return True
                Case Else
                    Return False
            End Select
        End Function

        ''' <summary>
        ''' Get list of all supported mapper numbers
        ''' </summary>
        Public Shared Function GetSupportedMappers() As Byte()
            Return New Byte() {0, 1, 2, 3, 4, 9, 66}
        End Function

        ''' <summary>
        ''' Get friendly name for mapper
        ''' </summary>
        Public Shared Function GetMapperName(mapperNumber As Byte) As String
            Select Case mapperNumber
                Case 0
                    Return "NROM"
                Case 1
                    Return "MMC1 (SxROM)"
                Case 2
                    Return "UxROM"
                Case 3
                    Return "CNROM"
                Case 4
                    Return "MMC3 (TxROM)"
                Case 9
                    Return "MMC2 (PxROM)"
                Case 66
                    Return "GxROM"
                Case Else
                    Return $"Unknown Mapper ({mapperNumber})"
            End Select
        End Function

        ''' <summary>
        ''' Get detailed information about a mapper
        ''' </summary>
        Public Shared Function GetMapperInfo(mapperNumber As Byte) As String
            Select Case mapperNumber
                Case 0
                    Return "No mapper - simple direct mapping. 16KB or 32KB PRG, up to 8KB CHR."
                Case 1
                    Return "Nintendo MMC1. Switchable PRG/CHR banks, serial register loading, 8KB cart RAM."
                Case 2
                    Return "UxROM. Switchable 16KB PRG banks, fixed CHR."
                Case 3
                    Return "CNROM. Fixed PRG, switchable 8KB CHR banks."
                Case 4
                    Return "Nintendo MMC3. Advanced banking, scanline IRQ counter, 8KB cart RAM."
                Case 9
                    Return "Nintendo MMC2. 16KB PRG banking with special CHR banking for split-screen effects."
                Case 66
                    Return "GxROM. Simple 32KB PRG + 8KB CHR banking."
                Case Else
                    Return "No information available."
            End Select
        End Function

        ''' <summary>
        ''' Get example games that use this mapper
        ''' </summary>
        Public Shared Function GetExampleGames(mapperNumber As Byte) As String()
            Select Case mapperNumber
                Case 0
                    Return {"Donkey Kong", "Mario Bros", "Excitebike", "Ice Climber"}
                Case 1
                    Return {"The Legend of Zelda", "Metroid", "Kid Icarus", "Mega Man 2"}
                Case 2
                    Return {"Mega Man", "Castlevania", "Contra", "Duck Tales"}
                Case 3
                    Return {"Solomon's Key", "Arkanoid", "Paperboy", "Cybernoid"}
                Case 4
                    Return {"Super Mario Bros 3", "Mega Man 3-6", "Kirby's Adventure", "Batman"}
                Case 9
                    Return {"Punch-Out!!", "Mike Tyson's Punch-Out!!", "Rad Racer"}
                Case 66
                    Return {"Super Mario Bros + Duck Hunt", "Gumshoe"}
                Case Else
                    Return {}
            End Select
        End Function

    End Class
End Namespace