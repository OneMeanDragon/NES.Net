Imports System.Runtime.InteropServices

Public Module ConversionSystemFunctions

	Public Function BytesToStruct(inBuffer As Byte(), pos As Integer, len As Integer, ObjectType As Type) As Object
		Dim objResult As Object
		Dim ptrBuffer As IntPtr = Marshal.AllocHGlobal(len)
		Marshal.Copy(inBuffer, pos, ptrBuffer, len)
		objResult = Marshal.PtrToStructure(ptrBuffer, ObjectType)
		Marshal.FreeHGlobal(ptrBuffer)
		Return objResult
	End Function
	Public Function BytesToStruct(inBuffer As Byte(), len As Integer, ObjectType As Type) As Object
		Dim objResult As Object
		Dim ptrBuffer As IntPtr = Marshal.AllocHGlobal(len)
		Marshal.Copy(inBuffer, 0, ptrBuffer, len)
		objResult = Marshal.PtrToStructure(ptrBuffer, ObjectType)
		Marshal.FreeHGlobal(ptrBuffer)
		Return objResult
	End Function
	Public Function StructToBytes(ObjectIn As Object) As Byte()
		Dim rawsize As Integer = Marshal.SizeOf(ObjectIn)
		Dim outBuffer As IntPtr = Marshal.AllocHGlobal(rawsize)
		Marshal.StructureToPtr(ObjectIn, outBuffer, False)
		Dim rawbytes(rawsize - 1) As Byte
		Marshal.Copy(outBuffer, rawbytes, 0, rawsize)
		Marshal.FreeHGlobal(outBuffer)
		Return rawbytes
	End Function

End Module
