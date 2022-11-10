#include "file.bi"
'#include "crt.bi"

	
type CRC321 extends object
	
	declare constructor ()
	declare function buildtable() as boolean
	declare function CRC32(ByVal lngCRC32 As uLong, bArrayIn() As uByte, ByVal dblLength As Double) As Long
	'declare function CRCFromFile(FilePath as string) as String

	
	
	protected:


	bolTableReady As boolean
	lngArrayCRCByte(0 To 255) As Long
	
	
End Type


 dim shared mycrc32 as CRC321 ptr 
 
 constructor CRC321()
 
 
 bolTableReady = this.buildtable()
 
 End Constructor
 Function CRC321.BuildTable() As Boolean
          Dim intBytePos As uInteger
    Dim bBitPos As uByte
    
    ' Definition of polynomial for this type of CRC
    Const lngLimit = &HEDB88320
    
    Dim lngCRC As uLong
    
    For intBytePos = 0 To 255 Step 1
        lngCRC = intBytePos
        
        For bBitPos = 0 To 7 Step 1
            If lngCRC And 1 Then
              lngCRC = (((lngCRC And &HFFFFFFFE) \ 2) And &H7FFFFFFF) Xor lngLimit
            Else
              lngCRC = ((lngCRC And &HFFFFFFFE) \ 2) And &H7FFFFFFF
            End If
        Next bBitPos
        
        ' Add the checksum of the current byte to the lookup table
        lngArrayCRCByte(intBytePos) = lngCRC
    Next intBytePos
    
    ' Table is ready.  Error handling routine would return false.
    return  True
 End Function

 Function CRC321.CRC32(Byval lngCRC32 As uLong, bArrayIn() As uByte, ByVal dblLength As Double) As Long
    Dim dblCurPos As Double
    Dim lngTemp As Long
    
    ' Make sure the table is ready, just in case.
    If bolTableReady = false Then bolTableReady = this.buildtable
    
    ' Array could be empty
    If UBound(bArrayIn) < LBound(bArrayIn) Then
        ' Files with no data have a CRC of 0
       return 0
    Else
        lngTemp = lngCRC32 Xor &HFFFFFFFF
        
        For dblCurPos = 0 To dblLength
            lngTemp = (((lngTemp And &HFFFFFF00) \ &H100) And &HFFFFFF) Xor (lngArrayCRCByte((lngTemp And &HFF) Xor bArrayIn(dblCurPos)))
        Next dblCurPos
        
        ' Return the value
        return lngTemp Xor &HFFFFFFFF
    End If
End Function
 
 


Public Function CRCFromFile(ByVal File_Path As String) As String
	 
 mycrc32 = new CRC321()
    Dim bArrayFile() As Byte
    Dim lngCRC32 As Long
    Dim lngChunkSize As Long
    Dim lngSize As Long

    lngSize = FileLen(File_Path)
    lngChunkSize = 2048

    If lngSize <> 0 Then

        ' Read byte array from file
        Open File_Path For Binary Access Read As #1

        Do While Seek(1) < lngSize

            If (lngSize - Seek(1)) > lngChunkSize Then
                ' Process data in chunks. Chunky!
                Do While Seek(1) < (lngSize - lngChunkSize)
                    ReDim bArrayFile(lngChunkSize - 1)
                    Get #1, , bArrayFile()
                    lngCRC32 = mycrc32->CRC32(lngCRC32, bArrayFile(), lngChunkSize - 1)
                Loop
            Else
                ' Blast it at them
                ReDim bArrayFile(lngSize - Seek(1))
                Get #1, , bArrayFile()
                
                lngCRC32 = mycrc32->CRC32(lngCRC32, bArrayFile(), UBound(bArrayFile))
            End If

        Loop

        Close #1

        ' Everyone expects to view checksums in Hex strings.  Add buffer zeros if
        ' needed by smaller values.
        return Right("00000000" & Hex(lngCRC32), 8)
    Else
        ' File of zero bytes has a CRC of 0
        return  "00000000"
    End If
End Function












'
'print CRCFromFile("roms/SMB.nes")
'sleep

