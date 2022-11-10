#Include Once "win/commdlg.bi"
'CONST SAVE_DIALOG = &H01000000 'Specify this flag to use a save dialog instead of an open dialog
'CONST OPEN_DIALOG = &H02000000

'Declare Function ComDlgFileName(HWND As hwnd, Title As String,InitialDir AS String, Filter AS String, Flags AS uint32_t) As String





'Function ComDlgFileName(HWND As hwnd, Title As String,InitialDir AS String, Filter AS String, Flags AS uint32_t) As String
'DIM As OPENFILENAME ofn 
'
'
'    Title = Title + CHR(0)
'    InitialDir = InitialDir + CHR(0)
'
'
'    DO
'       MID(Filter, INSTR(Filter, "|")) = CHR(0)
'    LOOP WHILE INSTR(Filter, "|")
'    Filter = Filter + CHR(0) + CHR(0)
'    DIM AS STRING * 2048 oFile
'    ofn.lStructSize = LEN(ofn)
'    ofn.hwndOwner = hwnd
'    ofn.lpstrFilter = StrPtr(filter) 
'    ofn.nFilterIndex = 1
'    ofn.lpstrFile =  strptr(oFile) 
'    ofn.nMaxFile = strptr(oFile) 
'    ofn.lpstrFileTitle = ofn.lpstrFile
'    ofn.nMaxFileTitle = ofn.nMaxFile
'    ofn.lpstrInitialDir = strptr(InitialDir)
'    ofn.lpstrTitle = strptr(Title) 
'    IF OPEN_DIALOG AND Flags OR (SAVE_DIALOG AND Flags) = 0 THEN
'       ' IF OFN_ALLOWMULTISELECT AND Flags THEN Flags = Flags OR OFN_EXPLORER
'        ofn.Flags = Flags
'        GetOpenFileName @ofn
'        IF GetLastError = 0 THEN
'            IF OFN_ALLOWMULTISELECT AND Flags THEN
'            '    DIM AS String tmp: tmp = ofn.lpstrFile + ofn.nFileOffset
'            '    DIM AS _MEM pFiles: pFiles = _MEM(tmp, tcslen(tmp))
'            '    
'                
''                DIM AS STRING file, outfiles, directory
''                DIM AS _MEM dir: dir = _MEM(ofn.lpstrFile, tcslen(ofn.lpstrFile))
''                directory = SPACE$(tcslen(ofn.lpstrFile))
''                _MEMGET dir, dir.OFFSET, directory
''                _MEMFREE dir
''                DIM AS LONG i
''                WHILE tcslen(tmp)
''                    file = SPACE$(tcslen(tmp))
''                    _MEMGET pFiles, pFiles.OFFSET, file
''                    SELECT CASE i
''                        CASE 0
''                            outfiles = directory + "\" + file
''                        CASE ELSE
''                            outfiles = outfiles + "|" + directory + "\" + file
''                    END SELECT
''                    i = i + 1
''                    tmp = tmp + (tcslen(tmp) + 1)
''                    pFiles = _MEM(tmp, tcslen(tmp))
''                WEND
''                _MEMFREE pFiles
''                IF i = 1 THEN
''                    file = directory
''                    ComDlgFileName = file
''                ELSE
''                    ComDlgFileName = outfiles
''                END IF
'            ELSE
'                return Mid(oFile, 1, INSTR(oFile, Chr(0)) - 1)
'          END IF
''        END IF
'    ELSEIF SAVE_DIALOG AND Flags THEN
'        ofn.Flags = Flags
'        GetSaveFileName @ofn 
'        Return MID(oFile, 1, INSTR(oFile, CHR(0)) - 1)
'    END IF
'    End If
'
'       END Function
''        
'        
'        '    'TYPE OPENFILENAME
''    '    AS _UNSIGNED LONG lStructSize
''    '    $IF 64BIT THEN
''    '        AS STRING * 4 padding
''    '    $END IF
''    '    AS _OFFSET hwndOwner, hInstance, lpstrFilter, lpstrCustomFilter
''    '    AS _UNSIGNED LONG nMaxCustFilter, nFilterIndex
''    '    AS _OFFSET lpstrFile
''    '    AS _UNSIGNED LONG nMaxFile
''    '    $IF 64BIT THEN
''    '        AS STRING * 4 padding2
''    '    $END IF
''    '    AS _OFFSET lpstrFileTitle
''    '    AS _UNSIGNED LONG nMaxFileTitle
''    '    $IF 64BIT THEN
''    '        AS STRING * 4 padding3
''    '    $END IF
''    '    AS _OFFSET lpstrInitialDir, lpstrTitle
''    '    AS _UNSIGNED LONG Flags
''    '    AS INTEGER nFileOffset, nFileExtension
''    '    AS _OFFSET lpstrDefExt, lCustData, lpfnHook, lpTemplateName, pvReserved
''    '    AS _UNSIGNED LONG dwReserved, FlagsEx
''    'END TYPE
''
''    'DECLARE DYNAMIC LIBRARY "Comdlg32"
''    '    SUB GetOpenFileName ALIAS "GetOpenFileNameA" (BYVAL ofn AS _OFFSET)
''    '    SUB GetSaveFileName ALIAS "GetSaveFileNameA" (BYVAL ofn AS _OFFSET)
''    'END DECLARE
''
''    '$IF 64BIT THEN
''    '    DECLARE CUSTOMTYPE LIBRARY ".\internal\c\c_compiler\x86_64-w64-mingw32\include\tchar"
''    '        FUNCTION tcslen%& ALIAS "_tcslen" (BYVAL str AS _OFFSET)
''    '    END DECLARE
''    '$ELSE
''    '    Declare CustomType Library ".\internal\c\c_compiler\i686-w64-mingw32\include\tchar"
''    '    Function tcslen%& Alias "_tcslen" (ByVal str As _Offset)
''    '    End Declare
''    '$END IF
''
''    'DECLARE LIBRARY
''    '    FUNCTION GetLastError~& ()
''    'END DECLARE
'
Function file_opensave( Byval hWnd As HWND,Open_or_save As boolean = TRUE ) As String

        Dim ofn As OPENFILENAME
        Dim filename As Zstring * MAX_PATH+1
       
        With ofn
                .lStructSize       = sizeof( OPENFILENAME )
                .hwndOwner         = hWnd
                .hInstance         = GetModuleHandle( NULL )
                .lpstrCustomFilter = NULL
                .nMaxCustFilter    = 0
                .nFilterIndex      = 1
                .lpstrFile         = @filename
                .nMaxFile          = sizeof( filename )
                .lpstrFileTitle    = NULL
                .nMaxFileTitle     = 0
                .lpstrInitialDir   = NULL
                .Flags             = OFN_EXPLORER Or OFN_FILEMUSTEXIST Or OFN_PATHMUSTEXIST
                .nFileOffset       = 0
                .nFileExtension    = 0
                
                .lCustData         = 0
                .lpfnHook          = NULL
                .lpTemplateName    = NULL
        End With
       
       If Open_or_save = TRUE Then
       	ofn.lpstrTitle        = @"File Open"
       	ofn.lpstrDefExt       = @"NES"
       	ofn.lpstrFilter       = Strptr( !"All Files, (*.*)\0*.*\0NES Files, (*.NES)\0*.NES\0\0" )

        If( GetOpenFileName( @ofn ) = FALSE ) Then
                Return ""
        Else
                Return filename
        End If
       Else  
       	ofn.lpstrTitle        = @"File Save"
       	ofn.lpstrDefExt       = @"BMP"
       	ofn.lpstrFilter       = Strptr( !"BMP files, (*.BMP)\0*.BMP\0\0" )

       	If( GetSaveFileName( @ofn ) = FALSE ) Then
                Return ""
        Else
                Return filename
       End If
       End If
        

End Function
