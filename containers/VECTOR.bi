#include Once "crt.bi"

#macro MACRO_START_CHECK_TYPE(vector_type)
	
	#if Typeof(vector_type) = Typeof(Byte)
	#elseif Typeof(vector_type) = Typeof(Ubyte)
	#elseif Typeof(vector_type) = Typeof(Short)
	#elseif Typeof(vector_type) = Typeof(Ushort)
	#elseif Typeof(vector_type) = Typeof(Long)
	#elseif Typeof(vector_type) = Typeof(Ulong)
	#elseif Typeof(vector_type) = Typeof(Integer)
	#elseif Typeof(vector_type) = Typeof(Uinteger)
	#elseif Typeof(vector_type) = Typeof(Longint)
	#elseif Typeof(vector_type) = Typeof(Ulongint)
	#elseif Typeof(vector_type) = Typeof(Single)
	#elseif Typeof(vector_type) = Typeof(Double)
	#elseif Typeof(vector_type) = Typeof(Boolean)
	#elseif Typeof(vector_type) = Typeof(String)
	#elseif Typeof(vector_type) = Typeof(Any Ptr)
	#else
	
#endmacro

#macro MACRO_END_CHECK_TYPE()
	
	#endif
	
#endmacro

#macro MVectorTemplate(vector_type , AllocMem...)
	
	#ifndef TVECTOR##vector_type
		
		Type TVECTOR##vector_type
			
			#if Typeof(vector_type) = Typeof(String)
				
				vData As Zstring Ptr Ptr
				
			#else
				
				vData As vector_type Ptr
				
			#endif
			
			iSize As Long
			
			iRealMemSize As Long    
			
			Declare Constructor()
			
			Declare Constructor ( Byref rhs As TVECTOR##vector_type)
			
			Declare Constructor(iSize As Long)
			
			Declare Constructor(iSize As Long , vData As vector_type)
			
			Declare Destructor()
			
			Declare Function size() As Long ' Возвращает количество элементов в векторе 
			
			Declare Function back() As vector_type ' Доступ к последнему элементу 
			
			Declare Function front() As vector_type ' Доступ к первому элементу 
			
			Declare Function empty() As Boolean ' Возвращает true, если вектор пуст 
			
			Declare Function capacity() As Long ' Возвращает количество элементов, которое может содержать вектор до того, как ему потребуется выделить больше места. 
			
			Declare Function Clear() As Boolean ' Удаляет все элементы вектора 
			
			Declare Function insert Overload(iPos As Long  , vData As vector_type) As Boolean ' Вставка элементов в вектор перед iPos
			
			Declare Function insert Overload(iPos As Long  , vData() As vector_type) As Boolean ' Вставка элементов в вектор перед iPos из массива
			
			Declare Function insert Overload(iPos As Long  , vData() As vector_type , iStartPos As Long, iEndPos As Long) As Boolean ' Вставка элементов из массива c диапазоном в вектор перед iPos
			
			Declare Function Erase Overload(iPos As Long) As Boolean ' Удаляет указанный элемент вектора
			
			Declare Function Erase Overload(iStartPos As Long , iEndPos As Long) As Boolean  ' Удаляет диапазон указанных элементов вектора
			
			Declare Function push_back(vData As vector_type) As Boolean ' Вставка элемента в конец вектора 
			
			Declare Function pop_back() As vector_type  ' Удалить последний элемент вектора
			
			Declare Function resize Overload(iCount As Long ) As Boolean ' Изменяет размер вектора на заданную величину 
			
			Declare Function resize Overload(iCount As Long ,  vData As vector_type) As Boolean ' Изменяет размер вектора на заданную величину 
			
			Declare Function assign Overload(iCount As Long ,  vData As vector_type) As Boolean ' Заменяет содержимое контейнера вектора
			
			Declare Function assign Overload(iStartPos As Long , iEndPos As Long ,  vData As vector_type) As Boolean ' Заменяет содержимое контейнера вектора в диапазоне
			
			Declare Function swapV(v As TVECTOR##vector_type) As Boolean ' Обменивает значения векторов
			
			Declare Function shrink_to_fit() As Boolean ' Уменьшает количество используемой памяти за счёт освобождения неиспользованной
			
			Declare Property at(iIndex As Long) As vector_type ' доступ к ячейкам с проверкой диапазона (получение)
			
			Declare Property at(iIndex As Long , vData As vector_type) ' доступ к ячейкам с проверкой диапазона (установка)
			
			Declare Function realloc() As Boolean ' перераспределение памяти
			
			Declare Function IsParameterMacro() As Long
			
			#if Typeof(vector_type) = Typeof(String)
				Declare Operator [] (Byref iIndex As Long) As vector_type ' обычный доступ к ячейкам
			#else
				Declare Operator [] (Byref iIndex As Long) Byref As vector_type ' обычный доступ к ячейкам
			#endif
			
			Declare Operator Let (Byref v2 As TVECTOR##vector_type) ' копирование одного вектора в другой
			
			Declare Operator Let (Byref v As vector_type) ' присваивание значения в вектор
			
		End Type
		
		Constructor TVECTOR##vector_type()
			
			This.iSize = 0
			
			This.iRealMemSize = 0   
			
		End Constructor
		
		Constructor TVECTOR##vector_type(iSize As Long)
			
			This.vData = Callocate(iSize , Sizeof(vector_type))
			
			If This.vData Then
				
				This.iSize = iSize
				
				This.iRealMemSize = iSize                   
				
			Endif
			
			For i As Long = 0 To iSize - 1
				
				#if Typeof(vector_type) = Typeof(String)
					
					(This.vData)[i] = Callocate(100)
					
				#elseif Typeof(vector_type) = Typeof(Wstring Ptr)
					
					If IsParameterMacro() Then
					
						(This.vData)[i] = Callocate(100*Sizeof(Wstring))
					
					Endif
					
				#elseif Typeof(vector_type) = Typeof(Zstring Ptr)
					
					If IsParameterMacro() Then
						
						(This.vData)[i] = Callocate(100)
						
					Endif
					
				#else
					
					If IsParameterMacro() Then
						
						MACRO_START_CHECK_TYPE(vector_type)
							
							(This.vData)[i] = Callocate(Sizeof(**This.vData))
							
						MACRO_END_CHECK_TYPE()
						
					Endif
					
				#endif
				
			Next
			
		End Constructor
		
		Constructor TVECTOR##vector_type(iSize As Long , vData As vector_type)
			
			This.vData = Callocate(iSize , Sizeof(vector_type))
			
			If This.vData Then
				
				This.iSize = iSize
				
				This.iRealMemSize = iSize
				
				For i As Long = 0 To iSize-1
					
					#if Typeof(vector_type) = Typeof(String)
						
						(This.vData)[i] = Callocate(Len(vData)+1)
						
						If (This.vData)[i] Then
							
							*((This.vData)[i]) = vData
							
						Endif
						
					#elseif Typeof(vector_type) = Typeof(Wstring Ptr)
						
						If IsParameterMacro() Then
						
							(This.vData)[i] = Callocate((Len(*vData)+1)*Sizeof(Wstring))
							
							If (This.vData)[i] Then
								
								*((This.vData)[i]) = *vData
								
							Endif
						
						Else
						
							(This.vData)[i] = vData
						
						Endif
						
					#elseif Typeof(vector_type) = Typeof(Zstring Ptr)
						
						If IsParameterMacro() Then
						
							(This.vData)[i] = Callocate(Len(*vData)+1)
							
							If (This.vData)[i] Then
								
								*((This.vData)[i]) = *vData
								
							Endif
						
						Else
						
							(This.vData)[i] = vData
						
						Endif
						
					#else
						
						If IsParameterMacro() Then
							
							MACRO_START_CHECK_TYPE(vector_type)
								
								If vData Then
									
									(This.vData)[i] = Callocate(Sizeof(**This.vData))
									
									If (This.vData)[i] Then
										
										memcpy(Cast(Any Ptr , Cast(Integer , (This.vData)[i])) ,_
										Cast(Any Ptr , Cast(Integer , vData)) , Sizeof(**This.vData))
										
									Endif
									
								Endif
								
							MACRO_END_CHECK_TYPE()
							
						Else
							
							(This.vData)[i] = vData
							
						Endif
						
					#endif
					
				Next        
				
			Endif       
			
		End Constructor
		
		Destructor TVECTOR##vector_type()
			
			#if Typeof(vector_type) = Typeof(String)
				
				For i As Long = 0 To iSize - 1
					
					If (This.vData)[i] Then
						
						Deallocate((This.vData)[i])
						
					Endif
					
				Next
				
			#elseif Typeof(vector_type) = Typeof(Wstring Ptr)
				
				If IsParameterMacro() Then
				
					For i As Long = 0 To iSize - 1
						
						If (This.vData)[i] Then
							
							Deallocate((This.vData)[i])
							
						Endif
						
					Next
				
				Endif
				
			#elseif Typeof(vector_type) = Typeof(Zstring Ptr)
				
				If IsParameterMacro() Then
				
					For i As Long = 0 To iSize - 1
						
						If (This.vData)[i] Then
							
							Deallocate((This.vData)[i])
							
						Endif
						
					Next
				
				Endif
				
			#else
				
				If IsParameterMacro() Then
					
					For i As Long = 0 To iSize - 1
						
						If (This.vData)[i] Then
							
							Deallocate(Cast(Any Ptr , Cast(Integer ,(This.vData)[i])))
							
						Endif
						
					Next
					
				Endif
				
			#endif
			
			If This.vData Then Deallocate(This.vData)
			
			This.vData = 0
			
			This.iSize = 0
			
			This.iRealMemSize = 0   
			
		End Destructor
		
		' Возвращает количество элементов в векторе 
		Function TVECTOR##vector_type.size() As Long 
			
			Return iSize
			
		End Function
		
		' Доступ к последнему элементу
		Function TVECTOR##vector_type.back() As vector_type 
			
			If This.vData Andalso This.iSize Then
				
				#if Typeof(vector_type) = Typeof(String)
					
					If (This.vData)[This.iSize-1] Then
						
						Return *((This.vData)[This.iSize-1])
						
					Endif
					
				#else
					
					Return (This.vData)[This.iSize-1]
					
				#endif
				
			Endif
			
		End Function
		
		' Доступ к первому элементу
		Function TVECTOR##vector_type.front() As vector_type 
			
			If This.vData Andalso This.iSize Then
				
				#if Typeof(vector_type) = Typeof(String)
					
					If (This.vData)[0] Then
						
						Return *((This.vData)[0])
						
					Endif
					
				#else
					
					Return (This.vData)[0]
					
				#endif
				
			Endif    
			
		End Function
		
		' Возвращает true, если вектор пуст
		Function TVECTOR##vector_type.empty() As Boolean 
			
			If This.vData Andalso This.iSize Then
				
				Return False
				
			Else
				
				Return True
				
			Endif
			
		End Function
		
		' Возвращает количество элементов, которое может содержать вектор до того, как ему потребуется выделить больше места.
		Function TVECTOR##vector_type.capacity() As Long 
			
			Return This.iRealMemSize
			
		End Function
		
		' Удаляет все элементы вектора
		Function TVECTOR##vector_type.Clear() As Boolean 
			
			If This.vData Andalso This.iSize Then
				
				#if Typeof(vector_type) = Typeof(String)
					
					For i As Long = 0 To iSize - 1
						
						If (This.vData)[i] Then
							
							Deallocate((This.vData)[i])
							
						Endif
						
					Next
					
				#elseif Typeof(vector_type) = Typeof(Wstring Ptr)
					
					If IsParameterMacro() Then
						
						For i As Long = 0 To iSize - 1
							
							If (This.vData)[i] Then
								
								Deallocate((This.vData)[i])
								
							Endif
							
						Next
					
					Endif
					
				#elseif Typeof(vector_type) = Typeof(Zstring Ptr)
					
					If IsParameterMacro() Then
					
						For i As Long = 0 To iSize - 1
							
							If (This.vData)[i] Then
								
								Deallocate((This.vData)[i])
								
							Endif
							
						Next
					
					Endif
					
				#else
					
					If IsParameterMacro() Then
						
						For i As Long = 0 To iSize - 1
							
							If (This.vData)[i] Then
								
								Deallocate(Cast(Any Ptr , Cast(Integer ,(This.vData)[i])))
								
							Endif
							
						Next
						
					Endif
					
				#endif
				
				Deallocate(This.vData)
				
				This.vData = 0
				
				This.iSize = 0
				
				This.iRealMemSize = 0
				
				Return True 
				
			Else
				
				Return False
				
			Endif
			
		End Function
		
		' Вставка элементов в вектор перед iPos
		Function TVECTOR##vector_type.insert Overload(iPos As Long  , vData As vector_type) As Boolean 
			
			If iPos >=0 Then
				
				If This.iSize >= This.iRealMemSize Then
					
					This.iRealMemSize += 10
					
					This.vData = Reallocate(This.vData , (This.iRealMemSize)* Sizeof(vector_type))
					
					If This.vData = 0 Then
						
						Return False
						
					Endif
					
				Endif
				
				If iPos >= iSize Then
					
					#if Typeof(vector_type) = Typeof(String)
						
						(This.vData)[iSize] = Callocate(Len(vData)+1)
						
						If (This.vData)[iSize] Then
							
							*((This.vData)[iSize]) = vData
							
						Else
							
							Return False
							
						Endif
						
					#elseif Typeof(vector_type) = Typeof(Wstring Ptr)
						
						If IsParameterMacro() Then
						
							If vData Then
								
								(This.vData)[iSize] = Callocate((Len(*vData)+1)*Sizeof(Wstring))
								
								If (This.vData)[iSize] Then
									
									*((This.vData)[iSize]) = *vData
									
								Else
									
									Return False
									
								Endif
								
							Else
								
								Return False
								
							Endif
						
						Else
							
							(This.vData)[iSize] = vData
							
						Endif
						
					#elseif Typeof(vector_type) = Typeof(Zstring Ptr)
						
						If IsParameterMacro() Then
						
							If vData Then
								
								(This.vData)[iSize] = Callocate(Len(*vData)+1)
								
								If (This.vData)[iSize] Then
									
									*((This.vData)[iSize]) = *vData
									
								Else
									
									Return False
									
								Endif
								
							Else
								
								Return False
								
							Endif
						
						Else
							
							(This.vData)[iSize] = vData
							
						Endif
						
					#else
						
						If IsParameterMacro() Then
							
							MACRO_START_CHECK_TYPE(vector_type)
								
								If vData Then
									
									(This.vData)[iSize] = Callocate(Sizeof(**This.vData))
									
									If (This.vData)[iSize] Then
										
										memcpy(Cast(Any Ptr , Cast(Integer , (This.vData)[iSize])) ,_
										Cast(Any Ptr , Cast(Integer , vData)) , Sizeof(**This.vData))
										
									Endif
									
								Else
									
									Return False
									
								Endif
								
							MACRO_END_CHECK_TYPE()
							
						Else
							
							(This.vData)[iSize] = vData
							
						Endif
						
					#endif
					
				Else
					
					For i As Long = iSize To 0 Step -1
						
						If i > iPos Then
							
							(This.vData)[i] = (This.vData)[i-1]
							
						Else
							
							#if Typeof(vector_type) = Typeof(String)
								
								(This.vData)[i] = Callocate(Len(vData)+1)
								
								If (This.vData)[i] Then
									
									*((This.vData)[i]) = vData
									
								Else
									
									Return False
									
								Endif
								
							#elseif Typeof(vector_type) = Typeof(Wstring Ptr)
								
								If IsParameterMacro() Then
								
									If vData Then
										
										(This.vData)[i] = Callocate((Len(*vData)+1)*Sizeof(Wstring))
										
										If (This.vData)[i] Then
											
											*((This.vData)[i]) = *vData
											
										Else
											
											Return False
											
										Endif
										
									Else
										
										Return False
										
									Endif
								
								Else
									
									(This.vData)[i] = vData
									
								Endif
								
							#elseif Typeof(vector_type) = Typeof(Zstring Ptr)
								
								If IsParameterMacro() Then
								
									If vData Then
										
										(This.vData)[i] = Callocate(Len(*vData)+1)
										
										If (This.vData)[i] Then
											
											*((This.vData)[i]) = *vData
											
										Else
											
											Return False
											
										Endif
										
									Else
										
										Return False
										
									Endif
								
								Else
									
									(This.vData)[i] = vData
									
								Endif
								
							#else
								
								If IsParameterMacro() Then
									
									MACRO_START_CHECK_TYPE(vector_type)
										
										If vData Then
											
											(This.vData)[i] = Callocate(Sizeof(**This.vData))
											
											If (This.vData)[i] Then
												
												memcpy(Cast(Any Ptr , Cast(Integer , (This.vData)[i])) ,_
												Cast(Any Ptr , Cast(Integer , vData)) , Sizeof(**This.vData))
												
											Endif
											
										Else
											
											Return False
											
										Endif
										
									MACRO_END_CHECK_TYPE()
									
								Else
									
									(This.vData)[i] = vData
									
								Endif
								
							#endif
							
							Exit For
							
						Endif
						
					Next
					
				Endif
				
				iSize +=1
				
				Return True
				
			Else
				
				Return False
				
			Endif   
			
		End Function
		
		' Вставка элементов из массива в вектор перед iPos
		Function TVECTOR##vector_type.insert Overload(iPos As Long  , vData() As vector_type) As Boolean 
			
			If iPos >=0 Then
				
				If This.iSize + Ubound(vData) >= This.iRealMemSize Then
					
					This.iRealMemSize = This.iSize + Ubound(vData) + 10
					
					This.vData = Reallocate(This.vData , (This.iRealMemSize)* Sizeof(vector_type))
					
					If This.vData = 0 Then
						
						Return False
						
					Endif
					
				Endif
				
				If iPos >= iSize Then
					
					For i As Long = iSize To iSize + Ubound(vData)
						
						#if Typeof(vector_type) = Typeof(String)
							
							(This.vData)[i] = Callocate(Len(vData(i-iSize))+1)
							
							If (This.vData)[i] Then
								
								*((This.vData)[i]) = vData(i-iSize)
								
							Else
								
								Return False
								
							Endif
							
						#elseif Typeof(vector_type) = Typeof(Wstring Ptr)
							
							If IsParameterMacro() Then
							
								If vData(i-iSize) Then
									
									(This.vData)[i] = Callocate((Len(*vData(i-iSize))+1)*Sizeof(Wstring))
									
									If (This.vData)[i] Then
										
										*((This.vData)[i]) = *(vData(i-iSize))
										
									Else
										
										Return False
										
									Endif
									
								Else
									
									Return False
									
								Endif
							
							Else
								
								(This.vData)[i] = vData(i-iSize)
								
							Endif
							
						#elseif Typeof(vector_type) = Typeof(Zstring Ptr)
							
							If IsParameterMacro() Then
							
								If vData(i-iSize) Then
									
									(This.vData)[i] = Callocate(Len(*vData(i-iSize))+1)
									
									If (This.vData)[i] Then
										
										*((This.vData)[i]) = *(vData(i-iSize))
										
									Else
										
										Return False
										
									Endif
									
								Else
									
									Return False
									
								Endif
							
							Else
								
								(This.vData)[i] = vData(i-iSize)
								
							Endif
							
						#else
							
							If IsParameterMacro() Then
								
								MACRO_START_CHECK_TYPE(vector_type)
									
									If vData(i-iSize) Then
										
										(This.vData)[i] = Callocate(Sizeof(**This.vData))
										
										If (This.vData)[i] Then
											
											memcpy(Cast(Any Ptr , Cast(Integer , (This.vData)[i])) ,_
											Cast(Any Ptr , Cast(Integer , vData(i-iSize))) , Sizeof(**This.vData))
											
										Endif
										
									Else
										
										Return False
										
									Endif
									
								MACRO_END_CHECK_TYPE()
								
							Else
								
								(This.vData)[i] = vData(i-iSize)
								
							Endif
							
						#endif
						
					Next
					
				Else
					
					Dim As Long iIndex = Ubound(vData)
					
					For i As Long = iSize+Ubound(vData) To 0 Step -1
						
						If i > iPos+Ubound(vData) Then
							
							(This.vData)[i] = (This.vData)[i-Ubound(vData)-1]
							
						Else
							
							#if Typeof(vector_type) = Typeof(String)
								
								(This.vData)[i] = Callocate(Len(vData(iIndex))+1)
								
								If (This.vData)[i] Then
									
									*((This.vData)[i]) = vData(iIndex)
									
								Else
									
									Return False
									
								Endif
								
							#elseif Typeof(vector_type) = Typeof(Wstring Ptr)
							
								If IsParameterMacro() Then
									
									If vData(iIndex) Then
										
										(This.vData)[i] = Callocate((Len(*vData(iIndex))+1)*Sizeof(Wstring))
										
										If (This.vData)[i] Then
											
											*((This.vData)[i]) = *(vData(iIndex))
											
										Else
											
											Return False
											
										Endif
										
									Else
										
										Return False
										
									Endif

								Else
								
									(This.vData)[i] = vData(iIndex)
								
								Endif
								
							#elseif Typeof(vector_type) = Typeof(Zstring Ptr)
							
								If IsParameterMacro() Then
									
									If vData(iIndex) Then
										
										(This.vData)[i] = Callocate(Len(*vData(iIndex))+1)
										
										If (This.vData)[i] Then
											
											*((This.vData)[i]) = *(vData(iIndex))
											
										Else
											
											Return False
											
										Endif
										
									Else
										
										Return False
										
									Endif
									
								Else
								
									(This.vData)[i] = vData(iIndex)
								
								Endif
								
							#else
								
								If IsParameterMacro() Then
									
									MACRO_START_CHECK_TYPE(vector_type)
										
										If vData(iIndex) Then
											
											(This.vData)[i] = Callocate(Sizeof(**This.vData))
											
											If (This.vData)[i] Then
												
												memcpy(Cast(Any Ptr , Cast(Integer , (This.vData)[i])) ,_
												Cast(Any Ptr , Cast(Integer , vData(iIndex))) , Sizeof(**This.vData))
												
											Endif
											
										Else
											
											Return False
											
										Endif
										
									MACRO_END_CHECK_TYPE()
									
								Else
									
									(This.vData)[i] = vData(iIndex)
									
								Endif
								
							#endif
							
							iIndex-=1
							
							If iIndex < 0 Then Exit For 
							
						Endif
						
					Next
					
				Endif
				
				iSize += Ubound(vData)+1
				
				Return True
				
			Else
				
				Return False
				
			Endif   
			
		End Function
		
		' Вставка элементов из массива c диапазоном в вектор перед iPos
		Function TVECTOR##vector_type.insert Overload(iPos As Long  , vData() As vector_type , iStartPos As Long, iEndPos As Long) As Boolean 
			
			If iPos >=0 Andalso iStartPos>=0 Andalso iEndPos >= iStartPos Then
				
				Dim As Long iEndMinusStart = (iEndPos - iStartPos)
				
				If This.iSize + iEndMinusStart >= This.iRealMemSize Then
					
					This.iRealMemSize = This.iSize + iEndMinusStart + 10
					
					This.vData = Reallocate(This.vData , (This.iRealMemSize)* Sizeof(vector_type))
					
					If This.vData = 0 Then
						
						Return False
						
					Endif
					
				Endif
				
				If iPos >= iSize Then
					
					For i As Long = iSize To iSize + iEndMinusStart
						
						#if Typeof(vector_type) = Typeof(String)
							
							(This.vData)[i] = Callocate(Len(vData(i - iSize + iStartPos))+1)
							
							If (This.vData)[i] Then
								
								*((This.vData)[i]) = vData(i - iSize + iStartPos)
								
							Else
								
								Return False
								
							Endif
							
						#elseif Typeof(vector_type) = Typeof(Wstring Ptr)
							
							If IsParameterMacro() Then
							
								If vData(i - iSize + iStartPos) Then
									
									(This.vData)[i] = Callocate((Len(*vData(i - iSize + iStartPos))+1)*Sizeof(Wstring))
									
									If (This.vData)[i] Then
										
										*((This.vData)[i]) = *(vData(i - iSize + iStartPos))
										
									Else
										
										Return False
										
									Endif
									
								Else
									
									Return False
									
								Endif
							
							Else
							
								(This.vData)[i] = vData(i - iSize + iStartPos)
							
							Endif
							
						#elseif Typeof(vector_type) = Typeof(Zstring Ptr)
							
							If IsParameterMacro() Then
							
								If vData(i - iSize + iStartPos) Then
									
									(This.vData)[i] = Callocate(Len(*vData(i - iSize + iStartPos))+1)
									
									If (This.vData)[i] Then
										
										*((This.vData)[i]) = *(vData(i - iSize + iStartPos))
										
									Else
										
										Return False
										
									Endif
									
								Else
									
									Return False
									
								Endif
							
							Else
							
								(This.vData)[i] = vData(i - iSize + iStartPos)
							
							Endif
							
						#else
							
							If IsParameterMacro() Then
								
								MACRO_START_CHECK_TYPE(vector_type)
									
									If vData(i - iSize + iStartPos) Then
										
										(This.vData)[i] = Callocate(Sizeof(**This.vData))
										
										If (This.vData)[i] Then
											
											memcpy(Cast(Any Ptr , Cast(Integer , (This.vData)[i])) ,_
											Cast(Any Ptr , Cast(Integer , vData(i - iSize + iStartPos))) , Sizeof(**This.vData))
											
										Endif
										
									Else
										
										Return False
										
									Endif
									
								MACRO_END_CHECK_TYPE()
								
							Else
								
								(This.vData)[i] = vData(i - iSize + iStartPos)
								
							Endif
							
						#endif
						
					Next
					
				Else
					
					Dim As Long iIndex = iEndPos
					
					For i As Long = iSize+iEndMinusStart To 0 Step -1
						
						If i > iPos+iEndMinusStart Then
							
							(This.vData)[i] = (This.vData)[i-iEndMinusStart-1]
							
						Else
							
							#if Typeof(vector_type) = Typeof(String)
								
								(This.vData)[i] = Callocate(Len(vData(iIndex))+1)
								
								If (This.vData)[i] Then
									
									*((This.vData)[i]) = vData(iIndex)
									
								Endif
								
							#elseif Typeof(vector_type) = Typeof(Wstring Ptr)
								
								If IsParameterMacro() Then
								
									If vData(iIndex) Then
										
										(This.vData)[i] = Callocate((Len(*vData(iIndex))+1)*Sizeof(Wstring))
										
										If (This.vData)[i] Then
											
											*((This.vData)[i]) = *(vData(iIndex))
											
										Endif
										
									Else
										
										Return False
										
									Endif
								
								Else
								
									(This.vData)[i] = vData(iIndex)
								
								Endif
								
							#elseif Typeof(vector_type) = Typeof(Zstring Ptr)
								
								If IsParameterMacro() Then
								
									If vData(iIndex) Then
										
										(This.vData)[i] = Callocate(Len(*vData(iIndex))+1)
										
										If (This.vData)[i] Then
											
											*((This.vData)[i]) = *(vData(iIndex))
											
										Endif
										
									Else
										
										Return False
										
									Endif
								
								Else
								
									(This.vData)[i] = vData(iIndex)
								
								Endif
								
							#else
								
								If IsParameterMacro() Then
									
									MACRO_START_CHECK_TYPE(vector_type)
										
										If vData(iIndex) Then
											
											(This.vData)[i] = Callocate(Sizeof(**This.vData))
											
											If (This.vData)[i] Then
												
												memcpy(Cast(Any Ptr , Cast(Integer , (This.vData)[i])) ,_
												Cast(Any Ptr , Cast(Integer , vData(iIndex))) , Sizeof(**This.vData))
												
											Endif
											
										Else
											
											Return False
											
										Endif
										
									MACRO_END_CHECK_TYPE()
									
								Else
									
									(This.vData)[i] = vData(iIndex)
									
								Endif								
								
							#endif
							
							iIndex-=1
							
							If iIndex < iStartPos Then Exit For 
							
						Endif
						
					Next
					
				Endif
				
				iSize += iEndMinusStart+1
				
				Return True
				
			Else
				
				Return False
				
			Endif
			
		End Function
		
		' Удаляет указанный элемент вектора 
		Function TVECTOR##vector_type.Erase Overload(iPos As Long) As Boolean 
			
			If This.vData Andalso This.iSize _ 
			Andalso iPos >=0 Andalso iPos < iSize Then
				
				If iPos = iSize-1 Then
					
					#if Typeof(vector_type) = Typeof(String)
						
						If (This.vData)[iSize-1] Then
							
							Deallocate((This.vData)[iSize-1])
							
						Endif
						
					#elseif Typeof(vector_type) = Typeof(Wstring Ptr)
						
						If IsParameterMacro() Andalso (This.vData)[iSize-1] Then
							
							Deallocate((This.vData)[iSize-1])
							
						Endif
						
					#elseif Typeof(vector_type) = Typeof(Zstring Ptr)
						
						If IsParameterMacro() Andalso (This.vData)[iSize-1] Then
							
							Deallocate((This.vData)[iSize-1])
							
						Endif
						
					#else
						
						If IsParameterMacro() Andalso (This.vData)[iSize-1] Then
							
							Deallocate(Cast(Any Ptr , Cast(Integer ,(This.vData)[iSize-1])))
							
						Endif
						
					#endif
					
					(This.vData)[iSize-1] = 0
					
				Else
					
					For i As Long = 0 To iSize-2
						
						If i >= iPos Then
							
							(This.vData)[i] = (This.vData)[i+1]
							
						Endif
						
					Next
					
				Endif
				
				This.iSize -=1
				
				Return This.realloc()
				
			Else
				
				Return False
				
			Endif   
			
		End Function
		
		' Удаляет диапазон указанных элементов вектора  
		Function TVECTOR##vector_type.Erase Overload(iStartPos As Long , iEndPos As Long) As Boolean 
			
			If iStartPos = iEndPos Then 
				
				Return Erase(iStartPos)
				
			Endif
			
			If This.vData Andalso This.iSize _
			Andalso iStartPos < iEndPos _
			Andalso iStartPos >= 0 Andalso iEndPos < This.iSize Then
				
				If iEndPos = iSize-1 Then
					
					For i As Long = iStartPos To iEndPos
						
						#if Typeof(vector_type) = Typeof(String)
							
							If (This.vData)[i] Then
								
								Deallocate((This.vData)[i])
								
							Endif
							
						#elseif Typeof(vector_type) = Typeof(Wstring Ptr)
							
							If IsParameterMacro() Andalso (This.vData)[i] Then
								
								Deallocate((This.vData)[i])
								
							Endif
							
						#elseif Typeof(vector_type) = Typeof(Zstring Ptr)
							
							If IsParameterMacro() Andalso (This.vData)[i] Then
								
								Deallocate((This.vData)[i])
								
							Endif
							
						#else
							
							If IsParameterMacro() Andalso (This.vData)[i] Then
								
								Deallocate(Cast(Any Ptr , Cast(Integer ,(This.vData)[i])))
								
							Endif
							
						#endif
						
						(This.vData)[i] = 0
						
					Next    
					
				Else
					
					Dim As Long iStartIndex = iStartPos
					
					For i As Long = 0 To iSize-2
						
						If i >= iEndPos Then
							
							(This.vData)[iStartIndex] = (This.vData)[i+1]
							
							iStartIndex+=1
							
						Endif
						
					Next            
					
				Endif
				
				This.iSize = This.iSize - (iEndPos - iStartPos + 1)
				
				Return This.realloc()
				
			Else
				
				Return False        
				
			Endif
			
		End Function
		
		' Вставка элемента в конец вектора  
		Function TVECTOR##vector_type.push_back(vData As vector_type) As Boolean
			
			If This.iSize >= This.iRealMemSize Then
				
				This.iRealMemSize += 10
				
				This.vData = Reallocate(This.vData , (This.iRealMemSize)* Sizeof(vector_type))
				
				If This.vData = 0 Then
					
					Return False
					
				Endif
				
			Endif
			
			#if Typeof(vector_type) = Typeof(String)
				
				(This.vData)[iSize] = Callocate(Len(vData)+1)
				
				If (This.vData)[iSize] Then
					
					*((This.vData)[iSize]) = vData
					
				Endif
				
			#elseif Typeof(vector_type) = Typeof(Wstring Ptr)
				
				If IsParameterMacro() Then
				
					If vData Then
						
						(This.vData)[iSize] = Callocate((Len(*vData)+1)*Sizeof(Wstring))
						
						If (This.vData)[iSize] Then
							
							*((This.vData)[iSize]) = *vData
							
						Endif
						
					Else
						
						Return False
						
					Endif
				
				Else
				
					(This.vData)[iSize] = vData
				
				Endif
				
			#elseif Typeof(vector_type) = Typeof(Zstring Ptr)
				
				If IsParameterMacro() Then
				
					If vdata Then
						
						(This.vData)[iSize] = Callocate(Len(*vData)+1)
						
						If (This.vData)[iSize] Then
							
							*((This.vData)[iSize]) = *vData
							
						Endif
						
					Else
						
						Return False
						
					Endif
				
				Else
				
					(This.vData)[iSize] = vData
				
				Endif
				
			#else
				
				If IsParameterMacro() Then
					
					MACRO_START_CHECK_TYPE(vector_type)
						
						If vdata Then
							
							(This.vData)[iSize] = Callocate(Sizeof(**This.vData))
							
							If (This.vData)[iSize] Then
								
								memcpy(Cast(Any Ptr , Cast(Integer , (This.vData)[iSize])) ,_
								Cast(Any Ptr , Cast(Integer , vData)) , Sizeof(**This.vData))
								
							Endif
							
						Else
							
							Return False
							
						Endif
						
					MACRO_END_CHECK_TYPE()
					
				Else
					
					(This.vData)[iSize] = vData
					
				Endif
				
			#endif
			
			iSize +=1
			
			Return True
			
		End Function
		
		' Удалить последний элемент вектора 
		Function TVECTOR##vector_type.pop_back() As vector_type 
			
			If This.vData Andalso This.iSize Then
				
				This.iSize -=1
				
				#if Typeof(vector_type) = Typeof(String)
					
					If (This.vData)[This.iSize] Then
						
						Function = *((This.vData)[This.iSize])
						
						Deallocate((This.vData)[This.iSize])
						
					Endif
					
				#elseif Typeof(vector_type) = Typeof(Wstring Ptr)
					
					If IsParameterMacro() Then
					
						If (This.vData)[This.iSize] Then
							
							Function = ((This.vData)[This.iSize])
							
							Deallocate((This.vData)[This.iSize])
							
						Endif
					
					Else
					
						Function = (This.vData)[This.iSize]
					
					Endif
					
				#elseif Typeof(vector_type) = Typeof(Zstring Ptr)
					
					If IsParameterMacro() Then
					
						If (This.vData)[This.iSize] Then
							
							Function = ((This.vData)[This.iSize])
							
							Deallocate((This.vData)[This.iSize])
							
						Endif
					
					Else
					
						Function = (This.vData)[This.iSize]
					
					Endif
					
				#else
					
					If IsParameterMacro() Andalso (This.vData)[This.iSize] Then
						
						Deallocate(Cast(Any Ptr , Cast(Integer , (This.vData)[This.iSize])))
						
					Endif
					
					Function = (This.vData)[This.iSize]
					
				#endif
				
				(This.vData)[This.iSize] = 0
				
				This.realloc()
				
			Endif   
			
		End Function
		
		' Изменяет размер вектора на заданную величину  
		Function TVECTOR##vector_type.resize Overload(iCount As Long ) As Boolean
			
			If This.iSize > iCount Then
				
				This.iSize = iCount
				
				Return This.realloc()
				
			Elseif This.iSize < iCount Then
				
				If iCount >= This.iRealMemSize Then
					
					This.iRealMemSize = iCount + 10
					
					This.vData = Reallocate(This.vData , (This.iRealMemSize)* Sizeof(vector_type))
					
					If This.vData = 0 Then Return False
					
				Endif
				
				This.iSize = iCount
				
				Return True
				
			Else
				
				Return False
				
			Endif 
			
		End Function
		
		' Изменяет размер вектора на заданную величину  
		Function TVECTOR##vector_type.resize Overload(iCount As Long ,  vData As vector_type) As Boolean 
			
			If This.iSize > iCount Then
				
				This.iSize = iCount
				
				Return This.realloc()
				
			Elseif This.iSize < iCount Then
				
				If iCount >= This.iRealMemSize Then
					
					This.iRealMemSize = iCount + 10
					
					This.vData = Reallocate(This.vData , (This.iRealMemSize)* Sizeof(vector_type))
					
					If This.vData = 0 Then Return False
					
				Endif
				
				Dim As Long iOld = This.iSize
				
				This.iSize = iCount
				
				For i As Long = iOld To iCount - 1
					
					#if Typeof(vector_type) = Typeof(String)
						
						(This.vData)[i] = Callocate(Len(vData)+1)
						
						If (This.vData)[i] Then
							
							*((This.vData)[i]) = vData
							
						Endif
						
					#elseif Typeof(vector_type) = Typeof(Wstring Ptr)
						
						If IsParameterMacro() Then
						
							If vData Then
								
								(This.vData)[i] = Callocate((Len(*vData)+1)*Sizeof(Wstring))
								
								If (This.vData)[i] Then
									
									*((This.vData)[i]) = *vData
									
								Endif
								
							Else
								
								Return False
								
							Endif
						
						Else
						
							(This.vData)[i] = vData
						
						Endif
						
					#elseif Typeof(vector_type) = Typeof(Zstring Ptr)
						
						If IsParameterMacro() Then
						
							If vData Then
								
								(This.vData)[i] = Callocate(Len(*vData)+1)
								
								If (This.vData)[i] Then
									
									*((This.vData)[i]) = *vData
									
								Endif
								
							Else
								
								Return False
								
							Endif
						
						Else
						
							(This.vData)[i] = vData
						
						Endif
						
					#else
						
						If IsParameterMacro() Then
							
							MACRO_START_CHECK_TYPE(vector_type)
								
								If vData Then
									
									(This.vData)[i] = Callocate(Sizeof(**This.vData))
									
									If (This.vData)[i] Then
										
										memcpy(Cast(Any Ptr , Cast(Integer , (This.vData)[i])) ,_
										Cast(Any Ptr , Cast(Integer , vData)) , Sizeof(**This.vData))
										
									Else
										
										Return False
										
									Endif
									
								Else
									
									Return False
									
								Endif
								
							MACRO_END_CHECK_TYPE()
							
						Else
							
							(This.vData)[i] = vData
							
						Endif
						
					#endif
					
				Next
				
				Return True
				
			Else
				
				Return False
				
			Endif
			
		End Function
		
		' Заменяет содержимое контейнера вектора    
		Function TVECTOR##vector_type.assign Overload(iCount As Long ,  vData As vector_type) As Boolean
			
			If This.iSize < iCount Andalso iCount >= This.iRealMemSize Then
				
				This.iRealMemSize = iCount + 10
				
				This.vData = Reallocate(This.vData , (This.iRealMemSize)* Sizeof(vector_type))
				
				If This.vData = 0 Then Return False
				
				This.iSize = iCount
				
			Endif
			
			For i As Long = 0 To iCount - 1
				
				#if Typeof(vector_type) = Typeof(String)
					
					If (This.vData)[i] Then
						
						Deallocate((This.vData)[i])
						
					Endif
					
					(This.vData)[i] = Callocate(Len(vData)+1)
					
					If (This.vData)[i] Then
						
						*((This.vData)[i]) = vData
						
					Endif
					
				#elseif Typeof(vector_type) = Typeof(Wstring Ptr)
					
					If IsParameterMacro() Then
					
						If vData Then
							
							If (This.vData)[i] Then
								
								Deallocate((This.vData)[i])
								
							Endif
							
							(This.vData)[i] = Callocate((Len(*vData)+1)*Sizeof(Wstring))
							
							If (This.vData)[i] Then
								
								*((This.vData)[i]) = *vData
								
							Endif
							
						Else
							
							Return False
							
						Endif
					
					Else
					
						(This.vData)[i] = vData
					
					Endif
					
				#elseif Typeof(vector_type) = Typeof(Zstring Ptr)
					
					If IsParameterMacro() Then
					
						If vData Then
							
							If (This.vData)[i] Then
								
								Deallocate((This.vData)[i])
								
							Endif
							
							(This.vData)[i] = Callocate(Len(*vData)+1)
							
							If (This.vData)[i] Then
								
								*((This.vData)[i]) = *vData
								
							Endif
							
						Else
							
							Return False
							
						Endif
					
					Else
					
						(This.vData)[i] = vData
					
					Endif
					
				#else
					
					If IsParameterMacro() Then
						
						MACRO_START_CHECK_TYPE(vector_type)
						
						If vData Then
							
							memcpy(Cast(Any Ptr , Cast(Integer , (This.vData)[i])) ,_
							Cast(Any Ptr , Cast(Integer , vData)) , Sizeof(**This.vData))
							
						Else
							
							Return False
							
						Endif
						
						MACRO_END_CHECK_TYPE()
						
					Else
						
						(This.vData)[i] = vData
						
					Endif
					
				#endif
				
			Next    
			
			Return True  
			
		End Function
		
		' Заменяет содержимое контейнера вектора в диапазоне
		Function TVECTOR##vector_type.assign Overload(iStartPos As Long , iEndPos As Long ,  vData As vector_type) As Boolean
			
			If iStartPos >= 0 Then
				
				If This.iSize <= iEndPos Andalso iEndPos > 0 Andalso (iEndPos-1) >= This.iRealMemSize Then
					
					This.iRealMemSize = iEndPos + 10
					
					This.vData = Reallocate(This.vData , (This.iRealMemSize)* Sizeof(vector_type))
					
					If This.vData = 0 Then Return False
					
					This.iSize = iEndPos + 1
					
				Endif
				
				For i As Long = iStartPos To iEndPos
					
					#if Typeof(vector_type) = Typeof(String)
						
						If (This.vData)[i] Then
							
							Deallocate((This.vData)[i])
							
						Endif
						
						(This.vData)[i] = Callocate(Len(vData)+1)
						
						If (This.vData)[i] Then
							
							*((This.vData)[i]) = vData
							
						Endif
						
					#elseif Typeof(vector_type) = Typeof(Wstring Ptr)
						
						If IsParameterMacro() Then
						
							If vData Then
								
								If (This.vData)[i] Then
									
									Deallocate((This.vData)[i])
									
								Endif
								
								(This.vData)[i] = Callocate((Len(*vData)+1)*Sizeof(Wstring))
								
								If (This.vData)[i] Then
									
									*((This.vData)[i]) = *vData
									
								Endif
								
							Else
								
								Return False
								
							Endif
						
						Else
						
							(This.vData)[i] = vData
						
						Endif
						
					#elseif Typeof(vector_type) = Typeof(Zstring Ptr)
						
						If IsParameterMacro() Then
						
							If vData Then
								
								If (This.vData)[i] Then
									
									Deallocate((This.vData)[i])
									
								Endif
								
								(This.vData)[i] = Callocate(Len(*vData)+1)
								
								If (This.vData)[i] Then
									
									*((This.vData)[i]) = *vData
									
								Endif
								
							Else
								
								Return False
								
							Endif
						
						Else
						
							(This.vData)[i] = vData
						
						Endif
						
					#else
						
						If IsParameterMacro() Then
							
							MACRO_START_CHECK_TYPE(vector_type)
							
							If vData Then
								
								memcpy(Cast(Any Ptr , Cast(Integer , (This.vData)[i])) ,_
								Cast(Any Ptr , Cast(Integer , vData)) , Sizeof(**This.vData))
								
							Else
								
								Return False
								
							Endif
							
							MACRO_END_CHECK_TYPE()
							
						Else
							
							(This.vData)[i] = vData
							
						Endif
						
					#endif
					
				Next    
				
				Return True 
				
			Else
				
				Return False        
				
			Endif    
			
		End Function
		
		' Обменивает значения векторов
		Function TVECTOR##vector_type.swapV(v As TVECTOR##vector_type) As Boolean
			
			If This.vData Andalso v.vData Andalso _ 
			This.iSize Andalso v.iSize Andalso _
			This.iRealMemSize Andalso v.iRealMemSize Then
				
				Dim As Any Ptr pTemp = This.vData
				
				This.vData = v.vData
				
				v.vData = pTemp
				
				Dim As Long iTemp = This.iSize
				
				This.iSize = v.iSize
				
				v.iSize = iTemp
				
				iTemp = This.iRealMemSize
				
				This.iRealMemSize = v.iRealMemSize
				
				v.iRealMemSize = iTemp  
				
				Return True 
				
			Else
				
				Return False
				
			Endif
			
		End Function
		
		' Уменьшает количество используемой памяти за счёт освобождения неиспользованной
		Function TVECTOR##vector_type.shrink_to_fit() As Boolean
			
			If This.vData Then
				
				If This.iSize < This.iRealMemSize Then
					
					This.vData = Reallocate(This.vData , (This.iSize) * Sizeof(vector_type))
					
					If This.vData Then
						
						This.iRealMemSize = This.iSize
						
						Return True
						
					Else
						
						Return False
						
					Endif           
					
				Endif
				
			Else
				
				Return False
				
			Endif
			
		End Function
		
		' доступ к ячейкам с проверкой диапазона (получение)
		Property TVECTOR##vector_type.at(iIndex As Long) As vector_type
			
			If iIndex < 0 Orelse iIndex > This.iSize-1 Then
				
				Print "out of range!"
				
			Else
				
				#if Typeof(vector_type) = Typeof(String)
					
					If (This.vData)[iIndex] Then
						
						Return *((This.vData)[iIndex])
						
					Endif
					
				#else
					
					Return (This.vData)[iIndex]
					
				#endif
				
			Endif
			
		End Property
		
		' доступ к ячейкам с проверкой диапазона (установка)
		Property TVECTOR##vector_type.at(iIndex As Long , vData As vector_type)
			
			If iIndex < 0 Orelse iIndex > This.iSize-1 Then
				
				Print "out of range!"
				
			Else
				
				#if Typeof(vector_type) = Typeof(String)
					
					If (This.vData)[iIndex] Then
						
						Deallocate((This.vData)[iIndex])
						
					Endif
					
					(This.vData)[iIndex] = Callocate(Len(vData)+1)
					
					If (This.vData)[iIndex] Then
						
						*((This.vData)[iIndex]) = vData
						
					Endif
					
				#elseif Typeof(vector_type) = Typeof(Wstring Ptr)
					
					If IsParameterMacro() Then
					
						If vData Then
							
							If (This.vData)[iIndex] Then
								
								Deallocate((This.vData)[iIndex])
								
							Endif
							
							(This.vData)[iIndex] = Callocate((Len(*vData)+1)*Sizeof(Wstring))
							
							If (This.vData)[iIndex] Then
								
								*((This.vData)[iIndex]) = *vData
								
							Endif
							
						Endif
					
					Else
					
						(This.vData)[iIndex] = vData
					
					Endif
					
				#elseif Typeof(vector_type) = Typeof(Zstring Ptr)
					
					If IsParameterMacro() Then
					
						If vData Then
							
							If (This.vData)[iIndex] Then
								
								Deallocate((This.vData)[iIndex])
								
							Endif
							
							(This.vData)[iIndex] = Callocate(Len(*vData)+1)
							
							If (This.vData)[iIndex] Then
								
								*((This.vData)[iIndex]) = *vData
								
							Endif
							
						Endif
					
					Else
					
						(This.vData)[iIndex] = vData
					
					Endif
					
				#else
					
					If IsParameterMacro() Then
						
						If vData Then
							
							MACRO_START_CHECK_TYPE(vector_type)
								
								If (This.vData)[iIndex] Then
									
									Deallocate(Cast(Any Ptr , Cast(Integer , (This.vData)[iIndex])))
									
								Endif
								
								(This.vData)[iIndex] = Callocate(Sizeof(**This.vData))
								
								If (This.vData)[iIndex] Then
									
									memcpy(Cast(Any Ptr , Cast(Integer , (This.vData)[iIndex])) ,_
									Cast(Any Ptr , Cast(Integer , vData)) , Sizeof(**This.vData))
									
								Endif
								
							MACRO_END_CHECK_TYPE()
							
						Endif	
						
					Else
						
						(This.vData)[iIndex] = vData
						
					Endif
					
				#endif       
				
			Endif   
			
		End Property
		
		' перераспределение памяти
		Function TVECTOR##vector_type.realloc() As Boolean
			
			If This.iSize+20 < This.iRealMemSize Then
				
				This.iRealMemSize = This.iSize + 10
				
				This.vData = Reallocate(This.vData , (This.iRealMemSize)* Sizeof(vector_type))
				
				If This.vData Then
					
					Return True
					
				Else
					
					Return False
					
				Endif
				
			Endif   
			
		End Function
		
		' проверка: есть ли второй параметр макроса
		Function TVECTOR##vector_type.IsParameterMacro() As Long
			
			Function = 0
			
			MACRO_START_CHECK_TYPE(vector_type)
				
				Return Val(Trim(#AllocMem))
				
			MACRO_END_CHECK_TYPE()
			
		End Function
		
		' обычный доступ к ячейкам
		
		#if Typeof(vector_type) = Typeof(String)
			
			Operator TVECTOR##vector_type.[] (Byref iIndex As Long) As vector_type
				
				If (This.vData)[iIndex] Then
					
					Return *((This.vData)[iIndex])
					
				Endif
				
			End Operator
			
		#else
			
			Operator TVECTOR##vector_type.[] (Byref iIndex As Long) Byref As vector_type
				
				Return (This.vData)[iIndex]
				
			End Operator
			
		#endif
		
		' копирование одного вектора в другой
		Operator TVECTOR##vector_type.Let (Byref v2 As TVECTOR##vector_type) 
			
			If v2.vData Andalso v2.iRealMemSize Then
				
				If This.vData Then
					
					#if Typeof(vector_type) = Typeof(String)
						
						For i As Long = 0 To iSize - 1
							
							If (This.vData)[i] Then
								
								Deallocate((This.vData)[i])
								
							Endif
							
						Next
						
					#elseif Typeof(vector_type) = Typeof(Wstring Ptr)
						
						If IsParameterMacro() Then
						
							For i As Long = 0 To iSize - 1
								
								If (This.vData)[i] Then
									
									Deallocate((This.vData)[i])
									
								Endif
								
							Next
						
						Endif
						
					#elseif Typeof(vector_type) = Typeof(Zstring Ptr)
						
						If IsParameterMacro() Then
						
							For i As Long = 0 To iSize - 1
								
								If (This.vData)[i] Then
									
									Deallocate((This.vData)[i])
									
								Endif
								
							Next
						
						Endif
						
					#else
						
						If IsParameterMacro() Then
							
							For i As Long = 0 To iSize - 1
								
								If (This.vData)[i] Then
									
									Deallocate(Cast(Any Ptr , Cast(Integer , (This.vData)[i])))
									
								Endif
								
							Next
							
						Endif
						
					#endif
					
					Deallocate(This.vData)
					
				Endif   
				
				This.vData = Callocate(v2.iRealMemSize , Sizeof(vector_type) )
				
				If This.vData Then  
					
					This.iRealMemSize = v2.iRealMemSize
					
					This.iSize = v2.iSize
					
					For i As Long = 0 To v2.iSize -1
						
						#if Typeof(vector_type) = Typeof(String)
							
							If v2.vData[i] Then
								
								(This.vData)[i] = Callocate(Len(*(v2.vData[i]))+1)
								
								If (This.vData)[i] Then
									
									*((This.vData)[i]) = *(v2.vData[i])
									
								Endif
								
							Endif
							
						#elseif Typeof(vector_type) = Typeof(Wstring Ptr)
							
							If IsParameterMacro() Then
							
								If v2.vData[i] Then
									
									(This.vData)[i] = Callocate((Len(*(v2.vData[i]))+1)*Sizeof(Wstring))
									
									If (This.vData)[i] Then
										
										*((This.vData)[i]) = *(v2.vData[i])
										
									Endif
									
								Endif
							
							Else
							
								This.vData[i] = v2.vData[i]
							
							Endif
							
						#elseif Typeof(vector_type) = Typeof(Zstring Ptr)
							
							If IsParameterMacro() Then
							
								If v2.vData[i] Then
									
									(This.vData)[i] = Callocate(Len(*(v2.vData[i]))+1)
									
									If (This.vData)[i] Then
										
										*((This.vData)[i]) = *(v2.vData[i])
										
									Endif
									
								Endif
							
							Else
							
								This.vData[i] = v2.vData[i]
							
							Endif
							
						#else
							
							If IsParameterMacro() Then
								
								MACRO_START_CHECK_TYPE(vector_type)
								
									If v2.vData[i] Then
										
										(This.vData)[i] = Callocate(Sizeof(**This.vData))
										
										If (This.vData)[i] Then
											
											memcpy(Cast(Any Ptr , Cast(Integer ,(This.vData)[i])) , _
											Cast(Any Ptr , Cast(Integer ,v2.vData[i])) , Sizeof(**This.vData))
											
										Endif
										
									Endif
									
								MACRO_END_CHECK_TYPE()
								
							Else
								
								This.vData[i] = v2.vData[i]
								
							Endif
							
						#endif
						
					Next
					
				Endif
				
			Else
				
				This.Clear()
				
			Endif
			
		End Operator
		
		' проверка на равенство
		Operator = ( v1 As TVECTOR##vector_type , v2 As TVECTOR##vector_type) As Boolean 
			
			If v1.vData Andalso v2.vData _
			Andalso v1.iSize = v2.iSize Then
				
				If v1.iSize Then
					
					For i As Long = 0 To v1.iSize-1
						
						#if Typeof(vector_type) = Typeof(String)
							
							If (v1.vData)[i] Andalso (v2.vData)[i] Then
								
								If *((v1.vData)[i]) <> *((v2.vData)[i]) Then
									
									Return False
									
								Endif
								
							Elseif (v1.vData)[i] <> (v2.vData)[i] Then
								
								Return False
								
							Endif
							
						#elseif Typeof(vector_type) = Typeof(Wstring Ptr)
							
							If v1.IsParameterMacro() Then
							
								If (v1.vData)[i] Andalso (v2.vData)[i] Then
									
									If *((v1.vData)[i]) <> *((v2.vData)[i]) Then
										
										Return False
										
									Endif
									
								Elseif (v1.vData)[i] <> (v2.vData)[i] Then
									
									Return False
									
								Endif
							
							Else
							
								If (v1.vData)[i] <> (v2.vData)[i] Then
									
									Return False
									
								Endif
							
							Endif
							
						#elseif Typeof(vector_type) = Typeof(Zstring Ptr)
							
							If v1.IsParameterMacro() Then
							
								If (v1.vData)[i] Andalso (v2.vData)[i] Then
									
									If *((v1.vData)[i]) <> *((v2.vData)[i]) Then
										
										Return False
										
									Endif
									
								Elseif (v1.vData)[i] <> (v2.vData)[i] Then
									
									Return False
									
								Endif
							
							Else
							
								If (v1.vData)[i] <> (v2.vData)[i] Then
									
									Return False
									
								Endif
							
							Endif
							
						#else
							
							If v1.IsParameterMacro() Then
								
								MACRO_START_CHECK_TYPE(vector_type)
								
								If (v1.vData)[i] Andalso (v2.vData)[i] Then
									
									If memcmp(Cast(Any Ptr , Cast(Integer ,(v1.vData)[i])) ,_
									Cast(Any Ptr , Cast(Integer ,(v2.vData)[i])) , Sizeof(*((v1.vData)[i]))) Then
										
										Return False
										
									Endif
									
								Elseif (v1.vData)[i] <> (v2.vData)[i] Then
									
									Return False
									
								Endif
								
								MACRO_END_CHECK_TYPE()
								
							Else
								
								If (v1.vData)[i] <> (v2.vData)[i] Then
									
									Return False
									
								Endif
								
							Endif
							
						#endif
						
					Next
					
				Endif
				
				Return True
				
			Elseif (v1.iSize = 0 Andalso v2.iSize = 0) Then
				
				Return True
				
			Else
				
				Return False
				
			Endif
			
		End Operator
		
	#endif
	
#endmacro
