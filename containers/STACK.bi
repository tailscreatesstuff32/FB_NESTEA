#macro MStackTemplate(stack_type)
	
	#ifndef TSTACKNODE##stack_type
		
		Type TSTACKNODE##stack_type
			
			As stack_type xValue
			
			As TSTACKNODE##stack_type Ptr pNext = 0
			
		End Type
		
	#endif
	
	#ifndef TSTACK##stack_type
		
		Type TSTACK##stack_type
			
			Declare Sub push(As stack_type)
			
			Declare Function pop() As stack_type
			
			Declare Function front() As stack_type
			
			Declare Function size() As Long
			
			As Long iSize = 0
			
			As TSTACKNODE##stack_type Ptr pFirst = 0
			
		End Type
		
		Sub TSTACK##stack_type.push(xParam As stack_type)
			
			Dim As TSTACKNODE##stack_type Ptr pTemp = New TSTACKNODE##stack_type
			
			pTemp->xValue = xParam
			
			If pFirst = 0 Then
				
				pFirst = pTemp
				
				iSize = 1
				
			Else
				
				pTemp->pNext = pFirst
				
				pFirst = pTemp
				
				iSize += 1
				
			Endif
			
		End Sub
		
		Function TSTACK##stack_type.pop() As stack_type
			
			If iSize Andalso pFirst Then
				
				Dim As TSTACKNODE##stack_type Ptr p = pFirst->pNext
				
				Function = pFirst->xValue
				
				Delete(pFirst)
				
				pFirst = p
				
				iSize -=1
				
			Endif
			
		End Function
		
		Function TSTACK##stack_type.front() As stack_type
			
			If pFirst Then
				
				Return pFirst->xValue
				
			Endif
			
		End Function
		
		Function TSTACK##stack_type.size() As Long
			
			Return iSize
			
		End Function
		
	#endif
	
#endmacro
