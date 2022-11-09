#macro MListTemplate(list_type)
	
	#ifndef TLISTNODE##list_type
		
		Type TLISTNODE##list_type
			
			xValue As list_type
			
			pPrev As TLISTNODE##list_type Ptr = 0
			
			pNext As TLISTNODE##list_type Ptr = 0
			
		End Type
		
	#endif
	
	#ifndef TLIST##list_type
		
		Type TLIST##list_type
			
			pFirst As TLISTNODE##list_type Ptr = 0
			
			pLast As TLISTNODE##list_type Ptr = 0
			
			iSize As Long = 0
			
			Declare Destructor()
			
			Declare Sub Add(xValue As list_type)
			
			Declare Sub InsertItemIndex(iIndex As Long , xValue As list_type)
			
			Declare Sub InsertItem(pItem As TLISTNODE##list_type Ptr, xValue As list_type)
			
			Declare Sub DeleteItemIndex(iIndex As Long)
			
			Declare Sub DeleteItem(pItem As TLISTNODE##list_type Ptr)
			
			Declare Sub DeleteAll()
			
			Declare Sub SetValueIndex(iIndex As Long , xValue As list_type)
			
			Declare Sub SetValue(pItem As TLISTNODE##list_type Ptr , xValue As list_type)
			
			Declare Sub MoveItemIndex(iIndexFrom As Long , iIndexTo As Long)
			
			Declare Sub MoveItem(pItemFrom As TLISTNODE##list_type Ptr , pItemTo As TLISTNODE##list_type Ptr)
			
			Declare Function GetFirst() As TLISTNODE##list_type Ptr
			
			Declare Function GetLast() As TLISTNODE##list_type Ptr
			
			Declare Function GetNext(pItem As TLISTNODE##list_type Ptr) As TLISTNODE##list_type Ptr
			
			Declare Function GetPrev(pItem As TLISTNODE##list_type Ptr) As TLISTNODE##list_type Ptr
			
			Declare Function GetSize() As Long
			
			Declare Function GetValueIndex(iIndex As Long) As list_type
			
			Declare Function GetValue(pItem As TLISTNODE##list_type Ptr) As list_type
			
		End Type
		
		Destructor TLIST##list_type()
			
			DeleteAll()
			
		End Destructor
		
		Sub TLIST##list_type.Add(xValue As list_type)
			
			Dim As TLISTNODE##list_type Ptr pTemp = New TLISTNODE##list_type
			
			pTemp->xValue = xValue
			
			If pFirst = 0 Then
				
				pFirst = pTemp
				
				pLast = pTemp
				
			Else
				
				pLast->pNext = pTemp
				
				pTemp->pPrev = pLast
				
				pLast = pTemp
				
			Endif
			
			iSize+=1
			
		End Sub
		
		Sub TLIST##list_type.InsertItemIndex(iIndex As Long , xValue As list_type)
			
			If pFirst = 0 Orelse iIndex >= iSize Then
				
				This.Add(xValue)
				
				Exit Sub
				
			Endif
			
			If iIndex < 0 Then
				
				Exit Sub
				
			Endif
			
			Dim pFind As TLISTNODE##list_type Ptr = pFirst
			
			For i As Long = 0 To iIndex
				
				If i = iIndex Then
					
					Exit For
					
				Endif
				
				If pFind->pNext Then
					
					pFind = pFind->pNext	
					
				Endif
				
			Next
			
			InsertItem(pFind , xValue)
			
		End Sub
		
		Sub TLIST##list_type.InsertItem(pItem As TLISTNODE##list_type Ptr, xValue As list_type)
			
			If pFirst = 0 Orelse pItem = 0 Then
				
				This.Add(xValue)
				
				Exit Sub
				
			Endif
			
			Dim As TLISTNODE##list_type Ptr pTemp = New TLISTNODE##list_type
			
			pTemp->xValue = xValue
			
			If pItem = pFirst Then
				
				pTemp->pNext = pFirst
				
				pFirst = pTemp
				
			Else
				
				pTemp->pNext = pItem
				
				pTemp->pPrev = pItem->pPrev
				
				pItem->pPrev->pNext = pTemp
				
				pItem->pPrev = pTemp
				
			Endif
			
			iSize+=1	
			
		End Sub
		
		Sub TLIST##list_type.DeleteItemIndex(iIndex As Long)
			
			If pFirst = 0 Orelse iIndex >= iSize Orelse iIndex < 0 Orelse iSize = 0 Then
				
				Exit Sub
				
			Endif
			
			Dim pFind As TLISTNODE##list_type Ptr = pFirst
			
			For i As Long = 0 To iIndex
				
				If i = iIndex Then
					
					Exit For
					
				Endif
				
				If pFind->pNext Then
					
					pFind = pFind->pNext	
					
				Endif
				
			Next
			
			DeleteItem(pFind)
			
		End Sub
		
		Sub TLIST##list_type.DeleteItem(pItem As TLISTNODE##list_type Ptr)
			
			If pItem = 0 Orelse iSize = 0 Then
				
				Exit Sub
				
			Endif
			
			If pItem = pFirst Then
				
				pFirst = pFirst->pNext
				
				If pFirst Then
					
					pFirst->pPrev = 0
					
				Endif
				
				If pItem = pLast Then
					
					pLast = pFirst
					
				Endif
				
			Elseif pItem = pLast Then
				
				pLast = pLast->pPrev
				
				pLast->pNext = 0
				
			Else
				
				pItem->pPrev->pNext = pItem->pNext
				
				pItem->pNext->pPrev = pItem->pPrev
				
			Endif
			
			Delete pItem
			
			iSize -=1
			
			If iSize = 0 Then
				
				pFirst = 0
				
				pLast = 0
				
			Endif
			
		End Sub
		
		Sub TLIST##list_type.DeleteAll()
			
			Dim As TLISTNODE##list_type Ptr pDel , pTemp = pFirst
			
			While pTemp
				
				pDel = pTemp
				
				pTemp = pTemp->pNext
				
				Delete pDel
				
			Wend
			
			iSize = 0
			
			pFirst = 0
			
			pLast = 0
			
		End Sub
		
		Sub TLIST##list_type.SetValueIndex(iIndex As Long , xValue As list_type)
			
			If pFirst = 0 Orelse iIndex >= iSize Orelse iIndex < 0 Orelse iSize = 0 Then
				
				Exit Sub
				
			Endif
			
			Dim pFind As TLISTNODE##list_type Ptr = pFirst
			
			For i As Long = 0 To iIndex
				
				If i = iIndex Then
					
					Exit For
					
				Endif
				
				If pFind->pNext Then
					
					pFind = pFind->pNext	
					
				Endif
				
			Next
			
			pFind->xValue = xValue
			
		End Sub
		
		Sub TLIST##list_type.SetValue(pItem As TLISTNODE##list_type Ptr , xValue As list_type)
			
			If pItem = 0 Orelse pFirst = 0 Then
				
				Exit Sub
				
			Endif
			
			pItem->xValue = xValue
			
		End Sub
		
		Sub TLIST##list_type.MoveItemIndex(iIndexFrom As Long , iIndexTo As Long)
			
			If pFirst = 0 Orelse iIndexFrom >= iSize Orelse iIndexFrom < 0 _
			Orelse iIndexTo >=iSize Orelse iIndexTo < 0 Orelse iSize < 2 _
			Orelse iIndexFrom = iIndexTo Then
				
				Exit Sub
				
			Endif
			
			Dim pFindFrom As TLISTNODE##list_type Ptr = pFirst
			
			Dim pFindTo As TLISTNODE##list_type Ptr = pFirst
			
			Dim pResultFrom As TLISTNODE##list_type Ptr
			
			Dim pResultTo As TLISTNODE##list_type Ptr
			
			For i As Long = 0 To iSize - 1
				
				If i = iIndexFrom Then
					
					pResultFrom = pFindFrom
					
					If pResultTo Then
						
						Exit For
						
					Endif
					
				Endif
				
				If pFindFrom->pNext Then
					
					pFindFrom = pFindFrom->pNext	
					
				Endif
				
				If i = iIndexTo Then
					
					pResultTo = pFindTo
					
					If pResultFrom Then
						
						Exit For
						
					Endif
					
				Endif
				
				If pFindTo->pNext Then
					
					pFindTo = pFindTo->pNext
					
				Endif
				
			Next
			
			MoveItem(pResultFrom , pResultTo)
			
		End Sub
		
		Sub TLIST##list_type.MoveItem(pItemFrom As TLISTNODE##list_type Ptr , pItemTo As TLISTNODE##list_type Ptr)
			
			If pItemFrom = 0 Orelse pItemTo = 0 Orelse pItemFrom = pItemTo Orelse pFirst = 0 Orelse iSize < 2 Then
				
				Exit Sub
				
			Endif
			
			If pItemFrom = pFirst Andalso pItemTo = pLast Then
				
				pFirst = pFirst->pNext
				
				pFirst->pPrev = 0
				
				pItemFrom->pPrev = pLast
				
				pLast->pNext = pItemFrom
				
				pItemFrom->pNext = 0
				
				pLast = pItemFrom
				
			Elseif pItemFrom = pLast Andalso pItemTo = pFirst Then
				
				pLast = pLast->pPrev
				
				pLast->pNext = 0
				
				pItemFrom->pNext = pFirst
				
				pFirst->pPrev = pItemFrom
				
				pItemFrom->pPrev = 0
				
				pFirst = pItemFrom
				
			Elseif pItemFrom = pLast Then
				
				pLast = pLast->pPrev
				
				pItemFrom->pPrev = pItemTo->pPrev
				
				pItemTo->pPrev->pNext = pItemFrom
				
				pItemFrom->pNext = pItemTo
				
				pItemTo->pPrev = pItemFrom
				
				pLast->pNext = 0
				
			Elseif pItemFrom = pFirst Then
				
				pFirst = pFirst->pNext
				
				pItemFrom->pNext = pItemTo->pNext
				
				pItemTo->pNext->pPrev = pItemFrom
				
				pItemFrom->pPrev = pItemTo
				
				pItemTo->pNext = pItemFrom
				
				pFirst->pPrev = 0
				
			Elseif pItemTo = pFirst Then
				
				pItemFrom->pNext->pPrev = pItemFrom->pPrev
				
				pItemFrom->pPrev->pNext = pItemFrom->pNext
				
				pItemFrom->pNext = pItemTo
				
				pItemTo->pPrev = pItemFrom
				
				pItemFrom->pPrev = 0
				
				pFirst = pItemFrom
				
			Elseif pItemTo = pLast Then
				
				pItemFrom->pNext->pPrev = pItemFrom->pPrev
				
				pItemFrom->pPrev->pNext = pItemFrom->pNext
				
				pItemFrom->pPrev = pItemTo
				
				pItemTo->pNext = pItemFrom
				
				pItemFrom->pNext = 0
				
				pLast = pItemFrom
				
			Else
				
				pItemFrom->pPrev->pNext = pItemFrom->pNext
				
				pItemFrom->pNext->pPrev = pItemFrom->pPrev
				
				If pItemFrom<pItemTo Then
					
					pItemFrom->pNext = pItemTo->pNext
					
					pItemTo->pNext->pPrev = pItemFrom
					
					pItemTo->pNext = pItemFrom
					
					pItemFrom->pPrev = pItemTo
					
				Else
					
					pItemTo->pPrev->pNext = pItemFrom
					
					pItemFrom->pPrev = pItemTo->pPrev
					
					pItemFrom->pNext = pItemTo
					
					pItemTo->pPrev = pItemFrom
					
				Endif
				
			Endif
			
		End Sub
		
		Function TLIST##list_type.GetFirst() As TLISTNODE##list_type Ptr
			
			Return pFirst
			
		End Function
		
		Function TLIST##list_type.GetLast() As TLISTNODE##list_type Ptr
			
			Return pLast
			
		End Function
		
		Function TLIST##list_type.GetNext(pItem As TLISTNODE##list_type Ptr) As TLISTNODE##list_type Ptr
			
			Return pItem->pNext
			
		End Function
		
		Function TLIST##list_type.GetPrev(pItem As TLISTNODE##list_type Ptr) As TLISTNODE##list_type Ptr
			
			Return pItem->pPrev
			
		End Function
		
		Function TLIST##list_type.GetSize() As Long
			
			Return iSize
			
		End Function
		
		Function TLIST##list_type.GetValueIndex(iIndex As Long) As list_type
			
			If iIndex < 0 Orelse iIndex >= iSize Orelse pFirst = 0 Then
				
				Exit Function
				
			Endif
			
			Dim As TLISTNODE##list_type Ptr pTemp = pFirst
			
			For i As Long = 0 To iIndex
				
				If iIndex = i Then
					
					Exit For
					
				Endif
				
				If pTemp->pNext Then
					
					pTemp = pTemp->pNext
					
				Endif
				
			Next
			
			Return pTemp->xValue
			
		End Function
		
		Function TLIST##list_type.GetValue(pItem As TLISTNODE##list_type Ptr) As list_type
			
			Return pItem->xValue
			
		End Function
		
	#endif
	
#endmacro
