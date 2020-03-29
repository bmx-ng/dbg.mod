SuperStrict

Import "debugger.glue.c"

NoDebug

Import BRL.Socket
Import pub.stdc

?win32
Include "deref_win32.bmx"
?linux
Include "deref_linux.bmx"
?macos
Include "deref_macos.bmx"
?


Private

Const derefFailure:String = "{?}"
Const derefSymbol:String = "->"

?Win32
Extern "Win32"
Const SW_SHOW:Int=5
Const SW_RESTORE:Int=9
Function IsIconic:Int( hwnd:Byte Ptr )="WINBOOL IsIconic( HWND )!"
Function GetForegroundWindow:Byte Ptr()="HWND GetForegroundWindow()!"
Function SetForegroundWindow:Int( hwnd:Byte Ptr )="WINBOOL SetForegroundWindow( HWND )!"
Function ShowWindow:Int( hwnd:Byte Ptr,cmdShow:Int )="WINBOOL ShowWindow( HWND ,int )!"
Function GetCurrentThreadId:Int()="DWORD GetCurrentThreadId()!"
End Extern
?

?MacOS
Extern
Function CGDisplayIsCaptured:Int( displayId:Int )
End Extern
?

Extern
	Function bbIsMainThread:Int()="bbIsMainThread"
	Function bbGCValidate:Int( mem:Byte Ptr ) = "int bbGCValidate( void * )!"

	Function DebugScopeName:String( scope:Int Ptr )="bmx_debugger_DebugScopeName"
	Function bmx_debugger_DebugScopeKind:UInt( scope:Int Ptr )
	Function bmx_debugger_DebugScopeDecl:Byte Ptr( scope:Int Ptr )

	Function DebugDeclName:String( decl:Int Ptr )="bmx_debugger_DebugDeclName"
	Function bmx_debugger_DebugDeclType:String( decl:Int Ptr )
	Function bmx_debugger_DebugDeclKind:UInt( decl:Int Ptr )
	Function bmx_debugger_DebugDeclNext:Byte Ptr( decl:Int Ptr )
	Function bmx_debugger_DebugDecl_VarAddress:Byte Ptr( decl:Int Ptr )
	Function bmx_debugger_DebugDecl_ConstValue:String( decl:Int Ptr )
	Function bmx_debugger_DebugDecl_FieldOffset:Byte Ptr(decl:Int Ptr, inst:Byte Ptr)
	Function bmx_debugger_DebugDecl_StringFromAddress:String( addr:Byte Ptr )
	Function bmx_debugger_DebugDeclTypeChar:Int( decl:Int Ptr, index:Int )
	Function bmx_debugger_DebugDecl_ArraySize:Int( decl:Byte Ptr )
	Function bmx_debugger_DebugDecl_ArrayDecl:Byte Ptr(inst:Byte Ptr)
	Function bmx_debugger_DebugDecl_ArrayDeclIndexedPart(decl:Byte Ptr, inst:Byte Ptr, index:Int)
	Function bmx_debugger_DebugDecl_ArrayDeclFree(decl:Byte Ptr)

	Function bmx_debugger_DebugDecl_clas:Byte Ptr( inst:Byte Ptr )
	Function bmx_debugger_DebugDecl_isStringClass:Int( clas:Byte Ptr )
	Function bmx_debugger_DebugDecl_isArrayClass:Int( clas:Byte Ptr )
	Function bmx_debugger_DebugDecl_isBaseObject:Int( clas:Byte Ptr )
	
	Function bmx_debugger_DebugClassSuper:Byte Ptr(clas:Byte Ptr)
	Function bmx_debugger_DebugClassScope:Byte Ptr(clas:Byte Ptr)
	
	Function DebugStmFile:String( stm:Int Ptr )="bmx_debugger_DebugStmFile"
	Function DebugStmLine:Int( stm:Int Ptr )="bmx_debugger_DebugStmLine"
	Function DebugStmChar:Int( stm:Int Ptr )="bmx_debugger_DebugStmChar"
	Function DebugBreakOnLine:Int( stm:Int Ptr )="bmx_debugger_DebugBreakOnLine"

	Function bmx_debugger_ref_bbNullObject:Byte Ptr()
	Function bmx_debugger_ref_bbEmptyArray:Byte Ptr()
	Function bmx_debugger_ref_bbEmptyString:Byte Ptr()
	Function bmx_debugger_ref_brl_blitz_NullFunctionError:Byte Ptr()
	
	Function bbObjectStructInfo:Byte Ptr(name:Byte Ptr)="BBDebugScope * bbObjectStructInfo( char * )!"
	Function bmx_debugger_DebugEnumDeclValue:String(decl:Byte Ptr, val:Byte Ptr)
	
	Function bmx_debugger_AddBreakpoint:Int(filename:String, line:Int)
	Function bmx_debugger_RemoveBreakpoint:Int(filename:String, line:Int)
	
	Function bmx_snprintf:Int(buf:Byte Ptr, size:Size_T, num:Size_T)
End Extern

?Not ptr64
Function ToHex$( val:Int )
	Local buf:Short[8]
	For Local k:Int=7 To 0 Step -1
		Local n:Int=(val&15)+Asc("0")
		If n>Asc("9") n=n+(Asc("A")-Asc("9")-1)
		buf[k]=n
		val:Shr 4
	Next
	Return String.FromShorts( buf,8 ).ToLower()
End Function
?ptr64
Function ToHex$( val:Long )
	Local buf:Short[16]
	For Local k:Int=15 To 0 Step -1
		Local n:Int=(val&15)+Asc("0")
		If n>Asc("9") n=n+(Asc("A")-Asc("9")-1)
		buf[k]=n
		val:Shr 4
	Next
	Return String.FromShorts( buf,16 ).ToLower()
End Function
?

Function IsAlpha:Int( ch:Int )
	Return (ch>=Asc("a") And ch<=Asc("z")) Or (ch>=Asc("A") And ch<=Asc("Z"))
End Function

Function IsNumeric:Int( ch:Int )
	Return ch>=Asc("0") And ch<=Asc("9")
End Function

Function IsAlphaNumeric:Int( ch:Int )
	Return IsAlpha(ch) Or IsNumeric(ch)
End Function

Function IsUnderscore:Int( ch:Int )
	Return ch=Asc("_")
End Function

Function Ident$( tag$ Var )
	If Not tag Return ""
	If Not IsAlpha( tag[0] ) And Not IsUnderscore( tag[0] ) Return ""
	Local i:Int=1
	While i<tag.length And (IsAlphaNumeric(tag[i]) Or IsUnderscore(tag[i]))
		i:+1
	Wend
	Local id$=tag[..i]
	tag=tag[i..]
	Return id
End Function

Function TypeName$( tag$ Var )
	
	Local t$=tag[..1]
	tag=tag[1..]

	Select t
	Case "b"
		Return "Byte"
	Case "s"
		Return "Short"
	Case "i"
		Return "Int"
	Case "u"
		Return "UInt"
	Case "l"
		Return "Long"
	Case "y"
		Return "ULong"
	Case "j"
		Return "Int128"
	Case "f"
		Return "Float"
	Case "d"
		Return "Double"
	Case "h"
		Return "Float64"
	Case "k"
		Return "Float128"
	Case "m"
		Return "Double128"
	Case "$"
		Return "String"
	Case "z"
		Return "CString"
	Case "w"
		Return "WString"
	Case "t"
		Return "Size_T"
	Case "W"
		Return "WParam"
	Case "X"
		Return "LParam"
	Case ":","?","#","@","/"
		Local id$=Ident( tag )
		While tag And tag[0]=Asc(".")
			tag=tag[1..]
			id=Ident( tag )
		Wend
		If Not id DebugError "Invalid object typetag"
		Return id
	Case "*"
		Return TypeName( tag )+" Ptr"
	Case "["
		Local length:Int
		While tag[..1]=","
			tag=tag[1..]
			t:+","
		Wend
		While IsNumeric(tag[0])
			length = length * 10 + Int(tag[..1])
			tag=tag[1..]
		Wend
		If tag[..1]<>"]" DebugError "Invalid array typetag"
		tag=tag[1..]
		If length Then
			Return TypeName( tag )+t+length+"]"
		Else
			Return TypeName( tag )+t+"]"
		End If
	Case "("
		If tag[..1]<>")"
			t:+TypeName( tag )
			While tag[..1]=","
				tag=tag[1..]
				t:+","+TypeName( tag )
			Wend
			If tag[..1]<>")" DebugError "Invalid function typetag"
		EndIf
		tag=tag[1..]
		Return TypeName( tag )+t+")"
	End Select

	If Not tag.length Return ""
	DebugError "Invalid debug typetag:"+t

End Function

'int offsets into 12 byte DebugStm struct
'Const DEBUGSTM_FILE=0
'Const DEBUGSTM_LINE=1
'Const DEBUGSTM_CHAR=2

'int offsets into 16 byte DebugDecl struct
'Const DEBUGDECL_KIND=0
'Const DEBUGDECL_NAME=1
'Const DEBUGDECL_TYPE=2
'Const DEBUGDECL_ADDR:Int=3

'DEBUGDECL_KIND values
Const DEBUGDECLKIND_END:Int=0
Const DEBUGDECLKIND_CONST:Int=1
Const DEBUGDECLKIND_LOCAL:Int=2
Const DEBUGDECLKIND_FIELD:Int=3
Const DEBUGDECLKIND_GLOBAL:Int=4
Const DEBUGDECLKIND_VARPARAM:Int=5
Const DEBUGDECLKIND_TYPEMETHOD:Int=6
Const DEBUGDECLKIND_TYPEFUNCTION:Int=7

'int offsets into 12+n_decls*4 byte DebugScope struct
'Const DEBUGSCOPE_KIND=0
'Const DEBUGSCOPE_NAME=1
'Const DEBUGSCOPE_DECLS=2

'DEBUGSCOPE_KIND values
Const DEBUGSCOPEKIND_FUNCTION:Int=1
Const DEBUGSCOPEKIND_TYPE:Int=2
Const DEBUGSCOPEKIND_LOCAL:Int=3
Const DEBUGSCOPEKIND_INTERFACE:Int=4
Const DEBUGSCOPEKIND_STRUCT:Int=5

Function DebugError( t$ )
	WriteStderr "Debugger Error:"+t+"~n"
	End
End Function

Function DebugDeclKind$( decl:Int Ptr )
	Select bmx_debugger_DebugDeclKind(decl)
	Case DEBUGDECLKIND_CONST Return "Const"
	Case DEBUGDECLKIND_LOCAL Return "Local"
	Case DEBUGDECLKIND_FIELD Return "Field"
	Case DEBUGDECLKIND_GLOBAL Return "Global"
	Case DEBUGDECLKIND_VARPARAM Return "Local"
	End Select
	DebugError "Invalid decl kind"
End Function

Function DebugDeclType$( decl:Int Ptr )
	Local t$=bmx_debugger_DebugDeclType(decl)
	Local ty$=TypeName( t )
	Return ty
End Function

Function DebugDeclSize:Int( decl:Int Ptr )

	Local tag:Int=bmx_debugger_DebugDeclTypeChar(decl, 0)

	Select tag
	Case Asc("b") Return 1
	Case Asc("s") Return 2
	Case Asc("i") Return 4
	Case Asc("u") Return 4
	Case Asc("f") Return 4
	Case Asc("l") Return 8
	Case Asc("y") Return 8
	Case Asc("d") Return 8
	Case Asc("h") Return 8
	Case Asc("j") Return 16
	Case Asc("k") Return 16
	Case Asc("m") Return 16
	' size_t (t) fall-through to ptr64 size below
	End Select

?Not ptr64
	Return 4
?ptr64
	Return 8
?

End Function

Function DebugEscapeString$( s$ )
	If s.length>4096 s=s[..4096]
	s=s.Replace( "~~","~~~~")
	s=s.Replace( "~0","~~0" )
	s=s.Replace( "~t","~~t" )
	s=s.Replace( "~n","~~n" )
	s=s.Replace( "~r","~~r" )
	s=s.Replace( "~q","~~q" )
	Return "~q"+s+"~q"
End Function

Function DebugDeclValue$( decl:Int Ptr,inst:Byte Ptr )

	If bmx_debugger_DebugDeclKind(decl)=DEBUGDECLKIND_CONST
		Return DebugEscapeString(bmx_debugger_DebugDecl_ConstValue(decl))
	End If

	Local p:Byte Ptr
	Select bmx_debugger_DebugDeclKind(decl)
	Case DEBUGDECLKIND_GLOBAL
		p=bmx_debugger_DebugDecl_VarAddress(decl)
	Case DEBUGDECLKIND_LOCAL
		p=bmx_debugger_DebugDecl_VarAddress(decl)
	Case DEBUGDECLKIND_FIELD
		p=bmx_debugger_DebugDecl_FieldOffset(decl, inst)
	Case DEBUGDECLKIND_VARPARAM
		p=bmx_debugger_DebugDecl_VarAddress(decl)
?Not ptr64
		p=Byte Ptr ( (Int Ptr p)[0] )
?ptr64
		p=Byte Ptr ( (Long Ptr p)[0] )
?
	Default
		DebugError "Invalid decl kind"
	End Select
	
	Local tag:Int=bmx_debugger_DebugDeclTypeChar(decl, 0)
	
	Select tag
	Case Asc("b")
		Return String.FromInt( (Byte Ptr p)[0] )
	Case Asc("s")
		Return String.FromInt( (Short Ptr p)[0] )
	Case Asc("i")
		Return String.FromInt( (Int Ptr p)[0] )
	Case Asc("u")
		Return String.FromUInt( (UInt Ptr p)[0] )
	Case Asc("l")
		Return String.FromLong( (Long Ptr p)[0] )
	Case Asc("y")
		Return String.FromULong( (ULong Ptr p)[0] )
	Case Asc("f")
		Return String.FromFloat( (Float Ptr p)[0] )
	Case Asc("d")
		Return String.FromDouble( (Double Ptr p)[0] )
	Case Asc("t")
		Return String.FromSizet( (Size_T Ptr p)[0] )
?win32
	Case Asc("W")
		Return String.FromWParam( (WParam Ptr p)[0] )
	Case Asc("X")
		Return String.FromLParam( (LParam Ptr p)[0] )
?
	Case Asc("$")
		p=(Byte Ptr Ptr p)[0]
		Return DebugEscapeString( bmx_debugger_DebugDecl_StringFromAddress(p) )
	Case Asc("z")
		p=(Byte Ptr Ptr p)[0]
		If Not p Return "Null"
		Local s$=String.FromCString( p )
		Return DebugEscapeString( s )
	Case Asc("w")
		p=(Byte Ptr Ptr p)[0]
		If Not p Return "Null"
		Local s$=String.FromWString( Short Ptr p )
		Return DebugEscapeString( s )
	Case Asc("*"),Asc("?"),Asc("#")
		Local deref:String
		If tag = Asc("*") Then deref = DebugDerefPointer(decl, p)
?Not ptr64
		Return "$" + ToHex( (Int Ptr p)[0] ) + deref
?ptr64
		Return "$" + ToHex( (Long Ptr p)[0] ) + deref
?
	Case Asc("(")
		p=(Byte Ptr Ptr p)[0]
		If p=bmx_debugger_ref_brl_blitz_NullFunctionError() Return "Null"
	Case Asc(":")
		p=(Byte Ptr Ptr p)[0]
		If p=bmx_debugger_ref_bbNullObject() Return "Null"
		If p=bmx_debugger_ref_bbEmptyArray() Return "Null[]"
		If p=bmx_debugger_ref_bbEmptyString() Return "Null$"
	Case Asc("[")
		p=(Byte Ptr Ptr p)[0]
		If Not p Return "Null"
		If IsNumeric(bmx_debugger_DebugDeclTypeChar(decl, 1)) Then
			Local index:Int = 1
			Local length:Int
			While IsNumeric(bmx_debugger_DebugDeclTypeChar(decl, index))
				length = length * 10 + Int(Chr(bmx_debugger_DebugDeclTypeChar(decl, index)))
				index :+ 1
			Wend
?Not ptr64
			Return "$"+ToHex( Int p ) + "^" + length
?ptr64
			Return "$"+ToHex( Long p ) + "^" + length
?		
		End If
		If Not bmx_debugger_DebugDecl_ArraySize(p) Return "Null"
	Case Asc("@")
?Not ptr64
		Return "$"+ToHex( Int p ) + "@" + bmx_debugger_DebugDeclType(decl)[1..]
?ptr64
		Return "$"+ToHex( Long p ) + "@" + bmx_debugger_DebugDeclType(decl)[1..]
?
	Case Asc("h")
		Return Float Ptr (Varptr p)[0] + "," + Float Ptr (Varptr p)[1]
	Case Asc("j")
		Return Int Ptr (Varptr p)[0] + "," + Int Ptr (Varptr p)[1] + "," + Int Ptr (Varptr p)[2] + "," + Int Ptr (Varptr p)[3]
	Case Asc("k")
		Return Float Ptr (Varptr p)[0] + "," + Float Ptr (Varptr p)[1] + "," + Float Ptr (Varptr p)[2] + "," + Float Ptr (Varptr p)[3]
	Case Asc("m")
		Return Double Ptr(Varptr p)[0] + "," + Double Ptr (Varptr p)[1]
	Case Asc("/")
		Return bmx_debugger_DebugEnumDeclValue(decl, p)
	Default
		DebugError "Invalid decl typetag:"+Chr(tag)
	End Select
	
?Not ptr64
	Return "$"+ToHex( Int p )
?ptr64
	Return "$"+ToHex( Long p )
?
End Function

Function DebugScopeKind$( scope:Int Ptr )
	Select bmx_debugger_DebugScopeKind(scope)
	Case DEBUGSCOPEKIND_FUNCTION Return "Function"
	Case DEBUGSCOPEKIND_TYPE Return "Type"
	Case DEBUGSCOPEKIND_LOCAL Return "Local"
	Case DEBUGSCOPEKIND_INTERFACE Return "Interface"
	Case DEBUGSCOPEKIND_STRUCT Return "Struct"
	End Select
	DebugError "Invalid scope kind"
End Function

Function DebugDerefPointer:String(decl:Int Ptr, pointer:Byte Ptr)
	Const derefFailure:String = "{?}"
	Const derefSymbol:String = "->"
	
	Local declType:String = DebugDeclType(decl)
	Local dataType:String = declType
	Local ptrDepth:Int = 0
	While dataType.EndsWith(" Ptr")
		dataType = dataType[..dataType.length - " Ptr".length]
		ptrDepth :+ 1
	Wend
	
	Local result:String = ""
	
	Local dataSize:Size_T
	Select dataType
 		Case "Byte"      dataSize = SizeOf(Byte Null)
		Case "Short"     dataSize = SizeOf(Short Null)
		Case "Int"       dataSize = SizeOf(Int Null)
		Case "UInt"      dataSize = SizeOf(UInt Null)
		Case "Long"      dataSize = SizeOf(Long Null)
		Case "ULong"     dataSize = SizeOf(ULong Null)
		Case "Size_T"    dataSize = SizeOf(Size_T Null)
		Case "Float"     dataSize = SizeOf(Float Null)
		Case "Double"    dataSize = SizeOf(Double Null)
	? Ptr64
		Case "Float64"   dataSize = SizeOf(Float64 Null)
		Case "Float128"  dataSize = SizeOf(Float128 Null)
		Case "Double128" dataSize = SizeOf(Double128 Null)
		Case "Int128"    dataSize = SizeOf(Int128 Null)
	? Win32	
		Case "WParam"    dataSize = SizeOf(WParam Null)
		Case "LParam"    dataSize = SizeOf(LParam Null)
	?
		Default          dataSize = 0 ' cannot dereference this
	EndSelect

	Local buffer:Byte Ptr = MemAlloc(Size_T(Max(dataSize, SizeOf(Byte Ptr Null))))
	(Byte Ptr Ptr buffer)[0] = Null
	
	Local res:Int
	?win32
	result = DebugDerefPointerWin32(dataSize, ptrDepth, pointer, buffer, res)
	?linux
	result = DebugDerefPointerLinux(dataSize, ptrDepth, pointer, buffer, res)
	?macos
	result = DebugDerefPointerMacos(dataSize, ptrDepth, pointer, buffer, res)
	?

	If Not res Then
		Return result
	End If

	Local value:String
	Select dataType
 		Case "Byte"      value = String((Byte   Ptr buffer)[0])
		Case "Short"     value = String((Short  Ptr buffer)[0])
		Case "Int"       value = String((Int    Ptr buffer)[0])
		Case "UInt"      value = String((UInt   Ptr buffer)[0])
		Case "Long"      value = String((Long   Ptr buffer)[0])
		Case "ULong"     value = String((ULong  Ptr buffer)[0])
		Case "Size_T"    value = String((Size_T Ptr buffer)[0])
		Case "Float"     value = String((Float  Ptr buffer)[0])
		Case "Double"    value = String((Double Ptr buffer)[0])
	? Ptr64
		Case "Float64"   value = String((Float  Ptr buffer)[0]) + "," + ..
		                         String((Float  Ptr buffer)[1])
		Case "Float128"  value = String((Float  Ptr buffer)[0]) + "," + ..
		                         String((Float  Ptr buffer)[1]) + "," + ..
		                         String((Float  Ptr buffer)[2]) + "," + ..
		                         String((Float  Ptr buffer)[3])
		Case "Double128" value = String((Double Ptr buffer)[0]) + "," + ..
		                         String((Double Ptr buffer)[1])
		Case "Int128"    value = String((Int    Ptr buffer)[0]) + "," + ..
		                         String((Int    Ptr buffer)[1]) + "," + ..
		                         String((Int    Ptr buffer)[2]) + "," + ..
		                         String((Int    Ptr buffer)[3])
	? Win32	
		Case "WParam"    value = String((WParam Ptr buffer)[0])
		Case "LParam"    value = String((LParam Ptr buffer)[0])
	?
		Default
			MemFree buffer
			result :+ derefSymbol + derefFailure
			Return result
	EndSelect
	MemFree buffer
	result :+ derefSymbol + "{" + value + "}"
	?
	
	Return result
EndFunction

Extern
Global bbOnDebugStop()="void bbOnDebugStop()!"
Global bbOnDebugLog( message$ )="void bbOnDebugLog( BBString * )!"
Global bbOnDebugEnterStm( stm:Int Ptr )="void bbOnDebugEnterStm( BBDebugStm * )!"
Global bbOnDebugEnterScope( scope:Int Ptr)="void bbOnDebugEnterScope( BBDebugScope * )!"
Global bbOnDebugLeaveScope()="void bbOnDebugLeaveScope()!"
Global bbOnDebugPushExState()="void bbOnDebugPushExState()!"
Global bbOnDebugPopExState()="void bbOnDebugPopExState()!"
Global bbOnDebugUnhandledEx( ex:Object )="void bbOnDebugUnhandledEx( BBObject * )!"
End Extern

bbOnDebugStop=OnDebugStop
bbOnDebugLog=OnDebugLog
bbOnDebugEnterStm=OnDebugEnterStm
bbOnDebugEnterScope=OnDebugEnterScope
bbOnDebugLeaveScope=OnDebugLeaveScope
bbOnDebugPushExState=OnDebugPushExState
bbOnDebugPopExState=OnDebugPopExState
bbOnDebugUnhandledEx=OnDebugUnhandledEx

?Win32
Global _ideHwnd:Byte Ptr=GetForegroundWindow();
Global _appHwnd:Byte Ptr
?

'********** Debugger code here **********

Const MODE_RUN:Int=0
Const MODE_STEP:Int=1
Const MODE_STEPIN:Int=2
Const MODE_STEPOUT:Int=3
Const MODE_BREAK:Int=4

Type TScope
	Field scope:Int Ptr,inst:Byte Ptr,stm:Int Ptr
End Type

Type TExState
	Field scopeStackTop:Int
End Type

Type TDbgState
	Field Mode:Int,debugLevel:Int,funcLevel:Int
	Field currentScope:TScope=New TScope
	Field scopeStack:TScope[],scopeStackTop:Int
	Field exStateStack:TExState[],exStateStackTop:Int
End Type

?Threaded
Extern
Function bbThreadAllocData:Int()
Function bbThreadSetData( index:Int,data:Object )
Function bbThreadGetData:TDbgState( index:Int )="BBObject* bbThreadGetData(int )!"
End Extern
?

Global _bpCount:Int
Global _debugPort:Int = 31666
Global _debugWait:Int = True
Global _debugSetup:Int = True

Global _debugData:TDebugData = New TDebugData

_initDebugger()

Const STATEMENT_TIMER:Int = 50
Global _debugSocket:TSocket, _serverSocket:TSocket
Global _debugConnected:Int

_debugSocket = TSocket.CreateTCP()
If Not _debugSocket Then
	_debugSocket = TSocket.CreateTCP()
End If

If _debugSocket Then
	OnDebugLog("Listening on " + _debugPort)

	_debugSocket.Bind(_debugPort)
	_debugSocket.Listen(1)
	_debugSetup = False
End If

Function _initDebugger()

	' port to accept debugger connections against
	Local env:String = getenv_("BMX_DEBUG_PORT")
	If env Then
		Local port:Int = env.ToInt()
		If port Then
			_debugPort = port
		End If
	End If
	' wait for debugger to connect before running?
	' set to non zero for Yes
	env = getenv_("BMX_DEBUG_WAIT")
	If env Then
		Local wait:Int = env.ToInt()
		If wait Then
			_debugWait = True
		End If
	End If

End Function

Function GetDbgState:TDbgState()
	Global dbgStateMain:TDbgState=New TDbgState
?Threaded
	If bbIsMainThread() Return dbgStateMain
	Global dbgStateId:Int=bbThreadAllocData()
	Local dbgState:TDbgState=bbThreadGetData( dbgStateId )
	If Not dbgState
		dbgState = New TDbgState
		bbThreadSetData( dbgStateId,dbgState )
	End If
	Return dbgState
?Not Threaded
	Return dbgStateMain
?
End Function

Global _WAITING_FOR_INPUT:Int

Function ReadDebug$(skipNotAvail:Int = False)

	If _debugConnected Then
		If Not _serverSocket.Connected() Then
			_debugConnected = False
			Return Null
		End If
	
		If _WAITING_FOR_INPUT Then
			Return ""
		End If
	
		Const BUFLEN:Int = 64
		Local buf:Byte Ptr = StackAlloc(BUFLEN)
		Local p:Int, n:Byte
		Local s:String

		Local count:Int = _serverSocket.ReadAvail()

		If skipNotAvail And Not count Then
			Return ""
		End If

		_WAITING_FOR_INPUT = True
		
		Repeat
			_serverSocket.Recv(Varptr n, 1)

			If n = 10 And s.length > 0 Then
				Exit
			End If

			buf[p]=n
			p:+1
			If p<>BUFLEN And p < count Then
				Continue
			End If
			s:+ String.FromBytes(buf,p)
			p=0
		
			count = _serverSocket.ReadAvail()

			If n = 13 Then
				Continue
			End If
			
			Delay 50
		Forever
		
		If p > 0 Then
			s :+ String.FromBytes(buf,p)
		End If
		
		_WAITING_FOR_INPUT = False

		Return s
		'Return ReadStdin()
	End If
End Function

Function WriteDebug( t$, complete:Int = False )

	If _debugConnected Then
		If Not _serverSocket.Connected() Then
			_debugConnected = False
			Return
		End If

		_debugData.Append(t)

		If complete Then

			Local n:Size_T = bmx_snprintf(_debugData.data, 8, Size_T(_debugData.length - 8))
			_debugData.data[_debugData.length] = 0
			
			_serverSocket.Send(_debugData.data, Size_T(_debugData.length))
			
			_debugData.Reset()
		End If
	End If
End Function

Function DumpScope( scope:Byte Ptr, inst:Byte Ptr )
	Local decl:Byte Ptr=bmx_debugger_DebugScopeDecl(scope)
	Local kind$=DebugScopeKind( scope )
	Local name$=DebugScopeName( scope )
	
	If Not name name="<local>"
	
	WriteDebug kind+" "+name+"~n"
	While bmx_debugger_DebugDeclKind(decl)<>DEBUGDECLKIND_END
		Select bmx_debugger_DebugDeclKind(decl)
		Case DEBUGDECLKIND_TYPEMETHOD,DEBUGDECLKIND_TYPEFUNCTION
			decl = bmx_debugger_DebugDeclNext(decl)
			Continue
		End Select
		Local kind$=DebugDeclKind( decl )
		Local name$=DebugDeclname( decl )
		Local tipe$=DebugDeclType( decl )
		Local value$=DebugDeclValue( decl, inst )
		
		If tipe.find("[") >= 0 Then
?Not ptr64
			Local pointer:Int = Int( value )
?ptr64
			Local pointer:Long = Long( value )
?
?Not ptr64
			Local dinst:Int Ptr=Int Ptr pointer
?ptr64
			Local dinst:Long Ptr=Long Ptr pointer
?			

			Local length:Int=bmx_debugger_DebugDecl_ArraySize(dinst)
			value :+ ":" + length

		End If
		
		WriteDebug kind+" "+name+":"+tipe+"="+value+"~n"

		decl = bmx_debugger_DebugDeclNext(decl)
	Wend
End Function

Function DumpClassScope( clas:Int Ptr,inst:Byte Ptr )

	Local supa:Int Ptr = bmx_debugger_DebugClassSuper(clas)
	
	If Not supa Return
	
	DumpClassScope supa,inst
	
	DumpScope bmx_debugger_DebugClassScope(clas),inst

End Function

Function DumpObject( inst:Byte Ptr,index:Int, count:Int = 100 )

	Local clas:Byte Ptr=bmx_debugger_DebugDecl_clas(inst)
	
	If bmx_debugger_DebugDecl_isStringClass(clas)

		WriteDebug DebugEscapeString(bmx_debugger_DebugDecl_StringFromAddress(inst))+"~n"

		Return

	Else If bmx_debugger_DebugDecl_isArrayClass(clas)

		Local length:Int=bmx_debugger_DebugDecl_ArraySize(inst)

		If Not length Return
		
		Local decl:Byte Ptr = bmx_debugger_DebugDecl_ArrayDecl(inst)

		For Local i:Int=1 To count

			If index>=length Exit
			
			bmx_debugger_DebugDecl_ArrayDeclIndexedPart(decl, inst, index)
		
			Local value$=DebugDeclValue( decl,inst )
			
			WriteDebug "["+index+"]="+value+"~n"
			
			index:+1
			
		Next

		bmx_debugger_DebugDecl_ArrayDeclFree(decl)
		
'		If index<length
'
'			WriteDebug "...=$"+ToHex(Int inst)+":"+index+"~n"
'	
'		EndIf
		
	Else
			
		If bmx_debugger_DebugDecl_isBaseObject(clas) Then
			WriteDebug "Object~n"
			Return
		EndIf
	
		DumpClassScope clas,inst
	
	EndIf
	
End Function

Function DumpStruct( inst:Byte Ptr,index:Int,structName:String )
	Local s:Byte Ptr = structName.ToCString()
	Local scope:Byte Ptr = bbObjectStructInfo(s)
	If scope Then
		DumpScope scope,inst
	End If
	MemFree s
End Function

Function DumpScopeStack()
	Local dbgState:TDbgState = GetDbgState()
	For Local i:Int=Max(dbgState.scopeStackTop-100,0) Until dbgState.scopeStackTop
		Local t:TScope=dbgState.scopeStack[i]
		Local stm:Int Ptr=t.stm
		If Not stm Continue
		WriteDebug "@"+DebugStmFile(stm)+"<"+DebugStmLine(stm)+","+DebugStmChar(stm)+">~n"
		DumpScope t.scope, t.inst
	Next
End Function

Function UpdateDebug( msg$ )
	Global indebug:Int
	If indebug Return
	indebug=True
	
	Local dbgState:TDbgState = GetDbgState()
	
?Win32
	_appHwnd=GetForegroundWindow();
	'SetForegroundWindow( _ideHwnd );
?
?MacOs
	'fullscreen debug too hard in MacOS!
	If CGDisplayIsCaptured( 0 )
		WriteStdout msg
		End
	EndIf
?

	' we really need a connection...
	Local timeout:Int = 5000
	While Not _checkConnection()
		timeout:-1
		If Not timeout Then
			End
		End If
	Wend

	WriteDebug msg
	Repeat
		WriteDebug "~n", True
		Local line$=ReadDebug()

		Select line[..1].ToLower()
		Case "r"
			dbgState.Mode=MODE_RUN
			Exit
		Case "s"
			dbgState.Mode=MODE_STEP
			dbgState.debugLevel=dbgState.funcLevel
			Exit
		Case "e"
			dbgState.Mode=MODE_STEPIN
			Exit
		Case "l"
			dbgState.Mode=MODE_STEPOUT
			dbgState.debugLevel=dbgState.scopeStackTop-1
			Exit
		Case "t"
			WriteDebug "StackTrace{~n"
			DumpScopeStack
			WriteDebug "}~n"
		Case "d"
			Local t$=line[1..].Trim()
			Local index:Int
			Local count:Int = 100
			Local i:Int=t.Find(":")
			If i<>-1
				Local Range:String = t[i+1..]
				t=t[..i]
				Local n:Int = Range.Find(",")
				If n <> -1 Then
					index=Int(Range[..n])
					count=Int(Range[n+1..])
				Else
					index = Int(Range)
				End If
			EndIf
			
			Local structType:String
			Local saLength:Int
			Local n:Int = t.Find("@")
			If n <> -1 Then
				structType = t[n+1..]
				t = t[..n]
			Else
				n = t.Find("^")
				If n <> -1 Then
					saLength = Int(t[n+1..])
					t = t[..n]
				Else
					If t[..1]="$" t=t[1..].Trim()
					If t[..2].ToLower()="0x" t=t[2..].Trim()
				End If
			End If

?Not ptr64
			Local pointer:Int = Int( "$"+t )
?ptr64
			Local pointer:Long = Long( "$"+t )
?
			If Not structType And Not (pointer And bbGCValidate(Byte Ptr(pointer))) Then Continue
			If saLength Continue
?Not ptr64
			Local inst:Int Ptr=Int Ptr pointer
			Local cmd$="ObjectDump@"+ToHex( Int inst )
?ptr64
			Local inst:Long Ptr=Long Ptr pointer
			Local cmd$="ObjectDump@"+ToHex( Long inst )
?			
			If structType Then
				cmd :+ "@" + structType
			End If
			If i<>-1 cmd:+":"+index
			WriteDebug cmd$+"{~n"

			If structType Then
				DumpStruct inst,index,structType
			Else
				DumpObject inst,index,count
			End If
			WriteDebug "}~n"
		Case "h"
			WriteDebug "T - Stack trace~n"
			WriteDebug "R - Run from here~n"
			WriteDebug "S - Step through source code~n"
			WriteDebug "E - Step into function call~n"
			WriteDebug "L - Leave function or local block~n"
			WriteDebug "Q - Quit~n"
			WriteDebug "H - This text~n"
			WriteDebug "Dxxxxxxxx - Dump object at hex address xxxxxxxx~n"
		Case "q"
			End
		End Select
	Forever

?Win32
	If _appHwnd And _appHwnd<>_ideHwnd 
		If IsIconic(_apphwnd)
			ShowWindow _appHwnd,SW_RESTORE
		Else
			ShowWindow _appHwnd,SW_SHOW
		EndIf		
		_apphwnd=0
	EndIf
?
	indebug=False
End Function

Function _checkConnection:Int()
	If Not _debugConnected Then

		Global checking:Int
		If _debugSocket And Not checking And Not _debugSetup Then
		
			checking = True
			
			If _debugWait Then
				' wait 5 mins
				_serverSocket = _debugSocket.Accept(300000)
				
				' after this, fallback to normal and commence running
				If Not _serverSocket
					_debugWait = False
				End If
			Else
				_serverSocket = _debugSocket.Accept(0)
			End If
			If _serverSocket Then
				_debugConnected = True
			End If

			checking = False
		End If
	End If
	
	Return _debugConnected
End Function

Function OnDebugStop()
	UpdateDebug "DebugStop:~n"
End Function

Function OnDebugLog( message$ )
	WriteStdout "DebugLog:"+message+"~n"
End Function

Function OnDebugEnterStm( stm:Int Ptr )
	Local dbgState:TDbgState = GetDbgState()
	dbgState.currentScope.stm=stm

	_checkConnection()
	
	Select dbgState.Mode
	Case MODE_RUN
		If _debugConnected Then

			' because of the nature of the debugger, we need to switch modes since OnDebugEnterStm is called
			' during socket access - which screws the stack up.
			dbgState.Mode = MODE_BREAK
			Local s:String = ReadDebug(True)
			
			If _debugWait Then
					_debugWait = False
					
					' enter debugging mode!
					OnDebugStop()
					
					' done debugging. If we are still in BREAK mode, return to Running mode
					If dbgState.Mode = MODE_BREAK Then
						dbgState.Mode = MODE_RUN
					End If
					
					Return
			End If
			
			If s Then
				If s[..1].ToLower() = "x" Then
					' enter debugging mode!
					OnDebugStop()
					
					' done debugging. If we are still in BREAK mode, return to Running mode
					If dbgState.Mode = MODE_BREAK Then
						dbgState.Mode = MODE_RUN
					End If
					
					Return
				Else If s[..1].ToLower() = "b" Then
					' format 
					'   b<FILENAME@line>
					Local off:Int = s.find("@")
					If off >= 1 Then
					
						Local file:String = s[2..off]
						Local line:Int = s[off + 1..s.length - 2].ToInt()
					
						If bmx_debugger_AddBreakpoint(file, line) Then
							_bpCount :+ 1
						End If
					End If
				Else If s[..1].ToLower() = "z" Then
					' format 
					'   z<FILENAME@line>
					Local off:Int = s.find("@")
					If off >= 1 Then
						Local file:String = s[2..off]
						Local line:Int = s[off + 1..s.length - 2].ToInt()

						Local count:Int = bmx_debugger_RemoveBreakpoint(file, line)
						If count Then
							_bpCount :- count
						End If
					End If
				End If

				' done debugging. If we are still in BREAK mode, return to Running mode
				If dbgState.Mode = MODE_BREAK Then
					dbgState.Mode = MODE_RUN
				End If
				
				Return

			Else If _bpCount Then
			
				' breakpoints have been defined
				If DebugBreakOnLine(stm) Then
					' enter debugging mode!
					OnDebugStop()
					
					' done debugging. If we are still in BREAK mode, return to Running mode
					If dbgState.Mode = MODE_BREAK Then
						dbgState.Mode = MODE_RUN
					End If
					
					Return				
				End If		

				' carry on as before
				dbgState.Mode = MODE_RUN
				Return
			Else
			
				' carry on as before
				dbgState.Mode = MODE_RUN
				Return
			End If
		Else
			' carry on as before
			Return
		End If
	Case MODE_STEP
		If dbgState.funcLevel>dbgState.debugLevel 
			Return
		EndIf
	Case MODE_STEPOUT
		If dbgState.scopeStackTop>dbgState.debugLevel
			Return
		EndIf
	Case MODE_BREAK
		Return
	End Select
	
	UpdateDebug "Debug:~n"
End Function

Function OnDebugEnterScope( scope:Int Ptr)',inst:Byte Ptr )
	Local dbgState:TDbgState = GetDbgState()
	GCSuspend

	If dbgState.scopeStackTop=dbgState.scopeStack.length 
		dbgState.scopeStack=dbgState.scopeStack[..dbgState.scopeStackTop * 2 + 32]
		For Local i:Int=dbgState.scopeStackTop Until dbgState.scopeStack.length
			dbgState.scopeStack[i]=New TScope
		Next
	EndIf
	
	dbgState.currentScope=dbgState.scopeStack[dbgState.scopeStackTop]

	dbgState.currentScope.scope=scope
	dbgState.currentScope.inst=0

	dbgState.scopeStackTop:+1

	If bmx_debugger_DebugScopeKind(dbgState.currentScope.scope)=DEBUGSCOPEKIND_FUNCTION dbgState.funcLevel:+1

	GCResume	
End Function

Function OnDebugLeaveScope()
	Local dbgState:TDbgState = GetDbgState()
	GCSuspend

	If Not dbgState.scopeStackTop DebugError "scope stack underflow"

	If bmx_debugger_DebugScopeKind(dbgState.currentScope.scope)=DEBUGSCOPEKIND_FUNCTION dbgState.funcLevel:-1
	
	dbgState.scopeStackTop:-1

	If dbgState.scopeStackTop
		dbgState.currentScope=dbgState.scopeStack[dbgState.scopeStackTop-1]
	Else
		dbgState.currentScope=Null
	EndIf

	GCResume	
End Function

Function OnDebugPushExState()

	Local dbgState:TDbgState = GetDbgState()
	GCSuspend

	If dbgState.exStateStackTop=dbgState.exStateStack.length 
		dbgState.exStateStack=dbgState.exStateStack[..dbgState.exStateStackTop * 2 + 32]
		For Local i:Int=dbgState.exStateStackTop Until dbgState.exStateStack.length
			dbgState.exStateStack[i]=New TExState
		Next
	EndIf
	
	dbgState.exStateStack[dbgState.exStateStackTop].scopeStackTop=dbgState.scopeStackTop
	
	dbgState.exStateStackTop:+1

	GCResume	
End Function

Function OnDebugPopExState()

	Local dbgState:TDbgState = GetDbgState()
	GCSuspend

	If Not dbgState.exStateStackTop DebugError "exception stack underflow"

	dbgState.exStateStackTop:-1

	dbgState.scopeStackTop=dbgState.exStateStack[dbgState.exStateStackTop].scopeStackTop
	
	If dbgState.scopeStackTop
		dbgState.currentScope=dbgState.scopeStack[dbgState.scopeStackTop-1]
	Else
		dbgState.currentScope=Null
	EndIf

	GCResume	
End Function

Function OnDebugUnhandledEx( ex:Object )

	GCSuspend
	
	UpdateDebug "Unhandled Exception:"+ex.ToString()+"~n"

	GCResume	
End Function

Type TDebugData
	Field length:Int = 8
	Field data:Byte[1024]
	
	Field prefix:Byte Ptr = "~~>".ToUTF8String()
	
	Method Append(s:String)
		Local b:Byte Ptr = s.toUTF8String()
		Local i:Size_T = strlen_(b)
		
		If length + i + 2 >= data.length Then
			data = data[..length + i + data.length * 2/3]
		End If
		
		MemCopy(Byte Ptr(data) + length, prefix, 2)
		MemCopy(Byte Ptr(data) + length + 2, b, i)
		MemFree(b)
		length :+ i + 2
	End Method
	
	Method Reset()
		length = 8
	End Method
End Type