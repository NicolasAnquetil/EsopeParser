Class {
	#name : 'F77IASTParserTest',
	#superclass : 'TestCase',
	#category : 'Fortran77-IAST-Tests',
	#package : 'Fortran77-IAST-Tests'
}

{ #category : 'helpers' }
F77IASTParserTest >> assertAnchor: anAnchor is: anArray [
	"anArray: startLine startColumn endLine endColumn (exclusive)"

	self assert: anAnchor class equals: IASTIndexedFileAnchor.
	self assert: anAnchor fileName equals: './fortran77.f'.
	self
		assert: { anAnchor startLine. anAnchor startColumn. anAnchor endLine. anAnchor endColumn }
		equals: anArray

]

{ #category : 'helpers' }
F77IASTParserTest >> describe: anObject [
	"compact textual description of an IAST model, for debugging"

	| stream |
	stream := WriteStream on: String new.
	self describe: anObject on: stream.
	^ stream contents

]

{ #category : 'helpers' }
F77IASTParserTest >> describe: x on: s [
	x ifNil: [ ^ s nextPutAll: 'nil' ].
	(x isCollection and: [ x isString not ]) ifTrue: [
		s nextPut: ${.
		x do: [ :each | self describe: each on: s ] separatedBy: [ s space ].
		^ s nextPut: $} ].
	(x isKindOf: IASTAbstractEntity) ifFalse: [ ^ s print: x ].
	s nextPutAll: (x class name allButFirst: 4); nextPut: $(.
	(x respondsTo: #entityName) ifTrue: [ s print: x entityName ].
	((x respondsTo: #isWrite) and: [ x isWrite == true ]) ifTrue: [ s nextPutAll: ' W' ].
	((x respondsTo: #indices) and: [ x indices isEmptyOrNil not ]) ifTrue: [ s nextPutAll: ' idx='. self describe: x indices on: s ].
	((x respondsTo: #arguments) and: [ x arguments notNil ]) ifTrue: [ s nextPutAll: ' args='. self describe: x arguments on: s ].
	((x respondsTo: #format) and: [ x format notNil ]) ifTrue: [ s nextPutAll: ' fmt='. self describe: x format on: s ].
	(x isKindOf: IASTVariable) ifTrue: [
		s nextPutAll: ' t='. x typeSpec ifNil: [ s nextPutAll: '-' ] ifNotNil: [ :t | s nextPutAll: t entityName; print: t length ].
		x dimensions ifNotEmpty: [ s nextPutAll: ' dims='. self describe: x dimensions on: s ] ].
	(x isKindOf: IASTCommon) ifTrue: [ s nextPutAll: ' vars='. self describe: x variables on: s ].
	(x isKindOf: IASTImplicit) ifTrue: [ s nextPutAll: ' rules='. self describe: x rules on: s ].
	(x isKindOf: IASTImplicitTypingRule) ifTrue: [ s nextPutAll: x forType entityName; print: x ranges ].
	(x isKindOf: IASTComment) ifTrue: [ s print: x text ].
	(x isKindOf: IASTProgramUnit) ifTrue: [ s nextPutAll: ' body='. self describe: x body on: s ].
	(x isKindOf: IASTImpliedDo) ifTrue: [ s nextPutAll: ' specs='. self describe: x doSpecs on: s. s nextPutAll: ' exprs='. self describe: x expressions on: s ].
	s nextPut: $)

]

{ #category : 'helpers' }
F77IASTParserTest >> famixModelOf: aProgramFile [
	| visitor |
	visitor := IASTToFamixFortranVisitor new.
	aProgramFile accept: visitor.
	^ visitor model

]

{ #category : 'helpers' }
F77IASTParserTest >> lines: anArray [
	^ String lf join: anArray

]

{ #category : 'helpers' }
F77IASTParserTest >> parse: aString [
	"Answer the IASTProgramFile of aString, failing the test if it is rejected"

	| result |
	result := (self parserClass new fileName: './fortran77.f'; yourself) parse: aString.
	self deny: result isPetit2Failure description: [ result printString ].
	^ result

]

{ #category : 'helpers' }
F77IASTParserTest >> parseLines: anArray [
	^ self parse: (self lines: anArray)

]

{ #category : 'accessing' }
F77IASTParserTest >> parserClass [
	^ F77IASTParser

]

{ #category : 'tests' }
F77IASTParserTest >> testAccessArrayWithIndices [
	| body var |
	body := self unitBody: #(
		'      subroutine hello'
		'      integer arr(10)'
		'      someVar = arr(x)'
		'      end').
	self assert: body second size equals: 2.
	var := body second second.
	self assert: var class equals: IASTVarAccess.
	self assert: var entityName equals: 'arr'.
	self assert: var indices size equals: 1.
	self assert: var indices first class equals: IASTVarAccess.
	self assert: var indices first entityName equals: 'x'.
	self assertAnchor: var sourceAnchor is: #(3 17 3 23)

]

{ #category : 'tests' }
F77IASTParserTest >> testAccessArrayWithIndicesLiteral [
	| body var |
	body := self unitBody: #(
		'      subroutine hello'
		'      integer arr(10)'
		'      someVar = arr(5)'
		'      end').
	self assert: body size equals: 2.
	self assert: body second size equals: 2.
	var := body second second.
	self assert: var class equals: IASTVarAccess.
	self assert: var entityName equals: 'arr'.
	"a nil index says that this is an array"
	self assert: var indices size equals: 1.
	self assert: var indices first isNil

]

{ #category : 'tests' }
F77IASTParserTest >> testAccessArrayWithSeveralIndices [
	| body var |
	body := self unitBody: #(
		'      subroutine hello'
		'      integer arr(10, 10, 10)'
		'      someVar = arr(x, 2, y)'
		'      end').
	self assert: body size equals: 2.
	self assert: body second size equals: 2.
	var := body second second.
	self assert: var class equals: IASTVarAccess.
	self assert: var entityName equals: 'arr'.
	self assert: var indices size equals: 3.
	self assert: var indices first entityName equals: 'x'.
	self assert: var indices second isNil.
	self assert: var indices third entityName equals: 'y'

]

{ #category : 'tests' }
F77IASTParserTest >> testArithmeticIfStatement [
	self assert: ((self unitBody: #(
		'      program p'
		'      if (x - y) 10, 20, 30'
		'   10 continue'
		'   20 continue'
		'   30 end')) first collect: #entityName) asArray equals: #('x' 'y')

]

{ #category : 'tests' }
F77IASTParserTest >> testArrayDeclarationDimensions [
	| var |
	var := (self unitBody: #(
		'      subroutine hello(n, b)'
		'      real arr(0:n, 10), b(*)'
		'      end')) first first.
	self assert: var dimensions size equals: 2.
	self assert: var dimensions first class equals: IASTVarAccess.
	self assert: var dimensions first entityName equals: 'n'.
	self assert: var dimensions second class equals: IASTUnknownArrayDimension

]

{ #category : 'tests' }
F77IASTParserTest >> testArrayDeclaredInCommon [
	| var |
	var := (self unitBody: #(
		'      subroutine hello'
		'      common /c/ arr(10)'
		'      x = arr(i)'
		'      end')) second second.
	self assert: var class equals: IASTVarAccess

]

{ #category : 'tests' }
F77IASTParserTest >> testArrayDeclaredWithDimension [
	| body var |
	body := self unitBody: #(
		'      subroutine hello'
		'      dimension arr(10)'
		'      x = arr(i)'
		'      end').
	var := body second second.
	self assert: var class equals: IASTVarAccess.
	self assert: var indices first entityName equals: 'i'

]

{ #category : 'tests' }
F77IASTParserTest >> testArrayElementAssignmentIsNotAStatementFunction [
	| stmt |
	stmt := (self unitBody: #(
		'      program p'
		'      real t(3)'
		'      t(i) = 0'
		'      end')) second.
	self assert: stmt first class equals: IASTVarAccess.
	self assert: stmt first isWrite

]

{ #category : 'tests' }
F77IASTParserTest >> testAssignementStatement [
	| stmt |
	stmt := (self unitBody: #(
		'      subroutine hello'
		'      someVar = 3'
		'      end')) first.
	self assert: stmt size equals: 1.
	self assert: stmt first class equals: IASTVarAccess.
	self assert: stmt first entityName equals: 'somevar'.
	self assert: stmt first isWrite.
	self assertAnchor: stmt first sourceAnchor is: #(2 7 2 14)

]

{ #category : 'tests' }
F77IASTParserTest >> testAssignementStatementWithComplexRHS [
	| stmt |
	stmt := (self unitBody: #(
		'      subroutine hello'
		'      someVar = otherVar + thirdVar'
		'      end')) first.
	self assert: stmt size equals: 3.
	self assert: (stmt collect: #class) asArray equals: { IASTVarAccess. IASTVarAccess. IASTVarAccess }.
	self assert: (stmt collect: #entityName) asArray equals: #('somevar' 'othervar' 'thirdvar').
	self assert: (stmt collect: #isWrite) asArray equals: #(true false false)

]

{ #category : 'tests' }
F77IASTParserTest >> testBlankCommonStatement [
	| common |
	common := (self unitBody: #(
		'      subroutine hello'
		'      common a, b'
		'      end')) first first.
	self assert: common entityName isNil.
	self assert: (common variables collect: #entityName) asArray equals: #('a' 'b').
	self assertAnchor: common sourceAnchor is: #(2 14 2 18)

]

{ #category : 'tests' }
F77IASTParserTest >> testBlankLinesAreIgnored [
	| programFile |
	programFile := self parseLines: #(
		''
		'      program p'
		''
		'      end'
		'').
	self assert: programFile body size equals: 1.
	self assert: programFile body first body isEmpty

]

{ #category : 'tests' }
F77IASTParserTest >> testBlockDataWithOneCommonOneData [
	| blockdata |
	blockdata := (self parseLines: #(
		'      block data hello'
		'      common /mycom/ i'
		'      data i /12345/'
		'      end')) body first.
	self assert: blockdata class equals: IASTBlockData.
	self assert: blockdata entityName equals: 'hello'.
	"DATA in a block data gives no access"
	self assert: blockdata body size equals: 1.
	self assert: blockdata body first first class equals: IASTCommon.
	self assert: blockdata accesses isEmpty.
	self assert: blockdata comments isEmpty

]

{ #category : 'tests' }
F77IASTParserTest >> testCRLFAnchors [
	| programFile |
	programFile := self parse: 'c     blah' , String crlf , '      end' , String crlf.
	self assertAnchor: programFile body first sourceAnchor is: #(1 1 1 11).
	self assertAnchor: programFile body second sourceAnchor is: #(2 7 2 10)

]

{ #category : 'tests' }
F77IASTParserTest >> testComment [
	| programFile stmt |
	programFile := self parseLines: #(
		'c     blah'
		'      end').
	self assert: programFile body size equals: 2.
	stmt := programFile body first.
	self assert: stmt class equals: IASTComment.
	self assert: stmt text equals: '     blah'.
	self assert: stmt isProgUnit.
	self assertAnchor: stmt sourceAnchor is: #(1 1 1 11)

]

{ #category : 'tests' }
F77IASTParserTest >> testCommentInside [
	| programFile stmt |
	programFile := self parseLines: #(
		'      integer function hello()'
		'c     blah'
		'      end').
	self assert: programFile body size equals: 1.
	self assert: programFile body first body size equals: 1.
	stmt := programFile body first body first.
	self assert: stmt class equals: IASTComment.
	self assert: stmt text equals: '     blah'.
	self deny: stmt isProgUnit.
	self assert: programFile body first comments size equals: 1

]

{ #category : 'tests' }
F77IASTParserTest >> testCommentKinds [
	| comments |
	comments := (self parseLines: #(
		'      program p'
		'C upper'
		'* star'
		'! bang'
		'   ! indented bang'
		'      end')) body first body.
	self assert: (comments collect: #text) asArray equals: #(' upper' ' star' ' bang' ' indented bang')

]

{ #category : 'tests' }
F77IASTParserTest >> testCommentOutside [
	| programFile stmt |
	programFile := self parseLines: #(
		'      program p'
		'      end'
		'c     blah'
		'      integer function hello()'
		'      end').
	self assert: programFile body size equals: 3.
	stmt := programFile body second.
	self assert: stmt class equals: IASTComment.
	self assert: stmt isProgUnit.
	self assert: stmt text equals: '     blah'.
	self assert: programFile body third class equals: IASTFunction

]

{ #category : 'tests' }
F77IASTParserTest >> testCommonStatement [
	| stmt common |
	stmt := (self unitBody: #(
		'      subroutine hello'
		'      common /mycom/ i, t(10) // k'
		'      end')) first.
	self assert: stmt size equals: 2.
	common := stmt first.
	self assert: common class equals: IASTCommon.
	self assert: common entityName equals: 'mycom'.
	self assert: (common variables collect: #class) asArray equals: { IASTVariable. IASTVariable }.
	self assert: (common variables collect: #entityName) asArray equals: #('i' 't').
	self assert: common variables second dimensions size equals: 1.
	"variable anchors are their name"
	self assertAnchor: common sourceAnchor is: #(2 15 2 26).
	common := stmt second.
	self assert: common entityName isNil.
	self assert: (common variables collect: #entityName) asArray equals: #('k')

]

{ #category : 'tests' }
F77IASTParserTest >> testCommonVariableTypeIsMerged [
	| body common |
	body := self unitBody: #(
		'      subroutine hello'
		'      common /consts/ c, pi, t'
		'      double precision c, pi'
		'      integer k'
		'      dimension t(3)'
		'      end').
	"the type declarations of c and pi, and the dimension of t, are merged in the common"
	self assert: (body flattened collect: #class) asArray equals: { IASTCommon. IASTVariable }.
	common := body first first.
	self assert: (common variables collect: [ :each | each typeSpec ifNotNil: [ :type | type entityName ] ]) asArray equals: #('double precision' 'double precision' nil).
	self assert: common variables third dimensions size equals: 1.
	self assert: body second first entityName equals: 'k'

]

{ #category : 'tests' }
F77IASTParserTest >> testComputedAndAssignedGoto [
	| body |
	body := self unitBody: #(
		'      PROGRAM MYPROG'
		'      assign 10 to lab'
		'      go to (10, 20) k'
		'      go to lab, (10, 20)'
		'   10 continue'
		'   20 END').
	self assert: (body flattened collect: #entityName) asArray equals: #('lab' 'k' 'lab').
	self assert: (body flattened collect: #isWrite) asArray equals: #(true false false)

]

{ #category : 'tests' }
F77IASTParserTest >> testContinueStatement [
	self assert: (self unitBody: #(
		'      PROGRAM MYPROG'
		'      continue'
		'      END')) isEmpty

]

{ #category : 'tests' }
F77IASTParserTest >> testContinuedHeader [
	"anchors come from the actual position of the tokens"

	| sub |
	sub := (self parseLines: #(
		'      subroutine hello(a,'
		'     &                 b)'
		'      end')) body first.
	self assert: (sub parameters collect: #entityName) asArray equals: #('a' 'b').
	self assertAnchor: sub parameters second sourceAnchor is: #(2 24 2 25)

]

{ #category : 'tests' }
F77IASTParserTest >> testDataStatement [
	| stmt |
	stmt := (self unitBody: #(
		'      program p'
		'      data a, b /2*0.0/, (c(i), i = 1, n) /3*k/'
		'      end')) first.
	self assert: (stmt collect: #entityName) asArray equals: #('a' 'b' 'c' 'i' 'n' 'k').
	self assert: (stmt collect: #isWrite) asArray equals: #(true true true true true false)

]

{ #category : 'tests' }
F77IASTParserTest >> testDimensionMergedWithTypeDeclaration [
	| body |
	body := self unitBody: #(
		'      subroutine hello'
		'      real b'
		'      dimension b(10), c(5)'
		'      end').
	self assert: (body flattened collect: #entityName) asArray equals: #('b' 'c').
	self assert: body first first dimensions size equals: 1.
	self assert: body first first typeSpec entityName equals: 'real'.
	self assert: body second first typeSpec isNil

]

{ #category : 'tests' }
F77IASTParserTest >> testEmptyBlockData [
	| programFile block |
	programFile := self parseLines: #(
		'      BLOCK data hello'
		'      end').
	self assert: programFile body size equals: 1.
	block := programFile body first.
	self assert: block class equals: IASTBlockData.
	self assert: block entityName equals: 'hello'.
	self assert: block body isEmpty.
	self assert: block comments isEmpty.
	self assertAnchor: block sourceAnchor is: #(1 7 2 10)

]

{ #category : 'tests' }
F77IASTParserTest >> testEmptyExitStatement [
	self assert: (self unitBody: #(
		'      PROGRAM MYPROG'
		'      do i = 1, 2'
		'        exit'
		'      enddo'
		'      END')) first size equals: 1

]

{ #category : 'tests' }
F77IASTParserTest >> testEmptyFunction [
	| programFile fct |
	programFile := self parseLines: #(
		'      integer function hello()'
		'      end').
	self assert: programFile body size equals: 1.
	fct := programFile body first.
	self assert: fct class equals: IASTFunction.
	self assert: fct entityName equals: 'hello'.
	self assert: fct parameters isEmpty.
	self assert: fct returnType class equals: IASTTypeRef.
	self assert: fct returnType entityName equals: 'integer'.
	self assert: fct body isEmpty.
	self assertAnchor: fct sourceAnchor is: #(1 7 2 10)

]

{ #category : 'tests' }
F77IASTParserTest >> testEmptyFunctionWithArguments [
	| fct |
	fct := (self parseLines: #(
		'      integer function hello(i,j)'
		'      end')) body first.
	self assert: fct entityName equals: 'hello'.
	self assert: (fct parameters collect: #class) asArray equals: { IASTParameter. IASTParameter }.
	self assert: (fct parameters collect: #entityName) asArray equals: #('i' 'j')

]

{ #category : 'tests' }
F77IASTParserTest >> testEmptyIncludedFile [
	| programFile |
	programFile := (self parserClass newStartingAt: #includeFile)
		fileName: './fortran77.f';
		parse: ''.
	self assert: programFile body first body isEmpty

]

{ #category : 'tests' }
F77IASTParserTest >> testEmptyProgram [
	| programFile prog |
	programFile := self parseLines: #(
		'      program hello'
		'      end').
	self assert: programFile class equals: IASTProgramFile.
	self assert: programFile filename equals: './fortran77.f'.
	self assert: programFile body size equals: 1.
	prog := programFile body first.
	self assert: prog class equals: IASTMainProgram.
	self assert: prog entityName equals: 'hello'.
	self assert: prog body isEmpty.
	self assert: prog comments isEmpty.
	self assertAnchor: prog sourceAnchor is: #(1 7 2 10)

]

{ #category : 'tests' }
F77IASTParserTest >> testEmptySubroutine [
	| programFile sub |
	programFile := self parseLines: #(
		'      subroutine hello'
		'      end').
	self assert: programFile body size equals: 1.
	sub := programFile body first.
	self assert: sub class equals: IASTSubroutine.
	self assert: sub entityName equals: 'hello'.
	self assert: sub parameters isEmpty.
	self assert: sub body isEmpty.
	self assertAnchor: sub sourceAnchor is: #(1 7 2 10)

]

{ #category : 'tests' }
F77IASTParserTest >> testEmptySubroutineTwoParams [
	| sub |
	sub := (self parseLines: #(
		'      subroutine hello( param1, param2 )'
		'      end')) body first.
	self assert: sub entityName equals: 'hello'.
	self assert: (sub parameters collect: #class) asArray equals: { IASTParameter. IASTParameter }.
	self assert: (sub parameters collect: #entityName) asArray equals: #('param1' 'param2').
	self assertAnchor: sub parameters second sourceAnchor is: #(1 33 1 39).
	self assert: sub body isEmpty

]

{ #category : 'tests' }
F77IASTParserTest >> testEndWithName [
	| sub |
	sub := (self parseLines: #(
		'      subroutine hello'
		'      end subroutine hello')) body first.
	self assertAnchor: sub sourceAnchor is: #(1 7 2 27)

]

{ #category : 'tests' }
F77IASTParserTest >> testExpressionOperators [
	| stmt |
	stmt := (self unitBody: #(
		'      program p'
		'      l = .not. (a .lt. -b ** c) .and. d // e(1:2) .eqv. .true. .or. f .ne. (1.5e3, 2.0)'
		'      end')) first.
	self assert: (stmt collect: #entityName) asArray equals: #('l' 'a' 'b' 'c' 'd' 'e' 'f')

]

{ #category : 'tests' }
F77IASTParserTest >> testExternalStatement [
	| prog stmt |
	prog := (self parseLines: #(
		'      program p'
		'      external fct'
		'      end')) body first.
	self assert: prog body size equals: 1.
	stmt := prog body first first.
	self assert: stmt class equals: IASTVarAccess.
	self assert: stmt entityName equals: 'fct'.
	self assert: stmt annotations equals: 'external'.
	self assert: stmt isExternal.
	self assertAnchor: stmt sourceAnchor is: #(2 16 2 19)

]

{ #category : 'tests' }
F77IASTParserTest >> testFamixModel [
	"the IAST model can be converted to Famix by the existing IASTToFamixFortranVisitor"

	| model |
	model := self famixModelOf: (self parseLines: #(
		'c     a program'
		'      program main'
		'      integer n, t(10)'
		'      common /c/ x'
		'      n = 10'
		'      call sub(n, t)'
		'      write (*, *) f(n)'
		'      end'
		'      subroutine sub(k, a)'
		'      integer k, a(*)'
		'      a(k) = k'
		'      end'
		'      integer function f(i)'
		'      f = i * 2'
		'      end'
		'      block data bd'
		'      common /c/ x'
		'      data x /1.0/'
		'      end')).
	self assert: (model allWithType: FamixF77PUMain) size equals: 1.
	self assert: (model allWithType: FamixF77PUSubroutine) size equals: 1.
	self assert: (model allWithType: FamixF77PUFunction) size equals: 1.
	self assert: (model allWithType: FamixF77PUBlockdata) size equals: 1.
	self assert: ((model allWithType: FamixF77Invocation) collect: #signature) asSet equals: #('sub' 'f') asSet.
	self assert: (model allWithType: FamixF77Access) notEmpty.
	self assert: ((model allWithType: FamixF77Variable) collect: #name) asSet equals: #('n' 't' 'k' 'a' 'x') asSet

]

{ #category : 'tests' }
F77IASTParserTest >> testFileStatements [
	| body |
	body := self unitBody: #(
		'      subroutine f'
		'      open (unit=lu, file=name, status=''old'', err=10)'
		'      inquire (unit=lu, exist=ex, number=num)'
		'      backspace 6'
		'      rewind iu'
		'      end file (unit=lu)'
		'      endfile 6'
		'      close (lu)'
		'   10 end').
	self assert: (body collect: #entityName) asArray equals: #('open' 'inquire' 'backspace' 'rewind' 'endfile' 'endfile' 'close').
	self assert: (body first format collect: #entityName) asArray equals: #('lu' 'name').
	self assert: (body second format collect: #isWrite) asArray equals: #(false true true).
	self assert: (body fourth format collect: #entityName) asArray equals: #('iu')

]

{ #category : 'tests' }
F77IASTParserTest >> testFunctionInvocationWithoutArgument [
	| call |
	call := (self unitBody: #(
		'      program p'
		'      x = f()'
		'      end')) first second.
	self assert: call class equals: IASTInvocation.
	self assert: call arguments isEmpty

]

{ #category : 'tests' }
F77IASTParserTest >> testFunctionReturnTypes [
	#( #('DOUBLE PRECISION' 'double precision') #('DOUBLEPRECISION' 'double precision')
	   #('REAL*8' 'real') #('CHARACTER*(*)' 'character') #('LOGICAL' 'logical') #('DOUBLE COMPLEX' 'double complex') )
		do: [ :pair | | fct |
			fct := (self parseLines: { '      ' , pair first , ' FUNCTION F(X)'. '      END' }) body first.
			self assert: fct returnType entityName equals: pair second ]

]

{ #category : 'tests' }
F77IASTParserTest >> testFunctionWithFunctionInvocation [
	| stmt |
	stmt := (self unitBody: #(
		'      integer function efunc()'
		'      i = ifunc (i, j)'
		'      end')) first.
	self assert: stmt second class equals: IASTInvocation.
	self assert: stmt second entityName equals: 'ifunc'.
	self assert: stmt second arguments size equals: 2.
	self deny: stmt second isIntrinsic.
	self assertAnchor: stmt second sourceAnchor is: #(2 11 2 23)

]

{ #category : 'tests' }
F77IASTParserTest >> testFunctionWithoutType [
	| fct |
	fct := (self parseLines: #(
		'      FUNCTION F(X)'
		'      END')) body first.
	self assert: fct class equals: IASTFunction.
	self assert: fct returnType isNil.
	self assertAnchor: fct sourceAnchor is: #(1 7 2 10)

]

{ #category : 'tests' }
F77IASTParserTest >> testGotoStatement [
	self assert: (self unitBody: #(
		'      PROGRAM MYPROG'
		'      goto 100'
		'  100 stop'
		'      END')) isEmpty

]

{ #category : 'tests' }
F77IASTParserTest >> testIfStatement [
	| body |
	body := self unitBody: #(
		'      integer function hello()'
		'      if(a) a=1'
		'      end').
	self assert: body size equals: 1.
	self assert: (body first collect: #entityName) asArray equals: #('a' 'a').
	self assert: (body first collect: #isWrite) asArray equals: #(false true)

]

{ #category : 'tests' }
F77IASTParserTest >> testImplicitNoneStatement [
	| body |
	body := self unitBody: #(
		'      PROGRAM MYPROG'
		'       implicit none'
		'      END').
	self assert: body size equals: 1.
	self assert: body first class equals: IASTImplicit.
	self assert: body first rules isNil.
	self assertAnchor: body first sourceAnchor is: #(2 8 2 21)

]

{ #category : 'tests' }
F77IASTParserTest >> testImplicitStatementSeveralRanges [
	| stmt rule |
	stmt := (self unitBody: #(
		'      PROGRAM MYPROG'
		'       implicit character (c-e, h, L-M), double precision (D)'
		'      END')) first.
	self assert: stmt rules size equals: 2.
	rule := stmt rules first.
	self assert: rule ranges size equals: 3.
	self assertCollection: rule ranges first equals: { $c. $e }.
	self assertCollection: rule ranges second equals: { $h. $h }.
	self assertCollection: rule ranges third equals: { $l. $m }.
	self assertAnchor: rule sourceAnchor is: #(2 17 2 40).
	rule := stmt rules second.
	self assert: rule forType entityName equals: 'double precision'.
	self assertCollection: rule ranges first equals: { $d. $d }

]

{ #category : 'tests' }
F77IASTParserTest >> testImplicitStatementSimpleChar [
	| stmt rule |
	stmt := (self unitBody: #(
		'      PROGRAM MYPROG'
		'       implicit character (c)'
		'      END')) first.
	self assert: stmt class equals: IASTImplicit.
	self assert: stmt rules size equals: 1.
	rule := stmt rules first.
	self assert: rule class equals: IASTImplicitTypingRule.
	self assert: rule forType class equals: IASTTypeRef.
	self assert: rule forType entityName equals: 'character'.
	self assert: rule ranges size equals: 1.
	self assertCollection: rule ranges first equals: { $c. $c }

]

{ #category : 'tests' }
F77IASTParserTest >> testIncludeDirectiveOutsideProgramUnits [
	| programFile |
	programFile := self parseLines: #(
		'# include <sys.h>'
		'#ifdef DEBUG'
		'      program p'
		'#endif'
		'      end').
	self assert: (programFile body collect: #class) asArray equals: { IASTInclude. IASTMainProgram }.
	self assert: programFile body first entityName equals: 'sys.h'.
	self assert: programFile body second body isEmpty

]

{ #category : 'tests' }
F77IASTParserTest >> testIncludeFile [
	| stmt |
	stmt := (self unitBody: #(
		'      subroutine hello( )'
		'#include "file.inc"'
		'      end')) first.
	self assert: stmt class equals: IASTInclude.
	self assert: stmt includeCommand equals: '#include'.
	self assert: stmt entityName equals: 'file.inc'.
	self assertAnchor: stmt sourceAnchor is: #(2 1 2 20)

]

{ #category : 'tests' }
F77IASTParserTest >> testIncludeStatement [
	| stmt |
	stmt := (self unitBody: #(
		'      subroutine hello'
		'      include ''file.inc'''
		'      end')) first.
	self assert: stmt class equals: IASTInclude.
	self assert: stmt includeCommand equals: 'include'.
	self assert: stmt entityName equals: 'file.inc'.
	self assertAnchor: stmt sourceAnchor is: #(2 7 2 25)

]

{ #category : 'tests' }
F77IASTParserTest >> testIncludedFileFragment [
	| programFile unit |
	programFile := (self parserClass newStartingAt: #includeFile)
		fileName: './fortran77.f';
		parse: (self lines: #(
			'c     a fragment'
			'      integer t(10)'
			'      common /c/ t'
			'      x = t(i) + f(i)')).
	self deny: programFile isPetit2Failure.
	self assert: programFile body size equals: 1.
	unit := programFile body first.
	self assert: unit class equals: IASTIncludedFile.
	self assert: unit entityName equals: 'fortran77.f'.
	"the declaration of t is merged in the common"
	self assert: unit body size equals: 3.
	self assert: unit body first class equals: IASTComment.
	self assert: unit body second first class equals: IASTCommon.
	self assert: unit body second first variables first dimensions size equals: 1.
	self assert: unit body third second class equals: IASTVarAccess.
	self assert: unit body third third class equals: IASTInvocation.
	self assertAnchor: unit sourceAnchor is: #(1 1 4 22)

]

{ #category : 'tests' }
F77IASTParserTest >> testIndexWithSeveralAccesses [
	| var |
	var := (self unitBody: #(
		'      subroutine hello'
		'      integer arr(10)'
		'      x = arr(i + j)'
		'      end')) second second.
	self assert: var indices size equals: 1.
	self assert: (var indices first collect: #entityName) asArray equals: #('i' 'j')

]

{ #category : 'tests' }
F77IASTParserTest >> testIntegerVariableDeclaration [
	| body var |
	body := self unitBody: #(
		'      subroutine hello'
		'      integer i'
		'      end').
	self assert: body first size equals: 1.
	var := body first first.
	self assert: var class equals: IASTVariable.
	self assert: var entityName equals: 'i'.
	self assert: var typeSpec class equals: IASTTypeRef.
	self assert: var typeSpec entityName equals: 'integer'.
	self assert: var dimensions isEmpty.
	self assertAnchor: var sourceAnchor is: #(2 15 2 16)

]

{ #category : 'tests' }
F77IASTParserTest >> testIntrinsicFunctionInvocation [
	| stmt |
	stmt := (self unitBody: #(
		'      program p'
		'      x = SQRT(y) + max(a, 1.0)'
		'      end')) first.
	self assert: (stmt allButFirst collect: #class) asArray equals: { IASTInvocation. IASTInvocation }.
	self assert: (stmt allButFirst collect: #entityName) asArray equals: #('sqrt' 'max').
	self assert: (stmt allButFirst allSatisfy: #isIntrinsic).
	self assert: (stmt second arguments collect: #entityName) asArray equals: #('y').
	self assert: (stmt third arguments collect: #entityName) asArray equals: #('a')

]

{ #category : 'tests' }
F77IASTParserTest >> testIntrinsicStatement [
	self assert: (self unitBody: #(
		'      subroutine hello'
		'      intrinsic SIN, COS'
		'      end')) isEmpty

]

{ #category : 'tests' }
F77IASTParserTest >> testLabelledDoAndDoWhile [
	| prog |
	prog := (self parseLines: #(
		'      program hello'
		'      do 10, k = i, j, 2'
		'        x = k'
		'   10 continue'
		'      do while (x .gt. 0)'
		'        x = x - 1'
		'      end do'
		'      end')) body first.
	self assert: (prog accesses collect: #entityName) asArray equals: #('k' 'i' 'j' 'x' 'k' 'x' 'x' 'x').
	self assert: (prog accesses collect: #isWrite) asArray equals: #(true false false true false false true false)

]

{ #category : 'tests' }
F77IASTParserTest >> testLogicalIfWithCall [
	| stmt |
	stmt := (self unitBody: #(
		'      subroutine hello'
		'      if (x) call s(y)'
		'      end')) first.
	self assert: (stmt collect: #class) asArray equals: { IASTVarAccess. IASTInvocation }

]

{ #category : 'tests' }
F77IASTParserTest >> testMainProgramWithoutHeader [
	| programFile prog |
	programFile := self parseLines: #(
		'      end').
	prog := programFile body first.
	self assert: prog class equals: IASTMainProgram.
	self assert: prog entityName isNil.
	self assertAnchor: prog sourceAnchor is: #(1 7 1 10)

]

{ #category : 'tests' }
F77IASTParserTest >> testMultipleStatements [
	| body stmt |
	body := self unitBody: #(
		'      subroutine hello'
		'      integer someVar'
		'      someVar = 3 * someVar'
		'      end').
	self assert: body size equals: 2.
	stmt := body first first.
	self assert: stmt class equals: IASTVariable.
	self assert: stmt typeSpec class equals: IASTTypeRef.
	self assert: stmt entityName equals: 'somevar'.
	stmt := body second first.
	self assert: stmt class equals: IASTVarAccess.
	self assert: stmt isWrite.
	self assert: stmt entityName equals: 'somevar'.
	stmt := body second second.
	self assert: stmt class equals: IASTVarAccess.
	self deny: stmt isWrite.
	self assert: stmt entityName equals: 'somevar'

]

{ #category : 'tests' }
F77IASTParserTest >> testNestedFunctionInvocations [
	| call |
	call := (self unitBody: #(
		'      program p'
		'      integer t(5)'
		'      x = f(g(y), t(i))'
		'      end')) second second.
	self assert: call class equals: IASTInvocation.
	self assert: (call arguments collect: #class) asArray equals: { IASTInvocation. IASTVarAccess }.
	self assert: call arguments first arguments first entityName equals: 'y'.
	self assert: call arguments second indices first entityName equals: 'i'

]

{ #category : 'tests' }
F77IASTParserTest >> testParameterStatement [
	| stmt access |
	stmt := (self unitBody: #(
		'      subroutine paramstmt'
		'      PARAMETER (LEGERE=1, GRAVE=2, FATALE=3, FINALE=4)'
		'      END')) first.
	self assert: stmt size equals: 4.
	stmt do: [ :each |
		self assert: each class equals: IASTVarAccess.
		self assert: each isWrite.
		self assert: each isParameterDefinition ].
	self assert: (stmt collect: #entityName) asArray equals: #('legere' 'grave' 'fatale' 'finale').
	access := stmt detect: [ :e | e entityName = 'grave' ].
	self assertAnchor: access sourceAnchor is: #(2 28 2 33)

]

{ #category : 'tests' }
F77IASTParserTest >> testParameterStatementWithExpression [
	| stmt |
	stmt := (self unitBody: #(
		'      subroutine p'
		'      PARAMETER (N = 10, M = N * 2)'
		'      END')) first.
	self assert: (stmt collect: #entityName) asArray equals: #('n' 'm' 'n').
	self assert: (stmt collect: #isWrite) asArray equals: #(true true false)

]

{ #category : 'tests' }
F77IASTParserTest >> testParseFile [
	| file programFile |
	file := FileSystem memory root / 'hello.f'.
	file writeStreamDo: [ :stream | stream nextPutAll: (self lines: #('      subroutine hello' '      end')) ].
	programFile := self parserClass new parseFile: file.
	self assert: programFile filename equals: file fullName.
	self assert: programFile body first entityName equals: 'hello'.
	self assert: programFile body first sourceAnchor fileName equals: file fullName

]

{ #category : 'tests' }
F77IASTParserTest >> testProgramWithEmptyDoStatement [
	| prog |
	prog := (self parseLines: #(
		'      program hello'
		'      do i=1, n'
		'      enddo'
		'      end')) body first.
	self assert: (prog accesses collect: #entityName) asArray equals: #('i' 'n').
	self assert: prog accesses first isWrite

]

{ #category : 'tests' }
F77IASTParserTest >> testProgramWithMultipleProgUnits [
	| programFile node |
	programFile := self parseLines: #(
		'      program myprog'
		'      end'
		''
		'      integer function ifunc( k, l )'
		'      end'
		''
		'      subroutine mysub (msg)'
		'      end').
	self assert: programFile body size equals: 3.
	node := programFile body first.
	self assert: node class equals: IASTMainProgram.
	self assert: node entityName equals: 'myprog'.
	self assert: node body size equals: 0.
	self assert: node accesses size equals: 0.
	self assert: node invocations size equals: 0.
	node := programFile body second.
	self assert: node class equals: IASTFunction.
	self assert: (node parameters collect: #entityName) asArray equals: #('k' 'l').
	self assertAnchor: node sourceAnchor is: #(4 7 5 10).
	node := programFile body third.
	self assert: node class equals: IASTSubroutine.
	self assert: (node parameters collect: #entityName) asArray equals: #('msg')

]

{ #category : 'tests' }
F77IASTParserTest >> testProgramWithOneStatement [
	| prog |
	prog := (self parseLines: #(
		'      program hello'
		'      a = -b'
		'      end')) body first.
	self assert: prog body size equals: 1.
	self assert: prog comments isEmpty.
	self assert: prog body first size equals: 2

]

{ #category : 'tests' }
F77IASTParserTest >> testProgramWithSimpleIfElseStatementWithBody [
	| prog |
	prog := (self parseLines: #(
		'      program hello'
		'      if (i .lt. j) then'
		'        i = i + 1'
		'      else if (i .gt. 0) then'
		'        call s(i)'
		'      else'
		'        j = j - 1'
		'      endif'
		'      end')) body first.
	self assert: prog body size equals: 1.
	self assert: (prog accesses collect: #entityName) asArray equals: #('i' 'j' 'i' 'i' 'i' 'j' 'j').
	self assert: (prog accesses collect: #isWrite) asArray equals: #(false false true false false true false).
	self assert: (prog invocations collect: #entityName) asArray equals: #('s')

]

{ #category : 'tests' }
F77IASTParserTest >> testProgramWithSimpleIfStatementWithoutBody [
	| prog |
	prog := (self parseLines: #(
		'      program hello'
		'      if (i .lt. j) then'
		'      endif'
		'      end')) body first.
	self assert: (prog accesses collect: #entityName) asArray equals: #('i' 'j')

]

{ #category : 'tests' }
F77IASTParserTest >> testReadStatement [
	| stmt implied |
	stmt := (self unitBody: #(
		'      subroutine r'
		'      integer a(10)'
		'      read (5, *, iostat=ios, end=99) u, (a(i), i = 1, m)'
		'   99 end')) second.
	self assert: stmt class equals: IASTIOStatement.
	self assert: stmt entityName equals: 'read'.
	self assert: (stmt format collect: #entityName) asArray equals: #('ios').
	self assert: stmt format first isWrite.
	self assert: stmt arguments first entityName equals: 'u'.
	self assert: stmt arguments first isWrite.
	implied := stmt arguments second.
	self assert: implied class equals: IASTImpliedDo.
	self assert: (implied expressions collect: #entityName) asArray equals: #('a').
	self assert: implied expressions first isWrite.
	self assert: (implied doSpecs collect: #entityName) asArray equals: #('i' 'm').
	self assert: (implied doSpecs collect: #isWrite) asArray equals: #(true false)

]

{ #category : 'tests' }
F77IASTParserTest >> testRejectedSource [
	| result |
	result := (self parserClass new fileName: 'x.f'; yourself) parse: (self lines: #('      program p')).
	self assert: result isPetit2Failure

]

{ #category : 'tests' }
F77IASTParserTest >> testReturnConstantStatement [
	self assert: (self unitBody: #(
		'      integer function hello()'
		'      return 42'
		'      end')) isEmpty

]

{ #category : 'tests' }
F77IASTParserTest >> testReturnStatement [
	| body |
	body := self unitBody: #(
		'      integer function hello()'
		'      return someVar'
		'      end').
	self assert: body size equals: 1.
	self assert: body first first class equals: IASTVarAccess.
	self assert: body first first entityName equals: 'somevar'

]

{ #category : 'tests' }
F77IASTParserTest >> testSaveAndEntryStatements [
	self assert: (self unitBody: #(
		'      subroutine hello(a)'
		'      save x, /c/'
		'      entry e2(a)'
		'      end')) isEmpty

]

{ #category : 'tests' }
F77IASTParserTest >> testShortReadAndPrint [
	| body |
	body := self unitBody: #(
		'      subroutine r'
		'      read *, v'
		'      print 100, w, (x(k), k = 1, 3)'
		'  100 format (1x, 3hABC, i5, a)'
		'      end').
	self assert: body size equals: 2.
	self assert: body first entityName equals: 'read'.
	self assert: body first arguments first isWrite.
	self assert: body second entityName equals: 'print'.
	self assert: body second arguments first entityName equals: 'w'.
	self deny: body second arguments first isWrite.
	self assert: body second arguments second class equals: IASTImpliedDo

]

{ #category : 'tests' }
F77IASTParserTest >> testStatementFunction [
	| body fct |
	body := self unitBody: #(
		'      program p'
		'      f(x, y) = x * y + z'
		'      w = f(1.0, a)'
		'      end').
	fct := body first.
	self assert: fct class equals: IASTStatementFunction.
	self assert: fct entityName equals: 'f'.
	self assert: (fct parameters collect: #class) asArray equals: { IASTParameter. IASTParameter }.
	self assert: (fct parameters collect: #entityName) asArray equals: #('x' 'y').
	self assert: (fct body collect: #entityName) asArray equals: #('x' 'y' 'z').
	self assert: body second second class equals: IASTInvocation.
	self assert: body second second entityName equals: 'f'

]

{ #category : 'tests' }
F77IASTParserTest >> testStringVariableDeclaration [
	| stmt |
	stmt := (self unitBody: #(
		'      subroutine hello'
		'      character*8 str, s2*4, s3'
		'      end')) first.
	self assert: (stmt collect: #entityName) asArray equals: #('str' 's2' 's3').
	self assert: (stmt collect: [ :each | each typeSpec entityName ]) asArray equals: #('character' 'character' 'character').
	self assert: (stmt collect: [ :each | each typeSpec length ]) asArray equals: #(8 4 8)

]

{ #category : 'tests' }
F77IASTParserTest >> testSubroutineInvocation [
	| stmt |
	stmt := (self unitBody: #(
		'      subroutine hello'
		'      call someSubRoutine'
		'      end')) first.
	self assert: stmt class equals: IASTInvocation.
	self assert: stmt entityName equals: 'somesubroutine'.
	self assert: stmt arguments isEmpty.
	self assertAnchor: stmt sourceAnchor is: #(2 7 2 26)

]

{ #category : 'tests' }
F77IASTParserTest >> testSubroutineInvocationWithArguments [
	| stmt |
	stmt := (self unitBody: #(
		'      subroutine hello'
		'      call someSubRoutine(bar,5,baz, *10, f(x))'
		'   10 end')) first.
	self assert: stmt class equals: IASTInvocation.
	self assert: stmt entityName equals: 'somesubroutine'.
	self assert: (stmt arguments collect: #class) asArray equals: { IASTVarAccess. IASTVarAccess. IASTInvocation }.
	self assert: (stmt arguments collect: #entityName) asArray equals: #('bar' 'baz' 'f')

]

{ #category : 'tests' }
F77IASTParserTest >> testSubroutineWithAlternateReturn [
	| sub |
	sub := (self parseLines: #(
		'      SUBROUTINE HELLO(A, *, B)'
		'      END')) body first.
	self assert: (sub parameters collect: #entityName) asArray equals: #('a' 'b')

]

{ #category : 'tests' }
F77IASTParserTest >> testSubstringIsNotAnInvocation [
	| stmt |
	stmt := (self unitBody: #(
		'      program p'
		'      character*10 s'
		'      x = s(i:j) // t(2:)'
		'      end')) second.
	self assert: (stmt collect: #class) asArray equals: { IASTVarAccess. IASTVarAccess. IASTVarAccess }.
	self assert: (stmt allButFirst collect: #entityName) asArray equals: #('s' 't').
	self assert: (stmt second indices first collect: #entityName) asArray equals: #('i' 'j')

]

{ #category : 'tests' }
F77IASTParserTest >> testSubstringOfArrayElement [
	| stmt |
	stmt := (self unitBody: #(
		'      program p'
		'      x = s(k)(i:j)'
		'      end')) first.
	self assert: (stmt collect: #entityName) asArray equals: #('x' 's' 'i' 'j').
	self assert: stmt second class equals: IASTVarAccess.
	self assert: stmt second indices first entityName equals: 'k'

]

{ #category : 'tests' }
F77IASTParserTest >> testTwoIntegerVariableDeclarations [
	| stmt |
	stmt := (self unitBody: #(
		'      subroutine hello'
		'      integer i, j'
		'      end')) first.
	self assert: (stmt collect: #entityName) asArray equals: #('i' 'j').
	self assert: stmt first typeSpec identicalTo: stmt second typeSpec

]

{ #category : 'tests' }
F77IASTParserTest >> testTypeLength [
	| fct |
	fct := (self parseLines: #(
		'      REAL*8 FUNCTION F(X)'
		'      END')) body first.
	self assert: fct returnType length equals: 8.
	self assertAnchor: fct returnType sourceAnchor is: #(1 7 1 13).
	fct := (self parseLines: #(
		'      CHARACTER*(*) FUNCTION F(X)'
		'      END')) body first.
	self assert: fct returnType length isNil

]

{ #category : 'tests' }
F77IASTParserTest >> testWriteStatementWithFormat [
	| stmt |
	stmt := (self unitBody: #(
		'      subroutine mysub'
		'      write(unit=1,fmt=var) ''msg'''
		'      end')) first.
	self assert: stmt arguments isEmpty.
	self assert: (stmt format collect: #entityName) asArray equals: #('var').
	self deny: stmt format first isWrite

]

{ #category : 'tests' }
F77IASTParserTest >> testWriteStatementWithFormatSeveralAcesses [
	| stmt |
	stmt := (self unitBody: #(
		'      subroutine mysub'
		'      write(unit=f,fmt=var) ''msg'''
		'      end')) first.
	self assert: (stmt format collect: #entityName) asArray equals: #('f' 'var').
	self assert: (stmt format noneSatisfy: #isWrite)

]

{ #category : 'tests' }
F77IASTParserTest >> testWriteStatementWithStringArgument [
	| stmt |
	stmt := (self unitBody: #(
		'      subroutine print'
		'       write(*,*) ''hello'''
		'      end')) first.
	self assert: stmt class equals: IASTIOStatement.
	self assert: stmt entityName equals: 'write'.
	self assert: stmt arguments isEmpty.
	self assert: stmt format isEmpty.
	self assertAnchor: stmt sourceAnchor is: #(2 8 2 26)

]

{ #category : 'tests' }
F77IASTParserTest >> testWriteStatementWithVarArguments [
	| stmt arg |
	stmt := (self unitBody: #(
		'      subroutine mysub (msg)'
		'      write(*,*) msg'
		'      end')) first.
	self assert: stmt class equals: IASTIOStatement.
	self assert: stmt arguments size equals: 1.
	self assert: stmt format isEmpty.
	self assertAnchor: stmt sourceAnchor is: #(2 7 2 21).
	arg := stmt arguments first.
	self assert: arg class equals: IASTVarAccess.
	self assert: arg entityName equals: 'msg'.
	self deny: arg isWrite

]

{ #category : 'helpers' }
F77IASTParserTest >> unitBody: anArray [
	"body of the only program unit of the source lines"

	^ (self parseLines: anArray) body first body

]
