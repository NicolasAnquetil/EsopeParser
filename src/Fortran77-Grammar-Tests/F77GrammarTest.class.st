"
I test F77Grammar. Program-level tests build fixed-form sources with #lines: so columns stay readable.
"
Class {
	#name : 'F77GrammarTest',
	#superclass : 'PP2CompositeNodeTest',
	#category : 'Fortran77-Grammar-Tests',
	#package : 'Fortran77-Grammar-Tests'
}

{ #category : 'helpers' }
F77GrammarTest >> lines: anArray [
	^ String lf join: anArray

]

{ #category : 'accessing' }
F77GrammarTest >> parserClass [
	^ F77Grammar

]

{ #category : 'helpers' }
F77GrammarTest >> reject: aString rule: aSymbol [
	self assert: ((self parserInstanceFor: aSymbol) end parse: aString) isPetit2Failure

]

{ #category : 'helpers' }
F77GrammarTest >> statementsAccepted: anArray [
	"each string is a statement placed in column 7 of a one-statement main program"
	anArray do: [ :each |
		self parse: (self lines: { '      ' , each. '      END' }) rule: #start ]

]

{ #category : 'helpers' }
F77GrammarTest >> statementsRejected: anArray [
	anArray do: [ :each |
		self fail: (self lines: { '      ' , each. '      END' }) rule: #start ]

]

{ #category : 'tests-expressions' }
F77GrammarTest >> testArithmeticExpressions [
	#('1' 'X' '-X' '+1.5' 'A + B' 'A - B * C' 'A / B / C' 'A ** B ** C' 'A ** -1'
	  '(A + B) * C' '-(A)' 'A*.5' 'X**2+Y**2' '(1.0, 2.0) * Z') do: [ :each |
		self parse: each rule: #expression ]

]

{ #category : 'tests-control' }
F77GrammarTest >> testAssignmentStatement [
	self statementsAccepted: #('X = 1' 'A(I, J) = B(J, I) * 2.0' 'S(1:3) = ''abc''' 'L = A .GT. B' 'IF(I) = 3' 'DO10I = 1' 'F(X) = X**2').
	self statementsRejected: #('X =' '= 1' 'X = 1 2' '1 = X')

]

{ #category : 'tests-expressions' }
F77GrammarTest >> testBadExpressionsRejected [
	#('A +' '(A' 'A)' 'A .FOO. B' 'A **' '* A' 'A ++ B' 'A .EQ. B .EQ. C' 'A(1' 'A B' '1A' 'A+-B') do: [ :each |
		self reject: each rule: #expression ]

]

{ #category : 'tests-layout' }
F77GrammarTest >> testBadLabelRejected [
	self fail: (self lines: #('1A    CONTINUE' '      END')) rule: #start

]

{ #category : 'tests-layout' }
F77GrammarTest >> testBlankLines [
	self parse: (self lines: #(
		''
		'      CONTINUE'
		'   '
		''
		'      END'
		'')) rule: #start

]

{ #category : 'tests-units' }
F77GrammarTest >> testBlockDataStatement [
	#('BLOCK DATA' 'BLOCKDATA' 'BLOCK DATA INIT') do: [ :each |
		self parse: each rule: #blockDataStatement ]

]

{ #category : 'tests-control' }
F77GrammarTest >> testBlockIf [
	self parse: (self lines: #(
		'      IF (X .GT. 0) THEN'
		'         Y = 1'
		'      ELSE IF (X .LT. 0) THEN'
		'         Y = -1'
		'      ELSEIF (X .EQ. 0) THEN'
		'      ELSE'
		'         IF (Z) THEN'
		'            Y = 0'
		'         ENDIF'
		'      END IF'
		'      END')) rule: #start.
	self parse: (self lines: #(
		'      IF (L) THEN'
		'C comment'
		'      ENDIF'
		'      END')) rule: #start

]

{ #category : 'tests-control' }
F77GrammarTest >> testCallStatement [
	#('CALL S' 'CALL S()' 'CALL S(A, B(1), 1.0 + X)' 'CALL S(A, *10, *20)' 'CALL S(5HHELLO, ''world'')') do: [ :each |
		self parse: each rule: #callStatement ].
	self statementsRejected: #('CALL' 'CALL S(' 'CALL 1')

]

{ #category : 'tests-layout' }
F77GrammarTest >> testCaseInsensitiveKeyword [
	self parse: (self lines: #('      continue' '      End')) rule: #start

]

{ #category : 'tests-lexical' }
F77GrammarTest >> testCharacterConstant [
	self parse: '''hello''' rule: #characterConstant.
	self parse: '''''' rule: #characterConstant.
	self parse: '''it''''s''' rule: #characterConstant.
	self parse: '"double"' rule: #characterConstant.
	self reject: '''unterminated' rule: #characterConstant.
	self reject: '''two' , String lf , 'lines''' rule: #characterConstant

]

{ #category : 'tests-expressions' }
F77GrammarTest >> testCharacterExpressions [
	#('''abc'' // S' 'A // B // C(1:2)') do: [ :each |
		self parse: each rule: #expression ]

]

{ #category : 'tests-layout' }
F77GrammarTest >> testColumn6Zero [
	self parse: '     0END' rule: #start

]

{ #category : 'tests-layout' }
F77GrammarTest >> testCommentLines [
	self parse: (self lines: #(
		'C a comment'
		'c another one'
		'* star comment'
		'! bang comment'
		'      CONTINUE'
		'   ! indented bang comment'
		'D     CONTINUE'
		'      END'
		'C trailing comment')) rule: #start

]

{ #category : 'tests-integration' }
F77GrammarTest >> testCommentsInsideContinuedStatement [
	self parse: (self lines: #(
		'      X = A +'
		'C comment between an initial line and its continuation'
		''
		'     1    B +'
		'     2    C'
		'      END')) rule: #start

]

{ #category : 'tests-specification' }
F77GrammarTest >> testCommonStatement [
	#('COMMON A, B' 'COMMON /BLK/ A, B(10)' 'COMMON // A' 'COMMON / / A' 'COMMON /B1/ X /B2/ Y, Z' 'COMMON A /B2/ Y, /B3/ Z') do: [ :each |
		self parse: each rule: #commonStatement ].
	#('COMMON' 'COMMON /BLK/' 'COMMON /BLK A') do: [ :each |
		self reject: each rule: #commonStatement ]

]

{ #category : 'tests-lexical' }
F77GrammarTest >> testComplexConstant [
	self parse: '(1.0, 2.0)' rule: #complexConstant.
	self parse: '(-1, +2.5E3)' rule: #complexConstant.
	self reject: '(X, 1)' rule: #complexConstant

]

{ #category : 'tests-layout' }
F77GrammarTest >> testContinuationLine [
	self parse: (self lines: #(
		'      CONTINUE'
		'     &'
		'      END')) rule: #start.
	self parse: (self lines: #(
		'      CONTINUE'
		'C comment between'
		''
		'     1'
		'      END')) rule: #start

]

{ #category : 'tests-layout' }
F77GrammarTest >> testContinuationNotBlankLabelRejected [
	self fail: (self lines: #('      CONTINUE' '1    &' '      END')) rule: #start

]

{ #category : 'tests-layout' }
F77GrammarTest >> testContinuationWithoutStatementRejected [
	self fail: (self lines: #('     &CONTINUE' '      END')) rule: #start

]

{ #category : 'tests-lexical' }
F77GrammarTest >> testContinuedCharacterConstant [
	| tokens |
	self parse: '''abc' , String lf , '     &def''' rule: #characterConstant.
	tokens := self tokensIn: ((self parserInstanceFor: #characterConstant) parse: '''abc' , String lf , '     &def''').
	self assert: tokens first start equals: 1.
	self assert: tokens first stop equals: 15

]

{ #category : 'tests-control' }
F77GrammarTest >> testContinuedStatementTokenPositions [
	| source tokens |
	source := self lines: #('      X = A +' '     &    2' '      END').
	tokens := self tokensIn: (self parse: source rule: #start).
	self assert: (tokens collect: [ :t | t value ]) asArray equals: #('X' '=' 'A' '+' '2' 'END').
	self assert: (tokens at: 5) start equals: (source indexOf: $2).
	self assert: (tokens at: 5) stop equals: (source indexOf: $2)

]

{ #category : 'tests-layout' }
F77GrammarTest >> testCrLf [
	self parse: '      CONTINUE', String crlf, '      END', String crlf rule: #start

]

{ #category : 'tests-specification' }
F77GrammarTest >> testDataStatement [
	#('DATA A /1.0/' 'DATA A, B /1, 2/' 'DATA A /1/, B /2/' 'DATA A /1/ B /2/' 'DATA X /10*0.0/'
	  'DATA (A(I), I = 1, 10) /10*0/' 'DATA ((M(I, J), I = 1, 2), J = 1, 2) /4*1/'
	  'DATA S /''abc''/, L /.TRUE./, Z /(1.0, -1.0)/' 'DATA N /-5/, H /4HABCD/' 'DATA A /N*PI/') do: [ :each |
		self parse: each rule: #dataStatement ].
	#('DATA A' 'DATA A /1' 'DATA /1/' 'DATA A //') do: [ :each |
		self reject: each rule: #dataStatement ]

]

{ #category : 'tests-expressions' }
F77GrammarTest >> testDesignators [
	#('A(1)' 'A(I, J+1, K)' 'F()' 'SQRT(X*X)' 'C(1:3)' 'C(:N)' 'C(2:)' 'C(:)' 'S(I)(1:2)' 'MAX(A, B, C)') do: [ :each |
		self parse: each rule: #expression ]

]

{ #category : 'tests-specification' }
F77GrammarTest >> testDimensionStatement [
	self parse: 'DIMENSION A(10), B(2, 3)' rule: #dimensionStatement.
	self reject: 'DIMENSION A' rule: #dimensionStatement

]

{ #category : 'tests-control' }
F77GrammarTest >> testDoEnddo [
	self parse: (self lines: #(
		'      do i=1,n'
		'        do while (j .lt. i)'
		'          j = j + 1'
		'        end do'
		'      enddo'
		'      DO 10 I = 1, N'
		'   10 CONTINUE'
		'      END')) rule: #start.
	self fail: (self lines: #('      do i=1,n' '      END')) rule: #start.
	self fail: (self lines: #('      enddo' '      END')) rule: #start

]

{ #category : 'tests-control' }
F77GrammarTest >> testDoLoopProgram [
	self parse: (self lines: #(
		'      DO 10 I = 1, N'
		'         DO 20 J = 1, M'
		'            A(I, J) = 0.0'
		'   20    CONTINUE'
		'   10 CONTINUE'
		'      IF (N) 30, 30, 40'
		'   30 STOP'
		'   40 RETURN'
		'      END')) rule: #start

]

{ #category : 'tests-control' }
F77GrammarTest >> testDoStatement [
	#('DO 10 I = 1, 10' 'DO 10, I = 1, N, 2' 'DO 100 X = 0.0, 1.0, 0.1') do: [ :each |
		self parse: each rule: #doStatement ].
	self statementsRejected: #('DO 10 I = 1' 'DO I = 1, 10' 'DO 10 I')

]

{ #category : 'tests-layout' }
F77GrammarTest >> testEmptySourceRejected [
	self fail: '' rule: #start

]

{ #category : 'tests-units' }
F77GrammarTest >> testEndWithUnitKind [

	#( 'END' 'END PROGRAM' 'END PROGRAM MAIN' 'ENDPROGRAM MAIN'
	   'END SUBROUTINE' 'END SUBROUTINE S' 'end subroutine s'
	   'END FUNCTION F' 'ENDFUNCTION' 'END BLOCK DATA' 'END BLOCKDATA INIT'
	   'ENDBLOCKDATA' ) do: [ :each |
		self parse: each rule: #endStatement ].
	#( 'END PROGRAM MAIN X' 'END MAIN' 'END PROGRAM 1' 'END MODULE M' )
		do: [ :each | self reject: each rule: #endStatement ].
	self
		parse: (self lines:
				 #( '      PROGRAM MAIN' '      CALL S' '      END PROGRAM MAIN'
				    '      SUBROUTINE S' '      END SUBROUTINE S'
				    '      REAL FUNCTION F(X)' '      F = X' '      END FUNCTION'
				    '      BLOCK DATA B' '      END BLOCK DATA B' ))
		rule: #start
]

{ #category : 'tests-specification' }
F77GrammarTest >> testEntryAndInclude [
	self parse: 'ENTRY E' rule: #entryStatement.
	self parse: 'ENTRY E(A, *)' rule: #entryStatement.
	self parse: 'INCLUDE ''common.inc''' rule: #includeStatement

]

{ #category : 'tests-specification' }
F77GrammarTest >> testEquivalenceStatement [
	self parse: 'EQUIVALENCE (A, B)' rule: #equivalenceStatement.
	self parse: 'EQUIVALENCE (A(1), B(2), C), (X, Y(3))' rule: #equivalenceStatement.
	self reject: 'EQUIVALENCE (A)' rule: #equivalenceStatement

]

{ #category : 'tests-control' }
F77GrammarTest >> testExitAndCycle [
	self parse: (self lines: #(
		'      do i = 1, n'
		'         if (a(i) .eq. 0) then'
		'            exit'
		'         end if'
		'         if (a(i) .lt. 0) cycle'
		'         CYCLE'
		'      end do'
		'      END')) rule: #start.
	self statementsAccepted: #('EXIT = 1' 'CYCLE(2) = 3').
	self statementsRejected: #('EXIT 10' 'CYCLE X')

]

{ #category : 'tests-expressions' }
F77GrammarTest >> testExpressionTokenPositions [
	| tokens |
	tokens := self tokensIn: ((self parserInstanceFor: #expression) parse: 'A .EQ. 1.5').
	self assert: (tokens collect: [ :t | t start -> t stop ]) asArray
		equals: { 1 -> 1. 3 -> 6. 8 -> 10 }

]

{ #category : 'tests-specification' }
F77GrammarTest >> testExternalIntrinsicSave [
	self parse: 'EXTERNAL F, G' rule: #externalStatement.
	self parse: 'INTRINSIC SIN, COS' rule: #intrinsicStatement.
	self parse: 'SAVE' rule: #saveStatement.
	self parse: 'SAVE A, /BLK/, B' rule: #saveStatement.
	self reject: 'EXTERNAL' rule: #externalStatement

]

{ #category : 'tests-io' }
F77GrammarTest >> testFileStatements [
	self statementsAccepted: #('OPEN (UNIT=10, FILE=''data.txt'', STATUS=''OLD'', ERR=99)' 'CLOSE (10)' 'CLOSE (UNIT=10, STATUS=''DELETE'')'
		'INQUIRE (FILE=NAME, EXIST=EX)' 'BACKSPACE 10' 'BACKSPACE (UNIT=10)' 'REWIND 10' 'REWIND (10, ERR=99)' 'ENDFILE 10' 'END FILE (10)').
	self statementsRejected: #('OPEN' 'OPEN 10' 'REWIND')

]

{ #category : 'tests-io' }
F77GrammarTest >> testFormatInProgram [
	self parse: (self lines: #(
		'      WRITE (6, 100) N, X'
		'  100 FORMAT (1X, ''N = '', I5,'
		'     &        '' X = '', F10.3)'
		'      END')) rule: #start

]

{ #category : 'tests-io' }
F77GrammarTest >> testFormatStatement [
	#('FORMAT (I5)' 'FORMAT (1X, I5, F10.3, E12.4E2, 1PE12.4, D20.10, G12.5, L2, A, A10)'
	  'FORMAT (''X = '', F5.2 / 2(I3, 2X), T10, TL2, TR3)' 'FORMAT (BN, BZ, SP, SS, S, -2P, :)'
	  'FORMAT (5HHELLO, 3X, 2(1X, 3(I2, 1X)))' 'FORMAT (I5//F3.1)' 'FORMAT ()' 'FORMAT (A, $)') do: [ :each |
		self parse: each rule: #formatStatement ].
	#('FORMAT' 'FORMAT (I5' 'FORMAT I5' 'FORMAT (2(I5)') do: [ :each |
		self reject: each rule: #formatStatement ]

]

{ #category : 'tests-units' }
F77GrammarTest >> testFunctionStatement [
	#('FUNCTION F()' 'FUNCTION F(X)' 'REAL FUNCTION F(X, Y)' 'DOUBLE PRECISION FUNCTION F(X)'
	  'CHARACTER*(*) FUNCTION F(S)' 'INTEGER*4 FUNCTION F(I)') do: [ :each |
		self parse: each rule: #functionStatement ].
	#('FUNCTION F' 'REAL FUNCTION' 'FUNCTION F(X, *)') do: [ :each |
		self reject: each rule: #functionStatement ]

]

{ #category : 'tests-control' }
F77GrammarTest >> testGotoStatements [
	self parse: 'GO TO 10' rule: #gotoStatement.
	self parse: 'GOTO 10' rule: #gotoStatement.
	self parse: 'GO TO (10, 20, 30), I' rule: #gotoStatement.
	self parse: 'GO TO (10, 20) I+1' rule: #gotoStatement.
	self parse: 'GO TO L' rule: #gotoStatement.
	self parse: 'GO TO L, (10, 20)' rule: #gotoStatement.
	self parse: 'ASSIGN 10 TO L' rule: #assignStatement.
	self statementsRejected: #('GO TO' 'GO TO (10, 20)' 'ASSIGN L TO 10')

]

{ #category : 'tests-units' }
F77GrammarTest >> testHeaderAfterStatementRejected [
	self fail: (self lines: #(
		'      CONTINUE'
		'      SUBROUTINE S'
		'      END')) rule: #start

]

{ #category : 'tests-lexical' }
F77GrammarTest >> testHollerithConstant [
	self parse: '3HABC' rule: #hollerithConstant.
	self parse: '5hA B,C' rule: #hollerithConstant.
	self reject: '0H' rule: #hollerithConstant.
	self reject: '4HABC' rule: #hollerithConstant.
	self reject: '3HABCD' rule: #hollerithConstant

]

{ #category : 'tests-control' }
F77GrammarTest >> testIfStatements [
	self parse: 'IF (X) 10, 20, 30' rule: #arithmeticIfStatement.
	self parse: 'IF (X .GT. 0) Y = 1' rule: #logicalIfStatement.
	self parse: 'IF (X .GT. 0) GO TO 10' rule: #logicalIfStatement.
	self parse: 'IF (X .GT. 0) CALL S(X)' rule: #logicalIfStatement.
	self statementsAccepted: #('IF (X) 10, 20, 30' 'IF (L) RETURN' 'IF (A .EQ. B) STOP').
	self statementsRejected: #('IF (X) 10, 20' 'IF X Y = 1' 'IF (L) IF (M) X = 1' 'IF (L) DO 10 I = 1, 2' 'IF (L) END')

]

{ #category : 'tests-specification' }
F77GrammarTest >> testImplicitStatement [
	#('IMPLICIT NONE' 'IMPLICIT REAL (A-H)' 'IMPLICIT DOUBLE PRECISION (A-H, O-Z)'
	  'IMPLICIT INTEGER (I-N), LOGICAL (L)' 'IMPLICIT REAL*8 (A-H,O-Z)' 'IMPLICIT CHARACTER*10 (C)') do: [ :each |
		self parse: each rule: #implicitStatement ].
	#('IMPLICIT' 'IMPLICIT REAL (AB-C)' 'IMPLICIT REAL A-H') do: [ :each |
		self reject: each rule: #implicitStatement ]

]

{ #category : 'tests-include' }
F77GrammarTest >> testIncludeFileContents [
	self parse: (self lines: #(
		'C common block definition'
		'      INTEGER NMAX'
		'      PARAMETER (NMAX = 100)'
		'      REAL A(NMAX)'
		'      COMMON /WORK/ A'
		'      DATA A /NMAX*0.0/'
		''
		'      IF (DEBUG) THEN'
		'         PRINT *, ''included'''
		'      END IF'
		'      DO I = 1, 10'
		'      ENDDO')) rule: #includeFile.
	self parse: '' rule: #includeFile.
	self parse: (self lines: #('C only comments' '')) rule: #includeFile

]

{ #category : 'tests-include' }
F77GrammarTest >> testIncludeFileImplicit [
	"content of Fortran77/implicit.for"
	self parse: (self lines: #(
		'      IMPLICIT LOGICAL (A,B,C,D,E,F,G,H)'
		'      IMPLICIT LOGICAL (I,J,J,K,L,M,N)'
		'      IMPLICIT LOGICAL (O,P,Q,R,S,T)'
		'      IMPLICIT LOGICAL (U,V,W,X,Y,Z)'
		'')) rule: #includeFile.
	self fail: (self lines: #('      IMPLICIT LOGICAL (A-Z)')) rule: #start

]

{ #category : 'tests-include' }
F77GrammarTest >> testIncludeFileRejected [
	"no END nor header in a fragment"
	self fail: (self lines: #('      INTEGER I' '      END')) rule: #includeFile.
	self fail: (self lines: #('      SUBROUTINE S' '      INTEGER I')) rule: #includeFile.
	"unbalanced constructs"
	self fail: (self lines: #('      IF (L) THEN' '      X = 1')) rule: #includeFile.
	self fail: (self lines: #('      X = 1' '      END IF')) rule: #includeFile.
	"syntax errors"
	self fail: (self lines: #('      INTEGER' '      X = 1')) rule: #includeFile.
	self fail: (self lines: #('  X = 1')) rule: #includeFile

]

{ #category : 'tests-include' }
F77GrammarTest >> testIncludeFileStartingAt [
	self deny: ((self parserClass newStartingAt: #includeFile) parse: '      INTEGER I') isPetit2Failure.
	self assert: ((self parserClass newStartingAt: #includeFile) parse: '      INTEGER') isPetit2Failure

]

{ #category : 'tests-lexical' }
F77GrammarTest >> testIntegerConstant [
	self parse: '0' rule: #integerConstant.
	self parse: '12345' rule: #integerConstant.
	self reject: '12A' rule: #integerConstant

]

{ #category : 'tests-io' }
F77GrammarTest >> testIoInLogicalIf [
	self statementsAccepted: #('IF (DEBUG) WRITE (6, *) X')

]

{ #category : 'tests-layout' }
F77GrammarTest >> testLabels [
	self parse: (self lines: #(
		'10    CONTINUE'
		'  200 CONTINUE'
		'99999 CONTINUE'
		'      END')) rule: #start

]

{ #category : 'tests-layout' }
F77GrammarTest >> testLeadingBlanksInStatement [
	self parse: (self lines: #('         CONTINUE   ' '  10  END')) rule: #start

]

{ #category : 'tests-lexical' }
F77GrammarTest >> testLogicalConstant [
	self parse: '.TRUE.' rule: #logicalConstant.
	self parse: '.false.' rule: #logicalConstant.
	self reject: '.TRUE' rule: #logicalConstant

]

{ #category : 'tests-expressions' }
F77GrammarTest >> testLogicalExpressions [
	#('A .EQ. B' 'I.LT.10' '1.EQ.2' 'X.GE.1.5' 'A .and. B .or. .not. C' '.NOT. .NOT. L'
	  'A .EQV. B .NEQV. C' '.TRUE.' '(I .GT. 0) .AND. (J .LE. N)' 'S // T .NE. ''XY''') do: [ :each |
		self parse: each rule: #expression ]

]

{ #category : 'tests-io' }
F77GrammarTest >> testLongFormatStatement [
	self parse: ('FORMAT (I5' padRightTo: 72) , ')' rule: #formatStatement

]

{ #category : 'tests-layout' }
F77GrammarTest >> testLongLines [
	"lines are not limited to 72 columns: everything is significant"
	self parse: (self lines: {
		('      X = 1' padRightTo: 72) , '+ 2'.
		('      END' padRightTo: 100) }) rule: #start.
	self fail: (self lines: {
		('      CONTINUE' padRightTo: 72) , 'ABCDEFGH'.
		'      END' }) rule: #start

]

{ #category : 'tests-integration' }
F77GrammarTest >> testMalformedProgramsRejected [
	"statement in columns 1-6"
	self fail: (self lines: #('  X = 1' '      END')) rule: #start.
	"continuation of nothing"
	self fail: (self lines: #('C comment' '     &X = 1' '      END')) rule: #start.
	"unbalanced END IF"
	self fail: (self lines: #('      IF (L) THEN' '      END IF' '      END IF' '      END')) rule: #start.
	"missing END"
	self fail: (self lines: #('      PROGRAM P' '      X = 1')) rule: #start.
	"syntax error inside a statement"
	self fail: (self lines: #('      X = (A + B' '      END')) rule: #start.
	"label too long"
	self fail: (self lines: #('123456 X = 1' '      END')) rule: #start

]

{ #category : 'tests-layout' }
F77GrammarTest >> testMinimalProgram [
	self parse: '      END' rule: #start.
	self parse: (self lines: #('      END' '')) rule: #start

]

{ #category : 'tests-units' }
F77GrammarTest >> testMissingEndRejected [
	self fail: (self lines: #(
		'      SUBROUTINE S'
		'      CONTINUE')) rule: #start

]

{ #category : 'tests-lexical' }
F77GrammarTest >> testName [
	self parse: 'X' rule: #name.
	self parse: 'abc123' rule: #name.
	self parse: 'MY_VAR$' rule: #name.
	self reject: '1X' rule: #name.
	self reject: '_X' rule: #name

]

{ #category : 'tests-lexical' }
F77GrammarTest >> testOperatorTokens [
	self parse: '*' rule: #star.
	self reject: '**' rule: #star.
	self parse: '**' rule: #power.
	self parse: '/' rule: #slash.
	self reject: '//' rule: #slash.
	self parse: '//' rule: #concat

]

{ #category : 'tests-specification' }
F77GrammarTest >> testParameterStatement [
	self parse: 'PARAMETER (N = 10)' rule: #parameterStatement.
	self parse: 'PARAMETER (PI = 3.14159, TWOPI = 2*PI, S = ''abc'')' rule: #parameterStatement.
	self reject: 'PARAMETER N = 10' rule: #parameterStatement.
	self reject: 'PARAMETER (N)' rule: #parameterStatement

]

{ #category : 'tests-units' }
F77GrammarTest >> testProgramStatement [
	self parse: 'PROGRAM MAIN' rule: #programStatement.
	self reject: 'PROGRAM' rule: #programStatement.
	self reject: 'PROGRAMMAIN' rule: #programStatement

]

{ #category : 'tests-units' }
F77GrammarTest >> testProgramUnits [
	self parse: (self lines: #(
		'C main program'
		'      PROGRAM MAIN'
		'      CONTINUE'
		'      END'
		'C a subroutine'
		'      SUBROUTINE S(A, B)'
		'      END'
		'      REAL FUNCTION F(X)'
		'      END'
		'      BLOCK DATA'
		'      END')) rule: #start

]

{ #category : 'tests-io' }
F77GrammarTest >> testReadWritePrint [
	#('READ (5, 100) A, B' 'READ (*, *) (A(I), I = 1, N)' 'READ (UNIT=5, FMT=*, END=99) X' 'READ 100, A' 'READ *, A, B'
	  'READ (5, *)' 'READ (U, ''(A)'', IOSTAT=IOS) LINE') do: [ :each |
		self parse: each rule: #readStatement ].
	#('WRITE (6, 100) A, B + 1' 'WRITE (6, *) ''X ='', X' 'WRITE (*, ''(I5)'') ((M(I, J), I = 1, 2), J = 1, 2)' 'WRITE (6, 100)' 'WRITE (UNIT=6, FMT=*) X') do: [ :each |
		self parse: each rule: #writeStatement ].
	#('PRINT *, X' 'PRINT 100' 'PRINT ''(A)'', ''hello''' 'PRINT *, (A(I), I = 1, 10, 2)') do: [ :each |
		self parse: each rule: #printStatement ].
	self statementsRejected: #('READ' 'WRITE 6, X' 'WRITE (6, *) X,' 'PRINT' 'READ (5, *) (A(I), I = 1)')

]

{ #category : 'tests-lexical' }
F77GrammarTest >> testRealConstant [
	#('1.' '1.5' '.5' '1.0E-3' '1E5' '1D0' '1.5d+10' '3.Q0') do: [ :each |
		self parse: each rule: #realConstant ].
	#('1' '.' 'E5' '1.EQ.' '1.5E') do: [ :each |
		self reject: each rule: #realConstant ]

]

{ #category : 'tests-integration' }
F77GrammarTest >> testRealisticFunctionAndBlockData [
	self parse: (self lines: #(
		'      CHARACTER*(*) FUNCTION TRIM2(S)'
		'      CHARACTER*(*) S'
		'      INTEGER I'
		'      DO 10 I = LEN(S), 1, -1'
		'         IF (S(I:I) .NE. '' '') GOTO 20'
		'   10 CONTINUE'
		'   20 TRIM2 = S(1:I)'
		'      END'
		'*'
		'      LOGICAL FUNCTION ISODD(N)'
		'      ISODD = MOD(N, 2) .EQ. 1'
		'      END'
		'*'
		'      BLOCK DATA INIT'
		'      INTEGER TABLE(5)'
		'      COMMON /TAB/ TABLE'
		'      DATA TABLE /1, 2, 3,'
		'     +            4, 5/'
		'      END')) rule: #start

]

{ #category : 'tests-integration' }
F77GrammarTest >> testRealisticProgram [
	self parse: (self lines: #(
		'C     ------------------------------------------------------------'
		'C     Solve a linear system by Gaussian elimination'
		'C     ------------------------------------------------------------'
		'      PROGRAM GAUSS'
		'      IMPLICIT NONE'
		'      INTEGER NMAX'
		'      PARAMETER (NMAX = 10)'
		'      DOUBLE PRECISION A(NMAX, NMAX), B(NMAX), X(NMAX)'
		'      INTEGER N, I, J, IERR'
		'      CHARACTER*20 TITLE'
		'      COMMON /SIZES/ N'
		'      DATA TITLE /''Gauss elimination''/'
		''
		'      N = 3'
		'      DO 20 I = 1, N'
		'         DO 10 J = 1, N'
		'            IF (I .EQ. J) THEN'
		'               A(I, J) = 2.0D0'
		'            ELSE IF (ABS(I - J) .EQ. 1) THEN'
		'               A(I, J) = -1.0D0'
		'            ELSE'
		'               A(I, J) = 0.0D0'
		'            END IF'
		'   10    CONTINUE'
		'         B(I) = 1.0D0'
		'   20 CONTINUE'
		'      CALL SOLVE(A, B, X, N, NMAX, IERR, *900)'
		'      WRITE (6, 100) TITLE, (X(I), I = 1, N)'
		'  100 FORMAT (1X, A20 / (1X, 3F12.6))'
		'      STOP'
		'  900 WRITE (6, *) ''Singular matrix, error code'','
		'     &   IERR'
		'      END'
		''
		'      SUBROUTINE SOLVE(A, B, X, N, LDA, IERR, *)'
		'      INTEGER N, LDA, IERR, I, J, K'
		'      DOUBLE PRECISION A(LDA, *), B(*), X(*), F, S'
		'      IERR = 0'
		'      DO 30 K = 1, N - 1'
		'         IF (A(K, K) .EQ. 0.0D0) GO TO 99'
		'         DO 30 I = K + 1, N'
		'            F = A(I, K) / A(K, K)'
		'            DO 25 J = K, N'
		'   25       A(I, J) = A(I, J) - F * A(K, J)'
		'            B(I) = B(I) - F * B(K)'
		'   30 CONTINUE'
		'      DO 50 I = N, 1, -1'
		'         S = B(I)'
		'         DO 40 J = I + 1, N'
		'            S = S - A(I, J) * X(J)'
		'   40    CONTINUE'
		'         X(I) = S / A(I, I)'
		'   50 CONTINUE'
		'      RETURN'
		'   99 IERR = K'
		'      RETURN 1'
		'      END')) rule: #start

]

{ #category : 'tests-layout' }
F77GrammarTest >> testSeveralProgramUnits [
	self parse: (self lines: #('      END' 'C second' '      END')) rule: #start

]

{ #category : 'tests-control' }
F77GrammarTest >> testSimpleStatements [
	self statementsAccepted: #('CONTINUE' 'RETURN' 'RETURN 1' 'STOP' 'STOP 1' 'STOP ''done''' 'PAUSE' 'PAUSE 99').
	self statementsRejected: #('CONTINUE 1' 'STOP X')

]

{ #category : 'tests-specification' }
F77GrammarTest >> testSpecificationPart [
	self parse: (self lines: #(
		'      SUBROUTINE S(A, N)'
		'      IMPLICIT NONE'
		'      INTEGER N, I'
		'      REAL A(N), B(100)'
		'      PARAMETER (M = 100)'
		'      COMMON /WORK/ B'
		'      DATA I /0/'
		'      SAVE I'
		'      END')) rule: #start

]

{ #category : 'tests-layout' }
F77GrammarTest >> testStatementInColumn6Rejected [
	self fail: '     END' rule: #start.
	self fail: 'END' rule: #start

]

{ #category : 'tests-units' }
F77GrammarTest >> testSubroutineStatement [
	#('SUBROUTINE S' 'SUBROUTINE S()' 'SUBROUTINE S(A)' 'SUBROUTINE S(A, B, C)' 'SUBROUTINE S(A, *, *)') do: [ :each |
		self parse: each rule: #subroutineStatement ].
	#('SUBROUTINE' 'SUBROUTINE S(' 'SUBROUTINE S(A,)' 'SUBROUTINE S(1)') do: [ :each |
		self reject: each rule: #subroutineStatement ]

]

{ #category : 'tests-control' }
F77GrammarTest >> testTabFormat [
	| tab |
	tab := String with: Character tab.
	self parse: (self lines: {
		tab , 'x = 1'.
		'10' , tab , 'continue'.
		tab , 'y = 2 +'.
		tab , '1 3'.
		'      END' }) rule: #start

]

{ #category : 'tests-layout' }
F77GrammarTest >> testTokenPositions [
	| source tokens |
	source := self lines: #('      CONTINUE' '      END').
	tokens := self tokensIn: (self parse: source rule: #start).
	self assert: (tokens collect: [ :t | t start -> t stop ]) asArray
		equals: { 7 -> 14. 22 -> 24 }

]

{ #category : 'tests-units' }
F77GrammarTest >> testTypeSpec [
	#('INTEGER' 'REAL' 'DOUBLE PRECISION' 'DOUBLEPRECISION' 'COMPLEX' 'DOUBLE COMPLEX' 'LOGICAL' 'CHARACTER'
	  'CHARACTER*8' 'CHARACTER*(*)' 'CHARACTER*(N+1)' 'REAL*8' 'integer*4') do: [ :each |
		self parse: each rule: #typeSpec ].
	#('INTEGERX' 'DOUBLE' 'CHARACTER*' 'REAL**8') do: [ :each |
		self reject: each rule: #typeSpec ]

]

{ #category : 'tests-specification' }
F77GrammarTest >> testTypeStatement [
	#('INTEGER I' 'INTEGER I, J, K' 'REAL A(10), B(0:N, *)' 'DOUBLE PRECISION D(N)'
	  'CHARACTER*8 S, T*4, U(10)*2' 'CHARACTER*(*) NAME' 'CHARACTER*10, S' 'LOGICAL FLAG' 'REAL*8 X(-1:1)' 'COMPLEX Z') do: [ :each |
		self parse: each rule: #typeStatement ].
	#('INTEGER' 'INTEGER I,' 'REAL A(' 'REAL A()' 'REAL 1X') do: [ :each |
		self reject: each rule: #typeStatement ]

]

{ #category : 'tests-control' }
F77GrammarTest >> testUnbalancedBlockIfRejected [
	self fail: (self lines: #('      IF (L) THEN' '      X = 1' '      END')) rule: #start.
	self fail: (self lines: #('      X = 1' '      END IF' '      END')) rule: #start.
	self fail: (self lines: #('      ELSE' '      END')) rule: #start.
	self fail: (self lines: #('      IF (L) THEN' '      ELSE' '      ELSE IF (M) THEN' '      END IF' '      END')) rule: #start.
	self fail: (self lines: #('      IF (L) THEN' '      ELSE' '      ELSE' '      END IF' '      END')) rule: #start

]

{ #category : 'helpers' }
F77GrammarTest >> tokensIn: aResult [
	| tokens |
	tokens := OrderedCollection new.
	self tokensIn: aResult into: tokens.
	^ tokens

]

{ #category : 'helpers' }
F77GrammarTest >> tokensIn: anObject into: aCollection [
	(anObject isKindOf: PP2Token) ifTrue: [ ^ aCollection add: anObject ].
	(anObject isCollection and: [ anObject isString not ]) ifTrue: [
		anObject do: [ :each | self tokensIn: each into: aCollection ] ]

]
