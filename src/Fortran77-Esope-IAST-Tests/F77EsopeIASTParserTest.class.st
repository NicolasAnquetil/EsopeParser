Class {
	#name : 'F77EsopeIASTParserTest',
	#superclass : 'F77IASTParserTest',
	#category : 'Fortran77-Esope-IAST-Tests',
	#package : 'Fortran77-Esope-IAST-Tests'
}

{ #category : 'testing' }
F77EsopeIASTParserTest class >> shouldInheritSelectors [
	^ true
]

{ #category : 'accessing' }
F77EsopeIASTParserTest >> parserClass [
	^ F77EsopeIASTParser

]

{ #category : 'tests' }
F77EsopeIASTParserTest >> testBigFileSample [
	| programFile units |
	programFile := self parse: F77EsopeGrammarTest esopeBigFileSample.
	units := programFile body select: [ :each | each isKindOf: IASTProgramUnit ].
	self assert: units notEmpty.
	self assert: (units anySatisfy: [ :unit | unit body flattened anySatisfy: [ :each | each isKindOf: IASTEsopeSegCommand ] ])

]

{ #category : 'tests' }
F77EsopeIASTParserTest >> testDeref [
	| stmt var |
	stmt := (self unitBody: #(
		'      subroutine hello'
		'      ptr.att = 5'
		'      end')) first.
	self assert: stmt size equals: 1.
	var := stmt first.
	self assert: var class equals: IASTVarAccess.
	self assert: var entityName equals: 'ptr'.
	self assert: var isWrite.
	"as FamixEsopeImporter: anchored on the pointer name"
	self assertAnchor: var sourceAnchor is: #(2 7 2 10).
	var := var dereferencedVariable.
	self assert: var class equals: IASTVarAccess.
	self assert: var entityName equals: 'att'.
	self deny: var isWrite.
	self assert: var dereferencedVariable isNil.
	self assertAnchor: var sourceAnchor is: #(2 11 2 14)

]

{ #category : 'tests' }
F77EsopeIASTParserTest >> testDerefFieldAccessWithDim [
	| stmt access |
	stmt := (self unitBody: #(
		'      PROGRAM MYPROG'
		'       var = lb.att(i, 2)'
		'      END')) first.
	self assert: stmt size equals: 2.
	access := stmt second.
	self assert: access class equals: IASTVarAccess.
	self assert: access entityName equals: 'lb'.
	self assert: access dereferencedVariable entityName equals: 'att'.
	self assert: access indices size equals: 2.
	self assert: access indices first entityName equals: 'i'.
	self assert: access indices second isNil

]

{ #category : 'tests' }
F77EsopeIASTParserTest >> testDerefIsNeverAnInvocation [
	| stmt |
	stmt := (self unitBody: #(
		'      PROGRAM MYPROG'
		'       x = lb.att(i) + f(j)'
		'      END')) first.
	self assert: (stmt collect: #class) asArray equals: { IASTVarAccess. IASTVarAccess. IASTInvocation }

]

{ #category : 'tests' }
F77EsopeIASTParserTest >> testDimensionOfComponent [
	"(/N) asks for the Nth dimension of a segment component"

	| access |
	access := (self unitBody: #(
		'      PROGRAM MYPROG'
		'       n = lb.att(/k)'
		'      END')) first second.
	self assert: access dereferencedVariable entityName equals: 'att'.
	self assert: access indices first entityName equals: 'k'

]

{ #category : 'tests' }
F77EsopeIASTParserTest >> testDotOperatorsAreNotComponents [
	| stmt |
	stmt := (self unitBody: #(
		'      PROGRAM MYPROG'
		'       l = p.eq.q .and. .true.'
		'      END')) first.
	self assert: (stmt collect: #entityName) asArray equals: #('l' 'p' 'q').
	self assert: (stmt allSatisfy: [ :each | each dereferencedVariable isNil ])

]

{ #category : 'tests' }
F77EsopeIASTParserTest >> testEsopeInclude [
	| body |
	body := self unitBody: #(
		'      subroutine hello'
		'-INC PPSTR'
		'%inc SMBOOK'
		'      end').
	self assert: (body collect: #class) asArray equals: { IASTInclude. IASTInclude }.
	self assert: (body collect: #includeCommand) asArray equals: #('-INC' '%INC').
	self assert: (body collect: #entityName) asArray equals: #('PPSTR' 'SMBOOK').
	self assertAnchor: body first sourceAnchor is: #(2 1 2 11)

]

{ #category : 'tests' }
F77EsopeIASTParserTest >> testFamixModelWithEsope [
	| model |
	model := self famixModelOf: (self parseLines: #(
		'      subroutine newbk(lib, title)'
		'      segment, book'
		'        character*40 btitle'
		'        integer bnum'
		'      end segment'
		'      pointeur bk.book'
		'      character*(*) title'
		'      segini, bk'
		'      bk.btitle = title'
		'      segdes, bk'
		'      end')).
	self assert: (model allWithType: FamixF77PUSubroutine) size equals: 1.
	"the visitor predefines the PSTR segment"
	self assert: ((model allWithType: FamixEsopeSegment) collect: #name) asSet equals: #('PSTR' 'book') asSet.
	self assert: ((model allWithSubTypesOf: FamixEsopeCommand) collect: [ :each | each class ]) asSet equals: { FamixEsopeCommandSegini. FamixEsopeCommandSegdes } asSet.
	self assert: ((model allWithType: FamixF77Variable) collect: #name) asSet equals: #('bk' 'title') asSet

]

{ #category : 'tests' }
F77EsopeIASTParserTest >> testPointeurDeclaration [
	| stmt |
	stmt := (self unitBody: #(
		'      subroutine hello'
		'      pointeur p1.pstr, p2.Pstr'
		'      end')) first.
	self assert: (stmt collect: #class) asArray equals: { IASTEsopePointer. IASTEsopePointer }.
	self assert: (stmt collect: #entityName) asArray equals: #('p1.pstr' 'p2.pstr').
	self assertAnchor: stmt first sourceAnchor is: #(2 7 2 23).
	self assertAnchor: stmt second sourceAnchor is: #(2 7 2 32)

]

{ #category : 'tests' }
F77EsopeIASTParserTest >> testSeginiCommand [
	| stmt |
	stmt := (self unitBody: #(
		'      subroutine hello'
		'      segini,lb'
		'      end')) first.
	self assert: stmt size equals: 1.
	self assert: stmt first class equals: IASTEsopeSegCommand.
	self assert: stmt first entityName equals: 'lb'.
	self assert: stmt first esopeCommand equals: 'segini'.
	self assert: stmt first isWritable isNil.
	self assertAnchor: stmt first sourceAnchor is: #(2 7 2 16)

]

{ #category : 'tests' }
F77EsopeIASTParserTest >> testSegmentCommandInLogicalIf [
	| stmt |
	stmt := (self unitBody: #(
		'      subroutine sub()'
		'      IF (N.GT.0) SEGINI P'
		'      end')) first.
	self assert: stmt first class equals: IASTVarAccess.
	self assert: stmt first entityName equals: 'n'.
	self assert: stmt second class equals: IASTEsopeSegCommand.
	self assert: stmt second entityName equals: 'p'.
	self assert: stmt second esopeCommand equals: 'segini'.
	self assertAnchor: stmt second sourceAnchor is: #(2 19 2 27)

]

{ #category : 'tests' }
F77EsopeIASTParserTest >> testSegmentCommandModeInFamix [
	| model commands |
	model := self famixModelOf: (self parseLines: #(
		'      subroutine hello'
		'      pointeur p.s'
		'      segact, p*MOD'
		'      segdes, p*nomod'
		'      segini, p'
		'      end')).
	commands := ((model allWithSubTypesOf: FamixEsopeCommand) sorted: [ :a :b | a sourceAnchor startLine < b sourceAnchor startLine ])
		collect: [ :each | each attributeAt: #entity ].
	self assert: (commands collect: #entityName) asArray equals: #('p' 'p' 'p').
	self assert: (commands collect: #isWritable) asArray equals: #(true false nil)

]

{ #category : 'tests' }
F77EsopeIASTParserTest >> testSegmentCommandSeveralPointers [
	| stmt |
	stmt := (self unitBody: #(
		'      subroutine hello'
		'      segact p1*mod, p2, p3 = p4'
		'      end')) first.
	self assert: (stmt collect: #class) asArray equals: { IASTEsopeSegCommand. IASTEsopeSegCommand. IASTEsopeSegCommand. IASTVarAccess }.
	"as FamixEsopeImporter: the mode is in the name, IASTToFamixEsopeVisitor>>checkForModSuffix: sets isWritable"
	self assert: (stmt collect: #entityName) asArray equals: #('p1*mod' 'p2' 'p3' 'p4').
	self assertAnchor: stmt first sourceAnchor is: #(2 7 2 20)

]

{ #category : 'tests' }
F77EsopeIASTParserTest >> testSegmentCommands [
	#( 'SEGACT' 'segadj' 'SegSup' 'segdes' 'segprt' ) do: [ :command | | stmt |
		stmt := (self unitBody: {
			'      subroutine hello'.
			'      ' , command , ', LB'.
			'      end' }) first.
		self assert: stmt first class equals: IASTEsopeSegCommand.
		self assert: stmt first esopeCommand equals: command asLowercase.
		self assert: stmt first entityName equals: 'lb'.
		self assertAnchor: stmt first sourceAnchor is: #(2 7 2 17) ]

]

{ #category : 'tests' }
F77EsopeIASTParserTest >> testSegmentDeclarationEmpty [
	| seg |
	seg := (self unitBody: #(
		'      subroutine hello'
		'      segment,user'
		'      end segment'
		'      end')) first.
	self assert: seg class equals: IASTEsopeSegment.
	self assert: seg entityName equals: 'user'.
	self assert: seg declarations isEmpty.
	self assertAnchor: seg sourceAnchor is: #(2 7 3 18)

]

{ #category : 'tests' }
F77EsopeIASTParserTest >> testSegmentDeclarationWithArrayAttribute [
	| seg |
	seg := (self unitBody: #(
		'      subroutine hello'
		'      segment, foo(n)*mod'
		'        integer bar(n)'
		'      end segment'
		'      end')) first.
	self assert: seg entityName equals: 'foo'.
	self assert: seg declarations size equals: 1.
	self assert: seg declarations first entityName equals: 'bar'.
	self assert: seg declarations first typeSpec entityName equals: 'integer'.
	self assert: seg declarations first dimensions size equals: 1.
	self assert: seg declarations first dimensions first entityName equals: 'n'

]

{ #category : 'tests' }
F77EsopeIASTParserTest >> testSegmentDeclarationWithContent [
	| body seg |
	body := self unitBody: #(
		'      subroutine hello'
		'      segment, user'
		'        character*40 uname'
		'c       a comment'
		'        integer uid, age'
		'      endsegment'
		'      end').
	self assert: body size equals: 1.
	seg := body first.
	self assert: seg class equals: IASTEsopeSegment.
	self assert: (seg declarations collect: #entityName) asArray equals: #('uname' 'uid' 'age').
	self assert: (seg declarations collect: [ :each | each typeSpec entityName ]) asArray equals: #('character' 'integer' 'integer').
	self assert: seg declarations first typeSpec length equals: 40.
	self assertAnchor: seg sourceAnchor is: #(2 7 6 17)

]

{ #category : 'tests' }
F77EsopeIASTParserTest >> testSegmentVariablesAreNotLocalVariables [
	| sub |
	sub := (self parseLines: #(
		'      subroutine hello'
		'      segment, user'
		'        integer uid'
		'      end segment'
		'      integer i'
		'      end')) body first.
	self assert: (sub localVariables collect: #entityName) asArray equals: #('i')

]

{ #category : 'tests' }
F77EsopeIASTParserTest >> testSmallFileSample [
	| programFile |
	programFile := self parse: F77EsopeGrammarTest esopeSmallFileSample.
	self assert: programFile body notEmpty.
	self assert: (programFile body anySatisfy: [ :each | each isKindOf: IASTProgramUnit ])

]
