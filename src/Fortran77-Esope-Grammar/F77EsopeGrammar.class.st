"
I extend F77Grammar with the ESOPE extensions (acceptor only, tokens keep their source positions):

- segment definitions:
      SEGMENT, NAME        (comma optional, optional (dims) and *MODE suffix)
        <type declarations>
      END SEGMENT          (or ENDSEGMENT)
- pointer declarations: POINTEUR P1.SEG1, P2.SEG2
- segment commands: SEGINI, SEGACT, SEGDES, SEGADJ, SEGSUP, SEGPRT
      SEGACT, P1, P2      SEGDES, P*NOMOD      SEGINI, P=Q
  (also allowed as the statement of a logical IF)
- component access through a pointer: P.ATTR, P.ATTR(I), P.CSTG(I:J), P.ATTR(/1) (dimension query)
  A dot followed by a dot-operator name (P.EQ.Q) is still an operator.
- include directives on their own line: -INC NAME, %INC NAME (#include is handled by F77Grammar)

Common FORTRAN extensions found in ESOPE code (DO ... ENDDO, tab-format lines) are handled by F77Grammar.
"
Class {
	#name : 'F77EsopeGrammar',
	#superclass : 'F77Grammar',
	#instVars : [
		'segmentDefinition',
		'segmentStatement',
		'endSegmentStatement',
		'pointerStatement',
		'segmentCommand',
		'segmentOperand',
		'componentDot',
		'dotOperatorName',
		'subscripts',
		'includeDirectiveLine'
	],
	#category : 'Fortran77-Esope-Grammar',
	#package : 'Fortran77-Esope-Grammar'
}

{ #category : 'productions' }
F77EsopeGrammar >> actionStatement [
	^ (self stmt: segmentCommand) / super actionStatement

]

{ #category : 'productions' }
F77EsopeGrammar >> bodyLine [
	^ segmentDefinition / super bodyLine

]

{ #category : 'productions' }
F77EsopeGrammar >> componentDot [
	"a dot introducing a component, not a dot operator or logical constant (P.EQ.Q)"
	^ self tok: $. asPParser , (dotOperatorName , $. asPParser) not , #letter asPParser and

]

{ #category : 'productions' }
F77EsopeGrammar >> designator [
	"variable, array element, function reference, substring, segment component through a pointer"
	^ name , subscripts optional , (componentDot , name , subscripts optional) star , substringRange optional

]

{ #category : 'productions' }
F77EsopeGrammar >> dotOperatorName [
	^ #('EQV' 'NEQV' 'EQ' 'NE' 'LT' 'LE' 'GT' 'GE' 'AND' 'OR' 'NOT' 'TRUE' 'FALSE')
		inject: PP2FailingNode new
		into: [ :choice :each | choice / each asPParser caseInsensitive ]

]

{ #category : 'productions' }
F77EsopeGrammar >> endSegmentStatement [
	^ self kw: 'END' or: 'SEGMENT'

]

{ #category : 'productions' }
F77EsopeGrammar >> includeDirectiveLine [
	"ESOPE include lines: -INC NAME or %INC NAME (#include is handled by F77Grammar>>preprocessorLine)"
	^ (self ppTok: (PP2PredicateObjectNode anyOf: '-%') , 'INC' asPParser caseInsensitive , nameChar not)
		, (self ppTok: (blank / eol) negate plus)
		, endOfStatement

]

{ #category : 'productions' }
F77EsopeGrammar >> nonCodeLine [
	^ includeDirectiveLine / super nonCodeLine

]

{ #category : 'productions' }
F77EsopeGrammar >> pointerStatement [
	"POINTEUR P1.SEG1, P2.SEG2"
	^ (self kw: 'POINTEUR') , (self listOf: name , componentDot , name)

]

{ #category : 'productions' }
F77EsopeGrammar >> sectionSubscript [
	"(/N) asks for the Nth dimension of a segment component"
	^ (slash , expression) / super sectionSubscript

]

{ #category : 'productions' }
F77EsopeGrammar >> segmentCommand [
	"SEGACT, P1, P2*MOD   SEGINI, P=Q"
	^ ((self kw: 'SEGINI') / (self kw: 'SEGACT') / (self kw: 'SEGDES') / (self kw: 'SEGADJ') / (self kw: 'SEGSUP') / (self kw: 'SEGPRT'))
		, comma optional , (self listOf: segmentOperand)

]

{ #category : 'productions' }
F77EsopeGrammar >> segmentDefinition [
	^ (self line: (self stmt: segmentStatement))
		, (nonCodeLine / (self line: (self stmt: typeStatement))) star
		, (self line: (self stmt: endSegmentStatement))

]

{ #category : 'productions' }
F77EsopeGrammar >> segmentOperand [
	^ designator , ((equals , designator) / (star , name)) optional

]

{ #category : 'productions' }
F77EsopeGrammar >> segmentStatement [
	"SEGMENT, NAME   or   SEGMENT NAME(DIMS)*MODE"
	^ (self kw: 'SEGMENT') , comma optional , name , (lparen , (self listOf: expression) , rparen) optional , (star , name) optional

]

{ #category : 'productions' }
F77EsopeGrammar >> statement [
	^ (self stmt: pointerStatement) / super statement

]

{ #category : 'productions' }
F77EsopeGrammar >> subscripts [
	^ lparen , (self listOf: sectionSubscript) optional , rparen

]
