"
ESOPE parser building IAST entities: F77EsopeGrammar + F77TIASTBuilder, plus:
- SEGMENT ... END SEGMENT gives an IASTEsopeSegment with its IASTVariable declarations,
- POINTEUR P.SEG gives an IASTEsopePointer named 'p.seg',
- SEGINI, SEGACT, ... give an IASTEsopeSegCommand per pointer (isWritable for P*MOD),
- P.ATTR(I) gives an IASTVarAccess to P whose dereferencedVariable is ATTR, with the indices of ATTR,
- -INC NAME / %INC NAME give an IASTInclude.
"
Class {
	#name : 'F77EsopeIASTParser',
	#superclass : 'F77EsopeGrammar',
	#traits : 'F77TIASTBuilder',
	#classTraits : 'F77TIASTBuilder classTrait',
	#category : 'Fortran77-Esope-IAST',
	#package : 'Fortran77-Esope-IAST'
}

{ #category : 'building' }
F77EsopeIASTParser >> designator [
	"P.ATTR(I): an access to the pointer P, whose dereferencedVariable is ATTR,
	with the indices of ATTR (as JsonToIASTVisitor does)"

	^ super designator ==> [ :nodes | self esopeDesignatorModel: nodes ]

]

{ #category : 'helpers' }
F77EsopeIASTParser >> esopeDesignatorModel: nodes [
	"nodes: name, optional subscripts, components (dot, name, optional subscripts), optional substring range.
	As with FamixEsopeImporter, the access is anchored on the pointer name only"

	| components root substring |
	components := nodes third.
	components ifEmpty: [ ^ self designatorModel: { nodes first. nodes second. nodes fourth } ].
	root := self newAccess: nodes first.
	root indices: (({ nodes second } , (components collect: #third) asArray)
		inject: OrderedCollection new
		into: [ :indices :parens |
			parens ifNotNil: [ indices addAll: (self indicesIn: parens) first ].
			indices ]).
	components
		inject: root
		into: [ :access :component |
			| field |
			field := self newAccess: component second.
			access dereferencedVariable: field.
			field ].
	substring := nodes fourth ifNil: [ #() ] ifNotNil: [ :range | self entitiesIn: range ].
	^ substring
		ifEmpty: [ root ]
		ifNotEmpty: [ { root } , substring ]

]

{ #category : 'building' }
F77EsopeIASTParser >> includeDirectiveLine [
	"-INC NAME or %INC NAME"

	^ super includeDirectiveLine ==> [ :nodes |
		self newInclude: nodes second value command: nodes first value asUppercase nodes: (nodes first: 2) ]

]

{ #category : 'building' }
F77EsopeIASTParser >> pointerStatement [
	"POINTEUR P1.SEG1, P2.SEG2: one IASTEsopePointer named 'p1.seg1' per pointer,
	anchored from the POINTEUR keyword (as FamixEsopeImporter)"

	^ super pointerStatement ==> [ :nodes |
		(self listItems: nodes second) collect: [ :pointer |
			IASTEsopePointer new
				entityName: (self nameOf: pointer first) , '.' , (self nameOf: pointer third);
				sourceAnchor: (self anchorFrom: nodes first to: pointer third);
				yourself ] ]

]

{ #category : 'building' }
F77EsopeIASTParser >> segmentCommand [
	"SEGACT, P1, P2*MOD: one IASTEsopeSegCommand per pointer"

	^ super segmentCommand ==> [ :nodes |
		| command |
		command := nodes first.
		(self listItems: nodes third) inject: #() into: [ :models :operand |
			models , (self segmentCommand: command operand: operand) ] ]

]

{ #category : 'helpers' }
F77EsopeIASTParser >> segmentCommand: aKeywordToken operand: nodes [
	"nodes: designator, optional (= designator) or (* mode).
	As with FamixEsopeImporter, a mode stays in the entityName ('p*mod'): IASTToFamixEsopeVisitor>>checkForModSuffix:
	removes it and sets isWritable"

	| pointer others mode |
	pointer := (self entitiesIn: nodes first) first.
	others := (self entitiesIn: nodes first) allButFirst.
	mode := ''.
	nodes second ifNotNil: [ :suffix |
		suffix first value = '*'
			ifTrue: [ mode := '*' , (self nameOf: suffix second) ]
			ifFalse: [ others := others , (self entitiesIn: suffix) ] ].
	^ { IASTEsopeSegCommand new
			esopeCommand: (self nameOf: aKeywordToken);
			entityName: pointer entityName , mode;
			sourceAnchor: (self anchorOf: { aKeywordToken. nodes });
			yourself } , others

]

{ #category : 'building' }
F77EsopeIASTParser >> segmentDefinition [
	"the first line sequence is the receiver, so PP2 flattens it:
	{label. segment statement. eol. lines. end segment line}"

	^ super segmentDefinition ==> [ :nodes |
		| header end |
		header := nodes second first.
		end := self lastTokenIn: nodes fifth.
		IASTEsopeSegment new
			entityName: (self nameOf: header third);
			declarations: (((self entitiesIn: nodes fourth) select: [ :each | each isVariable ]) asOrderedCollection);
			sourceAnchor: (self anchorFrom: (self firstTokenIn: header) to: end);
			yourself ]

]
