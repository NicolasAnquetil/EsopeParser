"
I am a PetitParser2 grammar (acceptor only, no AST) for FORTRAN 77 fixed-form source code.

I work directly on the raw source: no line joining or column truncation is done beforehand, so every terminal is a PP2Token whose start/stop are its real positions in the file.

Fixed-form layout is handled inside the grammar:
- comment lines: C, c, *, D, d or ! in column 1 (or a line whose first non blank is !);
- columns 1-5: label field; column 6: blank or 0 on an initial line;
- continuation lines (blank cols 1-5, col 6 not blank nor 0, possibly preceded by comment/blank lines) are part of #sep, the trimmer used around every token;
- lines may be longer than 72 columns: every character is significant (no column 73+ truncation);
- character constants may be continued on a continuation line.

Simplifications:
- blanks separate tokens (DO10I=1,10 is not accepted), but two-word keywords are accepted with or without blank (GO TO, END IF, ELSE IF, DOUBLE PRECISION, BLOCK DATA, END FILE);
- a name, keyword or number may not be split across a continuation;
- statement order inside a program unit is not checked; labelled DO loops are flat (label ended), block IF and DO ... ENDDO constructs are nested.

Extensions accepted: END PROGRAM/SUBROUTINE/FUNCTION/BLOCK DATA [name] (name not checked against the header), DO without label ... ENDDO / END DO, DO WHILE (cond) ... ENDDO, EXIT and CYCLE (anywhere, not checked to be inside a DO), tab-format lines ([label] TAB statement; TAB then digit 1-9 for a continuation), IMPLICIT NONE, INCLUDE, INTEGER*4 style lengths, double-quoted strings, ! comments, names with _ and $.

Start rules: #start for a whole source file (one or more program units), #includeFile for a fragment meant to be included (body lines only, no header nor END):
	(F77Grammar newStartingAt: #includeFile) parse: aString
"
Class {
	#name : 'F77Grammar',
	#superclass : 'PP2CompositeNode',
	#instVars : [
		'program',
		'programUnit',
		'bodyLine',
		'endLine',
		'statement',
		'continueStatement',
		'endStatement',
		'nonCodeLine',
		'commentLine',
		'blankLine',
		'blank',
		'eol',
		'eoi',
		'endOfStatement',
		'labelField',
		'initialLineMark',
		'continuationMark',
		'continuation',
		'inlineComment',
		'sep',
		'nameChar',
		'digits',
		'name',
		'integerConstant',
		'exponent',
		'realConstant',
		'logicalConstant',
		'characterConstant',
		'hollerithConstant',
		'signedNumber',
		'complexConstant',
		'unsignedConstant',
		'lparen',
		'rparen',
		'comma',
		'colon',
		'equals',
		'plus',
		'minus',
		'star',
		'power',
		'slash',
		'concat',
		'expression',
		'equivOperand',
		'orOperand',
		'andOperand',
		'relationalOperator',
		'relationalExpression',
		'concatExpression',
		'arithmeticExpression',
		'term',
		'factor',
		'primary',
		'designator',
		'sectionSubscript',
		'substringRange',
		'headerLine',
		'programStatement',
		'functionStatement',
		'subroutineStatement',
		'dummyArgument',
		'blockDataStatement',
		'typeSpec',
		'lengthSelector',
		'typeStatement',
		'entityDeclaration',
		'arrayDeclarator',
		'dimensionDeclarator',
		'dimensionStatement',
		'commonStatement',
		'commonBlockName',
		'commonList',
		'equivalenceStatement',
		'parameterStatement',
		'implicitStatement',
		'letterRange',
		'letter',
		'externalStatement',
		'intrinsicStatement',
		'saveStatement',
		'dataStatement',
		'dataSet',
		'dataObject',
		'dataImpliedDo',
		'doControl',
		'dataValue',
		'dataConstant',
		'entryStatement',
		'includeStatement',
		'ifConstruct',
		'blockIfStatement',
		'elseIfStatement',
		'elseStatement',
		'endIfStatement',
		'actionStatement',
		'assignmentStatement',
		'assignStatement',
		'gotoStatement',
		'arithmeticIfStatement',
		'logicalIfStatement',
		'doStatement',
		'callStatement',
		'callArgument',
		'returnStatement',
		'stopStatement',
		'pauseStatement',
		'readStatement',
		'writeStatement',
		'printStatement',
		'openStatement',
		'closeStatement',
		'inquireStatement',
		'backspaceStatement',
		'endfileStatement',
		'rewindStatement',
		'unitSpecification',
		'controlInfoList',
		'controlInfoItem',
		'format',
		'ioList',
		'ioItem',
		'ioImpliedDo',
		'formatStatement',
		'formatSpecification',
		'formatItem',
		'editDescriptor',
		'doConstruct',
		'blockDoStatement',
		'endDoStatement',
		'exitStatement',
		'cycleStatement',
		'includeFile'
	],
	#category : 'Fortran77-Grammar',
	#package : 'Fortran77-Grammar'
}

{ #category : 'productions' }
F77Grammar >> actionStatement [
	"executable statements allowed in a logical IF; assignment (or statement function) comes last because keywords are not reserved"
	^ (self stmt: continueStatement)
		/ (self stmt: gotoStatement)
		/ (self stmt: arithmeticIfStatement)
		/ (self stmt: assignStatement)
		/ (self stmt: callStatement)
		/ (self stmt: returnStatement)
		/ (self stmt: stopStatement)
		/ (self stmt: pauseStatement)
		/ (self stmt: exitStatement)
		/ (self stmt: cycleStatement)
		/ (self stmt: readStatement)
		/ (self stmt: writeStatement)
		/ (self stmt: printStatement)
		/ (self stmt: openStatement)
		/ (self stmt: closeStatement)
		/ (self stmt: inquireStatement)
		/ (self stmt: backspaceStatement)
		/ (self stmt: endfileStatement)
		/ (self stmt: rewindStatement)
		/ (self stmt: assignmentStatement)

]

{ #category : 'productions' }
F77Grammar >> andOperand [
	^ (self dotOp: 'NOT') star , relationalExpression

]

{ #category : 'productions' }
F77Grammar >> arithmeticExpression [
	^ (plus / minus) optional , term , ((plus / minus) , term) star

]

{ #category : 'productions' }
F77Grammar >> arithmeticIfStatement [
	^ (self kw: 'IF') , lparen , expression , rparen , integerConstant , comma , integerConstant , comma , integerConstant

]

{ #category : 'productions' }
F77Grammar >> arrayDeclarator [
	^ lparen , (self listOf: dimensionDeclarator) , rparen

]

{ #category : 'productions' }
F77Grammar >> assignStatement [
	^ (self kw: 'ASSIGN') , integerConstant , (self kw: 'TO') , name

]

{ #category : 'productions' }
F77Grammar >> assignmentStatement [
	^ designator , equals , expression

]

{ #category : 'productions' }
F77Grammar >> backspaceStatement [
	^ (self kw: 'BACKSPACE') , unitSpecification

]

{ #category : 'productions' }
F77Grammar >> blank [
	^ $  asPParser / Character tab asPParser

]

{ #category : 'productions' }
F77Grammar >> blankLine [
	^ (blank star , eol) / (blank plus , eoi)

]

{ #category : 'productions' }
F77Grammar >> blockDataStatement [
	^ (self kw: 'BLOCK' or: 'DATA') , name optional

]

{ #category : 'productions' }
F77Grammar >> blockDoStatement [
	"DO without label, or DO WHILE (extensions)"
	^ (self kw: 'DO') , (((self kw: 'WHILE') , lparen , expression , rparen) / (comma optional , doControl))

]

{ #category : 'productions' }
F77Grammar >> blockIfStatement [
	^ (self kw: 'IF') , lparen , expression , rparen , (self kw: 'THEN')

]

{ #category : 'productions' }
F77Grammar >> bodyLine [
	^ nonCodeLine / ifConstruct / doConstruct / (self line: statement)

]

{ #category : 'productions' }
F77Grammar >> callArgument [
	"*label is an alternate return"
	^ (star , integerConstant) / hollerithConstant / expression

]

{ #category : 'productions' }
F77Grammar >> callStatement [
	^ (self kw: 'CALL') , name , (lparen , (self listOf: callArgument) optional , rparen) optional

]

{ #category : 'productions' }
F77Grammar >> characterConstant [
	^ self tok: (self quotedBy: $') / (self quotedBy: $")

]

{ #category : 'productions' }
F77Grammar >> closeStatement [
	^ (self kw: 'CLOSE') , lparen , controlInfoList , rparen

]

{ #category : 'productions' }
F77Grammar >> colon [
	^ self tok: $: asPParser

]

{ #category : 'productions' }
F77Grammar >> comma [
	^ self tok: $, asPParser

]

{ #category : 'productions' }
F77Grammar >> commentLine [
	"C, c, *, D, d or ! in column 1, or ! as first non blank character (except in column 6 where it marks a continuation)"
	^ ((PP2PredicateObjectNode anyOf: 'Cc*Dd!')
		/ ((($  asPParser times: 5) , ($  asPParser / $0 asPParser / eol) not) not , blank star , $! asPParser))
		, (eol not , #any asPParser) star , (eol / eoi)

]

{ #category : 'productions' }
F77Grammar >> commonBlockName [
	"/NAME/, or // or / / for blank common"
	^ (slash , name optional , slash) / concat

]

{ #category : 'productions' }
F77Grammar >> commonList [
	^ self listOf: name , arrayDeclarator optional

]

{ #category : 'productions' }
F77Grammar >> commonStatement [
	^ (self kw: 'COMMON') , commonBlockName optional , commonList , (comma optional , commonBlockName , commonList) star

]

{ #category : 'productions' }
F77Grammar >> complexConstant [
	^ lparen , signedNumber , comma , signedNumber , rparen

]

{ #category : 'productions' }
F77Grammar >> concat [
	^ self tok: '//' asPParser

]

{ #category : 'productions' }
F77Grammar >> concatExpression [
	^ arithmeticExpression , (concat , arithmeticExpression) star

]

{ #category : 'productions' }
F77Grammar >> continuation [
	"end of the current line, then a continuation line up to and including column 6,
	or a tab-format continuation line: TAB then a digit 1-9"
	^ eol , nonCodeLine star ,
		((($  asPParser times: 5) , continuationMark)
		/ (Character tab asPParser , (PP2PredicateObjectNode anyOf: '123456789')))

]

{ #category : 'productions' }
F77Grammar >> continuationMark [
	"column 6 of a continuation line"
	^ ($  asPParser / $0 asPParser / eol) not , #any asPParser

]

{ #category : 'productions' }
F77Grammar >> continueStatement [
	^ self kw: 'CONTINUE'

]

{ #category : 'productions' }
F77Grammar >> controlInfoItem [
	"UNIT=6, FMT=*, ERR=10, IOSTAT=I, or positional unit/format"
	^ (name , equals , (star / expression)) / star / expression

]

{ #category : 'productions' }
F77Grammar >> controlInfoList [
	^ self listOf: controlInfoItem

]

{ #category : 'productions' }
F77Grammar >> cycleStatement [
	"extension: next iteration of the enclosing DO loop"
	^ self kw: 'CYCLE'

]

{ #category : 'productions' }
F77Grammar >> dataConstant [
	^ hollerithConstant / ((plus / minus) optional , unsignedConstant) / name

]

{ #category : 'productions' }
F77Grammar >> dataImpliedDo [
	^ lparen , (self listOf: (name , equals) not , dataObject) , comma , doControl , rparen

]

{ #category : 'productions' }
F77Grammar >> dataObject [
	^ dataImpliedDo / designator

]

{ #category : 'productions' }
F77Grammar >> dataSet [
	^ (self listOf: dataObject) , slash , (self listOf: dataValue) , slash

]

{ #category : 'productions' }
F77Grammar >> dataStatement [
	^ (self kw: 'DATA') , dataSet , (comma optional , dataSet) star

]

{ #category : 'productions' }
F77Grammar >> dataValue [
	"[r*] constant"
	^ ((integerConstant / name) , star) optional , dataConstant

]

{ #category : 'productions' }
F77Grammar >> designator [
	"variable, array element, function reference, substring"
	^ name , (lparen , (sectionSubscript , (comma , sectionSubscript) star) optional , rparen) optional , substringRange optional

]

{ #category : 'productions' }
F77Grammar >> digits [
	^ #digit asPParser plus

]

{ #category : 'productions' }
F77Grammar >> dimensionDeclarator [
	"[lower:] upper, upper may be * (assumed size)"
	^ (expression , colon) optional , (star / expression)

]

{ #category : 'productions' }
F77Grammar >> dimensionStatement [
	^ (self kw: 'DIMENSION') , (self listOf: name , arrayDeclarator)

]

{ #category : 'productions' }
F77Grammar >> doConstruct [
	^ (self line: (self stmt: blockDoStatement)) , bodyLine star , (self line: (self stmt: endDoStatement))

]

{ #category : 'productions' }
F77Grammar >> doControl [
	"I = start, end [, step]"
	^ name , equals , expression , comma , expression , (comma , expression) optional

]

{ #category : 'productions' }
F77Grammar >> doStatement [
	^ (self kw: 'DO') , integerConstant , comma optional , doControl

]

{ #category : 'private' }
F77Grammar >> dotOp: aString [
	^ self tok: ('.' , aString , '.') asPParser caseInsensitive

]

{ #category : 'productions' }
F77Grammar >> dummyArgument [
	"* is an alternate return"
	^ name / star

]

{ #category : 'productions' }
F77Grammar >> editDescriptor [
	"I5, F10.3, E12.4E2, 1PE12.4, 2X, A, TL5, BN, SP, -2P, $ ..."
	^ self tok: ((PP2PredicateObjectNode anyOf: '+-') optional , digits optional , #letter asPParser plus
		, (digits , ($. asPParser , digits) optional , ((PP2PredicateObjectNode anyOf: 'Ee') , digits) optional) optional)
		/ $$ asPParser

]

{ #category : 'productions' }
F77Grammar >> elseIfStatement [
	^ (self kw: 'ELSE' or: 'IF') , lparen , expression , rparen , (self kw: 'THEN')

]

{ #category : 'productions' }
F77Grammar >> elseStatement [
	^ self kw: 'ELSE'

]

{ #category : 'productions' }
F77Grammar >> endDoStatement [
	^ self kw: 'END' or: 'DO'

]

{ #category : 'productions' }
F77Grammar >> endIfStatement [
	^ self kw: 'END' or: 'IF'

]

{ #category : 'productions' }
F77Grammar >> endLine [
	^ self line: endStatement

]

{ #category : 'productions' }
F77Grammar >> endOfStatement [
	^ eol / eoi

]

{ #category : 'productions' }
F77Grammar >> endStatement [
	"END, or (Fortran 90 extension) END PROGRAM/SUBROUTINE/FUNCTION/BLOCK DATA [name]"

	^ (self kw: 'END' or: 'PROGRAM') / (self kw: 'END' or: 'SUBROUTINE')
	  / (self kw: 'END' or: 'FUNCTION')
	  / (self kw: 'END' or: 'BLOCKDATA')
	  / ((self kw: 'END') , (self kw: 'BLOCK' or: 'DATA'))
	  , name optional / (self kw: 'END')
]

{ #category : 'productions' }
F77Grammar >> endfileStatement [
	^ (self kw: 'END' or: 'FILE') , unitSpecification

]

{ #category : 'productions' }
F77Grammar >> entityDeclaration [
	^ name , arrayDeclarator optional , lengthSelector optional

]

{ #category : 'productions' }
F77Grammar >> entryStatement [
	^ (self kw: 'ENTRY') , name , (lparen , (self listOf: dummyArgument) optional , rparen) optional

]

{ #category : 'productions' }
F77Grammar >> eoi [
	^ #eoi asPParser

]

{ #category : 'productions' }
F77Grammar >> eol [
	^ String crlf asPParser / Character lf asPParser / Character cr asPParser

]

{ #category : 'productions' }
F77Grammar >> equals [
	^ self tok: $= asPParser

]

{ #category : 'productions' }
F77Grammar >> equivOperand [
	^ orOperand , ((self dotOp: 'OR') , orOperand) star

]

{ #category : 'productions' }
F77Grammar >> equivalenceStatement [
	^ (self kw: 'EQUIVALENCE') , (self listOf: lparen , designator , (comma , designator) plus , rparen)

]

{ #category : 'productions' }
F77Grammar >> exitStatement [
	"extension: leave the enclosing DO loop"
	^ self kw: 'EXIT'

]

{ #category : 'productions' }
F77Grammar >> exponent [
	^ (PP2PredicateObjectNode anyOf: 'EeDdQq') , (PP2PredicateObjectNode anyOf: '+-') optional , digits

]

{ #category : 'productions' }
F77Grammar >> expression [
	^ equivOperand , ((self dotOp: 'EQV') / (self dotOp: 'NEQV') , equivOperand) star

]

{ #category : 'productions' }
F77Grammar >> externalStatement [
	^ (self kw: 'EXTERNAL') , (self listOf: name)

]

{ #category : 'productions' }
F77Grammar >> factor [
	"** is right associative; a sign after ** is a common extension"
	^ primary , (power , (plus / minus) optional , factor) optional

]

{ #category : 'productions' }
F77Grammar >> format [
	"*, a label, or a character expression"
	^ star / expression

]

{ #category : 'productions' }
F77Grammar >> formatItem [
	^ comma / (self tok: $/ asPParser) / colon
		/ (integerConstant optional , formatSpecification)
		/ characterConstant / hollerithConstant / editDescriptor

]

{ #category : 'productions' }
F77Grammar >> formatSpecification [
	"separators are not checked: items, commas, slashes and colons in any order"
	^ lparen , formatItem star , rparen

]

{ #category : 'productions' }
F77Grammar >> formatStatement [
	^ (self kw: 'FORMAT') , formatSpecification

]

{ #category : 'productions' }
F77Grammar >> functionStatement [
	^ typeSpec optional , (self kw: 'FUNCTION') , name , lparen , (name , (comma , name) star) optional , rparen

]

{ #category : 'productions' }
F77Grammar >> gotoStatement [
	"unconditional, computed or assigned GO TO"
	^ (self kw: 'GO' or: 'TO') ,
		((lparen , (self listOf: integerConstant) , rparen , comma optional , expression)
		/ (name , (comma optional , lparen , (self listOf: integerConstant) , rparen) optional)
		/ integerConstant)

]

{ #category : 'productions' }
F77Grammar >> headerLine [
	^ self line: (self stmt: programStatement) / (self stmt: functionStatement) / (self stmt: subroutineStatement) / (self stmt: blockDataStatement)

]

{ #category : 'productions' }
F77Grammar >> hollerithConstant [
	"nHccc...c: n characters after the H"
	^ self tok: [ :context |
		  | start count |
		  start := context position.
		  count := 0.
		  [ context atEnd not and: [ context peek isDigit ] ]
			  whileTrue: [ count := count * 10 + context next digitValue ].
		  (context position > start and: [ count > 0 and: [ context atEnd not and: [ 'Hh' includes: context peek ] ] ])
			  ifFalse: [
				  context position: start.
				  PP2Failure message: 'Hollerith constant expected' context: context ]
			  ifTrue: [
				  context next.
				  [ count > 0 and: [ context atEnd not and: [ (String crlf includes: context peek) not ] ] ]
					  whileTrue: [ context next. count := count - 1 ].
				  count = 0
					  ifTrue: [ context contents copyFrom: start + 1 to: context position ]
					  ifFalse: [
						  context position: start.
						  PP2Failure message: 'Hollerith constant too short' context: context ] ] ] asPParser

]

{ #category : 'productions' }
F77Grammar >> ifConstruct [
	^ (self line: (self stmt: blockIfStatement)) , bodyLine star
		, ((self line: (self stmt: elseIfStatement)) , bodyLine star) star
		, ((self line: (self stmt: elseStatement)) , bodyLine star) optional
		, (self line: (self stmt: endIfStatement))

]

{ #category : 'productions' }
F77Grammar >> implicitStatement [
	^ (self kw: 'IMPLICIT') , ((self kw: 'NONE') / (self listOf: typeSpec , lparen , (self listOf: letterRange) , rparen))

]

{ #category : 'productions' }
F77Grammar >> includeFile [
	"Alternative start rule for a fragment meant to be included: the body of a program unit, without header nor END.
	Use: (F77Grammar newStartingAt: #includeFile) parse: aString"
	^ bodyLine star end

]

{ #category : 'productions' }
F77Grammar >> includeStatement [
	"extension"
	^ (self kw: 'INCLUDE') , characterConstant

]

{ #category : 'productions' }
F77Grammar >> initialLineMark [
	"column 6 of an initial line"
	^ $  asPParser / $0 asPParser

]

{ #category : 'productions' }
F77Grammar >> inlineComment [
	^ $! asPParser , (eol not , #any asPParser) star

]

{ #category : 'productions' }
F77Grammar >> inquireStatement [
	^ (self kw: 'INQUIRE') , lparen , controlInfoList , rparen

]

{ #category : 'productions' }
F77Grammar >> integerConstant [
	^ self tok: digits

]

{ #category : 'productions' }
F77Grammar >> intrinsicStatement [
	^ (self kw: 'INTRINSIC') , (self listOf: name)

]

{ #category : 'productions' }
F77Grammar >> ioImpliedDo [
	^ lparen , (self listOf: (name , equals) not , ioItem) , comma , doControl , rparen

]

{ #category : 'productions' }
F77Grammar >> ioItem [
	^ ioImpliedDo / expression

]

{ #category : 'productions' }
F77Grammar >> ioList [
	^ self listOf: ioItem

]

{ #category : 'private' }
F77Grammar >> kw: aString [
	"case insensitive keyword, not followed by a name character"
	^ self tok: aString asPParser caseInsensitive , nameChar not

]

{ #category : 'private' }
F77Grammar >> kw: aString or: anotherString [
	"two-word keyword, written with or without blanks (GO TO, END IF, ...)"
	^ (self kw: aString , anotherString) / ((self kw: aString) , (self kw: anotherString))

]

{ #category : 'productions' }
F77Grammar >> labelField [
	"columns 1 to 5"
	^ (#digit asPParser / $  asPParser) times: 5

]

{ #category : 'productions' }
F77Grammar >> lengthSelector [
	"*8, *(N+1), *(*)"
	^ star , (integerConstant / (lparen , (star / expression) , rparen))

]

{ #category : 'productions' }
F77Grammar >> letter [
	^ self tok: #letter asPParser , nameChar not

]

{ #category : 'productions' }
F77Grammar >> letterRange [
	^ letter , (minus , letter) optional

]

{ #category : 'private' }
F77Grammar >> line: aParser [
	"fixed-form line: label field and column 6, or tab-format line: [label] TAB statement"
	^ ((#digit asPParser star , Character tab asPParser) / (labelField , initialLineMark)) , aParser , endOfStatement

]

{ #category : 'private' }
F77Grammar >> listOf: aParser [
	"one or more aParser separated by commas"
	^ aParser , (comma , aParser) star

]

{ #category : 'productions' }
F77Grammar >> logicalConstant [
	^ self tok: '.TRUE.' asPParser caseInsensitive / '.FALSE.' asPParser caseInsensitive

]

{ #category : 'productions' }
F77Grammar >> logicalIfStatement [
	^ (self kw: 'IF') , lparen , expression , rparen , actionStatement

]

{ #category : 'productions' }
F77Grammar >> lparen [
	^ self tok: $( asPParser

]

{ #category : 'productions' }
F77Grammar >> minus [
	^ self tok: $- asPParser

]

{ #category : 'productions' }
F77Grammar >> name [
	^ self tok: #letter asPParser , nameChar star

]

{ #category : 'productions' }
F77Grammar >> nameChar [
	^ #word asPParser / $_ asPParser / $$ asPParser

]

{ #category : 'productions' }
F77Grammar >> nonCodeLine [
	^ commentLine / blankLine

]

{ #category : 'productions' }
F77Grammar >> openStatement [
	^ (self kw: 'OPEN') , lparen , controlInfoList , rparen

]

{ #category : 'productions' }
F77Grammar >> orOperand [
	^ andOperand , ((self dotOp: 'AND') , andOperand) star

]

{ #category : 'productions' }
F77Grammar >> parameterStatement [
	^ (self kw: 'PARAMETER') , lparen , (self listOf: name , equals , expression) , rparen

]

{ #category : 'productions' }
F77Grammar >> pauseStatement [
	^ (self kw: 'PAUSE') , (integerConstant / characterConstant) optional

]

{ #category : 'productions' }
F77Grammar >> plus [
	^ self tok: $+ asPParser

]

{ #category : 'productions' }
F77Grammar >> power [
	^ self tok: '**' asPParser

]

{ #category : 'productions' }
F77Grammar >> primary [
	^ unsignedConstant / (lparen , expression , rparen) / designator

]

{ #category : 'productions' }
F77Grammar >> printStatement [
	^ (self kw: 'PRINT') , format , (comma , ioList) optional

]

{ #category : 'productions' }
F77Grammar >> program [
	^ nonCodeLine star , programUnit plus , nonCodeLine star

]

{ #category : 'productions' }
F77Grammar >> programStatement [
	^ (self kw: 'PROGRAM') , name

]

{ #category : 'productions' }
F77Grammar >> programUnit [
	^ (nonCodeLine star , headerLine) optional , bodyLine star , endLine

]

{ #category : 'private' }
F77Grammar >> quotedBy: aCharacter [
	"character constant, possibly continued on next lines"
	| quote |
	quote := aCharacter asPParser.
	^ quote ,
		(continuation / (quote , quote) / ((quote / eol) not , #any asPParser)) star ,
		quote

]

{ #category : 'productions' }
F77Grammar >> readStatement [
	^ (self kw: 'READ') , ((lparen , controlInfoList , rparen , ioList optional) / (format , (comma , ioList) optional))

]

{ #category : 'productions' }
F77Grammar >> realConstant [
	"a dot followed by letters and a dot is an operator, as in 1.EQ.2"
	^ self tok: ((digits , $. asPParser , (#letter asPParser plus , $. asPParser) not , digits optional , exponent optional)
		/ ($. asPParser , digits , exponent optional)
		/ (digits , exponent))

]

{ #category : 'productions' }
F77Grammar >> relationalExpression [
	^ concatExpression , (relationalOperator , concatExpression) optional

]

{ #category : 'productions' }
F77Grammar >> relationalOperator [
	^ (self dotOp: 'EQ') / (self dotOp: 'NE') / (self dotOp: 'LT') / (self dotOp: 'LE') / (self dotOp: 'GT') / (self dotOp: 'GE')

]

{ #category : 'productions' }
F77Grammar >> returnStatement [
	^ (self kw: 'RETURN') , expression optional

]

{ #category : 'productions' }
F77Grammar >> rewindStatement [
	^ (self kw: 'REWIND') , unitSpecification

]

{ #category : 'productions' }
F77Grammar >> rparen [
	^ self tok: $) asPParser

]

{ #category : 'productions' }
F77Grammar >> saveStatement [
	^ (self kw: 'SAVE') , (self listOf: (slash , name , slash) / name) optional

]

{ #category : 'productions' }
F77Grammar >> sectionSubscript [
	^ (expression optional , colon , expression optional) / expression

]

{ #category : 'productions' }
F77Grammar >> sep [
	"Separators allowed around tokens (PP2 trimmers are applied only once)"
	^ (blank / continuation / inlineComment) star

]

{ #category : 'productions' }
F77Grammar >> signedNumber [
	^ (plus / minus) optional , (realConstant / integerConstant)

]

{ #category : 'productions' }
F77Grammar >> slash [
	^ self tok: $/ asPParser , $/ asPParser not

]

{ #category : 'productions' }
F77Grammar >> star [
	^ self tok: $* asPParser , $* asPParser not

]

{ #category : 'accessing' }
F77Grammar >> start [
	^ program end

]

{ #category : 'productions' }
F77Grammar >> statement [
	^ (self stmt: typeStatement)
		/ (self stmt: dimensionStatement)
		/ (self stmt: commonStatement)
		/ (self stmt: equivalenceStatement)
		/ (self stmt: parameterStatement)
		/ (self stmt: implicitStatement)
		/ (self stmt: externalStatement)
		/ (self stmt: intrinsicStatement)
		/ (self stmt: saveStatement)
		/ (self stmt: dataStatement)
		/ (self stmt: entryStatement)
		/ (self stmt: includeStatement)
		/ (self stmt: formatStatement)
		/ (self stmt: doStatement)
		/ (self stmt: logicalIfStatement)
		/ actionStatement

]

{ #category : 'private' }
F77Grammar >> stmt: aParser [
	"a statement must end its line"
	^ aParser , endOfStatement and

]

{ #category : 'productions' }
F77Grammar >> stopStatement [
	^ (self kw: 'STOP') , (integerConstant / characterConstant) optional

]

{ #category : 'productions' }
F77Grammar >> subroutineStatement [
	^ (self kw: 'SUBROUTINE') , name , (lparen , (dummyArgument , (comma , dummyArgument) star) optional , rparen) optional

]

{ #category : 'productions' }
F77Grammar >> substringRange [
	^ lparen , expression optional , colon , expression optional , rparen

]

{ #category : 'productions' }
F77Grammar >> term [
	^ factor , ((star / slash) , factor) star

]

{ #category : 'private' }
F77Grammar >> tok: aParser [
	^ aParser token trim: sep

]

{ #category : 'productions' }
F77Grammar >> typeSpec [
	^ ((self kw: 'INTEGER') / (self kw: 'REAL') / (self kw: 'DOUBLE' or: 'PRECISION') / (self kw: 'COMPLEX')
		/ (self kw: 'DOUBLE' or: 'COMPLEX') / (self kw: 'LOGICAL') / (self kw: 'CHARACTER')) , lengthSelector optional

]

{ #category : 'productions' }
F77Grammar >> typeStatement [
	^ typeSpec , comma optional , (self listOf: entityDeclaration)

]

{ #category : 'productions' }
F77Grammar >> unitSpecification [
	^ (lparen , controlInfoList , rparen) / expression

]

{ #category : 'productions' }
F77Grammar >> unsignedConstant [
	^ complexConstant / logicalConstant / characterConstant / realConstant / integerConstant

]

{ #category : 'productions' }
F77Grammar >> writeStatement [
	^ (self kw: 'WRITE') , lparen , controlInfoList , rparen , ioList optional

]
