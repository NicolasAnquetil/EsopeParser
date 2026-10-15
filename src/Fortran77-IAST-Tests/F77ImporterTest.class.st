Class {
	#name : 'F77ImporterTest',
	#superclass : 'TestCase',
	#category : 'Fortran77-IAST-Tests',
	#package : 'Fortran77-IAST-Tests'
}

{ #category : 'helpers' }
F77ImporterTest >> folderWith: aDictionary [
	"in-memory folder with files: name -> lines"

	| root |
	root := FileSystem memory root / 'src'.
	root ensureCreateDirectory.
	aDictionary keysAndValuesDo: [ :name :lines |
		(root / name) writeStreamDo: [ :stream | stream nextPutAll: (String lf join: lines); nextPut: Character lf ] ].
	^ root

]

{ #category : 'helpers' }
F77ImporterTest >> importerClass [
	^ F77Importer

]

{ #category : 'tests' }
F77ImporterTest >> testErrorHandlerCanBeSet [
	| folder importer handler |
	folder := self folderWith: (Dictionary new
		at: 'a.f' put: #('      subroutine a(x)' '      x = (' '      end');
		yourself).
	handler := FortranErrorManager new.
	importer := self importerClass new sourceFolder: folder; errorHandler: handler; yourself.
	importer import.
	self assert: importer errorHandler identicalTo: handler.
	self assert: importer hasErrors.
	self assert: handler errors size equals: 1.
	self assert: (handler errors first key includesSubstring: 'a.f').
	self assert: (handler errors first key includesSubstring: 'Parsing source files').
	self assert: (handler errors first value messageText includesSubstring: 'Syntax error')
]

{ #category : 'tests' }
F77ImporterTest >> testImportFolder [
	| folder importer model include |
	folder := self folderWith: (Dictionary new
		at: 'a.f' put: #('      subroutine a(x)' '      include ''com.inc''' '      call b(x)' '      v = 2' '      end');
		at: 'b.f' put: #('      subroutine b(y)' '      y = 1' '      end');
		at: 'com.inc' put: #('      common /com/ v');
		at: 'notes.txt' put: #('not fortran');
		yourself).
	importer := self importerClass new sourceFolder: folder; yourself.
	model := importer import.
	self assert: importer errors isEmpty description: [ importer errors printString ].
	self assert: importer iastModel size equals: 3.
	self assert: ((model allWithType: FamixF77PUSubroutine) collect: #name) asSet equals: #('a' 'b') asSet.
	self assert: (model allWithType: FamixF77IncludedFile) size equals: 1.
	include := (model allWithType: FamixF77Include) anyOne.
	self assert: include filename equals: 'com.inc'.
	self assert: (model allWithType: FamixF77Invocation) anyOne candidates anyOne name equals: 'b'

]

{ #category : 'tests' }
F77ImporterTest >> testIncludedFileNotFoundIsRecorded [
	"the resolver (resolveIncludes) also records it"

	| folder importer errors |
	folder := self folderWith: (Dictionary new
		at: 'a.f' put: #('      subroutine a' '      include ''missing.inc''' '      end');
		yourself).
	importer := self importerClass new sourceFolder: folder; yourself.
	importer import.
	errors := importer errors select: [ :error | error key beginsWith: 'Parsing included files' ].
	self assert: errors size equals: 1.
	self assert: (errors first key includesSubstring: 'missing.inc').
	self assert: (errors first value messageText includesSubstring: 'not found')
]

{ #category : 'tests' }
F77ImporterTest >> testOnSourceString [
	| model invocation |
	model := self importerClass onSourceString: (String lf join: #(
		'      subroutine a(x)'
		'      call b(x)'
		'      end'
		'      subroutine b(y)'
		'      y = 1'
		'      end')).
	self assert: (model allWithType: FamixF77PUSubroutine) size equals: 2.
	invocation := (model allWithType: FamixF77Invocation) anyOne.
	self assert: invocation candidates size equals: 1.
	self assert: invocation candidates anyOne name equals: 'b'

]

{ #category : 'tests' }
F77ImporterTest >> testStopOnError [
	| folder importer |
	folder := self folderWith: (Dictionary new
		at: 'a.f' put: #('      subroutine a(x)' '      x = (' '      end');
		yourself).
	importer := self importerClass new sourceFolder: folder; yourself.
	self deny: importer stopOnError.
	importer stopOnError: true.
	self assert: importer stopOnError.
	self should: [ importer import ] raise: Error
]

{ #category : 'tests' }
F77ImporterTest >> testSyntaxErrorIsRecorded [
	| folder importer |
	folder := self folderWith: (Dictionary new
		at: 'a.f' put: #('      subroutine a(x)' '      x = (' '      end');
		at: 'b.f' put: #('      subroutine b(y)' '      end');
		yourself).
	importer := self importerClass new sourceFolder: folder; yourself.
	importer import.
	self assert: importer errors size equals: 1.
	self assert: (importer errors first key includesSubstring: 'a.f').
	self assert: importer iastModel size equals: 1

]
