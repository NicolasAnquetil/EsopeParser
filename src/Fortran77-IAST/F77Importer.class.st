"
Imports a FORTRAN 77 project into a Famix model, like FamixFortranImporter, but the IAST comes directly from our PetitParser2 parser (F77IASTParser): no de-Esopifying, no fortran-src, no JSON, and the source anchors point in the real source files.

Steps (#importSteps): parse the source files (#extensions) to IAST, parse the files they include (found in the include folder, default: source folder) with the #includeFile start rule, IAST to Famix (IASTToFamixFortranVisitor), symbol resolution (FamixEsopeResolver), sanity checks.

	importer := F77Importer new sourceFolder: '/path/to/src'; includeFolder: '/path/to/inc'; yourself.
	model := importer import.
	importer errors.	""syntax errors and errors of the steps""
	importer iastModel.	""the IASTProgramFile of each file""

	F77Importer onSourceString: aString.	""as FamixFortranImporter class>>onSourceString:""
	F77Importer onFile: aPathString.

## Managing import errors

Import errors are recorded by the importer's errorHandler (a FortranErrorManager), with the context (step, file) where they happened:

	importer errorHandler.
	importer errors.	""context -> exception""
	importer hasErrors.

Another error manager can be given (e.g. to share it with other importers):

	importer errorHandler: FortranErrorManager new.

Additional checking is run on the final model (true by default):

	importer withSanityChecks: true.

For debugging, the importer can stop at the first error:

	importer stopOnError: true.
"
Class {
	#name : 'F77Importer',
	#superclass : 'Object',
	#instVars : [
		'srcFolder',
		'includeFolder',
		'iastModel',
		'famixModel',
		'errorHandler',
		'withSanityChecks'
	],
	#category : 'Fortran77-IAST',
	#package : 'Fortran77-IAST'
}

{ #category : 'accessing' }
F77Importer class >> extensions [
	^ #(f for F FOR f77 F77)

]

{ #category : 'accessing' }
F77Importer class >> includeExtensions [
	^ #(inc INC h)

]

{ #category : 'accessing' }
F77Importer class >> onFile: aPathString [
	self assert: aPathString asFileReference exists.
	^ self new modelOfFile: aPathString asFileReference

]

{ #category : 'accessing' }
F77Importer class >> onSourceString: aSourceString [
	"Famix model of the source code (as FamixFortranImporter class>>onSourceString:)"

	^ self new modelOfSourceString: aSourceString

]

{ #category : 'accessing' }
F77Importer >> errorHandler [
	^ errorHandler ifNil: [ errorHandler := FortranErrorManager new ]
]

{ #category : 'accessing' }
F77Importer >> errorHandler: aFortranErrorManager [
	errorHandler := aFortranErrorManager
]

{ #category : 'error' }
F77Importer >> errors [
	^ errorHandler errors

]

{ #category : 'accessing' }
F77Importer >> famixModel [
	^ famixModel

]

{ #category : 'accessing' }
F77Importer >> famixModel: aModel [
	famixModel := aModel

]

{ #category : 'run' }
F77Importer >> famixResolve [
	(FamixEsopeResolver on: famixModel)
		errorHandler: errorHandler;
		resolve

]

{ #category : 'private' }
F77Importer >> filesIn: aFolder withExtensions: extensions [
	aFolder ifNil: [ ^ #() ].
	^ (aFolder allFiles select: [ :file | extensions includes: file extension ])
		sorted: [ :a :b | a fullName < b fullName ]

]

{ #category : 'error' }
F77Importer >> hasErrors [
	^ errorHandler isNotEmpty

]

{ #category : 'run' }
F77Importer >> iASTToFamix [
	| visitor |
	visitor := IASTToFamixFortranVisitor new.
	iastModel do: [ :programFile |
		errorHandler enterContext: 'file:' , programFile filename.
		[ self prepareForFamix: programFile.
		  programFile accept: visitor ]
			on: Error
			do: [ :error | errorHandler error: error ].
		errorHandler leaveContext ].
	famixModel := visitor model

]

{ #category : 'accessing' }
F77Importer >> iastModel [
	"the IASTProgramFile of each file imported"

	^ iastModel

]

{ #category : 'accessing' }
F77Importer >> iastModel: aModel [

	iastModel := aModel

]

{ #category : 'run' }
F77Importer >> import [
	srcFolder ifNil: [ Warning signal: 'Set source folder first' ].
	self importSteps do: [ :step |
		errorHandler
			handleErrorsFor: step key
			running: [ self perform: step value ] ].
	^ famixModel

]

{ #category : 'run' }
F77Importer >> importSteps [
	"same steps as FamixFortranImporter, except that the IAST comes directly from our PetitParser2 parser
	(no de-Esopifying, no fortran-src, no JSON)"

	^ {
		'Parsing source files to IAST' -> #parseSourceFiles.
		'Parsing included files to IAST' -> #parseIncludedFiles.
		'Intermediary AST to Famix' -> #iASTToFamix.
		'Famix symbol resolution' -> #famixResolve.
		'Post import sanity checks' -> #postImportChecks }

]

{ #category : 'accessing' }
F77Importer >> includeFolder [
	^ includeFolder ifNil: [ srcFolder ]

]

{ #category : 'accessing' }
F77Importer >> includeFolder: folder [
	"folder is either a String or a FileReference"

	includeFolder := folder asFileReference

]

{ #category : 'private' }
F77Importer >> includedFileNamed: aName [
	"aName (or aName.inc) in the include folder or one of its sub-folders, ignoring case"

	| candidates |
	candidates := { aName. aName , '.inc' } collect: #asLowercase.
	^ (self filesIn: self includeFolder withExtensions: (self class includeExtensions , self class extensions , #('E' 'e' 'seg' 'obg')))
		detect: [ :file | (candidates includes: file basename asLowercase) or: [ candidates includes: (file path relativeTo: self includeFolder path) pathString asLowercase ] ]
		ifNone: [ nil ]

]

{ #category : 'private' }
F77Importer >> includedNamesIn: aProgramFile [
	^ (aProgramFile body flatCollect: [ :each |
		(each isKindOf: IASTInclude)
			ifTrue: [ { each entityName } ]
			ifFalse: [
				(each isKindOf: IASTProgramUnit)
					ifTrue: [ (each body ifNil: [ #() ]) flattened select: [ :e | e isKindOf: IASTInclude ] thenCollect: #entityName ]
					ifFalse: [ #() ] ] ]) asSet

]

{ #category : 'initialization' }
F77Importer >> initialize [
	super initialize.
	withSanityChecks := true.
	iastModel := OrderedCollection new.
	errorHandler := FortranErrorManager new

]

{ #category : 'model' }
F77Importer >> modelOfFile: aFileReference [
	| programFile visitor |
	programFile := self parserClass new parseFile: aFileReference.
	programFile isPetit2Failure ifTrue: [ ^ Error signal: 'Syntax error: ' , programFile printString ].
	visitor := IASTToFamixFortranVisitor new.
	self prepareForFamix: programFile.
	visitor visit: programFile.
	FamixEsopeResolver new model: visitor model; resolve.
	^ visitor model

]

{ #category : 'model' }
F77Importer >> modelOfSourceString: aSourceString [
	"as FamixFortranImporter class>>onSourceString: (IAST to Famix, then resolve); the file name is the one it uses"

	| programFile visitor |
	programFile := (self parserClass new fileName: './srcCodeOrigin.f'; yourself) parse: aSourceString.
	programFile isPetit2Failure ifTrue: [ ^ Error signal: 'Syntax error: ' , programFile printString ].
	visitor := IASTToFamixFortranVisitor new.
	self prepareForFamix: programFile.
	visitor visit: programFile.
	FamixEsopeResolver new model: visitor model; resolve.
	^ visitor model
]

{ #category : 'private' }
F77Importer >> parse: aFileReference startingAt: aSymbol [
	| result |
	errorHandler enterContext: 'file:' , aFileReference fullName.
	result := [ (self parserClass newStartingAt: aSymbol) parseFile: aFileReference ]
		on: Error
		do: [ :error | errorHandler error: error. nil ].
	(result isKindOf: IASTProgramFile)
		ifTrue: [ iastModel add: result ]
		ifFalse: [ result ifNotNil: [ self recordError: 'Syntax error: ' , result printString ] ].
	errorHandler leaveContext.
	^ result
]

{ #category : 'run' }
F77Importer >> parseIncludedFiles [
	"included files are those named by an include in the source files and found in the include folder"

	| parsed |
	parsed := Set new.
	(iastModel flatCollect: [ :programFile | self includedNamesIn: programFile ]) asSet do: [ :name |
		(self includedFileNamed: name)
			ifNil: [
				errorHandler enterContext: name.
				self recordError: 'included file not found: ' , name.
				errorHandler leaveContext ]
			ifNotNil: [ :file |
				(parsed includes: file fullName) ifFalse: [
					parsed add: file fullName.
					self parse: file startingAt: #includeFile ] ] ]
]

{ #category : 'run' }
F77Importer >> parseSourceFiles [
	(self filesIn: srcFolder withExtensions: self class extensions) do: [ :file |
		self parse: file startingAt: #start ]

]

{ #category : 'accessing' }
F77Importer >> parserClass [
	^ F77IASTParser

]

{ #category : 'run' }
F77Importer >> postImportChecks [
	withSanityChecks ifTrue: [
		famixModel entities do: [ :entity |
			errorHandler enterContext: entity asString.
			entity famixImportSanityCheck.
			errorHandler leaveContext ] ]

]

{ #category : 'private' }
F77Importer >> prepareForFamix: aProgramFile [
	"IASTToFamixFortranVisitor>>visitIASTIncludedFile: expects the fake subroutine of fortran-src
	(one line more at the beginning) and moves the start of the anchor one line down"

	aProgramFile body do: [ :unit |
		(unit isKindOf: IASTIncludedFile) ifTrue: [
			unit sourceAnchor startLine: unit sourceAnchor startLine - 1 ] ]

]

{ #category : 'private' }
F77Importer >> recordError: aString [
	"signal a real Error, so that the errorHandler can record it, or pass it when it must stop on errors"

	[ Error signal: aString ] on: Error do: [ :error | errorHandler error: error ]
]

{ #category : 'accessing' }
F77Importer >> sourceFolder [
	^ srcFolder

]

{ #category : 'accessing' }
F77Importer >> sourceFolder: folder [
	"folder is either a String or a FileReference"

	srcFolder := folder asFileReference

]

{ #category : 'accessing' }
F77Importer >> stopOnError [
	^ errorHandler stop
]

{ #category : 'error' }
F77Importer >> stopOnError: aBoolean [
	errorHandler stopOnError: aBoolean

]

{ #category : 'accessing' }
F77Importer >> withSanityChecks: aBoolean [
	withSanityChecks := aBoolean

]
