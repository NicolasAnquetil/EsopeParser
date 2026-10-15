Class {
	#name : 'F77EsopeImporterTest',
	#superclass : 'F77ImporterTest',
	#category : 'Fortran77-Esope-IAST-Tests',
	#package : 'Fortran77-Esope-IAST-Tests'
}

{ #category : 'testing' }
F77EsopeImporterTest class >> shouldInheritSelectors [
	^ true
]

{ #category : 'helpers' }
F77EsopeImporterTest >> importerClass [
	^ F77EsopeImporter

]

{ #category : 'tests' }
F77EsopeImporterTest >> testImportEsopeFolder [
	| folder importer model |
	folder := self folderWith: (Dictionary new
		at: 'newbk.E' put: #(
			'      subroutine newbk(title)'
			'#include "struc.inc"'
			'      character*(*) title'
			'      pointeur bk.book'
			'      segini, bk'
			'      bk.btitle = title'
			'      segdes, bk*MOD'
			'      end');
		at: 'struc.inc' put: #(
			'      segment, book'
			'        character*40 btitle'
			'      end segment');
		yourself).
	importer := self importerClass new sourceFolder: folder; yourself.
	model := importer import.
	self assert: importer errors isEmpty description: [ importer errors printString ].
	self assert: (model allWithType: FamixF77PUSubroutine) anyOne name equals: 'newbk'.
	self assert: (model allWithType: FamixF77IncludedFile) size equals: 1.
	self assert: ((model allWithType: FamixEsopeSegment) collect: #name) asSet equals: #('PSTR' 'book') asSet.
	self assert: ((model allWithSubTypesOf: FamixEsopeCommand) collect: #class) asSet equals: { FamixEsopeCommandSegini. FamixEsopeCommandSegdes } asSet

]

{ #category : 'tests' }
F77EsopeImporterTest >> testOnSourceString [
	| model |
	super testOnSourceString.
	model := self importerClass onSourceString: (String lf join: #(
		'      subroutine a'
		'      pointeur p.s'
		'      segact, p'
		'      end')).
	self assert: (model allWithSubTypesOf: FamixEsopeCommand) size equals: 1

]
