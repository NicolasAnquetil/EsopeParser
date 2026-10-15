"
Imports an ESOPE project into a Famix model, like `FamixEsopeImporter`: same as `F77Importer`, but with `F77EsopeIASTParser` (ESOPE is parsed directly, no rewriting into comments) and the ESOPE file extensions.
```
	F77EsopeImporter new sourceFolder: '/path/to/src'; import.
	F77EsopeImporter onSourceString: aString.
```
"
Class {
	#name : 'F77EsopeImporter',
	#superclass : 'F77Importer',
	#category : 'Fortran77-Esope-IAST',
	#package : 'Fortran77-Esope-IAST'
}

{ #category : 'accessing' }
F77EsopeImporter class >> extensions [
	"Fortran files and Esope files (FamixFortranImporter extensionsEsope)"

	^ super extensions , #(E ec e fc)

]

{ #category : 'accessing' }
F77EsopeImporter >> parserClass [
	^ F77EsopeIASTParser

]
