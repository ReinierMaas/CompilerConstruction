default: all

all: src/Lexer.hs src/Parser.hs src/AttributeGrammar.hs
	stack build

pdf: readme/readme.tex
	cd readme && pdflatex readme.tex

#----------------------------

src/Lexer.hs: src/Lexer.x
	cd src && alex Lexer.x

src/Parser.hs: src/Parser.y src/AttributeGrammar.hs src/Lexer.hs
	cd src && happy Parser.y

# uuagc:
#  -H              --haskellsyntax                 Use Haskell like syntax (equivalent to --lckeywords and --doublecolons --genlinepragmas)
#  -d              --data                          generate data type definition
#  -c              --catas                         generate catamorphisms
#  -f              --semfuns                       generate semantic functions
#  -w              --wrappers                      generate wrappers for semantic domains
#  -s              --signatures                    generate signatures for semantic functions
src/AttributeGrammar.hs: src/AttributeGrammar.ag
	cd src && uuagc -Hdcfws AttributeGrammar.ag --module AttributeGrammar

#----------------------------

clean:
	rm -f src/AttributeGrammar.hs src/Parser.hs src/Lexer.hs src/*.hi src/*.o src/Main

.PHONY: default all clean
