default: all

all: src/Lexer.hs src/Parser.hs src/AttributeGrammar.hs src/*
	stack build

pdf: readme/readme.tex output/*.png
	cd readme && pdflatex -shell-escape readme.tex

#----------------------------------------
#-- Create PNG's from analysis results --
#----------------------------------------

output/*.png: output/*.cp output/*.slv
	cpFiles := $(wildcard output/*.cp)
	dot -Tpng -O cpFiles
	slvFiles := $(wildcard output/*.slv)
	dot -Tpng -O slvFiles

output/*.cp output/*.slv: examples/* src/*
	stack exec mf graphs 1 examples\\ output\\

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
	rm -f src/AttributeGrammar.hs src/Parser.hs src/Lexer.hs src/*.hi src/*.o output/*

.PHONY: default all clean
