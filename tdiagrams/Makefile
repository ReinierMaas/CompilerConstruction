CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS     :=

all : haskell

src/CCO/Diag/AG.hs : src/CCO/Diag/AG.ag src/CCO/Diag/AG/Base.ag
	uuagc -Hdcfws -P src/CCO/Diag src/CCO/Diag/AG.ag

src/CCO/Picture/AG.hs : src/CCO/Picture/AG.ag src/CCO/Picture/AG/Base.ag \
		src/CCO/Picture/AG/Printing.ag
	uuagc -Hdcfws -P src/CCO/Picture src/CCO/Picture/AG.ag

haskell : src/CCO/Diag/AG.hs src/CCO/Picture/AG.hs
	stack build

.PHONY : haskell