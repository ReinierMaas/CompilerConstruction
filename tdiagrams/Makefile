CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS     :=

all : haskell

lib/CCO/Diag/AG.hs : lib/CCO/Diag/AG.ag lib/CCO/Diag/AG/Base.ag
	uuagc -Hdcfws -P lib/CCO/Diag lib/CCO/Diag/AG.ag

lib/CCO/Picture/AG.hs : lib/CCO/Picture/AG.ag lib/CCO/Picture/AG/Base.ag \
		lib/CCO/Picture/AG/Printing.ag
	uuagc -Hdcfws -P lib/CCO/Picture lib/CCO/Picture/AG.ag

haskell : lib/CCO/Diag/AG.hs lib/CCO/Picture/AG.hs
	stack build

.PHONY : haskell