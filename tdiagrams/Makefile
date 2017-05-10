all : build

diag : lib/CCO/Diag/AG.ag lib/CCO/Diag/AG/Base.ag
	uuagc -Hdcfws -P lib/CCO/Diag lib/CCO/Diag/AG.ag

typecheck : lib/CCO/TypeCheck/AG.ag lib/CCO/TypeCheck/AG/Base.ag \
 		diag
	uuagc -Hdcfws -P lib/CCO/TypeCheck lib/CCO/TypeCheck/AG.ag

diag2picture : lib/CCO/Diag2Picture/AG.ag lib/CCO/Diag2Picture/AG/Base.ag \
		diag picture
	uuagc -Hdcfws -P lib/CCO/Diag2Picture lib/CCO/Diag2Picture/AG.ag

picture : lib/CCO/Picture/AG.ag lib/CCO/Picture/AG/Base.ag \
		lib/CCO/Picture/AG/Printing.ag
	uuagc -Hdcfws -P lib/CCO/Picture lib/CCO/Picture/AG.ag

haskell : diag typecheck diag2picture picture

build : haskell
	stack build

test : haskell
	stack build --exec tdiagrams-test

e2e-test : haskell
	stack build --exec e2e-test

.PHONY : build