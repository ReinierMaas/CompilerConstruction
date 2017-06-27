all : build

lib/agquestions/ag.hs : lib/AgQuestions/AG.ag lib/AgQuestions/ag/Base.ag
	uuagc -Hdcfws -P lib/AgQuestions/ lib/AgQuestions/AG.ag

build : lib/agquestions/ag.hs
	stack build

.PHONY : build