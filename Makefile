# Flags: skip welcome, optimize, run goal (expects arg)
PROLOG = swipl --quiet -O -g

all: run_tests

run_tests:
	$(PROLOG) "['lib/crisp','examples/*'], crisp, halt"

example:
	$(PROLOG) "['lib/crisp','examples/concatenate'], crisp, halt"

stay:
	$(PROLOG) "['lib/crisp','examples/*'], crisp"
