# Flags: skip welcome, optimize, run goal (expects arg)
PROLOG = swipl --quiet -O -g

all: trim run_tests

trim:
	# Remove trailing whitespace and such. Not vital.
	- trim_and_clean *.md **/*.pl

run_tests:
	clear
	$(PROLOG) "['lib/crisp'], ['examples/*'], crisp, halt"

example:
	clear
	$(PROLOG) "['lib/crisp'], ['examples/concatenate'], crisp, halt"
