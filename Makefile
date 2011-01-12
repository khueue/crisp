all: trim run_tests

trim:
	# Remove trailing whitespace and such. Not vital.
	- trim_and_clean *.md **/*.pl

run_tests:
	clear
	swipl --quiet -O -g "['lib/crisp'], ['examples/*'], crisp, halt"

example:
	clear
	swipl --quiet -O -g "['lib/crisp'], ['examples/concatenate'], crisp, halt"
