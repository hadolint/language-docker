all: FORCE
	stack build

gh-pages:
	bash -e ./stack-gh-pages

doctests: FORCE
	find src | grep "\.hs$$" | while read pkg; do echo $$pkg; stack exec doctest $$pkg; done

examples: FORCE
	stack runghc ./examples/parse.hs
	stack runghc ./examples/parse-string.hs
	stack runghc ./examples/pretty-print.hs
	stack runghc ./examples/edsl.hs
	stack runghc ./examples/edsl-quasi.hs
	stack runghc ./examples/templating.hs
	stack runghc ./examples/complex.hs

hlint: FORCE
	find test | grep "\.hs$$" | xargs hlint
	find src | grep "\.hs$$" | xargs hlint

FORCE:
