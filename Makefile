all: FORCE
	stack build --flag language-dockerfile:hadolint --flag language-dockerfile:dockerfmt

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

pull-upstream: FORCE
	if ! ``git remote -v | grep lukasmartinelli`` ; then git remote add lukasmartinelli https://github.com/lukasmartinelli/hadolint ; fi
	git fetch lukasmartinelli
	git rebase lukasmartinelli/master

hlint: FORCE
	find test | grep "\.hs$$" | xargs hlint
	find src | grep "\.hs$$" | xargs hlint

FORCE:
