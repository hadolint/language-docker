all: FORCE
	stack build --flag language-dockerfile:hadolint --flag language-dockerfile:dockerfmt

examples: FORCE
	stack runghc ./examples/parse.hs
	stack runghc ./examples/parse-string.hs
	stack runghc ./examples/pretty-print.hs
	stack runghc ./examples/edsl.hs
	stack runghc ./examples/edsl-quasi.hs

pull-upstream: FORCE
	if ! ``git remote -v | grep lukasmartinelli`` ; then git remote add lukasmartinelli https://github.com/lukasmartinelli/hadolint ; fi
	git fetch lukasmartinelli
	git rebase lukasmartinelli/master

hlint: FORCE
	find test | grep "\.hs$$" | xargs hlint
	find src | grep "\.hs$$" | xargs hlint

FORCE:
