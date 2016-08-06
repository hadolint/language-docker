all: FORCE
	stack build --flag language-dockerfile:hadolint --flag language-dockerfile:dockerfmt

examples: FORCE
	stack runghc ./examples/test-dockerfile.hs

pull-upstream: FORCE
	if ! ``git remote -v | grep lukasmartinelli`` ; then git remote add lukasmartinelli https://github.com/lukasmartinelli/hadolint ; fi
	git fetch lukasmartinelli
	git rebase lukasmartinelli/master

hlint: FORCE
	find test | grep "\.hs$$" | xargs hlint
	find src | grep "\.hs$$" | xargs hlint

FORCE:
