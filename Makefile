pull-upstream:
	if ! ``git remote -v | grep lukasmartinelli`` ; then git remote add lukasmartinelli https://github.com/lukasmartinelli/hadolint ; fi
	git fetch lukasmartinelli
	git rebase lukasmartinelli/master

hlint:
	find test | grep "\.hs$$" | xargs hlint
	find src | grep "\.hs$$" | xargs hlint
