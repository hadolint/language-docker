FROM scratch

RUN set -ex; \
	apt-get update; \
	if ! which gpg; then \
		apt-get install -y --no-install-recommends gnupg; \
	fi; \
	if ! gpg --version | grep -q '^gpg (GnuPG) 1\.'; then \
# Ubuntu includes "gnupg" (not "gnupg2", but still 2.x), but not dirmngr, and gnupg 2.x requires dirmngr
# so, if we're not running gnupg 1.x, explicitly install dirmngr too
		apt-get install -y --no-install-recommends dirmngr; \
	fi; \
	rm -rf /var/lib/apt/lists/*
