FROM foo:7-slim

# An extra space after the env value should be no problem
ENV container=false\
    container2=true 

ENV A="a.sh" D="c"\
    B="installDBBinaries.sh" 

ENV X "Y" Z

ENV DOG=Rex\ The\ Dog\ 
    CAT=Top\ Cat

ENV DOCKER_TLS_CERTDIR=
ENV foo\ a=afoo' bar 'baz"qu\"z"
ENV BASE_PATH		/var/spool/apt-mirror
