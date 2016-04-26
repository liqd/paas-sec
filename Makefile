SHELL=/bin/bash
EXEC=`test -d .stack-work/ && echo "stack exec --" || echo "cabal exec --"`
HLINT=$(EXEC) hlint
PAAS_SEC_SOURCES=-isrc -itests
#SENSEI_STRICT=-Wall

.phony:

%.unregister:
	-$(EXEC) ghc-pkg unregister $*

sensei: .phony paas-sec.unregister
	$(EXEC) sensei $(PAAS_SEC_SOURCES) $(SENSEI_STRICT) tests/Spec.hs $(SENSEI_DEFAULT_ARGS) $(SENSEI_ARGS)

seito: .phony
	sleep 0.2 && seito

hlint:
	$(HLINT) --version
	find src tests -name '*.hs' | xargs $(HLINT)

wc:
	find src tests -name '*.hs' | xargs wc

tags: .phony
	hasktags -b ./{src,tests} {../aula,../../thentos-*}/{src,tests,exec}

grep.%:
	git grep -Hni $*

clean:
	find . -name '*~' -exec rm {} \;
