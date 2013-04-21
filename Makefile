all: build-junk

BUILDDIR = /tmp/build/http-streams

ifdef V
MAKEFLAGS=-R
else
MAKEFLAGS=-s -R
REDIRECT=>/dev/null
endif

.PHONY: all dirs test build-core junk build-junk tests build-tests benchmarks \
	build-benchmarks build-tags config

#
# Disable missing signatures so that you can actually do development and
# let type-inference get on with things without Haskell bothering you.
# Likewise ignore unused functions since they're usually there while exploring
# various alternative implementations of a function.
#

GHC=ghc \
	-rtsopts \
	-Wall \
	-fwarn-tabs \
	-fno-warn-missing-signatures \
	-fno-warn-unused-binds

CORE_SOURCES=$(shell find src -name '*.hs' -type f)
TEST_SOURCES=$(shell find tests -name '*.hs' -type f)


dirs: $(BUILDDIR)/.dir

$(BUILDDIR)/.dir:
	@/bin/echo -e "MKDIR\t$(BUILDDIR)"
	mkdir -p $(BUILDDIR)
	@/bin/echo -e "MKDIR\t$(BUILDDIR)/core"
	mkdir $(BUILDDIR)/core
	@/bin/echo -e "MKDIR\t$(BUILDDIR)/tests"
	mkdir $(BUILDDIR)/tests
	@/bin/echo -e "MKDIR\t$(BUILDDIR)/junk"
	mkdir $(BUILDDIR)/junk
	@/bin/echo -e "MKDIR\t$(BUILDDIR)/bench"
	mkdir $(BUILDDIR)/bench
	touch $(BUILDDIR)/.dir


config: config.h
config.h: Setup.hs http-streams.cabal
	@/bin/echo -e "CABAL\tconfigure"
	cabal configure --enable-tests


#
# Build core library.
#

build-core: dirs config $(BUILDDIR)/core/httpclient.bin httpclient

$(BUILDDIR)/core/httpclient.bin: $(CORE_SOURCES)
	@/bin/echo -e "GHC\t$@"
	$(GHC) --make -O2 -threaded  \
		-prof -fprof-auto \
		-outputdir $(BUILDDIR)/core \
		-i"$(BUILDDIR):src" \
		-I"." \
		-o $@ \
		src/HttpClient.hs
	@/bin/echo -e "STRIP\t$@"
	strip $@

httpclient:
	@/bin/echo -e "LN -s\t$@"
	ln -s $(BUILDDIR)/core/client.bin $@

junk: build-junk
build-junk: dirs config tests/Snippet.hs $(BUILDDIR)/junk/snippet.bin snippet tags

tests/Snippet.hs:
	@/bin/echo -e "Make a symlink from Snippet.hs -> whichever code you wish to run"
	@false

$(BUILDDIR)/junk/snippet.bin: $(CORE_SOURCES) $(TEST_SOURCES)
	@/bin/echo -e "GHC\t$@"
	$(GHC) --make -O2 -threaded  \
		-prof -fprof-auto-top \
		-outputdir $(BUILDDIR)/junk \
		-i"$(BUILDDIR):src:tests" \
		-I"." \
		-o $@ \
		-Wwarn \
		-main-is Snippet.main \
		tests/Snippet.hs
	@/bin/echo -e "STRIP\t$@"
	strip $@

snippet:
	@/bin/echo -e "LN -s\t$@"
	ln -s $(BUILDDIR)/junk/snippet.bin $@

tags: $(CORE_SOURCES) $(TEST_SOURCES)
	@/bin/echo -e "CTAGS\ttags"
	hothasktags $^ > tags

#
# Build test suite code
#

tests: build-tests
build-tests: dirs config $(BUILDDIR)/tests/check.bin check tags

$(BUILDDIR)/tests/check.bin: $(CORE_SOURCES) $(TEST_SOURCES)
	@/bin/echo -e "GHC\t$@"
	$(GHC) --make -O2 -threaded  \
		-outputdir $(BUILDDIR)/tests \
		-i"$(BUILDDIR):src:tests" \
		-I"." \
		-o $@ \
		-main-is Check.main \
		tests/Check.hs
	@/bin/echo -e "STRIP\t$@"
	strip $@

check:
	@/bin/echo -e "LN -s\t$@"
	ln -s $(BUILDDIR)/tests/check.bin $@

#
# Run tests directly. If using inotify, invoke instead as follows:
#
# $ inotifymake tests -- ./check
#

test: build-tests
	@/bin/echo -e "EXEC\tcheck"
	$(BUILDDIR)/tests/check.bin

#
# Benchmarking code
#

benchmarks: build-benchmarks
build-benchmarks: dirs config tests/Benchmark.hs $(BUILDDIR)/bench/bench.bin bench tags

tests/Benchmark.hs:
	@/bin/echo -e "Make a symlink from Benchmark.hs -> whichever code you wish to run"
	@false

$(BUILDDIR)/bench/bench.bin: $(CORE_SOURCES) $(TEST_SOURCES)
	@/bin/echo -e "GHC\t$@"
	$(GHC) --make -O2 -threaded  \
		-outputdir $(BUILDDIR)/bench \
		-i"$(BUILDDIR):src:tests" \
		-I"." \
		-o $@ \
		tests/Benchmark.hs
	@/bin/echo -e "STRIP\t$@"
	strip $@

bench:
	@/bin/echo -e "LN -s\t$@"
	ln -s $(BUILDDIR)/bench/bench.bin $@

benchmark: build-benchmarks
	@/bin/echo -e "EXEC\tbench"
	$(BUILDDIR)/bench/bench.bin -g -o report.html -s 1000

#
# Cleanup, etc
#

clean: 
	@/bin/echo -e "RM\tbuild artifacts"
	-rm -f *.hi *.o snippet check benchmark
	-rm -f *.prof
	-rm -rf $(BUILDDIR)
	-rm -rf dist/
	@if [ -f tags ] ; then /bin/echo -e "RM\ttags" ; rm tags ; fi
	@if [ -f config.h ] ; then /bin/echo -e "RM\tconfig.h" ; rm config.h ; fi

doc: dist/setup-config tags
	@/bin/echo -e "CABAL\thaddock"
	cabal haddock

format: $(CORE_SOURCES) $(TEST_SOURCES)
	stylish-haskell -i $^
