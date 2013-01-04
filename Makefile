all: build-junk

BUILDDIR = /tmp/build/http-streams

ifdef V
MAKEFLAGS=-R
else
MAKEFLAGS=-s -R
REDIRECT=>/dev/null
endif

.PHONY: all dirs test build-core junk build-junk tests build-tests benchmarks build-benchmarks

#
# Disable missing signatures so that you can actually do development and
# let type-inference get on with things without Haskell bothering you.
# Likewise ignore unused functions since they're usually there while exploring
# various alternative implementations of a function.
#

GHC=ghc \
	-rtsopts \
	-Wall \
	-Werror \
	-fwarn-tabs \
	-fno-warn-missing-signatures \
	-fno-warn-unused-binds

CORE_SOURCES=$(shell find src -name '*.hs')
TEST_SOURCES=$(shell find tests -name '*.hs')


dirs: $(BUILDDIR)/.dir

$(BUILDDIR)/.dir:
	@echo "MKDIR\t$(BUILDDIR)"
	mkdir -p $(BUILDDIR)
	@echo "MKDIR\t$(BUILDDIR)/core"
	mkdir $(BUILDDIR)/core
	@echo "MKDIR\t$(BUILDDIR)/tests"
	mkdir $(BUILDDIR)/tests
	@echo "MKDIR\t$(BUILDDIR)/junk"
	mkdir $(BUILDDIR)/junk
	@echo "MKDIR\t$(BUILDDIR)/bench"
	mkdir $(BUILDDIR)/bench
	touch $(BUILDDIR)/.dir

#
# Build core library.
#

build-core: dirs $(BUILDDIR)/core/httpclient.bin httpclient

$(BUILDDIR)/core/httpclient.bin: $(CORE_SOURCES)
	@echo "GHC\t$@"
	$(GHC) --make -O -threaded  \
		-prof -fprof-auto \
		-outputdir $(BUILDDIR)/core \
		-i"$(BUILDDIR):src" \
		-o $@ \
		src/HttpClient.hs
	@echo "STRIP\t$@"
	strip $@

httpclient:
	@echo "LN -s\t$@"
	ln -s $(BUILDDIR)/core/client.bin $@

junk: build-junk
build-junk: dirs $(BUILDDIR)/junk/snippet.bin snippet

$(BUILDDIR)/junk/snippet.bin: $(CORE_SOURCES) $(TEST_SOURCES)
	@echo "GHC\t$@"
	$(GHC) --make -O -threaded  \
		-prof -fprof-auto-top \
		-outputdir $(BUILDDIR)/junk \
		-i"$(BUILDDIR):src:tests" \
		-o $@ \
		-Wwarn -fno-warn-unused-imports \
		tests/Snippet.hs
	@echo "STRIP\t$@"
	strip $@

snippet:
	@echo "LN -s\t$@"
	ln -s $(BUILDDIR)/junk/snippet.bin $@

#
# Build test suite code
#

tests: build-tests
build-tests: dirs $(BUILDDIR)/tests/check.bin check

$(BUILDDIR)/tests/check.bin: $(CORE_SOURCES) $(TEST_SOURCES)
	@echo "GHC\t$@"
	$(GHC) --make -O -threaded  \
		-prof -fprof-auto \
		-outputdir $(BUILDDIR)/tests \
		-i"$(BUILDDIR):src:tests" \
		-o $@ \
		tests/Check.hs
	@echo "STRIP\t$@"
	strip $@

check:
	@echo "LN -s\t$@"
	ln -s $(BUILDDIR)/tests/check.bin $@

#
# Run tests directly. If using inotify, invoke instead as follows:
#
# $ inotifymake tests -- ./check
#

test: build-tests
	@echo "EXEC\tcheck"
	$(BUILDDIR)/tests/check.bin

#
# Benchmarking code
#

benchmarks: build-benchmarks
build-benchmarks: dirs $(BUILDDIR)/bench/bench.bin bench

$(BUILDDIR)/bench/bench.bin: $(CORE_SOURCES) $(TEST_SOURCES)
	@echo "GHC\t$@"
	$(GHC) --make -O -threaded  \
		-prof -fprof-auto \
		-outputdir $(BUILDDIR)/bench \
		-i"$(BUILDDIR):src:tests" \
		-o $@ \
		tests/Benchmark.hs
	@echo "STRIP\t$@"
	strip $@

bench:
	@echo "LN -s\t$@"
	ln -s $(BUILDDIR)/bench/bench.bin $@


clean: 
	@echo "RM\ttemp files"
	-rm -f *.hi *.o snippet check tags benchmark
	-rm -f *.prof
	-rm -rf $(BUILDDIR)
	-rm -rf dist/

doc: dist/setup-config
	@echo "CABAL\thaddock"
	cabal haddock

dist/setup-config:
	@echo "CABAL\tconfigure"
	cabal configure
