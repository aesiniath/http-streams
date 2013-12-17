all: snippet

#
# The name of the binary(ies) you want to build if `make` is invoked without an
# explicit target - which is so important it gets to go on the first line. On
# to business: the purpose of this Makefile is to let you build Haskell
# programs with a minimum of fuss. You should be able to type
#
# $ make
#
# and have ./stormy be built; this will happen if and only if there is a
# Haskell source file with a lower-cased named and a main in it by that name in
# src/. This also works with lower-cased source files in tests/:
#
# $ make check
#
# will build ./check from tests/check.hs.
#
# As wired, src/ binaries are built tight and tests/ binaries are built with
# profiling enabled.
#

#
# GHC makes a lot of temporary files. Where to put them?
#

BUILDDIR=/tmp/build/http-streams

#
# Haskell compiler and build options. As specified here we always build with
# the full threaded runtime and GHC profiling enabled, 'cause, you need those
# things.
#

GHC=ghc \
	-rtsopts \
	-O2 \
	-threaded \
	-Wall \
	-fwarn-tabs \
	-fno-warn-missing-signatures \
	-fno-warn-unused-binds

#
# The rest is all machinery. Here we do the V=1 trick to set verbosity.
#

ifdef V
MAKEFLAGS=-R
else
MAKEFLAGS=-s -R
REDIRECT=2>/dev/null
endif

.PHONY: all test tests config

#
# Source files, main and testing
#

CORE_SOURCES=$(shell find src -name '*.hs' -type f)
TEST_SOURCES=$(shell find tests -name '*.hs' -type f)


%: $(BUILDDIR)/%.bin
	@/bin/echo -e "STRIP\t$@"
	strip -s $<
	mv $< $@

$(BUILDDIR)/%.bin: config.h src/%.hs $(CORE_SOURCES) tags
	@if [ ! -d $(BUILDDIR) ] ; then /bin/echo -e "MKDIR\t$(BUILDDIR)" ; mkdir -p $(BUILDDIR) ; fi
	@/bin/echo -e "GHC\t$@"
	$(GHC) --make \
		-outputdir $(BUILDDIR)/$* \
		-i"$(BUILDDIR):src" \
		-I"." \
		-o $@ \
		src/$*.hs


tags: $(CORE_SOURCES) $(TEST_SOURCES)
	@/bin/echo -e "CTAGS\ttags"
	@hothasktags $^ > tags $(REDIRECT)

#
# Build test suite code
#

tests: config check

$(BUILDDIR)/%.bin: config.h tests/%.hs $(CORE_SOURCES) $(TEST_SOURCES) tags
	@if [ ! -d $(BUILDDIR) ] ; then /bin/echo -e "MKDIR\t$(BUILDDIR)" ; mkdir -p $(BUILDDIR) ; fi
	@/bin/echo -e "GHC\t$@"
	$(GHC) --make \
		-prof -fprof-auto \
		-outputdir $(BUILDDIR)/tests \
		-i"$(BUILDDIR):src:tests" \
		-I"." \
		-o $@ \
		tests/$*.hs

#
# Run tests directly. If using inotify, invoke instead as follows:
#
# $ inotifymake tests -- ./check
#

test: config check
	@/bin/echo -e "EXEC\tcheck"
	./check

#
# Cleanup, etc
#

clean: 
	@/bin/echo -e "RM\ttempory files"
	-rm -f *.hi *.o
	-rm -f *.prof
	-rm -rf $(BUILDDIR)
	-rm -rf dist/
	@/bin/echo -e "RM\texecutables"
	-ls src tests | grep ^[[:lower:]]*.hs | xargs basename -s .hs -a | xargs rm -f
	@if [ -f tags ] ; then /bin/echo -e "RM\ttags" ; rm tags ; fi
	-rm -f config.h


format: $(CORE_SOURCES) $(TEST_SOURCES)
	stylish-haskell -i $^

#
# Specific to building http-streams
#

config: config.h

config.h: Setup.hs http-streams.cabal
	@/bin/echo -e "CABAL\tconfigure"
	cabal configure --enable-tests

doc: config.h $(CORE_SOURCES) all
	@/bin/echo -e "CABAL\thaddock"
	cabal haddock

dist: config.h
	cabal sdist

tests/snippet.hs:
	@/bin/echo -e "Make a symlink from snippet.hs -> whichever code you wish to run"
	@false

install: config.h all
	cabal install --force-reinstalls
