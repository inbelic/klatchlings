SUBDIRS_BUILD := game.build
SUBDIRS_CLEAN := game.clean

build: $(SUBDIRS_BUILD)

clean: $(SUBDIRS_CLEAN)

%.build:
	$(MAKE) build -C $*

%.clean:
	$(MAKE) clean -C $*
