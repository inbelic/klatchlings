SUBDIRS_BUILD := server.build game.build client.build
SUBDIRS_CLEAN := server.clean game.clean client.clean

build: $(SUBDIRS_BUILD)

clean: $(SUBDIRS_CLEAN)

%.build:
	$(MAKE) build -C $*

%.clean:
	$(MAKE) clean -C $*
