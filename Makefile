SUBDIRS_BUILD := server.build client.build klatchlings-game.build ui.build
SUBDIRS_CLEAN := server.clean client.clean klatchlings-game.clean ui.clean

build: $(SUBDIRS_BUILD)

run: build
	$(MAKE) run -C client

clean: $(SUBDIRS_CLEAN)

%.build:
	$(MAKE) build -C $*

%.clean:
	$(MAKE) clean -C $*
