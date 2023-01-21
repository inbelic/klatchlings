CXX := erlc

ODIR = build
OUT = -o $(ODIR)

_OBJS=port_relayer.beam client.beam game_objects.beam game_harness.beam
OBJS= $(patsubst %,$(ODIR)/%,$(_OBJS))

NAME=trg-client

build: $(NAME)

run: build
	cd build; \
	erl -uipath ../../ui/build/trg-ui

all: build

$(ODIR)/%.beam: %.erl
	$(CXX) $(OUT) $<

$(NAME): $(OBJS)

clean:
	rm -rf build/*
