CXX := erlc

ODIR = build
OUT = -o $(ODIR)

_OBJS=server.beam harness.beam client_sup.beam client.beam
OBJS= $(patsubst %,$(ODIR)/%,$(_OBJS))

NAME=server

build: $(NAME)

run: build
	cd build; \
	erl

$(ODIR)/%.beam: %.erl
	$(CXX) $(OUT) $<

$(NAME): $(OBJS)

clean:
	rm -rf build/*
