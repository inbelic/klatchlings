CXX := erlc

ODIR = build
OUT = -o $(ODIR)

_OBJS=porter.beam client.beam harness.beam
OBJS= $(patsubst %,$(ODIR)/%,$(_OBJS))

NAME=trg-client

build: $(NAME)

run: build
	cd build; \
	erl

all: build

$(ODIR)/%.beam: %.erl
	$(CXX) $(OUT) $<

$(NAME): $(OBJS)

clean:
	rm -rf build/*
