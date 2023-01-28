CXX := erlc

ODIR = build
OUT = -o $(ODIR)

_OBJS=porter.beam client.beam harness.beam
OBJS= $(patsubst %,$(ODIR)/%,$(_OBJS))

NAME=trg-client

build: $(NAME)

set-up:
	cp run-app build/run-app; \
	chmod u+x build/run-app

run: build set-up
	cd build; \
	./run-app

all: build

$(ODIR)/%.beam: %.erl
	$(CXX) $(OUT) $<

$(NAME): $(OBJS)

clean:
	rm -rf build/*
