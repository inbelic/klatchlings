CXX := erlc

ODIR = build
OUT = -o $(ODIR)

_OBJS=client_conn_mgr.beam client_conn.beam server.beam
OBJS= $(patsubst %,$(ODIR)/%,$(_OBJS))

SRC=src

NAME=server

build: $(NAME)

run: build
	cd build; \
	erl

$(ODIR)/%.beam: $(SRC)/%.erl
	$(CXX) $(OUT) $<

$(NAME): $(OBJS)

clean:
	rm -rf build/*
