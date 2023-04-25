LIBS=-lsfml-graphics -lsfml-window -lsfml-system
CXX := g++ -Wall

_OBJS=erl_comms.o main.o
OBJS= $(patsubst %,$(ODIR)/%,$(_OBJS))

ODIR=build
OUT=$(ODIR)/klatch-ui

build: $(OUT)

all: build run

$(ODIR)/%.o: src/%.cpp
	$(CXX) -c $< -o $@

$(ODIR)/%.o: include/%.h
	$(CXX) -c $< -o $@

$(OUT): $(OBJS)
	$(CXX) -o $(OUT) $(OBJS) $(LIBS)

run: build
	cd build; \
	./klatch-ui

dbg: build
	cd build; \
	valgrind ./klatch-ui

clean:
	rm -f $(OUT) $(ODIR)/*.o

.PHONY: clean
