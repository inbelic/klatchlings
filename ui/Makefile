LIBS=-lsfml-graphics -lsfml-window -lsfml-system
CXX := g++

_OBJS=main.o erlcomms.o
OBJS= $(patsubst %,$(ODIR)/%,$(_OBJS))

ODIR=build
OUT=$(ODIR)/klatchlings-ui

build: $(OUT)

all: build run

$(ODIR)/%.o: %.cpp
	$(CXX) -c $< -o $@

$(ODIR)/%.o: %.h
	$(CXX) -c $< -o $@

$(OUT): $(OBJS)
	$(CXX) -o $(OUT) $(OBJS) $(LIBS)

clean:
	rm -f $(OUT) $(ODIR)/*.o

.PHONY: clean
