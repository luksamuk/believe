CC     = clang
CFLAGS = -g --std=c11
CLIBS  = -lgc
BIN    = believe

.PHONY: clean

$(BIN): $(BIN).o
	$(CC) $(CFLAGS) $(CLIBS) -o believe believe.o

%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ $^

clean:
	rm -rf *.o $(BIN)
