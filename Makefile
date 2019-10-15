CC     = clang
CFLAGS = --std=c11 -g -Wall
CLIBS  = -lgc
BIN    = believe

.PHONY: clean

$(BIN): *.o
	$(CC) $(CFLAGS) $(CLIBS) -o $@ $^

%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ $^

clean:
	rm -rf *.o $(BIN)
