CC     = clang
CFLAGS = --std=c11 -g -Wall -O2 -DBEL_DEBUG
CLIBS  = -lgc
BIN    = believe
OBJ    = believe.o

.PHONY: clean

$(BIN): $(OBJ)
	$(CC) $(CFLAGS) $(CLIBS) -o $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $^

clean:
	rm -rf *.o $(BIN)
