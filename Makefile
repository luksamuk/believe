CC     = clang
CFLAGS = -std=c17 -g -O2 -Wall -DBEL_DEBUG
CLIBS  = -lgc -lm
BIN    = believe
OBJ    = believe.o

.PHONY: clean

$(BIN): $(OBJ)
	$(CC) $(CFLAGS) $(CLIBS) -o $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $^

clean:
	rm -rf *.o $(BIN)
