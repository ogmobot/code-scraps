CC=gcc
CFLAGS=-Wall
SOURCES=rsa-encode.c numeric.c
OBJECTS=$(SOURCES:.c=.o)
EXECUTABLE=rsa-encode

all: $(SOURCES) $(EXECUTABLE)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(OBJECTS) -o $@

clean:
	rm -f *.o
	rm -rf zig-cache/
