CC=gcc
CFLAGS=-Wall
SOURCES=rsa-encode.c modexp.c
OBJECTS=$(SOURCES:.c=.o)
EXECUTABLE=rsa-encode

all: $(SOURCES) $(EXECUTABLE)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(OBJECTS) -o $@

clean:
	rm -f *.o
