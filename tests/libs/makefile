# change this variable to point to the directory with Lua headers
# of the version being tested
LUA_DIR = ../../src

CC = g++

# compilation should generate Dynamic-Link Libraries
CFLAGS = -Wall -O2 -I$(LUA_DIR) -ansi -fpic -shared

# libraries used by the tests
LIBS=lib1.so lib11.so lib2.so lib21.so v-lib2.so
all: $(LIBS)

lib1.so: lib1.c
	$(CC) $(CFLAGS) -o lib1.so lib1.c

lib11.so: lib11.c
	$(CC) $(CFLAGS) -o lib11.so lib11.c

lib2.so: lib2.c
	$(CC) $(CFLAGS) -o lib2.so lib2.c

lib21.so: lib21.c
	$(CC) $(CFLAGS) -o lib21.so lib21.c

v-lib2.so: lib2.so
	mv lib2.so ./v-lib2.so

clean:
	rm -f $(LIBS)
