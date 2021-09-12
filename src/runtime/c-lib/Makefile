.PHONY: clean all

all: clib.dylib libclib.a

clib.dylib:
	@clang -shared -undefined dynamic_lookup -o clib.so lib.c pngstuff.c -I/usr/local/include

libclib.a:
	@clang -O -c lib.c
	@clang -O -c pngstuff.c -I/usr/local/include
	@ar rcs clib.a lib.o pngstuff.o

clean:
	rm -r *.o *.a *.dylib
