CLANG=/usr/bin/clang
CFLAGS=-target riscv64-unknown-freebsd -march=rv64imfd
LD=/usr/bin/ld.lld
LDFLAGS=-T riscv-test.ld
OBJDUMP=/usr/bin/llvm-objdump
OBJDUMPFLAGS=-mattr=+f,+d -D

all: generated

init.o: init.s
	$(CLANG) $(CFLAGS) -c init.s

epilog.o: epilog.s
	$(CLANG) $(CFLAGS) -c epilog.s

stack.o: stack.s
	$(CLANG) $(CFLAGS) -c stack.s

generated.o: generated.s
	$(CLANG) $(CFLAGS) -c generated.s

generated: init.o generated.o epilog.o stack.o
	$(LD) $(LDFLAGS) -o generated init.o generated.o epilog.o stack.o


