
all: fptest

fptest: fptest.c
	gcc -o fptest fptest.c -lm
