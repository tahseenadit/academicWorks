CC = gcc
CFLAGS=-Wall


led_example : led_example.o led_matrix.o
	$(CC) -o led_example led_example.o led_matrix.o

led_example.o : led_example.c
	$(CC) -c led_example.c

led_matrix.o : led_matrix.c
	$(CC) -c led_matrix.c

clean :
	rm led_example.o led_matrix.o

