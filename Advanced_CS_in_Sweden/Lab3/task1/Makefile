CC = gcc
CFLAGS=-Wall


task_1 : task_1.o led_matrix.o
	$(CC) -o task_1 task_1.o led_matrix.o

task_1.o : task_1.c
	$(CC) -c task_1.c

led_matrix.o : led_matrix.c
	$(CC) -c led_matrix.c

clean :
	rm task_1.o led_matrix.o

