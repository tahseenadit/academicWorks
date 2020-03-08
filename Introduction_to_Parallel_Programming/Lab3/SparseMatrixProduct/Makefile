CC = gcc
CFLAGS=-Wall


sparseMatrixProduct : sparseMatrixProduct.o
	$(CC) -fopenmp -o sparseMatrixProduct sparseMatrixProduct.o

sparseMatrixProduct.o : sparseMatrixProduct.c
	$(CC) -fopenmp -c sparseMatrixProduct.c

clean :
	rm sparseMatrixProduct.o

