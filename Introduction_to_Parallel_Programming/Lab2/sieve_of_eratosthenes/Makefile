CC = g++
CPPFLAGS=-Wall -pthread

sieve : sieve.o
	$(CC) -std=c++11 $(CPPFLAGS) -o sieve sieve.o

sieve.o : sieve.cpp
	$(CC) -std=c++11 $(CPPFLAGS) -c sieve.cpp

clean :
	rm sieve.o

