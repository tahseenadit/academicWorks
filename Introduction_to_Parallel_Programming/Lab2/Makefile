CC = g++
CPPFLAGS=-Wall -pthread

exercise1 : exercise1.o
	$(CC) -std=c++11 $(CPPFLAGS) -o exercise1 exercise1.o

exercise1.o : exercise1.cpp
	$(CC) -std=c++11 $(CPPFLAGS) -c exercise1.cpp

clean :
	rm exercise1.o

