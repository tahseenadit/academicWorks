#include <iostream>
#include <thread>
#include <chrono>
#include <cmath>
#include <mutex>
#include <string.h>

std::mutex mutex;

double x_1;
double x_2;
double area = 0.0;

void calculateArea(double x1, double x2, double trapezes_per_thread,double chunks)
{
  double y1;
  double y2;
  double newarea;

  mutex.lock();
  if(x_1 != 1){
    if(trapezes_per_thread < 1){

      x_2 = x_1 + (chunks*trapezes_per_thread);
      if(x_2 > 1){
	x_2 = 1;
      }
      y1 = atan(x_1)*4.0;
      y2 = atan(x_2)*4.0;
      newarea = ((y1+y2)/2)*(x_2-x_1);
      area = area + newarea;
      x_1 = x_2;
    
    }else{
      int trapeze = trapezes_per_thread / 1;
      double portion = fmod(trapezes_per_thread, 1);

      for(int i = 0; i < trapeze; i++){
	y1 = atan(x_1)*4.0;
	x_2 = x_1 + chunks;
	if(x_2 > 1){
	  x_2 = 1;
	}
	y2 = atan(x_2)*4.0;
	newarea = ((y1+y2)/2)*(x_2-x_1);
	area = area + newarea;
	x_1 = x_2;
	if(x_1 == 1){
	  break;
	}
      }

      if(portion != 0 && x_1 != 1){
	y1 = atan(x_1)*4.0;
	x_2 = x_1 + (chunks*portion);
	if(x_2 > 1){
	  x_2 = 1;
	}
	y2 = atan(x_2)*4.0;
	newarea = ((y1+y2)/2)*(x_2-x_1);
	area = area + newarea;
	x_1 = x_2;
      }
    }
  }
  mutex.unlock();
}

void help()
{
  std::cout << "Provide arguments T and N." << std::endl;
  std::cout << "Usage: T N" << std::endl;
  std::cout << std::endl;
  std::cout << "  T: number of threads (must be 1 or greater than 1)" << std::endl;
  std::cout << "  N: number of trapezes (must be 1 or greater than 1)" << std::endl;
}

int main(int argc, char *argv[])
{
  if (argc == 2 && strcmp(argv[1], "-h")==0){
    help();
    return 0;
  }else if (argc != 3){
    std::cout << "Wrong input given. Use -h for help." << std::endl;
    return 0;
  }

  double threads;
  try
    {
      threads = std::stoi(argv[1]);
    }
  catch (std::exception)
    {
      help();
      return 0;
    }
  if (threads < 1)
    {
      help();
      return 0;
    }

  double trapezes;
  try
    {
      trapezes = std::stoi(argv[2]);
    }
  catch (std::exception)
    {
      help();
      return 0;
    }
  if (trapezes < 1)
    {
      help();
      return 0;
    }

  float chunks = 1.0/trapezes;
  x_1 = 0;
  x_2 = x_1 + chunks;
  
  // *** timing begins here ***
  auto start_time = std::chrono::system_clock::now();

  // create and join threads
  std::thread *t = new std::thread[(int)threads];
  double trapezes_per_thread = trapezes / threads;
  
  for (int i=0; i<threads; ++i)
    {
      t[i] = std::thread(calculateArea, x_1, x_2, trapezes_per_thread,chunks);
    }

  for (int i=0; i<threads; ++i)
    {
      t[i].join();
    }

  std::chrono::duration<double> duration =
    (std::chrono::system_clock::now() - start_time);
  // *** timing ends here ***

  std::cout << "Total area: " << area << std::endl; 
  std::cout << "Finished in " << duration.count() << " seconds (wall clock)." << std::endl;

  return 0;
}
