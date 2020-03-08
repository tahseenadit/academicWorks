#include <stdio.h>

void printString(){

  printf("One half is 50%%\n");
}

void printDiff(){

  int a;
  a = 10 - 3;
  printf("The difference between 10 and 3 is %d\n", a);

}

void printDiv(){
  float a;
  a = 1.000000/3.000000;
  printf("1.000000/3.000000 is %f\n", a);

}

int main(){

  printString();
  printDiff();
  printDiv();
  return 0;
}

