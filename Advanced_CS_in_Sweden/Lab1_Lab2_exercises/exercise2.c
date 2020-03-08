#include <stdio.h>

void printSum(int a, int b){
  int c;
  c = a+b;
  printf("You entered %d and %d, their sum is: %d\n", a, b, c);
}

void printProduct(float a, float b){
  float c;
  c = a*b;
  printf("You entered %f and %f, their product is: %f\n", a, b, c);
}

void printWord(char a[]){
  printf("%s %s\n", a, a);
}

int main(){
  int a,b;
  printf("Give two integers: ");
  scanf("%d %d", &a, &b);
  printSum(a,b);
  float c,d;
  printf("Give two floats: ");
  scanf("%f %f", &c, &d);
  printProduct(c,d);
  char w[100];
  printf("Give a word: ");
  scanf("%s", w);
  printWord(w);
  return 0;

}
