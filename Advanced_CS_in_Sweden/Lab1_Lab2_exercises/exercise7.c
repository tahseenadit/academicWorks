#include<stdio.h>

int addition(int a, int b){

  return a+b;

}

int subtraction(int a, int b){

  return a-b;

}

int multiplication(int a, int b){

  return a*b;

}

int division(int a, int b){

  return a/b;

}

int main(){
  int a,b,add,sub,mul,div;
  printf("Give a: ");
  scanf(" %d", &a);
  printf("Give b: ");
  scanf(" %d", &b);
  add = addition(a,b);
  printf("%d + %d = %d\n", a,b,add);
  sub = subtraction(a,b);
  printf("%d - %d = %d\n", a,b,sub);
  mul = multiplication(a,b);
  printf("%d * %d = %d\n", a,b,mul);
  div = division(a,b);
  printf("%d / %d = %d\n", a,b,div);
  
  return 0;
}
