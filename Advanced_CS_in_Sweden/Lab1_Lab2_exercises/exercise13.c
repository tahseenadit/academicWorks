#include<stdio.h>

int j=0;
int i=1;
int k;

int fib(int n){

  while(n>0){
    k = i + j;
    j = i;
    i = k;
    printf("%d\n", j);
    n--;
  }
  
  return j;

}


int main(){

  int n;
  printf("Give n: ");
  scanf(" %d", &n);
  fib(n);
  return 0;
}
