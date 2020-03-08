#include<stdio.h>

void countZero(int a[], int b){
  int cnt = 0;
  b--;
  while(b>=0){
    if(a[b] == 0){
      cnt++;
    }
    b--;
  }

  printf("Number of 0's: %d\n", cnt);

}

void printArray(int a[], int b){

  b--;
  int i = 0;
  printf("Initial array: { ");
  while(i<=b){
    if(i == 9){
      printf("%d }\n", a[i]);
    }else{
      printf("%d, ", a[i]);
    }
    i++;
  }

}

void printTripledArray(int a[], int b){

  b--;
  int i = 0;
  printf("Tripled array: { ");
  while(i<=b){
    if(i == 9){
      printf("%d }\n", a[i]*3);
    }else{
      printf("%d, ", a[i]*3);
    }
    i++;
  }

}

int main(){
  int a[10],i;
  printf("Input 10 numbers: ");
  for(i=0;i<10;i++){
    scanf(" %d", &a[i]);
  }

  printArray(a,i);
  countZero(a,i);
  printTripledArray(a,i);

}
