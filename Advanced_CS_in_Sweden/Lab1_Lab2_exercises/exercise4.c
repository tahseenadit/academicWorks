#include <stdio.h>

void printEven(){
  printf("Even numbers between 0 and 40:\n");
  int i = 0;
  while(i <= 40){
    if((i % 2) == 0){
      printf("%d ",i);
    }
    i+=1;
  }
  printf("\n");
}

void printNumbers(){

  int i,j;
  printf("Numbers 1 to 100:\n");
  for(i=0;i<100;i=i+10){
    for(j=1;j<=10;j++){
      if(i == 0 && j < 10){
	printf(" %d ", i+j);
      }else{
	printf("%d ", i+j);
      }
    }
    printf("\n");
  }
}

void printSquare(){

  int a;
  printf("Give a number: ");
  scanf("%d",&a);
  while(1){
    if(a == 0){
      printf("You entered zero.\n");
      break;
    }else{
      printf("The square of %d is %d\n", a, a*a);
      printf("Give a number: ");
      scanf("%d",&a);
    }
  }
}

int main(){

  printEven();
  printNumbers();
  printSquare();
  
  return 0;
  
}
