#include <stdio.h>

void printNumbers(){

  int i,j;
  char c;
  printf("Give a number: ");
  scanf("%d",&i);
  while(1){
    for(j=1;j<=i;j++){
      printf("%d\n",j);
    }
    printf("Run again (y/n)? ");
    scanf(" %c", &c);
    if(c == 'n'){
      printf("Exiting...\n");
      break;
    }else{
      printf("Give a number: ");
      scanf("%d",&i);
    }    
  }
}

int main(){

  printNumbers();
  
  return 0;
  
}
