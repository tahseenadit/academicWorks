#include <stdio.h>

void append(char *str1, char *str2){

  int i,lenx;
  lenx = 0;
  for(i=0;i<100;i++){
    if(str1[i]==0){
      break;
    }else{
      lenx++;
    }
  }
  lenx = lenx;
  for(i=0;i<100;i++){

    if(str2[i]==0){
      str1[lenx] = str2[i];
      break;
    }else{
      str1[lenx] = str2[i];
      lenx++;
    }
  }
  printf("Result of append: %s\n", str1);
}

int main(){

  char x[100], y[100];
  printf("Enter first word: ");
  scanf(" %s", x);
  printf("Enter second word: ");
  scanf(" %s", y);
  append(x,y);
  return 0;
}
