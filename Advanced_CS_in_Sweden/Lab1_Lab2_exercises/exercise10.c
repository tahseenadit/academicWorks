#include<stdio.h>
#include<stdlib.h>
#include<string.h>

void bubbleSort(char **strsort, char *temp,  int num){

  int i,j,k;
  
  for(i=0;i<num-1;i++){
    j = i+1;
    k = strcmp(strsort[i],strsort[j]);
    if(k>0){
      temp = strsort[i];
      strsort[i] = strsort[j];
      strsort[j] = temp;
      i = -1;
    }

  }

  printf("Input when sorted:\n");
  for(i=0;i<num;i++){
    printf("%s\n", strsort[i]);
  }

}


int main(){
  int num,length,i,k;
  printf("Number of strings: ");
  scanf(" %d", &num);
  printf("Maximum string length: ");
  scanf(" %d", &length);
  k = length + 1;
  k = k*sizeof(char);
  char *temp = (char*)malloc(k);
  k = num;
  char *numb[num];
  for(i=0;i<num;i++){
    k = length + 1;
    numb[i] = (char*)malloc(k*sizeof(char));    
    printf("Give string %d: ",i);
    scanf(" %s", numb[i]);
    
  }

  bubbleSort(numb,temp,num);
  
  free(temp);
  for(i=0;i<num;i++){
    free(numb[i]);
  }
  return 0;
}
