#include<stdio.h>
#include<stdlib.h>

void threeColorsSort(int *theArray, int arraySize){

  int i,j,k,temp;
  k = 0;

  for(i=0;i<=2;i++){

    for(j=k;j<arraySize;j++){

      if(theArray[j]==i){
	
	temp = theArray[k];
	theArray[k] = theArray[j];
	theArray[j] = temp;
	k++;
      }

    }
  }

  printf("Input when sorted: \n");

  for(i=0;i<arraySize;i++){

    printf("%d\n", theArray[i]);

  }

}

int main(){

  int input,i;
  printf("Number of inputs: ");
  scanf(" %d", &input);
  int *theArray = (int*)malloc(input*sizeof(int));
  for(i=0;i<input;i++){
    printf("Give number %d: ", i);
    scanf(" %d", &theArray[i]);
    if(theArray[i]>2 || theArray[i]<0){
      i = i-1;
    }
  }
  threeColorsSort(theArray, input);

  free(theArray);
  return 0;
}
