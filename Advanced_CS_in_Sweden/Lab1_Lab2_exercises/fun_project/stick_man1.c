#include<stdio.h>

int main(){

  int i;
  int j;
  int arr[8][8];
  
  for(i=0;i<8;i++){
    for(j=0;j<8;j++){
      arr[i][j]=0;
    }
  }

  arr[0][4]=1;
  arr[1][3]=1;
  arr[1][4]=1;
  arr[1][5]=1;
  arr[2][4]=1;
  arr[3][4]=1;
  arr[4][4]=1;
  arr[5][3]=1;  
  arr[5][5]=1;
  arr[6][2]=1;
  arr[6][6]=1;
  arr[7][1]=1;
  arr[7][7]=1;
  
  for(i=0;i<8;i++){
    for(j=0;j<8;j++){
      printf(" %d", arr[i][j]);
    }
    printf("\n");
  }

  return 0;

}
