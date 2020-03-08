#include <stdio.h>

void compareZero(int a){

  if(a == 0){
    printf("The number you entered equals zero\n");
  }else{

    printf("The number you entered does not equal zero\n");
  }

}

void compareFloats(float a,float b){

  if(a > b){
    printf("%f is the largest\n", a);
  }else{
    printf("%f is the largest\n", b);
  }
}

void compareDivision(int a){

  if((a % 2) == 0){
    printf("Result is: %d\n", a/2);
  }else{
    printf("Result is: %d\n", a*3);
  }
}

void compareUnique(int a,int b,int c){

  if(a == b || a == c || b == c){
    printf("Some numbers are equal\n");
  }else{
    printf("All are unique\n");
  }
}

int main(){

  int a;
  printf("Give an integer: ");
  scanf("%d",&a);
  compareZero(a);
  
  float b,c;
  printf("Give two floats: ");
  scanf("%f %f",&b, &c);
  compareFloats(b,c);

  printf("Give an integer: ");
  scanf("%d",&a);
  compareDivision(a);

  int d,e;
  printf("Give three integers: ");
  scanf("%d %d %d",&a,&d,&e);
  compareUnique(a,d,e);
  
  return 0;
  
}
