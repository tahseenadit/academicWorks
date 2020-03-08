#include<stdio.h>

float minimumTwoFloats(float a, float b){

  if(a<b){
    return a;
  }else{
    return b;
  }

}

float minimumFourFloats(float a,float b,float c,float d){

  float minimum;
  minimum = minimumTwoFloats(a,b);
  minimum = minimumTwoFloats(minimum,c);
  minimum = minimumTwoFloats(minimum,d);
  return minimum;

}


float maximumTwoFloats(float a, float b){

  if(a<b){
    return b;
  }else{
    return a;
  }

}

float maximumFourFloats(float a,float b,float c,float d){

  float maximum;
  maximum = maximumTwoFloats(a,b);
  maximum = maximumTwoFloats(maximum,c);
  maximum = maximumTwoFloats(maximum,d);
  return maximum;
  

}

float sumofFloats(float a,float b,float c,float d){

  return a+b+c+d;
  
}

int main(){
  float a,b,c,d,min,max,sum,mean;
  printf("Give four floats: ");
  scanf(" %f %f %f %f", &a,&b,&c,&d);
  min = minimumFourFloats(a,b,c,d);
  printf("min: %f\n", min);
  max = maximumFourFloats(a,b,c,d);
  printf("max: %f\n", max);
  sum = sumofFloats(a,b,c,d);
  printf("sum: %f\n", sum);
  mean = sum/4;
  printf("mean: %f\n", mean);
  return 0;
}
