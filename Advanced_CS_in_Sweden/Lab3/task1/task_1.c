#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <unistd.h>
#include <sys/wait.h>

#include "led_matrix.h"

int rand_range(int min_n, int max_n){

  return rand() % (max_n + 1 - min_n) + min_n;
}

int sleep_ms(int milliseconds){

  if(usleep(1000* milliseconds) == -1){
    return -1;
  }
}

int main(){

  int pid;
  uint16_t color;
  int red_val,green_val,blue_val;

  srand(time(NULL));
  
  if(open_led_matrix() == -1){
    return -1;
  }

  pid = fork();

  if(pid == 0){
    red_val = rand_range(0, 255);
    green_val = rand_range(0, 255);
    blue_val = rand_range(0, 255);
    color = make_rgb565_color(red_val, green_val, blue_val);
    if(usleep(2000000) == -1){
      return -1;
    }
    set_led(0,0,color);
    printf("I'm the child! Lighting LED at (0,0)\n");
  }else{
    red_val = rand_range(0, 255);
    green_val = rand_range(0, 255);
    blue_val = rand_range(0, 255);
    color = make_rgb565_color(red_val, green_val, blue_val);
    set_led(0,1,color);
    printf("I'm the parent! Lighting LED at (0,1)\n");
  }
  wait(NULL);  
  if(usleep(2000000) == -1){
    return -1;
  }
  clear_leds();
  if (close_led_matrix() == -1){    
    return -1;
  }

  return 0;

}
