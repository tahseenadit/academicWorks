#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <unistd.h>
#include <sys/wait.h>

#include "led_matrix.h"

uint16_t color;

int rand_range(int min_n, int max_n){

  return rand() % (max_n + 1 - min_n) + min_n;
}

void sleep_ms(int milliseconds){

  usleep(1000* milliseconds);
}

void pointless_calculation () {
  int amount_of_pointlessness = 100000000;
  int x = 0;
  for ( int i = 0; i < amount_of_pointlessness ; i++) {
    x+=i;
  }
}

void run_child (int n) {
  int row = n;
  int i;
  int red_val, green_val, blue_val;
  for (i = 0; i < 8;i++) {
	pointless_calculation();
	red_val = rand_range(0, 255);
	green_val = rand_range(0, 255);
	blue_val = rand_range(0, 255);
	color = make_rgb565_color(red_val, green_val, blue_val);
	set_led(row,i,color);	
  }
}

int main(){

  int pid;
  int i,j;
 
  srand(time(NULL));

  if(open_led_matrix() == -1){
    return -1;
  }

  for(i=1; i <= 8; i++){
    for(j=0; j <= (i-1); j++){
      pid = fork();
      if(pid == 0){
	nice(j);
	run_child(j);
	exit(0);
      }
    }
    for(j=0;j <= (i-1); j++){
      wait(NULL);
    }
    if(usleep(1000000) == -1){
      return -1;
    }
    clear_leds();
    
  }
 
  if (close_led_matrix() == -1){
    return -1;
  }

  return 0;

}
