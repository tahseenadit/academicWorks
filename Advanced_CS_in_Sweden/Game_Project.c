// Project for Advanced CS Studies.
// Group 33



#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include <time.h>
#include <string.h>

#include "/usr/include/python2.7/Python.h"
#include "joystick.h"
#include "led_matrix.h"


uint16_t car_color = RGB565_CYAN;

int car_len = 3;

int run = 1;

int score = 0;
int speed = 500;
int n = 3;

int led_num_car1 = 43;
int led_num_car2 = 44;
int led_num_car3 = 51;
int led_num_car4 = 52;
int led_num_car5 = 59;
int led_num_car6 = 60;

int start;
int led_num_obs1;
int led_num_obs2;
int led_num_obs3;
int led_num_obs4;
int led_num_obs5;
int led_num_obs6;


void* generate_obs(void *arg)
{  
	while (run)
	{
		if(led_num_obs1<64)
		{
			led_num_obs1 = led_num_obs1 + 8;
			led_num_obs2 = led_num_obs2 + 8;
			led_num_obs3 = led_num_obs3 + 8;
			led_num_obs4 = led_num_obs4 + 8;
			led_num_obs5 = led_num_obs5 + 8;
			led_num_obs6 = led_num_obs6 + 8;

		}
		else
		{
			sleep_ms(200);
			score = score + 10;
			if(score == n*10 && speed > 100)
			{
				speed = speed - 100;
				n = n + 3;
			}
			start = 1 + rand() % 5;
			led_num_obs1 = start;
			led_num_obs2 = start + 1;
			led_num_obs3 = start + 8;
			led_num_obs4 = start + 9;
			led_num_obs5 = start + 16;
			led_num_obs6 = start + 17;
		}
		sleep_ms(speed);
	}
	pthread_exit(0);
}

void* handle_input(void *arg) 
{
  struct js_event ev;
  open_joystick_device();
    
  while(1)
    {
		ev = read_joystick_input();

		if(ev.type==JOYSTICK_PRESS)
		{
			if (ev.direction == DIRECTION_WEST && led_num_car1 > 41) 
			{
				led_num_car1 = led_num_car1 -1;
				led_num_car2 = led_num_car2 -1;
				led_num_car3 = led_num_car3 -1;
				led_num_car4 = led_num_car4 -1;
				led_num_car5 = led_num_car5 -1;
				led_num_car6 = led_num_car6 -1;
			
			} 
			else if (ev.direction == DIRECTION_EAST && led_num_car1 < 45) 
			{
				led_num_car1 = led_num_car1 +1;
				led_num_car2 = led_num_car2 +1;
				led_num_car3 = led_num_car3 +1;
				led_num_car4 = led_num_car4 +1;
				led_num_car5 = led_num_car5 +1;
				led_num_car6 = led_num_car6 +1;
				
			} 
			else if (ev.direction == DIRECTION_DOWN) 
			{
				run=0;
				break;
			}       
		}
    }

  close_joystick_device();
  pthread_exit(0);
}

void* detect_collision(void *arg)
{
  while(run)
  {
    if(led_num_car1 == led_num_obs6 || led_num_car2 == led_num_obs5){
      run = 0;
    }else if(led_num_car1 == led_num_obs4 || led_num_car2 == led_num_obs3){
      run = 0;
    }else if(led_num_car1 == led_num_obs2 || led_num_car2 == led_num_obs1){
      run = 0;
    }else if(led_num_car3 == led_num_obs6 || led_num_car4 == led_num_obs5){
      run = 0;
    }else if(led_num_car3 == led_num_obs4 || led_num_car4 == led_num_obs3){
      run = 0;
    }else if(led_num_car3 == led_num_obs2 || led_num_car4 == led_num_obs1){
      run = 0;
    }else if(led_num_car5 == led_num_obs6 || led_num_car6 == led_num_obs5){
      run = 0;
    }else if(led_num_car5 == led_num_obs4 || led_num_car6 == led_num_obs3){
      run = 0;
    }else if(led_num_car5 == led_num_obs2 || led_num_car6 == led_num_obs1){
      run = 0;
    }else if(led_num_car1 == led_num_obs5 || led_num_car2 == led_num_obs6){
      run = 0;
    }    
  }

  pthread_exit(0);
}

void sleep_ms(int ms) 
{
	usleep(1000 * ms);
}

int main() 
{
    int sound_status;
	sound_status = system("omxplayer --no-keys --loop sound.mp3 &");
	int car_pos = 0;
	open_led_matrix();

	pthread_t inputthread,obsthread,collthread;
	int n = 1;

	srand(time(NULL));
	start = 1 + rand() % 5;
	led_num_obs1 = start;
	led_num_obs2 = start + 1;
	led_num_obs3 = start + 8;
	led_num_obs4 = start + 9;
	led_num_obs5 = start + 16;
	led_num_obs6 = start + 17;

	
	if (pthread_create(&inputthread, NULL, handle_input, &n)) 
	{
	  fprintf(stderr, "Error creating input thread.\n");
	  return -1;
	}
       
	if (pthread_create(&obsthread, NULL, generate_obs, &n)) 
	{
	  fprintf(stderr, "Error creating new obstacle.\n");
	  return -1;
	}

	if (pthread_create(&collthread, NULL, detect_collision, &n)) 
	{
	  fprintf(stderr, "Error detecting collision.\n");
	  return -1;
	}

	while (run) 
	{
		clear_leds();
		set_led_num(led_num_car1, RGB565_GREEN);
		set_led_num(led_num_car2, RGB565_GREEN);
		set_led_num(led_num_car3, RGB565_GREEN);
		set_led_num(led_num_car4, RGB565_GREEN);
		set_led_num(led_num_car5, RGB565_GREEN);
		set_led_num(led_num_car6, RGB565_GREEN);

		set_led_num(led_num_obs1, RGB565_RED);
		set_led_num(led_num_obs2, RGB565_RED);
		set_led_num(led_num_obs3, RGB565_RED);
		set_led_num(led_num_obs4, RGB565_RED);
		set_led_num(led_num_obs5, RGB565_RED);
		set_led_num(led_num_obs6, RGB565_RED);
		
		
		car_pos = (car_pos + 8) % NUM_LEDS;	
		for (int i = 0; i < car_len; i++) 
		{
			int led_num = (car_pos + (i*8)) % NUM_LEDS;
			set_led_num(led_num, RGB565_YELLOW);
			int led_num_2 = (car_pos + ((i+4)*8)) % NUM_LEDS;
			set_led_num(led_num_2, RGB565_YELLOW);
			int led_num_3 = (car_pos + ((i*8)+7)) % NUM_LEDS;
			set_led_num(led_num_3, RGB565_YELLOW);
			int led_num_4 = (car_pos + ((i+4)*8+7)) % NUM_LEDS;
			set_led_num(led_num_4, RGB565_YELLOW);
		}
		sleep_ms(100);
	}
      
	pthread_join(inputthread, NULL);
	pthread_join(obsthread, NULL);
	pthread_join(collthread, NULL);

	system("killall omxplayer.bin");
	clear_leds();

	Py_Initialize();
	char str[10] = "";
    sprintf(str,"%d",score);
	char str2[100] = "from sense_hat import SenseHat\nsh = SenseHat()\nsh.show_message(\"Score: ";
	char str3[10] = "\")";
	strcat(str2,str);
	strcat(str2,str3);
	PyRun_SimpleString(str2);
	Py_Finalize();
	close_led_matrix();
}