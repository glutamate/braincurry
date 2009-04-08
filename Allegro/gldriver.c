#include <stdlib.h>    // For malloc() etc.
#include <stdio.h>     // For printf(), fopen() etc.
#include <math.h>      // For sin(), cos() etc.
#include <GL/glfw.h>   // For GLFW, OpenGL and GLU
#include <sys/time.h>
#include <time.h>
#include <dlfcn.h>
#include <string.h>
#include <signal.h>
//#include "dyndriver.h"
#include <sched.h>

#define max(a,b) (((a) > (b)) ? (a) : (b))
#define min(a,b) (((a) < (b)) ? (a) : (b))

char *file_name;
long tmax_us;
void *dl_handle=NULL;
int ready=0;
struct timeval tstart;
void (*anim)(double);
long t_tic;

void reset_time() {
    gettimeofday(&tstart, NULL);
}
long usecs_elapsed() {
  struct timeval tnow;
  gettimeofday(&tnow, NULL);
  return ((long) (tnow.tv_sec-tstart.tv_sec))*1000*1000+((long) (tnow.tv_usec-tstart.tv_usec));
}

void tic() {
  t_tic = usecs_elapsed();
}

long toc() {
  return usecs_elapsed()-t_tic;
}

void print_time() {
     printf("time = %ld us\n", usecs_elapsed());
}
int wait_until_usecs(long us) {
    struct timespec ts;
    long wait_for_us=max(us-usecs_elapsed(),1);
    ts.tv_sec = (long) floor(((double) wait_for_us)/(1000*1000));
    ts.tv_nsec = 1000*(wait_for_us-ts.tv_sec*1000*1000);
    //printf("wait for %d s %ld ns\n", ts.tv_sec, ts.tv_nsec);
    nanosleep (&ts, NULL);

}
void gl_driver_init() {
    int    ok;             // Flag telling if the window was opened
    int    running;        // Flag telling if the program is running

    // Initialize GLFW
    glfwInit();

    // Open window
    ok = glfwOpenWindow(
        640, 480,          // Width and height of window
        8, 8, 8,           // Number of red, green, and blue bits for color buffer
        8,                 // Number of bits for alpha buffer
        24,                // Number of bits for depth buffer (Z-buffer)
        0,                 // Number of bits for stencil buffer
        GLFW_FULLSCREEN    // We want a desktop window (could be GLFW_FULLSCREEN)
    );

    // If we could not open a window, exit now
    if( !ok )
    {
        glfwTerminate();
        //return 0;
    }
    //glMatrixMode( GL_PROJECTION );
//glLoadIdentity();
                         //glFrustrum(-0.02, 0.02, -0.015, 0.015);
    // Set window title
    glfwSetWindowTitle( "My OpenGL program" );
    glfwSwapInterval(1);

    glClearColor(0.0f, 0.23f, 0.0f, 0.0f);
      glClear(GL_COLOR_BUFFER_BIT);
      glfwSwapBuffers();



  reset_time();
  wait_until_usecs(2*1000*1000);
  reset_time();
  ready = 1;
}


void open_anim() {
  double  (*tmax_ms)();
  void  (*bg_col)();
  char *error;
  if(!ready) {
	printf("already opening, call back later\n");
	return;
   }

  ready=0;

  dl_handle = dlopen( file_name, RTLD_NOW );
  if (!dl_handle) {
    printf( "error opening dynanim: %s\n", dlerror() );
    return;
  }

  anim = dlsym( dl_handle, "the_animation" );
  error = dlerror();
  if (error != NULL) {
    printf( "error resolving animation %s\n", error );
    return;
  }

  tmax_ms = dlsym( dl_handle, "tmax_s" );
  error = dlerror();
  if (error != NULL) {
    printf( "error resolving max time %s\n", error );
    return;
  }

  bg_col = dlsym( dl_handle, "bg_col" );
  error = dlerror();
  if (error == NULL) {
    //struct mycol bgc;
    //bgc= bg_col();
    //Uint32 bg_color32 = SDL_MapRGB(bg->format, bgc.r, bgc.g, bgc.b);
    //SDL_Rect rct = {0, 0, 640, 480};
    //SDL_FillRect(bg, &rct, bg_color32);
    printf("changing bg from dynanim\n");

  }

  //printf("requested tmax = %ld ms\n", tmax_ms());

  tmax_us = (long) (tmax_ms()*1000*1000);
  ready=1;

}

void close_anim() {
  dlclose(dl_handle);
}

double play_anim() {
  long tnow, tlast, t0, largest_diff=0, largest_exec=0, largest_swap=0;
  tnow=0;
  tlast=0;
  int s;
  char *error;
  if(!ready) {
	printf("busy, call back later\n");
   }
  if(!dl_handle) {
	printf("no open animation\n");
   }
  ready=0;
  printf("play anim for %ld us\n", tmax_us);
  reset_time();
  while (tnow<tmax_us)
    {
      glClear(GL_COLOR_BUFFER_BIT);
      tlast=tnow;
      tnow=usecs_elapsed();
      largest_diff=max(largest_diff, tnow-tlast);
      //draw_bg();
      tic();
      anim( ((double) tnow)/1000000.0);
      largest_exec=max(largest_exec, toc());
      //my_flip();
      tic();
      glfwSwapBuffers();
      largest_swap=max(largest_swap, toc());
      //      wait_until_usecs(tnow+10);
    }
  //centre_rect(screen, 640,0,0,0);
  //draw_bg();
  //SDL_Flip(screen);
  ready=1;
      glClear(GL_COLOR_BUFFER_BIT);
      glfwSwapBuffers();
  printf("largest time to execute animation = %ld us\n", largest_exec);
  printf("largest time to swap = %ld us\n", largest_swap);
  printf("largest time difference between frames = %ld us\n", largest_diff);
  return largest_diff;

}

void signal_prepare(int s) {
  if(dl_handle) close_anim();
  open_anim();

}

void signal_go(int s) {
  //if(!dl_handle) e_anim();
  play_anim();
}

main(int argc, char *argv[])
{
  struct sched_param schedparam;
  schedparam.sched_priority = 80;
  printf("setsched=%d\n", sched_setscheduler(0, SCHED_FIFO, &schedparam));
  signal(SIGUSR1, &signal_prepare);
  signal(SIGUSR2, &signal_go);
  if(argc!=2) { printf("dyndriver: please supply file namefor dynamic animation\n"); exit(0);}
  file_name = argv[1];
  gl_driver_init();
  //open_anim();
  //printf("largest time difference between frames = %g ms\n", play_anim());
  while(1) {pause();}
  close_anim();
}
