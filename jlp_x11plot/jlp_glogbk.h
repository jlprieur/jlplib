/*************************************************************************
* jlp_logbk.h
* JLP_GLogbk Class: routine to create a popup window 
*  in order to display the values of some parameters
*
* C++ version with classes
*
* JLP
* Version 04/12/2008 
**************************************************************************/
#ifndef __jlp_glogbk_h                     /* sentry */
#define __jlp_glogbk_h

#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <string.h>

/*
#define DEBUG
*/

#define LOGBOOK_NLINES_MAX 30 

// Definition of JLP_GLogbk class:
class JLP_GLogbk {

public:
  
// Main Constructor 
JLP_GLogbk(const char *title){ 
  Setup(); 
  strncpy(Logbook_title, title, 140);
};   

// JLP_GLogbk(const JLP_GLogbk&);  // Constructor for copies

// Defined in "jlp_logbk.cpp":
int UpdateLogbookBox(char *logbook_fname);

// Pure virtual function (should be followed by "=0")
// Defined in "jlp_logbk_x11.cpp":
virtual int WaitForClose(int *process_data) = 0;
virtual int CloseWindow() = 0;

protected:

/*******************************************************************
* JLP_GLogbk::Setup
* Initialize common variables 
*******************************************************************/
void Setup() {
register int i;

// Labels:
 strcpy(CloseButton_label,"Close");
 strcpy(ProcessButton_label,"Process");

// Counters:
 nLines = 0;

// Max items:
 nLines_max = LOGBOOK_NLINES_MAX;

// Initialize all lines to '\0': 
 for(i = 0; i < nLines_max; i++) {
   Logbook_label[i][0] = '\0';
   }

 return;
}

// Pure virtual functions (should be followed by "=0")
// Defined in jlp_logbk_x11.cpp:
  virtual int OpenLogbookBox(const char *title) = 0;
  virtual int DrawLogbookBox() = 0;
  virtual int CreateCloseButton(int x_ul, int y_ul, int d_width, int d_height) = 0;
  virtual int CreateProcessButton(int x_ul, int y_ul, int d_width, int d_height) = 0;

// Private variables:
int window_is_open, nLines, nLines_max;
int CloseButton_x, CloseButton_y;
int CloseButton_height, CloseButton_width;
int ProcessButton_x, ProcessButton_y;
int ProcessButton_height, ProcessButton_width;
int Logbook_screen;
int Logbook_width, Logbook_height;
char CloseButton_label[20], ProcessButton_label[20];
char Logbook_title[120];
unsigned long Gray[256];
unsigned long int Black, White;
int Font_width, Font_height;

char Logbook_label[LOGBOOK_NLINES_MAX][80];
};

#endif    /* __jlp_glogbk_h sentry */
