/****************************************************************************
* Name: jlp_read_keywd.h
*
* JLP
* Version 11/03/2019
****************************************************************************/
#ifndef _jlp_read_keywd_h
#define _jlp_read_keywd_h

// In "jlp_read_keywd.cpp":
int jlp_read_keywd_char(char *in_line, char *keyword, char *cvalue);
int jlp_read_keywd_int(char *in_line, char *keyword, int *ivalue);
int jlp_read_keywd_double(char *in_line, char *keyword, double *dvalue);
int jlp_read_keywd_float(char *in_line, char *keyword, float *fvalue);

#endif
