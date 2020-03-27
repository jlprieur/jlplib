/****************************************************************************
* Name: jlp_rw_keywd.h
*
* JLP
* Version 11/03/2016
****************************************************************************/
#ifndef _jlp_rw_keywd_h
#define _jlp_rw_keywd_h

// In "jlp_rw_keywd.cpp":
int jlp_rd_char_keywd(char *in_line, char *keyword, char *cvalue);
int jlp_rd_int_keywd(char *in_line, char *keyword, int *ivalue);
int jlp_rd_double_keywd(char *in_line, char *keyword, double *dvalue);
int jlp_rd_float_keywd(char *in_line, char *keyword, float *fvalue);

#endif
