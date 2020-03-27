/*************************************************************************
* jlp_rw_keywd.cpp
* To read/write keywrods in ASCII files
*
* JLP
* Version 12/06/2016
**************************************************************************/
#include <stdio.h>
#include <string.h> 
#include "jlp_rw_keywd.h"

// #define DEBUG

/* Contains :
int jlp_rd_char_keywd(char *in_line, char *keyword, char *cvalue);
int jlp_rd_int_keywd(char *in_line, char *keyword, int *ivalue);
int jlp_rd_double_keywd(char *in_line, char *keyword, double *dvalue);
int jlp_rd_float_keywd(char *in_line, char *keyword, float *fvalue);
*/

/***********************************************************************
* Read keyword that is located at the beginning of the line 
* for instance:
* N_OB=   47 or NB_OB = 47                        (keyword: "N_OBS:")
***********************************************************************/
int jlp_rd_char_keywd(char *in_line, char *keyword, char *cvalue)
{
 int klen, status = -1, ival;
 char *pc, buffer[128];

 *cvalue = '\0'; 
 klen = strlen(keyword);

// Test if line starts with keyword:
 if(strncmp(in_line,keyword, klen) == 0) {
// Good line:
   if(in_line[klen + 1] != '\0' && in_line[klen + 2] != '\0') {
     pc = &in_line[klen + 2];
     strncpy(buffer, pc, 128); 
     status = 0;
   }
 }

if(status == 0) {
// Removes heading blanks: 
pc = buffer;
while(*pc == ' ') pc++;
strncpy(cvalue, pc, 128); 

// Removes everything after comments (starting with "!"):
pc = cvalue;
while(*pc && *pc != '!' && *pc != '\r' && *pc != '\n') pc++;
*pc = '\0';
}

return(status);
}
/***********************************************************************
* Read keyword that is located at the beginning of the line 
* for instance:
* N_OB:   47                        (keyword: "N_OBS:")
***********************************************************************/
int jlp_rd_int_keywd(char *in_line, char *keyword, int *ivalue)
{
 int klen, status = -1, ival;
 char *pc;

 klen = strlen(keyword);

// Test if line starts with keyword:
 if(strncmp(in_line,keyword, klen) == 0) {
// Good line:
   if(in_line[klen + 1] != '\0' && in_line[klen + 2] != '\0') {
     pc = &in_line[klen + 2];
     if(sscanf(pc, "%d", &ival) == 1) {
       *ivalue = ival; 
       status = 0;
       }
   }
 }
return(status);
}
/***********************************************************************
* Read keyword that is located at the beginning of the line 
* for instance:
* N_OB:   47                        (keyword: "N_OBS:")
***********************************************************************/
int jlp_rd_double_keywd(char *in_line, char *keyword, double *dvalue)
{
int klen, status = -1;
double dval;
char *pc;

 klen = strlen(keyword);

// Test if line starts with keyword:
 if(strncmp(in_line,keyword, klen) == 0) {
// Good line:
   if(in_line[klen + 1] != '\0' && in_line[klen + 2] != '\0') {
     pc = &in_line[klen + 2];
     if(sscanf(pc, "%lf", &dval) == 1) {
       *dvalue = dval;
        status = 0;
       }
   }
 }
return(status);
}
/***********************************************************************
* Read keyword that is located at the beginning of the line 
* for instance: 
* N_OB:   47                        (keyword: "N_OBS:")
* K1:    *                *         (keyword: "N_OBS:")
***********************************************************************/
int jlp_rd_float_keywd(char *in_line, char *keyword, float *fvalue)
{
 int klen, status = -1;
 float fval;
 char *pc;

 klen = strlen(keyword);

// Test if line starts with keyword:
 if(strncmp(in_line,keyword, klen) == 0) { 
// Good line:
   if(in_line[klen + 1] != '\0' && in_line[klen + 2] != '\0') { 
     pc = &in_line[klen + 2]; 
     if(sscanf(pc, "%f", &fval) == 1) {
       *fvalue = fval; 
        status = 0;
       }
   }
 }
return(status);
}
