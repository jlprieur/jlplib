/*************************************************************************
* jlp_gsegset_rw_paramfiles.cpp
* To read/write parameter files
*
* JLP
* Version 12/06/2017
**************************************************************************/
using namespace std;
#include <stdio.h> 

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifndef WX_PRECOMP
    #include "wx/wx.h"
#endif

#include "jlp_rw_keywd.h"  // jlp_rd_char_keywd(), ...
#include "jlp_gsegset_params.h"   // Init_GSEG_PARAM_from_SETTINGS, ...
#include "jlp_gsegset_rw_ascii.h"

// #define DEBUG

static int decode_line_param(char *in_line, GSEG_PARAM *SpeaParam0, 
                             int n_param);

/*************************************************************************
* Save parameters to file (Keywords format) 
*
* INPUT:
* out_param_file: name of the file
*
*************************************************************************/
int WriteParamToKeywordFile(const char *out_param_file, GSEG_SETTINGS Spea0) 
{
int status, i, type, imax, nparams, kk;
char cvalue[128], keywd[40];
GSEG_PARAM SpeaParam0[512];
FILE *fp_out;

// Load current values to SpeaParam0:
Init_GSEG_PARAM_from_SETTINGS(SpeaParam0, Spea0, &nparams);

if((fp_out = fopen(out_param_file, "w")) == NULL) {
  fprintf(stderr, "WriteParamToKeywordFile/Fatal error opening output file >%s<\n",
          out_param_file);
  return(-1);
  }

for(i = 0; i < 35; i++) {
  type = SpeaParam0[i].type;
  strcpy(keywd, SpeaParam0[i].keywd.mb_str());
  switch(type) {
    case 0:
      fprintf(fp_out, "%s = %d\n", keywd, SpeaParam0[i].ivalue);
      break;
    case 1:
      fprintf(fp_out, "%s = %.3f\n", keywd, SpeaParam0[i].fvalue);
      break;
    case 2:
      if(SpeaParam0[i].bvalue == true) strcpy(cvalue, "y");
      else strcpy(cvalue, "n");
      fprintf(fp_out, "%s = %s\n", keywd, cvalue);
      break;
    case 3:
      strncpy(cvalue, SpeaParam0[i].svalue.mb_str(), 127);
      fprintf(fp_out, "%s = %s\n", keywd, cvalue);
      break;
    }
}

// n_weightblk, int1_blk[0], int2_blk[0], weight_blk[0], etc 
kk = 35;
for(i = 0; i < 1 + 3 * Spea0.n_weightblk; i++) {
  strcpy(keywd, SpeaParam0[kk].keywd.mb_str());

  if(SpeaParam0[kk].type == 0)
    fprintf(fp_out, "%s = %d\n", keywd, SpeaParam0[kk].ivalue);
  else
// For weights (real values):
    fprintf(fp_out, "%s = %.3f\n", keywd, SpeaParam0[kk].fvalue);

  kk++;
  }

// n_drop_int, int1_drop[0], int2_drop[0], etc.  
kk = 36 + 3 * NBLK_MAX;
for(i = 0; i < 1 + 2 * Spea0.n_drop_int; i++) {
  strcpy(keywd, SpeaParam0[kk].keywd.mb_str());
  fprintf(fp_out, "%s = %d\n", keywd, SpeaParam0[kk].ivalue);
  kk++;
  }

fclose(fp_out);

return(0);
}
/*************************************************************************
* Read parameters from file (Keywords format) 
*
* INPUT:
* in_param_file: name of the file
*
*************************************************************************/
int ReadParamFromKeywordFile(const char *in_param_file, GSEG_SETTINGS &Spea0) 
{
char in_line[128];
FILE *fp_in;
GSEG_PARAM SpeaParam0[512];
int nparams;

// Load default values to SpeaParam1:
Init_GSEG_SETTINGS_with_default_values(Spea0);
Init_GSEG_PARAM_from_SETTINGS(SpeaParam0, Spea0, &nparams);

if((fp_in = fopen(in_param_file, "r")) == NULL) {
  fprintf(stderr, "ReadParamFromKeywordFile/Fatal error opening input file >%s<\n",
          in_param_file);
  return(-1);
  }

while(!feof(fp_in)) {
  if(fgets(in_line, 128, fp_in)) {
/* Check if it is not a comment: (\r: CarriageReturn (int)'\r'=13)*/ 
    if((in_line[0] != '%') && (in_line[0] != '\r') && (in_line[0] != '\r')){
      decode_line_param(in_line, SpeaParam0, nparams);
    } /* EOF in_line != % */
  } /* EOF fgets */
 } /* EOF while */

fclose(fp_in);

// Load values to Spea0:
Copy_GSEG_PARAM_to_SETTINGS(SpeaParam0, Spea0);

return(0);
}
/***************************************************************************
*
***************************************************************************/
static int decode_line_param(char *in_line, GSEG_PARAM *SpeaParam0, int n_param) 
{
int status, i, type, ivalue;
float fvalue;
bool bvalue;
char cvalue[128], keywd[128];

status = -1;
for(i = 0; i < n_param && status != 0; i++) {
  type = SpeaParam0[i].type;
  strcpy(keywd, SpeaParam0[i].keywd.mb_str());
  switch(type) {
    case 0:
      status = jlp_rd_int_keywd(in_line, keywd, &ivalue);
      if(status == 0) {
        SpeaParam0[i].ivalue = ivalue; 
#ifdef DEBUG
printf("DEBUG/decode_line i=%d key=%s ivalue=%d \n", i, keywd, ivalue);
#endif
        }
      break;
    case 1:
      status = jlp_rd_float_keywd(in_line, keywd, &fvalue);
      if(status == 0) {
        SpeaParam0[i].fvalue = fvalue; 
#ifdef DEBUG
printf("DEBUG/decode_line: i=%d key=%s fvalue=%f \n", i, keywd, fvalue);
#endif
        }
      break;
    case 2:
      status = jlp_rd_char_keywd(in_line, keywd, cvalue);
      if(status == 0) {
        if(cvalue[0] == 'n' || cvalue[0] == 'N') bvalue = false;
          else bvalue = true;
        SpeaParam0[i].bvalue = bvalue; 
#ifdef DEBUG
printf("DEBUG/decode_line: i=%d key=%s bvalue=%d\n", i, keywd, bvalue);
#endif
        }
      break;
    case 3:   
      status = jlp_rd_char_keywd(in_line, keywd, cvalue);
      if(status == 0) {
        SpeaParam0[i].svalue = wxString(cvalue); 
#ifdef DEBUG
printf("DEBUG/decode_line: i=%d key=%s cvalue=%s\n", i, keywd, cvalue);
#endif
        }
      break;
    } 
}

return(status);
}
