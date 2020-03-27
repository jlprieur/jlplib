/****************************************************************************
* Name: jlp_gsegset_rw_ascii.h
*
* JLP
* Version 11/03/2016
****************************************************************************/
#ifndef _jlp_gsegset_rw_ascii_h
#define _jlp_gsegset_rw_ascii_h

#include "jlp_gsegset_defs.h"    // GSEG_SETTINGS, NBLK_MAX, etc

// In "jlp_gsegset_rw_ascii.cpp":
 int ReadParamFromKeywordFile(const char *in_filename, GSEG_SETTINGS &GsegSet0);
 int WriteParamToKeywordFile(const char *in_filename, GSEG_SETTINGS GsegSet0);
 int ReadParamFromGsegrafFile(const char *in_filename, GSEG_SETTINGS &GsegSet0);
 int WriteParamToGsegrafFile(const char *in_filename, GSEG_SETTINGS GsegSet0);

#endif
