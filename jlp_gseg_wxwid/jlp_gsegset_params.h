/****************************************************************************
* Name: jlp_gsegset_params.h
*
* JLP
* Version 11/03/2016
****************************************************************************/
#ifndef _jlp_gsegset_params_h
#define _jlp_gsegset_params_h

#include "jlp_gsegset_defs.h"    // GSEG_SETTINGS, NBLK_MAX, etc

 void Init_GSEG_PARAM_from_SETTINGS(GSEG_PARAM *GsegParam0,
                                    GSEG_SETTINGS GsegSet1, int *nparams);
 void Init_GSEG_SETTINGS_with_default_values(GSEG_SETTINGS &GsegSet0);
 void Copy_GSEG_PARAM_to_SETTINGS(GSEG_PARAM *GsegParam0, 
                                  GSEG_SETTINGS &GsegSet1);
 void Copy_GSEG_SETTINGS(GSEG_SETTINGS &GsegSet0, GSEG_SETTINGS GsegSet2);

#endif
