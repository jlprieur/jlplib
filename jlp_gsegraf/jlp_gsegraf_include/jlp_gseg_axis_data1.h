/*************************************************************************
* jlp_gseg_axis_data1.h
*
* JLP
* Version 20/03/2019
**************************************************************************/
#ifndef _jlp_gseg_axis_data1_h
#define _jlp_gseg_axis_data1_h

#include "jlp_gsegraf_defs.h"   // GSEG_AXIS_DATA structure

/* Declaring linkage specification to have "correct names"
* that can be linked with C programs */

#ifdef __cplusplus
extern "C" {
#endif

int jlp_gseg_init_axis_data(int gdev_graphic_type0, GSEG_AXIS_DATA *axdata0);
int jlp_gseg_copy_axis_data(GSEG_AXIS_DATA *axdata0, GSEG_AXIS_DATA axdata1);

#ifdef __cplusplus
}
#endif

#endif
