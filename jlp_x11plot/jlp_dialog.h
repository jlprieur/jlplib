/*********************************************************************
* jlp_dialog.h
* prototypes of functions contained in "jlp_dialog.cpp" 
* (C interface with JLP_Dialog Class)
*
* JLP
* Version 10/11/2006
*********************************************************************/
#ifndef __jlp_dialog_h
#define __jlp_dialog_h

#include "jlp_gdev_def.h"

/* Declaring linkage specification to have "correct names"
* that can be linked with C programs */

#ifdef __cplusplus
extern "C" {
#endif

/* In "jlp_dialog.cpp" */
int JLP_Dlg_GetString(char *title, char *explanations, char *prompt,
                      char *sval, int *is_OK);
int JLP_Dlg_GetInt(char *title, char *explanations, char *prompt,
                   int *ival, int *is_OK);
int JLP_Dlg_GetFloat(char *title, char *explanations, char *prompt,
                        double *fval, int *is_OK);
int JLP_Dlg_CurveHardcopyOptions(char *pst_filename, char *pst_plotdev,
                                 char *title, int *TeX_fonts, int *is_OK);
int JLP_Dlg_ImageHardcopyOptions(char *pst_filename, char *pst_plotdev,
                                 int *TeX_fonts, int *lut_scale,
                                 int *black_and_white, int *high_resolution,
                                 int *is_OK);

/* In "jlp_logbook.cpp" */ 
int JLP_Logbook_display(char *logbook_fname, int *process_data);
int JLP_Logbook_open(char *title, int *idv_logbook);
int JLP_Logbook_close(int idv_logbook);
int JLP_Logbook_update(char *logbook_fname, int idv_logbook);

#ifdef __cplusplus
}
#endif

#endif  /* EOF sentry */
